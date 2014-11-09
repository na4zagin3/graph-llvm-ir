open Printf;;

(* LLVM libraries *)
open Llvm;;
open Llvm_bitreader;;

(* Utilities *)
(* let is_empty s = 0 = String.compare "" s *)
let is_empty s = 0 = String.length s
let is_void t = classify_type t == TypeKind.Void
let is_branch t =
  match instr_opcode t with
  | Opcode.Ret
  | Opcode.Br
  | Opcode.Switch
  | Opcode.IndirectBr
  | Opcode.Invoke
  | Opcode.Invalid2
  | Opcode.Unreachable -> true
  | _ -> false
let name_of_block b = value_name (value_of_block b)
let first_instr b =
  match instr_begin b with
  | At_end _ -> raise (Invalid_argument "first_instr: empty BasicBlock")
  | Before i -> i
let from_some = function
  | None -> raise (Invalid_argument "from_some")
  | Some x -> x
let rec iota n ?(step=1) m =
  if m <= 0 then []
  else n :: iota (n + step) ~step (m - 1)

(* Declear Variables? *)
(* let #USE_CLUSTERS = 0 *)
let cluster_edges = 0
let inv_nodes = 0
(* let EXPLICIT_CONTROL = 0 *)
(* let CONTROL_BETWEEN_DATAFLOW_TREES = 1 *)

let tmp_i = ref 1

(**
    This function establishes explicit names for nameless numeric
    temporaries in IR. It also should give human-readable IDs to each
    statement in IR. Actually, as this is SSA, it uses result temporary
    name as an ID for statement. And fails here, because void-typed
    statements do not allow to set temporary name. So, this needs rework,
    and so far worked around during graph construction.
*)
let number_tmps modl =
  let f_insn i =
      let i_name = value_name i in
      let i_type = type_of i in
      if is_empty i_name && not (is_void i_type)
      then let new_name = Printf.sprintf "t%d" !tmp_i in
        set_value_name new_name i;
       tmp_i := !tmp_i + 1
      else () in
  let f_block b =  begin
    iter_instrs f_insn b
  end in
  let f_function = iter_blocks f_block in
  iter_functions f_function modl

module BBlockMap = Map.Make(struct type t = llbasicblock let compare = compare end)
module ValueMap = Map.Make(struct type t = llvalue let compare = compare end)
type options = {
  dag_control: bool;
  block_edges: bool;
  block_edges_helpers: bool;
  block: bool;
  control: bool;
}
module Graph = struct
  type t = {
    f: llvalue;
    out: out_channel;
    options: options;
    mutable edges: string list; 
    mutable anon_bblock_cnt: int;
    mutable anon_bblock_names: string BBlockMap.t;
    mutable void_instr_cnt: int;
    mutable void_instr_names: string ValueMap.t
  }
  let create f out options = {
    f = f;
    out = out;
    options = options;
    edges = [];
    anon_bblock_cnt = 0;
    anon_bblock_names = BBlockMap.empty;
    void_instr_cnt = 0;
    void_instr_names = ValueMap.empty
  }

  let write self line =
    output_string self.out line;
    output_string self.out "\n"

  let start_graph self =
    write self "digraph G {";
    write self "compound=true";
    if self.options.dag_control
    then write self "rankdir=BT"
    else ();
    if self.options.block_edges && not self.options.block_edges_helpers
    (* If we use cluster edges w/o intervening nodes, we need to bump
       rank (vertical) separation, because otherwise there's very
       little vert. space left to render edges after cutting out
       cluster rectangle *)
    then write self "ranksep=1"
    else ();
    write self "label=\"Black edges - dataflow, red edges - control flow\""

  let edge self from_node to_node extra =
    self.edges <- (Printf.sprintf "\"%s\" -> \"%s\"%s" from_node to_node extra) :: self.edges

  (** Returns basic block name, i.e. its entry label, or made name
      if label if absent. *)
  let  block_name self b =
    if not (is_empty (name_of_block b) ) then (name_of_block b)
    else if BBlockMap.mem b self.anon_bblock_names then BBlockMap.find b self.anon_bblock_names
    else begin (self.anon_bblock_cnt <- self.anon_bblock_cnt + 1);
         let n = Printf.sprintf "unk_block_%d" self.anon_bblock_cnt in
         self.anon_bblock_names <- BBlockMap.add b n self.anon_bblock_names;
         n
    end
    
  (** Returns instruction name, for which result variable name is used.
      If result variable name is absent (void statement), make up name.  *)
  let instr_name self i =
    if ValueMap.mem i self.void_instr_names
    then ValueMap.find i self.void_instr_names
    else if not (is_empty (value_name i))
    then value_name i
    else begin
        self.void_instr_cnt <- self.void_instr_cnt + 1;
        let n = Printf.sprintf "_%d" self.void_instr_cnt in
        self.void_instr_names <- ValueMap.add i n self.void_instr_names;
        n
    end

    let declare_clusters self =
      if self.options.block
      then begin
        (* Pre-allocate label nodes to subgraphs, otherwise Graphviz puts them to wrong subgraphs *)
        let f b =
          let name = block_name self b in
          begin if true (* not self.options.block_edges_helpers *)
          then write self (Printf.sprintf "subgraph \"cluster_%s\" {" name)
          else () end;

          begin if not self.options.block_edges
          then write self (Printf.sprintf "\"%s\" [label=\"label: \"%s\"\"]" name name)
          else if self.options.block_edges_helpers
          then write self (Printf.sprintf "\"%s\" [shape=point height=0.02 width=0.02 color=red fixedsize=true]" name)
          else () end;

          begin if true (* not self.options.block_edges_helpers *)
          then write self "}"
          else () end in
        iter_blocks f self.f;
        write self "\n" end
        else ()

    let render self =
      (* print `f`*)
      start_graph self;
      declare_clusters self;
      let f_block b =
        let block_name = block_name self b in
        self.edges <- [];
        begin if self.options.block
        then begin
          write self (Printf.sprintf "subgraph \"cluster_%s\" {" block_name);
          write self (Printf.sprintf "label=%s" block_name) end
        else () end;
        (*
        begin if not self.options.block_edges
        then write self (Printf.sprintf "\"%s\" [label=\"label: %s\"]" block_name block_name)
        else if self.options.block_edges_helpers
        then write self (Printf.sprintf "\"%s\" [shape=point]" b.name)
        else () end;
        *)

        (* Create block entry label node and edge from it to first IR instruction *)
        begin
            if not self.options.block_edges || self.options.block_edges_helpers
            then begin
              let attr_color = "[color=red]" in
              let attr_weight = if name_of_block b == "entry" then "[weight=5]" else "" in
              let attr_lhead = if self.options.block_edges then Printf.sprintf "[lhead=\"cluster_%s\"]" block_name else "" in
              let attr = String.concat "" [attr_color; attr_weight; attr_lhead] in
              if self.options.control
              then begin
                let i0 = first_instr b in
                if value_name i0 == ""
                then begin
                  let n = instr_name self i0 in
                  edge self  block_name n (attr ^ "//!a") end
                else edge self block_name (value_name i0) (attr ^ "//!b")
              end
              else ()
            end
        end;

        begin if self.options.dag_control
                then let last_void_inst = ref block_name in
                let loop_instr i =
                    if is_void (type_of i)
                    then
                      let n = instr_name self i in
                      (* edge self !last_void_inst n "[color=blue]" *)
                      edge self n !last_void_inst "[color=blue dir=back] //!c";
                      last_void_inst := n
                    else () in
                iter_instrs loop_instr b
        end;

        begin
          let last_inst_name = ref None in
          iter_instrs (fun i ->
            let n = instr_name self i in
            write self (Printf.sprintf "\"%s\" [label=\"%s\"]" n (string_of_llvalue i));
            begin
              if self.options.control
              then match !last_inst_name with
                | None -> ()
                | Some name -> edge self name n "[color=red weight=2] //!d"
              else if is_branch i && num_operands i = 1
              then edge self (from_some !last_inst_name) n "[color=red] //!e"
              else ()
            end;
            begin
              let operands = List.map (operand i) (iota 0 (num_operands i)) in
              List.iter (fun a ->
                let arg_val = if is_constant a && is_empty (value_name a)
                                then string_of_llvalue a
                                else value_name a in
                if is_branch i && value_is_block a
                then
                  (* For jump targets, we jump from current node to label (arg) *)
                  let arg_val =
                    if self.options.block_edges && not self.options.block_edges_helpers
                    then value_name (first_instr (block_of_value a))
                    else arg_val in
                  let attr_color = "[color=red]" in
                  let attr_block_edges =
                    if self.options.block_edges
                    then Printf.sprintf "[color=red][lhead=\"cluster_%s\"][ltail=\"cluster_%s\"][weight=5]" (value_name a) block_name
                    else "" in
                  let attr_block_edges_helpers =
                    if self.options.block_edges && self.options.block_edges_helpers
                    then "[arrowhead=none]"
                    else "" in
                  let attrs = String.concat "" [attr_color; attr_block_edges; attr_block_edges_helpers] in
                  edge self n arg_val (attrs ^ "//!f")
                else
                  (* For data, flow is from opearnd to operation *)
                  edge self arg_val n ("//!g" ^ Printf.sprintf "\n/*!g (%s) (%s)*/" (string_of_llvalue a) (value_name a))
              ) operands;
              last_inst_name := Some n
            end
          ) b
        end;
        begin
          if self.options.block
          then write self "}"
          else ()
        end;
        List.iter (write self) self.edges
      in
      iter_blocks f_block self.f;
      write self "}"
end

let block = ref false
let control = ref true
let dag_control = ref false
let block_edges = ref false
let block_edges_helpers = ref false

let specs = [
  "--block",
    Arg.Bool (fun b -> block := b),
    "draw basic blocks as clusters";
  "--control",
    Arg.Bool (fun b -> control := b),
    "draw explicit control flow based on instruction order (default)";
  "--dag-control",
    Arg.Bool (fun b -> dag_control := b),
    "analyze DAGs in a basic block and draw implied control flow among them (consider using --control=false)";
  "--block-edges",
    Arg.Bool (fun b -> block_edges := b),
    "(try to) draw inter-block edges between blocks, not between nodes";
  "--block-edges-helpers",
    Arg.Bool (fun b -> block_edges := b),
    "Add Graphviz-specific hacks to produce better layout";
]

let usage = Sys.argv.(0) ^ " <file.ll>"

let files = ref []
let _ = Arg.parse specs (fun s -> files := s :: !files) usage
let _ =
  if List.length !files <> 1
  then begin
    print_string "Wrong number of arguments\n";
    Arg.usage specs usage;
    exit 1
  end
  else ()

let _ =
  if not !control && not !dag_control
  then
    control := true

let mem = MemoryBuffer.of_file Sys.argv.(1);;
let context = create_context ();;
let modl = Llvm_irreader.parse_ir context mem;;

let _ = number_tmps modl

let _ =
  iter_functions (fun f ->
    if not (is_declaration f)
    then begin
      Printf.printf "Writing %s.dot\n" (value_name f);
      let out = open_out (Printf.sprintf "%s.dot" (value_name f)) in
      let options = {
        dag_control = !dag_control;
        block_edges = !block_edges;
        block_edges_helpers = !block_edges_helpers;
        block = !block;
        control = !control;
      } in
      let g = Graph.create f out options in
      Graph.render g
    end
  ) modl

