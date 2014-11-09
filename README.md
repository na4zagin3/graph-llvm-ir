graph-llvm-ir
=============

This is a rewriting of https://github.com/pfalcon/graph-llvm-ir with OCaml for a environment where llvmpy doesn’t exists.

Original README
---------------

(Useful) combinations of rendering options:

--control (default):

Renders both explicit control flow present in LLVM IR (sequential
between statements in basic blocks, jumps between basic blocks),
and dataflow dependencies. Control flow has higher weight (that
means that control flow edges tend to be more straight).

--dag-control:

Ignore explicit flow control present in LLVM IR and instead compute
order of evaluation of independent (i.e. disconnected) dataflow DAGs
within basic block. Root node of a DAG consider to be an instruction
of type void. (The idea is that void instruction is executed solely
for side effect, and then it must be last instruction in evaluation of
some DAG, i.e. its root. This is clearly a heuristic, which needs to
be tested on various inputs yet.)


--block

For both options above, you can add --block to clusterize
instructions of the same basic block together within a rectangle
block. This seems like natural way to do it, but leaves questions
open where to put leaves of dataflow graphs (i.e. variables, constants,
etc.) So far, these are rendered as DAG structure also, which means
they are not part of any basic block cluster. But rendering them
in such way leads to edges going from different basic blocks to the
same leaf nodes, leading to a mess in the graph. Possible other
options: duplicate leaf nodes; don't render at all (can be kinda
assumed).

--block-edges

This makes control edges between basic blocks actually go between
basic blocks, not specific instructions in them. This may be useful
for some kinds of presentations. This also removes extra nodes
to represent labels. Results of the latter changes are mixed though,
it leads to not ideal placing of leaf non-cluster nodes and thus
deformed graphs.
