OCAMLOPTS=
LLVM_INC=-I +llvm-3.0
LLVM_CMA=llvm.cma llvm_bitreader.cma llvm_irreader.cma
OCAML_CMA=str.cma


%.cmo: %.ml
	ocamlc $(OCAMLOPTS) $(LLVM_INC) -c $<

%: %.cmo
	ocamlc -cc g++ $(LLVM_INC) $(OCAML_CMA) $(LLVM_CMA) $< -o $@

.PHONY: all clean

all: graph-llvm-ir

clean:
	rm -f *.cm* *.o test

graph-llvm-ir: graphLlvmIr
	cp $< $@
