## see: http://www.ocaml.info/home/ocaml_sources.html#toc16

## put here the names of your source files (in the right order)
SOURCES = \
		dataStructures/binomialtree.ml dataStructures/largeArray.ml dataStructures/memoryManagement.ml dataStructures/longString.ml dataStructures/heap.ml dataStructures/safeHeap.ml \
		dataStructures/tools.ml dataStructures/cache.ml main/parameter.ml dataStructures/mapExt.ml dataStructures/valMap.ml dataStructures/dynamicArray.ml dataStructures/mods.ml dataStructures/random_tree.ml \
		Error/debug.ml Error/ExceptionDefn.ml dataStructures/liftSet.ml \
		grammar/ast.ml grammar/kappaParser.mly grammar/kappaLexer.mll \
		pattern/signature.ml pattern/environment.ml \
		siteGraphs/node.ml pattern/mixture.mli  \
		pattern/mixture.ml pattern/precondition.ml pattern/precondition.mli \
		siteGraphs/graph.ml siteGraphs/species.ml pattern/matching.ml compartment/diffusion.ml pattern/dynamics.ml \
		simulation/state.ml simulation/nonLocal.ml grammar/eval.ml simulation/external.ml  \
		cflow/cflow_handler.ml cflow/profiling.ml cflow/causal.ml  \
		cflow/kappa_instantiation.ml cflow/po_cut.ml cflow/pseudo_inverse.ml cflow/blackboard_generation.ml cflow/blackboard.ml cflow/propagation_heuristics.ml \
		cflow/generic_branch_and_cut_solver.ml cflow/dag.ml cflow/compression_main.ml  simulation/plot.ml Error/safe.ml simulation/run.ml Compartment/vol.ml simulation/scheduler.ml \
		main/main.ml 

## the name of the resulting executable
ifeq (Windows,$(findstring Windows,$(OS)))
	RESULT = KaSim.exe
else
	RESULT = KaSim
endif

## generate type information (.annot files)
ANNOTATE = no

## make target (see manual) : byte-code, debug-code, native-code, ...
all: native-code
debug: debug-code

PREFIX? = 

ifeq ($(wildcard $(PREFIX)/menhir),) 
    OCAMLYACC = $(PREFIX)/ocamlyacc ;
else 
    OCAMLYACC = $(PREFIX)/menhir
endif

OCAMLCP = $(PREFIX)/ocamlcp
OCAMLLEX = $(PREFIX)/ocamllex
OCAMLC = $(PREFIX)/ocamlc.opt 
OCAMLOPT = $(PREFIX)/ocamlopt.opt #-g -ccopt -g -ccopt -pg
OCAMLDEP = $(PREFIX)/ocamldep
CC = gcc

include OCamlMakefile
