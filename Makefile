# Patryk Bundyra pb429159
# University of Warsaw 2023

GHC         = ghc
FLAGS		= -Wall -Werror -fno-warn-orphans -Wno-incomplete-patterns -Wno-name-shadowing
OUT_FILE 	= lagger

all:
	${GHC} ${FLAGS} -o ${OUT_FILE} Main.hs

clean :
	-rm -f Lagger/*.hi Lagger/*.o Lagger/*.log Lagger/*.aux Lagger/*.dvi
	-rm -f Evaluator/*.hi Evaluator/*.o
	-rm -f lagger *.hi *.o *.log *.aux *.dvi
	-rm -f Typechecker/*.hi Typechecker/*.o
	-rm -f Common/*.hi Common/*.o