## File generated by the BNF Converter (bnfc 2.9.4).

# Makefile for building the parser and test program.

GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : TestInstant

# Rules for building the parser.

AbsInstant.hs LexInstant.x ParInstant.y PrintInstant.hs TestInstant.hs : Instant.cf
	bnfc --haskell Instant.cf

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

TestInstant : AbsInstant.hs LexInstant.hs ParInstant.hs PrintInstant.hs TestInstant.hs
	${GHC} ${GHC_OPTS} $@

# Rules for cleaning generated files.

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi

distclean : clean
	-rm -f AbsInstant.hs AbsInstant.hs.bak ComposOp.hs ComposOp.hs.bak DocInstant.txt DocInstant.txt.bak ErrM.hs ErrM.hs.bak LayoutInstant.hs LayoutInstant.hs.bak LexInstant.x LexInstant.x.bak ParInstant.y ParInstant.y.bak PrintInstant.hs PrintInstant.hs.bak SkelInstant.hs SkelInstant.hs.bak TestInstant.hs TestInstant.hs.bak XMLInstant.hs XMLInstant.hs.bak ASTInstant.agda ASTInstant.agda.bak ParserInstant.agda ParserInstant.agda.bak IOLib.agda IOLib.agda.bak Main.agda Main.agda.bak Instant.dtd Instant.dtd.bak TestInstant LexInstant.hs ParInstant.hs ParInstant.info ParDataInstant.hs Makefile


# EOF
