GHC ?= ghc

SRC = \
	Data/Rope.hs \
	Data/Rope/Seq.hs \
	Util/Benchmark.hs \
	Util/GetCPUTime.hs \
	cbits/getcputime.c

GHCFLAGS+=$(EXTRAGHCFLAGS)

GHCFLAGS+=-rtsopts -threaded -O2
#GHCFLAGS+=-prof -fprof-auto
#GHCFLAGS+=-eventlog

GHCFLAGS+= \
	-hide-all-packages \
	-package array \
	-package base \
	-package deepseq \
	-package random \
	-package time \
	-package vector

#GHCFLAGS+=-ddump-to-file
#GHCFLAGS+=-ddump-simpl
#GHCFLAGS+=-ddump-simpl-iterations
#GHCFLAGS+=-dsuppress-all -dppr-case-as-let -dppr-cols200

MONADPAR=../monad-par

.PHONY : all
all : map-tree map-rope

.PHONY : clean
clean:
	rm -rf map-rope map-tree obj

map-tree : examples/map-tree/Main.hs $(SRC)
	$(GHC) $(GHCFLAGS) --make $^ \
	    -odir obj/map-tree/$* -hidir obj/map-tree/$* \
	    -isrc/map-tree/$* -i$(MONADPAR) \
	    -o $@

map-rope : examples/map-rope/Main.hs $(SRC)
	$(GHC) $(GHCFLAGS) --make $^\
	    -odir obj/map-rope/$* -hidir obj/map-rope/$* \
	    -isrc/map-rope/$* -i$(MONADPAR) \
	    -o $@
