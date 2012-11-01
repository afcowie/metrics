all: build-core

BUILDDIR = /tmp/build/statistics

ifdef V
MAKEFLAGS=-R
else
MAKEFLAGS=-s -R
REDIRECT=>/dev/null
endif

.PHONY: all dirs test build-core build-tests run-data

#
# Disable missing signatures so that you can actually do development and
# let type-inference get on with things without Haskell bothering you.
# Likewise ignore unused functions since they're usually there while exploring
# various alternative implementations of a function.
#

GHC=ghc \
	-Wall \
	-Werror \
	-fwarn-tabs \
	-fno-warn-missing-signatures \
	-fno-warn-unused-binds

CORE_SOURCES=$(shell find src -name '*.hs')
TEST_SOURCES=$(shell find tests -name '*.hs')


dirs: $(BUILDDIR)/.dir

$(BUILDDIR)/.dir:
	@echo "MKDIR\t$(BUILDDIR)"
	mkdir -p $(BUILDDIR)
	@echo "MKDIR\t$(BUILDDIR)/core"
	mkdir $(BUILDDIR)/core
	@echo "MKDIR\t$(BUILDDIR)/tests"
	mkdir $(BUILDDIR)/tests
	touch $(BUILDDIR)/.dir

#
# Build core program. If using inotify, invoke using:
#
# $ inotifymake -- ./collector
#

build-core: dirs $(BUILDDIR)/core/collector.bin collector

$(BUILDDIR)/core/collector.bin: $(CORE_SOURCES)
	@echo "GHC\t$@"
	$(GHC) --make -O -threaded  \
		-prof -fprof-auto \
		-outputdir $(BUILDDIR)/core \
		-i"$(BUILDDIR):src" \
		-o $@ \
		src/Collector.hs
	@echo "STRIP\t$@"
	strip $@

collector:
	@echo "LN -s\t$@"
	ln -s $(BUILDDIR)/core/collector.bin $@

build-snippet: dirs $(BUILDDIR)/snippet/snippet.bin snippet

$(BUILDDIR)/snippet/snippet.bin: $(CORE_SOURCES)
	@echo "MKDIR\t$(BUILDDIR)/snippet"
	-mkdir $(BUILDDIR)/snippet
	@echo "GHC\t$@"
	$(GHC) --make -O -threaded  \
		-prof -fprof-auto \
		-outputdir $(BUILDDIR)/snippet \
		-i"$(BUILDDIR):src" \
		-o $@ \
		src/Snippet.hs
	@echo "STRIP\t$@"
	strip $@

snippet:
	@echo "LN -s\t$@"
	ln -s $(BUILDDIR)/snippet/snippet.bin $@

#
# Build test suite code
#

build-tests: dirs $(BUILDDIR)/tests/check.bin check

$(BUILDDIR)/tests/check.bin: $(CORE_SOURCES) $(TEST_SOURCES)
	hasktags -cx .
	@echo "GHC\t$@"
	$(GHC) --make -O -threaded  \
		-prof -fprof-auto \
		-outputdir $(BUILDDIR)/tests \
		-i"$(BUILDDIR):src:tests" \
		-o $@ \
		tests/Check.hs
	@echo "STRIP\t$@"
	strip $@

check:
	@echo "LN -s\t$@"
	ln -s $(BUILDDIR)/tests/check.bin $@

#
# Run tests directly. If using inotify, invoke instead as follows:
#
# $ inotifymake build-tests -- ./check
#

test: build-tests run-data
	@echo "EXEC\tcheck"
	$(BUILDDIR)/tests/check.bin

data: run-data
run-data: tests/redis.pid
tests/redis.pid:
	tests/start.sh

clean: 
	@echo "RM\ttemp files"
	-rm -f *.hi *.o collector snippet check tags
	-rm -rf $(BUILDDIR)
	tests/stop.sh
	-rm -f tests/dump.rdb
