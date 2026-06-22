# sml-ssg build
#
#   make            build the test binary with MLton (default)
#   make test       build + run tests under MLton
#   make test-poly  run tests under Poly/ML (use-and-run; no link step)
#   make all-tests  run the suite under both compilers
#   make example    build + run the demo
#   make clean      remove build artifacts
#
# Layout B (dependent): own sources live in src/; sml-markdown and sml-template
# are vendored under lib/ (sharing one copy of sml-html + sml-buffer) and loaded
# first, then the SSG pipeline.

MLTON      ?= mlton
POLY       ?= poly
BIN        := bin
BUFFERDIR  := lib/github.com/sjqtentacles/sml-buffer
HTMLDIR    := lib/github.com/sjqtentacles/sml-html
MDDIR      := lib/github.com/sjqtentacles/sml-markdown
TPLDIR     := lib/github.com/sjqtentacles/sml-template
TEST_MLB   := test/test.mlb
SRCS       := $(wildcard $(BUFFERDIR)/* $(HTMLDIR)/* $(MDDIR)/* $(TPLDIR)/* src/* test/*.sml) $(TEST_MLB)

.PHONY: all test poly test-poly all-tests example clean

all: $(BIN)/test-mlton

example: $(BIN)/demo
	./$(BIN)/demo

$(BIN)/demo: $(SRCS) examples/demo.sml examples/sources.mlb | $(BIN)
	$(MLTON) -output $@ examples/sources.mlb

$(BIN)/test-mlton: $(SRCS) | $(BIN)
	$(MLTON) -output $@ $(TEST_MLB)

test: $(BIN)/test-mlton
	$(BIN)/test-mlton

# Poly/ML has no native .mlb support; the suite runs at top level and exits on
# its own. Load the vendored sources in dependency order (sml-buffer, sml-html,
# sml-markdown, sml-template -- sml-html loaded ONCE), then the SSG pipeline,
# then the test driver.
poly test-poly:
	printf 'use "$(BUFFERDIR)/buffer.sig";\nuse "$(BUFFERDIR)/buffer.sml";\nuse "$(HTMLDIR)/escape.sig";\nuse "$(HTMLDIR)/escape.sml";\nuse "$(HTMLDIR)/html.sig";\nuse "$(HTMLDIR)/html.sml";\nuse "$(MDDIR)/markdown.sig";\nuse "$(MDDIR)/markdown.sml";\nuse "$(TPLDIR)/template.sig";\nuse "$(TPLDIR)/template.sml";\nuse "src/ssg.sig";\nuse "src/ssg.sml";\nuse "test/harness.sml";\nuse "test/test.sml";\nuse "test/entry.sml";\nuse "test/main.sml";\n' | $(POLY) -q --error-exit

all-tests: test test-poly

$(BIN):
	mkdir -p $(BIN)

clean:
	rm -f $(BIN)/test-mlton $(BIN)/demo
