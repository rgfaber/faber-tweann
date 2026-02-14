# Makefile for faber-tweann
#
# Combines Erlang (rebar3) and Rust (cargo) build steps.
# The NIF is optional - pure Erlang fallback is available.

.PHONY: all compile nif test clean distclean dialyzer doc release help

# Directories
NATIVE_DIR = native
PRIV_DIR = priv
NIF_LIB = $(PRIV_DIR)/tweann_nif.so

# Default target
all: compile

# Compile Erlang code (will attempt NIF build via rebar3 hooks)
compile:
	rebar3 compile

# Build the Rust NIF explicitly
nif:
	@echo "Building Rust NIF..."
	@mkdir -p $(PRIV_DIR)
	cd $(NATIVE_DIR) && cargo build --release
	cp $(NATIVE_DIR)/target/release/libtweann_nif.so $(NIF_LIB)
	@echo "NIF built successfully: $(NIF_LIB)"

# Build NIF in debug mode (faster compilation, slower runtime)
nif-debug:
	@echo "Building Rust NIF (debug)..."
	@mkdir -p $(PRIV_DIR)
	cd $(NATIVE_DIR) && cargo build
	cp $(NATIVE_DIR)/target/debug/libtweann_nif.so $(NIF_LIB)
	@echo "NIF built (debug): $(NIF_LIB)"

# Run all tests
test: compile
	rebar3 eunit
	rebar3 ct

# Run only unit tests
eunit: compile
	rebar3 eunit

# Run only common tests
ct: compile
	rebar3 ct

# Run dialyzer for type checking
dialyzer: compile
	rebar3 dialyzer

# Generate documentation
doc: compile
	rebar3 ex_doc

# Clean build artifacts
clean:
	rebar3 clean
	rm -f $(NIF_LIB)
	@if [ -d $(NATIVE_DIR)/target ]; then \
		cd $(NATIVE_DIR) && cargo clean; \
	fi

# Deep clean including dependencies
distclean: clean
	rm -rf _build
	rm -rf $(PRIV_DIR)

# Prepare a release build
release: nif compile test dialyzer
	@echo "Release build complete"

# Check Rust code
check-rust:
	@if [ -d $(NATIVE_DIR) ]; then \
		cd $(NATIVE_DIR) && cargo check && cargo clippy; \
	else \
		echo "No native directory found"; \
	fi

# Format Rust code
fmt-rust:
	@if [ -d $(NATIVE_DIR) ]; then \
		cd $(NATIVE_DIR) && cargo fmt; \
	fi

# Benchmark the NIF
bench: nif compile
	@echo "Running NIF benchmark..."
	rebar3 shell --eval " \
		case tweann_nif:is_loaded() of \
			true -> \
				{ok, N, _} = network_compiler:compile_simple(10, [20, 20], 5), \
				Inputs = [rand:uniform() || _ <- lists:seq(1, 10)], \
				AvgUs = tweann_nif:benchmark_evaluate(N, Inputs, 10000), \
				io:format(\"Average evaluation time: ~.2f microseconds~n\", [AvgUs]); \
			false -> \
				io:format(\"NIF not loaded. Run 'make nif' first.~n\") \
		end, \
		halt(0). \
	"

# Show help
help:
	@echo "faber-tweann Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  all          - Compile Erlang code (default)"
	@echo "  compile      - Compile Erlang code"
	@echo "  nif          - Build Rust NIF (release mode)"
	@echo "  nif-debug    - Build Rust NIF (debug mode)"
	@echo "  test         - Run all tests (eunit + ct)"
	@echo "  eunit        - Run unit tests only"
	@echo "  ct           - Run common tests only"
	@echo "  dialyzer     - Run dialyzer type checking"
	@echo "  doc          - Generate documentation"
	@echo "  clean        - Clean build artifacts"
	@echo "  distclean    - Deep clean including deps"
	@echo "  release      - Full release build with tests"
	@echo "  check-rust   - Check and lint Rust code"
	@echo "  fmt-rust     - Format Rust code"
	@echo "  bench        - Run NIF benchmark"
	@echo "  help         - Show this help"
	@echo ""
	@echo "Note: The Rust NIF is optional. If not built, pure Erlang"
	@echo "      fallback will be used (slower but functional)."
