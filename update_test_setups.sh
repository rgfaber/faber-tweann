#!/bin/bash
# Update all test files to register example morphologies

# List of test files that need morphology registration
FILES=(
    "test/unit/constructor_tests.erl"
    "test/unit/genome_mutator_tests.erl"
    "test/unit/crossover_tests.erl"
    "test/unit/species_identifier_tests.erl"
    "test/unit/fitness_postprocessor_tests.erl"
    "test/genome_mutator_logging_test.erl"
    "test/process_safety_test.erl"
)

for file in "${FILES[@]}"; do
    if [ -f "$file" ]; then
        # Check if file has a setup() function
        if grep -q "^setup() ->" "$file"; then
            echo "Updating $file..."
            # Insert morphology registration at start of setup()
            sed -i '/^setup() ->$/a\    %% Ensure application started (for morphology registry)\n    application:ensure_all_started(faber_tweann),\n    %% Register example morphologies\n    test_helper:register_all_example_morphologies(),' "$file"
        fi
    fi
done

echo "Test setup updates complete!"
