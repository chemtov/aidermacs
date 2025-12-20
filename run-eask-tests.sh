#!/bin/bash
# run-eask-tests.sh - Run all aidermacs tests using Eask

set -e  # Exit on first error

echo "========================================="
echo "Running Aidermacs Test Suite with Eask"
echo "========================================="
echo ""

# Ensure dependencies are installed
echo "Installing dependencies..."
eask install-deps --dev
echo ""

# Run all ERT tests
echo "Running ERT tests..."
eask test ert test/*.el

echo ""
echo "========================================="
echo "✅ All tests passed successfully!"
echo "========================================="
