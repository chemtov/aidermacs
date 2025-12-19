#!/bin/bash
# run-tests.sh - Run all aidermacs tests in the correct order

set -e  # Exit on first error

echo "========================================="
echo "Running Aidermacs Test Suite"
echo "========================================="
echo ""

# Test 1: Compilation tests (must run first)
echo "Step 1: Running compilation tests..."
emacs --batch -l test-compile.el
if [ $? -ne 0 ]; then
    echo "❌ Compilation tests failed!"
    exit 1
fi
echo ""

# Test 2: Eval-buffer tests
echo "Step 2: Running eval-buffer tests..."
emacs --batch -l test-eval.el
if [ $? -ne 0 ]; then
    echo "❌ Eval-buffer tests failed!"
    exit 1
fi
echo ""

# Test 3: Recording system tests
echo "Step 3: Running recording system tests..."
emacs --batch -l test-recording-simple.el
if [ $? -ne 0 ]; then
    echo "❌ Recording system tests failed!"
    exit 1
fi
echo ""

echo "Step 4: Running template system tests..."
emacs --batch -l aidermacs-templates.el -l test-template-system.el
if [ $? -ne 0 ]; then
    echo "❌ Template system tests failed!"
    exit 1
fi
echo ""

echo "========================================="
echo "✅ All tests passed successfully!"
echo "========================================="