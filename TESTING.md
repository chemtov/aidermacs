# Aidermacs Testing Guide

This document describes the testing infrastructure for aidermacs.

## Test Suite Overview

The aidermacs test suite consists of three main components:

1. **Compilation Tests** (`test-compile.el`) - Must run first
2. **Recording System Tests** (`test-recording-simple.el`)
3. **Template System Tests** (`test-template-system.el`)

## Running Tests

### Quick Start

Run all tests in the correct order:

```bash
./run-tests.sh
```

### Individual Test Suites

#### 1. Compilation Tests (Run First!)

The compilation tests **must** run before any other tests. They:
- Compile all elisp files to catch syntax errors
- Detect undefined functions and variables
- Verify all dependencies are properly declared
- Clean up all .elc files to prevent them from being committed

```bash
emacs --batch -l test-compile.el
```

**Why run this first?**
- Catches compilation errors that would cause runtime failures
- Ensures code quality before running functional tests
- Prevents .elc files from polluting the repository

#### 2. Recording System Tests

Tests for the asciicinema recording feature:

```bash
emacs --batch -l test-recording-simple.el
```

Tests include:
- Asciinema availability check
- Recording filename generation
- Directory creation
- Project name extraction
- Path validation

#### 3. Template System Tests

Tests for the template system:

```bash
emacs --batch -l aidermacs-templates.el -l test-template-system.el
```

Tests include:
- Placeholder extraction and replacement
- Template metadata parsing
- File operations
- Interactive editing
- Marginalia integration

## Test Files

### test-compile.el

Compilation test suite that:
- Compiles files in dependency order
- Provides stubs for optional dependencies (markdown-mode)
- Loads compiled files for subsequent compilations
- Tracks and cleans up all .elc files
- Reports detailed compilation results

**Exit codes:**
- 0: All files compiled successfully
- 1: One or more files failed to compile

### test-recording-simple.el

Unit tests for recording functions without requiring full aidermacs loading.

### test-template-system.el

Comprehensive tests for the template system including:
- Basic functionality tests
- Filesystem operations
- Metadata parsing
- Interactive features

## Continuous Integration

For CI/CD pipelines, use the test runner script:

```bash
#!/bin/bash
cd /path/to/aidermacs
./run-tests.sh
```

The script will:
1. Run compilation tests first
2. Exit immediately if compilation fails
3. Run remaining tests in order
4. Report overall success/failure

## Adding New Tests

### For New Features

1. Add unit tests to appropriate test file
2. Update `run-tests.sh` if needed
3. Document test coverage in this file

### For New Files

1. Add filename to `test-compile-files` list in `test-compile.el`
2. Ensure proper dependency order
3. Add any required stubs for optional dependencies

## Troubleshooting

### Compilation Tests Fail

**Problem:** Files fail to compile

**Solutions:**
1. Check for syntax errors in the reported file
2. Verify all `require` statements have corresponding files
3. Check for undefined functions (add `declare-function` if needed)
4. Ensure optional dependencies are properly handled

### .elc Files Left Behind

**Problem:** .elc files remain after tests

**Solutions:**
1. Check that `test-compile-cleanup` is being called
2. Verify `test-compile-elc-files` list is populated correctly
3. Manually clean up: `rm *.elc`

### Tests Pass Locally But Fail in CI

**Problem:** Tests work on your machine but not in CI

**Solutions:**
1. Ensure all dependencies are installed in CI environment
2. Check Emacs version compatibility
3. Verify file paths are correct
4. Check for environment-specific issues

## Best Practices

1. **Always run compilation tests first**
   - Catches errors early
   - Prevents wasted time on broken code

2. **Keep tests fast**
   - Use mocks for external dependencies
   - Avoid unnecessary file I/O
   - Clean up temporary files

3. **Test in isolation**
   - Each test should be independent
   - Don't rely on test execution order (except compilation)
   - Clean up after each test

4. **Document test coverage**
   - Add comments explaining what each test verifies
   - Update this document when adding new tests
   - Include examples of expected behavior

## Test Coverage

Current test coverage:

- ✅ Compilation of all elisp files
- ✅ Recording system functionality
- ✅ Template system functionality
- ✅ Placeholder extraction and replacement
- ✅ Metadata parsing
- ✅ File operations
- ⚠️  Integration tests (manual only)
- ⚠️  UI/UX tests (manual only)

## Future Improvements

- [ ] Add ERT-based tests for better integration
- [ ] Add coverage reporting
- [ ] Add performance benchmarks
- [ ] Add integration tests for full workflows
- [ ] Add tests for backend-specific functionality
- [ ] Add tests for transient menu interactions
