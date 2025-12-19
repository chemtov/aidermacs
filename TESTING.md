# Aidermacs Testing Guide

This document describes the testing infrastructure for aidermacs using Eask and ERT.

## Test Suite Overview

The aidermacs test suite uses **Eask** for dependency management and test execution, with **ERT** (Emacs Lisp Regression Testing) as the testing framework.

### Test Files

All tests are located in the `test/` directory:

1. **aidermacs-compile-test.el** - Byte-compilation tests
2. **aidermacs-eval-test.el** - Eval-buffer tests (macro expansion, runtime errors)
3. **aidermacs-recording-test.el** - Recording system tests
4. **aidermacs-templates-test.el** - Template system tests

## Prerequisites

### Install Eask

Eask is required to run the tests. Install it using npm:

```bash
npm install -g @emacs-eask/cli
```

Or download binaries from the [Eask releases page](https://github.com/emacs-eask/cli/releases).

Verify installation:

```bash
eask --version
```

## Running Tests

### Quick Start

Run all tests using the provided script:

```bash
./run-eask-tests.sh
```

This script will:
1. Install all dependencies automatically
2. Run all ERT tests in the `test/` directory
3. Report results

### Manual Test Execution

You can also run tests manually using Eask:

```bash
# Install dependencies first
eask install-deps --dev

# Run all tests
eask test ert test/*.el

# Run specific test file
eask test ert test/aidermacs-templates-test.el
```

### Individual Test Suites

#### 1. Compilation Tests

Tests that compile all elisp files to catch syntax errors:

```bash
eask test ert test/aidermacs-compile-test.el
```

**Why run this first?**
- Catches compilation errors that would cause runtime failures
- Ensures code quality before running functional tests
- Prevents .elc files from polluting the repository

#### 2. Eval-Buffer Tests

Tests that evaluate each file to catch macro expansion and runtime errors:

```bash
eask test ert test/aidermacs-eval-test.el
```

**What it catches:**
- Macro expansion errors (e.g., `transient-define-prefix` issues)
- Runtime evaluation errors
- `defcustom` validation errors
- Autoload issues
- Feature loading problems

**How it works:**
- Eask automatically installs dependencies (transient, compat, markdown-mode)
- Caches packages in `.eask/` sandbox for fast subsequent runs
- Evaluates each file using `load-file`
- First run takes ~30-60s (downloads packages), subsequent runs ~5-10s

**Why it's important:**  
Byte-compilation doesn't catch everything! For example, this compiles fine but fails at eval:

```elisp
(transient-define-prefix my-menu ()
  "Menu"
  [["Section"
    ("a" "Action" missing-function)]])  ; Error at eval time!
```

#### 3. Recording System Tests

Tests for the asciicinema recording feature:

```bash
eask test ert test/aidermacs-recording-test.el
```

Tests include:
- Asciinema availability check
- Recording filename generation
- Directory creation
- Project name extraction
- Path validation

#### 4. Template System Tests

Tests for the template system:

```bash
eask test ert test/aidermacs-templates-test.el
```

Tests include:
- Placeholder extraction and replacement
- Template metadata parsing
- File operations
- Interactive editing
- Marginalia integration

## Test Organization

All tests use the ERT (Emacs Lisp Regression Testing) framework and are located in the `test/` directory.

### test/aidermacs-compile-test.el

Compilation test suite that:
- Compiles files in dependency order
- Provides stubs for optional dependencies (markdown-mode)
- Loads compiled files for subsequent compilations
- Tracks and cleans up all .elc files
- Reports detailed compilation results

### test/aidermacs-eval-test.el

Eval-buffer test suite that:
- Evaluates each file using `load-file`
- Catches macro expansion and runtime errors
- Tests in isolated Eask sandbox
- Reports detailed evaluation results

### test/aidermacs-recording-test.el

Unit tests for recording functions:
- Asciinema availability checks
- Filename generation
- Directory creation
- Path validation

### test/aidermacs-templates-test.el

Comprehensive tests for the template system:
- Basic functionality tests
- Filesystem operations
- Metadata parsing
- Interactive features
- Placeholder handling

## Continuous Integration

For CI/CD pipelines, use the test runner script:

```bash
#!/bin/bash
cd /path/to/aidermacs
./run-eask-tests.sh
```

The script will:
1. Install dependencies automatically
2. Run all ERT tests
3. Report overall success/failure

## Adding New Tests

### For New Features

1. Add unit tests to appropriate test file
2. Update `run-eask-tests.sh` if needed
3. Document test coverage in this file

### For New Files

1. Add filename to `aidermacs-compile-test-files` list in `test/aidermacs-compile-test.el`
2. Add filename to `aidermacs-eval-test-files` list in `test/aidermacs-eval-test.el`
3. Ensure proper dependency order
4. Add any required stubs for optional dependencies

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
1. Check that `aidermacs-compile-test-cleanup` is being called
2. Verify `aidermacs-compile-test-elc-files` list is populated correctly
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
- ✅ Eval-buffer testing (macro expansion, runtime errors)
- ✅ Recording system functionality
- ✅ Template system functionality
- ✅ Placeholder extraction and replacement
- ✅ Metadata parsing
- ✅ File operations
- ⚠️  Integration tests (manual only)
- ⚠️  UI/UX tests (manual only)

## Additional Resources

- [Eask Usage Guide](docs/EASK_USAGE.md) - Detailed Eask documentation
- [Eask Official Documentation](https://emacs-eask.github.io/)
- [ERT Manual](https://www.gnu.org/software/emacs/manual/html_mono/ert.html)

## Future Improvements

- [ ] Add coverage reporting
- [ ] Add performance benchmarks
- [ ] Add integration tests for full workflows
- [ ] Add tests for backend-specific functionality
- [ ] Add tests for transient menu interactions
