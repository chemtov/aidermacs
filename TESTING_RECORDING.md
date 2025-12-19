# Testing Plan for Asciicinema Recording Feature

## Unit Tests ✓

All unit tests pass successfully:
- Test 1: asciinema availability check
- Test 2: Recording filename generation
- Test 3: Recording directory creation
- Test 4: Project name in filename
- Test 5: Absolute path verification
- Test 6: Unique filename generation

Run unit tests with:
```bash
emacs --batch -l test-recording-simple.el
```

## Integration Test Plan

### Prerequisites
1. Install asciinema: `pip install asciinema` or `brew install asciinema`
2. Set `aidermacs-backend` to `'vterm`
3. Ensure vterm package is installed

### Manual Integration Test Steps

#### Test 1: Basic Recording Session
1. Start Emacs
2. Run `M-x aidermacs-transient-menu`
3. Press `A` to start session with recording
4. Verify message shows recording file path
5. Send a simple command like `/ask what is 2+2?`
6. Wait for response
7. Run `/exit` to end session
8. Check that .cast file exists in `~/Videos` (or configured directory)
9. Verify .cast file is valid JSON:
   ```bash
   head -1 ~/Videos/<project>-<timestamp>.cast | python -m json.tool
   ```

#### Test 2: Recording File Format
1. After Test 1, inspect the .cast file
2. Verify first line is JSON header with version, width, height
3. Verify subsequent lines are event arrays [time, code, data]
4. Play back recording:
   ```bash
   asciinema play ~/Videos/<project>-<timestamp>.cast
   ```

#### Test 3: Transient Menu Integration
1. Start Emacs
2. Run `M-x aidermacs-transient-menu`
3. Verify "A" key shows "Start with Recording (NOT RECORDING)"
4. Start recording session with `A`
5. Run transient menu again
6. Verify "A" key now shows "Recording Session (RECORDING)"
7. Verify recording indicator shows session is active

#### Test 4: Mutual Exclusion
1. Start a regular session with `a`
2. Try to start recording session with `A`
3. Verify it switches to existing session (doesn't start new recording)
4. Exit session
5. Start recording session with `A`
6. Try to start regular session with `a`
7. Verify it switches to recording session

#### Test 5: Error Handling
1. Uninstall asciinema temporarily
2. Try to start recording session
3. Verify error message: "Asciinema is not installed..."
4. Reinstall asciinema
5. Set `aidermacs-backend` to `'comint`
6. Try to start recording session
7. Verify error message: "Recording is only supported with vterm backend..."

#### Test 6: Custom Recording Directory
1. Set `aidermacs-recording-directory` to a custom path
2. Start recording session
3. Verify .cast file is created in custom directory
4. Verify directory is created if it doesn't exist

#### Test 7: Project Name in Filename
1. Navigate to different projects
2. Start recording sessions in each
3. Verify each .cast file has correct project name prefix
4. Verify timestamp format is YYYYMMDD-HHMMSS

### Expected Results

All tests should pass with:
- ✓ Recording files created successfully
- ✓ Valid .cast file format
- ✓ Correct filename format: `<project>-<timestamp>.cast`
- ✓ Transient menu shows correct status
- ✓ Proper error messages for missing dependencies
- ✓ Recording captures entire aider session
- ✓ Recording stops when aider exits

### Known Limitations

1. Recording only works with vterm backend
2. Comint backend is not supported (would require different implementation)
3. Recording file is not automatically cleaned up if session crashes
4. No way to pause/resume recording mid-session

### Future Enhancements

1. Add option to automatically upload recordings to asciinema.org
2. Add option to compress recordings with gzip
3. Add option to trim idle time from recordings
4. Add support for comint backend
5. Add cleanup of old recordings
