# Aidermacs Development Guide

## Development Setup with Straight.el

### Problem: Templates Directory Not Found in Build

When developing aidermacs with straight.el, the templates are in the **repos** directory (`~/.config/emacs.d/straight/repos/aidermacs/templates/`) but the package is loaded from the **build** directory (`~/.config/emacs.d/straight/build/aidermacs/`). By default, straight.el doesn't copy the `templates/` directory to the build location.

### Solution: Configure Straight Recipe

Update your straight.el recipe to include the templates directory **with proper directory structure**:

```elisp
(use-package aidermacs
  :straight (aidermacs
             :type git
             :host github
             :repo "chemtov/aidermacs"  ; Your fork
             :branch "main"
             :files (:defaults ("templates" "templates/*")))
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  ;; Your configuration here
  )
```

**Important**: The nested list format `("templates" "templates/*")` is crucial! This tells straight.el to:
1. Create a `templates/` subdirectory in the build directory
2. Copy all files from `repos/aidermacs/templates/*` into `build/aidermacs/templates/`

The simpler format `"templates/*.txt"` would flatten the structure and put files directly in the build root.

### Straight.el Commands for Development

After updating the recipe:

```elisp
;; 1. Rebuild the package to copy templates to build directory
M-x straight-rebuild-package RET aidermacs RET

;; 2. If you've made changes to templates in the repos directory
M-x straight-rebuild-package RET aidermacs RET

;; 3. To pull latest changes from your fork
M-x straight-pull-package RET aidermacs RET

;; 4. To rebuild all packages (nuclear option)
M-x straight-rebuild-all RET

;; 5. To check the build directory contents
M-x dired RET ~/.config/emacs.d/straight/build/aidermacs/ RET
```

### Verifying the Fix

After rebuilding, run the diagnostic:

```elisp
M-x aidermacs-templates-diagnose
```

You should see:
```
Default templates directory: /home/user/.config/emacs.d/straight/build/aidermacs/templates
  Exists: t
  Found 5 default templates: (code-review debug-issue fetch-and-analyze-url implement-feature refactor-code)
```

### Development Workflow

1. **Make changes** in `~/.config/emacs.d/straight/repos/aidermacs/`
2. **Rebuild** with `M-x straight-rebuild-package RET aidermacs RET`
3. **Test** the changes in your Emacs session
4. **Commit** changes in the repos directory
5. **Push** to your fork

### Alternative: Symlink for Development

If you're actively developing and don't want to rebuild constantly:

```bash
cd ~/.config/emacs.d/straight/build/aidermacs/
ln -s ../../repos/aidermacs/templates templates
```

This creates a symlink so changes to templates in repos are immediately available in build.

### Recipe Files Specification

The `:files` specification in the recipe uses patterns. **Critical**: To preserve directory structure, use nested lists!

**Correct (preserves directory structure)**:
```elisp
:files (:defaults ("templates" "templates/*"))
```
This creates `build/aidermacs/templates/` and copies files into it.

**Incorrect (flattens directory structure)**:
```elisp
:files ("*.el" "templates/*.txt")
```
This puts template files directly in `build/aidermacs/` root!

**How it works**:
- `:defaults` - Includes all `.el` files (equivalent to `"*.el"`)
- `("templates" "templates/*")` - Nested list format:
  - First element `"templates"` - Creates the subdirectory
  - Second element `"templates/*"` - Specifies which files to copy into it

### Troubleshooting

**Templates still not found after rebuild?**

1. Check the build directory:
   ```bash
   ls -la ~/.config/emacs.d/straight/build/aidermacs/templates/
   ```

2. Verify the recipe is correct:
   ```elisp
   M-x straight-get-recipe RET aidermacs RET
   ```

3. Force a clean rebuild:
   ```elisp
   M-x straight-remove-package RET aidermacs RET
   M-x straight-use-package RET aidermacs RET
   ```

**locate-library returns nil?**

This is normal during development. The diagnostic uses multiple fallback methods to find templates.

## Testing

### Running Tests

```bash
cd ~/.config/emacs.d/straight/repos/aidermacs
emacs --batch -l aidermacs-templates.el -l test-template-system.el
```

### Adding New Tests

Tests are in `test-template-system.el`. Add new tests following the existing pattern:

```elisp
;; Test N: Description
(let ((template "test content"))
  (let ((result (aidermacs-templates--some-function template)))
    (message "Test N - Description: %s" result)
    (cl-assert (equal result expected-value))))
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Run tests to ensure nothing breaks
5. Update documentation if needed
6. Submit a pull request

## Release Process

1. Update version numbers in all `.el` files
2. Update CHANGELOG.md
3. Tag the release: `git tag -a v1.x.x -m "Release v1.x.x"`
4. Push tags: `git push origin v1.x.x`
5. Create GitHub release with notes
