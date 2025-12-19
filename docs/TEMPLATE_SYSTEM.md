# Aidermacs Template System

## Overview

The Aidermacs template system allows you to create reusable prompt templates with placeholders that can be filled in interactively. This is perfect for common workflows, complex prompts, or standardized interactions with the LLM.

## Features

- **Simple Text Files**: Templates are stored as plain text files in a configurable directory
- **Placeholder Support**: Use `{Placeholder-Text}` syntax for dynamic inputs
- **Multiple Placeholders**: Each template can have multiple placeholders
- **Interactive Replacement**: Placeholders are replaced via `completing-read` prompts
- **Full Integration**: Works seamlessly with aidermacs command sending infrastructure

## Installation and Configuration

The template system is automatically loaded with aidermacs. No special setup is needed.

### Template Locations

Aidermacs searches for templates in two locations:

1.  **Default Templates**: A set of example templates is bundled with the aidermacs package. These are located in a `templates` subdirectory within the package installation directory. These files should not be edited directly as they may be overwritten during updates.

2.  **User Templates**: Your personal templates are stored in `~/.emacs.d/aidermacs-templates/`. This is the place to create, edit, and manage your own templates. If a user template has the same name as a default template, the user's version will be used.

### Example `use-package` setup

If you are using `use-package`, here is a sample configuration. This setup ensures that the bundled templates are correctly located.

```emacs-lisp
(use-package aidermacs
  ;; Example for straight.el users:
  ;; :straight (aidermacs :type git :host github :repo "MatthewZMD/aidermacs")
  :config
  ;; Your aidermacs configuration here
  )
```

## Usage

### Creating Templates

#### Method 1: Interactive Creation

1. Press `N` in the aidermacs transient menu (or `M-x aidermacs-create-template`)
2. Enter a template name (e.g., "fetch-and-analyze")
3. Enter the template content with placeholders

Example:
```
/web {Enter-URL}

Please analyze the content and {What-to-do-with-it}
```

#### Method 2: Manual File Creation

Create a `.txt` file in your templates directory:

```bash
# File: ~/.emacs.d/aidermacs-templates/my-template.txt
/code Implement {Feature-Name} with the following requirements:
- {Requirement-1}
- {Requirement-2}
- Use {Technology-Stack}
```

### Using Templates

1. Press `P` in the aidermacs transient menu (or `M-x aidermacs-use-template`)
2. Select a template from the list
3. Fill in each placeholder when prompted
4. The processed template is automatically sent to aidermacs

### Managing Templates

- **Edit Template**: Press `E` in the transient menu
- **Delete Template**: Press `D` in the transient menu
- **Open Templates Directory**: Press `O` in the transient menu

## Placeholder Syntax

Placeholders use the format `{Prompt-Text}` where:
- The text between braces becomes the prompt shown to the user
- Use descriptive names like `{Enter-URL}` or `{Feature-Description}`
- Placeholders are case-sensitive
- Duplicate placeholders are only prompted once

### Examples

```
{Enter-URL}           → Prompts: "Enter-URL: "
{Feature-Name}        → Prompts: "Feature-Name: "
{Programming-Language} → Prompts: "Programming-Language: "
```

## Example Templates

### 1. Fetch and Analyze URL

**File**: `fetch-and-analyze-url.txt`
```
/web {Enter-URL}

Please analyze the content from the URL above and {What-to-do-with-it}
```

**Usage**: Fetch web content and perform custom analysis

### 2. Implement Feature

**File**: `implement-feature.txt`
```
/architect Please implement the following feature: {Feature-Description}

Requirements:
- {Requirement-1}
- {Requirement-2}
- Follow best practices for {Programming-Language}
- Include appropriate error handling
- Add comments explaining complex logic
```

**Usage**: Structured feature implementation with requirements

### 3. Code Review

**File**: `code-review.txt`
```
/ask Please review the code in {File-Name} focusing on:

1. Code quality and readability
2. Potential bugs or edge cases
3. Performance considerations
4. Security concerns
5. {Additional-Focus-Area}

Provide specific suggestions for improvement.
```

**Usage**: Comprehensive code review with custom focus areas

### 4. Refactor Code

**File**: `refactor-code.txt`
```
/code Refactor the code to {Refactoring-Goal}

Please ensure:
- Maintain existing functionality
- Improve code readability
- Follow {Coding-Standard} standards
- Update any related documentation
```

**Usage**: Guided code refactoring

### 5. Debug Issue

**File**: `debug-issue.txt`
```
/ask I'm experiencing the following issue: {Issue-Description}

Error message: {Error-Message}

Expected behavior: {Expected-Behavior}

Please help me identify the root cause and suggest a fix.
```

**Usage**: Structured debugging assistance

## Advanced Usage

### Complex Templates

You can create sophisticated templates with multiple sections:

```
/architect Design and implement a {Component-Type} for {Project-Name}

## Context
{Project-Context}

## Requirements
1. {Requirement-1}
2. {Requirement-2}
3. {Requirement-3}

## Technical Constraints
- Must use {Technology-1}
- Should integrate with {Technology-2}
- Performance target: {Performance-Target}

## Additional Notes
{Additional-Notes}
```

### Combining with Aider Commands

Templates can use any aider command:
- `/code` - Make code changes
- `/ask` - Ask questions
- `/architect` - Design solutions
- `/web` - Fetch web content
- `/help` - Get help

### Best Practices

1. **Descriptive Placeholder Names**: Use clear, self-explanatory names
   - Good: `{API-Endpoint-URL}`
   - Bad: `{url1}`

2. **Structured Templates**: Organize complex prompts with sections
   ```
   ## Context
   {Context}

   ## Goal
   {Goal}

   ## Constraints
   {Constraints}
   ```

3. **Reusable Patterns**: Create templates for recurring tasks
   - API integration
   - Database schema changes
   - Test generation
   - Documentation updates

4. **Version Control**: Keep your templates in version control
   ```bash
   cd ~/.emacs.d/aidermacs-templates
   git init
   git add .
   git commit -m "Initial templates"
   ```

## Configuration

### Custom User Template Directory

You can customize the user templates directory:

```elisp
(setq aidermacs-user-templates-directory "~/my-aider-templates")
```

### Custom File Extension

```elisp
(setq aidermacs-templates-file-extension ".template")
```

## Keybindings

In the aidermacs transient menu:

- `P` - Use a template
- `N` - Create a new template
- `E` - Edit an existing template
- `D` - Delete a template
- `O` - Open templates directory

## Troubleshooting

### Templates Not Showing Up

1. Check the templates directory exists:
   ```elisp
   M-x aidermacs-open-templates-directory
   ```

2. Verify file extension matches:
   ```elisp
   M-: aidermacs-templates-file-extension
   ```

3. Ensure files have correct extension (default: `.txt`)

### Placeholders Not Being Replaced

1. Check placeholder syntax: `{Text}` not `{{Text}}` or `[Text]`
2. Ensure no extra spaces: `{Name}` not `{ Name }`
3. Verify braces are properly matched

## API Reference

### Functions

- `aidermacs-use-template` - Select and use a template
- `aidermacs-create-template` - Create a new template interactively
- `aidermacs-edit-template` - Edit an existing template
- `aidermacs-delete-template` - Delete a template
- `aidermacs-open-templates-directory` - Open templates directory in Dired

### Variables

- `aidermacs-templates-directory` - Directory for template files
- `aidermacs-templates-file-extension` - File extension for templates (default: ".txt")

## Contributing Templates

Share your useful templates with the community! Consider:

1. Creating a personal template repository
2. Contributing examples to the aidermacs project
3. Sharing templates in discussions or issues

## Future Enhancements

Potential future features:
- Template categories/tags
- Template variables with default values
- Template snippets/includes
- Template validation
- Template marketplace/sharing

## License

The template system is part of aidermacs and follows the same Apache-2.0 license.
