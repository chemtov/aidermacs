# Aidermacs Templates - Quick Start Guide

## 🚀 Quick Start (30 seconds)

### Use a Template
1. Open aidermacs transient menu: `M-x aidermacs-transient-menu`
2. Press `P` (Use Template)
3. Select a template
4. Fill in the prompts
5. Done! Command is sent automatically

### Create a Template
1. Press `N` (Create Template)
2. Enter name: `my-template`
3. Enter content: `/code Fix {Bug-Description} in {File-Name}`
4. Done! Template saved

## 📝 Placeholder Syntax

Use `{Prompt-Text}` for dynamic inputs:

```
/web {Enter-URL}
Analyze the content and {What-to-do}
```

When used, you'll be prompted:
- "Enter-URL: " → you type the URL
- "What-to-do: " → you type the action

## 🎯 Example Templates

### Simple
```
/ask Explain {Concept} in simple terms
```

### Complex
```
/architect Implement {Feature-Name}

Requirements:
- {Requirement-1}
- {Requirement-2}

Tech stack: {Technology}
```

## ⌨️ Keybindings (in transient menu)

| Key | Action |
|-----|--------|
| `P` | Use Template |
| `N` | Create Template |
| `E` | Edit Template |
| `D` | Delete Template |
| `O` | Open Templates Directory |

## 📁 Template Location

Default: `~/.emacs.d/aidermacs-templates/`

Customize:
```elisp
(setq aidermacs-templates-directory "~/my-templates")
```

## 💡 Pro Tips

1. **Descriptive Names**: Use clear placeholder names
   - Good: `{API-Endpoint-URL}`
   - Bad: `{url}`

2. **Reuse Placeholders**: Same name = asked once
   ```
   Hello {Name}, welcome {Name}!
   ```
   Only prompts for "Name" once

3. **Any Aider Command**: Templates work with all commands
   - `/code` - Make changes
   - `/ask` - Ask questions
   - `/architect` - Design solutions
   - `/web` - Fetch web content

4. **Version Control**: Keep templates in git
   ```bash
   cd ~/.emacs.d/aidermacs-templates
   git init
   ```

## 🔧 Troubleshooting

**Templates not showing?**
- Check directory: `M-x aidermacs-open-templates-directory`
- Verify extension: Files must end in `.txt`

**Placeholders not working?**
- Use `{Text}` not `{{Text}}` or `[Text]`
- No spaces: `{Name}` not `{ Name }`

## 📚 More Info

See `TEMPLATE_SYSTEM.md` for complete documentation.

## 🎉 You're Ready!

Start creating templates for your common workflows!
