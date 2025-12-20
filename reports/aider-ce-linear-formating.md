# Aider-CE Linear Output Formatting Guide

This document provides a hierarchical view of the distinct textual output formats produced by `aider-ce`, especially when running in a linear mode suitable for programmatic parsing (like in `aidermacs`).

## 1. Universal Formats (Appear in All Modes)

These formats can appear at any time, regardless of the active `chat-mode`.

### 1.1. Tool Call Blocks

This block announces an AI's intent to use a tool and its arguments.

**String Representation:**
```text
[<color>]Tool Call:[/<color>] <server_name> • <tool_name>
[<color>]Arguments:[/<color>] {<json_string>}
```
*or for tools with unwrapped arguments:*
```text
[<color>]Tool Call:[/<color>] <server_name> • <tool_name>
[<color>]<arg1_name>:[/]
<arg1_value>
[<color>]<arg2_name>:[/]
<arg2_value>
```

**Snippet:**
```text
[blue]Tool Call:[/] web-search • full-web-search
[blue]Arguments:[/] {"query": "Model Control Protocol specification site:modelcontextprotocol.io"}
```

**Source File(s):**
- `aider/tools/utils/output.py`

**Notes:** This is a primary pattern to detect. It is always followed by an interactive prompt asking for user confirmation.

### 1.2. Tool Result Messages

These messages appear after a tool has been executed.

#### 1.2.1. Tool Success

**String Representation:**
```text
✅ <Human-readable success message> (change_id: <id>)
```

**Snippet:**
```text
✅ Replaced 5 lines in path/to/file.py (change_id: 1a2b3c-4d5e)
```

**Source File(s):**
- `aider/tools/utils/helpers.py` (defines the format)
- `aider/io.py` (prints the message)

#### 1.2.2. Tool Error / Warning

**String Representation:**
```text
[<color>]ERROR:[/<color>] <Error message>
[<color>]WARNING:[/<color>] <Warning message>
```

**Snippet:**
```text
[red]ERROR:[/] Error in ReplaceText: File 'nonexistent.py' not found
```

**Source File(s):**
- `aider/io.py` (methods `tool_error`, `tool_warning`)

### 1.3. Interactive Prompts

These are direct questions to the user, requiring a response.

**String Representation:**
```text
<Question>? (<Options>) [<Default>]:
```

**Snippet:**
```text
Run tools? (Y)es/(N)o/(A)ll/(S)kip all [Yes]:
```

**Source File(s):**
- `aider/io.py` (primarily the `confirm_ask` method)

### 1.4. AI Reasoning Blocks

Indicates the AI is in a "thinking" phase, separate from its final answer.

**String Representation:**
```text
--------------
► **THINKING**
...
(The AI's reasoning as markdown)
...
------------
► **ANSWER**
```

**Snippet:**
```text
--------------
► **THINKING**
The user wants to find all files related to the template system. I should use the `grep` tool to search for the word "template" in the codebase. This will give me a starting point for the refactoring task.
------------
► **ANSWER**
```

**Source File(s):**
- `aider/reasoning_tags.py`

### 1.5. Standard LLM Markdown

This is the general output from the LLM, including text, lists, and code blocks.

**String Representation:**
```markdown
## A Heading

- A list item
- Another list item

```python
# A code block
def hello():
    print("world")
```
```

**Source File(s):**
- `aider/io.py` (the `assistant_output` method renders this)

---

## 2. Agent Mode Formats (`/chat-mode agent`)

Agent mode is a hybrid mode that uses a sequence of Universal Formats but also introduces its own unique, structured information blocks.

### 2.1. Primary Agent Loop Output

The main output is a sequence of **Tool Call Blocks (1.1)**, **Interactive Prompts (1.3)**, and **Tool Result Messages (1.2)**.

### 2.2. Context Blocks (Agent-Specific)

Before sending a prompt to the LLM, the agent constructs special informational blocks to give the AI situational awareness. These are printed to the console for the user to see and are wrapped in XML-like tags.

**String Representation:**
```xml
<context name="<block_name>">
## <Block Title>

... (Content of the block) ...

</context>
```

**Snippets of different context blocks:**
```xml
<context name="context_summary">
## Current Context Overview

Model context limit: 200,000 tokens

### Editable Files

- my_file.py: 1,234 tokens (🟡 Medium)
...
</context>
```
```xml
<context name="gitStatus">
## Git Repository Status

Current branch: main
Status:
 M my_file.py
?? new_file.py
...
</context>
```
```xml
<context name="tool_usage_history">
## Turn and Tool Call Statistics
- Current turn: 3
- Total tool calls this turn: 5
</context>
```

**Source File(s):**
- `aider/coders/agent_coder.py` (methods like `get_context_summary`, `get_git_status`, `_generate_tool_context`, etc.)

**Notes:** This is a key format to parse. The content inside the block is structured markdown.

### 2.3. Direct Code Edits (Hybrid Capability)

Unlike a pure tool-based agent, the `agent` in `aider-ce` can also directly propose code changes using the `SEARCH/REPLACE` format.

**String Representation:**
- A `SEARCH/REPLACE` Diff Block (see format 4.1).

**Source File(s):**
- `aider/coders/agent_coder.py` (the `reply_completed` method contains logic to detect and apply these blocks).

---

## 3. Architect Mode Formats (`/chat-mode architect`)

This is a multi-stage process with distinct output formats at each stage.

### 3.1. Stage 1: Conceptual Plan

**String Representation:**
- Standard LLM Markdown (see 1.5).

**Snippet:**
```markdown
To implement this feature, I will first add a test case to `tests/test_templates.py` that expects a header to be parsed. Then, I will modify the `Template` class in `src/templates.py` to extract the header.
```

**Source File(s):**
- `aider/coders/architect_prompts.py` (contains the prompt that asks for this plan).

### 3.2. Stage 2: Confirmation Prompt

**String Representation:**
- An Interactive Prompt (see 1.3).

**Snippet:**
```text
Edit the files? (Y)es/(N)o/(T)weak [Yes]:
```

**Source File(s):**
- `aider/coders/architect_coder.py` (triggers this specific prompt).

### 3.3. Stage 3: The `SEARCH/REPLACE` Diff Block

This is the unique format for specifying code changes in this mode, generated by a secondary coder.

**String Representation:**
- A `SEARCH/REPLACE` Diff Block (see format 4.1).

**Source File(s):**
- `aider/coders/architect_coder.py` (consumes this format).
- `aider/coders/editblock_prompts.py` (contains the prompt used by the secondary coder to generate this format).

---

## 4. Standard Code Editing Formats

These are simpler modes that produce a single type of code edit.

### 4.1. Diff / EditBlock Mode (`/chat-mode diff`)

This mode uses the `SEARCH/REPLACE` format for all code modifications.

**String Representation:**
```diff
<file_path>
<<<<<<< SEARCH
(Code to be replaced)
