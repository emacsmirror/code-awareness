# CLAUDE.md - AI Assistant Guide for Code Awareness Emacs Package

This document provides comprehensive information about the Code Awareness Emacs package for AI assistants working with this codebase.

## Table of Contents
- [Project Overview](#project-overview)
- [Codebase Structure](#codebase-structure)
- [Architecture](#architecture)
- [Development Workflow](#development-workflow)
- [Key Conventions](#key-conventions)
- [Testing](#testing)
- [Common Tasks](#common-tasks)
- [Important Notes for AI Assistants](#important-notes-for-ai-assistants)

---

## Project Overview

**Code Awareness** is a low-noise collaboration toolset for Emacs that highlights code intersections between your working copy and other team members. It provides an early warning system for merge conflicts and enables instant traveling between working copies of multiple developers without needing to commit and push.

### Key Information
- **License**: GPLv3 (package), Proprietary (Code Awareness binary)
- **Current Version**: 1.0.15
- **Minimum Emacs Version**: 27.1 (required for mutex/condition-variable support)
- **Homepage**: https://github.com/CodeAwareness/ca.emacs
- **Status**: Production-ready with full feature parity with VS Code extension

### Core Features
1. **Real-time code intersection highlighting** - Visual feedback on overlapping code changes
2. **Peer diff viewing** - Compare your code with teammates' working copies
3. **Branch comparison** - View diffs between branches without switching
4. **Merge conflict prevention** - Early warning system for potential conflicts
5. **IPC communication** - Robust communication with Code Awareness application
6. **Theme-aware highlighting** - Automatic light/dark theme detection

---

## Codebase Structure

```
kawa.emacs/
├── code-awareness.el                    # Main entry point (1766 lines)
│   ├── Configuration & customization
│   ├── Minor mode definition
│   ├── IPC event handlers
│   ├── Highlighting system
│   └── Buffer management
│
├── code-awareness-pipe.el               # Circular buffer pipe (435 lines)
│   ├── Circular buffer implementation
│   ├── First-class variable system
│   └── Low-level pipe operations
│
├── code-awareness-list-pipe.el          # List-based pipe (174 lines)
│   ├── Simpler list-based pipe
│   └── Alternative to circular buffer
│
├── code-awareness-process-sockets.el    # Socket abstraction (167 lines)
│   ├── Cross-platform socket handling
│   ├── Mutex-based concurrency
│   └── Process management
│
├── tests/
│   └── test-peer-selection.el           # Test suite (95 lines)
│
├── docs/
│   ├── spacemacs-config.el              # Spacemacs integration example
│   └── doom-config.el                   # Doom Emacs integration example
│
├── Makefile                              # Build automation
├── Cask                                  # Dependency management
├── README.org                            # User documentation
├── README-Development.md                 # Development documentation
├── Changelog.org                         # Version history
└── TODO.md                               # Future enhancements
```

### File Dependencies
```
code-awareness.el
  ├─ requires: code-awareness-pipe
  ├─ requires: code-awareness-list-pipe
  ├─ requires: code-awareness-process-sockets
  ├─ requires: json (built-in)
  ├─ requires: cl-lib (built-in)
  └─ optional: hl-line (for enhanced highlighting)
```

---

## Architecture

### Layered Architecture
```
┌─────────────────────────────────────────┐
│   code-awareness.el (Main Entry)        │
│   - Minor mode definition               │
│   - Event handlers & business logic     │
│   - Highlighting system                 │
│   - Buffer management                   │
└─────────────┬───────────────────────────┘
              │
    ┌─────────┴────────┬─────────────────┐
    │                  │                 │
    ▼                  ▼                 ▼
┌─────────┐    ┌──────────────┐   ┌──────────────┐
│  Pipe   │    │  List Pipe   │   │   Process    │
│ System  │    │   System     │   │   Sockets    │
└─────────┘    └──────────────┘   └──────────────┘
```

### IPC Communication System

**Protocol**: JSON-based messages with `\f` (form feed) delimiter

**Message Structure**:
```json
{
  "flow": "req|res|err",
  "domain": "*|code|auth|peer|branch",
  "action": "active-path|auth:info|peer:select|etc",
  "data": { ... },
  "caw": "client-guid"
}
```

**Connection Flow**:
1. Generate GUID → `code-awareness--generate-guid()`
2. Connect to catalog → `code-awareness--connect-to-catalog()`
3. Register client → `code-awareness--register-client()`
4. Poll for socket → `code-awareness--poll-for-local-service()`
5. Connect to main IPC → `code-awareness--connect-to-local-service()`
6. Initialize workspace → `code-awareness--init-workspace()`
7. Send auth:info → `code-awareness--send-auth-info()`

**Socket Paths**:
- Unix: `~/.kawa-code/sockets/caw.{GUID}`
- Windows: `\\.\pipe\caw.{GUID}`

### Highlighting System

**Dual Implementation** for maximum compatibility:

1. **Custom Overlay System**: Traditional Emacs overlays
2. **HL-Line Integration**: Modern hl-line mode for better empty line handling

**Highlight Types**:
- **Conflict**: Red highlighting for merge conflicts
- **Overlap**: Orange highlighting for overlapping changes
- **Peer**: Blue highlighting for peer modifications
- **Modified**: Green highlighting for local changes

**Theme Detection**:
```elisp
(frame-parameter nil 'background-mode) → 'dark or 'light
```

### State Management

**Centralized Store Pattern**:
```elisp
code-awareness--store              ; Central state object
code-awareness--active-project     ; Current project data
code-awareness--active-buffer      ; Currently tracked buffer
code-awareness--highlights         ; Hash table: buffer → overlays
code-awareness--hl-line-overlays   ; Hash table: buffer → line → overlay
code-awareness--user               ; User authentication data
code-awareness--tokens             ; Auth tokens
code-awareness--events-table       ; Event dispatcher: "domain:action" → handler
code-awareness--response-handlers  ; Response handlers: "res:domain:action" → callback
```

### Event System

**Hash Table-Based Dispatch**:
```elisp
code-awareness--events-table: Maps "domain:action" → handler function
```

**Supported Events**:
- `peer:select`, `peer:unselect` - Peer selection management
- `branch:select`, `branch:unselect` - Branch diff operations
- `auth:logout` - Authentication cleanup
- `context:add`, `context:del`, `context:open-rel` - Context operations

---

## Development Workflow

### Initial Setup

```bash
# Install dependencies
cask install

# Compile the package
make compile

# Run tests
make test

# Clean compiled files
make clean

# Lint (compile then clean)
make lint
```

### Makefile Targets

```makefile
make prepare  # Downloads and updates Cask dependencies
make compile  # Compiles with byte-compile-error-on-warn=t
make test     # Runs buttercup test suite
make clean    # Removes .elc files
make lint     # Compiles then cleans (for CI/CD)
make checkdoc # Runs documentation linter
```

### Development Cycle

1. **Make changes** to `.el` files
2. **Run `make compile`** - Must compile cleanly with NO warnings
3. **Run `make test`** - All tests must pass
4. **Run `make checkdoc`** - Documentation must be complete
5. **Test manually** - Use `make clean-start` to test in fresh Emacs

### Git Workflow

**Branch Naming**:
- Feature branches: `feature/description`
- Bug fixes: `fix/description`
- Development: `claude/claude-md-{session-id}`

**Commit Messages**:
- Start with category in brackets: `[Icons]`, `[IPC]`, `[Highlighting]`
- One commit per feature
- Example: `[IPC] Add auto-reconnection with exponential backoff`

**Contribution Guidelines**:
1. One commit per feature
2. No compiler warnings (strictly enforced)
3. All tests must pass
4. Document new features in README
5. Follow Emacs Lisp conventions

---

## Key Conventions

### Emacs Lisp Conventions

**Naming**:
- Public API: `code-awareness-function-name`
- Internal/private: `code-awareness--function-name` (double dash)
- Constants: `code-awareness--constant-name`
- Variables: `code-awareness-variable-name`

**Indentation**:
- **No tabs**: Use spaces only
- **Tab width**: 2 spaces (configured in `.dir-locals.el`)
- **Lexical binding**: Always use `; -*- lexical-binding: t -*-`

**Documentation**:
- All public functions must have docstrings
- Use triple semicolons `;;;` for section headers
- Use double semicolons `;;` for normal comments
- Use single semicolon `;` for inline comments

**Code Organization** (in code-awareness.el):
```elisp
;;; Commentary:
;;; Code:

;;; Configuration (defcustom, defconst)
;;; State Management (defvar)
;;; Utility Functions
;;; IPC Communication
;;; Event Handlers
;;; Highlighting System
;;; Buffer Management
;;; Minor Mode Definition
;;; Interactive Commands

(provide 'code-awareness)
;;; code-awareness.el ends here
```

### Design Patterns

#### 1. Functional Closure Pattern (Pipe System)
```elisp
(lambda (fn-or-var)
  (cl-case fn-or-var
    ((env) env)
    (t (error "Invalid arguments"))))
```
Creates first-class objects with private state.

#### 2. First-Class Variables
```elisp
code-awareness-pipe-make-var(val)   ; Constructor
code-awareness-pipe-var-ref         ; Accessor
code-awareness-pipe-set-var!        ; Mutator
code-awareness-pipe-inc-var!        ; Increment
```

#### 3. Hash Table-Based Dispatch
```elisp
(defun code-awareness--init-event-handlers ()
  (clrhash code-awareness--events-table)
  (puthash "peer:select" #'code-awareness--handle-peer-select ...)
  (puthash "branch:select" #'code-awareness--handle-branch-select ...))
```

#### 4. Debouncing Pattern
```elisp
(defun code-awareness--schedule-update ()
  (when code-awareness--update-timer
    (cancel-timer code-awareness--update-timer))
  (setq code-awareness--update-timer
        (run-with-timer code-awareness-update-delay nil
                        #'code-awareness--refresh-active-file)))
```

#### 5. Sentinel Pattern for Process Monitoring
```elisp
(defun code-awareness--ipc-sentinel (process event)
  "Handle IPC connection state changes."
  (cond
   ((string-match "open" event) ...)
   ((string-match "failed\\|exited\\|connection broken" event) ...)))
```

#### 6. Auto-Reconnection Strategy
Exponential backoff polling:
```elisp
Delays: 0.5s, 1s, 2s, 4s, 8s, 16s, ...
Max attempts: 10
```

### Performance Considerations

1. **Use compiled bytecode**: Always run `make compile` before deployment
2. **Debounce updates**: Don't update highlights on every keystroke
3. **Lazy initialization**: Initialize features only when needed
4. **Efficient overlays**: Clean up overlays when not needed
5. **Hash tables**: Use hash tables for O(1) lookups, not alists

---

## Testing

### Test Framework
Uses **Buttercup** (BDD testing framework for Emacs Lisp)

### Test Structure
```elisp
(describe "Feature Name"
  (it "should do something specific"
    (expect (some-function) :to-equal expected-value)))
```

### Running Tests
```bash
# Run all tests
make test

# Run with Cask
cask exec buttercup -L . tests/
```

### Test Coverage
Current test file: `tests/test-peer-selection.el` (95 lines)

**Areas to test**:
- IPC message parsing
- Highlight application
- Event handler dispatch
- Connection lifecycle
- Error handling

---

## Common Tasks

### Adding a New Event Handler

1. **Define the handler function**:
```elisp
(defun code-awareness--handle-new-event (data)
  "Handle the new-event from Code Awareness.
DATA contains the event payload."
  (code-awareness--log 'info "Handling new event: %S" data)
  ;; Implementation here
  )
```

2. **Register in event table**:
```elisp
(defun code-awareness--init-event-handlers ()
  ;; ... existing handlers ...
  (puthash "domain:new-event" #'code-awareness--handle-new-event
           code-awareness--events-table))
```

3. **Test the handler**:
```elisp
(describe "code-awareness--handle-new-event"
  (it "should process new event correctly"
    (expect (code-awareness--handle-new-event test-data)
            :to-equal expected-result)))
```

### Adding a New Configuration Option

```elisp
;;;###autoload
(defcustom code-awareness-new-option default-value
  "Description of what this option does."
  :type 'type-specification
  :group 'code-awareness-config)
```

**Common types**: `'string`, `'number`, `'boolean`, `'symbol`, `'(repeat string)`

### Adding a New Interactive Command

```elisp
;;;###autoload
(defun code-awareness-new-command ()
  "Short description for M-x completion.
Longer description in docstring body."
  (interactive)
  (when code-awareness-mode
    ;; Implementation here
    (code-awareness--log 'info "Command executed")))
```

### Modifying the Highlighting System

**To add a new highlight type**:

1. **Define the color**:
```elisp
(defcustom code-awareness-new-type-color-light "#hexcolor"
  "Color for new highlight type in light theme."
  :type 'string
  :group 'code-awareness-config)

(defcustom code-awareness-new-type-color-dark "#hexcolor"
  "Color for new highlight type in dark theme."
  :type 'string
  :group 'code-awareness-config)
```

2. **Create the face**:
```elisp
(defun code-awareness--init-highlight-faces ()
  ;; ... existing faces ...
  (let ((is-dark (eq (frame-parameter nil 'background-mode) 'dark)))
    (setq code-awareness--new-type-face
          (list :background (if is-dark
                                code-awareness-new-type-color-dark
                              code-awareness-new-type-color-light)
                :extend t))))
```

3. **Apply the highlight**:
```elisp
(let ((ov (make-overlay start end)))
  (overlay-put ov 'face code-awareness--new-type-face)
  (overlay-put ov 'code-awareness t))
```

### Debugging Tips

**Enable debug mode**:
```elisp
(setq code-awareness-debug t)
```

**View logs**:
```elisp
M-x code-awareness-show-log-buffer
```

**Log levels**:
```elisp
(code-awareness--log 'error "Error message: %s" error-data)
(code-awareness--log 'warn "Warning: %s" warning)
(code-awareness--log 'info "Info: %s" info)
(code-awareness--log 'debug "Debug: %s" debug-data)
```

**Test IPC connection**:
```elisp
M-x code-awareness-connection-status
M-x code-awareness-auth-status
```

**Inspect state**:
```elisp
;; Evaluate in *scratch* buffer
code-awareness--store
code-awareness--active-project
code-awareness--connected
code-awareness--authenticated
```

---

## Important Notes for AI Assistants

### Code Quality Standards

**CRITICAL: Zero Tolerance for Warnings**
- The package MUST compile with `byte-compile-error-on-warn t`
- **NO warnings allowed** - the build will fail if any warnings occur
- Always run `make compile` after making changes
- Common issues:
  - Unused lexical variables: Prefix with `_` (e.g., `_unused-var`)
  - Free variables: Add proper `defvar` declarations
  - Obsolete functions: Update to modern equivalents
  - Missing docstrings: Add comprehensive documentation

### Common Pitfalls to Avoid

1. **Don't use `setq` for buffer-local variables without `defvar-local`**
```elisp
;; Wrong
(setq my-var value)

;; Right
(defvar-local my-var nil "Description.")
(setq my-var value)
```

2. **Don't forget lexical binding header**
```elisp
;;; file-name.el --- Description -*- lexical-binding: t -*-
```

3. **Don't use deprecated functions**
```elisp
;; Deprecated
(flet ...)
(cl-flet ...)  ; Use this instead

;; Deprecated
(loop ...)
(cl-loop ...)  ; Use this instead
```

4. **Don't forget to clean up resources**
```elisp
;; Always clean up overlays
(when (overlayp ov)
  (delete-overlay ov))

;; Always cancel timers
(when (timerp timer)
  (cancel-timer timer))

;; Always close processes
(when (process-live-p process)
  (delete-process process))
```

5. **Don't skip EDiff sessions**
```elisp
;; Always check for EDiff before updating
(unless (bound-and-true-p ediff-this-buffer-ediff-sessions)
  (code-awareness--update-highlights))
```

### Cross-Platform Considerations

**Path handling**:
```elisp
;; Always normalize paths for JSON
(defun code-awareness--cross-platform-path (path)
  "Convert PATH to forward slashes for JSON compatibility."
  (replace-regexp-in-string "\\\\" "/" path))
```

**Socket paths**:
- Unix: `~/.kawa-code/sockets/caw.GUID`
- Windows: `\\.\pipe\caw.GUID`

**Line endings**:
- Use `\n` for JSON messages
- Use `\f` (form feed) as message delimiter

### Security Considerations

1. **Never log sensitive data**: Auth tokens should not appear in logs
2. **Validate all IPC messages**: Check message structure before processing
3. **Sanitize file paths**: Use `expand-file-name` and validate paths exist
4. **Handle malformed JSON**: Wrap `json-parse-string` in condition-case

### Performance Best Practices

1. **Use hash tables for lookups**: O(1) vs O(n) for alists
2. **Debounce expensive operations**: Use timers for highlights
3. **Clean up overlays**: Don't accumulate overlays indefinitely
4. **Avoid redundant updates**: Check if buffer actually changed
5. **Use compiled bytecode**: 2-5x faster than interpreted code

### Documentation Requirements

**Every public function must have**:
- Summary line (one sentence)
- Detailed description (if needed)
- Parameter documentation (if any)
- Return value documentation (if applicable)

```elisp
(defun code-awareness-example (param1 param2)
  "Short summary of what this function does.

Longer description can go here if needed.

PARAM1 is the first parameter.
PARAM2 is the second parameter.

Returns a processed result."
  (body))
```

### Testing Requirements

**Before submitting changes**:
1. ✓ `make compile` - Must succeed with NO warnings
2. ✓ `make test` - All tests must pass
3. ✓ `make checkdoc` - Documentation must be complete
4. ✓ Manual testing - Actually test the feature in Emacs
5. ✓ Verify README updated if needed

### Version Compatibility

**Minimum Emacs 27.1** required for:
- `make-mutex` / `make-condition-variable`
- `with-mutex` macro
- Improved JSON parsing

**Optional features**:
- `hl-line-mode` - For enhanced highlighting (Emacs 21+)
- `tab-bar-mode` - For tab integration (Emacs 27.1+)

### Code Awareness Ecosystem

This package is part of the larger Code Awareness ecosystem:
- **Code Awareness Binary**: Proprietary backend (required)
- **Kawa Code**: The main application (required)
- **VS Code Extension**: Reference implementation
- **Muninn**: Data processing service (optional)

**IPC Protocol**: Shared across all implementations - changes must be coordinated

---

## Quick Reference

### Key Functions

**IPC Communication**:
- `code-awareness--transmit(action, data)` - Send message
- `code-awareness--handle-ipc-message(msg)` - Process incoming
- `code-awareness--setup-response-handler(domain, action, file-path)` - Register response handler

**Highlighting**:
- `code-awareness--apply-hl-line-highlights-from-data(buffer, hl-data, type)` - Apply highlights
- `code-awareness--clear-buffer-highlights(buffer)` - Remove highlights from buffer
- `code-awareness--clear-all-highlights()` - Remove all highlights

**Buffer Management**:
- `code-awareness--refresh-active-file()` - Main refresh entry point
- `code-awareness--get-buffer-file-path(buffer)` - Get normalized path
- `code-awareness--schedule-update()` - Debounced update

**State**:
- `code-awareness--enable()` - Initialize and start
- `code-awareness--disable()` - Clean up and stop
- `code-awareness--reconnect()` - Reconnect to IPC

### Key Variables

**Connection State**:
- `code-awareness--connected` - Boolean: IPC connected
- `code-awareness--authenticated` - Boolean: User authenticated
- `code-awareness--caw` - String: Client GUID

**Configuration**:
- `code-awareness-debug` - Boolean: Enable debug logging
- `code-awareness-update-delay` - Number: Update debounce delay (seconds)
- `code-awareness-catalog` - String: Catalog name

**State Management**:
- `code-awareness--store` - Hash table: Central state
- `code-awareness--active-buffer` - Buffer: Currently tracked
- `code-awareness--highlights` - Hash table: Buffer → overlays

### File Map

| File | Purpose | Lines | Key Exports |
|------|---------|-------|-------------|
| `code-awareness.el` | Main entry point | 1766 | Minor mode, commands, IPC handlers |
| `code-awareness-pipe.el` | Circular buffer | 435 | Pipe implementation |
| `code-awareness-list-pipe.el` | List-based pipe | 174 | Alternative pipe |
| `code-awareness-process-sockets.el` | Socket abstraction | 167 | Cross-platform sockets |

---

## Resources

- **GitHub**: https://github.com/CodeAwareness/ca.emacs
- **Website**: https://code-awareness.com
- **Discord**: https://discord.com/channels/1412284975247790112/
- **Wiki**: https://github.com/CodeAwareness/code-awareness-emacs/wiki

---

*Last Updated: 2025-11-16*
*Version: 1.0.15*
*For Code Awareness Emacs Package*
