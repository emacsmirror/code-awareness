# Peer Selection in CodeAwareness for Emacs

This document describes the peer selection functionality in CodeAwareness for Emacs, which allows you to view diffs between your working copy and a peer's version of the same file.

## Overview

When a peer is selected in the Muninn app, the local service sends a `peer:select` message to the Emacs client. The Emacs implementation then:

1. Receives the peer selection event
2. Sends a `code:diff-peer` request to get the diff information
3. Opens a diff view showing the differences between your file and the peer's version
4. Closes the diff view when the peer is unselected

## How it Works

### Event System

The implementation uses an event-driven architecture with a centralized event handler table:

- `codeawareness--events-table`: Hash table mapping event names to handler functions
- `codeawareness--register-event-handler()`: Registers new event handlers
- `codeawareness--init-event-handlers()`: Initializes all event handlers

### Message Flow

1. **Peer Selection**: Muninn app sends a `code:peer:select` message with peer data
2. **Event Routing**: Message is routed to the appropriate handler via the events table
3. **Diff Request**: Emacs sends a `code:diff-peer` request with project and peer information
4. **Diff Response**: Local service responds with peer file path and diff metadata
5. **Diff Display**: Emacs opens a diff view using either `ediff` (if available) or `diff-mode`

### Key Functions

- `codeawareness--handle-peer-select(peer-data)`: Handles peer selection events
- `codeawareness--handle-peer-unselect()`: Handles peer unselection events
- `codeawareness--handle-peer-diff-response(data)`: Processes diff response data
- `codeawareness--open-diff-view(peer-file, user-file, title)`: Opens the diff view
- `codeawareness--close-diff-buffers()`: Closes all diff buffers

## Usage

### Automatic Operation

The peer selection functionality works automatically when:

1. CodeAwareness mode is enabled (`M-x codeawareness-mode`)
2. You have an active file open
3. A peer is selected in the Muninn app

### Manual Testing

You can manually trigger peer diff viewing using:

```
C-c C-a d
```

This will attempt to show a diff for the currently selected peer (if any).

## Diff View Options

The implementation supports two diff viewing modes:

1. **ediff** (preferred): Uses Emacs's built-in `ediff` functionality for a rich diff experience
2. **diff-mode** (fallback): Uses a simple diff buffer with `diff-mode` for basic diff viewing

## Configuration

No additional configuration is required. The functionality integrates with the existing CodeAwareness infrastructure.

## Testing

Run the test suite to verify the functionality:

```elisp
(require 'test-peer-selection)
(run-all-peer-selection-tests)
```

## Troubleshooting

### Common Issues

1. **No diff shown**: Ensure you have an active file open and a peer is selected
2. **ediff not available**: The system will fall back to diff-mode automatically
3. **IPC connection issues**: Check that the local service is running and connected

### Debugging

Enable debug logging to see detailed information about peer selection events:

```elisp
(setq codeawareness-log-level 'debug)
```

## Implementation Details

### Event Handler Registration

Event handlers are registered in `codeawareness--init-event-handlers()`:

```elisp
(codeawareness--register-event-handler "peer:select" #'codeawareness--handle-peer-select)
(codeawareness--register-event-handler "peer:unselect" #'codeawareness--handle-peer-unselect)
(codeawareness--register-event-handler "branch:select" #'codeawareness--handle-branch-select)
(codeawareness--register-event-handler "auth:logout" #'codeawareness--handle-auth-logout)
;; ... more handlers
```

### Message Format

Peer selection messages follow this format:

```json
{
  "flow": "req",
  "domain": "code", 
  "action": "peer:select",
  "data": {
    "_id": "67bc3432cf9b5efe459008dd",
    "name": "Alice",
    "company": "Kawa Code",
    "phone": "123-123-1234",
    "email": "alice@codeawareness.com",
    "createdAt": "2025-02-24T08:56:18.540Z",
    "active": true,
    "updatedAt": "2025-08-29T00:38:56.447Z",
    "lang": "en"
  },
  "err": "",
  "caw": "0"
}
```

### State Management

The implementation maintains peer selection state in:

- `codeawareness--selected-peer`: Currently selected peer data
- `codeawareness--events-table`: Event handler registry
- Diff buffers: Automatically created and managed

### Integration Points

- **IPC System**: Uses existing IPC infrastructure for communication
- **Buffer Management**: Integrates with Emacs buffer management
- **Project System**: Uses active project information for diff requests
- **Event System**: Centralized event handling for all CodeAwareness events
