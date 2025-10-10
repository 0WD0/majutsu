# Majutsu (Jujutsu) for Emacs

Majutsu provides a magit-inspired interface for
[Jujutsu](https://github.com/martinvonz/jj), offering an efficient way to
interact with JJ repositories from within Emacs.

## Features

- **Magit-style log viewer** with collapsible sections and syntax highlighting
- **Interactive rebase** with visual source/destination selection via transients
- **Bookmark management** with create, abandon, forget, and track operations
- **Commit and describe** with dedicated message buffers and window management
- **Diff viewing** with file and hunk-level navigation
- **Context-sensitive actions** via DWIM (Do What I Mean) Enter key behavior
- **Git integration** with push/fetch operations and configurable options
- **Built-in conflict resolution** using Emacs ediff and smerge-mode

## Requirements

- Emacs 26.1 or later
- [Jujutsu (jj)](https://github.com/jj-vcs/jj) installed and in PATH
- [magit](https://magit.vc/) (for section management and UI components)
- [transient](https://github.com/magit/transient) (usually bundled with magit)

## Installation

### Doom Emacs
```lisp
(package! majutsu :recipe (:host github :repo "bolivier/majutsu"))
```

### use-package with straight.el
```lisp
(use-package majutsu
  :straight (:host github :repo "bolivier/majutsu"))
```

### use-package with built-in package-vc integration
```lisp
(use-package majutsu
  :vc (:url "https://github.com/bolivier/majutsu"))
```

### Manual
Clone this repository and add it to your load path:
```lisp
(add-to-list 'load-path "/path/to/majutsu")
(require 'majutsu)
```

## Usage

Start with `M-x majutsu-log` to open the main interface. Each project gets its own
buffer (`*majutsu-log:project-name*`).

### Key Bindings

#### Navigation
- `n`/`p` - Navigate between sections
- `RET` - Context-sensitive action (edit changeset, jump to file/line in diffs)
- `.` - Jump to current changeset (@)
- `TAB` - Toggle section folding

#### Basic Operations
- `g` - Refresh log
- `c` - Commit (opens message buffer)
- `d` - Describe changeset at point (opens message buffer)
- `e` - Edit changeset (jj edit)
- `u` - Undo last operation
- `R` - Redo last operation
- `s` - Squash
- `N` - New changeset here

#### Advanced Operations
- `r` - Rebase transient menu
  - `s` - Set rebase source
  - `d` - Toggle rebase destination
  - `r` - Execute rebase
  - `c` - Clear selections
- `b` - Bookmark transient menu
  - `c` - Create bookmark
  - `a` - Abandon bookmark
  - `f` - Forget bookmark
  - `t` - Track remote bookmark
- `G` - Git operations transient
  - `-n` - Toggle --allow-new flag
  - `-b` - Set bookmark to push
  - `p` - Push
  - `f` - Fetch

#### Conflict Resolution
- `E` - Edit conflicts with ediff
- `M` - Edit conflicts with smerge-mode

#### Message Buffers
When editing commit/describe messages:
- `C-c C-c` - Finish and execute
- `C-c C-k` - Cancel

### Workflow Example

1. `M-x majutsu-log` - Open JJ interface
2. Navigate to desired changeset with `j`/`k`
3. `c` - Commit current changes
4. Edit message, `C-c C-c` to finish
5. `r` - Open rebase menu, select source with `s`, destinations with `d`, execute with `r`
6. `b` - Manage bookmarks as needed
7. `G` `p` - Push to remote

## Configuration

```lisp
;; Customize jj executable path if needed
(setq majutsu-executable "/path/to/jj")
```

## Contributing

Issues and pull requests welcome! This project aims to provide a solid JJ
interface while maintaining magit-like usability patterns.
