# Repository Guidelines

## Project Structure & Module Organization
- `majutsu.el` – Entry point and keymaps.
- `majutsu-core.el` – Core utilities and configuration.
- `majutsu-process.el` – Asynchronous process handling.
- `majutsu-log.el` – Log view and parsing.
- `majutsu-transient.el` – Transient menus and state.
- `majutsu-diff.el` - Diff related.
- `majutsu-commands.el` – Interactive commands.
- `majutsu-template.el` – Embedded DSL for composing Jujutsu templates.
- `majutsu-evil.el` – Native Evil-mode integration and keybindings.
- `NEWS.org` – Release notes and user-facing changes.
- `Eask` – Dependency and build configuration.
- `README.org` – Usage and installation notes.
- `docs/` – Living design notes/specifications (primarily Chinese).
- `LICENSE` – Project license.
- `test/` – ERT tests.

Keep modules small and cohesive.

## Build, Test, and Development Commands
This project uses [Eask](https://github.com/emacs-eask/eask) for development.

- Install dependencies: `eask install-deps`
- Byte-compile: `eask compile`
- Run tests: `eask test ert test/*.el`

## Testing Guidelines
- Use ERT; place tests in `test/` and name them like `test-majutsu-*`.
- Focus on parsing, transient construction, and command dispatch.
- Keep tests deterministic; avoid network/external side effects.

## Commit & Pull Request Guidelines
- Commits: imperative mood, concise subject, context in body when needed.
  - Example: `Improve log parsing for custom template`.
- PRs: clear summary, rationale, before/after notes, and screenshots/GIFs for UI changes.
- Link related issues/PRs and update `README.org` when behavior changes.

## Documentation
- **Code changes require documentation updates.** When modifying commands, keybindings, or user-facing behavior, update the corresponding sections in:
  - `docs/majutsu.org` – The main user manual (Texinfo-compatible Org format)
  - `README.org` – Quick-start guide (keep minimal, link to manual for details)
- Keep `README.org` usage/key binding sections in sync with UI changes.
- Update `docs/` specs when refining workflows or template semantics; preserve bilingual context and add English summaries for new sections when possible.
- Leverage `man jj-*` (e.g., `man jj-bookmark`) for upstream command docs when refining command integrations.

## Agent-Specific Instructions
- Keep changes minimal and focused; do not reformat unrelated code.
- Follow naming and byte-compilation checks; add ERT tests when introducing non-trivial logic.

## Version Control Workflow with Jujutsu (jj)

This project uses [Jujutsu](https://github.com/martinvonz/jj) for version control. AI agents should interact directly with the `jj` CLI.

### Basic jj Commands

**Check repository status:**
```bash
jj status                    # View current working copy state
jj log -n 20     # View recent 20 changes from all authors
```

**View changes:**
```bash
jj diff --git                # Show diff in git format (recommended for AI parsing)
jj diff                      # Show diff with color codes (human-readable)
jj diff -r <revision> --git # Show diff of specific revision in git format
jj show <revision>           # Show change summary and diff
```

**Describe changes:**
```bash
jj describe -m "message"           # Set description for current change (@)
jj describe <revision> -m "message"  # Set description for specific revision
```

**Create new changes:**
```bash
jj new -m "message"                # Create new change with description
jj new <parent-rev> -m "message"   # Create new change based on parent-rev
jj new                             # Create new empty change (will prompt for description later)
```

**Manage changes:**
```bash
jj abandon                   # Abandon current change (@)
jj abandon <revision>        # Abandon specific revision
jj squash                    # Squash current change into parent
jj rebase -s <source> -d <dest>  # Rebase source onto destination
```

**Bookmarks (branches):**
```bash
jj bookmark list             # List all bookmarks
jj bookmark create <name>    # Create bookmark at current change
jj bookmark set <name>       # Move bookmark to current change
```

**Undo mistakes:**
```bash
jj op log                    # View operation history
jj undo                      # Undo last operation
jj op restore <operation>    # Restore to specific operation
```

### When to Commit Changes

**During development:**
- **While working** - Use `jj describe -m "description"` to update the current change's description
  - This is like `git commit --amend` but without the complexity
  - You can do this multiple times as your work evolves

**After completing a logical unit:**
- **Task completed** - Use `jj new -m "next task description"` to start a new change
  - This "commits" your current work and creates a fresh change
  - Similar to `git commit` + starting new work

**Example workflow:**
```bash
# Start working on feature
jj describe -m "feat: add new feature"

# Make some changes, test them
# ... edit files ...

# Update description as work progresses
jj describe -m "feat: add feature X with Y support"

# Feature complete, create new change for next task
jj new -m "docs: update README for feature X"

# Or if you want to squash and move on
jj squash    # Squash into parent
jj new       # Start fresh
```

### Agent Commit Guidelines

**When to use `jj describe`:**
- Updating the description of ongoing work
- Refining the message as you understand the change better
- Before running tests or requesting review

**When to use `jj new`:**
- After completing a distinct, working feature
- When starting a logically separate task
- After a change is ready for review/merge

**Commit message format:**
- Use conventional commits: `type: description`
- Types: `feat`, `fix`, `docs`, `refactor`, `test`, `chore`
- Keep subject line under 72 characters
- Add context in body if needed

**Examples:**
```bash
jj describe -m "feat: add async log refresh"
jj describe -m "fix: handle empty revset in log view"
jj describe -m "docs: update AGENTS.md with jj workflow"
jj describe -m "refactor: extract template helpers to separate module"
```

### Important Notes for AI Agents

1. **Always check status first** - Run `jj status` before making commits
2. **Use `--git` format for diffs** - `jj diff --git` produces git-format diffs without color codes, easier for AI to parse
3. **Use describe for incremental work** - Don't create new changes for every small edit
4. **Create new change when switching contexts** - Use `jj new` to separate logical units
5. **Leverage jj's flexibility** - You can always `jj describe` to refine messages, no need to get it perfect first time
6. **Use operation log for safety** - `jj op log` shows all operations, `jj undo` can revert mistakes
7. **Consult official documentation** - For detailed command options and advanced usage, refer to:
   - `jj help <command>` - Built-in command help
   - [Jujutsu Documentation](https://martinvonz.github.io/jj/) - Official docs
   - `man jj-<command>` - Man pages (e.g., `man jj-rebase`, `man jj-bookmark`)

## Revsets Language

Revsets are used to select commits. Most jj commands accept revset expressions.

### Common Symbols

- `@` - Current working copy commit
- `@-` - Parent of current commit
- `@+` - Children of current commit
- `root()` - Virtual root commit (oldest ancestor)

### Operators

- `x-` - Parents of x
- `x+` - Children of x
- `::x` - Ancestors of x (including x)
- `x::` - Descendants of x (including x)
- `x..y` - Ancestors of y that are not ancestors of x (like git's `x..y`)
- `x::y` - Descendants of x that are also ancestors of y (git's `--ancestry-path x..y`)
- `~x` - NOT x (everything except x)
- `x & y` - AND (commits in both)
- `x | y` - OR (commits in either)

### Useful Functions

- `all()` - All visible commits
- `mine()` - Commits authored by current user
- `heads(x)` - Commits in x with no descendants in x
- `roots(x)` - Commits in x with no ancestors in x
- `latest(x, count)` - Latest count commits by timestamp
- `description(pattern)` - Commits with matching description
- `author(pattern)` - Commits by matching author
- `files(fileset)` - Commits modifying files matching fileset
- `empty()` - Commits with no file changes
- `merges()` - Merge commits
- `bookmarks([pattern])` - Local bookmarks matching pattern
- `remote_bookmarks([bookmark], [remote])` - Remote bookmarks

### Examples

```bash
# View your recent work
jj log -r 'mine()' -n 10

# View commits modifying specific directory
jj log -r 'files("src/")' -n 20

# View commits between two revisions
jj log -r '@-..@'

# Find empty commits
jj log -r 'empty()'

# View commits with specific description
jj log -r 'description("fix")'

# Complex: your commits that modified Rust files
jj log -r 'mine() & files(glob:"**/*.rs")'
```

## Filesets Language

Filesets are used to select files. Many jj commands accept fileset expressions.

### File Patterns

- `path` or `"path"` - cwd-relative path prefix (default is `prefix-glob:`)
- `glob:"pattern"` - cwd-relative glob (non-recursive)
- `prefix-glob:"pattern"` - cwd-relative glob (includes subdirs)
- `root:"path"` - workspace-relative path
- `root-glob:"pattern"` - workspace-relative glob

Note: Append `-i` for case-insensitive matching (e.g., `glob-i:"*.txt"`)

### Operators

- `~x` - NOT x (all files except x)
- `x & y` - AND (files in both)
- `x | y` - OR (files in either)

### Functions

- `all()` - All files
- `none()` - No files

### Examples

```bash
# Diff excluding Cargo.lock
jj diff '~Cargo.lock'

# Show changes to Rust files only
jj diff 'glob:"**/*.rs"'

# List files in src/ excluding tests
jj file list 'src ~ glob:"**/*_test.rs"'

# View changes to multiple specific files
jj diff 'foo.rs | bar.rs'

# Complex: all .rs files except in tests/ directory
jj diff 'glob:"**/*.rs" ~ tests'
```

### When to Use Filesets

- `jj diff [filesets]` - Show diff for specific files
- `jj log -r 'files(fileset)'` - Find commits modifying files
- `jj file list [filesets]` - List files matching pattern
- `jj split [filesets]` - Split change including/excluding files
- `jj squash [filesets]` - Squash only specific files
