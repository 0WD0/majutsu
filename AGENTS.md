# Repository Guidelines

## Project Structure & Module Organization
- `majutsu.el` – Primary Emacs Lisp source (major mode + commands).
- `README.org` – Usage and installation notes.
- `LICENSE` – Project license.
- `test/` – ERT tests (add as needed, e.g., `test/majutsu-test.el`).

Keep modules small and cohesive. Prefer adding helpers as private functions inside `majutsu.el`. Split into new files only when a logical boundary is obvious.

## Build, Test, and Development Commands
- Byte-compile: `emacs -Q --batch -L . -f batch-byte-compile majutsu.el`
- Load check: `emacs -Q --batch -L . --eval '(progn (require (quote majutsu)) (message "loaded"))'`
- Interactive dev: `emacs -Q -L . -l majutsu.el` then `M-x majutsu-log`
- checkdoc: `emacs -Q --batch -L . majutsu.el -f checkdoc`
- package-lint (if installed): `emacs -Q --batch -L . -l package-lint.el majutsu.el -f package-lint-batch-and-exit`
- Run tests: `emacs -Q --batch -L . -L test -l majutsu.el -l test/majutsu-test.el -f ert-run-tests-batch-and-exit`

## Coding Style & Naming Conventions
- Emacs Lisp, 2-space indentation; no tabs.
- Prefix all symbols with `majutsu-`; private helpers may use `majutsu--`.
- Interactive commands use `(interactive)` and concise, action-first docstrings.
- User options via `defcustom` with appropriate `:type`.
- Avoid `shell-command`; prefer `process-file`/`process-lines` and validate inputs.

## Testing Guidelines
- Use ERT; place tests in `test/` and name them like `test-majutsu-*`.
- Focus on parsing, transient construction, and command dispatch.
- Keep tests deterministic; avoid network/external side effects.

## Commit & Pull Request Guidelines
- Commits: imperative mood, concise subject, context in body when needed.
  - Example: `Improve log parsing for custom template`.
- PRs: clear summary, rationale, before/after notes, and screenshots/GIFs for UI changes.
- Link related issues/PRs and update `README.org` when behavior changes.

## Security & Configuration Tips
- Ensure `jj` is in `PATH` or set `majutsu-executable`:
  - Example: `(setq majutsu-executable "/usr/local/bin/jj")`
- Handle errors from external processes robustly; never parse untrusted shell output without quoting.

## Agent-Specific Instructions
- Keep changes minimal and focused; do not reformat unrelated code.
- Follow naming and byte-compilation checks; add ERT tests when introducing non-trivial logic.
