# Repository Guidelines

This repository provides an Emacs major mode for Jujutsu (jj), named Majutsu. Use this guide for quick, consistent contributions.

## Project Structure & Module Organization
- `majutsu.el` – Primary Emacs Lisp source (major mode + commands).
- `readme.md` – Usage and installation notes.
- `LICENSE` – Project license.

Keep new modules small and cohesive. Prefer adding helpers as private functions in `jj-mode.el` unless a logical split is obvious.

## Build, Test, and Development Commands
- Byte-compile: `emacs -Q --batch -L . -f batch-byte-compile majutsu.el`
- Load check (non-interactive): `emacs -Q --batch -L . --eval '(progn (require (quote majutsu)) (message "loaded"))'`
- Interactive dev: `emacs -Q -L . -l majutsu.el` then `M-x majutsu-log`

Optional tooling if installed:
- `checkdoc` in batch: `emacs -Q --batch -L . majutsu.el -f checkdoc` (reports docstring/style issues)
- `package-lint`: `emacs -Q --batch -L . -l package-lint.el majutsu.el -f package-lint-batch-and-exit`

## Coding Style & Naming Conventions
- Emacs Lisp style with 2-space indentation; no tabs.
- Prefix all public and internal symbols with `majutsu-` (private helpers may use `majutsu--`).
- Interactive commands: define with `(interactive)` and clear docstrings (first line concise summary).
- Variables customizable by users should be `defcustom` with appropriate `:type`.
- Avoid `shell-command`; prefer `call-process`/`process-lines` and validate inputs.

## Testing Guidelines
- ERT preferred. Place tests in `test/` with files like `majutsu-test.el` and names `test-majutsu-*`.
- Run: `emacs -Q --batch -L . -L test -l majutsu.el -l test/majutsu-test.el -f ert-run-tests-batch-and-exit`.
- Aim to cover parsing, transient construction, and command dispatch.

## Commit & Pull Request Guidelines
- Commits: imperative mood, concise subject, include context in the body when needed. Example: `Improve log parsing for custom template`.
- PRs: include summary, rationale, before/after notes, and screenshots/GIFs for UI changes. Link related issues/PRs. Add usage notes to `readme.md` when behavior changes.

## Security & Configuration Tips
- Ensure `jj` is in `PATH` or set `majutsu-executable` (e.g., `(setq majutsu-executable "/usr/local/bin/jj")`).
- Handle errors from external processes robustly; never parse untrusted shell output without quoting.

## Agent-Specific Instructions
- Keep changes minimal and focused; do not reformat unrelated code.
- Follow naming, byte-compilation checks, and add ERT tests when introducing non-trivial logic.
