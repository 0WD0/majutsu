#!/usr/bin/env bash
set -euo pipefail

repo_root=${REPO_ROOT:?REPO_ROOT is required}
emacs_bin=${EMACS:-emacs}
source_org="$repo_root/docs/majutsu.org"
tracked_texi="$repo_root/docs/majutsu.texi"
tmp_dir=$(mktemp -d "${TMPDIR:-/tmp}/majutsu-texi.XXXXXX")
trap 'rm -rf -- "$tmp_dir"' EXIT

actual_emacs=$(
  "$emacs_bin" --batch -Q --eval '(princ emacs-version)' 2>/dev/null
)
actual_org=$(
  "$emacs_bin" --batch -Q -l org --eval '(princ (org-version))' 2>/dev/null
)

grep -Fq 'GENERATED—DO NOT EDIT' "$tracked_texi" || {
  printf '%s\n' 'TEXINFO_TRACKED=FAIL reason=missing_generated_marker' >&2
  exit 1
}

cp "$source_org" "$tmp_dir/majutsu.org"
(
  cd "$tmp_dir"
  "$emacs_bin" --batch -Q -l org \
    majutsu.org -f org-texinfo-export-to-texinfo >/dev/null
)
generated_texi="$tmp_dir/majutsu.texi"
grep -Fq 'GENERATED—DO NOT EDIT' "$generated_texi" || {
  printf '%s\n' 'TEXINFO_EXPORT=FAIL reason=generated_output_missing_marker' >&2
  exit 1
}

if ! makeinfo --no-split "$generated_texi" -o "$tmp_dir/majutsu.info" \
  >"$tmp_dir/makeinfo.stdout" 2>"$tmp_dir/makeinfo.stderr"; then
  printf '%s\n' 'TEXINFO_MAKEINFO=FAIL reason=compile_error' >&2
  cat "$tmp_dir/makeinfo.stderr" >&2
  exit 1
fi
if [[ -s "$tmp_dir/makeinfo.stderr" ]]; then
  printf '%s\n' 'TEXINFO_MAKEINFO=FAIL reason=warnings' >&2
  cat "$tmp_dir/makeinfo.stderr" >&2
  exit 1
fi

printf 'TEXINFO_EXPORT=PASS sha256=%s\n' \
  "$(sha256sum "$generated_texi" | awk '{print $1}')"
printf 'TEXINFO_TOOLCHAIN=PASS emacs=%s org=%s\n' "$actual_emacs" "$actual_org"
printf '%s\n' 'TEXINFO_MAKEINFO=PASS warnings=0'

