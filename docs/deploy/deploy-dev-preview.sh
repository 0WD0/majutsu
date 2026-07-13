#!/bin/sh
set -eu

repo_root=$(CDPATH='' cd -- "$(dirname -- "$0")/../.." && pwd)
dist=${1:-"$repo_root/dist"}
commit=${2:-${MAJUTSU_DOCS_COMMIT:-}}
project=${MAJUTSU_CLOUDFLARE_PROJECT:-majutsu-docs}
branch=${MAJUTSU_PREVIEW_BRANCH:-dev}

case "$dist" in /*) ;; *) dist="$repo_root/$dist" ;; esac
if test -z "$commit"; then
  commit=$(jj --repository "$repo_root" log --no-graph -r @ -T commit_id 2>/dev/null || true)
fi
if test -z "$commit"; then
  printf '%s\n' 'DEPLOY_DEV_PREVIEW=FAIL build commit is required' >&2
  exit 2
fi
case "$commit" in
  *[!0-9a-f]*)
    printf 'DEPLOY_DEV_PREVIEW=FAIL invalid commit ID: %s\n' "$commit" >&2
    exit 2
    ;;
esac
if test "${#commit}" -ne 40; then
  printf 'DEPLOY_DEV_PREVIEW=FAIL invalid commit ID: %s\n' "$commit" >&2
  exit 2
fi
case "$branch" in
  '' | [!A-Za-z0-9]* | *[!A-Za-z0-9._/-]* | */ | /* | *..* | *//*)
    printf 'DEPLOY_DEV_PREVIEW=FAIL unsafe branch: %s\n' "$branch" >&2
    exit 2
    ;;
esac
test -f "$dist/docs/dev/guide/introduction/index.html"
test -f "$dist/_headers"
test -f "$dist/_redirects"
node "$repo_root/docs/deploy/check-dev-preview.mjs" "$dist"

remote_production_branch=
if test -n "${CLOUDFLARE_ACCOUNT_ID:-}" && test -n "${CLOUDFLARE_API_TOKEN:-}"; then
  project_json=$(curl --fail-with-body --silent --show-error \
    --connect-timeout 10 --max-time 30 \
    --header "Authorization: Bearer $CLOUDFLARE_API_TOKEN" \
    "https://api.cloudflare.com/client/v4/accounts/$CLOUDFLARE_ACCOUNT_ID/pages/projects/$project")
  remote_production_branch=$(printf '%s' "$project_json" | node -e '
    let input = "";
    process.stdin.setEncoding("utf8");
    process.stdin.on("data", (chunk) => { input += chunk; });
    process.stdin.on("end", () => {
      const response = JSON.parse(input);
      const branch = response.result?.production_branch;
      if (response.success !== true || typeof branch !== "string" || !branch) process.exit(2);
      process.stdout.write(branch);
    });
  ') || {
    printf '%s\n' 'DEPLOY_DEV_PREVIEW=FAIL Cloudflare project response is invalid' >&2
    exit 4
  }
elif test "${MAJUTSU_DEPLOY_EXECUTE:-no}" = yes; then
  printf '%s\n' 'DEPLOY_DEV_PREVIEW=BLOCKED Cloudflare credentials are required' >&2
  exit 4
fi

if test -n "$remote_production_branch" && test "$branch" = "$remote_production_branch"; then
  printf 'DEPLOY_DEV_PREVIEW=FAIL branch %s is the production branch\n' "$branch" >&2
  exit 2
fi

printf 'DEPLOY_DEV_PREVIEW_DRY_RUN=PASS dir=%s project=%s branch=%s commit=%s' \
  "$dist" "$project" "$branch" "$commit"
if test -n "$remote_production_branch"; then
  printf ' production_branch=%s' "$remote_production_branch"
else
  printf ' production_branch=not-queried'
fi
printf '\n'

if test "${MAJUTSU_DEPLOY_EXECUTE:-no}" != yes; then exit 0; fi
if ! command -v wrangler >/dev/null 2>&1; then
  printf '%s\n' 'DEPLOY_DEV_PREVIEW=BLOCKED wrangler is not installed' >&2
  exit 4
fi

exec wrangler pages deploy "$dist" \
  --project-name "$project" \
  --branch "$branch" \
  --commit-hash "$commit" \
  --commit-message "Majutsu development docs $commit"
