#!/usr/bin/env node
import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import path from 'node:path';

const [originArg, commitArg] = process.argv.slice(2);
if (!originArg) fail('origin is required');
const origin = new URL(originArg);
const repoRoot = path.resolve(fileURLToPath(new URL('../../', import.meta.url)));
const commit = commitArg || process.env.MAJUTSU_DOCS_COMMIT || currentCommit(repoRoot);
if (!commit) fail('build commit is required');

const redirectTarget = '/docs/dev/guide/introduction/';
for (const route of ['/docs', '/docs/', '/docs/dev', '/docs/dev/']) {
  const response = await request(route);
  assert(response.status === 302, `${route}: expected 302, got ${response.status}`);
  assert(response.headers.get('location') === redirectTarget, `${route}: redirect differs`);
  assertPreviewHeaders(response, route);
}

const intro = await request(redirectTarget);
assert(intro.status === 200, `introduction: expected 200, got ${intro.status}`);
assertPreviewHeaders(intro, 'introduction');
assert(isRevalidated(intro.headers.get('cache-control')), 'introduction cache policy differs');
const html = await intro.text();
assert(html.includes(commit), `introduction does not identify build commit ${commit}`);

const nonDev = await request('/docs/not-a-development-route/');
assert(nonDev.status === 404, `non-development route: expected 404, got ${nonDev.status}`);
assertPreviewHeaders(nonDev, 'non-development 404');

const assetPath = html.match(/["'](\/_astro\/[^"']+)["']/)?.[1];
assert(assetPath, 'introduction does not reference a hashed asset');
const asset = await request(assetPath);
assert(asset.status === 200, `hashed asset: expected 200, got ${asset.status}`);
assert(/\bimmutable\b/i.test(asset.headers.get('cache-control') ?? ''), 'hashed asset cache is not immutable');

const robots = await request('/robots.txt');
assert(robots.status === 200, `robots.txt: expected 200, got ${robots.status}`);
assert((await robots.text()) === 'User-agent: *\nDisallow: /\n', 'robots.txt permits crawling');

process.stdout.write(`REMOTE_SMOKE=PASS channel=dev origin=${origin.origin} commit=${commit}\n`);

function request(pathname) {
  return fetch(new URL(pathname, origin), { redirect: 'manual', signal: AbortSignal.timeout(10_000) });
}

function assertPreviewHeaders(response, label) {
  assert(/\bnoindex\b/i.test(response.headers.get('x-robots-tag') ?? ''), `${label} lacks noindex`);
  for (const header of [
    'content-security-policy',
    'permissions-policy',
    'referrer-policy',
    'x-content-type-options',
  ]) assert(response.headers.has(header), `${label} lacks ${header}`);
}

function currentCommit(root) {
  try {
    return execFileSync(
      'jj',
      ['--repository', root, 'log', '--no-graph', '-r', '@', '-T', 'commit_id'],
      { encoding: 'utf8', stdio: ['ignore', 'pipe', 'ignore'] },
    ).trim();
  } catch {
    return '';
  }
}

function isRevalidated(value) {
  return /\bmax-age=0\b/i.test(value ?? '') && /\bmust-revalidate\b/i.test(value ?? '');
}

function assert(value, message) {
  if (!value) fail(message);
}

function fail(message) {
  process.stderr.write(`REMOTE_SMOKE=FAIL ${message}\n`);
  process.exit(1);
}
