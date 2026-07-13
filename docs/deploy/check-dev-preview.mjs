#!/usr/bin/env node
import { createServer } from 'node:http';
import { lstat, readFile, readdir, stat } from 'node:fs/promises';
import path from 'node:path';

const root = path.resolve(process.argv[2] ?? 'dist');
const docsRoot = path.join(root, 'docs');
const devRoot = path.join(docsRoot, 'dev');
const introduction = 'docs/dev/guide/introduction/index.html';
const redirectsText = [
  '/docs /docs/dev/guide/introduction/ 302',
  '/docs/ /docs/dev/guide/introduction/ 302',
  '/docs/dev /docs/dev/guide/introduction/ 302',
  '/docs/dev/ /docs/dev/guide/introduction/ 302',
  '',
].join('\n');

assert((await lstat(devRoot).catch(() => null))?.isDirectory(), 'missing /docs/dev output');

const headersPath = path.join(root, '_headers');
const headersText = await readFile(headersPath, 'utf8').catch(() => '');

const files = await inventory(root);
assert(files.includes(introduction), 'introduction route missing');
assert(
  !files.some((name) => name.startsWith('docs/') && !name.startsWith('docs/dev/')),
  'non-development docs route present',
);
assert(await readFile(path.join(root, '_redirects'), 'utf8') === redirectsText, 'redirect policy differs');

const requiredHeaders = [
  'Content-Security-Policy:',
  'Permissions-Policy:',
  'Referrer-Policy:',
  'X-Content-Type-Options:',
];
const globalHeaders = parseGlobalHeaders(headersText);
assert(
  /\bnoindex\b/i.test(globalHeaders['X-Robots-Tag'] ?? ''),
  'global noindex header missing',
);
for (const header of requiredHeaders) assert(headersText.includes(header), `security header missing: ${header}`);
assert(
  await readFile(path.join(root, 'robots.txt'), 'utf8') === 'User-agent: *\nDisallow: /\n',
  'robots policy differs',
);

const redirects = new Map(redirectsText.trim().split('\n').map((line) => {
  const [from, to, statusCode] = line.split(/\s+/);
  return [from, { to, statusCode: Number(statusCode) }];
}));
const server = createServer(async (request, response) => {
  const url = new URL(request.url, 'http://localhost');
  const responseHeaders = { ...globalHeaders, ...cacheHeaders(url.pathname) };
  const redirect = redirects.get(url.pathname);
  if (redirect) {
    response.writeHead(redirect.statusCode, { ...responseHeaders, location: redirect.to });
    response.end();
    return;
  }

  let relative = decodeURIComponent(url.pathname).replace(/^\/+/, '');
  if (!relative || relative.endsWith('/')) relative += 'index.html';
  const target = path.resolve(root, relative);
  try {
    assert(target.startsWith(`${root}${path.sep}`), 'unsafe request path');
    assert((await stat(target)).isFile(), 'not a file');
    response.writeHead(200, responseHeaders);
    response.end(await readFile(target));
  } catch {
    response.writeHead(404, responseHeaders);
    response.end('not found');
  }
});

await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
try {
  const origin = `http://127.0.0.1:${server.address().port}`;
  for (const route of ['/docs', '/docs/', '/docs/dev', '/docs/dev/']) {
    const response = await fetch(`${origin}${route}`, { redirect: 'manual' });
    assert(response.status === 302, `${route} does not redirect`);
    assert(response.headers.get('location') === '/docs/dev/guide/introduction/', `${route} redirect differs`);
    assertPreviewHeaders(response, route, requiredHeaders);
  }

  const intro = await fetch(`${origin}/docs/dev/guide/introduction/`);
  assert(intro.status === 200, 'introduction is not reachable');
  assertPreviewHeaders(intro, 'introduction', requiredHeaders);
  assert(isRevalidated(intro.headers.get('cache-control')), 'HTML cache policy differs');

  const nonDev = await fetch(`${origin}/docs/not-a-development-route/`, { redirect: 'manual' });
  assert(nonDev.status === 404, 'non-development route is reachable');
  assertPreviewHeaders(nonDev, 'non-development 404', requiredHeaders);

  const asset = files.find((name) => name.startsWith('_astro/'));
  if (asset) {
    const response = await fetch(`${origin}/${asset}`);
    assert(response.status === 200, 'hashed asset is not reachable');
    assert(/\bimmutable\b/i.test(response.headers.get('cache-control') ?? ''), 'hashed asset is not immutable');
  }
} finally {
  await new Promise((resolve) => server.close(resolve));
}

process.stdout.write(`DEV_PREVIEW_CHECK=PASS files=${files.length} route=/docs/dev/ noindex=true\n`);

async function inventory(dir, prefix = '') {
  const result = [];
  for (const entry of await readdir(dir, { withFileTypes: true })) {
    const name = prefix ? `${prefix}/${entry.name}` : entry.name;
    const absolute = path.join(dir, entry.name);
    const info = await lstat(absolute);
    assert(!info.isSymbolicLink(), `symlink present: ${name}`);
    if (entry.isDirectory()) result.push(...await inventory(absolute, name));
    else if (entry.isFile()) result.push(name);
  }
  return result.sort();
}

function parseGlobalHeaders(text) {
  const result = {};
  const block = text.match(/^\/\*\n((?:[ \t]+[^\n]+\n)*)/);
  assert(block, 'global header block is invalid');
  for (const line of block[1].trim().split('\n')) {
    const separator = line.indexOf(':');
    assert(separator > 0, `invalid header line: ${line}`);
    result[line.slice(0, separator).trim()] = line.slice(separator + 1).trim();
  }
  return result;
}

function cacheHeaders(pathname) {
  if (pathname.startsWith('/_astro/')) {
    return { 'Cache-Control': 'public, max-age=31536000, immutable' };
  }
  return { 'Cache-Control': 'public, max-age=0, must-revalidate' };
}

function assertPreviewHeaders(response, label, securityHeaders) {
  assert(/\bnoindex\b/i.test(response.headers.get('x-robots-tag') ?? ''), `${label} lacks noindex`);
  for (const header of securityHeaders) {
    assert(response.headers.has(header.slice(0, -1)), `${label} lacks ${header}`);
  }
}

function isRevalidated(value) {
  return /\bmax-age=0\b/i.test(value ?? '') && /\bmust-revalidate\b/i.test(value ?? '');
}

function assert(value, message) {
  if (!value) throw new Error(`DEV_PREVIEW_CHECK=FAIL ${message}`);
}
