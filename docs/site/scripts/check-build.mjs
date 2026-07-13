import { access, readFile, readdir, stat } from 'node:fs/promises';
import path from 'node:path';
import process from 'node:process';
import { gzipSync } from 'node:zlib';
import { parse } from 'parse5';

const [manifestPath, reportPath, rawDistRoot] = process.argv.slice(2);
if (!manifestPath || !reportPath || !rawDistRoot) {
  throw new Error('usage: check-build.mjs MANIFEST REPORT DIST_ROOT');
}
const distRoot = path.resolve(rawDistRoot);
const redirectSources = new Set(
  (await readFile(path.join(distRoot, '_redirects'), 'utf8'))
    .split(/\r?\n/)
    .map((line) => line.trim().split(/\s+/)[0])
    .filter((source) => source?.startsWith('/')),
);
const [manifest, report] = await Promise.all(
  [manifestPath, reportPath].map(async (file) => JSON.parse(await readFile(file, 'utf8'))),
);

assert(manifest.schemaVersion === 1 && report.schemaVersion === 1, 'Unexpected Manifest/report schema');
assert(!manifest.build.unreleasable && !report.unreleasable, 'Site artifacts are marked unreleasable');
assert(!manifest.diagnostics.length && !report.diagnostics.length, 'Site artifacts contain diagnostics');
assert(
  manifest.pages.length === report.pageCount,
  `Manifest/report page count mismatch: ${manifest.pages.length}/${report.pageCount}`,
);

const navigationIds = [];
const visitNavigation = (item) => {
  navigationIds.push(item.id);
  for (const child of item.children ?? []) visitNavigation(child);
};
for (const item of manifest.navigation) visitNavigation(item);
assert(
  navigationIds.length === manifest.pages.length &&
    new Set(navigationIds).size === manifest.pages.length &&
    manifest.pages.every((page) => navigationIds.includes(page.id)),
  'Manifest navigation must cover every page exactly once',
);

const distFiles = await walkFiles(distRoot);
assert(!distFiles.some((file) => /\.(?:md|mdx)$/i.test(file)), 'Markdown source leaked into dist');
for (const file of distFiles.filter((file) => /\.(?:html|js|css|json|xml|svg)$/i.test(file))) {
  const contents = await readFile(file, 'utf8');
  assert(
    !/(?:file:\/\/|\/(?:home|Users|private\/tmp|tmp)\/|[A-Za-z]:\\(?:Users|home|tmp)\\)/.test(contents),
    `${posix(path.relative(distRoot, file))}: absolute machine path leaked`,
  );
}
const htmlFiles = distFiles.filter((file) => file.endsWith('.html'));
const documents = new Map();
for (const file of htmlFiles) {
  const relative = posix(path.relative(distRoot, file));
  const route = routeForHtml(relative);
  const html = await readFile(file, 'utf8');
  const tree = parse(html);
  const nodes = allElements(tree);
  const ids = new Set();
  for (const node of nodes) {
    const id = attr(node, 'id');
    if (!id) continue;
    assert(!ids.has(id), `${route}: duplicate id #${id}`);
    ids.add(id);
  }
  const canonicalNodes = nodes.filter((node) => node.tagName === 'link' && attrTokens(node, 'rel').includes('canonical'));
  assert(canonicalNodes.length === 1, `${route}: expected exactly one canonical link`);
  const expectedCanonicalRoute = relative === '404.html' ? '/404/' : route;
  assert(
    attr(canonicalNodes[0], 'href') === `https://majutsu.org${expectedCanonicalRoute}`,
    `${route}: canonical URL mismatch`,
  );
  const titles = nodes.filter((node) => node.tagName === 'title');
  assert(titles.length === 1 && textContent(titles[0]).trim(), `${route}: missing unique title`);
  const main = nodes.find((node) => node.tagName === 'main');
  assert(main, `${route}: missing main landmark`);
  const headings = allElements(main).filter((node) => /^h[1-6]$/.test(node.tagName));
  assert(headings.filter((node) => node.tagName === 'h1').length === 1, `${route}: expected one main h1`);
  let previousDepth = 0;
  for (const heading of headings) {
    const depth = Number(heading.tagName.slice(1));
    assert(previousDepth === 0 || depth <= previousDepth + 1, `${route}: heading level skips h${previousDepth} to h${depth}`);
    previousDepth = depth;
  }
  const icons = nodes.filter((node) => node.tagName === 'link' && attrTokens(node, 'rel').some((rel) => rel.includes('icon')));
  assert(icons.some((node) => attr(node, 'href') === '/favicon.svg'), `${route}: missing /favicon.svg`);
  assert(!/(?:file:\/\/|\/(?:home|Users|private\/tmp|tmp)\/|[A-Za-z]:\\(?:Users|home|tmp)\\)/.test(html), `${route}: absolute machine path leaked`);
  assert(!/href=["'][^"']+\.(?:md|mdx)(?:[?#"'])/i.test(html), `${route}: Markdown link leaked into build`);
  assert(!/(?:^|\n)\s*```/.test(textContent(main)), `${route}: raw Markdown fence leaked into content`);
  for (const code of nodes.filter((node) => node.tagName === 'code' && attrTokens(node, 'class').some((name) => name.startsWith('language-')))) {
    assert(
      code.parentNode?.tagName === 'pre' && attrTokens(code.parentNode, 'class').includes('shiki'),
      `${route}: source block bypassed build-time Shiki`,
    );
  }
  for (const node of nodes) {
    for (const name of ['href', 'src', 'action', 'poster']) {
      const value = attr(node, name);
      if (!value) continue;
      const normalized = value.trim().replace(/[\u0000-\u0020]+/g, '').toLowerCase();
      assert(!/^(?:javascript|vbscript|file|data):/.test(normalized), `${route}: unsafe ${name} URL ${value}`);
    }
  }
  documents.set(route, { file, html, ids, nodes, title: textContent(titles[0]).trim() });
}

assert(documents.has('/') && documents.has('/404.html'), 'Build must contain root index and 404.html');
assert(/Page not found/i.test(documents.get('/404.html').html), '404 page is missing its not-found message');
await access(path.join(distRoot, 'favicon.svg'));
const pinnedSource = `github.com/0WD0/majutsu/blob/${manifest.build.commitId}/docs/majutsu.org`;
assert(documents.get('/').html.includes(pinnedSource), 'Homepage source link is not pinned to the build commit');

for (const [route, document] of documents) {
  for (const node of document.nodes) {
    for (const name of ['href', 'src', 'action', 'poster']) {
      const raw = attr(node, name);
      if (!raw || raw.startsWith('//')) continue;
      let url;
      try { url = new URL(raw, `https://majutsu.org${route}`); } catch { throw new Error(`${route}: invalid ${name} URL ${raw}`); }
      if (url.origin !== 'https://majutsu.org') continue;
      const target = targetForPathname(url.pathname);
      assert(target, `${route}: internal ${name} escapes site root: ${raw}`);
      const targetStat = await safeStat(target);
      if (!targetStat && redirectSources.has(url.pathname)) continue;
      assert(targetStat?.isFile(), `${route}: broken internal ${name}: ${raw}`);
      if (!url.hash || !target.endsWith('.html')) continue;
      const targetRoute = routeForHtml(posix(path.relative(distRoot, target)));
      const targetDocument = documents.get(targetRoute);
      let fragment;
      try { fragment = decodeURIComponent(url.hash.slice(1)); }
      catch { throw new Error(`${route}: malformed percent encoding in fragment ${raw}`); }
      assert(targetDocument?.ids.has(fragment), `${route}: missing fragment ${raw}`);
    }
  }
}

for (const page of manifest.pages) {
  const document = documents.get(page.route);
  assert(document, `Built route is missing: ${page.route}`);
  assert(document.html.includes(page.title) && document.html.includes('org-document'), `Built route is missing Org content: ${page.route}`);
  assert(document.html.includes(pinnedSource), `${page.route}: source link is not pinned to the build commit`);
  const canonical = document.nodes.find((node) => node.tagName === 'link' && attrTokens(node, 'rel').includes('canonical'));
  assert(attr(canonical, 'href') === `https://majutsu.org${page.route}`, `${page.route}: page canonical mismatch`);
  const issue = document.nodes.find((node) => node.tagName === 'a' && attr(node, 'href')?.startsWith('https://github.com/0WD0/majutsu/issues/new?'));
  assert(issue, `${page.route}: missing Report issue link`);
  const issueBody = new URL(attr(issue, 'href')).searchParams.get('body') ?? '';
  assert(issueBody.includes(`Page: https://majutsu.org${page.route}`), `${page.route}: issue link misses page URL`);
  assert(issueBody.includes(`Version: ${page.version}`), `${page.route}: issue link misses version`);
  assert(issueBody.includes(`Build: ${manifest.build.commitId}`), `${page.route}: issue link misses build context`);
}
const totalBytes = await totalSize(distRoot);
const pagefindBytes = await totalSize(path.join(distRoot, 'pagefind'));
const largest = await largestFile(distRoot);
assert(totalBytes <= 3 * 1024 * 1024, `Bundle budget exceeded: dist is ${totalBytes} bytes (limit 3 MiB)`);
assert(pagefindBytes <= 2 * 1024 * 1024, `Bundle budget exceeded: Pagefind is ${pagefindBytes} bytes (limit 2 MiB)`);
const representative = documents.get('/docs/dev/guide/getting-started/');
assert(representative, 'Performance budget representative page is missing');
const externalScripts = uniqueAssetFiles(representative, 'script', 'src');
const externalStyles = representative.nodes
  .filter((node) => node.tagName === 'link' && attrTokens(node, 'rel').includes('stylesheet'))
  .map((node) => assetFile(attr(node, 'href')))
  .filter(Boolean);
const scriptGzipBytes = (await Promise.all(externalScripts.map(gzipFileSize))).reduce((sum, size) => sum + size, 0) +
  representative.nodes.filter((node) => node.tagName === 'script' && !attr(node, 'src'))
    .reduce((sum, node) => sum + gzipSync(Buffer.from(textContent(node))).length, 0);
const cssGzipBytes = (await Promise.all([...new Set(externalStyles)].map(gzipFileSize))).reduce((sum, size) => sum + size, 0);
const firstLoadGzipBytes = gzipSync(Buffer.from(representative.html)).length + scriptGzipBytes + cssGzipBytes;
assert(scriptGzipBytes < 30 * 1024, `Initial JavaScript gzip budget exceeded: ${scriptGzipBytes} bytes`);
assert(cssGzipBytes < 70 * 1024, `CSS gzip budget exceeded: ${cssGzipBytes} bytes`);
assert(firstLoadGzipBytes < 350 * 1024, `First-load gzip budget exceeded: ${firstLoadGzipBytes} bytes`);

console.log(
  `DOCS_CHECK=PASS pages=${manifest.pages.length} html=${documents.size} navigation=${navigationIds.length} ` +
    `links=valid diagnostics=0 dist_bytes=${totalBytes} pagefind_bytes=${pagefindBytes} largest_bytes=${largest.size} ` +
    `js_gzip=${scriptGzipBytes} css_gzip=${cssGzipBytes} first_load_gzip=${firstLoadGzipBytes}`,
);

function assert(condition, message) { if (!condition) throw new Error(message); }
function attr(node, name) { return node?.attrs?.find((item) => item.name === name)?.value; }
function attrTokens(node, name) { return (attr(node, name) ?? '').toLowerCase().split(/\s+/).filter(Boolean); }
function allElements(root) {
  const result = [];
  const visit = (node) => { if (node.tagName) result.push(node); for (const child of node.childNodes ?? []) visit(child); };
  visit(root);
  return result;
}
function textContent(node) { return node?.nodeName === '#text' ? node.value : (node?.childNodes ?? []).map(textContent).join(''); }
function posix(value) { return value.split(path.sep).join('/'); }
function routeForHtml(relative) {
  if (relative === 'index.html') return '/';
  if (relative.endsWith('/index.html')) return `/${relative.slice(0, -'index.html'.length)}`;
  return `/${relative}`;
}
function targetForPathname(pathname) {
  let decoded;
  try { decoded = decodeURIComponent(pathname); } catch { return null; }
  const target = path.resolve(distRoot, decoded.replace(/^\/+/, ''));
  if (target !== distRoot && !target.startsWith(`${distRoot}${path.sep}`)) return null;
  if (decoded === '/404/' || decoded === '/404') return path.join(distRoot, '404.html');
  if (path.extname(target)) return target;
  return path.join(target, 'index.html');
}
async function safeStat(file) { try { return await stat(file); } catch { return null; } }
async function walkFiles(root) {
  const result = [];
  for (const entry of await readdir(root, { withFileTypes: true })) {
    const file = path.join(root, entry.name);
    if (entry.isDirectory()) result.push(...await walkFiles(file));
    else if (entry.isFile()) result.push(file);
  }
  return result;
}
async function totalSize(root) {
  const info = await safeStat(root);
  if (!info) return 0;
  const files = await walkFiles(root);
  return (await Promise.all(files.map(async (file) => (await stat(file)).size))).reduce((a, b) => a + b, 0);
}
async function largestFile(root) {
  let best = { file: '', size: 0 };
  for (const file of await walkFiles(root)) {
    const size = (await stat(file)).size;
    if (size > best.size) best = { file: posix(path.relative(root, file)), size };
  }
  return best;
}
function assetFile(raw) {
  if (!raw) return null;
  const url = new URL(raw, 'https://majutsu.org/');
  if (url.origin !== 'https://majutsu.org') return null;
  const file = targetForPathname(url.pathname);
  return file && path.extname(file) ? file : null;
}
function uniqueAssetFiles(document, tagName, attribute) {
  return [...new Set(document.nodes
    .filter((node) => node.tagName === tagName)
    .map((node) => assetFile(attr(node, attribute)))
    .filter(Boolean))];
}
async function gzipFileSize(file) { return gzipSync(await readFile(file), { level: 9 }).length; }
