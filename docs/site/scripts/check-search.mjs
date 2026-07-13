import { spawn } from 'node:child_process';
import { readFile } from 'node:fs/promises';
import path from 'node:path';
import process from 'node:process';
import { fileURLToPath } from 'node:url';

const [rawDist, manifestPath, fixturePath] = process.argv.slice(2);
if (!rawDist || !manifestPath || !fixturePath) {
  throw new Error('usage: check-search.mjs DIST_ROOT MANIFEST SEARCH_FIXTURE');
}
const dist = path.resolve(rawDist);
const [manifest, fixture] = await Promise.all(
  [manifestPath, fixturePath].map(async (file) => JSON.parse(await readFile(file, 'utf8'))),
);
if (fixture.schemaVersion !== 1) throw new Error('Unsupported search fixture schema');

const port = 4331 + Math.floor(Math.random() * 500);
const origin = `http://127.0.0.1:${port}`;
const server = spawn(process.execPath, [fileURLToPath(new URL('./serve-dist.mjs', import.meta.url)), dist, '--port', String(port)], {
  stdio: ['ignore', 'pipe', 'inherit'],
});
try {
  await waitForServer(origin);
  const pagefindSource = await readFile(path.join(dist, 'pagefind/pagefind.js'), 'utf8');
  const pagefind = await import(`data:text/javascript;base64,${Buffer.from(pagefindSource).toString('base64')}`);
  const index = pagefind.createInstance({ basePath: `${origin}/pagefind/`, baseUrl: origin, noWorker: true });
  await index.init();

  for (const testCase of fixture.queries) {
    const actual = await searchRoutes(index, testCase.query, 3);
    const expected = testCase.expectedRoutes;
    if (!actual.length || !expected.length || expected.some((route) => !actual.includes(route))) {
      throw new Error(`Search top3 mismatch for ${JSON.stringify(testCase.query)}: ${JSON.stringify(actual)}`);
    }
  }

  const published = manifest.pages.filter((page) => !page.landing);
  for (const page of published) {
    const routes = await searchRoutes(index, page.title, 20);
    if (!routes.includes(page.route)) {
      throw new Error(`Published page is not searchable by title: ${page.id} (${page.title})`);
    }
  }
  await index.destroy();
  console.log(`DOCS_SEARCH_CHECK=PASS queries=${fixture.queries.length} searchable_pages=${published.length} top3=expected-identities`);
} finally {
  server.kill('SIGTERM');
}

async function searchRoutes(index, query, limit) {
  const search = await index.search(query);
  const data = await Promise.all(search.results.slice(0, limit).map((result) => result.data()));
  return data.map(({ url }) => new URL(url, origin).pathname.replace(/\/+$/, '/') || '/');
}

async function waitForServer(url) {
  for (let attempt = 0; attempt < 50; attempt += 1) {
    if (server.exitCode !== null) throw new Error(`Static server exited with ${server.exitCode}`);
    try {
      const response = await fetch(url);
      if (response.ok) return;
    } catch {}
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
  throw new Error(`Timed out waiting for ${url}`);
}
