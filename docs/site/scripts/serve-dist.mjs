import { createReadStream, statSync } from 'node:fs';
import { createServer } from 'node:http';
import path from 'node:path';
import process from 'node:process';

const root = path.resolve(process.argv[2] ?? '../../dist');
const portIndex = process.argv.indexOf('--port');
const port = Number(portIndex === -1 ? 4329 : process.argv[portIndex + 1]);
const types = new Map([
  ['.css', 'text/css; charset=utf-8'], ['.html', 'text/html; charset=utf-8'],
  ['.js', 'text/javascript; charset=utf-8'], ['.json', 'application/json'],
  ['.svg', 'image/svg+xml'], ['.wasm', 'application/wasm'],
  ['.pagefind', 'application/octet-stream'], ['.pf_fragment', 'application/octet-stream'],
  ['.pf_index', 'application/octet-stream'], ['.pf_meta', 'application/octet-stream'],
]);

createServer((request, response) => {
  const url = new URL(request.url ?? '/', 'http://localhost');
  let relative = decodeURIComponent(url.pathname).replace(/^\/+/, '');
  let status = 200;
  let candidate = path.resolve(root, relative || 'index.html');
  if (candidate !== root && !candidate.startsWith(`${root}${path.sep}`)) {
    response.writeHead(400).end('Bad request');
    return;
  }
  try {
    if (statSync(candidate).isDirectory()) candidate = path.join(candidate, 'index.html');
    if (!statSync(candidate).isFile()) throw new Error('not a file');
  } catch {
    candidate = path.join(root, '404.html');
    status = 404;
  }
  response.writeHead(status, {
    'content-type': types.get(path.extname(candidate)) ?? 'application/octet-stream',
    'cache-control': 'no-store',
  });
  createReadStream(candidate).pipe(response);
}).listen(port, '127.0.0.1', () => console.log(`Serving ${root} at http://127.0.0.1:${port}`));
