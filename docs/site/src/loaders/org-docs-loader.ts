import { createHash } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import path from 'node:path';
import type { Loader, LoaderContext } from 'astro/loaders';
import { z } from 'astro/zod';
import { parseFragment, serialize } from 'parse5';
import { createHighlighter } from 'shiki';

const highlighterPromise = createHighlighter({
  themes: ['github-light', 'github-dark'],
  langs: ['emacs-lisp', 'sh'],
});

const headingSchema = z.object({
  depth: z.number().int().min(1).max(6),
  slug: z.string().min(1),
  text: z.string().min(1),
});

const sourceSchema = z.object({
  path: z.string().min(1),
  startLine: z.number().int().positive().optional(),
  endLine: z.number().int().positive().optional(),
});

const manifestSchema = z.object({
  schemaVersion: z.literal(1),
  sources: z.array(
    z.object({
      path: z.string().min(1),
      sha256: z.string().regex(/^[a-f0-9]{64}$/),
    }),
  ),
  build: z.object({
    commitId: z.string().regex(/^[a-f0-9]{40}$/).optional(),
    contentHash: z.string().regex(/^[a-f0-9]{64}$/),
    unreleasable: z.boolean(),
  }),
  pages: z.array(
    z.object({
      id: z.string().min(1),
      path: z.string().min(1),
      version: z.string().min(1),
      route: z.string().min(1),
      title: z.string().min(1),
      description: z.string(),
      contentFile: z.string().min(1),
      digest: z.string().regex(/^[a-f0-9]{64}$/),
      source: sourceSchema,
      headings: z.array(headingSchema),
    }),
  ),
  navigation: z.array(z.unknown()),
  redirects: z.array(z.unknown()),
  diagnostics: z.array(z.object({ severity: z.string() }).passthrough()),
});

type LoaderOptions = { repoRoot: string; manifest: string; sourceRevision?: string };
type SyncContext = Pick<LoaderContext, 'logger' | 'parseData' | 'store'>;

export function orgDocsLoader(options: LoaderOptions): Loader {
  const repoRoot = path.resolve(options.repoRoot);
  const manifestPath = resolveInside(repoRoot, options.manifest, 'manifest');
  const artifactRoot = path.dirname(manifestPath);
  let watchedPaths = new Set<string>([manifestPath]);

  async function sync({ logger, parseData, store }: SyncContext) {
    const rawManifest = JSON.parse(await readFile(manifestPath, 'utf8')) as unknown;
    const manifest = manifestSchema.parse(rawManifest);

    if (manifest.build.unreleasable) {
      throw new Error('Refusing to load an unreleasable Loam manifest');
    }
    if (manifest.diagnostics.some(({ severity }) => severity === 'error')) {
      throw new Error('Refusing to load a Loam manifest containing error diagnostics');
    }

    const ids = new Set<string>();
    const pageIds = new Set<string>();
    const nextWatchedPaths = new Set<string>([manifestPath]);
    const entries = [];

    for (const [index, page] of manifest.pages.entries()) {
      const id = entryIdFromRoute(page.route);
      if (ids.has(id)) throw new Error(`Duplicate docs entry route: ${page.route}`);
      if (pageIds.has(page.id)) throw new Error(`Duplicate Loam page id: ${page.id}`);
      ids.add(id);
      pageIds.add(page.id);

      const contentPath = resolveInside(artifactRoot, page.contentFile, 'contentFile');
      const sourcePath = resolveInside(repoRoot, page.source.path, 'source');
      const html = await readFile(contentPath, 'utf8');
      const actualDigest = sha256(html);
      if (actualDigest !== page.digest) {
        throw new Error(
          `Digest mismatch for ${page.contentFile}: expected ${page.digest}, got ${actualDigest}`,
        );
      }
      const renderedHtml = await highlightSourceBlocks(html);

      const data = await parseData({
        id,
        data: {
          title: page.title,
          description: page.description,
          editUrl: sourceUrl(page.source, options.sourceRevision),
          sidebar: { order: index + 1 },
          org: {
            pageId: page.id,
            logicalPath: page.path,
            version: page.version,
            route: page.route,
            source: page.source,
            sourceRevision: options.sourceRevision,
          },
        },
      });

      entries.push({
        id,
        data,
        body: renderedHtml,
        filePath: path.relative(repoRoot, sourcePath),
        digest: sha256(`shiki-v1\0${page.digest}\0${renderedHtml}`),
        rendered: { html: renderedHtml, metadata: { headings: page.headings } },
      });
      nextWatchedPaths.add(contentPath);
      nextWatchedPaths.add(sourcePath);
    }

    store.clear();
    for (const entry of entries) store.set(entry);
    watchedPaths = nextWatchedPaths;
    logger.info(`Loaded ${entries.length} Org pages from Loam Manifest v1`);
  }

  return {
    name: 'loam-org-docs-loader',
    async load(context) {
      await sync(context);
      context.watcher?.add([...watchedPaths]);
      context.watcher?.on('change', async (changedPath) => {
        if (!watchedPaths.has(path.resolve(changedPath))) return;
        context.logger.info(`Reloading after ${path.relative(repoRoot, changedPath)} changed`);
        await sync(context);
        context.watcher?.add([...watchedPaths]);
      });
    },
  } satisfies Loader;
}

function entryIdFromRoute(route: string): string {
  if (!route.startsWith('/') || route.includes('?') || route.includes('#')) {
    throw new Error(`Manifest route must be an absolute pathname: ${route}`);
  }
  const id = route.replace(/^\/+|\/+$/g, '');
  if (!id || id.split('/').includes('..')) {
    throw new Error(`Manifest route cannot produce a safe entry id: ${route}`);
  }
  return id;
}

function resolveInside(root: string, relativePath: string, field: string): string {
  if (path.isAbsolute(relativePath)) throw new Error(`${field} must be relative: ${relativePath}`);
  const resolved = path.resolve(root, relativePath);
  if (resolved !== root && !resolved.startsWith(`${root}${path.sep}`)) {
    throw new Error(`${field} escapes its configured root: ${relativePath}`);
  }
  return resolved;
}

function sha256(value: string): string {
  return createHash('sha256').update(value).digest('hex');
}

async function highlightSourceBlocks(html: string): Promise<string> {
  const fragment: any = parseFragment(html);
  const highlighter = await highlighterPromise;
  const visit = (node: any) => {
    for (const child of [...(node.childNodes ?? [])]) visit(child);
    if (node.tagName !== 'pre') return;
    const code = (node.childNodes ?? []).find((child: any) => child.tagName === 'code');
    const languageClass = code?.attrs?.find((item: any) => item.name === 'class')?.value
      ?.split(/\s+/).find((item: string) => item.startsWith('language-'));
    const language = languageClass?.slice('language-'.length);
    if (!language || !['emacs-lisp', 'sh'].includes(language)) return;
    const highlightedFragment: any = parseFragment(highlighter.codeToHtml(textContent(code), {
      lang: language,
      themes: { light: 'github-light', dark: 'github-dark' },
    }));
    const highlighted = highlightedFragment.childNodes?.[0];
    if (!highlighted) throw new Error(`Shiki produced no output for ${language}`);
    const classAttribute = highlighted.attrs.find((item: any) => item.name === 'class');
    if (classAttribute) classAttribute.value = `${classAttribute.value} org-src-block`;
    else highlighted.attrs.push({ name: 'class', value: 'shiki org-src-block' });
    highlighted.attrs.push({ name: 'data-language', value: language });
    const highlightedCode = highlighted.childNodes?.find((child: any) => child.tagName === 'code');
    highlightedCode?.attrs?.push({ name: 'class', value: languageClass });
    const siblings = node.parentNode.childNodes;
    siblings[siblings.indexOf(node)] = highlighted;
    highlighted.parentNode = node.parentNode;
  };
  visit(fragment);
  return serialize(fragment);
}

function textContent(node: any): string {
  return node?.nodeName === '#text'
    ? node.value
    : (node?.childNodes ?? []).map(textContent).join('');
}

function sourceUrl(source: z.infer<typeof sourceSchema>, revision?: string): string {
  const action = revision ? `blob/${revision}` : 'edit/main';
  const url = new URL(`https://github.com/0WD0/majutsu/${action}/${source.path}`);
  if (source.startLine) {
    url.hash = source.endLine && source.endLine !== source.startLine
      ? `L${source.startLine}-L${source.endLine}`
      : `L${source.startLine}`;
  }
  return url.toString();
}
