import { fileURLToPath } from 'node:url';
import { readFileSync } from 'node:fs';
import path from 'node:path';
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

const repoRoot = path.resolve(
  process.env.MAJUTSU_DOCS_REPO_ROOT ?? fileURLToPath(new URL('../../', import.meta.url)),
);
const manifestPath = path.resolve(
  repoRoot,
  process.env.MAJUTSU_DOCS_MANIFEST ?? '.cache/majutsu-docs/compiled/manifest.json',
);
const outDir = path.resolve(
  process.env.MAJUTSU_DOCS_OUT_DIR ?? fileURLToPath(new URL('../../dist/', import.meta.url)),
);
const cacheDir = path.resolve(
  process.env.MAJUTSU_DOCS_CACHE_DIR ?? fileURLToPath(new URL('../../.cache/majutsu-docs/astro/', import.meta.url)),
);
const manifest = JSON.parse(readFileSync(manifestPath, 'utf8'));
const docsCommit = manifest.build.commitId ?? null;

const groupLabels = new Map([
  ['guide', 'Guide'],
  ['workflows', 'Workflows'],
  ['repository', 'Repository'],
  ['extend', 'Extend'],
  ['reference', 'Reference'],
]);
const sidebar = [...groupLabels].map(([prefix, label]) => ({
  label,
  items: manifest.pages
    .filter((page) => page.path.startsWith(`${prefix}/`))
    .map((page) => ({ label: page.title, link: page.route })),
}));

export default defineConfig({
  site: 'https://majutsu.org',
  outDir,
  emptyOutDir: true,
  vite: {
    define: {
      __MAJUTSU_DOCS_COMMIT__: JSON.stringify(docsCommit),
    },
  },
  cacheDir,
  integrations: [
    starlight({
      title: 'Majutsu',
      description: 'A Magit-inspired interface for Jujutsu in Emacs.',
      favicon: '/favicon.svg',
      head: [
        {
          tag: 'link',
          attrs: { rel: 'apple-touch-icon', href: '/apple-touch-icon.png' },
        },
        {
          tag: 'meta',
          attrs: { property: 'og:image', content: 'https://majutsu.org/og.png' },
        },
        {
          tag: 'meta',
          attrs: { name: 'twitter:card', content: 'summary_large_image' },
        },
        {
          tag: 'meta',
          attrs: { name: 'twitter:image', content: 'https://majutsu.org/og.png' },
        },
      ],
      components: {
        Footer: './src/components/Footer.astro',
        MarkdownContent: './src/components/OrgContent.astro',
        MobileMenuToggle: './src/components/MobileMenuToggle.astro',
        SiteTitle: './src/components/SiteTitle.astro',
      },
      customCss: ['./src/styles/org.css'],
      sidebar,
      tableOfContents: { minHeadingLevel: 2, maxHeadingLevel: 4 },
      social: [
        { icon: 'github', label: 'GitHub', href: 'https://github.com/0WD0/majutsu' },
      ],
    }),
  ],
});
