import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { defineCollection } from 'astro:content';
import { docsSchema } from '@astrojs/starlight/schema';
import { z } from 'astro/zod';
import { orgDocsLoader } from './loaders/org-docs-loader';

const defaultRepoRoot = fileURLToPath(new URL('../../../', import.meta.url));
const repoRoot = path.resolve(process.env.MAJUTSU_DOCS_REPO_ROOT ?? defaultRepoRoot);
const manifest = process.env.MAJUTSU_DOCS_MANIFEST ?? '.cache/majutsu-docs/compiled/manifest.json';
const sourceRevision = process.env.MAJUTSU_DOCS_SOURCE_REVISION?.trim() || undefined;

const orgPageDataSchema = z.object({
  org: z.object({
    pageId: z.string(),
    logicalPath: z.string(),
    version: z.string(),
    route: z.string(),
    source: z.object({
      path: z.string(),
      startLine: z.number().int().positive().optional(),
      endLine: z.number().int().positive().optional(),
    }),
    sourceRevision: z.string().regex(/^[a-f0-9]{40}$/).optional(),
  }),
});

const docs = defineCollection({
  loader: orgDocsLoader({
    repoRoot,
    manifest,
    sourceRevision,
  }),
  schema: docsSchema({ extend: orgPageDataSchema }),
});

export const collections = { docs };
