import AxeBuilder from '@axe-core/playwright';
import { expect, test } from '@playwright/test';

const deepRoute = '/docs/dev/workflows/manipulating/';

test('root and deep documentation navigation work', async ({ page }) => {
  await page.goto('/');
  await expect(page.locator('h1')).toContainText('Shape history');
  await expect(page.getByRole('link', { name: 'one Org source' })).toHaveAttribute(
    'href',
    /github\.com\/0WD0\/majutsu\/blob\/[a-f0-9]{40}\/docs\/majutsu\.org/,
  );
  await page.goto(`${deepRoute}#interactive-patching`);
  await expect(page.locator('#interactive-patching')).toBeVisible();
  await expect(page.locator('nav.sidebar a[aria-current="page"]')).toContainText('Manipulating');
  await expect(page.locator('a[rel="prev"]')).toBeVisible();
  await expect(page.locator('a[rel="next"]')).toBeVisible();
  const source = page.getByRole('link', { name: 'View source' });
  await expect(source).toHaveAttribute(
    'href',
    /github\.com\/0WD0\/majutsu\/blob\/[a-f0-9]{40}\/docs\/majutsu\.org/,
  );
  const report = page.getByRole('link', { name: 'Report issue' });
  const reportHref = await report.getAttribute('href');
  expect(reportHref).toContain('github.com/0WD0/majutsu/issues/new');
  const issue = new URL(reportHref);
  expect(issue.searchParams.get('body')).toContain('https://majutsu.org/docs/dev/workflows/manipulating/');
  expect(issue.searchParams.get('body')).toMatch(/Version: dev\nBuild: [a-f0-9]{40}/);
});

test('search and theme controls work', async ({ page }) => {
  await page.goto('/docs/dev/guide/introduction/');
  const searchButton = page.getByRole('button', { name: 'Search' });
  await expect(searchButton).toBeEnabled();
  await searchButton.click();
  const input = page.getByRole('textbox', { name: 'Search' });
  await input.fill('interactive patching');
  await expect(page.locator('#starlight__search a').first()).toHaveAttribute('href', /workflows\/manipulating/);
  await page.keyboard.press('Escape');
  const theme = page.locator('starlight-theme-select select:visible').first();
  await theme.selectOption('light');
  await expect(page.locator('html')).toHaveAttribute('data-theme', 'light');
  await page.reload();
  await expect(page.locator('html')).toHaveAttribute('data-theme', 'light');
});

test('documentation mode adapts workflows across pages', async ({ page }) => {
  await page.goto('/docs/dev/guide/getting-started/');
  const vanillaLoop = page
    .locator('p[data-keymap-scope="Vanilla"]')
    .filter({ hasText: 'A safe first loop' });
  const evilLoop = page
    .locator('p[data-keymap-scope="Evil"]')
    .filter({ hasText: 'A safe first loop' });
  await expect(page.locator('html')).toHaveAttribute('data-majutsu-docs-mode', 'vanilla');
  await expect(vanillaLoop).toBeVisible();
  await expect(evilLoop).toBeHidden();
  await expect(vanillaLoop).toContainText('refresh with g');

  await page.getByRole('button', { name: 'Evil', exact: true }).click();
  await expect(page.locator('html')).toHaveAttribute('data-majutsu-docs-mode', 'evil');
  await expect(vanillaLoop).toBeHidden();
  await expect(evilLoop).toBeVisible();
  await expect(evilLoop).toContainText('refresh with g r');

  await page.goto('/docs/dev/guide/interface-concepts/');
  await expect(page.locator('html')).toHaveAttribute('data-majutsu-docs-mode', 'evil');
  await expect(
    page.locator('p[data-keymap-scope="Evil"]').filter({ hasText: 'Press `' }),
  ).toBeVisible();
  await expect(
    page.locator('p[data-keymap-scope="Vanilla"]').filter({ hasText: 'Press $' }),
  ).toBeHidden();

  await page.goto('/docs/dev/workflows/conflict-resolution/#majutsu-conflict-mode-commands');
  await expect(page.locator('dl[data-keymap-scope="Evil"]')).toBeVisible();
  await expect(page.locator('dl[data-keymap-scope="Vanilla"]')).toBeHidden();
  await page.getByRole('button', { name: 'Vanilla', exact: true }).click();
  await expect(page.locator('dl[data-keymap-scope="Vanilla"]')).toBeVisible();
  await expect(page.locator('dl[data-keymap-scope="Evil"]')).toBeHidden();
});

test('module semantics keeps metadata outside the description grid', async ({ page }) => {
  await page.goto('/docs/dev/workflows/inspecting/#module-semantics');
  const heading = page.locator('#module-semantics');
  const semantics = heading.locator('xpath=following-sibling::dl[1]');
  await expect(semantics.locator(':scope > dt')).toHaveCount(4);
  await expect(semantics.locator(':scope > dd')).toHaveCount(4);
  await expect(semantics.locator(':scope > :not(dt):not(dd)')).toHaveCount(0);
  expect(await semantics.evaluate((list) => list.scrollWidth - list.clientWidth)).toBeLessThanOrEqual(1);

  const metadata = page.locator('#row-metadata');
  await expect(metadata).toBeVisible();
  await expect(metadata.locator('xpath=following-sibling::*[1]')).toHaveJSProperty('tagName', 'P');
  await expect(metadata.locator('xpath=following-sibling::*[2]')).toHaveJSProperty('tagName', 'P');
});

test('keystroke index exposes authored binding prerequisites', async ({ page }) => {
  await page.goto('/docs/dev/reference/keystrokes/');
  const entries = page.locator('.org-reference-list > dt');
  await expect(entries.first()).toBeVisible();
  await expect(entries).toHaveCount(await page.locator('dt[data-reference-interface]').count());
  const commandBindings = page.locator('dt[data-reference-kind="command-binding"]');
  await expect(commandBindings).toHaveCount(
    await page.locator('dt[data-reference-kind="command-binding"][data-reference-command]').count(),
  );

  const conflict = page.locator(
    'dt[data-reference-prefix="C-c ^"][data-reference-identity="n / p"]',
  );
  await expect(conflict).toHaveCount(1);
  await expect(conflict).toHaveAttribute('data-reference-kind', 'command-binding');
  await expect(conflict).toHaveAttribute('data-reference-interface', 'Conflict buffer');
  await expect(conflict).toHaveAttribute('data-reference-mode', 'majutsu-conflict-mode');
  await expect(conflict.getByText('C-c ^', { exact: true })).toBeVisible();

  const vanilla = page.getByRole('button', { name: 'Vanilla', exact: true });
  const evil = page.getByRole('button', { name: 'Evil', exact: true });
  const vanillaProcess = page.locator(
    'dt[data-reference-command="majutsu-process-buffer"][data-reference-scope="Vanilla"]',
  );
  const evilProcess = page.locator(
    'dt[data-reference-command="majutsu-process-buffer"][data-reference-scope="Evil"]',
  );
  await expect(vanilla).toHaveAttribute('aria-pressed', 'true');
  await expect(evil).toHaveAttribute('aria-pressed', 'false');
  await expect(vanillaProcess).toBeVisible();
  await expect(evilProcess).toBeHidden();
  expect((await page.locator('.org-reference-key').allTextContents()).join(' ')).not.toMatch(
    /\((?:Vanilla|Evil)\)/,
  );

  await evil.click();
  await expect(evil).toHaveAttribute('aria-pressed', 'true');
  await expect(vanilla).toHaveAttribute('aria-pressed', 'false');
  await expect(evilProcess).toBeVisible();
  await expect(vanillaProcess).toBeHidden();
  await expect(page.locator('.keymap-switcher-count')).toHaveText(/\d+ of \d+ bindings/);
  await page.reload();
  await expect(evil).toHaveAttribute('aria-pressed', 'true');
  await expect(evilProcess).toBeVisible();
  await expect(vanillaProcess).toBeHidden();
});

test('@mobile mobile navigation and layout work', async ({ page }) => {
  await page.goto('/docs/dev/guide/getting-started/');
  const menu = page.getByRole('button', { name: 'Menu' });
  await expect(menu).toHaveAttribute('aria-expanded', 'false');
  await menu.click();
  await expect(page.locator('body')).toHaveAttribute('data-mobile-menu-expanded');
  await expect(page.locator('starlight-menu-button')).toHaveAttribute('aria-expanded', 'true');
  await expect(menu).toHaveAttribute('aria-expanded', 'true');
  const dimensions = await page.evaluate(() => ({ width: document.documentElement.clientWidth, scroll: document.documentElement.scrollWidth }));
  expect(dimensions.scroll).toBeLessThanOrEqual(dimensions.width + 1);
  await menu.click();
  await expect(menu).toHaveAttribute('aria-expanded', 'false');
});

test('404 documentation home copy matches the build channel', async ({ page }) => {
  const response = await page.goto('/404-does-not-exist');
  expect(response.status()).toBe(404);
  await expect(page.getByRole('link', { name: 'Development docs' })).toHaveAttribute(
    'href',
    '/docs/dev/guide/introduction/',
  );
  await expect(page.getByRole('link', { name: 'Stable docs' })).toHaveCount(0);
});

test('representative pages have no serious accessibility violations or overflow', async ({ page }) => {
  for (const route of ['/', '/docs/dev/guide/getting-started/', deepRoute, '/docs/dev/reference/keystrokes/', '/404-does-not-exist']) {
    const response = await page.goto(route);
    if (route.includes('does-not-exist')) expect(response.status()).toBe(404);
    await expect(page.locator('h1')).toBeVisible();
    const results = await new AxeBuilder({ page }).analyze();
    const blocking = results.violations.filter(({ impact }) => impact === 'serious' || impact === 'critical');
    expect(blocking, `${route}: ${blocking.map(({ id }) => id).join(', ')}`).toEqual([]);
    const overflow = await page.evaluate(() => document.documentElement.scrollWidth - document.documentElement.clientWidth);
    expect(overflow, `${route} horizontal overflow`).toBeLessThanOrEqual(1);
  }
});

test('content remains usable without JavaScript', async ({ browser }) => {
  const context = await browser.newContext({ javaScriptEnabled: false });
  const page = await context.newPage();
  await page.goto('/docs/dev/workflows/inspecting/#log-buffer');
  await expect(page.locator('h1')).toHaveText('Inspecting');
  await expect(page.locator('#log-buffer')).toBeVisible();
  await expect(page.locator('nav.sidebar a[href="/docs/dev/workflows/inspecting/"]')).toHaveCount(1);
  await page.goto('/docs/dev/reference/keystrokes/');
  await expect(page.locator('.keymap-switcher')).toBeHidden();
  await expect(
    page.locator('dt[data-reference-command="majutsu-process-buffer"][data-reference-scope="Vanilla"]'),
  ).toBeVisible();
  await expect(
    page.locator('dt[data-reference-command="majutsu-process-buffer"][data-reference-scope="Evil"]'),
  ).toBeVisible();
  await page.goto('/docs/dev/guide/getting-started/');
  await expect(
    page.locator('p[data-keymap-scope="Vanilla"]').filter({ hasText: 'A safe first loop' }),
  ).toBeVisible();
  await expect(
    page.locator('p[data-keymap-scope="Evil"]').filter({ hasText: 'A safe first loop' }),
  ).toBeVisible();
  await context.close();
});

test('source blocks are highlighted and copy progressively', async ({ page }) => {
  await page.addInitScript(() => {
    Object.defineProperty(Navigator.prototype, 'clipboard', {
      configurable: true,
      get: () => ({ writeText: async (value) => localStorage.setItem('majutsu-test-clipboard', value) }),
    });
  });
  await page.goto('/docs/dev/extend/template-dsl/');
  const source = page.locator('figure.org-src-block > pre.shiki').first();
  await expect(source.locator('.line').first()).toBeVisible();
  const copy = page.locator('button.copy-code').first();
  await expect(copy).toHaveAccessibleName('Copy code');
  await copy.click();
  await expect(copy).toHaveAccessibleName('Copied');
  await expect(copy).toHaveAttribute('data-state', 'copied');
  expect(await page.evaluate(() => localStorage.getItem('majutsu-test-clipboard'))).toContain('root');
});
