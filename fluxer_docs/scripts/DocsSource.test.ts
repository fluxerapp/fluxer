import assert from 'node:assert/strict';
import {mkdir, mkdtemp, rm, writeFile} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import path from 'node:path';
import {test} from 'node:test';
import {listDraftMarkdownFiles, listMarkdownFiles, slugifyHeading} from './DocsSource.ts';

test('Markdown discovery includes nested pages and excludes assets and dependencies', async (context) => {
	const directory = await mkdtemp(path.join(tmpdir(), 'fluxer-docs-source-'));
	context.after(() => rm(directory, {recursive: true, force: true}));
	await mkdir(path.join(directory, 'nested'));
	await mkdir(path.join(directory, 'node_modules'));
	await Promise.all(
		['index.md', 'nested/page.mdx', 'nested/image.svg', 'draft.mdx.bak', 'node_modules/README.md'].map((file) =>
			writeFile(path.join(directory, file), ''),
		),
	);
	assert.deepEqual((await listMarkdownFiles(directory)).sort(), [
		path.join(directory, 'index.md'),
		path.join(directory, 'nested/page.mdx'),
	]);
	await assert.rejects(listMarkdownFiles(path.join(directory, 'missing')), {code: 'ENOENT'});
});

test('Markdown discovery keeps draft pages apart from published pages', async (context) => {
	const directory = await mkdtemp(path.join(tmpdir(), 'fluxer-docs-source-'));
	context.after(() => rm(directory, {recursive: true, force: true}));
	await writeFile(path.join(directory, 'published.md'), '---\ntitle: Published\ndraft: false\n---\n');
	await writeFile(path.join(directory, 'withheld.mdx'), '---\ntitle: Withheld\ndraft: true\n---\n');
	await writeFile(path.join(directory, 'mentions.md'), '---\ntitle: Mentions\n---\n\ndraft: true\n');
	assert.deepEqual(await listMarkdownFiles(directory), [
		path.join(directory, 'mentions.md'),
		path.join(directory, 'published.md'),
	]);
	assert.deepEqual(await listDraftMarkdownFiles(directory), [path.join(directory, 'withheld.mdx')]);
});

for (const [heading, expected] of [
	['  JSON   Body  ', 'json-body'],
	['`user_id` and [display name](/users)', 'userid-and-display-name'],
	['<span id="user">User</span> object', 'user-object'],
	['OAuth2: tokens (read-only)', 'oauth2-tokens-read-only'],
	['', ''],
]) {
	test(`heading ${JSON.stringify(heading)} becomes ${JSON.stringify(expected)}`, () => {
		assert.equal(slugifyHeading(heading), expected);
	});
}
