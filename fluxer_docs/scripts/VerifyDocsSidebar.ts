// SPDX-License-Identifier: AGPL-3.0-or-later

import {readdir, readFile} from 'node:fs/promises';
import path from 'node:path';
import {fileURLToPath} from 'node:url';

const DOCS_ROOT = fileURLToPath(new URL('../src/content/docs/', import.meta.url));
const ASTRO_CONFIG = fileURLToPath(new URL('../astro.config.ts', import.meta.url));

const SIDEBAR_START = 'sidebar: [';
const QUOTED_ENTRY = /'([^']+)'/gu;
const LINK_ENTRY = /link:\s*'([^']+)'/gu;

async function walk(directory: string): Promise<Array<string>> {
	const entries = await readdir(directory, {withFileTypes: true});
	const files: Array<string> = [];
	for (const entry of entries) {
		const resolved = path.join(directory, entry.name);
		if (entry.isDirectory()) {
			files.push(...(await walk(resolved)));
			continue;
		}
		if (entry.name.endsWith('.mdx') || entry.name.endsWith('.md')) {
			files.push(resolved);
		}
	}
	return files;
}

function slugOf(file: string): string {
	const relative = path.relative(DOCS_ROOT, file);
	const withoutExtension = relative.replace(/\.(mdx|md)$/u, '');
	if (withoutExtension === 'index') {
		return '';
	}
	return withoutExtension.replace(/\/index$/u, '');
}

function sidebarBlock(source: string): string {
	const start = source.indexOf(SIDEBAR_START);
	if (start === -1) {
		throw new Error('sidebar block not found in astro.config.ts');
	}
	let depth = 0;
	for (let cursor = start + SIDEBAR_START.length - 1; cursor < source.length; cursor += 1) {
		const character = source[cursor];
		if (character === '[') {
			depth += 1;
		}
		if (character === ']') {
			depth -= 1;
			if (depth === 0) {
				return source.slice(start, cursor + 1);
			}
		}
	}
	throw new Error('sidebar block is unbalanced');
}

const config = await readFile(ASTRO_CONFIG, 'utf8');
const block = sidebarBlock(config);

const labels = new Set<string>();
for (const match of block.matchAll(/label:\s*'([^']+)'/gu)) {
	labels.add(match[1]);
}

const linkSlugs = new Set<string>();
for (const match of block.matchAll(LINK_ENTRY)) {
	linkSlugs.add(match[1].replace(/^\//u, '').replace(/\/$/u, ''));
}

const referenced = new Set<string>(linkSlugs);
for (const match of block.matchAll(QUOTED_ENTRY)) {
	const value = match[1];
	if (labels.has(value)) {
		continue;
	}
	if (value.startsWith('/')) {
		continue;
	}
	if (!value.includes('/') && !value.match(/^[a-z0-9-]+$/u)) {
		continue;
	}
	referenced.add(value);
}

const files = await walk(DOCS_ROOT);
const actual = new Set(files.map(slugOf));

const missingPages = [...referenced].filter((slug) => !actual.has(slug)).sort();
const orphanPages = [...actual].filter((slug) => !referenced.has(slug)).sort();

console.log(`sidebar entries: ${referenced.size.toString()}`);
console.log(`content pages: ${actual.size.toString()}`);

let failures = 0;
if (missingPages.length > 0) {
	console.log('');
	console.log(`sidebar references a page that does not exist: ${missingPages.length.toString()}`);
	for (const slug of missingPages) {
		console.log(`  ${slug}`);
	}
	failures += missingPages.length;
}
if (orphanPages.length > 0) {
	console.log('');
	console.log(`page exists but is not in the sidebar: ${orphanPages.length.toString()}`);
	for (const slug of orphanPages) {
		console.log(`  ${slug === '' ? '(index)' : slug}`);
	}
	failures += orphanPages.length;
}

const INTERNAL_LINK = /\]\((\/[^)\s#]*)(#[^)\s]*)?\)/gu;
const SAME_PAGE_LINK = /\]\((#[^)\s]*)\)/gu;
const HEADING = /^#{2,6}\s+(.+?)\s*$/u;
const EXPLICIT_ANCHOR = /<[a-z]+\s+id=["']([^"']+)["']/gu;
const dangling: Array<string> = [];
const anchorable = new Set(actual);

function slugify(heading: string): string {
	return heading
		.replace(/`/gu, '')
		.replace(/\[([^\]]*)\]\([^)]*\)/gu, '$1')
		.replace(/<[^>]*>/gu, '')
		.toLowerCase()
		.replace(/[^a-z0-9\s-]/gu, '')
		.trim()
		.replace(/\s+/gu, '-');
}

const anchorsByPage = new Map<string, Set<string>>();
const ambiguousByPage = new Map<string, Set<string>>();
for (const file of files) {
	const slug = slugOf(file);
	const source = await readFile(file, 'utf8');
	const anchors = new Set<string>();
	const counts = new Map<string, number>();
	for (const line of source.split('\n')) {
		const heading = line.match(HEADING);
		if (heading != null) {
			const generated = slugify(heading[1]);
			anchors.add(generated);
			counts.set(generated, (counts.get(generated) ?? 0) + 1);
		}
	}
	for (const explicit of source.matchAll(EXPLICIT_ANCHOR)) {
		anchors.add(explicit[1]);
	}
	anchorsByPage.set(slug, anchors);
	const ambiguous = new Set<string>();
	for (const [generated, count] of counts) {
		if (count > 1 && !source.includes(`id="${generated}"`)) {
			ambiguous.add(generated);
		}
	}
	ambiguousByPage.set(slug, ambiguous);
}

for (const file of files) {
	const relative = path.relative(DOCS_ROOT, file);
	const selfSlug = slugOf(file);
	const source = await readFile(file, 'utf8');
	for (const match of source.matchAll(INTERNAL_LINK)) {
		const target = match[1].replace(/^\//u, '').replace(/\/$/u, '');
		if (target.length === 0) {
			if (match[2] != null) {
				const fragment = match[2].slice(1);
				const rootAnchors = anchorsByPage.get('');
				if (rootAnchors != null && fragment.length > 0 && !rootAnchors.has(fragment)) {
					dangling.push(`${relative} -> /#${fragment} (anchor)`);
				}
			}
			continue;
		}
		if (!anchorable.has(target)) {
			dangling.push(`${relative} -> /${target}/`);
			continue;
		}
		if (match[2] != null) {
			const fragment = match[2].slice(1);
			const anchors = anchorsByPage.get(target);
			if (fragment.length > 0 && anchors != null && !anchors.has(fragment)) {
				dangling.push(`${relative} -> /${target}/#${fragment} (anchor)`);
			} else if (fragment.length > 0 && (ambiguousByPage.get(target)?.has(fragment) ?? false)) {
				dangling.push(`${relative} -> /${target}/#${fragment} (that heading repeats on the target page)`);
			}
		}
	}
	for (const match of source.matchAll(SAME_PAGE_LINK)) {
		const fragment = match[1].slice(1);
		const anchors = anchorsByPage.get(selfSlug);
		if (fragment.length > 0 && anchors != null && !anchors.has(fragment)) {
			dangling.push(`${relative} -> #${fragment} (same-page anchor)`);
		} else if (fragment.length > 0 && (ambiguousByPage.get(selfSlug)?.has(fragment) ?? false)) {
			dangling.push(`${relative} -> #${fragment} (that heading repeats on this page)`);
		}
	}
}
if (dangling.length > 0) {
	console.log('');
	console.log(`links to a page that does not exist: ${dangling.length.toString()}`);
	for (const entry of [...new Set(dangling)].sort()) {
		console.log(`  ${entry}`);
	}
	failures += dangling.length;
}

if (failures > 0) {
	console.error(`FAIL: ${failures.toString()} structure problems`);
	process.exit(1);
}
console.log('OK: sidebar, content and internal links are in sync');
