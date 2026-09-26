import {readdir, readFile} from 'node:fs/promises';
import path from 'node:path';
import {fileURLToPath} from 'node:url';

export const DOCS_ROOT = fileURLToPath(new URL('../src/content/docs/', import.meta.url));
export const HTTP_METHODS: ReadonlySet<string> = new Set(['GET', 'HEAD', 'POST', 'PATCH', 'PUT', 'DELETE', 'OPTIONS']);

export interface MarkdownPage {
	readonly file: string;
	readonly relativePath: string;
	readonly source: string;
	readonly lines: ReadonlyArray<string>;
}

export async function listMarkdownFiles(directory: string): Promise<Array<string>> {
	const files: Array<string> = [];
	for (const entry of await readdir(directory, {withFileTypes: true})) {
		if (entry.name === 'node_modules') {
			continue;
		}
		const resolved = path.join(directory, entry.name);
		if (entry.isDirectory()) {
			files.push(...(await listMarkdownFiles(resolved)));
		} else if (entry.isFile() && /\.mdx?$/u.test(entry.name)) {
			files.push(resolved);
		}
	}
	return files.sort();
}

export async function readMarkdownPages(directory: string): Promise<Array<MarkdownPage>> {
	const pages: Array<MarkdownPage> = [];
	for (const file of await listMarkdownFiles(directory)) {
		const source = await readFile(file, 'utf8');
		pages.push({
			file,
			relativePath: path.relative(directory, file).split(path.sep).join('/'),
			source,
			lines: source.split('\n'),
		});
	}
	return pages;
}

export function splitTableRow(line: string): Array<string> {
	const row = line.trim();
	const cells: Array<string> = [];
	let start = row.startsWith('|') ? 1 : 0;
	for (let index = start; index < row.length; index += 1) {
		if (row[index] === '\\' && (row[index + 1] === '\\' || row[index + 1] === '|')) {
			index += 1;
			continue;
		}
		if (row[index] === '|') {
			cells.push(row.slice(start, index).trim());
			start = index + 1;
		}
	}
	if (start < row.length) {
		cells.push(row.slice(start).trim());
	}
	return cells;
}

export function routeShape(method: string, routePath: string): string {
	return `${method} ${routePath.split('?')[0].replace(/\{[^}]*\}/gu, '{}')}`;
}

export function slugifyHeading(heading: string): string {
	return heading
		.replace(/`/gu, '')
		.replace(/\[([^\]]*)\]\([^)]*\)/gu, '$1')
		.replace(/<[^>]*>/gu, '')
		.toLowerCase()
		.replace(/[^a-z0-9\s-]/gu, '')
		.trim()
		.replace(/\s+/gu, '-');
}
