import {readdir} from 'node:fs/promises';
import path from 'node:path';
import {fileURLToPath} from 'node:url';

export const DOCS_ROOT = fileURLToPath(new URL('../src/content/docs/', import.meta.url));

export async function listMarkdownFiles(directory: string): Promise<Array<string>> {
	const files: Array<string> = [];
	for (const entry of await readdir(directory, {withFileTypes: true})) {
		if (entry.name === 'node_modules') {
			continue;
		}
		const resolved = path.join(directory, entry.name);
		if (entry.isDirectory()) {
			files.push(...(await listMarkdownFiles(resolved)));
		} else if (/\.mdx?$/u.test(entry.name)) {
			files.push(resolved);
		}
	}
	return files;
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
