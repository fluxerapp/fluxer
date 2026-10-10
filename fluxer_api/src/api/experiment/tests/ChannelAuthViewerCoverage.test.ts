// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {describe, expect, it} from 'vitest';

const API_ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', '..');
const CALL = '.getChannelAuthenticated(';

function listSourceFiles(directory: string): Array<string> {
	return fs.readdirSync(directory, {withFileTypes: true}).flatMap((entry) => {
		const resolved = path.join(directory, entry.name);
		if (entry.isDirectory()) return listSourceFiles(resolved);
		return entry.name.endsWith('.ts') && !entry.name.endsWith('.test.ts') ? [resolved] : [];
	});
}

function callArguments(source: string, openParen: number): string {
	let depth = 0;
	for (let index = openParen; index < source.length; index++) {
		const char = source[index];
		if (char === '(') depth++;
		if (char === ')') depth--;
		if (depth === 0) return source.slice(openParen + 1, index);
	}
	throw new Error('unbalanced call');
}

function collectCallSites(): Map<string, Array<string>> {
	const sites = new Map<string, Array<string>>();
	for (const file of listSourceFiles(API_ROOT)) {
		const source = fs.readFileSync(file, 'utf8');
		let index = source.indexOf(CALL);
		while (index !== -1) {
			const relative = path.relative(API_ROOT, file).split(path.sep).join('/');
			const args = callArguments(source, index + CALL.length - 1);
			sites.set(relative, [...(sites.get(relative) ?? []), args]);
			index = source.indexOf(CALL, index + CALL.length);
		}
	}
	return sites;
}

describe('getChannelAuthenticated viewer coverage', () => {
	const sites = collectCallSites();

	it('passes a thread viewer at every call site', () => {
		const missing = [...sites].flatMap(([file, calls]) =>
			calls.filter((args) => !/\bviewer\b/.test(args)).map((args) => `${file}: ${args.replace(/\s+/g, ' ')}`),
		);
		expect(missing).toEqual([]);
	});
});
