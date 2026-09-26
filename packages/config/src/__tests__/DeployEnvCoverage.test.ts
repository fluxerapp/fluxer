// SPDX-License-Identifier: AGPL-3.0-or-later

import {readdirSync, readFileSync} from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {describe, expect, test} from 'vitest';

const SELF_HOSTING = path.join(fileURLToPath(new URL('../../../../', import.meta.url)), 'deploy/self-hosting');

const INTERPOLATION_PATTERN = /\$\{([A-Z][A-Z0-9_]*)[:?}-]/g;
const CADDY_PLACEHOLDER_PATTERN = /\{\$([A-Z][A-Z0-9_]*)[:}]/g;
const DECLARATION_PATTERN = /^#?([A-Z][A-Z0-9_]*)=/gm;
const ACTIVE_DECLARATION_PATTERN = /^([A-Z][A-Z0-9_]*)=/gm;
const LINE_DECLARATION_PATTERN = /^#?([A-Z][A-Z0-9_]*)=(.*)$/u;
const LINE_REFERENCE_PATTERN = /\$\{?([A-Z][A-Z0-9_]*)/gu;

const read = (name: string) => readFileSync(path.join(SELF_HOSTING, name), 'utf8');

const composeFiles = readdirSync(SELF_HOSTING)
	.filter((name) => name.endsWith('.yml'))
	.sort();

const namesMatching = (source: string, pattern: RegExp) =>
	new Set([...source.matchAll(pattern)].map(([, name]) => name));

const interpolatedNames = (source: string) => namesMatching(source, INTERPOLATION_PATTERN);

const example = read('.env.example');
const declared = new Set([...example.matchAll(DECLARATION_PATTERN)].map(([, name]) => name));

describe('.env.example covers every name the compose files interpolate', () => {
	for (const file of composeFiles) {
		test(`every \${NAME} in ${file} has a line in .env.example`, () => {
			const missing = [...interpolatedNames(read(file))].filter((name) => !declared.has(name)).sort();
			expect(missing).toEqual([]);
		});
	}

	test('every {$NAME} in the Caddyfile has a line in .env.example', () => {
		const missing = [...namesMatching(read('Caddyfile'), CADDY_PLACEHOLDER_PATTERN)]
			.filter((name) => !declared.has(name))
			.sort();
		expect(missing).toEqual([]);
	});

	test('no name is assigned twice', () => {
		const seen = new Set<string>();
		const repeated = new Set<string>();
		for (const [, name] of example.matchAll(ACTIVE_DECLARATION_PATTERN)) {
			if (seen.has(name)) {
				repeated.add(name);
			}
			seen.add(name);
		}
		expect([...repeated].sort()).toEqual([]);
	});

	test('every name a .env.example line reads is declared above it', () => {
		const lines = example.split('\n');
		const declaredAt = new Map<string, number>();
		lines.forEach((line, index) => {
			const match = LINE_DECLARATION_PATTERN.exec(line);
			if (match && !declaredAt.has(match[1])) {
				declaredAt.set(match[1], index);
			}
		});
		const unresolved: Array<string> = [];
		lines.forEach((line, index) => {
			const match = LINE_DECLARATION_PATTERN.exec(line);
			if (!match) {
				return;
			}
			for (const [, name] of match[2].matchAll(LINE_REFERENCE_PATTERN)) {
				const at = declaredAt.get(name);
				if (at === undefined || at > index) {
					unresolved.push(`${match[1]} reads ${name}`);
				}
			}
		});
		expect(unresolved.sort()).toEqual([]);
	});
});
