// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {describe, expect, test} from 'vitest';

const SELF_HOSTING = path.join(fileURLToPath(new URL('../../../../', import.meta.url)), 'deploy/self-hosting');

const INTERPOLATION_PATTERN = /\$\{([A-Z][A-Z0-9_]*)[:?}-]/g;
const CADDY_PLACEHOLDER_PATTERN = /\{\$([A-Z][A-Z0-9_]*)[:}]/g;
const DECLARATION_PATTERN = /^#?([A-Z][A-Z0-9_]*)=/gm;
const ACTIVE_DECLARATION_PATTERN = /^([A-Z][A-Z0-9_]*)=/gm;

const read = (name: string) => readFileSync(path.join(SELF_HOSTING, name), 'utf8');

const namesMatching = (source: string, pattern: RegExp) =>
	new Set([...source.matchAll(pattern)].map(([, name]) => name));

const interpolatedNames = (source: string) => namesMatching(source, INTERPOLATION_PATTERN);

const example = read('.env.example');
const declared = new Set([...example.matchAll(DECLARATION_PATTERN)].map(([, name]) => name));

describe('.env.example covers every name the compose files interpolate', () => {
	for (const file of ['docker-compose.yml', 'docker-compose.proxy.yml']) {
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
});
