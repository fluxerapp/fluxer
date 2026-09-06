// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {describe, expect, test} from 'vitest';

const SELF_HOSTING = path.join(fileURLToPath(new URL('../../../../', import.meta.url)), 'deploy/self-hosting');

const compose = readFileSync(path.join(SELF_HOSTING, 'docker-compose.yml'), 'utf8');

const serviceBlock = (name: string): string => {
	const start = compose.indexOf(`\n  ${name}:\n`);
	if (start === -1) {
		throw new Error(`docker-compose.yml has no ${name} service`);
	}
	const rest = compose.slice(start + 1);
	const next = rest.slice(1).search(/\n {2}[a-z][a-z0-9_-]*:\n/u);
	return next === -1 ? rest : rest.slice(0, next + 1);
};

const sharedEnv = compose.slice(compose.indexOf('x-fluxer-env: &fluxer-env'), compose.indexOf('\nx-fluxer-service:'));

describe('the shipped compose stack wires every service it starts', () => {
	test('no service sizes a pool for a connection it cannot make', () => {
		for (const [, name] of compose.matchAll(/\n {2}([a-z][a-z0-9_-]*):\n/gu)) {
			const block = serviceBlock(name);
			if (block.includes('FLUXER_POSTGRES_MAX_CONNECTIONS')) {
				expect(block).toMatch(/<<: \*fluxer-(postgres-)?env/u);
			}
		}
	});

	test('the shared block sets the client-IP trust the merged services read', () => {
		expect(sharedEnv).toContain('FLUXER_TRUST_CLIENT_IP_HEADER: "true"');
		expect(sharedEnv).toContain('FLUXER_CLIENT_IP_HEADER_NAME: x-forwarded-for');
	});
});
