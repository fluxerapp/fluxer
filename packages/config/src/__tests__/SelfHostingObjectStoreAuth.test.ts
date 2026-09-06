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

describe('the shipped object store checks the credentials the stack sends', () => {
	const init = serviceBlock('seaweedfs-init');

	test('seaweedfs-init applies an S3 identity built from the .env credentials', () => {
		expect(init).toContain(
			's3.configure -user=fluxer -access_key=$$FLUXER_S3_ACCESS_KEY -secret_key=$$FLUXER_S3_SECRET_KEY',
		);
		expect(init).toContain('-apply');
	});

	test('seaweedfs-init is handed the same credentials the api requires', () => {
		for (const name of ['FLUXER_S3_ACCESS_KEY', 'FLUXER_S3_SECRET_KEY']) {
			expect(init).toMatch(new RegExp(`${name}: \\$\\{${name}:\\?set ${name} in \\.env\\}`, 'u'));
		}
	});

	test('a failed identity write fails the init instead of leaving the store open', () => {
		const configure = init.indexOf('s3.configure');
		const ready = init.indexOf('echo "buckets ready"');
		expect(configure).toBeGreaterThan(-1);
		expect(ready).toBeGreaterThan(configure);
		expect(init).toContain('echo "seaweedfs-init could not configure the S3 identity" >&2');
		expect(init).toContain('exit 1');
	});

	test('media-proxy signs its reads, which the store now refuses to serve unsigned', () => {
		expect(serviceBlock('media-proxy')).toContain('FLUXER_S3_READ_SIGNED: "true"');
	});

	test('every service that reaches the store waits for the identity to exist', () => {
		for (const name of ['api', 'worker', 'media-proxy']) {
			expect(serviceBlock(name)).toContain('seaweedfs-init: {condition: service_completed_successfully}');
		}
	});
});
