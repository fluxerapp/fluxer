// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, test} from 'vitest';
import {composeService, serviceEnvironment, serviceList} from './SelfHostingCompose';

describe('the shipped object store checks the credentials the stack sends', () => {
	const entrypoint = serviceList('seaweedfs-init', 'entrypoint');
	const init = entrypoint[2];

	test('seaweedfs-init applies an S3 identity built from the .env credentials', () => {
		expect(entrypoint.slice(0, 2)).toEqual(['/bin/sh', '-c']);
		expect(entrypoint).toHaveLength(3);
		expect(init).toContain(
			's3.configure -user=fluxer -access_key=$$FLUXER_S3_ACCESS_KEY -secret_key=$$FLUXER_S3_SECRET_KEY',
		);
		expect(init).toContain('-apply');
	});

	test('seaweedfs-init is handed the same credentials the api requires', () => {
		for (const name of ['FLUXER_S3_ACCESS_KEY', 'FLUXER_S3_SECRET_KEY']) {
			expect(serviceEnvironment('seaweedfs-init')[name]).toBe(`\${${name}:?set ${name} in .env}`);
		}
	});

	test('a failed identity write fails the init instead of leaving the store open', () => {
		const configure = init.indexOf('s3.configure');
		const ready = init.indexOf('echo "buckets ready"');
		expect(configure).toBeGreaterThan(-1);
		expect(ready).toBeGreaterThan(configure);
		expect(init).toMatch(
			/if ! echo "s3\.configure [^"]+"[^;]+; then\s+echo "seaweedfs-init could not configure the S3 identity" >&2;\s+exit 1;\s+fi;/u,
		);
	});

	test('media-proxy signs its reads, which the store now refuses to serve unsigned', () => {
		expect(serviceEnvironment('media-proxy').FLUXER_S3_READ_SIGNED).toBe('true');
	});

	test('every service that reaches the store waits for the identity to exist', () => {
		for (const name of ['api', 'worker', 'media-proxy']) {
			expect(composeService(name).depends_on, name).toMatchObject({
				'seaweedfs-init': {condition: 'service_completed_successfully'},
			});
		}
	});
});
