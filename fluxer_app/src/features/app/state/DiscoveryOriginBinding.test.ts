// SPDX-License-Identifier: AGPL-3.0-or-later

import {DEPLOYED_OFFICIAL_DOCUMENT} from '@fluxer/instance_bootstrap/src/__tests__/DiscoveryFixtures';
import {
	fetchInstanceDiscovery,
	InstanceDiscoveryOriginMismatchError,
	InstanceDiscoveryUnreachableError,
} from '@fluxer/instance_bootstrap/src/Discovery';
import {describe, expect, it} from 'vitest';

function serve(document: unknown): typeof globalThis.fetch {
	return (async () =>
		new Response(JSON.stringify(document), {
			status: 200,
			headers: {'content-type': 'application/json'},
		})) as unknown as typeof globalThis.fetch;
}

function documentWithEndpoints(overrides: Record<string, string>): unknown {
	return {
		...DEPLOYED_OFFICIAL_DOCUMENT,
		endpoints: {...DEPLOYED_OFFICIAL_DOCUMENT.endpoints, ...overrides},
	};
}

describe('a discovery document must declare the origin that served it', () => {
	it('accepts the official document fetched from the official API host', async () => {
		const result = await fetchInstanceDiscovery({
			input: 'https://api.fluxer.app',
			fetch: serve(DEPLOYED_OFFICIAL_DOCUMENT),
		});
		expect(result.kind).toBe('ok');
	});

	it('accepts a self-hosted document whose API is a path on the serving origin', async () => {
		const result = await fetchInstanceDiscovery({
			input: 'https://self.hosted.example',
			fetch: serve(
				documentWithEndpoints({
					api: 'https://self.hosted.example/api',
					api_client: 'https://self.hosted.example/api',
					api_public: 'https://self.hosted.example/api',
				}),
			),
		});
		expect(result.kind).toBe('ok');
	});

	it('refuses a document that claims the official API while served by someone else', async () => {
		await expect(
			fetchInstanceDiscovery({input: 'https://evil.example', fetch: serve(DEPLOYED_OFFICIAL_DOCUMENT)}),
		).rejects.toBeInstanceOf(InstanceDiscoveryUnreachableError);
		await expect(
			fetchInstanceDiscovery({input: 'https://evil.example', fetch: serve(DEPLOYED_OFFICIAL_DOCUMENT)}),
		).rejects.toThrow(/declared an API at https:\/\/api\.fluxer\.app/u);
	});

	it('refuses a document whose api_client points somewhere other than the origin that served it', async () => {
		await expect(
			fetchInstanceDiscovery({
				input: 'https://self.hosted.example',
				fetch: serve(
					documentWithEndpoints({
						api: 'https://self.hosted.example/api',
						api_client: 'https://api.fluxer.app',
						api_public: 'https://self.hosted.example/api',
					}),
				),
			}),
		).rejects.toBeInstanceOf(InstanceDiscoveryUnreachableError);
	});

	it('carries both origins on the mismatch error', () => {
		const error = new InstanceDiscoveryOriginMismatchError('https://evil.example', 'https://api.fluxer.app');
		expect(error.servedBy).toBe('https://evil.example');
		expect(error.declared).toBe('https://api.fluxer.app');
	});
});
