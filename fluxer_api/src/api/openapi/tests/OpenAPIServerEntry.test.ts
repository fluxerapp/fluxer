// SPDX-License-Identifier: AGPL-3.0-or-later

import {Hono} from 'hono';
import {describe, expect, it} from 'vitest';
import {Config} from '../../Config';
import type {HonoEnv} from '../../types/HonoEnv';
import {buildOpenAPISpecBody, OpenAPIController} from '../OpenAPIController';

interface ServerEntry {
	url: string;
	description: string;
}

function parseServers(body: string): Array<ServerEntry> {
	return (JSON.parse(body) as {servers: Array<ServerEntry>}).servers;
}

describe('OpenAPI server entry', () => {
	it('declares the deployment client API endpoint', () => {
		expect(parseServers(buildOpenAPISpecBody('https://chat.example.com/api'))).toEqual([
			{url: 'https://chat.example.com/api/v1', description: 'This deployment'},
		]);
	});

	it('strips trailing slashes from the configured endpoint', () => {
		expect(parseServers(buildOpenAPISpecBody('https://chat.example.com/api//'))[0].url).toBe(
			'https://chat.example.com/api/v1',
		);
	});

	it('serves the configured endpoint instead of an upstream host', async () => {
		const app = new Hono<HonoEnv>();
		OpenAPIController(app);
		const response = await app.request('/openapi.json');
		expect(response.status).toBe(200);
		const spec = (await response.json()) as {servers: Array<ServerEntry>; paths: Record<string, unknown>};
		expect(spec.servers).toEqual([
			{url: `${Config.endpoints.apiClient.replace(/\/+$/u, '')}/v1`, description: 'This deployment'},
		]);
		expect(spec.servers[0].url).not.toContain('api.fluxer.app');
		expect(Object.keys(spec.paths).length).toBeGreaterThan(0);
	});
});
