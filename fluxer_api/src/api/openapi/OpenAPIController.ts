// SPDX-License-Identifier: AGPL-3.0-or-later

import * as fs from 'node:fs';
import type {Hono} from 'hono';
import {Config} from '../Config';
import {RateLimitMiddleware} from '../middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '../RateLimitConfig';
import type {HonoEnv} from '../types/HonoEnv';
import {resolveAssetPath} from '../utils/AssetPaths';

const SPEC_PATH = resolveAssetPath('openapi', 'openapi.json');
const SPEC_DOCUMENT = JSON.parse(fs.readFileSync(SPEC_PATH, 'utf-8')) as Record<string, unknown>;

let specBody: string | null = null;

export function buildOpenAPISpecBody(apiClientEndpoint: string): string {
	return JSON.stringify({
		...SPEC_DOCUMENT,
		servers: [{url: `${apiClientEndpoint.trim().replace(/\/+$/u, '')}/v1`, description: 'This deployment'}],
	});
}

export function OpenAPIController(app: Hono<HonoEnv>): void {
	app.get('/openapi.json', RateLimitMiddleware(RateLimitConfigs.INSTANCE_INFO), (ctx) => {
		specBody ??= buildOpenAPISpecBody(Config.endpoints.apiClient);
		ctx.header('Access-Control-Allow-Origin', '*');
		ctx.header('Content-Type', 'application/json; charset=utf-8');
		return ctx.body(specBody);
	});
}
