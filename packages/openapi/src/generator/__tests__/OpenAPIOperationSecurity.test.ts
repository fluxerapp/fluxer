// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {extractRoutesFromControllers} from '@fluxer/openapi/src/extractors/RouteExtractor';
import {OpenAPIOperationBuilder} from '@fluxer/openapi/src/generator/OpenAPIOperationBuilder';
import type {ExtractedRoute} from '@fluxer/openapi/src/OpenAPITypes';
import {SchemaRegistry} from '@fluxer/openapi/src/registry/SchemaRegistry';
import {afterAll, beforeAll, describe, expect, it} from 'vitest';

const CONTROLLER_SOURCE = `
export function FixtureController(app: HonoApp) {
	app.get(
		'/fixture/bot',
		LoginRequired,
		BotOnly,
		OpenAPI({
			operationId: 'fixture_bot',
			summary: 'Fixture',
			description: 'Fixture route.',
			responseSchema: null,
			statusCode: 204,
			tags: ['Fixture'],
		}),
		async (ctx) => ctx.body(null, 204),
	);
	app.get(
		'/fixture/user',
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'fixture_user',
			summary: 'Fixture',
			description: 'Fixture route.',
			responseSchema: null,
			statusCode: 204,
			tags: ['Fixture'],
		}),
		async (ctx) => ctx.body(null, 204),
	);
	app.get(
		'/fixture/any',
		LoginRequired,
		OpenAPI({
			operationId: 'fixture_any',
			summary: 'Fixture',
			description: 'Fixture route.',
			responseSchema: null,
			statusCode: 204,
			tags: ['Fixture'],
		}),
		async (ctx) => ctx.body(null, 204),
	);
}
`;

describe('operation security', () => {
	let directory: string;
	let routes: Map<string, ExtractedRoute>;
	beforeAll(() => {
		directory = fs.mkdtempSync(path.join(os.tmpdir(), 'openapi-security-'));
		const controllerPath = path.join(directory, 'FixtureController.ts');
		fs.writeFileSync(controllerPath, CONTROLLER_SOURCE);
		routes = new Map(extractRoutesFromControllers([controllerPath]).map((route) => [route.path, route]));
	});
	afterAll(() => {
		fs.rmSync(directory, {recursive: true, force: true});
	});

	function securityOf(routePath: string) {
		const route = routes.get(routePath);
		if (!route) throw new Error(`Missing fixture route ${routePath}`);
		const builder = new OpenAPIOperationBuilder({schemaRegistry: new SchemaRegistry(), usedOperationIds: new Set()});
		return {route, security: builder.buildOperation(route).security};
	}

	it('declares bot token security for a BotOnly route', () => {
		const {route, security} = securityOf('/fixture/bot');
		expect(route.hasBotOnly).toBe(true);
		expect(security).toEqual([{botToken: []}]);
	});

	it('keeps DefaultUserOnly routes on user tokens', () => {
		const {route, security} = securityOf('/fixture/user');
		expect(route.hasBotOnly).toBe(false);
		expect(route.hasDefaultUserOnly).toBe(true);
		expect(security).toEqual([{sessionToken: []}]);
	});

	it('keeps LoginRequired routes open to bot and user tokens', () => {
		const {route, security} = securityOf('/fixture/any');
		expect(route.hasBotOnly).toBe(false);
		expect(security).toEqual([{botToken: []}, {sessionToken: []}]);
	});
});
