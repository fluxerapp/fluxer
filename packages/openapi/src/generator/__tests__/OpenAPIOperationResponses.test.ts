// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {extractRoutesFromControllers} from '@fluxer/openapi/src/extractors/RouteExtractor';
import {OpenAPIOperationBuilder} from '@fluxer/openapi/src/generator/OpenAPIOperationBuilder';
import type {OpenAPISchemaTarget} from '@fluxer/openapi/src/OpenAPIGenerationTypes';
import type {ExtractedRoute} from '@fluxer/openapi/src/OpenAPITypes';
import {SchemaRegistry} from '@fluxer/openapi/src/registry/SchemaRegistry';
import {afterAll, beforeAll, describe, expect, it} from 'vitest';

const CONTROLLER_SOURCE = `
export function FixtureController(app: HonoApp) {
	app.patch(
		'/fixture/maybe',
		LoginRequired,
		OpenAPI({
			operationId: 'fixture_maybe',
			summary: 'Fixture',
			description: 'Fixture route.',
			responseSchema: FixtureResponse,
			statusCode: [200, 204],
			bodylessStatusCodes: [204],
			tags: ['Fixture'],
		}),
		async (ctx) => ctx.body(null, 204),
	);
	app.get(
		'/fixture/always',
		LoginRequired,
		OpenAPI({
			operationId: 'fixture_always',
			summary: 'Fixture',
			description: 'Fixture route.',
			responseSchema: FixtureResponse,
			tags: ['Fixture'],
		}),
		async (ctx) => ctx.json({}),
	);
}
`;

const REF = {$ref: '#/components/schemas/FixtureResponse'};

describe('operation responses', () => {
	let directory: string;
	let routes: Map<string, ExtractedRoute>;
	beforeAll(() => {
		directory = fs.mkdtempSync(path.join(os.tmpdir(), 'openapi-responses-'));
		const controllerPath = path.join(directory, 'FixtureController.ts');
		fs.writeFileSync(controllerPath, CONTROLLER_SOURCE);
		routes = new Map(extractRoutesFromControllers([controllerPath]).map((route) => [route.path, route]));
	});
	afterAll(() => {
		fs.rmSync(directory, {recursive: true, force: true});
	});

	function successSchema(routePath: string, target?: OpenAPISchemaTarget) {
		const route = routes.get(routePath);
		if (!route) throw new Error(`Missing fixture route ${routePath}`);
		const schemaRegistry = new SchemaRegistry(target);
		schemaRegistry.register('FixtureResponse', {type: 'object'});
		const builder = new OpenAPIOperationBuilder({schemaRegistry, usedOperationIds: new Set()});
		const responses = builder.buildOperation(route).responses;
		return {schema: responses['200'].content?.['application/json'].schema, responses};
	}

	it('marks the body nullable when another success status has no body', () => {
		const {schema, responses} = successSchema('/fixture/maybe');
		expect(schema).toEqual({anyOf: [REF, {type: 'null'}]});
		expect(responses['204']).toEqual({description: 'No Content'});
	});

	it('uses nullable for the openapi 3.0 target', () => {
		expect(successSchema('/fixture/maybe', 'openapi-3.0').schema).toEqual({allOf: [REF], nullable: true});
	});

	it('keeps a plain reference when every success status has a body', () => {
		expect(successSchema('/fixture/always').schema).toEqual(REF);
	});
});
