// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {convertPathToOpenAPI, extractPathParameterNames} from '@fluxer/openapi/src/extractors/PathParameters';
import {discoverControllerFiles, extractRoutesFromControllers} from '@fluxer/openapi/src/extractors/RouteExtractor';
import {OpenAPIOperationBuilder} from '@fluxer/openapi/src/generator/OpenAPIOperationBuilder';
import type {ExtractedRoute} from '@fluxer/openapi/src/OpenAPITypes';
import {SchemaRegistry} from '@fluxer/openapi/src/registry/SchemaRegistry';
import {DonationManageQuery} from '@fluxer/schema/src/domains/donation/DonationSchemas';
import {beforeAll, describe, expect, it} from 'vitest';

const API_PACKAGE_PATH = path.join(fileURLToPath(new URL('../../../../../', import.meta.url)), 'fluxer_api');

describe('discoverControllerFiles', () => {
	let files: Array<string>;
	let routes: Array<ExtractedRoute>;
	let shapes: Set<string>;
	beforeAll(() => {
		files = discoverControllerFiles(API_PACKAGE_PATH);
		routes = extractRoutesFromControllers(files);
		shapes = new Set(routes.map((route) => `${route.method.toUpperCase()} ${route.path}`));
	});
	it('leaves test sources out of the discovered set', () => {
		expect(files.filter((file) => file.endsWith('.test.ts'))).toEqual([]);
		expect(files.filter((file) => file.includes('/tests/'))).toEqual([]);
	});
	it('reads routes registered in a *Controller.ts file', () => {
		expect(shapes).toContain('GET /gifs/search');
		expect(shapes).toContain('GET /gifs/featured');
	});
	it('reads routes registered outside a *Controller.ts file', () => {
		expect(shapes).toContain('POST /webhooks/twilio/sms');
		expect(shapes).toContain('GET /_metrics');
		expect(shapes).toContain('GET /_health');
	});
	it('preserves wire-body overrides separately from runtime validators', () => {
		const route = routes.find((route) => route.explicitOperationId === 'update_channel');
		expect(route).toMatchObject({
			explicitRequestSchemaName: 'ChannelUpdateRequestBody',
			validators: expect.arrayContaining([{target: 'json', schemaName: 'ChannelUpdateRequest'}]),
		});
	});
	it('preserves an explicit false request-body requirement', () => {
		const route = routes.find((route) => route.explicitOperationId === 'delete_channel');
		expect(route).toMatchObject({
			explicitRequestBodyRequired: false,
			explicitRequestSchemaName: 'SudoVerificationSchema',
		});
	});
	it('preserves explicitly anonymous security metadata', () => {
		const route = routes.find((route) => route.path === '/donations/manage');
		expect(route?.explicitSecurity).toEqual([]);
	});
	it.each([
		'download_latest_desktop_version_checksum',
		'download_desktop_version_checksum',
	])('preserves the plain-text checksum response for %s', (operationId) => {
		const route = routes.find((route) => route.explicitOperationId === operationId);
		expect(route).toMatchObject({
			responseSchemaName: 'DownloadChecksumResponse',
			responseContentType: 'text/plain',
			explicitStatusCodes: [200],
		});
	});
	it('rejects a bodyless status that is absent from the route response statuses', () => {
		const route = routes.find((route) => route.path === '/donations/manage');
		assert(route);
		const schemaRegistry = new SchemaRegistry();
		schemaRegistry.registerZod('DonationManageQuery', DonationManageQuery);
		const builder = new OpenAPIOperationBuilder({schemaRegistry, usedOperationIds: new Set()});
		expect(() => builder.buildOperation({...route, bodylessStatusCodes: [299]})).toThrow(
			'Bodyless status 299 is not declared for GET /donations/manage',
		);
	});
	it('resolves paths and metadata from shared admin route registrations', () => {
		const route = routes.find((route) => route.explicitOperationId === 'create_admin_blocklist_entry');
		expect(route).toMatchObject({
			method: 'post',
			path: '/admin/blocklists/:list_type/entries',
			explicitRequestSchemaName: 'AdminBlocklistEntryCreateRequest',
		});
	});
	it('converts route parameters without changing literal path segments', () => {
		const path = '/admin/voice/regions/:region_id/servers/:server_id';
		expect(shapes).toContain(`PATCH ${path}`);
		expect(extractPathParameterNames(path)).toEqual(['region_id', 'server_id']);
		expect(convertPathToOpenAPI(path)).toBe('/admin/voice/regions/{region_id}/servers/{server_id}');
		expect(convertPathToOpenAPI('/users/@me')).toBe('/users/@me');
	});
});
