import {fileURLToPath} from 'node:url';
import {OpenAPIGenerator} from '@fluxer/openapi/src/OpenAPIGenerator';
import type {OpenAPIDocument} from '@fluxer/openapi/src/OpenAPITypes';
import {beforeAll, describe, expect, it} from 'vitest';

const REPOSITORY_PATH = fileURLToPath(new URL('../../../../', import.meta.url));

describe('OpenAPI generation from API controllers', () => {
	let document: OpenAPIDocument;
	beforeAll(async () => {
		document = await new OpenAPIGenerator({basePath: REPOSITORY_PATH, routeScope: 'all'}).generate();
	});

	it('describes the channel body before its channel type is injected', () => {
		expect(document.paths['/channels/{channel_id}'].patch.requestBody).toMatchObject({
			required: false,
			content: {'application/json': {schema: {$ref: '#/components/schemas/ChannelUpdateRequestBody'}}},
		});
		const options = document.components.schemas.ChannelUpdateRequestBody.anyOf;
		expect(options).toHaveLength(5);
		for (const option of options ?? []) {
			expect(option.properties).not.toHaveProperty('type');
			expect(option.required).toBeUndefined();
		}
	});

	it('keeps path-supplied voice IDs out of JSON request bodies', () => {
		expect(document.paths['/admin/voice/regions/{region_id}'].patch.requestBody?.required).toBe(false);
		expect(document.paths['/admin/voice/regions/{region_id}/servers/{server_id}'].patch.requestBody?.required).toBe(
			false,
		);
		expect(document.components.schemas.UpdateVoiceRegionRequestBody.properties).not.toHaveProperty('id');
		expect(document.components.schemas.UpdateVoiceServerRequestBody.properties).not.toHaveProperty('region_id');
		expect(document.components.schemas.UpdateVoiceServerRequestBody.properties).not.toHaveProperty('server_id');
		expect(document.paths['/admin/voice/regions/{region_id}/servers'].post.requestBody?.required).toBe(true);
		expect(document.components.schemas.CreateVoiceServerRequestBody.required).toEqual([
			'server_id',
			'endpoint',
			'api_key',
			'api_secret',
		]);
	});

	it('retains the explicit optional sudo body on channel deletion', () => {
		expect(document.paths['/channels/{channel_id}'].delete.requestBody).toMatchObject({
			required: false,
			content: {'application/json': {schema: {$ref: '#/components/schemas/SudoVerificationSchema'}}},
		});
	});

	it('documents bot authentication for the current application endpoint', () => {
		const operation = document.paths['/applications/@me'].get;
		expect(operation.security).toEqual([{botToken: []}]);
		expect(operation.responses).toHaveProperty('401');
	});

	it('publishes the real heap-snapshot media type', () => {
		expect(document.paths['/admin/system/heap-snapshots'].post.responses['200'].content).toEqual({
			'application/octet-stream': {schema: {$ref: '#/components/schemas/HeapSnapshotResponse'}},
		});
	});

	it('documents full and partial harvest archive downloads as binary ZIP responses', () => {
		const responses = document.paths['/harvest-downloads/{harvestId}'].get.responses;
		for (const status of ['200', '206']) {
			expect(responses[status].content).toEqual({
				'application/zip': {schema: {$ref: '#/components/schemas/HarvestArchiveResponse'}},
			});
		}
		expect(document.components.schemas.HarvestArchiveResponse).toMatchObject({
			type: 'string',
			format: 'binary',
			contentEncoding: 'binary',
		});
		expect(responses).not.toHaveProperty('204');
	});

	it.each([
		'/dl/desktop/{channel}/{plat}/{arch}/latest/{format}',
		'/dl/desktop/{channel}/{plat}/{arch}/{version}/{format}',
	])('distinguishes streamed download bytes from bodyless redirects for %s', (path) => {
		const responses = document.paths[path].get.responses;
		for (const status of ['200', '206']) {
			expect(responses[status].content).toEqual({
				'*/*': {schema: {$ref: '#/components/schemas/DownloadFileResponse'}},
			});
		}
		expect(responses['302']).toEqual({description: 'Success'});
		expect(responses).not.toHaveProperty('204');
	});

	it('publishes stream preview images as binary responses', () => {
		expect(document.paths['/streams/{stream_key}/preview'].get.responses['200'].content).toEqual({
			'image/*': {schema: {$ref: '#/components/schemas/StreamPreviewResponse'}},
		});
	});

	it('documents cache revalidation without a response body', () => {
		const responses = document.paths['/experiments'].get.responses;
		expect(responses['200'].content).toEqual({
			'application/json': {schema: {$ref: '#/components/schemas/ExperimentAssignmentsResponse'}},
		});
		expect(responses['304']).toEqual({description: 'Not Modified'});
	});

	it.each([
		{path: '/channels/{channel_id}/messages/bulk-delete-mine', method: 'post', status: '202'},
		{path: '/users/@me/guilds/{guild_id}/messages/bulk-delete-mine', method: 'post', status: '202'},
		{path: '/users/@me/messages/bulk-delete-mine', method: 'post', status: '202'},
		{path: '/donations/manage', method: 'get', status: '302'},
		{path: '/oauth2/token/revoke', method: 'post', status: '200'},
	])('preserves the bodyless $status response for $path', ({path, method, status}) => {
		const responses = document.paths[path][method].responses;
		expect(responses[status]).toEqual({description: 'Success'});
		expect(responses).not.toHaveProperty('204');
	});
});
