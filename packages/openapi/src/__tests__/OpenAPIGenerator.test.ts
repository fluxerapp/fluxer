import assert from 'node:assert/strict';
import {fileURLToPath} from 'node:url';
import {type APIErrorCode, APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ErrorCodeToI18nKey} from '@fluxer/errors/src/i18n/ErrorCodeMappings';
import {ERROR_I18N_MESSAGES} from '@fluxer/errors/src/i18n/ErrorI18nMessages';
import {OpenAPIGenerator} from '@fluxer/openapi/src/OpenAPIGenerator';
import {visitOpenAPISchemaObjects} from '@fluxer/openapi/src/OpenAPISchemaVisitor';
import type {OpenAPIDocument, OpenAPISchema} from '@fluxer/openapi/src/OpenAPITypes';
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
		const options = document.components.schemas.ChannelUpdateRequestBody.anyOf ?? [];
		expect(options).toHaveLength(11);
		const withType = options.filter((option) => option.properties && 'type' in option.properties);
		expect(withType).toHaveLength(2);
		for (const option of withType) {
			expect(option.properties?.type).toMatchObject({anyOf: [{const: 0}, {const: 5}]});
		}
		for (const option of options.slice(0, 6)) {
			expect(option.required).toBeUndefined();
		}
		expect(options.slice(6)).toEqual([
			{$ref: '#/components/schemas/ChannelUpdatePublicThreadRequestBody'},
			{$ref: '#/components/schemas/ChannelUpdatePrivateThreadRequestBody'},
			{$ref: '#/components/schemas/ChannelUpdateThreadParentRequestBody'},
			{$ref: '#/components/schemas/ChannelUpdateForumRequestBody'},
			{$ref: '#/components/schemas/ChannelUpdateMediaRequestBody'},
		]);
		for (const name of [
			'ChannelUpdateThreadParentRequestBody',
			'ChannelUpdateForumRequestBody',
			'ChannelUpdateMediaRequestBody',
		]) {
			const schema = document.components.schemas[name];
			expect(schema['x-fluxer-experiment']).toBe('channel_threads');
			expect(schema.properties).not.toHaveProperty('type');
			expect(schema.properties).toHaveProperty('default_auto_archive_duration');
		}
		expect(document.components.schemas.ChannelUpdateForumRequestBody.properties).toHaveProperty('available_tags');
		expect(document.components.schemas.ChannelCreateRequest.oneOf).toEqual(
			expect.arrayContaining([
				{$ref: '#/components/schemas/GuildForumChannelCreateRequest'},
				{$ref: '#/components/schemas/GuildMediaChannelCreateRequest'},
			]),
		);
		for (const name of ['ChannelUpdatePublicThreadRequestBody', 'ChannelUpdatePrivateThreadRequestBody']) {
			const schema = document.components.schemas[name];
			expect(schema['x-fluxer-experiment']).toBe('channel_threads');
			expect(schema.properties).not.toHaveProperty('type');
			expect(schema.properties).toHaveProperty('archived');
			expect(schema.required).toBeUndefined();
		}
		expect(document.components.schemas.ChannelUpdatePrivateThreadRequestBody.properties).toHaveProperty('invitable');
	});

	it('documents the bodyless response of thread member settings', () => {
		expect(document.paths['/channels/{channel_id}/thread-members/@me/settings'].patch.responses['204']).toEqual({
			description: 'No Content',
		});
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

	it('tags every channel thread operation with its experiment', () => {
		const tagged = Object.values(document.paths)
			.flatMap((pathItem) => Object.values(pathItem))
			.filter((operation) => operation['x-fluxer-experiment'] === 'channel_threads')
			.map((operation) => operation.operationId)
			.sort();
		expect(tagged).toEqual(
			[
				'start_thread_from_message',
				'start_thread',
				'list_guild_active_threads',
				'list_public_archived_threads',
				'list_private_archived_threads',
				'list_joined_private_archived_threads',
				'list_thread_members',
				'get_thread_member',
				'join_thread',
				'add_thread_member',
				'leave_thread',
				'remove_thread_member',
				'update_thread_member_settings',
				'search_threads',
				'get_channel_post_data',
				'create_forum_tag',
				'update_forum_tag',
				'delete_forum_tag',
				'list_admin_guild_threads',
				'delete_admin_thread_channel',
			].sort(),
		);
		expect(document.paths['/channels/{channel_id}'].get).not.toHaveProperty('x-fluxer-experiment');
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

	it('keeps every desktop download redirect out of the published document', () => {
		const published = Object.keys(document.paths).filter((path) => path.startsWith('/dl'));
		expect(published).toEqual([]);
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

const EU_COUNTRY_CODES = [
	'AT',
	'BE',
	'BG',
	'HR',
	'CY',
	'CZ',
	'DK',
	'EE',
	'FI',
	'FR',
	'DE',
	'GR',
	'HU',
	'IE',
	'IT',
	'LV',
	'LT',
	'LU',
	'MT',
	'NL',
	'PL',
	'PT',
	'RO',
	'SK',
	'SI',
	'ES',
	'SE',
];

function errorCodeMessage(code: APIErrorCode): string {
	return ERROR_I18N_MESSAGES[ErrorCodeToI18nKey[code]];
}

function resolveRef(document: OpenAPIDocument, schema: OpenAPISchema | undefined): OpenAPISchema {
	const name = schema?.$ref?.replace('#/components/schemas/', '');
	expect(name).toBeDefined();
	return document.components.schemas[name ?? ''];
}

function schemasWithEnum(document: OpenAPIDocument, values: ReadonlyArray<string>): Array<OpenAPISchema> {
	const found: Array<OpenAPISchema> = [];
	visitOpenAPISchemaObjects(document, (schema) => {
		if (Array.isArray(schema.enum) && schema.enum.join(',') === values.join(',')) found.push(schema);
	});
	return found;
}

describe('published public and admin documents', () => {
	const documents = new Map<'public' | 'admin', OpenAPIDocument>();
	function documentFor(scope: 'public' | 'admin'): OpenAPIDocument {
		const document = documents.get(scope);
		assert(document, `the ${scope} document was not generated`);
		return document;
	}
	beforeAll(async () => {
		documents.set(
			'public',
			await new OpenAPIGenerator({
				basePath: REPOSITORY_PATH,
				routeScope: 'public',
				schemaTarget: 'draft-2020-12',
			}).generate(),
		);
		documents.set(
			'admin',
			await new OpenAPIGenerator({
				basePath: REPOSITORY_PATH,
				routeScope: 'admin',
				schemaTarget: 'openapi-3.0',
			}).generate(),
		);
	});

	it.each(['public', 'admin'] as const)(
		'publishes every API error code with its message in the %s document',
		(scope) => {
			const {schemas} = documentFor(scope).components;
			const codes = Object.values(APIErrorCodes);
			expect(schemas.APIErrorCode.type).toBe('string');
			expect(schemas.APIErrorCode.enum).toEqual(codes);
			expect(schemas.APIErrorCode['x-enumDescriptions']).toEqual(codes.map(errorCodeMessage));
			for (const name of ['Error', 'ThrottledError']) {
				const code = schemas[name].properties?.code;
				expect(code).toMatchObject({type: 'string', description: expect.stringContaining('APIErrorCode')});
				expect(code).not.toHaveProperty('enum');
			}
		},
	);

	it('publishes the EU country codes once and references them from every DSA report body', () => {
		const document = documentFor('public');
		const euCountryCode = document.components.schemas.EuCountryCode;
		expect(euCountryCode.enum).toEqual(EU_COUNTRY_CODES);
		expect(euCountryCode.type).toBe('string');
		expect(schemasWithEnum(document, EU_COUNTRY_CODES)).toEqual([euCountryCode]);
		const body = resolveRef(
			document,
			document.paths['/reports/dsa'].post.requestBody?.content['application/json']?.schema,
		);
		expect(body.oneOf).toHaveLength(3);
		for (const branch of body.oneOf ?? []) {
			const properties = resolveRef(document, branch).properties;
			expect(properties?.reporter_country_of_residence).toEqual({
				$ref: '#/components/schemas/EuCountryCode',
				description: 'EU country code of the reporter residence',
			});
			expect(properties).not.toHaveProperty('reporter_fluxer_tag');
		}
	});

	it('describes only the report statuses that exist', () => {
		const status = documentFor('public').components.schemas.ReportResponse.properties?.status;
		expect(status).toEqual({type: 'string', description: 'Current status of the report (pending, resolved)'});
	});
});
