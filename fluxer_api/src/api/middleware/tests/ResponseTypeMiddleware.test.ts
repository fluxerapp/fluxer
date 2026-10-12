// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {Logger} from '@fluxer/logger/src/Logger';
import {SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {Hono} from 'hono';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';
import {z} from 'zod';

const SnowflakeResponse = z.object({id: SnowflakeType});

describe('ResponseTypeMiddleware', () => {
	const originalValidateResponses = Config.dev.validateResponses;

	beforeEach(() => {
		Config.dev.validateResponses = true;
	});

	afterEach(() => {
		Config.dev.validateResponses = originalValidateResponses;
	});

	test('serializes OpenAPI SnowflakeType response transforms as JSON strings', async () => {
		const app = new Hono<HonoEnv>();
		app.get(
			'/snowflake',
			OpenAPI({
				operationId: 'get_snowflake_test',
				summary: 'Get snowflake test',
				description: 'Returns a snowflake-shaped ID for response serialization regression coverage.',
				responseSchema: SnowflakeResponse,
				tags: ['Tests'],
			}),
			(ctx) => ctx.json({id: '123456789012345678'}),
		);

		const response = await app.request('/snowflake');

		expect(response.status).toBe(200);
		expect(await response.json()).toEqual({id: '123456789012345678'});
	});

	test.each([undefined, 'application/json', 'application/json; charset=utf-8', 'Application/JSON; charset=utf-8'])(
		'rejects mismatching responses while validation is enabled (content type: %s)',
		async (responseContentType) => {
			const app = new Hono<HonoEnv>();
			const middleware = OpenAPI({
				operationId: 'get_invalid_snowflake_test',
				summary: 'Get invalid snowflake',
				description: 'Returns an invalid snowflake to verify JSON response validation.',
				responseSchema: SnowflakeResponse,
				...(responseContentType === undefined ? {} : {responseContentType}),
				tags: ['Tests'],
			});
			app.get('/snowflake', middleware, (ctx) => ctx.json({id: 'not-a-snowflake'}));
			const errorLoggerSpy = vi.spyOn(Logger.prototype, 'error').mockImplementation(() => {});

			try {
				const response = await app.request('/snowflake');

				expect(response.status).toBe(500);
				expect(errorLoggerSpy).toHaveBeenCalledTimes(1);
				expect(errorLoggerSpy).toHaveBeenCalledWith(
					{
						body: {id: 'not-a-snowflake'},
						method: 'GET',
						path: '/snowflake',
						status: 200,
						validationErrors: [{message: 'INVALID_SNOWFLAKE_FORMAT', path: 'id'}],
					},
					'Response validation failed',
				);
			} finally {
				errorLoggerSpy.mockRestore();
			}
		},
	);

	test('passes the OpenAPI response through untouched while validation is disabled', async () => {
		Config.dev.validateResponses = false;
		const app = new Hono<HonoEnv>();
		app.get(
			'/snowflake',
			OpenAPI({
				operationId: 'get_snowflake_unvalidated_test',
				summary: 'Get snowflake test',
				description: 'Returns an unvalidated payload for response validation gating coverage.',
				responseSchema: SnowflakeResponse,
				tags: ['Tests'],
			}),
			(ctx) => ctx.json({id: 'not-a-snowflake', extra: 'kept'}),
		);

		const response = await app.request('/snowflake');

		expect(response.status).toBe(200);
		expect(await response.json()).toEqual({id: 'not-a-snowflake', extra: 'kept'});
	});

	test('preserves raw JSON file bytes under a binary response contract', async () => {
		const body = '{ "file": "contents" }';
		const app = new Hono<HonoEnv>();
		app.get(
			'/artifact',
			OpenAPI({
				operationId: 'get_artifact_test',
				summary: 'Get artifact file',
				description: 'Returns file bytes without treating JSON artifacts as structured API responses.',
				responseSchema: z.file(),
				responseContentType: '*/*',
				statusCode: 200,
				tags: ['Tests'],
			}),
			() => new Response(body, {headers: {'Content-Type': 'application/json'}}),
		);

		const response = await app.request('/artifact');
		expect(response.status).toBe(200);
		expect(response.headers.get('content-type')).toBe('application/json');
		expect(await response.text()).toBe(body);
	});
});
