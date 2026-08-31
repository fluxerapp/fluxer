// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@fluxer/logger/src/Logger';
import {SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {Hono} from 'hono';
import {afterAll, beforeEach, describe, expect, test, vi} from 'vitest';
import {z} from 'zod';
import {Config} from '../../Config';
import type {HonoEnv} from '../../types/HonoEnv';
import {OpenAPI, ResponseType} from '../ResponseTypeMiddleware';

const SnowflakeResponse = z.object({id: SnowflakeType});

describe('ResponseTypeMiddleware', () => {
	beforeEach(() => {
		Config.dev.validateResponses = true;
	});

	afterAll(() => {
		Config.dev.validateResponses = true;
	});

	test('serializes SnowflakeType response transforms as JSON strings', async () => {
		const app = new Hono<HonoEnv>();
		app.get('/snowflake', ResponseType(SnowflakeResponse), (ctx) => ctx.json({id: '123456789012345678'}));

		const response = await app.request('/snowflake');

		expect(response.status).toBe(200);
		expect(await response.json()).toEqual({id: '123456789012345678'});
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

	test('rejects mismatching responses while validation is enabled', async () => {
		const app = new Hono<HonoEnv>();
		app.get('/snowflake', ResponseType(SnowflakeResponse), (ctx) => ctx.json({id: 'not-a-snowflake'}));
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
	});

	test('passes the response through untouched while validation is disabled', async () => {
		Config.dev.validateResponses = false;
		const app = new Hono<HonoEnv>();
		app.get('/snowflake', ResponseType(SnowflakeResponse), (ctx) => ctx.json({id: 'not-a-snowflake', extra: 'kept'}));

		const response = await app.request('/snowflake');

		expect(response.status).toBe(200);
		expect(await response.json()).toEqual({id: 'not-a-snowflake', extra: 'kept'});
	});

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
});
