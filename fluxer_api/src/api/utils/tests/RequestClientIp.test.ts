// SPDX-License-Identifier: AGPL-3.0-or-later

import {AppErrorHandler} from '@fluxer/errors/src/domains/core/ErrorHandlers';
import {MissingClientIpError} from '@fluxer/ip_utils/src/ClientIp';
import {Hono} from 'hono';
import {describe, expect, it} from 'vitest';
import type {HonoEnv} from '../../types/HonoEnv';
import {getRequestClientIp, requireRequestClientIp, resolveClientIpWithOptions} from '../RequestClientIp';

interface CountingRequest {
	request: Request;
	countReads: (name: string) => number;
}

function createCountingRequest(headers: Record<string, string>): CountingRequest {
	const request = new Request('http://localhost/v1/messages', {headers});
	const reads = new Map<string, number>();
	const get = request.headers.get.bind(request.headers);
	Object.defineProperty(request.headers, 'get', {
		configurable: true,
		value: (name: string): string | null => {
			const key = name.toLowerCase();
			reads.set(key, (reads.get(key) ?? 0) + 1);
			return get(name);
		},
	});
	return {request, countReads: (name) => reads.get(name.toLowerCase()) ?? 0};
}

describe('RequestClientIp', () => {
	it('resolves the client ip once and hands the same value to every consumer', async () => {
		const {request, countReads} = createCountingRequest({'x-forwarded-for': '203.0.113.10, 10.0.0.1'});
		const seen: Array<string | null> = [];
		const app = new Hono<HonoEnv>();
		app.use(async (ctx, next) => {
			seen.push(getRequestClientIp(ctx));
			await next();
		});
		app.use(async (ctx, next) => {
			seen.push(getRequestClientIp(ctx));
			await next();
		});
		app.get('/v1/messages', (ctx) => {
			seen.push(requireRequestClientIp(ctx));
			return ctx.text('ok');
		});
		const response = await app.request(request);
		expect(response.status).toBe(200);
		expect(seen).toEqual(['203.0.113.10', '203.0.113.10', '203.0.113.10']);
		expect(countReads('x-forwarded-for')).toBe(1);
	});
	it('normalizes the resolved address the same way for every consumer', async () => {
		const seen: Array<string | null> = [];
		const app = new Hono<HonoEnv>();
		app.use(async (ctx, next) => {
			seen.push(getRequestClientIp(ctx));
			await next();
		});
		app.get('/v1/messages', (ctx) => {
			seen.push(getRequestClientIp(ctx));
			return ctx.text('ok');
		});
		await app.request('http://localhost/v1/messages', {headers: {'x-forwarded-for': '2001:DB8::1'}});
		expect(seen).toEqual(['2001:db8::1', '2001:db8::1']);
	});
	it('reports a missing client ip header exactly as before', async () => {
		const {request, countReads} = createCountingRequest({});
		const seen: Array<string | null> = [];
		const errors: Array<unknown> = [];
		const app = new Hono<HonoEnv>();
		app.use(async (ctx, next) => {
			seen.push(getRequestClientIp(ctx));
			await next();
		});
		app.get('/v1/messages', (ctx) => {
			seen.push(getRequestClientIp(ctx));
			try {
				requireRequestClientIp(ctx);
			} catch (error) {
				errors.push(error);
			}
			return ctx.text('ok');
		});
		await app.request(request);
		expect(seen).toEqual([null, null]);
		expect(errors[0]).toBeInstanceOf(MissingClientIpError);
		expect(countReads('x-forwarded-for')).toBe(1);
	});
	it('reports a malformed client ip header exactly as before', async () => {
		const seen: Array<string | null> = [];
		const app = new Hono<HonoEnv>();
		app.use(async (ctx, next) => {
			seen.push(getRequestClientIp(ctx));
			await next();
		});
		app.get('/v1/messages', (ctx) => {
			seen.push(getRequestClientIp(ctx));
			return ctx.text('ok');
		});
		await app.request('http://localhost/v1/messages', {headers: {'x-forwarded-for': 'not-an-ip'}});
		expect(seen).toEqual([null, null]);
	});
	it('never reuses a resolution made under a different header name', async () => {
		const seen: Array<string | null> = [];
		const app = new Hono<HonoEnv>();
		app.get('/v1/messages', (ctx) => {
			seen.push(getRequestClientIp(ctx));
			seen.push(resolveClientIpWithOptions(ctx, {trustClientIpHeader: true, clientIpHeaderName: 'x-real-ip'}));
			seen.push(getRequestClientIp(ctx));
			return ctx.text('ok');
		});
		await app.request('http://localhost/v1/messages', {
			headers: {'x-forwarded-for': '203.0.113.10', 'x-real-ip': '198.51.100.7'},
		});
		expect(seen).toEqual(['203.0.113.10', '198.51.100.7', '203.0.113.10']);
	});
	it('never reuses a resolution made under a different trust setting', async () => {
		const seen: Array<string | null> = [];
		const app = new Hono<HonoEnv>();
		app.get('/v1/messages', (ctx) => {
			seen.push(getRequestClientIp(ctx));
			seen.push(resolveClientIpWithOptions(ctx, {trustClientIpHeader: false, clientIpHeaderName: 'x-forwarded-for'}));
			seen.push(getRequestClientIp(ctx));
			return ctx.text('ok');
		});
		await app.request('http://localhost/v1/messages', {headers: {'x-forwarded-for': '203.0.113.10'}});
		expect(seen).toEqual(['203.0.113.10', null, '203.0.113.10']);
	});
	it('a missing client ip surfaces as 403 FORBIDDEN through AppErrorHandler', async () => {
		const app = new Hono<HonoEnv>();
		app.get('/v1/messages', (ctx) => {
			requireRequestClientIp(ctx);
			return ctx.text('ok');
		});
		app.onError(AppErrorHandler);
		const response = await app.request('http://localhost/v1/messages');
		expect(response.status).toBe(403);
		expect(await response.json()).toMatchObject({code: 'FORBIDDEN'});
	});
});
