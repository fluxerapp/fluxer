// SPDX-License-Identifier: AGPL-3.0-or-later

import {Headers} from '@fluxer/constants/src/Headers';
import {
	applyMiddlewareStack,
	createDefaultErrorLogger,
	createDefaultLogger,
	createStandardMiddlewareStack,
} from '@fluxer/hono/src/middleware/MiddlewareStack';
import type {RateLimitResult, RateLimitService} from '@fluxer/hono/src/middleware/RateLimit';
import {REQUEST_ID_KEY} from '@fluxer/hono/src/middleware/RequestId';
import type {Context} from 'hono';
import {Hono} from 'hono';
import {describe, expect, test, vi} from 'vitest';

function createMockRateLimitService(result: Partial<RateLimitResult> = {}): RateLimitService {
	const defaultResult: RateLimitResult = {
		allowed: true,
		limit: 100,
		remaining: 99,
		resetTime: new Date(Date.now() + 60000),
		...result,
	};
	return {
		checkLimit: vi.fn().mockResolvedValue(defaultResult),
	};
}

describe('createStandardMiddlewareStack', () => {
	test('includes cacheHeaders by default with no options', () => {
		const stack = createStandardMiddlewareStack();
		expect(stack).toHaveLength(2);
	});
	test('includes requestId middleware when configured', () => {
		const stack = createStandardMiddlewareStack({requestId: {}});
		expect(stack).toHaveLength(3);
	});
	test('includes cors middleware when enabled', () => {
		const stack = createStandardMiddlewareStack({cors: {enabled: true}});
		expect(stack).toHaveLength(3);
	});
	test('excludes cors middleware when disabled', () => {
		const stack = createStandardMiddlewareStack({cors: {enabled: false}});
		expect(stack).toHaveLength(2);
	});
	test('excludes cacheHeaders when set to false', () => {
		const stack = createStandardMiddlewareStack({cacheHeaders: false});
		expect(stack).toHaveLength(1);
	});
	test('includes logger middleware when log function provided', () => {
		const stack = createStandardMiddlewareStack({logger: {log: vi.fn()}});
		expect(stack).toHaveLength(3);
	});
	test('excludes logger middleware when no log function', () => {
		const stack = createStandardMiddlewareStack({logger: {}});
		expect(stack).toHaveLength(2);
	});
	test('includes rateLimit middleware when enabled with service', () => {
		const stack = createStandardMiddlewareStack({
			rateLimit: {enabled: true, service: createMockRateLimitService()},
		});
		expect(stack).toHaveLength(3);
	});
	test('excludes rateLimit middleware when no service provided', () => {
		const stack = createStandardMiddlewareStack({rateLimit: {enabled: true}});
		expect(stack).toHaveLength(2);
	});
	test('includes custom middleware', () => {
		const customMiddleware = vi.fn();
		const stack = createStandardMiddlewareStack({
			customMiddleware: [customMiddleware, customMiddleware],
		});
		expect(stack).toHaveLength(4);
	});
	test('combines all middleware in correct order', () => {
		const stack = createStandardMiddlewareStack({
			requestId: {},
			cors: {enabled: true},
			logger: {log: vi.fn()},
			rateLimit: {enabled: true, service: createMockRateLimitService()},
			customMiddleware: [vi.fn()],
		});
		expect(stack).toHaveLength(7);
	});
});

describe('applyMiddlewareStack', () => {
	test('applies version header middleware', async () => {
		const app = new Hono();
		applyMiddlewareStack(app, {});
		app.get('/test', (c) => c.json({ok: true}));
		const response = await app.request('/test');
		expect(response.headers.get(Headers.X_FLUXER_VERSION)).toBe('dev');
	});
	test('applies requestId middleware', async () => {
		const app = new Hono<{
			Variables: {
				[REQUEST_ID_KEY]: string;
			};
		}>();
		applyMiddlewareStack(app, {requestId: {}});
		app.get('/test', (c) => c.json({id: c.get(REQUEST_ID_KEY)}));
		const response = await app.request('/test');
		expect(response.headers.get(Headers.X_REQUEST_ID)).toBeTruthy();
	});
	test('applies cors middleware', async () => {
		const app = new Hono();
		applyMiddlewareStack(app, {cors: {enabled: true, origins: '*'}});
		app.get('/test', (c) => c.json({ok: true}));
		const response = await app.request('/test', {
			headers: {origin: 'https://example.com'},
		});
		expect(response.headers.get('Access-Control-Allow-Origin')).toBe('*');
	});
	test('applies error handler by default', async () => {
		const app = new Hono();
		applyMiddlewareStack(app, {});
		app.get('/test', () => {
			throw new Error('Test error');
		});
		const response = await app.request('/test');
		expect(response.status).toBe(500);
		const body = (await response.json()) as {
			code: string;
		};
		expect(body.code).toBe('INTERNAL_SERVER_ERROR');
	});
	test('skips requestId when skipRequestId is true', async () => {
		const app = new Hono<{
			Variables: {
				[REQUEST_ID_KEY]: string;
			};
		}>();
		applyMiddlewareStack(app, {
			requestId: {},
			skipRequestId: true,
		});
		app.get('/test', (c) => c.json({id: c.get(REQUEST_ID_KEY)}));
		const response = await app.request('/test');
		expect(response.headers.get(Headers.X_REQUEST_ID)).toBeNull();
	});
	test('skips cors when skipCors is true', async () => {
		const app = new Hono();
		applyMiddlewareStack(app, {
			cors: {enabled: true, origins: '*'},
			skipCors: true,
		});
		app.get('/test', (c) => c.json({ok: true}));
		const response = await app.request('/test', {
			headers: {origin: 'https://example.com'},
		});
		expect(response.headers.get('Access-Control-Allow-Origin')).toBeNull();
	});
	test('skips logger when skipLogger is true', async () => {
		const log = vi.fn();
		const app = new Hono();
		applyMiddlewareStack(app, {
			logger: {log},
			skipLogger: true,
		});
		app.get('/test', (c) => c.json({ok: true}));
		await app.request('/test');
		expect(log).not.toHaveBeenCalled();
	});
	test('skips rateLimit when skipRateLimit is true', async () => {
		const service = createMockRateLimitService();
		const app = new Hono();
		applyMiddlewareStack(app, {
			rateLimit: {enabled: true, service},
			skipRateLimit: true,
		});
		app.get('/test', (c) => c.json({ok: true}));
		await app.request('/test');
		expect(service.checkLimit).not.toHaveBeenCalled();
	});
	test('applies cache headers by default', async () => {
		const app = new Hono();
		applyMiddlewareStack(app, {});
		app.get('/test', (c) => c.json({ok: true}));
		const response = await app.request('/test');
		expect(response.headers.get('Cache-Control')).toBe('no-cache');
	});
	test('skips cache headers when skipCacheHeaders is true', async () => {
		const app = new Hono();
		applyMiddlewareStack(app, {skipCacheHeaders: true});
		app.get('/test', (c) => c.json({ok: true}));
		const response = await app.request('/test');
		expect(response.headers.get('Cache-Control')).toBeNull();
	});
	test('skips errorHandler when skipErrorHandler is true', async () => {
		const consoleErrorSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
		const error = new Error('Test error');
		const app = new Hono();
		applyMiddlewareStack(app, {
			skipErrorHandler: true,
		});
		app.get('/test', () => {
			throw error;
		});
		try {
			const response = await app.request('/test');
			expect(response.status).toBe(500);
			expect(consoleErrorSpy).toHaveBeenCalledTimes(1);
			expect(consoleErrorSpy).toHaveBeenCalledWith(error);
		} finally {
			consoleErrorSpy.mockRestore();
		}
	});
	test('applies custom middleware', async () => {
		const customMiddleware = vi.fn().mockImplementation(async (_c, next) => {
			await next();
		});
		const app = new Hono();
		applyMiddlewareStack(app, {
			customMiddleware: [customMiddleware],
		});
		app.get('/test', (c) => c.json({ok: true}));
		await app.request('/test');
		expect(customMiddleware).toHaveBeenCalled();
	});
	test('applies logger with skip paths', async () => {
		const log = vi.fn();
		const app = new Hono();
		applyMiddlewareStack(app, {
			logger: {log, skip: ['/_health']},
		});
		app.get('/_health', (c) => c.json({ok: true}));
		app.get('/api', (c) => c.json({ok: true}));
		await app.request('/_health');
		expect(log).not.toHaveBeenCalled();
		await app.request('/api');
		expect(log).toHaveBeenCalled();
	});
});

describe('createDefaultLogger', () => {
	test('logs request data as JSON', async () => {
		const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
		const logger = createDefaultLogger({serviceName: 'test-service'});
		logger({method: 'GET', path: '/api/test', status: 200, durationMs: 50});
		expect(consoleSpy).toHaveBeenCalledTimes(1);
		const loggedData = JSON.parse(consoleSpy.mock.calls[0][0] as string);
		expect(loggedData.service).toBe('test-service');
		expect(loggedData.method).toBe('GET');
		expect(loggedData.path).toBe('/api/test');
		expect(loggedData.status).toBe(200);
		expect(loggedData.durationMs).toBe(50);
		expect(loggedData.timestamp).toBeTruthy();
		consoleSpy.mockRestore();
	});
	test('skips paths in skip array', () => {
		const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
		const logger = createDefaultLogger({serviceName: 'test-service', skip: ['/_health']});
		logger({method: 'GET', path: '/_health', status: 200, durationMs: 1});
		expect(consoleSpy).not.toHaveBeenCalled();
		consoleSpy.mockRestore();
	});
	test('logs paths not in skip array', () => {
		const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
		const logger = createDefaultLogger({serviceName: 'test-service', skip: ['/_health']});
		logger({method: 'GET', path: '/api/users', status: 200, durationMs: 1});
		expect(consoleSpy).toHaveBeenCalledTimes(1);
		consoleSpy.mockRestore();
	});
});

describe('createDefaultErrorLogger', () => {
	test('logs error data as JSON', async () => {
		const consoleSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
		const errorLogger = createDefaultErrorLogger({serviceName: 'test-service'});
		const mockContext = {
			req: {
				path: '/api/test',
				method: 'POST',
			},
		} as Context;
		const error = new Error('Test error');
		errorLogger(error, mockContext);
		expect(consoleSpy).toHaveBeenCalledTimes(1);
		const loggedData = JSON.parse(consoleSpy.mock.calls[0][0] as string);
		expect(loggedData.service).toBe('test-service');
		expect(loggedData.error).toBe('Test error');
		expect(loggedData.stack).toBeTruthy();
		expect(loggedData.path).toBe('/api/test');
		expect(loggedData.method).toBe('POST');
		expect(loggedData.timestamp).toBeTruthy();
		consoleSpy.mockRestore();
	});
});

describe('integration tests', () => {
	test('full middleware stack works together', async () => {
		const log = vi.fn();
		const rateLimitService = createMockRateLimitService();
		const keyGenerator = vi.fn().mockResolvedValue('test-client');
		const app = new Hono();
		applyMiddlewareStack(app, {
			requestId: {},
			cors: {enabled: true, origins: '*'},
			logger: {log},
			rateLimit: {enabled: true, service: rateLimitService, keyGenerator},
		});
		app.get('/test', (c) => c.json({ok: true}));
		const response = await app.request('/test');
		expect(response.status).toBe(200);
		expect(response.headers.get(Headers.X_REQUEST_ID)).toBeTruthy();
		expect(response.headers.get('Access-Control-Allow-Origin')).toBe('*');
		expect(response.headers.get('Cache-Control')).toBe('no-cache');
		expect(response.headers.get('X-RateLimit-Limit')).toBe('100');
		expect(log).toHaveBeenCalled();
		expect(keyGenerator).toHaveBeenCalled();
		expect(rateLimitService.checkLimit).toHaveBeenCalled();
	});
	test('error handler catches errors from routes', async () => {
		const app = new Hono();
		applyMiddlewareStack(app, {requestId: {}});
		app.get('/error', () => {
			throw new Error('Route error');
		});
		const response = await app.request('/error');
		expect(response.status).toBe(500);
		expect(response.headers.get(Headers.X_REQUEST_ID)).toBeTruthy();
	});
	test('rate limiter blocks requests when limit exceeded', async () => {
		const rateLimitService = createMockRateLimitService({allowed: false, remaining: 0});
		const keyGenerator = vi.fn().mockResolvedValue('blocked-client');
		const app = new Hono();
		applyMiddlewareStack(app, {
			rateLimit: {enabled: true, service: rateLimitService, keyGenerator},
		});
		app.get('/test', (c) => c.json({ok: true}));
		const response = await app.request('/test');
		expect(response.status).toBe(429);
		expect(keyGenerator).toHaveBeenCalled();
	});
	test('health endpoints are skipped by default for rate limiting', async () => {
		const rateLimitService = createMockRateLimitService();
		const app = new Hono();
		applyMiddlewareStack(app, {
			rateLimit: {enabled: true, service: rateLimitService},
		});
		app.get('/_health', (c) => c.json({ok: true}));
		await app.request('/_health');
		expect(rateLimitService.checkLimit).not.toHaveBeenCalled();
	});
});
