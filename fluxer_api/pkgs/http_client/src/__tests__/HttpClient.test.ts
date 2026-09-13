// SPDX-License-Identifier: AGPL-3.0-or-later

import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {createTestServer, readRequestBody, type TestServer} from '@pkgs/http_client/src/__tests__/TestHttpServer';
import {createHttpClient} from '@pkgs/http_client/src/HttpClient';
import type {HttpClientMetrics, HttpClientTelemetry} from '@pkgs/http_client/src/HttpClientTelemetryTypes';
import type {RequestUrlPolicy, RequestUrlValidationContext} from '@pkgs/http_client/src/HttpClientTypes';
import {HttpError} from '@pkgs/http_client/src/HttpError';
import {afterAll, beforeAll, describe, expect, it} from 'vitest';

const TEST_USER_AGENT = 'FluxerHttpClient/1.0 (Test)';

describe('HttpClient', () => {
	let testServer: TestServer;
	let redirectServer: TestServer;
	beforeAll(async () => {
		testServer = await createTestServer();
		redirectServer = await createTestServer();
	});
	afterAll(async () => {
		await testServer.close();
		await redirectServer.close();
	});
	describe('createHttpClient', () => {
		it('creates an HTTP client with the provided user agent', async () => {
			const client = createHttpClient(TEST_USER_AGENT);
			testServer.setHandler((req, res) => {
				res.writeHead(200, {'Content-Type': 'text/plain'});
				res.end(req.headers['user-agent'] ?? '');
			});
			const response = await client.sendRequest({url: testServer.url});
			const body = await client.streamToString(response.stream);
			expect(body).toBe(TEST_USER_AGENT);
		});
	});
	describe('sendRequest', () => {
		describe('basic requests', () => {
			it('sends a GET request by default', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((req, res) => {
					res.writeHead(200, {'Content-Type': 'application/json'});
					res.end(JSON.stringify({method: req.method}));
				});
				const response = await client.sendRequest({url: testServer.url});
				const body = await client.streamToString(response.stream);
				const json = JSON.parse(body);
				expect(response.status).toBe(200);
				expect(json.method).toBe('GET');
			});
			it('sends a POST request with body', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				const requestBody = {message: 'Hello, World!'};
				testServer.setHandler(async (req, res) => {
					const body = await readRequestBody(req);
					res.writeHead(200, {'Content-Type': 'application/json'});
					res.end(JSON.stringify({method: req.method, receivedBody: JSON.parse(body)}));
				});
				const response = await client.sendRequest({
					url: testServer.url,
					method: 'POST',
					body: requestBody,
				});
				const body = await client.streamToString(response.stream);
				const json = JSON.parse(body);
				expect(response.status).toBe(200);
				expect(json.method).toBe('POST');
				expect(json.receivedBody).toEqual(requestBody);
			});
			it('sends URLSearchParams bodies as form-encoded payloads', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				const requestBody = new URLSearchParams({
					grant_type: 'authorization_code',
					code: 'test-code',
				});
				testServer.setHandler(async (req, res) => {
					const body = await readRequestBody(req);
					res.writeHead(200, {'Content-Type': 'application/json'});
					res.end(
						JSON.stringify({
							method: req.method,
							receivedBody: body,
							contentType: req.headers['content-type'],
						}),
					);
				});
				const response = await client.sendRequest({
					url: testServer.url,
					method: 'POST',
					body: requestBody,
				});
				const body = await client.streamToString(response.stream);
				const json = JSON.parse(body);
				expect(response.status).toBe(200);
				expect(json.method).toBe('POST');
				expect(json.receivedBody).toBe(requestBody.toString());
				expect(json.contentType).toContain('application/x-www-form-urlencoded');
			});
			it('sends a HEAD request', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((_req, res) => {
					res.writeHead(200, {'Content-Type': 'text/plain', 'X-Custom-Header': 'test-value'});
					res.end();
				});
				const response = await client.sendRequest({
					url: testServer.url,
					method: 'HEAD',
				});
				expect(response.status).toBe(200);
				expect(response.headers.get('X-Custom-Header')).toBe('test-value');
			});
		});
		describe('headers', () => {
			it('sends default headers including user agent', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((req, res) => {
					res.writeHead(200, {'Content-Type': 'application/json'});
					res.end(
						JSON.stringify({
							userAgent: req.headers['user-agent'],
							accept: req.headers['accept'],
							cacheControl: req.headers['cache-control'],
							pragma: req.headers['pragma'],
						}),
					);
				});
				const response = await client.sendRequest({url: testServer.url});
				const body = await client.streamToString(response.stream);
				const json = JSON.parse(body);
				expect(json.userAgent).toBe(TEST_USER_AGENT);
				expect(json.accept).toBe('*/*');
				expect(json.cacheControl).toBe('no-cache, no-store, must-revalidate');
				expect(json.pragma).toBe('no-cache');
			});
			it('allows custom headers to override defaults', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((req, res) => {
					res.writeHead(200, {'Content-Type': 'application/json'});
					res.end(
						JSON.stringify({
							accept: req.headers['accept'],
							customHeader: req.headers['x-custom-header'],
						}),
					);
				});
				const response = await client.sendRequest({
					url: testServer.url,
					headers: {
						Accept: 'application/json',
						'X-Custom-Header': 'custom-value',
					},
				});
				const body = await client.streamToString(response.stream);
				const json = JSON.parse(body);
				expect(json.accept).toBe('application/json');
				expect(json.customHeader).toBe('custom-value');
			});
			it('normalizes response headers correctly', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((_req, res) => {
					res.writeHead(200, {
						'Content-Type': 'text/plain',
						'X-Single-Value': 'single',
					});
					res.end('OK');
				});
				const response = await client.sendRequest({url: testServer.url});
				expect(response.headers.get('content-type')).toBe('text/plain');
				expect(response.headers.get('x-single-value')).toBe('single');
			});
		});
		describe('status codes', () => {
			it('returns 200 status for successful requests', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((_req, res) => {
					res.writeHead(200);
					res.end('OK');
				});
				const response = await client.sendRequest({url: testServer.url});
				expect(response.status).toBe(200);
			});
			it('returns 404 status for not found', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((_req, res) => {
					res.writeHead(404);
					res.end('Not Found');
				});
				const response = await client.sendRequest({url: testServer.url});
				expect(response.status).toBe(404);
			});
			it('returns 500 status for server errors', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((_req, res) => {
					res.writeHead(500);
					res.end('Internal Server Error');
				});
				const response = await client.sendRequest({url: testServer.url});
				expect(response.status).toBe(500);
			});
			it('handles 304 Not Modified without following redirects', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((_req, res) => {
					res.writeHead(HttpStatus.NOT_MODIFIED);
					res.end();
				});
				const response = await client.sendRequest({url: testServer.url});
				expect(response.status).toBe(HttpStatus.NOT_MODIFIED);
				expect(response.url).toBe(new URL(testServer.url).href);
			});
		});
		describe('redirects', () => {
			it('follows 301 redirect', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((_req, res) => {
					res.writeHead(301, {Location: `${redirectServer.url}/target`});
					res.end();
				});
				redirectServer.setHandler((req, res) => {
					res.writeHead(200, {'Content-Type': 'application/json'});
					res.end(JSON.stringify({path: req.url, method: req.method}));
				});
				const response = await client.sendRequest({url: testServer.url});
				const body = await client.streamToString(response.stream);
				const json = JSON.parse(body);
				expect(response.status).toBe(200);
				expect(response.url).toBe(`${redirectServer.url}/target`);
				expect(json.path).toBe('/target');
			});
			it('follows 301 redirect and changes POST to GET', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((_req, res) => {
					res.writeHead(301, {Location: `${redirectServer.url}/moved`});
					res.end();
				});
				redirectServer.setHandler((req, res) => {
					res.writeHead(200, {'Content-Type': 'application/json'});
					res.end(JSON.stringify({method: req.method}));
				});
				const response = await client.sendRequest({
					url: testServer.url,
					method: 'POST',
					body: {value: 'test'},
				});
				const body = await client.streamToString(response.stream);
				const json = JSON.parse(body);
				expect(response.status).toBe(200);
				expect(json.method).toBe('GET');
			});
			it('follows 302 redirect', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((_req, res) => {
					res.writeHead(302, {Location: `${redirectServer.url}/found`});
					res.end();
				});
				redirectServer.setHandler((req, res) => {
					res.writeHead(200, {'Content-Type': 'text/plain'});
					res.end(req.url ?? '');
				});
				const response = await client.sendRequest({url: testServer.url});
				const body = await client.streamToString(response.stream);
				expect(response.status).toBe(200);
				expect(body).toBe('/found');
			});
			it('follows 302 redirect preserving PATCH', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((_req, res) => {
					res.writeHead(302, {Location: `${redirectServer.url}/found`});
					res.end();
				});
				redirectServer.setHandler((req, res) => {
					res.writeHead(200, {'Content-Type': 'application/json'});
					res.end(JSON.stringify({method: req.method}));
				});
				const response = await client.sendRequest({
					url: testServer.url,
					method: 'PATCH',
					body: {value: 'test'},
				});
				const body = await client.streamToString(response.stream);
				const json = JSON.parse(body);
				expect(response.status).toBe(200);
				expect(json.method).toBe('PATCH');
			});
			it('follows 303 redirect and changes method to GET', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((_req, res) => {
					res.writeHead(303, {Location: `${redirectServer.url}/see-other`});
					res.end();
				});
				redirectServer.setHandler((req, res) => {
					res.writeHead(200, {'Content-Type': 'application/json'});
					res.end(JSON.stringify({method: req.method}));
				});
				const response = await client.sendRequest({
					url: testServer.url,
					method: 'POST',
					body: {data: 'test'},
				});
				const body = await client.streamToString(response.stream);
				const json = JSON.parse(body);
				expect(response.status).toBe(200);
				expect(json.method).toBe('GET');
			});
			it('drops content headers when redirect switches to GET', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				let contentType: string | undefined;
				let contentLength: string | undefined;
				testServer.setHandler((_req, res) => {
					res.writeHead(303, {Location: `${redirectServer.url}/see-other`});
					res.end();
				});
				redirectServer.setHandler((req, res) => {
					contentType = req.headers['content-type'] as string | undefined;
					contentLength = req.headers['content-length'] as string | undefined;
					res.writeHead(200, {'Content-Type': 'text/plain'});
					res.end('OK');
				});
				await client.sendRequest({
					url: testServer.url,
					method: 'POST',
					body: {data: 'test'},
				});
				expect(contentType).toBeUndefined();
				expect(contentLength).toBeUndefined();
			});
			it('drops content headers when 301 redirect switches to GET', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				let contentType: string | undefined;
				let contentLength: string | undefined;
				testServer.setHandler((_req, res) => {
					res.writeHead(301, {Location: `${redirectServer.url}/moved`});
					res.end();
				});
				redirectServer.setHandler((req, res) => {
					contentType = req.headers['content-type'] as string | undefined;
					contentLength = req.headers['content-length'] as string | undefined;
					res.writeHead(200, {'Content-Type': 'text/plain'});
					res.end('OK');
				});
				await client.sendRequest({
					url: testServer.url,
					method: 'POST',
					body: {data: 'test'},
				});
				expect(contentType).toBeUndefined();
				expect(contentLength).toBeUndefined();
			});
			it('strips sensitive headers on cross-origin redirects', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				const secretToken = 'Bearer ultra-secret-token';
				const secretCookie = 'session=super-secret';
				const secretProxyAuth = 'Basic dXNlcjpwYXNz';
				let leakedAuthorization: string | undefined;
				let leakedCookie: string | undefined;
				let leakedProxyAuthorization: string | undefined;
				testServer.setHandler((_req, res) => {
					res.writeHead(302, {Location: `${redirectServer.url}/steal`});
					res.end();
				});
				redirectServer.setHandler((req, res) => {
					leakedAuthorization = req.headers['authorization'] as string | undefined;
					leakedCookie = req.headers['cookie'] as string | undefined;
					leakedProxyAuthorization = req.headers['proxy-authorization'] as string | undefined;
					res.writeHead(200);
					res.end('OK');
				});
				await client.sendRequest({
					url: testServer.url,
					headers: {
						Authorization: secretToken,
						Cookie: secretCookie,
						'Proxy-Authorization': secretProxyAuth,
					},
				});
				expect(leakedAuthorization).toBeUndefined();
				expect(leakedCookie).toBeUndefined();
				expect(leakedProxyAuthorization).toBeUndefined();
			});
			it('keeps sensitive headers on same-origin redirects', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				const secretToken = 'Bearer safe-token';
				let receivedAuthorization: string | undefined;
				let requestCount = 0;
				testServer.setHandler((req, res) => {
					requestCount += 1;
					if (requestCount === 1) {
						res.writeHead(302, {Location: '/same-origin'});
						res.end();
						return;
					}
					receivedAuthorization = req.headers['authorization'] as string | undefined;
					res.writeHead(200);
					res.end('OK');
				});
				await client.sendRequest({
					url: testServer.url,
					headers: {
						Authorization: secretToken,
					},
				});
				expect(receivedAuthorization).toBe(secretToken);
			});
			it('follows 307 redirect preserving method', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((_req, res) => {
					res.writeHead(307, {Location: `${redirectServer.url}/temp`});
					res.end();
				});
				redirectServer.setHandler((req, res) => {
					res.writeHead(200, {'Content-Type': 'application/json'});
					res.end(JSON.stringify({method: req.method}));
				});
				const response = await client.sendRequest({
					url: testServer.url,
					method: 'POST',
					body: {data: 'test'},
				});
				const body = await client.streamToString(response.stream);
				const json = JSON.parse(body);
				expect(response.status).toBe(200);
				expect(json.method).toBe('POST');
			});
			it('follows 308 redirect preserving method', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((_req, res) => {
					res.writeHead(308, {Location: `${redirectServer.url}/permanent`});
					res.end();
				});
				redirectServer.setHandler((req, res) => {
					res.writeHead(200, {'Content-Type': 'application/json'});
					res.end(JSON.stringify({method: req.method}));
				});
				const response = await client.sendRequest({
					url: testServer.url,
					method: 'POST',
					body: {data: 'test'},
				});
				const body = await client.streamToString(response.stream);
				const json = JSON.parse(body);
				expect(response.status).toBe(200);
				expect(json.method).toBe('POST');
			});
			it('follows multiple redirects up to max limit', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				let redirectCount = 0;
				testServer.setHandler((_req, res) => {
					redirectCount++;
					if (redirectCount < 5) {
						res.writeHead(302, {Location: `${testServer.url}/redirect${redirectCount}`});
						res.end();
					} else {
						res.writeHead(200, {'Content-Type': 'application/json'});
						res.end(JSON.stringify({redirectCount}));
					}
				});
				const response = await client.sendRequest({url: testServer.url});
				const body = await client.streamToString(response.stream);
				const json = JSON.parse(body);
				expect(response.status).toBe(200);
				expect(json.redirectCount).toBe(5);
			});
			it('throws error when exceeding max redirects', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((_req, res) => {
					res.writeHead(302, {Location: `${testServer.url}/redirect`});
					res.end();
				});
				await expect(client.sendRequest({url: testServer.url})).rejects.toThrow(
					'Maximum number of redirects (5) exceeded',
				);
			});
			it('throws error when redirect has no Location header', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((_req, res) => {
					res.writeHead(302);
					res.end();
				});
				await expect(client.sendRequest({url: testServer.url})).rejects.toThrow(
					'Received redirect response without Location header',
				);
			});
			it('handles relative redirect URLs', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				let requestCount = 0;
				testServer.setHandler((req, res) => {
					requestCount++;
					if (requestCount === 1) {
						res.writeHead(302, {Location: '/relative-path'});
						res.end();
					} else {
						res.writeHead(200, {'Content-Type': 'application/json'});
						res.end(JSON.stringify({path: req.url}));
					}
				});
				const response = await client.sendRequest({url: testServer.url});
				const body = await client.streamToString(response.stream);
				const json = JSON.parse(body);
				expect(response.status).toBe(200);
				expect(json.path).toBe('/relative-path');
			});
			it('validates redirect targets with request URL policy before following', async () => {
				const validationCalls: Array<RequestUrlValidationContext> = [];
				const requestUrlPolicy: RequestUrlPolicy = {
					async validate(_url, context) {
						validationCalls.push(context);
						if (context.phase === 'redirect') {
							throw new HttpError('Blocked redirect target', undefined, undefined, true, 'network_error');
						}
					},
				};
				const client = createHttpClient({
					userAgent: TEST_USER_AGENT,
					requestUrlPolicy,
				});
				testServer.setHandler((_req, res) => {
					res.writeHead(302, {Location: `${redirectServer.url}/blocked`});
					res.end();
				});
				redirectServer.setHandler((_req, res) => {
					res.writeHead(200);
					res.end('should-not-be-called');
				});
				await expect(client.sendRequest({url: testServer.url})).rejects.toThrow('Blocked redirect target');
				expect(validationCalls).toHaveLength(2);
				expect(validationCalls[0]?.phase).toBe('initial');
				expect(validationCalls[1]?.phase).toBe('redirect');
			});
		});
		describe('timeout', () => {
			it('uses default timeout of 30 seconds', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((_req, res) => {
					res.writeHead(200);
					res.end('OK');
				});
				const response = await client.sendRequest({url: testServer.url});
				expect(response.status).toBe(200);
			});
			it('respects custom timeout', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				testServer.setHandler((_req, res) => {
					setTimeout(() => {
						res.writeHead(200);
						res.end('OK');
					}, 500);
				});
				await expect(
					client.sendRequest({
						url: testServer.url,
						timeout: 100,
					}),
				).rejects.toThrow();
			});
		});
		describe('abort signal', () => {
			it('aborts request when signal is triggered', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				const controller = new AbortController();
				testServer.setHandler((_req, res) => {
					setTimeout(() => {
						res.writeHead(200);
						res.end('OK');
					}, 1000);
				});
				const requestPromise = client.sendRequest({
					url: testServer.url,
					signal: controller.signal,
				});
				setTimeout(() => controller.abort(), 50);
				await expect(requestPromise).rejects.toThrow();
			});
			it('handles pre-aborted signal', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				const controller = new AbortController();
				controller.abort('Pre-aborted');
				testServer.setHandler((_req, res) => {
					res.writeHead(200);
					res.end('OK');
				});
				await expect(
					client.sendRequest({
						url: testServer.url,
						signal: controller.signal,
					}),
				).rejects.toThrow();
			});
		});
		describe('error handling', () => {
			it('throws HttpError for connection refused', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				try {
					await client.sendRequest({url: 'http://127.0.0.1:1'});
					expect.fail('Should have thrown');
				} catch (error) {
					expect(error).toBeInstanceOf(HttpError);
					const httpError = error as HttpError;
					expect(httpError.isExpected).toBe(true);
				}
			});
			it('throws HttpError for DNS resolution failure', async () => {
				const client = createHttpClient(TEST_USER_AGENT);
				try {
					await client.sendRequest({url: 'http://this-domain-does-not-exist-12345.invalid'});
					expect.fail('Should have thrown');
				} catch (error) {
					expect(error).toBeInstanceOf(HttpError);
					const httpError = error as HttpError;
					expect(httpError.isExpected).toBe(true);
				}
			});
		});
	});
	describe('streamToString', () => {
		it('converts response stream to string', async () => {
			const client = createHttpClient(TEST_USER_AGENT);
			testServer.setHandler((_req, res) => {
				res.writeHead(200, {'Content-Type': 'text/plain'});
				res.end('Hello, World!');
			});
			const response = await client.sendRequest({url: testServer.url});
			const body = await client.streamToString(response.stream);
			expect(body).toBe('Hello, World!');
		});
		it('handles empty response body', async () => {
			const client = createHttpClient(TEST_USER_AGENT);
			testServer.setHandler((_req, res) => {
				res.writeHead(204);
				res.end();
			});
			const response = await client.sendRequest({url: testServer.url});
			const body = await client.streamToString(response.stream);
			expect(body).toBe('');
		});
		it('handles large response body', async () => {
			const client = createHttpClient(TEST_USER_AGENT);
			const largeContent = 'x'.repeat(1024 * 1024);
			testServer.setHandler((_req, res) => {
				res.writeHead(200, {'Content-Type': 'text/plain'});
				res.end(largeContent);
			});
			const response = await client.sendRequest({url: testServer.url});
			const body = await client.streamToString(response.stream);
			expect(body.length).toBe(largeContent.length);
		});
		it('handles UTF-8 content correctly', async () => {
			const client = createHttpClient(TEST_USER_AGENT);
			const unicodeContent = 'Hello, World! Emoji: \u{1F600} Chinese: \u4E2D\u6587 Arabic: \u0639\u0631\u0628\u064A';
			testServer.setHandler((_req, res) => {
				res.writeHead(200, {'Content-Type': 'text/plain; charset=utf-8'});
				res.end(unicodeContent);
			});
			const response = await client.sendRequest({url: testServer.url});
			const body = await client.streamToString(response.stream);
			expect(body).toBe(unicodeContent);
		});
	});
	describe('telemetry', () => {
		it('records metrics for successful requests', async () => {
			const recordedMetrics: Array<{
				type: string;
				name: string;
				dimensions?: Record<string, string>;
			}> = [];
			const metrics: HttpClientMetrics = {
				counter: (params) => {
					recordedMetrics.push({type: 'counter', ...params});
				},
				histogram: (params) => {
					recordedMetrics.push({type: 'histogram', ...params});
				},
			};
			const telemetry: HttpClientTelemetry = {metrics};
			const client = createHttpClient(TEST_USER_AGENT, telemetry);
			testServer.setHandler((_req, res) => {
				res.writeHead(200);
				res.end('OK');
			});
			await client.sendRequest({url: testServer.url, serviceName: 'test-service'});
			const latencyMetric = recordedMetrics.find((m) => m.name === 'http_client.latency');
			const requestMetric = recordedMetrics.find((m) => m.name === 'http_client.request');
			const responseMetric = recordedMetrics.find((m) => m.name === 'http_client.response');
			expect(latencyMetric).toBeDefined();
			expect(latencyMetric?.dimensions?.service).toBe('test-service');
			expect(latencyMetric?.dimensions?.method).toBe('GET');
			expect(requestMetric).toBeDefined();
			expect(requestMetric?.dimensions?.service).toBe('test-service');
			expect(requestMetric?.dimensions?.status).toBe('2xx');
			expect(responseMetric).toBeDefined();
			expect(responseMetric?.dimensions?.service).toBe('test-service');
			expect(responseMetric?.dimensions?.status_code).toBe('2xx');
		});
		it('records metrics for error responses', async () => {
			const recordedMetrics: Array<{
				type: string;
				name: string;
				dimensions?: Record<string, string>;
			}> = [];
			const metrics: HttpClientMetrics = {
				counter: (params) => {
					recordedMetrics.push({type: 'counter', ...params});
				},
				histogram: (params) => {
					recordedMetrics.push({type: 'histogram', ...params});
				},
			};
			const telemetry: HttpClientTelemetry = {metrics};
			const client = createHttpClient(TEST_USER_AGENT, telemetry);
			testServer.setHandler((_req, res) => {
				res.writeHead(404);
				res.end('Not Found');
			});
			await client.sendRequest({url: testServer.url, serviceName: 'test-service'});
			const requestMetric = recordedMetrics.find((m) => m.name === 'http_client.request');
			expect(requestMetric?.dimensions?.status).toBe('404');
		});
		it('records metrics for network errors', async () => {
			const recordedMetrics: Array<{
				type: string;
				name: string;
				dimensions?: Record<string, string>;
			}> = [];
			const metrics: HttpClientMetrics = {
				counter: (params) => {
					recordedMetrics.push({type: 'counter', ...params});
				},
				histogram: (params) => {
					recordedMetrics.push({type: 'histogram', ...params});
				},
			};
			const telemetry: HttpClientTelemetry = {metrics};
			const client = createHttpClient(TEST_USER_AGENT, telemetry);
			try {
				await client.sendRequest({url: 'http://127.0.0.1:1', serviceName: 'test-service'});
			} catch {}
			const requestMetric = recordedMetrics.find((m) => m.name === 'http_client.request');
			expect(requestMetric?.dimensions?.status).toBe('network_error');
		});
		it('uses default service name when not provided', async () => {
			const recordedMetrics: Array<{
				type: string;
				name: string;
				dimensions?: Record<string, string>;
			}> = [];
			const metrics: HttpClientMetrics = {
				counter: (params) => {
					recordedMetrics.push({type: 'counter', ...params});
				},
				histogram: (params) => {
					recordedMetrics.push({type: 'histogram', ...params});
				},
			};
			const telemetry: HttpClientTelemetry = {metrics};
			const client = createHttpClient(TEST_USER_AGENT, telemetry);
			testServer.setHandler((_req, res) => {
				res.writeHead(200);
				res.end('OK');
			});
			await client.sendRequest({url: testServer.url});
			const latencyMetric = recordedMetrics.find((m) => m.name === 'http_client.latency');
			expect(latencyMetric?.dimensions?.service).toBe('unknown');
		});
	});
});

describe('HttpError', () => {
	it('creates error with message', () => {
		const error = new HttpError('Test error');
		expect(error.message).toBe('Test error');
		expect(error.name).toBe('HttpError');
		expect(error.status).toBeUndefined();
		expect(error.response).toBeUndefined();
		expect(error.isExpected).toBe(false);
		expect(error.errorType).toBeUndefined();
	});
	it('creates error with status code', () => {
		const error = new HttpError('Not Found', 404);
		expect(error.message).toBe('Not Found');
		expect(error.status).toBe(404);
	});
	it('creates error with response', () => {
		const response = new Response('Error body', {status: 500});
		const error = new HttpError('Server Error', 500, response);
		expect(error.status).toBe(500);
		expect(error.response).toBe(response);
	});
	it('creates error with isExpected flag', () => {
		const error = new HttpError('Expected error', 400, undefined, true);
		expect(error.isExpected).toBe(true);
	});
	it('creates error with errorType', () => {
		const error = new HttpError('Aborted', undefined, undefined, false, 'aborted');
		expect(error.errorType).toBe('aborted');
	});
	it('is an instance of Error', () => {
		const error = new HttpError('Test');
		expect(error).toBeInstanceOf(Error);
		expect(error).toBeInstanceOf(HttpError);
	});
});
