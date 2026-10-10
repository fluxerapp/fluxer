import {createTestServer, type TestServer} from '@pkgs/http_client/src/__tests__/TestHttpServer';
import {createHttpClient} from '@pkgs/http_client/src/HttpClient';
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
	describe('sendRequest', () => {
		describe('redirects', () => {
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
	});
});
