import {createServer, request as forwardRequest, type Server} from 'node:http';
import {createTestServer, type TestServer} from '@pkgs/http_client/src/__tests__/TestHttpServer';
import {createHttpClient} from '@pkgs/http_client/src/HttpClient';
import {createPublicInternetRequestUrlPolicy} from '@pkgs/http_client/src/PublicInternetRequestUrlPolicy';
import {afterAll, beforeAll, describe, expect, it} from 'vitest';

describe('PublicInternetRequestUrlPolicy egress proxy', () => {
	let origin: TestServer;
	let proxy: Server;
	let proxyUrl: string;
	const proxiedTargets: Array<string> = [];
	const savedEnv = {HTTP_PROXY: process.env.HTTP_PROXY, http_proxy: process.env.http_proxy};

	beforeAll(async () => {
		origin = await createTestServer();
		origin.setHandler((req, res) => {
			res.writeHead(200, {'content-type': 'text/plain'});
			res.end(`host=${req.headers.host}`);
		});
		proxy = createServer((req, res) => {
			proxiedTargets.push(req.url ?? '');
			const upstream = forwardRequest(
				{
					host: '127.0.0.1',
					port: origin.port,
					method: req.method,
					path: new URL(req.url ?? '/').pathname,
					headers: req.headers,
				},
				(upstreamResponse) => {
					res.writeHead(upstreamResponse.statusCode ?? 502, upstreamResponse.headers);
					upstreamResponse.pipe(res);
				},
			);
			upstream.on('error', () => res.destroy());
			req.pipe(upstream);
		});
		await new Promise<void>((resolve) => proxy.listen(0, '127.0.0.1', resolve));
		const address = proxy.address();
		if (!address || typeof address === 'string') {
			throw new Error('Failed to get proxy address');
		}
		proxyUrl = `http://127.0.0.1:${address.port}`;
	});

	afterAll(async () => {
		for (const [key, value] of Object.entries(savedEnv)) {
			if (value === undefined) {
				delete process.env[key];
			} else {
				process.env[key] = value;
			}
		}
		proxy.closeAllConnections();
		await new Promise<void>((resolve) => proxy.close(() => resolve()));
		await origin.close();
	});

	it('sends public requests through HTTP_PROXY and leaves name resolution to the proxy', async () => {
		delete process.env.http_proxy;
		process.env.HTTP_PROXY = proxyUrl;
		const requestUrlPolicy = createPublicInternetRequestUrlPolicy({
			lookupHost: async () => ['93.184.215.14'],
		});
		const client = createHttpClient({userAgent: 'test', requestUrlPolicy});

		const response = await client.sendRequest({url: 'http://media.example.com/image.png'});

		expect(response.status).toBe(200);
		expect(await client.streamToString(response.stream)).toBe('host=media.example.com');
		expect(proxiedTargets).toEqual(['http://media.example.com/image.png']);
	});
});
