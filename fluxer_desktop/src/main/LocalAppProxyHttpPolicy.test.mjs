// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();
const {
	buildAPIRequestHeaders,
	buildAPITargetURL,
	buildProxyResponseHeaders,
	LocalAppProxyCacheDefault,
	buildRemoteResourceRequestHeaders,
	isAllowedRemoteProxyTarget,
	isSupportedRemoteResourceMethod,
	localAppProxyNotFoundResponse,
	readLocalAppUploadId,
	readRequestContentLength,
	remoteProxyTargetURL,
	requestHasBody,
} = await import('@electron/main/LocalAppProxyHTTPPolicy');
const {DESKTOP_PROTOCOL_AUTHORIZATION_HEADER} = await import('@electron/main/LocalAppProtocolAuthorization');
const {LOCAL_APP_UPLOAD_ID_HEADER} = await import('@fluxer/desktop_ipc/src/LocalAppRouteContract');

function plan(overrides = {}) {
	return {
		instanceKey: 'https://api.fluxer.app',
		document: {},
		selfHosted: false,
		degraded: false,
		endpoints: {
			apiEndpoint: 'https://api.fluxer.app',
			apiPublicEndpoint: 'https://api.fluxer.app',
			webAppEndpoint: 'https://web.fluxer.app',
			mediaEndpoint: 'https://media.fluxer.app',
			staticCdnEndpoint: 'https://cdn.fluxer.app',
			uploadRelayEndpoint: 'https://media.fluxer.app/upload',
			gatewayEndpoint: 'wss://gateway.fluxer.app',
			inviteEndpoint: null,
			giftEndpoint: null,
			...overrides,
		},
	};
}

function allowsRemoteResource(targetURL, runtimePlan = plan()) {
	return isAllowedRemoteProxyTarget({method: 'GET', targetURL, plan: runtimePlan});
}

function allowsUploadRelay(targetURL, runtimePlan = plan()) {
	return isAllowedRemoteProxyTarget({method: 'PUT', targetURL, plan: runtimePlan});
}

function headerNames(headers) {
	return [...headers.keys()].sort();
}

describe('rewriting the local API path onto the instance API endpoint', () => {
	const cases = [
		['https://api.fluxer.app', '/api/k/v1/users/@me?x=1', '/api/k', 'https://api.fluxer.app/v1/users/@me?x=1'],
		['https://h.example/base/', '/api/k/v1/x', '/api/k', 'https://h.example/base/v1/x'],
		['https://h.example/base', '/api/k/v1/x', '/api/k', 'https://h.example/base/v1/x'],
		['https://h.example/base', '/api/k', '/api/k', 'https://h.example/base/'],
		['https://h.example', '/api/k', '/api/k', 'https://h.example/'],
		['https://h.example/base//', '/api/k/v1/x', '/api/k', 'https://h.example/base/v1/x'],
	];

	for (const [apiEndpoint, requestPath, localPathPrefix, expected] of cases) {
		test(`${apiEndpoint} + ${requestPath}`, () => {
			const requestURL = `fluxer-app://app${requestPath}`;
			assert.equal(buildAPITargetURL({apiEndpoint, requestURL, localPathPrefix}), expected);
		});
	}

	test('the renderer contributes only path and query, never the host', () => {
		const target = buildAPITargetURL({
			apiEndpoint: 'https://api.fluxer.app',
			requestURL: 'fluxer-app://app/api/k/..%2F..%2Fv1/x?y=1',
			localPathPrefix: '/api/k',
		});
		assert.equal(new URL(target).origin, 'https://api.fluxer.app');
	});
});

describe('outbound API request headers', () => {
	test('a non-GET carries the instance own web app origin, which MiddlewarePipeline requires', () => {
		const headers = buildAPIRequestHeaders({
			headers: new Headers({Origin: 'fluxer-app://app', Authorization: 'token'}),
			method: 'POST',
			origin: 'https://web.fluxer.app',
		});
		assert.equal(headers.get('Origin'), 'https://web.fluxer.app');
		assert.equal(headers.get('Authorization'), 'token');
	});

	test('a GET carries no Origin at all', () => {
		const headers = buildAPIRequestHeaders({
			headers: new Headers({Origin: 'fluxer-app://app'}),
			method: 'GET',
			origin: 'https://web.fluxer.app',
		});
		assert.equal(headers.get('Origin'), null);
	});

	test('a renderer-supplied Origin can never survive, even when the plan has none', () => {
		for (const method of ['GET', 'HEAD', 'POST', 'PATCH', 'DELETE']) {
			const headers = buildAPIRequestHeaders({
				headers: new Headers({Origin: 'https://web.fluxer.app'}),
				method,
				origin: null,
			});
			assert.equal(headers.get('Origin'), null, method);
		}
	});

	test('requestHasBody decides which methods get an Origin', () => {
		assert.equal(requestHasBody('GET'), false);
		assert.equal(requestHasBody('HEAD'), false);
		assert.equal(requestHasBody('POST'), true);
		assert.equal(requestHasBody('PUT'), true);
		assert.equal(requestHasBody('PATCH'), true);
		assert.equal(requestHasBody('DELETE'), true);
	});

	test('the protocol authorization secret and the upload id never leave the machine', () => {
		const headers = buildAPIRequestHeaders({
			headers: new Headers({
				[DESKTOP_PROTOCOL_AUTHORIZATION_HEADER]: 'secret',
				[LOCAL_APP_UPLOAD_ID_HEADER]: 'abc',
				Accept: 'application/json',
			}),
			method: 'POST',
			origin: 'https://web.fluxer.app',
		});
		assert.equal(headers.get(DESKTOP_PROTOCOL_AUTHORIZATION_HEADER), null);
		assert.equal(headers.get(LOCAL_APP_UPLOAD_ID_HEADER), null);
		assert.equal(headers.get('Accept'), 'application/json');
	});

	test('hop-by-hop, cookie, host, referer and content-length are dropped', () => {
		const headers = buildAPIRequestHeaders({
			headers: new Headers({
				Connection: 'keep-alive',
				'Keep-Alive': 'timeout=5',
				TE: 'trailers',
				Upgrade: 'websocket',
				Cookie: 'a=b',
				Host: 'app',
				Referer: 'fluxer-app://app/channels/@me',
				'Content-Length': '10',
				'Accept-Encoding': 'gzip',
				Expect: '100-continue',
				'Proxy-Connection': 'keep-alive',
				'X-Fluxer-Client-Installation-Id': 'install',
			}),
			method: 'POST',
			origin: null,
		});
		assert.deepEqual(headerNames(headers), ['x-fluxer-client-installation-id']);
	});
});

describe('outbound remote-resource request headers', () => {
	test('credentials are stripped on the /proxy route but survive on /api', () => {
		const source = new Headers({Authorization: 'token', 'X-Fluxer-Sudo-Mode-Jwt': 'jwt', Range: 'bytes=0-10'});
		const proxied = buildRemoteResourceRequestHeaders(source);
		assert.equal(proxied.get('Authorization'), null);
		assert.equal(proxied.get('X-Fluxer-Sudo-Mode-Jwt'), null);
		assert.equal(proxied.get('Range'), 'bytes=0-10');
		const api = buildAPIRequestHeaders({headers: source, method: 'GET', origin: null});
		assert.equal(api.get('Authorization'), 'token');
		assert.equal(api.get('X-Fluxer-Sudo-Mode-Jwt'), 'jwt');
	});
});

describe('proxied response headers', () => {
	test('CORS, cookies, encodings and lengths are stripped', () => {
		const headers = buildProxyResponseHeaders({
			cacheDefault: LocalAppProxyCacheDefault.NO_STORE,
			headers: new Headers({
				'Access-Control-Allow-Origin': '*',
				'Access-Control-Allow-Credentials': 'true',
				'Access-Control-Expose-Headers': 'X-Fluxer-Version',
				'Access-Control-Max-Age': '86400',
				'Set-Cookie': 'a=b',
				'Content-Encoding': 'gzip',
				'Content-Length': '10',
				'Content-Security-Policy': "default-src 'self'",
				'Transfer-Encoding': 'chunked',
				'Content-Type': 'application/json',
			}),
		});
		assert.equal(headers.get('Content-Type'), 'application/json');
		for (const name of [
			'Access-Control-Allow-Origin',
			'Access-Control-Allow-Credentials',
			'Access-Control-Expose-Headers',
			'Access-Control-Max-Age',
			'Set-Cookie',
			'Content-Encoding',
			'Content-Length',
			'Transfer-Encoding',
		]) {
			assert.equal(headers.get(name), null, name);
		}
	});

	test('the response is forced into a non-document, non-framable posture', () => {
		const headers = buildProxyResponseHeaders({
			cacheDefault: LocalAppProxyCacheDefault.NO_STORE,
			headers: new Headers({'Content-Type': 'text/html'}),
		});
		assert.equal(headers.get('Content-Security-Policy'), "sandbox; default-src 'none'; frame-ancestors 'none'");
		assert.equal(headers.get('X-Content-Type-Options'), 'nosniff');
	});

	test('headers the renderer actually reads through XHR survive', () => {
		const headers = buildProxyResponseHeaders({
			cacheDefault: LocalAppProxyCacheDefault.NO_STORE,
			headers: new Headers({
				'X-Fluxer-Version': '1',
				'Retry-After': '5',
				'X-RateLimit-Reset-After': '2.5',
				'X-Fluxer-Sudo-Mode-Jwt': 'rotated',
			}),
		});
		assert.equal(headers.get('X-Fluxer-Version'), '1');
		assert.equal(headers.get('Retry-After'), '5');
		assert.equal(headers.get('X-RateLimit-Reset-After'), '2.5');
		assert.equal(headers.get('X-Fluxer-Sudo-Mode-Jwt'), 'rotated');
	});

	test('an upstream Cache-Control always wins', () => {
		for (const cacheDefault of [LocalAppProxyCacheDefault.NO_STORE, LocalAppProxyCacheDefault.UPSTREAM]) {
			assert.equal(
				buildProxyResponseHeaders({
					cacheDefault,
					headers: new Headers({'Cache-Control': 'public, max-age=60'}),
				}).get('Cache-Control'),
				'public, max-age=60',
				cacheDefault,
			);
		}
	});

	test('an API response that declares no caching is held to no-store', () => {
		assert.equal(
			buildProxyResponseHeaders({
				cacheDefault: LocalAppProxyCacheDefault.NO_STORE,
				headers: new Headers(),
			}).get('Cache-Control'),
			'no-store',
		);
	});

	test('a remote resource that declares no caching keeps its upstream validators and stays cacheable', () => {
		const headers = buildProxyResponseHeaders({
			cacheDefault: LocalAppProxyCacheDefault.UPSTREAM,
			headers: new Headers({ETag: '"abc"', 'Last-Modified': 'Wed, 21 Oct 2015 07:28:00 GMT'}),
		});
		assert.equal(headers.get('Cache-Control'), null);
		assert.equal(headers.get('ETag'), '"abc"');
		assert.equal(headers.get('Last-Modified'), 'Wed, 21 Oct 2015 07:28:00 GMT');
	});
});

describe('resolving the remote resource target', () => {
	test('an absolute http target is accepted', () => {
		assert.equal(
			remoteProxyTargetURL('fluxer-app://app/proxy/k?url=https%3A%2F%2Fmedia.fluxer.app%2Fx.png'),
			'https://media.fluxer.app/x.png',
		);
	});

	test('a missing, relative, credentialed or non-http target is refused', () => {
		assert.equal(remoteProxyTargetURL('fluxer-app://app/proxy/k'), null);
		assert.equal(remoteProxyTargetURL('fluxer-app://app/proxy/k?url='), null);
		assert.equal(remoteProxyTargetURL('fluxer-app://app/proxy/k?url=%2Fetc%2Fpasswd'), null);
		assert.equal(remoteProxyTargetURL('fluxer-app://app/proxy/k?url=file%3A%2F%2F%2Fetc%2Fpasswd'), null);
		assert.equal(remoteProxyTargetURL('fluxer-app://app/proxy/k?url=javascript%3Aalert(1)'), null);
		assert.equal(remoteProxyTargetURL('fluxer-app://app/proxy/k?url=https%3A%2F%2Fu%3Ap%40evil.example%2F'), null);
	});

	test('only GET, HEAD and PUT reach the remote resource route', () => {
		assert.equal(isSupportedRemoteResourceMethod('GET'), true);
		assert.equal(isSupportedRemoteResourceMethod('HEAD'), true);
		assert.equal(isSupportedRemoteResourceMethod('PUT'), true);
		assert.equal(isSupportedRemoteResourceMethod('POST'), false);
		assert.equal(isSupportedRemoteResourceMethod('DELETE'), false);
		assert.equal(isSupportedRemoteResourceMethod('get'), false);
	});
});

describe('the remote resource origin allowlist', () => {
	test('only origins named by the active plan are reachable', () => {
		for (const target of [
			'https://api.fluxer.app/v1/x',
			'https://web.fluxer.app/x',
			'https://media.fluxer.app/attachments/x.png',
			'https://cdn.fluxer.app/libs/x.wasm',
		]) {
			assert.equal(allowsRemoteResource(target), true, target);
		}
	});

	test('an origin outside the plan is refused, including a lookalike host', () => {
		for (const target of [
			'https://evil.example/x',
			'https://media.fluxer.app.evil.example/x',
			'http://media.fluxer.app/x',
			'https://media.fluxer.app:8443/x',
			'fluxer-app://app/x',
		]) {
			assert.equal(allowsRemoteResource(target), false, target);
		}
	});

	test('the gateway origin is not a remote-resource target', () => {
		assert.equal(allowsRemoteResource('https://gateway.fluxer.app/x'), false);
	});

	test('GET and PUT take different admission paths', () => {
		const relay = 'https://media.fluxer.app/upload/v1/relay/attachments%2Fa.png?t=TOKEN';
		assert.equal(isAllowedRemoteProxyTarget({method: 'GET', targetURL: relay, plan: plan()}), true);
		assert.equal(
			isAllowedRemoteProxyTarget({method: 'PUT', targetURL: 'https://media.fluxer.app/x.png', plan: plan()}),
			false,
		);
	});
});

describe('the upload relay admission rule matches fluxer_api UploadRelay.ts', () => {
	test('a single-part relay PUT is admitted', () => {
		assert.equal(allowsUploadRelay('https://media.fluxer.app/upload/v1/relay/attachments%2Fa.png?t=TOKEN'), true);
	});

	test('a multipart part upload carrying uploadId and partNumber is admitted', () => {
		assert.equal(
			allowsUploadRelay(
				'https://media.fluxer.app/upload/v1/relay/attachments%2Fa.png?t=TOKEN&uploadId=U1&partNumber=2',
			),
			true,
		);
	});

	test('a relay endpoint advertised with its own /v1/relay suffix normalises the same way the API does', () => {
		const relayPlan = plan({uploadRelayEndpoint: 'https://media.fluxer.app/upload/v1/relay'});
		assert.equal(
			allowsUploadRelay('https://media.fluxer.app/upload/v1/relay/attachments%2Fa.png?t=TOKEN', relayPlan),
			true,
			'KNOWN BUG (WP-I): isAllowedUploadRelayEndpoint rejects any endpoint whose pathname contains a relay ' +
				'segment before uploadRelayBasePath gets to strip the /v1/relay suffix, so UPLOAD_RELAY_BASE_SUFFIX_PATTERN ' +
				'is dead code. fluxer_api relayEndpointBase (UploadRelay.ts:74) and the renderer parseUploadRelayBase ' +
				'(DesktopResourceUrl.ts:139) both strip the suffix first, so an operator who sets ' +
				'FLUXER_MEDIA_PROXY_UPLOAD_RELAY_ENDPOINT to a URL ending in /v1/relay gets renderer-wrapped uploads that ' +
				'main 403s. Fix: strip the suffix before the relay-segment guard, exactly as the renderer does.',
		);
	});

	test('a root relay endpoint admits /v1/relay/<key>', () => {
		const relayPlan = plan({uploadRelayEndpoint: 'https://media.fluxer.app'});
		assert.equal(allowsUploadRelay('https://media.fluxer.app/v1/relay/attachments%2Fa.png?t=TOKEN', relayPlan), true);
	});

	test('a missing token, an empty token or a duplicated token is refused', () => {
		const base = 'https://media.fluxer.app/upload/v1/relay/attachments%2Fa.png';
		assert.equal(allowsUploadRelay(base), false);
		assert.equal(allowsUploadRelay(`${base}?t=`), false);
		assert.equal(allowsUploadRelay(`${base}?t=A&t=B`), false);
	});

	test('an unknown query parameter is refused', () => {
		const base = 'https://media.fluxer.app/upload/v1/relay/attachments%2Fa.png';
		assert.equal(allowsUploadRelay(`${base}?t=TOKEN&redirect=https://evil.example`), false);
	});

	test('a hash, a wrong origin, an empty key or a path outside the relay prefix is refused', () => {
		assert.equal(allowsUploadRelay('https://media.fluxer.app/upload/v1/relay/a.png?t=T#frag'), false);
		assert.equal(allowsUploadRelay('https://evil.example/upload/v1/relay/a.png?t=T'), false);
		assert.equal(allowsUploadRelay('https://media.fluxer.app/upload/v1/relay/?t=T'), false);
		assert.equal(allowsUploadRelay('https://media.fluxer.app/upload/v1/x/a.png?t=T'), false);
		assert.equal(allowsUploadRelay('https://media.fluxer.app/v1/relay/a.png?t=T'), false);
	});

	test('no advertised relay endpoint means no relay PUT is ever admitted', () => {
		const relayPlan = plan({uploadRelayEndpoint: null});
		assert.equal(allowsUploadRelay('https://media.fluxer.app/upload/v1/relay/a.png?t=T', relayPlan), false);
	});
});

describe('upload correlation and content length', () => {
	test('only a safe correlation token is accepted', () => {
		assert.equal(readLocalAppUploadId(new Headers({[LOCAL_APP_UPLOAD_ID_HEADER]: 'abc-DEF_123'})), 'abc-DEF_123');
		assert.equal(readLocalAppUploadId(new Headers({[LOCAL_APP_UPLOAD_ID_HEADER]: 'a b'})), null);
		assert.equal(readLocalAppUploadId(new Headers({[LOCAL_APP_UPLOAD_ID_HEADER]: ''})), null);
		assert.equal(readLocalAppUploadId(new Headers({[LOCAL_APP_UPLOAD_ID_HEADER]: 'x'.repeat(129)})), null);
		assert.equal(readLocalAppUploadId(new Headers()), null);
	});

	test('content length is parsed strictly', () => {
		assert.equal(readRequestContentLength(new Headers({'Content-Length': '1024'})), 1024);
		assert.equal(readRequestContentLength(new Headers({'Content-Length': ' 1024 '})), 1024);
		assert.equal(readRequestContentLength(new Headers({'Content-Length': '-1'})), null);
		assert.equal(readRequestContentLength(new Headers({'Content-Length': '1e3'})), null);
		assert.equal(readRequestContentLength(new Headers()), null);
	});
});

describe('the not-found response is an oracle-free 404', () => {
	test('it never reveals why', async () => {
		const response = localAppProxyNotFoundResponse('Not found');
		assert.equal(response.status, 404);
		assert.equal(await response.text(), 'Not found');
		assert.equal(response.headers.get('Content-Type'), 'text/plain; charset=utf-8');
	});
});
