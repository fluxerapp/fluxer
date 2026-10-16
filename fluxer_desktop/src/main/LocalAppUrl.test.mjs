// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {isLocalAppRendererDocumentURL, isLocalAppURL, isReservedLocalAppProxyPath} = await import('./LocalAppURL.ts');

describe('local app URL recognition', () => {
	test('accepts the local app origin and its routes', () => {
		assert.equal(isLocalAppURL('fluxer-app://app/'), true);
		assert.equal(isLocalAppURL('fluxer-app://app'), true);
		assert.equal(isLocalAppURL('fluxer-app://app/channels/@me'), true);
		assert.equal(isLocalAppURL('fluxer-app://app/assets/deadbeefdeadbeef.js?v=1#x'), true);
	});

	test('rejects the credential, port and host bypasses', () => {
		assert.equal(isLocalAppURL('fluxer-app://evil@app:1234/x'), false);
		assert.equal(isLocalAppURL('fluxer-app://u:p@app/'), false);
		assert.equal(isLocalAppURL('fluxer-app://app:8080/'), false);
		assert.equal(isLocalAppURL('fluxer-app://evil/'), false);
		assert.equal(isLocalAppURL('fluxer-app://app.evil.example/'), false);
	});

	test('rejects every non-local scheme, including the opaque ones', () => {
		assert.equal(isLocalAppURL('data:text/html,x'), false);
		assert.equal(isLocalAppURL('about:blank'), false);
		assert.equal(isLocalAppURL('blob:fluxer-app://app/1234'), false);
		assert.equal(isLocalAppURL('file:///etc/passwd'), false);
		assert.equal(isLocalAppURL('https://web.fluxer.app/'), false);
		assert.equal(isLocalAppURL('fluxer://guild/channel'), false);
		assert.equal(isLocalAppURL('not a url'), false);
		assert.equal(isLocalAppURL(''), false);
		assert.equal(isLocalAppURL(null), false);
	});

	test('the recogniser does not lean on URL.origin, which is "null" for this scheme in Node', () => {
		assert.equal(new URL('fluxer-app://app').origin, 'null');
		assert.equal(new URL('data:text/html,x').origin, 'null');
		assert.equal(isLocalAppURL('data:text/html,x'), false);
	});
});

describe('reserved proxy paths', () => {
	test('claims the API and remote-resource prefixes', () => {
		for (const pathname of ['/api', '/api/', '/api/x', '/api/x/v1/users/@me', '/proxy', '/proxy/', '/proxy/x']) {
			assert.equal(isReservedLocalAppProxyPath(pathname), true, pathname);
		}
	});

	test('does not claim prefixes that merely start with the same letters', () => {
		for (const pathname of ['/apix', '/api-docs', '/proxying', '/', '/channels/@me', '/x/api']) {
			assert.equal(isReservedLocalAppProxyPath(pathname), false, pathname);
		}
	});
});

describe('renderer document URLs', () => {
	test('a reserved proxy path is never a renderer document', () => {
		assert.equal(isLocalAppRendererDocumentURL('fluxer-app://app/api/x'), false);
		assert.equal(isLocalAppRendererDocumentURL('fluxer-app://app/proxy/x?url=https://evil.example'), false);
	});

	test('ordinary app routes are renderer documents', () => {
		assert.equal(isLocalAppRendererDocumentURL('fluxer-app://app/'), true);
		assert.equal(isLocalAppRendererDocumentURL('fluxer-app://app/channels/@me'), true);
	});

	test('an untrusted origin is never a renderer document', () => {
		assert.equal(isLocalAppRendererDocumentURL('https://web.fluxer.app/channels/@me'), false);
		assert.equal(isLocalAppRendererDocumentURL('data:text/html,x'), false);
		assert.equal(isLocalAppRendererDocumentURL('about:blank'), false);
	});
});
