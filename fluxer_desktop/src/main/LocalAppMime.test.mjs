// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {isServedLocalAppExtension, localAppCacheControl, localAppContentType} = await import('./LocalAppMime.ts');
const {shouldRewriteLocalAppStaticMetadata} = await import('./LocalAppStaticMetadata.ts');

const IMMUTABLE = 'public, max-age=31536000, immutable';
const REVALIDATED = 'public, max-age=3600, must-revalidate';

describe('local app content types', () => {
	test('serves the types the renderer bundle actually needs', () => {
		assert.equal(localAppContentType('/assets/main.js'), 'application/javascript; charset=utf-8');
		assert.equal(localAppContentType('/assets/main.mjs'), 'application/javascript; charset=utf-8');
		assert.equal(localAppContentType('/assets/main.css'), 'text/css; charset=utf-8');
		assert.equal(localAppContentType('/index.html'), 'text/html; charset=utf-8');
		assert.equal(localAppContentType('/manifest.json'), 'application/json; charset=utf-8');
		assert.equal(localAppContentType('/web/favicon-32x32.png'), 'image/png');
		assert.equal(localAppContentType('/web/apple-touch-icon.jpg'), 'image/jpeg');
		assert.equal(localAppContentType('/assets/font.woff2'), 'font/woff2');
	});

	test('wasm is application/wasm so compileStreaming keeps working', () => {
		assert.equal(localAppContentType('/assets/tree-sitter-rust.wasm'), 'application/wasm');
	});

	test('an unknown extension falls back to a non-executable type', () => {
		assert.equal(localAppContentType('/assets/NOTICE.md'), 'application/octet-stream');
		assert.equal(localAppContentType('/assets/no-extension'), 'application/octet-stream');
	});

	test('extension lookup is case insensitive', () => {
		assert.equal(localAppContentType('/assets/MAIN.JS'), 'application/javascript; charset=utf-8');
	});
});

describe('SPA fallback eligibility', () => {
	test('a served extension means "this is a real asset, 404 if missing"', () => {
		assert.equal(isServedLocalAppExtension('assets/deadbeefdeadbeef.js'), true);
		assert.equal(isServedLocalAppExtension('web/favicon-32x32.png'), true);
	});

	test('a dotted route segment is not an asset and must reach index.html', () => {
		assert.equal(isServedLocalAppExtension('invite/abc.def'), false);
		assert.equal(isServedLocalAppExtension('users/1.2.3'), false);
		assert.equal(isServedLocalAppExtension('theme/my.custom.theme'), false);
		assert.equal(isServedLocalAppExtension('channels/@me'), false);
	});
});

describe('local app cache control', () => {
	test('content-hashed assets are immutable', () => {
		assert.equal(localAppCacheControl('/assets/deadbeefdeadbeef.js'), IMMUTABLE);
		assert.equal(localAppCacheControl('/assets/c4fd91dc82f7db6f.css'), IMMUTABLE);
		assert.equal(localAppCacheControl('/assets/main-c4fd91dc82f7db6f.js'), IMMUTABLE);
	});

	test('the rewritten documents are never cached', () => {
		assert.equal(localAppCacheControl('/index.html'), 'no-store');
		assert.equal(localAppCacheControl('/manifest.json'), 'no-store');
		assert.equal(localAppCacheControl('/browserconfig.xml'), 'no-store');
	});

	test('icons under /web are revalidated, never immutable (stale icon after upgrade)', () => {
		assert.equal(localAppCacheControl('/web/favicon-32x32.png'), REVALIDATED);
		assert.equal(localAppCacheControl('/web/mstile-150x150.png'), REVALIDATED);
		assert.equal(localAppCacheControl('/robots.txt'), REVALIDATED);
	});

	test('an unhashed file inside /assets is not immutable', () => {
		assert.equal(localAppCacheControl('/assets/fonts-NOTICE.txt'), REVALIDATED);
	});
});

describe('the rewritten-file lists in LocalAppMime and LocalAppStaticMetadata agree', () => {
	test('every rewritten static metadata file is served no-store', () => {
		for (const fileName of ['manifest.json', 'browserconfig.xml']) {
			assert.equal(shouldRewriteLocalAppStaticMetadata(fileName), true, fileName);
			assert.equal(localAppCacheControl(`/${fileName}`), 'no-store', fileName);
			assert.equal(localAppCacheControl(`/nested/${fileName}`), 'no-store', fileName);
		}
	});

	test('index.html is rewritten by the index path, not by the static metadata rewriter', () => {
		assert.equal(shouldRewriteLocalAppStaticMetadata('index.html'), false);
		assert.equal(localAppCacheControl('/index.html'), 'no-store');
	});
});
