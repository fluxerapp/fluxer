// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {localAppCacheControl} = await import('./LocalAppMime.ts');
const {shouldRewriteLocalAppStaticMetadata} = await import('./LocalAppStaticMetadata.ts');

const IMMUTABLE = 'public, max-age=31536000, immutable';
const REVALIDATED = 'public, max-age=3600, must-revalidate';

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
