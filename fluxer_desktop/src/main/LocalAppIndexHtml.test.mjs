// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {existsSync, readFileSync} from 'node:fs';
import path from 'node:path';
import {describe, test} from 'node:test';
import {installElectronStub, REPOSITORY_ROOT} from './LocalAppTestSupport.test.mjs';

installElectronStub();
const {randomLocalAppCSPNonce, rewriteLocalAppIndexHTML} = await import('@electron/main/LocalAppIndexHTML');

const SOURCE_INDEX_PATH = path.join(REPOSITORY_ROOT, 'fluxer_app', 'index.html');
const BUILT_INDEX_PATH = path.join(REPOSITORY_ROOT, 'fluxer_app', 'dist', 'index.html');
const SOURCE_INDEX = readFileSync(SOURCE_INDEX_PATH, 'utf8');
const MINIFIED_INDEX = SOURCE_INDEX.replaceAll(/<!--[\s\S]*?-->/gu, '');

const BASE_INPUT = {
	nonce: 'aabbccddeeff00112233445566778899',
	prebootTheme: null,
};

function inject(overrides = {}) {
	return rewriteLocalAppIndexHTML({html: SOURCE_INDEX, ...BASE_INPUT, ...overrides});
}

describe('local app rewriting of the shipped index.html', () => {
	test('every CSP nonce placeholder is substituted', () => {
		const html = inject();
		const placeholderCount = SOURCE_INDEX.split('nonce="{{CSP_NONCE_PLACEHOLDER}}"').length - 1;
		assert.ok(!html.includes('{{CSP_NONCE_PLACEHOLDER}}'));
		assert.ok(placeholderCount > 0);
		assert.equal(html.split(`nonce="${BASE_INPUT.nonce}"`).length - 1, placeholderCount);
	});

	test('the static CDN placeholder becomes a root-relative path', () => {
		const html = inject();
		assert.ok(!html.includes('{{STATIC_CDN_ENDPOINT}}'));
		assert.ok(html.includes('href="/web/favicon-32x32.png"'));
		assert.ok(html.includes('href="/web/apple-touch-icon.png"'));
	});

	test('does not inject scripts', () => {
		const html = inject();
		assert.equal(html.split('<script').length, SOURCE_INDEX.split('<script').length);
	});
});

describe('local app rewriting of production-shaped documents', () => {
	test('a minified document remains injection-free', () => {
		const html = rewriteLocalAppIndexHTML({html: MINIFIED_INDEX, ...BASE_INPUT});
		assert.ok(!html.includes('{{CSP_NONCE_PLACEHOLDER}}'));
		assert.ok(!html.includes('{{STATIC_CDN_ENDPOINT}}'));
		assert.equal(html.split('<script').length, MINIFIED_INDEX.split('<script').length);
		assert.ok(html.includes('<head>'), 'the opening head tag must survive');
	});

	test('a document with no head does not receive an injected script', () => {
		const html = rewriteLocalAppIndexHTML({html: '<html lang="en"><body></body></html>', ...BASE_INPUT});
		assert.equal(html, '<html lang="en"><body></body></html>');
	});

	test('the real built dist/index.html remains injection-free', {
		skip: existsSync(BUILT_INDEX_PATH) ? false : 'fluxer_app/dist is not built in this working tree',
	}, () => {
		const built = readFileSync(BUILT_INDEX_PATH, 'utf8');
		const html = rewriteLocalAppIndexHTML({html: built, ...BASE_INPUT});
		assert.ok(!html.includes('{{CSP_NONCE_PLACEHOLDER}}'));
		assert.ok(!html.includes('{{STATIC_CDN_ENDPOINT}}'));
		assert.equal(html.split('<script').length, built.split('<script').length);
	});
});

describe('the preboot theme stamp', () => {
	test('a safe theme token is stamped onto the html element', () => {
		assert.ok(inject({prebootTheme: 'dark'}).includes('<html lang="en" class="theme-dark">'));
	});

	test('no theme means no class attribute is added', () => {
		assert.ok(!inject({prebootTheme: null}).includes('class="theme-'));
	});

	test('a hostile theme token is refused rather than escaped', () => {
		const html = inject({prebootTheme: 'dark"><script>alert(1)</script>'});
		assert.ok(!html.includes('alert(1)'));
		assert.ok(!html.includes('class="theme-'));
	});

	test('an existing class attribute is never overwritten', () => {
		const html = rewriteLocalAppIndexHTML({
			html: '<html lang="en" class="preset"><head></head></html>',
			...BASE_INPUT,
			prebootTheme: 'light',
		});
		assert.ok(html.includes('class="preset"'));
		assert.ok(!html.includes('theme-light'));
	});
});

describe('the CSP nonce generator', () => {
	test('is 16 random bytes hex, matching generate_nonce in csp.rs', () => {
		const nonce = randomLocalAppCSPNonce();
		assert.match(nonce, /^[0-9a-f]{32}$/u);
		assert.notEqual(nonce, randomLocalAppCSPNonce());
	});
});
