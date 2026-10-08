// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {existsSync, readFileSync} from 'node:fs';
import path from 'node:path';
import {describe, test} from 'node:test';
import {installElectronStub, REPOSITORY_ROOT} from './LocalAppTestSupport.test.mjs';

installElectronStub();
const {rewriteLocalAppStaticMetadata, shouldRewriteLocalAppStaticMetadata} = await import(
	'@electron/main/LocalAppStaticMetadata'
);

const BUILT_MANIFEST_PATH = path.join(REPOSITORY_ROOT, 'fluxer_app', 'dist', 'manifest.json');
const BUILT_BROWSERCONFIG_PATH = path.join(REPOSITORY_ROOT, 'fluxer_app', 'dist', 'browserconfig.xml');

const MANIFEST_FIXTURE = JSON.stringify({
	name: 'Fluxer',
	start_url: '/',
	icons: [
		{src: '{{STATIC_CDN_ENDPOINT}}/web/android-chrome-192x192.png', sizes: '192x192'},
		{src: 'https://fluxerstatic.com/web/apple-touch-icon.png', sizes: '180x180'},
	],
});

const BROWSERCONFIG_FIXTURE =
	'<?xml version="1.0" encoding="utf-8"?><browserconfig><msapplication><tile>' +
	'<square150x150logo src="{{STATIC_CDN_ENDPOINT}}/web/mstile-150x150.png"/>' +
	'</tile></msapplication></browserconfig>';

describe('which files the static metadata rewriter owns', () => {
	test('it owns manifest.json and browserconfig.xml', () => {
		assert.equal(shouldRewriteLocalAppStaticMetadata('manifest.json'), true);
		assert.equal(shouldRewriteLocalAppStaticMetadata('browserconfig.xml'), true);
	});

	test('it does not own index.html, which the index path rewrites instead', () => {
		assert.equal(shouldRewriteLocalAppStaticMetadata('index.html'), false);
		assert.equal(shouldRewriteLocalAppStaticMetadata('assets/main.js'), false);
	});

	test('rewriting an unowned file throws rather than silently passing it through', () => {
		assert.throws(
			() => rewriteLocalAppStaticMetadata('index.html', '<html lang="en"></html>'),
			(error) => error.name === 'UnsupportedLocalAppStaticMetadataFileError',
		);
	});
});

describe('rewriting manifest.json for the local scheme', () => {
	test('the placeholder becomes a root-relative path and the result is still JSON', () => {
		const rewritten = rewriteLocalAppStaticMetadata('manifest.json', MANIFEST_FIXTURE);
		assert.ok(!rewritten.includes('{{STATIC_CDN_ENDPOINT}}'));
		const parsed = JSON.parse(rewritten);
		assert.equal(parsed.icons[0].src, '/web/android-chrome-192x192.png');
	});

	test('an absolute CDN URL baked in at build time is rewritten too', () => {
		const rewritten = rewriteLocalAppStaticMetadata('manifest.json', MANIFEST_FIXTURE);
		assert.equal(JSON.parse(rewritten).icons[1].src, '/web/apple-touch-icon.png');
		assert.ok(!rewritten.includes('https://fluxerstatic.com'));
	});

	test('a URL outside /web is left alone', () => {
		const source = '{"icons":[{"src":"https://cdn.example/assets/x.png"}]}';
		assert.equal(rewriteLocalAppStaticMetadata('manifest.json', source), source);
	});

	test('query and hash on a rewritten asset survive', () => {
		const source = '{"icons":[{"src":"https://cdn.example/web/x.png?v=2#a"}]}';
		assert.equal(rewriteLocalAppStaticMetadata('manifest.json', source), '{"icons":[{"src":"/web/x.png?v=2#a"}]}');
	});
});

describe('rewriting browserconfig.xml for the local scheme', () => {
	test('the tile logo becomes root-relative', () => {
		const rewritten = rewriteLocalAppStaticMetadata('browserconfig.xml', BROWSERCONFIG_FIXTURE);
		assert.ok(rewritten.includes('src="/web/mstile-150x150.png"'));
		assert.ok(!rewritten.includes('{{STATIC_CDN_ENDPOINT}}'));
	});
});

describe('against the real built artifacts when they are present', () => {
	const skip = existsSync(BUILT_MANIFEST_PATH) ? false : 'fluxer_app/dist is not built in this working tree';

	test('dist/manifest.json rewrites to a parseable manifest with only local icons', {skip}, () => {
		const rewritten = rewriteLocalAppStaticMetadata('manifest.json', readFileSync(BUILT_MANIFEST_PATH, 'utf8'));
		const parsed = JSON.parse(rewritten);
		assert.ok(Array.isArray(parsed.icons) && parsed.icons.length > 0);
		for (const icon of parsed.icons) {
			assert.ok(icon.src.startsWith('/web/'), icon.src);
		}
	});

	test('dist/browserconfig.xml rewrites to a local tile', {skip}, () => {
		if (!existsSync(BUILT_BROWSERCONFIG_PATH)) {
			return;
		}
		const rewritten = rewriteLocalAppStaticMetadata(
			'browserconfig.xml',
			readFileSync(BUILT_BROWSERCONFIG_PATH, 'utf8'),
		);
		assert.ok(rewritten.includes('src="/web/'));
		assert.ok(!rewritten.includes('{{STATIC_CDN_ENDPOINT}}'));
	});
});
