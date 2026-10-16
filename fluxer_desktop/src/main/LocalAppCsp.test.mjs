// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import path from 'node:path';
import {describe, test} from 'node:test';
import {installElectronStub, REPOSITORY_ROOT} from './LocalAppTestSupport.test.mjs';

installElectronStub();
const {buildLocalAppCSP} = await import('@electron/main/LocalAppCSP');
const {randomLocalAppCSPNonce, rewriteLocalAppIndexHTML} = await import('@electron/main/LocalAppIndexHTML');

const DESKTOP_GOLDEN = [
	"default-src 'self'",
	"script-src 'self' 'nonce-NONCE' 'wasm-unsafe-eval' blob: https://*.fluxer.app",
	"style-src 'self' 'unsafe-inline' https://*.fluxer.app https://fonts.googleapis.com https://api.fonts.coollabs.io",
	"img-src 'self' blob: data: https://*.fluxer.app https://i.ytimg.com https://*.youtube.com https://*.fluxer.media https://fluxer.media https: http://localhost:* http://*.localhost:* http://127.0.0.1:*",
	"media-src 'self' blob: https://*.fluxer.app https://*.youtube.com https://*.fluxer.media https://fluxer.media https: data: http://localhost:* http://*.localhost:* http://127.0.0.1:*",
	"font-src 'self' data: https://*.fluxer.app https://fonts.gstatic.com https://api.fonts.coollabs.io",
	"connect-src 'self' blob: data: https://*.fluxer.app wss://*.fluxer.app https://*.fluxer.media wss://*.fluxer.media https://fluxer-uploads.ewr1.vultrobjects.com https://fluxerstatus.com https://fluxer.media https: wss: http://localhost:* http://*.localhost:* http://127.0.0.1:*",
	"frame-src 'self' https://www.youtube.com/embed/ https://www.youtube.com/s/player/",
	"worker-src 'self' blob: https://*.fluxer.app",
	"manifest-src 'self' https://*.fluxer.app",
	"object-src 'none'",
	"base-uri 'self'",
	"frame-ancestors 'none'",
].join('; ');

function desktopPolicy() {
	return buildLocalAppCSP({nonce: 'NONCE'});
}

function directive(policy, name) {
	const found = policy.split('; ').find((entry) => entry.startsWith(`${name} `));
	assert.ok(found != null, `missing directive ${name}`);
	return found.slice(name.length + 1).split(' ');
}

describe('the local desktop CSP', () => {
	test('the policy matches the desktop builder byte for byte', () => {
		assert.equal(desktopPolicy(), DESKTOP_GOLDEN);
	});
});

describe('the desktop ceiling widens without displacing self', () => {
	test("'self' survives the ceiling in img-src, media-src and connect-src", () => {
		for (const name of ['img-src', 'media-src', 'connect-src']) {
			assert.equal(directive(desktopPolicy(), name)[0], "'self'", `${name} lost 'self'`);
		}
	});

	test('connect-src carries https:, wss: and blob:', () => {
		const connect = directive(desktopPolicy(), 'connect-src');
		for (const source of ['https:', 'wss:', 'blob:', 'data:']) {
			assert.ok(connect.includes(source), `connect-src missing ${source}`);
		}
	});

	test('img-src and media-src carry data: and blob: for theme files and audio worklets', () => {
		for (const name of ['img-src', 'media-src']) {
			const sources = directive(desktopPolicy(), name);
			assert.ok(sources.includes('data:'), `${name} missing data:`);
			assert.ok(sources.includes('blob:'), `${name} missing blob:`);
		}
	});

	test('worker-src carries blob: for the LiveKit E2EE worker', () => {
		assert.ok(directive(desktopPolicy(), 'worker-src').includes('blob:'));
	});

	test('no directive allows a third-party captcha host', () => {
		const policy = desktopPolicy();
		assert.ok(!policy.includes('hcaptcha'), policy);
		assert.ok(!policy.includes('challenges.cloudflare.com'), policy);
	});
});

describe('cleartext self-hosted instances', () => {
	test('loopback http instances load images and media on a cold start', () => {
		for (const name of ['img-src', 'media-src', 'connect-src']) {
			const sources = directive(desktopPolicy(), name);
			assert.ok(sources.includes('http://localhost:*'), `${name} missing loopback`);
			assert.ok(sources.includes('http://127.0.0.1:*'), `${name} missing loopback`);
		}
	});

	test('registered private http instance origins are granted, and nothing else is', () => {
		const policy = buildLocalAppCSP({
			nonce: 'NONCE',
			cleartextInstanceOrigins: [
				'http://192.168.1.20:48090',
				'https://web.example',
				'http://[fd00::1]:8080',
				'http://nas.lan/path',
				"http://evil'; script-src *",
			],
		});
		for (const name of ['img-src', 'media-src', 'connect-src']) {
			const sources = directive(policy, name);
			assert.ok(sources.includes('http://192.168.1.20:48090'), `${name} missing the instance`);
			assert.ok(!sources.includes('https://web.example'));
			assert.ok(!sources.some((source) => source.includes('fd00') || source.includes('nas.lan')));
		}
		assert.ok(!policy.includes('evil'), policy);
		assert.ok(!directive(policy, 'script-src').some((source) => source.startsWith('http:')));
	});
});

describe('the shipped index.html runs under the local CSP', () => {
	const sourceIndex = readFileSync(path.join(REPOSITORY_ROOT, 'fluxer_app', 'index.html'), 'utf8');

	test('every inline script carries the nonce the policy grants', () => {
		const nonce = randomLocalAppCSPNonce();
		const html = rewriteLocalAppIndexHTML({html: sourceIndex, nonce, prebootTheme: null});
		const policy = buildLocalAppCSP({nonce});
		const grantedNonce = directive(policy, 'script-src')[1];
		const inlineScriptTags = [...html.matchAll(/<script\b([^>]*)>/giu)]
			.map((match) => match[1])
			.filter((attributes) => !/\ssrc\s*=/iu.test(attributes));
		assert.ok(inlineScriptTags.length > 0);
		for (const attributes of inlineScriptTags) {
			assert.equal(grantedNonce, `'nonce-${nonce}'`);
			assert.ok(
				attributes.includes(`nonce="${nonce}"`),
				`inline script without the granted nonce: <script${attributes}>`,
			);
		}
	});
});
