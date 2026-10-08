// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {X509Certificate} from 'node:crypto';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {buildDesktopTrustAnchors, verifyAgainstDesktopSystemTrust} = await import('./DesktopSystemTrustVerifier.ts');
const {resolveDesktopLocallyAddedCertificates} = await import('./DesktopTrustedCertificates.ts');

const FIXTURES = path.join(path.dirname(fileURLToPath(import.meta.url)), 'fixtures', 'desktop-trust');
const read = (name) => fs.readFileSync(path.join(FIXTURES, `${name}.pem`), 'utf8');
const AUTHORITY_INVALID = -202;
const DEFER = -3;
const ACCEPT = 0;

function chain(names) {
	let certificate;
	for (const name of [...names].reverse()) certificate = {data: read(name), issuerCert: certificate};
	return certificate;
}

function verify(names, {hostname = 'host.example', errorCode = AUTHORITY_INVALID, anchors = ['root']} = {}) {
	const store = buildDesktopTrustAnchors(anchors.map(read));
	return verifyAgainstDesktopSystemTrust({hostname, certificate: chain(names), errorCode}, () => store);
}

describe('desktop system trust verifier', () => {
	test('accepts a chain that ends at a locally added CA', () => {
		assert.equal(verify(['leaf', 'int']), ACCEPT);
		assert.equal(verify(['leaf', 'int'], {hostname: 'HOST.example.'}), ACCEPT);
		assert.equal(verify(['leaf', 'int'], {hostname: '127.0.0.1'}), ACCEPT);
		assert.equal(verify(['leaf', 'int'], {hostname: '[::1]'}), ACCEPT);
		assert.equal(verify(['ec_leaf']), ACCEPT);
		assert.equal(verify(['wc_leaf'], {hostname: 'a.lan.home.example'}), ACCEPT);
	});

	test('defers to Chromium for other errors, anchors and hosts', () => {
		assert.equal(verify(['leaf', 'int'], {errorCode: -201}), DEFER);
		assert.equal(verify(['leaf', 'int'], {anchors: ['pl_int']}), DEFER);
		assert.equal(verify(['leaf', 'int'], {hostname: 'other.example'}), DEFER);
		assert.equal(verify(['wc_leaf'], {hostname: 'a.b.lan.home.example'}), DEFER);
		assert.equal(verify(['wc_leaf'], {hostname: 'a.home.example'}), DEFER);
		assert.equal(verify(['wc_leaf'], {hostname: 'victim.co.uk'}), DEFER);
	});

	test('defers on constrained, weak or unknown certificates in the path', () => {
		assert.equal(verify(['nc_leaf', 'nc_int']), DEFER);
		assert.equal(verify(['nc_leaf'], {anchors: ['nc_int']}), DEFER);
		assert.equal(verify(['eku_leaf', 'eku_int']), DEFER);
		assert.equal(verify(['pl_leaf', 'pl_int2', 'pl_int']), DEFER);
		assert.equal(verify(['crit_leaf']), DEFER);
		assert.equal(verify(['sha1_leaf']), DEFER);
		assert.equal(verify(['weak_leaf']), DEFER);
	});

	test('rejects non-DER booleans and Chromium blocklisted keys', () => {
		assert.equal(verify(['bool_ff_leaf']), ACCEPT);
		assert.equal(verify(['bool_01_leaf']), DEFER);
		assert.equal(buildDesktopTrustAnchors([read('kz_root')]).size, 0);
	});

	test('anchors only on admin added CAs that the distro bundle still trusts', () => {
		const root = fs.mkdtempSync(path.join(os.tmpdir(), 'desktop-trust-'));
		const write = (file, content) => {
			fs.mkdirSync(path.dirname(path.join(root, file)), {recursive: true});
			fs.writeFileSync(path.join(root, file), content);
		};
		const der = (name) => new X509Certificate(read(name)).raw;
		try {
			assert.deepEqual(resolveDesktopLocallyAddedCertificates(root), []);
			write('etc/ssl/certs/ca-certificates.crt', [read('root'), read('int'), read('kz_root')].join('\n'));
			write('usr/local/share/ca-certificates/site/root.crt', read('root'));
			write('usr/local/share/ca-certificates/int.der', der('int'));
			write('usr/local/share/ca-certificates/deselected.crt', read('pl_int'));
			write('cert.pem', read('pl_int'));
			process.env.SSL_CERT_FILE = path.join(root, 'cert.pem');
			const added = resolveDesktopLocallyAddedCertificates(root).map((pem) => new X509Certificate(pem).subject);
			assert.deepEqual(added.sort(), ['CN=Trust Test Int', 'CN=Trust Test Root']);
		} finally {
			delete process.env.SSL_CERT_FILE;
			fs.rmSync(root, {recursive: true, force: true});
		}
	});
});
