// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import fs from 'node:fs';
import https from 'node:https';
import path from 'node:path';
import {describe, test} from 'node:test';
import tls from 'node:tls';
import {fileURLToPath} from 'node:url';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {resolveDesktopTrustedCertificates} = await import('./DesktopTrustedCertificates.ts');
const {DesktopOriginTrust, DesktopOutboundHTTP} = await import('./DesktopOutboundHTTP.ts');

const FIXTURES = path.join(path.dirname(fileURLToPath(import.meta.url)), 'fixtures', 'desktop-tls');
const read = (name) => fs.readFileSync(path.join(FIXTURES, name), 'utf8');
const TRUSTED_CA = read('trusted-ca.pem');
const UNTRUSTED_CA = read('untrusted-ca.pem');

async function startTLSServer() {
	const server = https.createServer({cert: read('leaf.pem'), key: read('leaf.key')}, (_request, response) => {
		response.writeHead(200, {'content-type': 'text/plain'});
		response.end('ok');
	});
	await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
	return {port: server.address().port, server};
}

async function fetchThrough(certificates, port) {
	const outboundHTTP = new DesktopOutboundHTTP({
		resolveHostAddresses: async () => ['127.0.0.1'],
		trustedCertificates: certificates,
	});
	try {
		await outboundHTTP.registerAnchoredOrigins({anchorOrigin: `https://ca.example:${port}`, origins: []});
		const response = await outboundHTTP.request({
			body: null,
			expectedOrigin: `https://ca.example:${port}`,
			headers: null,
			method: 'GET',
			originTrust: DesktopOriginTrust.REGISTERED,
			serviceName: 'test',
			signal: null,
			timeoutMs: 3000,
			url: `https://ca.example:${port}/`,
		});
		response.message.resume();
		return response.status;
	} finally {
		outboundHTTP.cleanup();
	}
}

describe('desktop trusted certificates', () => {
	test('merges the bundled roots with the system store without duplicates', () => {
		const merged = resolveDesktopTrustedCertificates((store) => (store === 'default' ? ['a', 'b'] : ['b', 'c']));
		assert.deepEqual(merged, ['a', 'b', 'c']);
	});

	test('keeps the bundled roots when the system store cannot be read', () => {
		const merged = resolveDesktopTrustedCertificates((store) => {
			if (store === 'system') throw new Error('no system store');
			return ['a'];
		});
		assert.deepEqual(merged, ['a']);
	});

	test('reaches an instance whose CA is only trusted by the system store', async () => {
		const {port, server} = await startTLSServer();
		try {
			const certificates = resolveDesktopTrustedCertificates((store) =>
				store === 'system' ? [TRUSTED_CA] : tls.getCACertificates('default'),
			);
			assert.equal(await fetchThrough(certificates, port), 200);
		} finally {
			server.close();
		}
	});

	test('still refuses an instance signed by a CA nobody trusts', async () => {
		const {port, server} = await startTLSServer();
		try {
			const certificates = resolveDesktopTrustedCertificates((store) =>
				store === 'system' ? [UNTRUSTED_CA] : tls.getCACertificates('default'),
			);
			await assert.rejects(fetchThrough(certificates, port));
			await assert.rejects(fetchThrough(tls.getCACertificates('default'), port));
		} finally {
			server.close();
		}
	});
});
