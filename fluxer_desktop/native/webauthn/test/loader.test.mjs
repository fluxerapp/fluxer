// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import test from 'node:test';
import webauthnPure from '../pure.cjs';

test('loader resolves supported native filenames', () => {
	assert.equal(webauthnPure.nativeFileName('darwin', 'x64'), 'webauthn.darwin-x64.node');
	assert.equal(webauthnPure.nativeFileName('darwin', 'arm64'), 'webauthn.darwin-arm64.node');
	assert.equal(webauthnPure.nativeFileName('linux', 'x64'), 'webauthn.linux-x64-gnu.node');
	assert.equal(webauthnPure.nativeFileName('linux', 'arm64'), 'webauthn.linux-arm64-gnu.node');
	assert.equal(webauthnPure.nativeFileName('win32', 'x64'), 'webauthn.win32-x64-msvc.node');
	assert.equal(webauthnPure.nativeFileName('win32', 'arm64'), 'webauthn.win32-arm64-msvc.node');
	assert.equal(webauthnPure.nativeFileName('freebsd', 'x64'), null);
});

test('normalization creates spec-shaped client data and Windows transport bits', () => {
	const challenge = Buffer.from([1, 2, 3, 4]);
	const normalized = webauthnPure.normalizeCreateOptions({
		origin: 'https://web.canary.fluxer.app/channels/@me',
		challenge,
		rp: {id: 'fluxer.app', name: 'Fluxer'},
		user: {id: Buffer.from('user'), name: 'name', displayName: 'Name'},
		pubKeyCredParams: [{type: 'public-key', alg: -7}],
		authenticatorSelection: {
			authenticatorAttachment: 'platform',
			residentKey: 'preferred',
			userVerification: 'required',
		},
		excludeCredentials: [{type: 'public-key', id: Buffer.from('cred'), transports: ['internal', 'hybrid']}],
		attestation: 'none',
	});
	assert.equal(normalized.rpId, 'fluxer.app');
	assert.equal(normalized.authenticatorAttachment, 1);
	assert.equal(normalized.userVerification, 1);
	assert.equal(normalized.preferResidentKey, true);
	assert.equal(normalized.requireResidentKey, false);
	assert.equal(normalized.excludeCredentials[0].transports, 0x10 | 0x20);
	assert.deepEqual(JSON.parse(normalized.clientDataJSON.toString('utf8')), {
		type: 'webauthn.create',
		challenge: 'AQIDBA',
		origin: 'https://web.canary.fluxer.app',
		crossOrigin: false,
	});
});
