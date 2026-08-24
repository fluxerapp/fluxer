// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, test} from 'vitest';
import {signHarvestDownloadToken, verifyHarvestDownloadToken} from '../HarvestDownloadToken';

const SECRET = 'test-connection-secret';

function payload(overrides: Partial<Parameters<typeof signHarvestDownloadToken>[0]> = {}) {
	return {
		userId: '123',
		harvestId: '456',
		storageKey: 'exports/123/456/user-data.zip',
		expiresAt: Date.now() + 60_000,
		...overrides,
	};
}

describe('HarvestDownloadToken', () => {
	test('round trips a valid token', () => {
		const original = payload();
		const verified = verifyHarvestDownloadToken(signHarvestDownloadToken(original, SECRET), SECRET);
		expect(verified).toEqual(original);
	});

	test('rejects a token signed with a different secret', () => {
		const token = signHarvestDownloadToken(payload(), SECRET);
		expect(verifyHarvestDownloadToken(token, 'other-secret')).toBeNull();
	});

	test('rejects an expired token', () => {
		const token = signHarvestDownloadToken(payload({expiresAt: Date.now() - 1}), SECRET);
		expect(verifyHarvestDownloadToken(token, SECRET)).toBeNull();
	});

	test('rejects a tampered payload', () => {
		const token = signHarvestDownloadToken(payload(), SECRET);
		const [encoded, signature] = token.split('.');
		const decoded = JSON.parse(Buffer.from(encoded as string, 'base64url').toString('utf-8'));
		decoded.userId = '999';
		const forged = `${Buffer.from(JSON.stringify(decoded)).toString('base64url')}.${signature}`;
		expect(verifyHarvestDownloadToken(forged, SECRET)).toBeNull();
	});

	test('rejects a tampered signature', () => {
		const token = signHarvestDownloadToken(payload(), SECRET);
		expect(verifyHarvestDownloadToken(`${token.slice(0, -2)}xy`, SECRET)).toBeNull();
	});

	test('rejects malformed input', () => {
		for (const bad of ['', '.', 'nodot', 'a.b', '..', 'a.']) {
			expect(verifyHarvestDownloadToken(bad, SECRET)).toBeNull();
		}
	});

	test('rejects a payload missing required fields', () => {
		const encoded = Buffer.from(JSON.stringify({userId: '1'})).toString('base64url');
		const token = signHarvestDownloadToken(payload(), SECRET);
		const signature = token.split('.')[1];
		expect(verifyHarvestDownloadToken(`${encoded}.${signature}`, SECRET)).toBeNull();
	});

	test('is not verifiable as a raw HMAC of the base secret', () => {
		const {createHmac} = require('node:crypto') as typeof import('node:crypto');
		const encoded = Buffer.from(JSON.stringify(payload())).toString('base64url');
		const rawSignature = createHmac('sha256', SECRET).update(encoded).digest('base64url');
		expect(verifyHarvestDownloadToken(`${encoded}.${rawSignature}`, SECRET)).toBeNull();
	});
});
