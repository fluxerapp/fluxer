// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {applyUploadRelayDecision} from '@app/api/channel/services/UploadRelay';
import {afterEach, describe, expect, it} from 'vitest';

const relay = {
	endpoint: 'https://relay.example',
	relaySecretBase64: Buffer.alloc(32, 7).toString('base64'),
	tokenTtlSecs: 60,
	maxBodyBytes: 1024,
	keepDirectCountries: [],
};

function tokenPayload(url: string): Record<string, unknown> {
	const token = new URL(url).searchParams.get('t') ?? '';
	const [payload] = token.split('.');
	return JSON.parse(Buffer.from(payload ?? '', 'base64url').toString('utf-8')) as Record<string, unknown>;
}

const original = {dual: Config.s3.dualStoreEnabled, storeId: Config.s3.storeId};

afterEach(() => {
	Config.s3.dualStoreEnabled = original.dual;
	Config.s3.storeId = original.storeId;
});

describe('upload relay store id', () => {
	it('leaves tokens unchanged while the dual store is off', async () => {
		Config.s3.dualStoreEnabled = false;
		const url = await applyUploadRelayDecision({
			presign: async () => 'unused',
			bucket: 'uploads',
			key: 'a/b',
			relayDecision: relay,
		});
		expect(tokenPayload(url)).not.toHaveProperty('s');
	});

	it('puts the absolute store id in the token while the dual store is on', async () => {
		Config.s3.dualStoreEnabled = true;
		Config.s3.storeId = 'store-b';
		const url = await applyUploadRelayDecision({
			presign: async () => 'unused',
			bucket: 'uploads-new',
			key: 'a/b',
			relayDecision: relay,
		});
		expect(tokenPayload(url)).toMatchObject({s: 'store-b', b: 'uploads-new'});
	});
});
