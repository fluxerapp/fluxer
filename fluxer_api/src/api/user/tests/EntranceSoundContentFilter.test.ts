// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {phraseBlocklistCache} from '@app/api/middleware/PhraseBlocklistCache';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const BANNED = 'zebracrossingbanned';
const SAMPLE_RATE = 8000;

function wavWithBase64Text(text: string): string {
	const header = Buffer.alloc(44);
	const samples = Buffer.alloc(SAMPLE_RATE / 2, 0x80);
	const textBytes = Buffer.from(`${text}A`, 'base64');
	textBytes.copy(samples, 1);
	header.write('RIFF', 0, 'latin1');
	header.writeUInt32LE(36 + samples.length, 4);
	header.write('WAVEfmt ', 8, 'latin1');
	header.writeUInt32LE(16, 16);
	header.writeUInt16LE(1, 20);
	header.writeUInt16LE(1, 22);
	header.writeUInt32LE(SAMPLE_RATE, 24);
	header.writeUInt32LE(SAMPLE_RATE, 28);
	header.writeUInt16LE(1, 32);
	header.writeUInt16LE(8, 34);
	header.write('data', 36, 'latin1');
	header.writeUInt32LE(samples.length, 40);
	return Buffer.concat([header, samples]).toString('base64');
}

describe('Entrance sound uploads and the content filter', () => {
	let harness: ApiTestHarness;
	let account: TestAccount;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		account = await createTestAccount(harness);
		await ensureSessionStarted(harness, account.token);
		phraseBlocklistCache.add(BANNED);
	});
	afterEach(() => {
		phraseBlocklistCache.remove(BANNED);
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	it('accepts audio whose base64 payload contains a banned phrase', async () => {
		const audio = wavWithBase64Text(BANNED);
		expect(audio).toContain(BANNED);
		const sound = await createBuilder<{name: string; content_type: string}>(harness, account.token)
			.post('/users/@me/entrance-sounds')
			.body({name: 'doorbell', audio})
			.expect(HTTP_STATUS.CREATED)
			.execute();
		expect(sound).toMatchObject({name: 'doorbell', content_type: 'audio/wav'});
	});

	it('still blocks the banned phrase in the sound name', async () => {
		await createBuilder(harness, account.token)
			.post('/users/@me/entrance-sounds')
			.body({name: `my ${BANNED}`, audio: wavWithBase64Text('plainaudio')})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.CONTENT_BLOCKED)
			.execute();
	});
});
