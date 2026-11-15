// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {EntranceSoundRepository} from '@app/api/user/entrance_sound/EntranceSoundRepository';
import {
	deleteAccount,
	setPendingDeletionAt,
	triggerDeletionWorker,
	waitForDeletionCompletion,
} from '@app/api/user/tests/UserTestUtils';
import {afterAll, beforeAll, describe, expect, test} from 'vitest';

const SAMPLE_RATE = 8000;

function silentWav(): string {
	const header = Buffer.alloc(44);
	const samples = Buffer.alloc(SAMPLE_RATE / 2, 0x80);
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

describe('Account deletion and entrance sounds', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	test('permanent deletion removes entrance sounds, selections and stored audio', async () => {
		const account = await createTestAccount(harness);
		await ensureSessionStarted(harness, account.token);
		const sound = await createBuilder<{id: string; hash: string; extension: string}>(harness, account.token)
			.post('/users/@me/entrance-sounds')
			.body({name: 'doorbell', audio: silentWav()})
			.expect(HTTP_STATUS.CREATED)
			.execute();
		for (const scopeId of ['global', 'dms']) {
			await createBuilder(harness, account.token)
				.put('/users/@me/entrance-sound-selections')
				.body({scope_id: scopeId, sound_id: sound.id})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		}
		const userId = createUserID(BigInt(account.userId));
		const repository = new EntranceSoundRepository();
		const key = `entrance-sounds/${account.userId}/${sound.hash}.${sound.extension}`;
		expect(await repository.listSounds(userId)).toHaveLength(1);
		expect(await repository.listSelections(userId)).toHaveLength(2);
		expect(harness.storageService.hasObject(Config.s3.buckets.cdn, key)).toBe(true);

		await deleteAccount(harness, account.token, account.password);
		const past = new Date();
		past.setMinutes(past.getMinutes() - 1);
		await setPendingDeletionAt(harness, account.userId, past);
		await triggerDeletionWorker(harness);
		await waitForDeletionCompletion(harness, account.userId);

		expect(await repository.listSounds(userId)).toHaveLength(0);
		expect(await repository.listSelections(userId)).toHaveLength(0);
		expect(harness.storageService.hasObject(Config.s3.buckets.cdn, key)).toBe(false);
	});
});
