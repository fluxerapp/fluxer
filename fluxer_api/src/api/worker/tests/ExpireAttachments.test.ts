// SPDX-License-Identifier: AGPL-3.0-or-later

import {AttachmentDecayRepository} from '@app/api/attachment/AttachmentDecayRepository';
import {createAttachmentID, createChannelID, createMessageID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {AssetDeletionQueue} from '@app/api/infrastructure/AssetDeletionQueue';
import {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import {getExpiryBucket} from '@app/api/utils/AttachmentDecay';
import {processExpiredAttachments} from '@app/api/worker/tasks/ExpireAttachments';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {ms} from 'itty-time';
import {afterEach, describe, expect, it} from 'vitest';

const ATTACHMENT_ID = createAttachmentID(9001n);
const CHANNEL_ID = createChannelID(9002n);
const MESSAGE_ID = createMessageID(9003n);
const FILENAME = 'decaying.png';
const UPLOADED_AT = new Date('2026-01-01T00:00:00.000Z');
const FIRST_EXPIRY = new Date('2026-01-31T00:00:00.000Z');
const EXTENDED_EXPIRY = new Date('2026-02-25T00:00:00.000Z');

async function installDependencies(kvClient = new MockKVProvider()): Promise<AssetDeletionQueue> {
	const assetDeletionQueue = new AssetDeletionQueue(kvClient);
	const instanceConfigRepository = new InstanceConfigRepository();
	await instanceConfigRepository.setInstanceMediaConfig({attachment_decay: {enabled: true}});
	setWorkerDependenciesForTest({assetDeletionQueue, kvClient, instanceConfigRepository});
	return assetDeletionQueue;
}

async function queuedKeys(queue: AssetDeletionQueue): Promise<Array<string>> {
	return (await queue.getBatch(100)).map((item) => item.s3Key);
}

async function writeDecayRecord(repository: AttachmentDecayRepository, expiresAt: Date): Promise<void> {
	await repository.upsert({
		attachment_id: ATTACHMENT_ID,
		channel_id: CHANNEL_ID,
		message_id: MESSAGE_ID,
		filename: FILENAME,
		size_bytes: 1024n,
		uploaded_at: UPLOADED_AT,
		expires_at: expiresAt,
		last_accessed_at: UPLOADED_AT,
		cost: 1,
		lifetime_days: 30,
		status: null,
		expiry_bucket: getExpiryBucket(expiresAt),
	});
}

describe('processExpiredAttachments', () => {
	const originalSelfHosted = getConfig().instance.selfHosted;
	afterEach(() => {
		getConfig().instance.selfHosted = originalSelfHosted;
		clearWorkerDependencies();
	});

	it.each([
		[true, []],
		[false, [`attachments/${CHANNEL_ID}/${ATTACHMENT_ID}/${FILENAME}`]],
	])('pauses deletion during a reconcile run only when self-hosted (%s)', async (selfHosted, expectedKeys) => {
		getConfig().instance.selfHosted = selfHosted;
		const repository = new AttachmentDecayRepository();
		const kvClient = new MockKVProvider();
		await kvClient.setex('attachment_decay:reconcile_run', 60, 'run');
		const queue = await installDependencies(kvClient);
		await writeDecayRecord(repository, FIRST_EXPIRY);

		await processExpiredAttachments(new Date(FIRST_EXPIRY.getTime() + ms('1 day')));

		expect(await queuedKeys(queue)).toEqual(expectedKeys);
	});

	it('keeps the decay record when it clears a superseded expiry row', async () => {
		const repository = new AttachmentDecayRepository();
		await installDependencies();
		await writeDecayRecord(repository, FIRST_EXPIRY);
		await writeDecayRecord(repository, EXTENDED_EXPIRY);

		await processExpiredAttachments(new Date(FIRST_EXPIRY.getTime() + ms('1 day')));

		const record = await repository.fetchById(ATTACHMENT_ID);
		expect(record?.expires_at.toISOString()).toBe(EXTENDED_EXPIRY.toISOString());
		expect(await repository.fetchExpiredByBucket(getExpiryBucket(FIRST_EXPIRY), EXTENDED_EXPIRY, 10)).toHaveLength(0);
	});

	it('still queues the asset once the extended expiry passes', async () => {
		const repository = new AttachmentDecayRepository();
		const queue = await installDependencies();
		await writeDecayRecord(repository, FIRST_EXPIRY);
		await writeDecayRecord(repository, EXTENDED_EXPIRY);

		await processExpiredAttachments(new Date(FIRST_EXPIRY.getTime() + ms('1 day')));
		await processExpiredAttachments(new Date(EXTENDED_EXPIRY.getTime() + ms('1 day')));

		expect(await queuedKeys(queue)).toEqual([`attachments/${CHANNEL_ID}/${ATTACHMENT_ID}/${FILENAME}`]);
		expect(await repository.fetchById(ATTACHMENT_ID)).toBeNull();
	});
});
