// SPDX-License-Identifier: AGPL-3.0-or-later

import {ms} from 'itty-time';
import {afterEach, describe, expect, it} from 'vitest';
import {AttachmentDecayRepository} from '../../attachment/AttachmentDecayRepository';
import {createAttachmentID, createChannelID, createMessageID} from '../../BrandedTypes';
import type {IAssetDeletionQueue, QueuedAssetDeletion} from '../../infrastructure/IAssetDeletionQueue';
import type {InstanceConfigRepository} from '../../instance/InstanceConfigRepository';
import {getExpiryBucket} from '../../utils/AttachmentDecay';
import {processExpiredAttachments} from '../tasks/ExpireAttachments';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '../WorkerContext';

const ATTACHMENT_ID = createAttachmentID(9001n);
const CHANNEL_ID = createChannelID(9002n);
const MESSAGE_ID = createMessageID(9003n);
const FILENAME = 'decaying.png';
const UPLOADED_AT = new Date('2026-01-01T00:00:00.000Z');
const FIRST_EXPIRY = new Date('2026-01-31T00:00:00.000Z');
const EXTENDED_EXPIRY = new Date('2026-02-25T00:00:00.000Z');

function createQueue(): {queue: IAssetDeletionQueue; queued: Array<QueuedAssetDeletion>} {
	const queued: Array<QueuedAssetDeletion> = [];
	const queue = {
		async queueDeletion(item: Omit<QueuedAssetDeletion, 'queuedAt' | 'retryCount'>) {
			queued.push({...item});
		},
	} as unknown as IAssetDeletionQueue;
	return {queue, queued};
}

function installDependencies(queue: IAssetDeletionQueue): void {
	setWorkerDependenciesForTest({
		assetDeletionQueue: queue,
		instanceConfigRepository: {
			async getEffectiveAttachmentDecayConfig() {
				return {enabled: true};
			},
		} as unknown as InstanceConfigRepository,
	});
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
	afterEach(() => {
		clearWorkerDependencies();
	});

	it('keeps the decay record when it clears a superseded expiry row', async () => {
		const repository = new AttachmentDecayRepository();
		const {queue} = createQueue();
		installDependencies(queue);
		await writeDecayRecord(repository, FIRST_EXPIRY);
		await writeDecayRecord(repository, EXTENDED_EXPIRY);

		await processExpiredAttachments(new Date(FIRST_EXPIRY.getTime() + ms('1 day')));

		const record = await repository.fetchById(ATTACHMENT_ID);
		expect(record?.expires_at.toISOString()).toBe(EXTENDED_EXPIRY.toISOString());
		expect(await repository.fetchExpiredByBucket(getExpiryBucket(FIRST_EXPIRY), EXTENDED_EXPIRY, 10)).toHaveLength(0);
	});

	it('still queues the asset once the extended expiry passes', async () => {
		const repository = new AttachmentDecayRepository();
		const {queue, queued} = createQueue();
		installDependencies(queue);
		await writeDecayRecord(repository, FIRST_EXPIRY);
		await writeDecayRecord(repository, EXTENDED_EXPIRY);

		await processExpiredAttachments(new Date(FIRST_EXPIRY.getTime() + ms('1 day')));
		await processExpiredAttachments(new Date(EXTENDED_EXPIRY.getTime() + ms('1 day')));

		expect(queued.map((item) => item.s3Key)).toEqual([`attachments/${CHANNEL_ID}/${ATTACHMENT_ID}/${FILENAME}`]);
		expect(await repository.fetchById(ATTACHMENT_ID)).toBeNull();
	});
});
