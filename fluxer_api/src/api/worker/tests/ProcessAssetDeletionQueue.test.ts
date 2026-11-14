// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createRoleID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {AssetDeletionQueue} from '@app/api/infrastructure/AssetDeletionQueue';
import {NoopPurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import {MockStorageService} from '@app/api/test/mocks/MockStorageService';
import {createTestWorkerHelpers} from '@app/api/test/SyncTaskWorkerService';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import processAssetDeletionQueue from '@app/api/worker/tasks/ProcessAssetDeletionQueue';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {afterEach, describe, expect, it} from 'vitest';

function createHarness(storageService: MockStorageService) {
	const kvClient = new MockKVProvider();
	const assetDeletionQueue = new AssetDeletionQueue(kvClient);
	const guildRepository = new GuildRepository();
	setWorkerDependenciesForTest({
		assetDeletionQueue,
		purgeQueue: new NoopPurgeQueue(),
		storageService,
		userRepository: new UserRepository(kvClient),
		guildRepository,
	});
	return {assetDeletionQueue, guildRepository};
}

const ROLE_GUILD_ID = createGuildID(1430000000000000000n);
const ROLE_ID = createRoleID(1430000000000000001n);

async function seedRoleIcon(guildRepository: GuildRepository, iconHash: string | null): Promise<void> {
	await guildRepository.upsertRole({
		guild_id: ROLE_GUILD_ID,
		role_id: ROLE_ID,
		name: 'Artists',
		permissions: 0n,
		position: 1,
		hoist_position: null,
		color: 0,
		icon_hash: iconHash,
		unicode_emoji: null,
		hoist: false,
		mentionable: false,
		version: 1,
	});
}

function roleIconKey(hash: string): string {
	return `role-icons/${ROLE_ID}/${hash}`;
}

async function storeAndQueueRoleIcon(
	storageService: MockStorageService,
	assetDeletionQueue: AssetDeletionQueue,
	hash: string,
	withReference = true,
): Promise<void> {
	await storageService.uploadObject({bucket: Config.s3.buckets.cdn, key: roleIconKey(hash), body: new Uint8Array([1])});
	await assetDeletionQueue.queueDeletion({
		s3Key: roleIconKey(hash),
		cdnUrl: null,
		reason: 'asset_replaced',
		staleReference: withReference
			? {
					entityType: 'role',
					assetType: 'role_icon',
					entityId: ROLE_ID.toString(),
					guildId: ROLE_GUILD_ID.toString(),
					hash,
				}
			: undefined,
	});
}

async function queueAssets(assetDeletionQueue: AssetDeletionQueue, count: number): Promise<void> {
	for (let index = 0; index < count; index++) {
		await assetDeletionQueue.queueDeletion({
			s3Key: `attachments/100/200/file-${index}.png`,
			cdnUrl: null,
			reason: 'test',
		});
	}
}

describe('processAssetDeletionQueue', () => {
	afterEach(() => {
		clearWorkerDependencies();
	});

	it('attempts each queued asset once per run when storage is failing', async () => {
		const {assetDeletionQueue} = createHarness(new MockStorageService({shouldFailDelete: true}));
		await queueAssets(assetDeletionQueue, 3);

		await expect(processAssetDeletionQueue({}, createTestWorkerHelpers())).rejects.toThrow(/3 failures/);

		const remaining = await assetDeletionQueue.getBatch(10);
		expect(remaining.map((item) => item.retryCount)).toEqual([1, 1, 1]);
	});

	it('attempts each queued asset once per run across batch boundaries', async () => {
		const {assetDeletionQueue} = createHarness(new MockStorageService({shouldFailDelete: true}));
		await queueAssets(assetDeletionQueue, 60);

		await expect(processAssetDeletionQueue({}, createTestWorkerHelpers())).rejects.toThrow(/60 failures/);

		const remaining = await assetDeletionQueue.getBatch(100);
		expect(remaining).toHaveLength(60);
		expect(remaining.every((item) => item.retryCount === 1)).toBe(true);
	});

	it('drains the queue when storage succeeds', async () => {
		const {assetDeletionQueue} = createHarness(new MockStorageService());
		await queueAssets(assetDeletionQueue, 60);

		await processAssetDeletionQueue({}, createTestWorkerHelpers());

		expect(await assetDeletionQueue.getQueueSize()).toBe(0);
	});

	describe('role icons', () => {
		it('deletes a replaced role icon', async () => {
			const storageService = new MockStorageService();
			const {assetDeletionQueue, guildRepository} = createHarness(storageService);
			await seedRoleIcon(guildRepository, 'bbbbbbbb');
			await storeAndQueueRoleIcon(storageService, assetDeletionQueue, 'aaaaaaaa');

			await processAssetDeletionQueue({}, createTestWorkerHelpers());

			expect(storageService.hasObject(Config.s3.buckets.cdn, roleIconKey('aaaaaaaa'))).toBe(false);
		});

		it('keeps the current role icon', async () => {
			const storageService = new MockStorageService();
			const {assetDeletionQueue, guildRepository} = createHarness(storageService);
			await seedRoleIcon(guildRepository, 'aaaaaaaa');
			await storeAndQueueRoleIcon(storageService, assetDeletionQueue, 'aaaaaaaa');

			await processAssetDeletionQueue({}, createTestWorkerHelpers());

			expect(storageService.hasObject(Config.s3.buckets.cdn, roleIconKey('aaaaaaaa'))).toBe(true);
		});

		it('keeps a role icon queued without a stale reference', async () => {
			const storageService = new MockStorageService();
			const {assetDeletionQueue, guildRepository} = createHarness(storageService);
			await seedRoleIcon(guildRepository, null);
			await storeAndQueueRoleIcon(storageService, assetDeletionQueue, 'aaaaaaaa', false);

			await processAssetDeletionQueue({}, createTestWorkerHelpers());

			expect(storageService.hasObject(Config.s3.buckets.cdn, roleIconKey('aaaaaaaa'))).toBe(true);
			expect(await assetDeletionQueue.getQueueSize()).toBe(0);
		});
	});
});
