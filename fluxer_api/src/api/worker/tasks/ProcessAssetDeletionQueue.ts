// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {IPurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import type {
	IAssetDeletionQueue,
	QueuedAssetDeletion,
	QueuedAssetEntityType,
	QueuedAssetReference,
	QueuedAssetType,
} from '@app/api/infrastructure/IAssetDeletionQueue';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {Logger} from '@app/api/Logger';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const BATCH_SIZE = 50;
const MAX_ITEMS_PER_RUN = 500;

interface ProcessItemResult {
	storageDeleted: boolean;
	storageSkipped: boolean;
	cdnPurged: boolean;
}

interface AssetReferenceRepositories {
	userRepository: Pick<UserRepository, 'findUnique'>;
	guildRepository: Pick<GuildRepository, 'findUnique' | 'getMember'>;
}

interface AssetDeletionDependencies extends AssetReferenceRepositories {
	storageService: IStorageService;
	purgeQueue: IPurgeQueue;
}

const processAssetDeletionQueue: WorkerTaskHandler = async (_payload, _helpers) => {
	const {assetDeletionQueue, purgeQueue, storageService, userRepository, guildRepository} = getWorkerDependencies();
	const queueSize = await assetDeletionQueue.getQueueSize();
	if (!Number.isSafeInteger(queueSize) || queueSize < 0) {
		throw new Error('Asset deletion queue size must be a non-negative safe integer');
	}
	if (queueSize === 0) {
		Logger.debug('Asset deletion queue is empty');
		return;
	}
	Logger.info({queueSize}, 'Starting asset deletion queue processing');
	const dependencies: AssetDeletionDependencies = {storageService, purgeQueue, userRepository, guildRepository};
	let remainingReadBudget = Math.min(MAX_ITEMS_PER_RUN, queueSize);
	let totalProcessed = 0;
	let totalDeleted = 0;
	let totalSkipped = 0;
	let totalFailed = 0;
	let totalCdnPurged = 0;
	const failedItems: Array<QueuedAssetDeletion> = [];
	const errors: Array<unknown> = [];
	try {
		while (remainingReadBudget > 0) {
			const count = Math.min(BATCH_SIZE, remainingReadBudget);
			remainingReadBudget -= count;
			const batch = await assetDeletionQueue.getBatch(count);
			const results = await Promise.allSettled(batch.map((item) => processItem(item, dependencies)));
			for (const [index, result] of results.entries()) {
				if (result.status === 'fulfilled') {
					if (result.value.storageDeleted) totalDeleted++;
					if (result.value.storageSkipped) totalSkipped++;
					if (result.value.cdnPurged) totalCdnPurged++;
					continue;
				}
				const item = batch[index];
				totalFailed++;
				failedItems.push(item);
				Logger.error(
					{error: result.reason, s3Key: item.s3Key, cdnUrl: item.cdnUrl},
					'Failed to process asset deletion',
				);
			}
			totalProcessed += batch.length;
		}
	} catch (error) {
		errors.push(error);
	} finally {
		if (totalFailed > 0) {
			errors.push(
				new Error(
					`Asset deletion queue processing completed with ${totalFailed} failures out of ${totalProcessed} items`,
				),
			);
		}
		errors.push(...(await requeueFailedItems(failedItems, assetDeletionQueue)));
	}
	try {
		const remainingSize = await assetDeletionQueue.getQueueSize();
		Logger.info(
			{
				totalProcessed,
				totalDeleted,
				totalSkipped,
				totalFailed,
				totalCdnPurged,
				remainingSize,
			},
			'Finished asset deletion queue processing',
		);
	} catch (error) {
		errors.push(error);
	}
	if (errors.length === 1) throw errors[0];
	if (errors.length > 1) throw new AggregateError(errors, 'Asset deletion queue processing and retry failures');
};

async function requeueFailedItems(
	items: ReadonlyArray<QueuedAssetDeletion>,
	queue: IAssetDeletionQueue,
): Promise<Array<unknown>> {
	const errors: Array<unknown> = [];
	for (let offset = 0; offset < items.length; offset += BATCH_SIZE) {
		const batch = items.slice(offset, offset + BATCH_SIZE);
		const results = await Promise.allSettled(batch.map(async (item) => queue.requeueItem(item)));
		for (const [index, result] of results.entries()) {
			if (result.status === 'fulfilled') continue;
			const item = batch[index];
			errors.push(result.reason);
			Logger.error({error: result.reason, s3Key: item.s3Key, cdnUrl: item.cdnUrl}, 'Failed to requeue asset deletion');
		}
	}
	return errors;
}

async function processItem(item: QueuedAssetDeletion, deps: AssetDeletionDependencies): Promise<ProcessItemResult> {
	const storageResult = await processStoredAsset(item, deps);
	if (item.cdnUrl) {
		await deps.purgeQueue.addUrls([item.cdnUrl]);
		Logger.debug({cdnUrl: item.cdnUrl}, 'Queued asset CDN URL for purge');
	}
	return {
		storageDeleted: storageResult === 'deleted',
		storageSkipped: storageResult === 'skipped',
		cdnPurged: Boolean(item.cdnUrl),
	};
}

async function processStoredAsset(
	item: QueuedAssetDeletion,
	deps: AssetDeletionDependencies,
): Promise<'deleted' | 'skipped' | 'not_requested'> {
	if (!item.s3Key) return 'not_requested';
	if (await isQueuedAssetStillCurrent(item, deps)) {
		Logger.info({s3Key: item.s3Key, reason: item.reason}, 'Skipped deleting currently referenced asset');
		return 'skipped';
	}
	try {
		await deps.storageService.deleteObject(Config.s3.buckets.cdn, item.s3Key);
		Logger.debug({s3Key: item.s3Key, reason: item.reason}, 'Deleted asset from S3');
	} catch (error) {
		const isNotFound =
			error instanceof Error && (error.name === 'NotFound' || ('code' in error && error.code === 'NoSuchKey'));
		if (!isNotFound) throw error;
		Logger.debug({s3Key: item.s3Key}, 'Asset already deleted from S3 (NotFound)');
	}
	return 'deleted';
}

async function isQueuedAssetStillCurrent(
	item: QueuedAssetDeletion,
	deps: AssetReferenceRepositories,
): Promise<boolean> {
	const references = inferAssetReferencesFromS3Key(item.s3Key);
	const {staleReference} = item;
	if (
		staleReference &&
		!references.some(
			(reference) =>
				reference.entityType === staleReference.entityType &&
				reference.assetType === staleReference.assetType &&
				reference.entityId === staleReference.entityId &&
				reference.guildId === staleReference.guildId &&
				reference.hash === stripAnimationPrefix(staleReference.hash),
		)
	) {
		throw new Error('Asset deletion reference does not match its storage key');
	}
	for (const reference of references) {
		if (!reference.hash || reference.hash === '.' || reference.hash === '..' || reference.hash.includes('\0')) {
			throw new Error('Asset deletion storage key has an invalid hash component');
		}
		const currentHash = await getCurrentAssetHash(reference, deps);
		if (currentHash !== null && stripAnimationPrefix(currentHash) === reference.hash) return true;
	}
	return false;
}

async function getCurrentAssetHash(
	reference: QueuedAssetReference,
	deps: AssetReferenceRepositories,
): Promise<string | null> {
	const entityId = parseAssetEntityId(reference.entityId, 'entityId');
	const {assetType} = reference;
	if (reference.entityType === 'user' && (assetType === 'avatar' || assetType === 'banner')) {
		const user = await deps.userRepository.findUnique(createUserID(entityId));
		return (assetType === 'avatar' ? user?.avatarHash : user?.bannerHash) ?? null;
	}
	if (reference.entityType === 'guild') {
		const guild = await deps.guildRepository.findUnique(createGuildID(entityId));
		switch (assetType) {
			case 'icon':
				return guild?.iconHash ?? null;
			case 'banner':
				return guild?.bannerHash ?? null;
			case 'splash':
				return guild?.splashHash ?? null;
			case 'embed_splash':
				return guild?.embedSplashHash ?? null;
		}
	}
	if (reference.entityType === 'guild_member' && (assetType === 'avatar' || assetType === 'banner')) {
		const guildId = parseAssetEntityId(reference.guildId, 'guildId');
		const member = await deps.guildRepository.getMember(createGuildID(guildId), createUserID(entityId));
		return (assetType === 'avatar' ? member?.avatarHash : member?.bannerHash) ?? null;
	}
	throw new Error('Asset deletion reference has an unsupported entity and asset combination');
}

function inferAssetReferencesFromS3Key(s3Key: string): Array<QueuedAssetReference> {
	const parts = s3Key.split('/');
	switch (parts[0]) {
		case 'avatars':
			return [createAssetReference(parts, 'user', 'avatar')];
		case 'icons':
			return [createAssetReference(parts, 'guild', 'icon')];
		case 'splashes':
			return [createAssetReference(parts, 'guild', 'splash')];
		case 'embed-splashes':
			return [createAssetReference(parts, 'guild', 'embed_splash')];
		case 'banners':
			return [createAssetReference(parts, 'user', 'banner'), createAssetReference(parts, 'guild', 'banner')];
		case 'branding':
			throw new Error('Branding asset deletion requires an instance reference lookup');
		case 'guilds':
			if (parts.length !== 6 || parts[2] !== 'users' || (parts[4] !== 'avatars' && parts[4] !== 'banners')) {
				throw new Error('Asset deletion storage key has an invalid guild member path');
			}
			return [
				{
					entityType: 'guild_member',
					assetType: parts[4] === 'avatars' ? 'avatar' : 'banner',
					guildId: parts[1],
					entityId: parts[3],
					hash: parts[5],
				},
			];
		default:
			return [];
	}
}

function createAssetReference(
	parts: Array<string>,
	entityType: QueuedAssetEntityType,
	assetType: QueuedAssetType,
): QueuedAssetReference {
	if (parts.length !== 3) {
		throw new Error('Asset deletion storage key must contain an asset prefix, entity ID and hash');
	}
	return {entityType, assetType, entityId: parts[1], hash: parts[2]};
}

function parseAssetEntityId(value: string | undefined, field: 'entityId' | 'guildId'): bigint {
	if (!value || value.length > 19 || /\D/.test(value)) {
		throw new Error(`Asset deletion reference has an invalid ${field}`);
	}
	const parsed = SnowflakeType.safeParse(value);
	if (!parsed.success) throw new Error(`Asset deletion reference has an invalid ${field}`);
	return parsed.data;
}

function stripAnimationPrefix(hash: string): string {
	return hash.startsWith('a_') ? hash.substring(2) : hash;
}

export default processAssetDeletionQueue;
