// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {createGuildID, createUserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import type {GuildRepository} from '../../guild/repositories/GuildRepository';
import type {IPurgeQueue} from '../../infrastructure/BunnyPurgeQueue';
import type {
	IAssetDeletionQueue,
	QueuedAssetDeletion,
	QueuedAssetReference,
} from '../../infrastructure/IAssetDeletionQueue';
import type {IStorageService} from '../../infrastructure/IStorageService';
import {Logger} from '../../Logger';
import type {UserRepository} from '../../user/repositories/UserRepository';
import {getWorkerDependencies} from '../WorkerContext';

const BATCH_SIZE = 50;
const MAX_ITEMS_PER_RUN = 500;

interface ProcessItemResult {
	storageDeleted: boolean;
	storageSkipped: boolean;
	cdnPurged: boolean;
}

const processAssetDeletionQueue: WorkerTaskHandler = async (_payload, _helpers) => {
	const {assetDeletionQueue, purgeQueue, storageService, userRepository, guildRepository} = getWorkerDependencies();
	const queueSize = await assetDeletionQueue.getQueueSize();
	if (queueSize === 0) {
		Logger.debug('Asset deletion queue is empty');
		return;
	}
	Logger.info({queueSize}, 'Starting asset deletion queue processing');
	const maxItems = Math.min(MAX_ITEMS_PER_RUN, queueSize);
	let totalProcessed = 0;
	let totalDeleted = 0;
	let totalSkipped = 0;
	let totalFailed = 0;
	let totalCdnPurged = 0;
	while (totalProcessed < maxItems) {
		const batch = await assetDeletionQueue.getBatch(Math.min(BATCH_SIZE, maxItems - totalProcessed));
		if (batch.length === 0) {
			break;
		}
		const results = await Promise.allSettled(
			batch.map((item) =>
				processItem(item, {
					storageService,
					purgeQueue,
					assetDeletionQueue,
					userRepository,
					guildRepository,
				}),
			),
		);
		for (let i = 0; i < results.length; i++) {
			const result = results[i]!;
			const item = batch[i]!;
			if (result.status === 'fulfilled') {
				if (result.value.storageDeleted) {
					totalDeleted++;
				}
				if (result.value.storageSkipped) {
					totalSkipped++;
				}
				if (result.value.cdnPurged) {
					totalCdnPurged++;
				}
			} else {
				totalFailed++;
				Logger.error(
					{error: result.reason, s3Key: item.s3Key, cdnUrl: item.cdnUrl},
					'Failed to process asset deletion',
				);
			}
		}
		totalProcessed += batch.length;
	}
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
	if (totalFailed > 0) {
		throw new Error(
			`Asset deletion queue processing completed with ${totalFailed} failures out of ${totalProcessed} items`,
		);
	}
};

async function processItem(
	item: QueuedAssetDeletion,
	deps: {
		storageService: IStorageService;
		purgeQueue: IPurgeQueue;
		assetDeletionQueue: IAssetDeletionQueue;
		userRepository: Pick<UserRepository, 'findUnique'>;
		guildRepository: Pick<GuildRepository, 'findUnique' | 'getMember'>;
	},
): Promise<ProcessItemResult> {
	const result: ProcessItemResult = {
		storageDeleted: false,
		storageSkipped: false,
		cdnPurged: false,
	};
	try {
		if (item.s3Key) {
			const isStillReferenced = await isQueuedAssetStillCurrent(item, deps);
			if (isStillReferenced) {
				result.storageSkipped = true;
				Logger.info({s3Key: item.s3Key, reason: item.reason}, 'Skipped deleting currently referenced asset');
			} else {
				try {
					await deps.storageService.deleteObject(Config.s3.buckets.cdn, item.s3Key);
					result.storageDeleted = true;
					Logger.debug({s3Key: item.s3Key, reason: item.reason}, 'Deleted asset from S3');
				} catch (error: unknown) {
					const isNotFound =
						error instanceof Error &&
						(('name' in error && error.name === 'NotFound') ||
							('code' in error &&
								(
									error as {
										code?: string;
									}
								).code === 'NoSuchKey'));
					if (!isNotFound) {
						throw error;
					}
					result.storageDeleted = true;
					Logger.debug({s3Key: item.s3Key}, 'Asset already deleted from S3 (NotFound)');
				}
			}
		}
		if (item.cdnUrl) {
			await deps.purgeQueue.addUrls([item.cdnUrl]);
			result.cdnPurged = true;
			Logger.debug({cdnUrl: item.cdnUrl}, 'Queued asset CDN URL for purge');
		}
		return result;
	} catch (error) {
		await deps.assetDeletionQueue.requeueItem(item);
		throw error;
	}
}

async function isQueuedAssetStillCurrent(
	item: QueuedAssetDeletion,
	deps: {
		userRepository: Pick<UserRepository, 'findUnique'>;
		guildRepository: Pick<GuildRepository, 'findUnique' | 'getMember'>;
	},
): Promise<boolean> {
	const reference = item.staleReference ?? inferStaleReferenceFromS3Key(item.s3Key);
	if (!reference || stripAnimationPrefix(reference.hash) !== getStorageHashFromS3Key(item.s3Key)) {
		return false;
	}
	const currentHash = await getCurrentAssetHash(reference, deps);
	return currentHash !== null && stripAnimationPrefix(currentHash) === stripAnimationPrefix(reference.hash);
}

async function getCurrentAssetHash(
	reference: QueuedAssetReference,
	deps: {
		userRepository: Pick<UserRepository, 'findUnique'>;
		guildRepository: Pick<GuildRepository, 'findUnique' | 'getMember'>;
	},
): Promise<string | null> {
	const entityId = parseDecimalBigInt(reference.entityId);
	if (entityId === null) {
		return null;
	}
	if (reference.entityType === 'user') {
		const user = await deps.userRepository.findUnique(createUserID(entityId));
		if (!user) {
			return null;
		}
		switch (reference.assetType) {
			case 'avatar':
				return user.avatarHash;
			case 'banner':
				return user.bannerHash;
			default:
				return null;
		}
	}
	if (reference.entityType === 'guild') {
		const guild = await deps.guildRepository.findUnique(createGuildID(entityId));
		if (!guild) {
			return null;
		}
		switch (reference.assetType) {
			case 'icon':
				return guild.iconHash;
			case 'banner':
				return guild.bannerHash;
			case 'splash':
				return guild.splashHash;
			case 'embed_splash':
				return guild.embedSplashHash;
			default:
				return null;
		}
	}
	if (reference.entityType === 'guild_member') {
		const guildId = reference.guildId ? parseDecimalBigInt(reference.guildId) : null;
		if (guildId === null) {
			return null;
		}
		const member = await deps.guildRepository.getMember(createGuildID(guildId), createUserID(entityId));
		if (!member) {
			return null;
		}
		switch (reference.assetType) {
			case 'avatar':
				return member.avatarHash;
			case 'banner':
				return member.bannerHash;
			default:
				return null;
		}
	}
	return null;
}

function inferStaleReferenceFromS3Key(s3Key: string): QueuedAssetReference | null {
	const parts = s3Key.split('/');
	if (parts.length === 3) {
		const [prefix, entityId, hash] = parts;
		switch (prefix) {
			case 'avatars':
				return {entityType: 'user', assetType: 'avatar', entityId: entityId!, hash: hash!};
			case 'icons':
				return {entityType: 'guild', assetType: 'icon', entityId: entityId!, hash: hash!};
			case 'splashes':
				return {entityType: 'guild', assetType: 'splash', entityId: entityId!, hash: hash!};
			case 'embed-splashes':
				return {entityType: 'guild', assetType: 'embed_splash', entityId: entityId!, hash: hash!};
			default:
				return null;
		}
	}
	if (
		parts.length === 6 &&
		parts[0] === 'guilds' &&
		parts[2] === 'users' &&
		(parts[4] === 'avatars' || parts[4] === 'banners')
	) {
		return {
			entityType: 'guild_member',
			assetType: parts[4] === 'avatars' ? 'avatar' : 'banner',
			guildId: parts[1]!,
			entityId: parts[3]!,
			hash: parts[5]!,
		};
	}
	return null;
}

function parseDecimalBigInt(value: string): bigint | null {
	if (!/^\d+$/.test(value)) {
		return null;
	}
	try {
		return BigInt(value);
	} catch {
		return null;
	}
}

function stripAnimationPrefix(hash: string): string {
	return hash.startsWith('a_') ? hash.substring(2) : hash;
}

function getStorageHashFromS3Key(s3Key: string): string | null {
	return s3Key.split('/').pop() ?? null;
}

export default processAssetDeletionQueue;
