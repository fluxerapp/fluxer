// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {ArchiveAttemptSupersededError} from '@app/api/archive/ArchiveAttemptSupersededError';
import {
	type ArchiveTaskHandler,
	ArchiveTerminalFailureError,
	createArchiveTask,
	throwIfArchiveTerminallyFailed,
} from '@app/api/archive/ArchiveTask';
import {makeDataPackageAttachmentCdnUrl} from '@app/api/attachment/AttachmentUrls';
import {type AttachmentID, type ChannelID, createGuildID, type MessageID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {makeAttachmentCdnKey} from '@app/api/channel/services/message/MessageHelpers';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {Logger} from '@app/api/Logger';
import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import {writeZipArchive} from '@app/api/worker/utils/ArchiveFile';
import {createArchiveJsonBuffer} from '@app/api/worker/utils/ArchiveJson';
import {
	buildHashedAssetKey,
	buildSimpleAssetKey,
	getAnimatedAssetExtension,
	getEmojiExtension,
} from '@app/api/worker/utils/AssetArchiveHelpers';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {GUILD_TEXT_BASED_CHANNEL_TYPES} from '@fluxer/constants/src/ChannelConstants';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import {z} from 'zod';

const CHANNEL_CONCURRENCY = 4;
const ASSET_CONCURRENCY = 8;
const ATTACHMENT_CONCURRENCY = 16;
const P_START = 5;
const P_META = 15;
const P_MESSAGES = 60;
const P_ASSETS = 68;
const P_ATTACHMENTS = 88;
const P_ZIP = 95;
const MESSAGE_BATCH_SIZE = 100;
const MESSAGE_LIMIT_PER_CHANNEL = 1000;
const PayloadSchema = z.object({
	guildId: z.string(),
	archiveId: z.string(),
	requestedBy: z.string(),
	includeAttachments: z.boolean().default(false),
});

interface PendingAttachmentDownload {
	channelId: ChannelID;
	attachmentId: AttachmentID;
	filename: string;
}

export interface GuildHarvestAttachment {
	id: AttachmentID;
	filename: string;
	size: bigint;
	contentType: string;
	width: number | null;
	height: number | null;
}

export function buildGuildHarvestAttachment(
	channelId: ChannelID,
	attachment: GuildHarvestAttachment,
	includeAttachments: boolean,
): Record<string, unknown> {
	return {
		attachment_id: attachment.id.toString(),
		filename: attachment.filename,
		size: attachment.size.toString(),
		content_type: attachment.contentType,
		archive_path: includeAttachments ? `attachments/${channelId}/${attachment.id}/${attachment.filename}` : null,
		cdn_url: makeDataPackageAttachmentCdnUrl(channelId, attachment.id, attachment.filename),
		width: attachment.width,
		height: attachment.height,
	};
}

function cdnBucket(): string {
	return Config.s3.buckets.cdn;
}

async function downloadToDisk(
	storageService: IStorageService,
	bucket: string,
	key: string,
	destPath: string,
): Promise<void> {
	try {
		await fs.promises.mkdir(path.dirname(destPath), {recursive: true});
		await storageService.writeObjectToDisk(bucket, key, destPath);
	} catch (error) {
		if (!(error instanceof Error) || (error.name !== 'NoSuchKey' && error.name !== 'NotFound')) throw error;
		Logger.warn({key}, 'Skipping missing S3 object during harvest');
	}
}

const harvestGuildData: ArchiveTaskHandler = async (payload, helpers, attempt) => {
	const validated = PayloadSchema.parse(payload);
	helpers.logger.debug({payload}, 'Processing harvestGuildData task');
	const guildId = createGuildID(BigInt(validated.guildId));
	const archiveId = BigInt(validated.archiveId);
	const guildIdStr = guildId.toString();
	const {guildRepository, channelRepository, adminArchiveRepository, storageService} = getWorkerDependencies();
	const existingArchive = await adminArchiveRepository.findBySubjectAndArchiveId('guild', guildId, archiveId);
	if (!existingArchive) throw new Error('Admin archive record not found for guild');
	throwIfArchiveTerminallyFailed(existingArchive);
	if (existingArchive.completedAt) {
		Logger.info({guildId, archiveId}, 'Guild archive already completed, skipping');
		return;
	}
	const adminArchive = await adminArchiveRepository.markAsStarted(existingArchive, 'Starting guild archive');
	const {attemptId, expiresAt} = adminArchive;
	assert(attemptId !== null && expiresAt !== null, 'Claimed guild archive is incomplete');
	let progressUpdate = Promise.resolve();
	const updateProgress = (percent: number, step: string): Promise<void> => {
		progressUpdate = progressUpdate.then(() => adminArchiveRepository.updateProgress(adminArchive, percent, step));
		return progressUpdate;
	};
	let storageKey: string;
	let fileSize: bigint;
	try {
		await using tmpDir = await fs.promises.mkdtempDisposable(path.join(os.tmpdir(), 'fluxer-guild-archive-'));
		const contentDir = path.join(tmpDir.path, 'content');
		const zipPath = path.join(tmpDir.path, 'archive.zip');
		await fs.promises.mkdir(contentDir);
		const guild = await guildRepository.findUnique(guildId);
		if (!guild) throw new Error(`Guild ${guildIdStr} not found`);
		await updateProgress(P_START, 'Collecting guild metadata');
		const [roles, members, channels, emojis, stickers] = await Promise.all([
			guildRepository.listRoles(guildId),
			guildRepository.listMembers(guildId),
			channelRepository.channelData.listGuildChannels(guildId),
			guildRepository.listEmojis(guildId),
			guildRepository.listStickers(guildId),
		]);
		await updateProgress(P_META, 'Writing guild metadata');
		const guildJson = {
			guild: {
				id: guild.id.toString(),
				name: guild.name,
				owner_id: guild.ownerId.toString(),
				features: Array.from(guild.features),
				verification_level: guild.verificationLevel,
				default_message_notifications: guild.defaultMessageNotifications,
				explicit_content_filter: guild.explicitContentFilter,
				created_at: snowflakeToDate(guild.id).toISOString(),
			},
			roles: roles.map((r) => ({
				id: r.id.toString(),
				name: r.name,
				color: r.color,
				position: r.position,
				permissions: r.permissions.toString(),
				mentionable: r.isMentionable,
				hoist: r.isHoisted,
			})),
			members: members.map((m) => ({
				user_id: m.userId.toString(),
				joined_at: m.joinedAt.toISOString(),
				nickname: m.nickname,
				role_ids: Array.from(m.roleIds).map((id) => id.toString()),
				avatar_hash: m.avatarHash,
				banner_hash: m.bannerHash,
			})),
			emojis: emojis.map((e) => ({
				id: e.id.toString(),
				name: e.name,
				animated: e.isAnimated,
				creator_id: e.creatorId.toString(),
			})),
			stickers: stickers.map((s) => ({
				id: s.id.toString(),
				name: s.name,
				description: s.description,
				animated: s.animated,
				tags: s.tags,
				creator_id: s.creatorId.toString(),
			})),
			channels: channels.map((c) => ({
				id: c.id.toString(),
				name: c.name,
				type: c.type,
				parent_id: c.parentId?.toString() ?? null,
				topic: c.topic,
				nsfw: c.isNsfw,
				position: c.position,
				last_message_id: c.lastMessageId?.toString() ?? null,
			})),
		};
		await fs.promises.writeFile(path.join(contentDir, 'guild.json'), createArchiveJsonBuffer(guildJson));
		await updateProgress(P_META, `Harvesting messages from ${channels.length} channels`);
		const textChannels = channels.filter((c) => GUILD_TEXT_BASED_CHANNEL_TYPES.has(c.type));
		const pendingDownloads: Array<PendingAttachmentDownload> = [];
		let processedChannels = 0;
		await mapWithConcurrency(textChannels, CHANNEL_CONCURRENCY, async (channel) => {
			const messages: Array<object> = [];
			let beforeMessageId: MessageID | undefined;
			let channelDownloads: Array<PendingAttachmentDownload> = [];
			while (messages.length < MESSAGE_LIMIT_PER_CHANNEL) {
				const batch = await channelRepository.listMessages(channel.id, beforeMessageId, MESSAGE_BATCH_SIZE);
				if (batch.length === 0) break;
				for (const msg of batch) {
					if (msg.authorId == null) continue;
					const attachments: Array<object> = [];
					for (const att of msg.attachments) {
						attachments.push(buildGuildHarvestAttachment(channel.id, att, validated.includeAttachments));
						if (validated.includeAttachments) {
							channelDownloads.push({
								channelId: channel.id,
								attachmentId: att.id,
								filename: att.filename,
							});
						}
					}
					messages.push({
						id: msg.id.toString(),
						author_id: msg.authorId.toString(),
						timestamp: snowflakeToDate(msg.id).toISOString(),
						content: msg.content ?? null,
						attachments,
					});
				}
				beforeMessageId = batch[batch.length - 1]!.id;
			}
			const chanDir = path.join(contentDir, 'channels', channel.id.toString());
			await fs.promises.mkdir(chanDir, {recursive: true});
			await fs.promises.writeFile(path.join(chanDir, 'messages.json'), createArchiveJsonBuffer(messages));
			pendingDownloads.push(...channelDownloads);
			channelDownloads = [];
			processedChannels++;
			const pct = P_META + Math.floor((processedChannels / Math.max(textChannels.length, 1)) * (P_MESSAGES - P_META));
			await updateProgress(pct, `Messages: ${processedChannels}/${textChannels.length} channels`);
		});
		await updateProgress(P_MESSAGES, 'Downloading guild assets');
		type AssetJob = {
			key: string;
			dest: string;
		};
		const assetJobs: Array<AssetJob> = [];
		for (const {hash, prefix, fileName} of [
			{hash: guild.iconHash, prefix: 'icons', fileName: 'icon'},
			{hash: guild.bannerHash, prefix: 'banners', fileName: 'banner'},
			{hash: guild.splashHash, prefix: 'splashes', fileName: 'splash'},
			{hash: guild.embedSplashHash, prefix: 'embed-splashes', fileName: 'embed-splash'},
		]) {
			if (!hash) continue;
			const ext = getAnimatedAssetExtension(hash);
			assetJobs.push({
				key: buildHashedAssetKey(prefix, guildIdStr, hash),
				dest: path.join(contentDir, 'assets', 'guild', `${fileName}.${ext}`),
			});
		}
		for (const emoji of emojis) {
			const id = emoji.id.toString();
			assetJobs.push({
				key: buildSimpleAssetKey('emojis', id),
				dest: path.join(contentDir, 'assets', 'guild', 'emojis', `${id}.${getEmojiExtension(emoji.isAnimated)}`),
			});
		}
		for (const sticker of stickers) {
			const id = sticker.id.toString();
			assetJobs.push({
				key: buildSimpleAssetKey('stickers', id),
				dest: path.join(contentDir, 'assets', 'guild', 'stickers', `${id}.${sticker.animated ? 'gif' : 'png'}`),
			});
		}
		await mapWithConcurrency(assetJobs, ASSET_CONCURRENCY, async ({key, dest}) => {
			await downloadToDisk(storageService, cdnBucket(), key, dest);
		});
		await updateProgress(P_ASSETS, `Downloading ${pendingDownloads.length} attachments`);
		if (pendingDownloads.length > 0) {
			let doneCount = 0;
			let lastPct = P_ASSETS;
			await mapWithConcurrency(pendingDownloads, ATTACHMENT_CONCURRENCY, async (dl) => {
				const storageKey = makeAttachmentCdnKey(dl.channelId, dl.attachmentId, dl.filename);
				const dest = path.join(
					contentDir,
					'attachments',
					dl.channelId.toString(),
					dl.attachmentId.toString(),
					dl.filename,
				);
				await downloadToDisk(storageService, cdnBucket(), storageKey, dest);
				doneCount++;
				const newPct = P_ASSETS + Math.floor((doneCount / pendingDownloads.length) * (P_ATTACHMENTS - P_ASSETS));
				if (newPct > lastPct) {
					lastPct = newPct;
					await updateProgress(newPct, `Attachments: ${doneCount}/${pendingDownloads.length}`);
				}
			});
		}
		await updateProgress(P_ATTACHMENTS, 'Creating archive');
		await writeZipArchive(zipPath, (archive) => archive.directory(contentDir));
		await updateProgress(P_ZIP, 'Uploading archive');
		storageKey = `archives/guilds/${guildId}/${archiveId}/${attemptId}/guild-archive.zip`;
		const zipStat = await fs.promises.stat(zipPath);
		fileSize = BigInt(zipStat.size);
		await storageService.uploadObjectFromFile({
			bucket: Config.s3.buckets.harvests,
			key: storageKey,
			filePath: zipPath,
			contentLength: zipStat.size,
			contentType: 'application/zip',
			expiresAt,
		});
	} catch (error) {
		if (error instanceof ArchiveAttemptSupersededError) throw error;
		Logger.error({error, guildId, archiveId}, 'Failed to harvest guild data');
		const message = error instanceof Error ? error.message : String(error);
		try {
			if (attempt.isLastAttempt) {
				await adminArchiveRepository.markAsTerminallyFailed(adminArchive, message);
			} else {
				await adminArchiveRepository.markAsFailed(adminArchive, message);
			}
		} catch (failureError) {
			if (failureError instanceof ArchiveAttemptSupersededError) throw failureError;
			throw new AggregateError([error, failureError], 'Failed to prepare guild archive and record its failure');
		}
		if (attempt.isLastAttempt) throw new ArchiveTerminalFailureError(message);
		throw error;
	}
	await adminArchiveRepository.markAsCompleted(adminArchive, storageKey, fileSize, expiresAt);
};

export default createArchiveTask(harvestGuildData);
