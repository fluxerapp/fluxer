// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {GUILD_TEXT_BASED_CHANNEL_TYPES} from '@fluxer/constants/src/ChannelConstants';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import archiver from 'archiver';
import {ms} from 'itty-time';
import {z} from 'zod';
import {type AttachmentID, type ChannelID, createGuildID, type MessageID} from '../../BrandedTypes';
import {Config} from '../../Config';
import {makeAttachmentCdnKey, makeAttachmentCdnUrl} from '../../channel/services/message/MessageHelpers';
import {Logger} from '../../Logger';
import {createArchiveJsonBuffer} from '../utils/ArchiveJson';
import {
	buildHashedAssetKey,
	buildSimpleAssetKey,
	getAnimatedAssetExtension,
	getEmojiExtension,
} from '../utils/AssetArchiveHelpers';
import {getWorkerDependencies} from '../WorkerContext';
import type {WorkerDependencies} from '../WorkerDependencies';

const CHANNEL_CONCURRENCY = 4;
const ASSET_CONCURRENCY = 8;
const ATTACHMENT_CONCURRENCY = 16;
const P_START = 5;
const P_META = 15;
const P_MESSAGES = 60;
const P_ASSETS = 68;
const P_ATTACHMENTS = 88;
const P_ZIP = 95;
const P_DONE = 100;
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

async function parallel<T>(
	items: Array<T>,
	concurrency: number,
	fn: (item: T, index: number) => Promise<void>,
): Promise<void> {
	let i = 0;
	const worker = async () => {
		while (true) {
			const idx = i++;
			if (idx >= items.length) return;
			await fn(items[idx]!, idx);
		}
	};
	await Promise.all(Array.from({length: Math.min(concurrency, items.length || 1)}, worker));
}

function cdnBucket(): string {
	return Config.s3.buckets.cdn;
}

async function downloadToDisk(
	storageService: WorkerDependencies['storageService'],
	bucket: string,
	key: string,
	destPath: string,
): Promise<boolean> {
	try {
		await fs.promises.mkdir(path.dirname(destPath), {recursive: true});
		await storageService.writeObjectToDisk(bucket, key, destPath);
		return true;
	} catch (err) {
		const name = err instanceof Error ? err.name : String(err);
		if (name === 'NoSuchKey' || name === 'NotFound') return false;
		Logger.warn({key, err: name}, 'Skipping unreadable S3 object during harvest');
		return false;
	}
}

const harvestGuildData: WorkerTaskHandler = async (payload, helpers) => {
	const validated = PayloadSchema.parse(payload);
	helpers.logger.debug({payload}, 'Processing harvestGuildData task');
	const guildId = createGuildID(BigInt(validated.guildId));
	const archiveId = BigInt(validated.archiveId);
	const guildIdStr = guildId.toString();
	const {guildRepository, channelRepository, adminArchiveRepository, storageService} = getWorkerDependencies();
	const adminArchive = await adminArchiveRepository.findBySubjectAndArchiveId('guild', guildId, archiveId);
	if (!adminArchive) throw new Error('Admin archive record not found for guild');
	const upd = (pct: number, step: string) => adminArchiveRepository.updateProgress(adminArchive, pct, step);
	const fail = (msg: string) => adminArchiveRepository.markAsFailed(adminArchive, msg);
	const done = (key: string, size: bigint, exp: Date) =>
		adminArchiveRepository.markAsCompleted(adminArchive, key, size, exp);
	const tmpDir = await fs.promises.mkdtemp(path.join(os.tmpdir(), 'fluxer-guild-archive-'));
	const contentDir = path.join(tmpDir, 'content');
	const zipPath = path.join(tmpDir, 'archive.zip');
	await fs.promises.mkdir(contentDir);
	try {
		await adminArchiveRepository.markAsStarted(adminArchive, 'Starting guild archive');
		const guild = await guildRepository.findUnique(guildId);
		if (!guild) throw new Error(`Guild ${guildIdStr} not found`);
		await upd(P_START, 'Collecting guild metadata');
		const [roles, members, channels, emojis, stickers] = await Promise.all([
			guildRepository.listRoles(guildId),
			guildRepository.listMembers(guildId),
			channelRepository.channelData.listGuildChannels(guildId),
			guildRepository.listEmojis(guildId),
			guildRepository.listStickers(guildId),
		]);
		await upd(P_META, 'Writing guild metadata');
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
		await upd(P_META, `Harvesting messages from ${channels.length} channels`);
		const textChannels = channels.filter((c) => GUILD_TEXT_BASED_CHANNEL_TYPES.has(c.type));
		const threadRefs = await channelRepository.threads.listThreadRefsByGuild(guildId);
		const threadChannels = await channelRepository.listChannels(threadRefs.map((ref) => ref.thread_id));
		const messageableChannels = [...textChannels, ...threadChannels];
		const pendingDownloads: Array<PendingAttachmentDownload> = [];
		let processedChannels = 0;
		await parallel(messageableChannels, CHANNEL_CONCURRENCY, async (channel) => {
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
						attachments.push({
							attachment_id: att.id.toString(),
							filename: att.filename,
							size: att.size.toString(),
							content_type: att.contentType,
							archive_path: validated.includeAttachments ? `attachments/${channel.id}/${att.id}/${att.filename}` : null,
							cdn_url: makeAttachmentCdnUrl(channel.id, att.id, att.filename),
							width: att.width,
							height: att.height,
						});
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
			upd(pct, `Messages: ${processedChannels}/${textChannels.length} channels`).catch((error: unknown) =>
				Logger.warn({error}, 'Failed to report guild archive progress'),
			);
		});
		await upd(P_MESSAGES, 'Downloading guild assets');
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
		await parallel(assetJobs, ASSET_CONCURRENCY, async ({key, dest}) => {
			await downloadToDisk(storageService, cdnBucket(), key, dest);
		});
		await upd(P_ASSETS, `Downloading ${pendingDownloads.length} attachments`);
		if (pendingDownloads.length > 0) {
			let doneCount = 0;
			let lastPct = P_ASSETS;
			await parallel(pendingDownloads, ATTACHMENT_CONCURRENCY, async (dl) => {
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
					upd(newPct, `Attachments: ${doneCount}/${pendingDownloads.length}`).catch((error: unknown) =>
						Logger.warn({error}, 'Failed to report guild archive progress'),
					);
				}
			});
		}
		await upd(P_ATTACHMENTS, 'Creating archive');
		await new Promise<void>((resolve, reject) => {
			const output = fs.createWriteStream(zipPath);
			const arc = archiver('zip', {zlib: {level: 6}});
			arc.on('error', reject);
			output.on('error', reject);
			output.on('close', resolve);
			arc.pipe(output);
			arc.directory(contentDir, false);
			arc.finalize();
		});
		await upd(P_ZIP, 'Uploading archive');
		const expiresAt = new Date(Date.now() + ms('1 year'));
		const storageKey = `archives/guilds/${guildId}/${archiveId}/guild-archive.zip`;
		const fileSize = BigInt((await fs.promises.stat(zipPath)).size);
		await storageService.uploadObject({
			bucket: Config.s3.buckets.harvests,
			key: storageKey,
			body: fs.createReadStream(zipPath),
			contentType: 'application/zip',
			expiresAt,
		});
		await done(storageKey, fileSize, expiresAt);
		await upd(P_DONE, 'Completed');
	} catch (error) {
		Logger.error({error, guildId, archiveId}, 'Failed to harvest guild data');
		await fail(error instanceof Error ? error.message : String(error));
		throw error;
	} finally {
		await fs.promises.rm(tmpDir, {recursive: true, force: true});
	}
};

export default harvestGuildData;
