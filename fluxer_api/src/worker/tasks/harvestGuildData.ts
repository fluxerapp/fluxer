/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import archiver from 'archiver';
import type {Task} from 'graphile-worker';
import {createGuildID, type MessageID} from '~/BrandedTypes';
import {Config} from '~/Config';
import {ChannelTypes} from '~/Constants';
import {Logger} from '~/Logger';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';
import {
	appendAssetToArchive,
	buildHashedAssetKey,
	buildSimpleAssetKey,
	getAnimatedAssetExtension,
	getEmojiExtension,
	getStickerExtension,
} from '../utils/AssetArchiveHelpers';
import {getWorkerDependencies} from '../WorkerContext';

interface HarvestGuildDataPayload {
	guildId: string;
	archiveId: string;
	requestedBy: string;
}

const MESSAGE_BATCH_SIZE = 100;
const MESSAGE_LIMIT_PER_CHANNEL = 1000;
const INITIAL_PROGRESS = 5;
const METADATA_PROGRESS = 25;
const MESSAGE_PROGRESS_MAX = 75;
const ZIP_PROGRESS = 90;
const COMPLETE_PROGRESS = 100;
const ARCHIVE_EXPIRY_MS = 365 * 24 * 60 * 60 * 1000;

function assertPayload(payload: unknown): asserts payload is HarvestGuildDataPayload {
	if (typeof payload !== 'object' || payload === null) throw new Error('invalid payload');
	const data = payload as Record<string, unknown>;
	if (typeof data.guildId !== 'string') throw new Error('invalid guildId');
	if (typeof data.archiveId !== 'string') throw new Error('invalid archiveId');
	if (typeof data.requestedBy !== 'string') throw new Error('invalid requestedBy');
}

const harvestGuildData: Task = async (payload, helpers) => {
	assertPayload(payload);
	helpers.logger.debug('Processing harvestGuildData task', {payload});

	const guildId = createGuildID(BigInt(payload.guildId));
	const archiveId = BigInt(payload.archiveId);
	const guildIdString = guildId.toString();

	const {guildRepository, channelRepository, adminArchiveRepository, storageService} = getWorkerDependencies();

	const adminArchive = await adminArchiveRepository.findBySubjectAndArchiveId('guild', guildId, archiveId);
	if (!adminArchive) {
		throw new Error('Admin archive record not found for guild');
	}

	const progress = {
		markStarted: () => adminArchiveRepository.markAsStarted(adminArchive, 'Starting guild archive'),
		updateProgress: (percent: number, step: string) =>
			adminArchiveRepository.updateProgress(adminArchive, percent, step),
		markCompleted: (storageKey: string, fileSize: bigint, expiresAt: Date) =>
			adminArchiveRepository.markAsCompleted(adminArchive, storageKey, fileSize, expiresAt),
		markFailed: (message: string) => adminArchiveRepository.markAsFailed(adminArchive, message),
	};

	try {
		await progress.markStarted();

		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			throw new Error(`Guild ${guildId.toString()} not found`);
		}

		await progress.updateProgress(INITIAL_PROGRESS, 'Collecting guild metadata');

		const [roles, members, channels, emojis, stickers] = await Promise.all([
			guildRepository.listRoles(guildId),
			guildRepository.listMembers(guildId),
			channelRepository.channelData.listGuildChannels(guildId),
			guildRepository.listEmojis(guildId),
			guildRepository.listStickers(guildId),
		]);

		await progress.updateProgress(METADATA_PROGRESS, 'Harvesting channel messages');

		const channelMessages: Record<string, Array<unknown>> = {};
		let processedChannels = 0;
		for (const channel of channels) {
			if (channel.type !== ChannelTypes.GUILD_TEXT) {
				continue;
			}

			const messagesForChannel: Array<{
				id: string;
				author_id: string;
				timestamp: string;
				content: string | null;
			}> = [];

			let beforeMessageId: MessageID | undefined;
			while (messagesForChannel.length < MESSAGE_LIMIT_PER_CHANNEL) {
				const batch = await channelRepository.listMessages(channel.id, beforeMessageId, MESSAGE_BATCH_SIZE);
				if (batch.length === 0) break;

				for (const message of batch) {
					if (!message.authorId) {
						continue;
					}
					messagesForChannel.push({
						id: message.id.toString(),
						author_id: message.authorId.toString(),
						timestamp: new Date(SnowflakeUtils.extractTimestamp(message.id)).toISOString(),
						content: message.content ?? null,
					});
				}

				beforeMessageId = batch[batch.length - 1].id;
			}

			channelMessages[channel.id.toString()] = messagesForChannel;
			processedChannels++;

			const progressPercent = Math.min(
				METADATA_PROGRESS +
					Math.floor((processedChannels / Math.max(channels.length, 1)) * (MESSAGE_PROGRESS_MAX - METADATA_PROGRESS)),
				MESSAGE_PROGRESS_MAX,
			);
			await progress.updateProgress(progressPercent, `Harvested ${processedChannels}/${channels.length} channels`);
		}

		await progress.updateProgress(MESSAGE_PROGRESS_MAX, 'Downloading guild assets');

		const payloadJson = {
			guild: {
				id: guild.id.toString(),
				name: guild.name,
				owner_id: guild.ownerId.toString(),
				features: Array.from(guild.features),
				verification_level: guild.verificationLevel,
				default_message_notifications: guild.defaultMessageNotifications,
				explicit_content_filter: guild.explicitContentFilter,
				created_at: new Date(SnowflakeUtils.extractTimestamp(guild.id)).toISOString(),
			},
			roles: roles.map((role) => ({
				id: role.id.toString(),
				name: role.name,
				color: role.color,
				position: role.position,
				permissions: role.permissions.toString(),
				mentionable: role.isMentionable,
				hoist: role.isHoisted,
			})),
			members: members.map((member) => ({
				user_id: member.userId.toString(),
				joined_at: member.joinedAt.toISOString(),
				nickname: member.nickname,
				role_ids: Array.from(member.roleIds).map((id) => id.toString()),
				avatar_hash: member.avatarHash,
				banner_hash: member.bannerHash,
			})),
			emojis: emojis.map((emoji) => ({
				id: emoji.id.toString(),
				name: emoji.name,
				animated: emoji.isAnimated,
				creator_id: emoji.creatorId.toString(),
			})),
			stickers: stickers.map((sticker) => ({
				id: sticker.id.toString(),
				name: sticker.name,
				description: sticker.description,
				format_type: sticker.formatType,
				tags: sticker.tags,
				creator_id: sticker.creatorId.toString(),
			})),
			channels: channels.map((channel) => ({
				id: channel.id.toString(),
				name: channel.name,
				type: channel.type,
				parent_id: channel.parentId?.toString() ?? null,
				topic: channel.topic,
				nsfw: channel.isNsfw,
				position: channel.position,
				last_message_id: channel.lastMessageId?.toString() ?? null,
			})),
		};

		const payloadBuffer = Buffer.from(JSON.stringify(payloadJson, null, 2), 'utf-8');

		await progress.updateProgress(ZIP_PROGRESS, 'Creating archive');

		const tempDir = await fs.promises.mkdtemp(path.join(os.tmpdir(), 'fluxer-guild-archive-'));
		const zipPath = path.join(tempDir, `guild-${guildId}.zip`);

		try {
			const output = fs.createWriteStream(zipPath);
			const archive = archiver('zip', {zlib: {level: 9}});
			archive.pipe(output);

			archive.append(payloadBuffer, {name: 'guild.json'});
			const guildAssetEntries = [
				{hash: guild.iconHash, prefix: 'icons', fileName: 'icon'},
				{hash: guild.bannerHash, prefix: 'banners', fileName: 'banner'},
				{hash: guild.splashHash, prefix: 'splashes', fileName: 'splash'},
				{hash: guild.embedSplashHash, prefix: 'embed-splashes', fileName: 'embed-splash'},
			];

			for (const entry of guildAssetEntries) {
				if (!entry.hash) {
					continue;
				}

				const assetExtension = getAnimatedAssetExtension(entry.hash);
				const assetArchiveName = `assets/guild/${entry.fileName}.${assetExtension}`;
				const assetStorageKey = buildHashedAssetKey(entry.prefix, guildIdString, entry.hash);

				await appendAssetToArchive({
					archive,
					storageService,
					storageKey: assetStorageKey,
					archiveName: assetArchiveName,
					label: `guild ${entry.fileName}`,
					subjectId: guildIdString,
				});
			}

			for (const emoji of emojis) {
				const emojiId = emoji.id.toString();
				const emojiArchiveName = `assets/guild/emojis/${emojiId}.${getEmojiExtension(emoji.isAnimated)}`;
				const emojiStorageKey = buildSimpleAssetKey('emojis', emojiId);

				await appendAssetToArchive({
					archive,
					storageService,
					storageKey: emojiStorageKey,
					archiveName: emojiArchiveName,
					label: `emoji ${emojiId}`,
					subjectId: guildIdString,
				});
			}

			for (const sticker of stickers) {
				const stickerId = sticker.id.toString();
				const stickerArchiveName = `assets/guild/stickers/${stickerId}.${getStickerExtension(sticker.formatType)}`;
				const stickerStorageKey = buildSimpleAssetKey('stickers', stickerId);

				await appendAssetToArchive({
					archive,
					storageService,
					storageKey: stickerStorageKey,
					archiveName: stickerArchiveName,
					label: `sticker ${stickerId}`,
					subjectId: guildIdString,
				});
			}

			for (const [channelId, messages] of Object.entries(channelMessages)) {
				archive.append(JSON.stringify(messages, null, 2), {name: `channels/${channelId}/messages.json`});
			}

			await archive.finalize();
			await new Promise<void>((resolve, reject) => {
				output.on('close', resolve);
				output.on('error', reject);
			});

			const zipBuffer = await fs.promises.readFile(zipPath);
			const expiresAt = new Date(Date.now() + ARCHIVE_EXPIRY_MS);
			const storageKey = `archives/guilds/${guildId}/${archiveId}/guild-archive.zip`;

			await storageService.uploadObject({
				bucket: Config.s3.buckets.harvests,
				key: storageKey,
				body: zipBuffer,
				contentType: 'application/zip',
				expiresAt,
			});

			await progress.markCompleted(storageKey, BigInt(zipBuffer.length), expiresAt);
			await progress.updateProgress(COMPLETE_PROGRESS, 'Completed');
		} finally {
			await fs.promises.rm(tempDir, {recursive: true, force: true});
		}
	} catch (error) {
		Logger.error({error, guildId, archiveId}, 'Failed to harvest guild data');
		await progress.markFailed(error instanceof Error ? error.message : String(error));
		throw error;
	}
};

export default harvestGuildData;
