// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';
import {createChannelID, createGuildID, createMessageID, createUserID, type UserID} from '../../BrandedTypes';
import {Logger} from '../../Logger';
import {mapWithConcurrency} from '../../utils/ConcurrencyUtils';
import {getWorkerDependencies} from '../WorkerContext';

const MENTION_SETTINGS_FETCH_CONCURRENCY = 16;
const MentionChunkEntrySchema = z.object({
	userId: z.string(),
	direct: z.boolean().optional(),
	role: z.boolean().optional(),
	everyone: z.boolean().optional(),
});
const PayloadSchema = z.object({
	channelId: z.string(),
	messageId: z.string(),
	guildId: z.string().optional(),
	chunkIndex: z.number().int().nonnegative().optional(),
	chunkCount: z.number().int().positive().optional(),
	mentions: z.array(MentionChunkEntrySchema),
});

interface MentionChunkEntry {
	userId: UserID;
	direct: boolean;
	role: boolean;
	everyone: boolean;
}

function mergeMentionEntries(entries: Array<z.infer<typeof MentionChunkEntrySchema>>): Array<MentionChunkEntry> {
	const merged = new Map<string, MentionChunkEntry>();
	for (const entry of entries) {
		const existing = merged.get(entry.userId);
		if (existing) {
			existing.direct ||= entry.direct === true;
			existing.role ||= entry.role === true;
			existing.everyone ||= entry.everyone === true;
			continue;
		}
		merged.set(entry.userId, {
			userId: createUserID(BigInt(entry.userId)),
			direct: entry.direct === true,
			role: entry.role === true,
			everyone: entry.everyone === true,
		});
	}
	return Array.from(merged.values());
}

const handleMentionChunk: WorkerTaskHandler = async (payload, helpers) => {
	const validated = PayloadSchema.parse(payload);
	helpers.logger.debug({payload: validated}, 'Processing handleMentionChunk task');
	const {userRepository, readStateService} = getWorkerDependencies();
	const channelId = createChannelID(BigInt(validated.channelId));
	const messageId = createMessageID(BigInt(validated.messageId));
	const guildId = validated.guildId ? createGuildID(BigInt(validated.guildId)) : null;
	const mentions = mergeMentionEntries(validated.mentions);
	if (mentions.length === 0) {
		Logger.debug({channelId, messageId}, 'Mention chunk empty, skipping');
		return;
	}
	const settingsByUserId = new Map<
		string,
		{
			suppressEveryone: boolean;
			suppressRoles: boolean;
		}
	>();
	if (guildId != null) {
		const mentionsNeedingSettings = mentions.filter((mention) => mention.everyone || mention.role);
		const resolvedSettings = await mapWithConcurrency(
			mentionsNeedingSettings,
			MENTION_SETTINGS_FETCH_CONCURRENCY,
			async (mention) => ({
				userId: mention.userId,
				settings: await userRepository.findGuildSettings(mention.userId, guildId),
			}),
		);
		for (const {userId, settings: userSettings} of resolvedSettings) {
			settingsByUserId.set(userId.toString(), {
				suppressEveryone: userSettings?.suppressEveryone ?? false,
				suppressRoles: userSettings?.suppressRoles ?? false,
			});
		}
	}
	const countedMentions = mentions.filter((mention) => {
		if (mention.direct) {
			return true;
		}
		const settings = settingsByUserId.get(mention.userId.toString());
		if (mention.everyone && !settings?.suppressEveryone) {
			return true;
		}
		if (mention.role && !settings?.suppressRoles) {
			return true;
		}
		return false;
	});
	if (countedMentions.length === 0) {
		Logger.debug(
			{
				channelId,
				messageId,
				guildId,
				chunkIndex: validated.chunkIndex,
				chunkCount: validated.chunkCount,
				suppressedMentionCount: mentions.length,
			},
			'Mention chunk fully suppressed by notification settings',
		);
		return;
	}
	await readStateService.bulkIncrementMentionCounts(
		countedMentions.map((mention) => ({userId: mention.userId, channelId, messageId})),
	);
	if (guildId != null) {
		await userRepository.createRecentMentions(
			countedMentions.map((mention) => {
				const settings = settingsByUserId.get(mention.userId.toString());
				return {
					user_id: mention.userId,
					channel_id: channelId,
					message_id: messageId,
					guild_id: guildId,
					is_everyone: mention.everyone && !settings?.suppressEveryone,
					is_role: mention.role && !settings?.suppressRoles,
				};
			}),
		);
	}
	Logger.debug(
		{
			channelId,
			messageId,
			guildId,
			chunkIndex: validated.chunkIndex,
			chunkCount: validated.chunkCount,
			totalMentioned: countedMentions.length,
			suppressedMentionCount: mentions.length - countedMentions.length,
		},
		'Handled mention chunk',
	);
};

export default handleMentionChunk;
