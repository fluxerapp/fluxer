// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelID, createChannelID, createMessageID, type GuildID, type UserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {EvidenceChannelKind} from '@app/api/database/types/EnforcementTypes';
import type {MessageAttachment} from '@app/api/database/types/MessageTypes';
import type {IARMessageContextRow} from '@app/api/database/types/ReportTypes';
import type {EvidenceMessageLocation} from '@app/api/enforcement/EnforcementNotice';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';
import {type IARMessageContext, type IARSubmission, ReportType} from '@app/api/report/ReportModels';
import {ChannelTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';

function isAuthoredContent(type: number): boolean {
	return type === MessageTypes.DEFAULT || type === MessageTypes.REPLY;
}

function isConversation(type: number): boolean {
	return type === ChannelTypes.DM || type === ChannelTypes.GROUP_DM;
}

function coversConversation(report: IARSubmission, channel: Channel): boolean {
	if (report.reportedChannelId === channel.id) return true;
	return (
		report.reportType === ReportType.USER &&
		channel.type === ChannelTypes.DM &&
		report.reporterId !== null &&
		report.reportedUserId !== null &&
		channel.recipientIds.has(report.reporterId) &&
		channel.recipientIds.has(report.reportedUserId)
	);
}

function attachmentMetadata(attachment: MessageAttachment): MessageAttachment {
	return {
		attachment_id: attachment.attachment_id,
		filename: attachment.filename,
		size: attachment.size,
		title: null,
		description: null,
		width: null,
		height: null,
		content_type: attachment.content_type,
		content_hash: null,
		placeholder: null,
		flags: 0,
		duration: null,
		nsfw: null,
		waveform: null,
	};
}

function orNull<T>(items: Array<T>): Array<T> | null {
	return items.length > 0 ? items : null;
}

function fromReportContext(entry: IARMessageContext, report: IARSubmission): IARMessageContextRow {
	return {
		message_id: entry.messageId,
		channel_id: entry.channelId ?? report.reportedChannelId,
		author_id: entry.authorId,
		webhook_id: entry.webhookId,
		author_username: entry.authorUsername,
		author_discriminator: entry.authorDiscriminator,
		author_avatar_hash: entry.authorAvatarHash,
		content: entry.content,
		timestamp: entry.timestamp,
		edited_timestamp: entry.editedTimestamp,
		type: entry.type,
		flags: entry.flags,
		mention_everyone: entry.mentionEveryone,
		mention_users: orNull(entry.mentionUsers),
		mention_roles: orNull(entry.mentionRoles),
		mention_channels: orNull(entry.mentionChannels),
		attachments: null,
		embeds: orNull(entry.embeds),
		sticker_items: orNull(entry.stickers),
		missing_attachments: orNull([...entry.attachments, ...entry.missingAttachments].map(attachmentMetadata)),
	};
}

function fromLiveMessage(message: Message, author: {username: string; discriminator: number}): IARMessageContextRow {
	return {
		message_id: message.id,
		channel_id: message.channelId,
		author_id: message.authorId,
		webhook_id: message.webhookId,
		author_username: author.username,
		author_discriminator: author.discriminator,
		author_avatar_hash: null,
		content: message.content || null,
		timestamp: snowflakeToDate(message.id),
		edited_timestamp: message.editedTimestamp,
		type: message.type,
		flags: message.flags,
		mention_everyone: message.mentionEveryone,
		mention_users: orNull([...message.mentionedUserIds]),
		mention_roles: orNull([...message.mentionedRoleIds]),
		mention_channels: orNull([...message.mentionedChannelIds]),
		attachments: null,
		embeds: orNull(message.embeds.map((embed) => embed.toMessageEmbed())),
		sticker_items: orNull(message.stickers.map((sticker) => sticker.toMessageStickerItem())),
		missing_attachments: orNull(
			message.attachments.map((attachment) => attachmentMetadata(attachment.toMessageAttachment())),
		),
	};
}

export function restrictedEvidence(message: IARMessageContextRow): IARMessageContextRow {
	return {
		message_id: message.message_id,
		channel_id: message.channel_id,
		author_id: null,
		webhook_id: null,
		author_username: '',
		author_discriminator: 0,
		author_avatar_hash: null,
		content: null,
		timestamp: message.timestamp,
		edited_timestamp: null,
		type: 0,
		flags: 0,
		mention_everyone: false,
		mention_users: null,
		mention_roles: null,
		mention_channels: null,
		attachments: null,
		embeds: null,
		sticker_items: null,
		missing_attachments: orNull(
			(message.missing_attachments ?? []).map((attachment) => ({
				...attachmentMetadata(attachment),
				filename: '',
				size: 0n,
				content_type: '',
			})),
		),
	};
}

export type EvidenceSubject =
	| {type: 'user'; id: UserID; username: string; discriminator: number}
	| {type: 'guild'; id: GuildID};

export interface MessageReference {
	channel_id: bigint;
	message_id: bigint;
}

interface CollectEvidenceParams {
	subject: EvidenceSubject;
	messages: ReadonlyArray<MessageReference>;
	path: string;
	reports: ReadonlyArray<IARSubmission>;
	channelRepository: ChannelRepository;
}

const UNKNOWN_AUTHOR = {username: '', discriminator: 0};

export async function collectEvidence({
	subject,
	messages,
	path,
	reports,
	channelRepository,
}: CollectEvidenceParams): Promise<Array<IARMessageContextRow>> {
	const channels = new Map<bigint, Channel | null>();
	const findChannel = async (channelId: bigint): Promise<Channel | null> => {
		if (!channels.has(channelId)) {
			channels.set(channelId, await channelRepository.channelData.findUnique(createChannelID(channelId)));
		}
		return channels.get(channelId) ?? null;
	};
	const belongsToSubject = async (authorId: bigint | null, channelId: bigint | null): Promise<boolean> => {
		if (subject.type === 'user') return authorId === subject.id;
		return channelId !== null && (await findChannel(channelId))?.guildId === subject.id;
	};
	const reported = reports.flatMap((report) =>
		(report.messageContext ?? []).map((entry) => fromReportContext(entry, report)),
	);
	const collected = new Map<bigint, IARMessageContextRow>();
	for (const [index, entry] of messages.entries()) {
		const entryPath = `${path}.${index}`;
		const fromReport = reported.find(
			(message) => message.message_id === entry.message_id && message.channel_id === entry.channel_id,
		);
		if (fromReport) {
			if (!(await belongsToSubject(fromReport.author_id, fromReport.channel_id))) {
				throw InputValidationError.fromCode(entryPath, ValidationErrorCodes.ENFORCEMENT_EVIDENCE_NOT_SUBJECT);
			}
			collected.set(fromReport.message_id, fromReport);
			continue;
		}
		const channelId = createChannelID(entry.channel_id);
		const message = await channelRepository.messages.getMessage(channelId, createMessageID(entry.message_id));
		if (!message) {
			throw InputValidationError.fromCode(entryPath, ValidationErrorCodes.ENFORCEMENT_EVIDENCE_NOT_FOUND);
		}
		if (!(await belongsToSubject(message.authorId, channelId))) {
			throw InputValidationError.fromCode(entryPath, ValidationErrorCodes.ENFORCEMENT_EVIDENCE_NOT_SUBJECT);
		}
		const channel = await findChannel(channelId);
		if (channel && isConversation(channel.type) && !reports.some((report) => coversConversation(report, channel))) {
			throw InputValidationError.fromCode(entryPath, ValidationErrorCodes.ENFORCEMENT_EVIDENCE_DM_UNREPORTED);
		}
		collected.set(message.id, fromLiveMessage(message, subject.type === 'user' ? subject : UNKNOWN_AUTHOR));
	}
	for (const message of reported) {
		if (
			isAuthoredContent(message.type) &&
			!collected.has(message.message_id) &&
			(await belongsToSubject(message.author_id, message.channel_id))
		) {
			collected.set(message.message_id, message);
		}
	}
	return [...collected.values()];
}

function channelKindOf(channel: Channel): EvidenceChannelKind {
	if (channel.type === ChannelTypes.DM) return 'direct_message';
	if (channel.type === ChannelTypes.GROUP_DM) return 'group_message';
	return 'channel';
}

function channelIdsOf(messages: ReadonlyArray<IARMessageContextRow>): Set<ChannelID> {
	return new Set(messages.flatMap((message) => (message.channel_id ? [message.channel_id as ChannelID] : [])));
}

export async function snapshotEvidenceChannelKinds(
	messages: ReadonlyArray<IARMessageContextRow>,
	channelRepository: ChannelRepository,
): Promise<Map<bigint, EvidenceChannelKind>> {
	const kinds = new Map<bigint, EvidenceChannelKind>();
	for (const channelId of channelIdsOf(messages)) {
		const channel = await channelRepository.channelData.findUnique(channelId);
		if (channel) kinds.set(channelId, channelKindOf(channel));
	}
	return new Map(
		messages.flatMap((message) => {
			const kind = message.channel_id ? kinds.get(message.channel_id) : undefined;
			return kind ? [[message.message_id, kind] as const] : [];
		}),
	);
}

export async function resolveEvidenceLocations(
	messages: ReadonlyArray<IARMessageContextRow>,
	recordedKinds: ReadonlyMap<bigint, EvidenceChannelKind> | null,
	channelRepository: ChannelRepository,
	guildRepository: GuildRepository,
): Promise<Array<EvidenceMessageLocation>> {
	const channels = new Map<string, EvidenceMessageLocation['location']>();
	for (const channelId of channelIdsOf(messages)) {
		const channel = await channelRepository.channelData.findUnique(channelId);
		if (!channel) continue;
		const kind = channelKindOf(channel);
		if (kind !== 'channel') {
			channels.set(channelId.toString(), {type: kind});
		} else if (channel.guildId) {
			const guild = await guildRepository.findUnique(channel.guildId);
			if (guild) {
				channels.set(channelId.toString(), {
					type: 'channel',
					channelName: channel.name ?? '',
					communityName: guild.name,
				});
			}
		}
	}
	return messages.map((message) => {
		const locationKey = message.channel_id?.toString() ?? 'none';
		const recorded = recordedKinds?.get(message.message_id);
		const location =
			recorded && recorded !== 'channel' ? {type: recorded} : (channels.get(locationKey) ?? {type: 'unknown' as const});
		return {locationKey, location, timestamp: message.timestamp};
	});
}
