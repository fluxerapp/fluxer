// SPDX-License-Identifier: AGPL-3.0-or-later

import {createInviteCode, type GuildID, type UserID} from '@app/api/BrandedTypes';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import type {AttachmentMeta} from '@app/api/infrastructure/activity/Contract.generated';
import {Logger} from '@app/api/Logger';
import {getGuildRepository, getInviteRepository} from '@app/api/middleware/ServiceSingletons';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';
import type {User} from '@app/api/models/User';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {findInvites} from '@app/api/utils/InviteUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';

const CONTENT_MAX_CHARS = 2000;
const LIST_MAX = 10;
const MENTIONS_MAX = 20;
const LINK_PATTERN = /https?:\/\/([^\s/?#<>"']+)/giu;

function linkDomains(content: string): Array<string> {
	const domains = new Set<string>();
	for (const match of content.matchAll(LINK_PATTERN)) {
		const host = match[1]
			?.toLowerCase()
			.replace(/:\d+$/u, '')
			.replace(/^www\./u, '');
		if (host) domains.add(host);
		if (domains.size >= LIST_MAX) break;
	}
	return [...domains];
}

interface InviteTarget {
	guildId: string | null;
	ownerId: string | null;
}

async function inviteTarget(code: string): Promise<InviteTarget> {
	try {
		const invite = await getInviteRepository().findUnique(createInviteCode(code));
		if (!invite?.guildId) return {guildId: null, ownerId: null};
		const guild = await getGuildRepository().findUnique(invite.guildId);
		return {guildId: invite.guildId.toString(), ownerId: guild ? guild.ownerId.toString() : null};
	} catch {
		return {guildId: null, ownerId: null};
	}
}

function inviteTargets(codes: ReadonlyArray<string>): Promise<Array<InviteTarget>> {
	return Promise.all(codes.map(inviteTarget));
}

async function guildMemberCount(guildId: GuildID): Promise<number | null> {
	try {
		const guild = await getGuildRepository().findUnique(guildId);
		return guild ? guild.memberCount : null;
	} catch {
		return null;
	}
}

export interface MessageCreatedActivity {
	user: User;
	message: Message;
	channel: Channel;
	guildId: GuildID | null;
	guildOwnerId: UserID | null;
	dmRecipientId: UserID | null;
	channelHadMessages: boolean;
	delivered: boolean;
	userRepository: Pick<IUserRepository, 'getRelationship'>;
}

function attachmentMeta(message: Message): Array<AttachmentMeta> {
	return message.attachments.slice(0, LIST_MAX).map((attachment) => ({
		size: Number(attachment.size),
		content_type: attachment.contentType || null,
		hash: attachment.contentHash ? attachment.contentHash.toLowerCase() : null,
	}));
}

async function buildAndEmit(
	kind: 'message_created' | 'message_updated',
	params: MessageCreatedActivity,
): Promise<void> {
	const {user, message, channel, guildId, dmRecipientId} = params;
	const content = Array.from(message.content ?? '')
		.slice(0, CONTENT_MAX_CHARS)
		.join('');
	const inviteCodes = findInvites(message.content, LIST_MAX);
	const domains = linkDomains(message.content ?? '');
	const [targets, memberCount] = await Promise.all([
		inviteTargets(inviteCodes),
		guildId && (domains.length > 0 || inviteCodes.length > 0) ? guildMemberCount(guildId) : Promise.resolve(null),
	]);
	const mentionedIds = [...message.mentionedUserIds];
	const recipientIsFriend = dmRecipientId
		? (await params.userRepository.getRelationship(user.id, dmRecipientId, RelationshipTypes.FRIEND)) !== null
		: false;
	await emitActivity(
		kind,
		user.id.toString(),
		{
			user_id: user.id.toString(),
			message_id: message.id.toString(),
			channel_id: channel.id.toString(),
			channel_type: guildId ? 'guild' : channel.type === ChannelTypes.GROUP_DM ? 'group_dm' : 'dm',
			guild_id: guildId ? guildId.toString() : null,
			dm_recipient_id: dmRecipientId ? dmRecipientId.toString() : null,
			recipient_is_friend: recipientIsFriend,
			channel_prior_messages: params.channelHadMessages,
			is_bot: user.isBot,
			content,
			attachment_count: message.attachments.length,
			attachment_names: message.attachments.slice(0, LIST_MAX).map((attachment) => attachment.filename),
			attachments: attachmentMeta(message),
			link_domains: domains,
			invite_codes: inviteCodes,
			invite_guild_ids: targets.map((target) => target.guildId),
			invite_guild_owner_ids: targets.map((target) => target.ownerId),
			mention_user_ids: mentionedIds.slice(0, MENTIONS_MAX).map(String),
			mentions_recipient: dmRecipientId !== null && mentionedIds.some((id) => id === dmRecipientId),
			mention_everyone: message.mentionEveryone,
			author_owns_guild: params.guildOwnerId !== null && params.guildOwnerId === user.id,
			guild_member_count: memberCount,
			delivered: params.delivered,
		},
		null,
		kind === 'message_created'
			? message.id.toString()
			: `${message.id}:${message.editedTimestamp?.getTime() ?? Date.now()}`,
	);
}

export function emitMessageCreated(params: MessageCreatedActivity): void {
	void buildAndEmit('message_created', params).catch((error: unknown) => {
		Logger.debug({error}, 'Message activity event could not be built');
	});
}

export function emitMessageUpdated(params: MessageCreatedActivity): void {
	void buildAndEmit('message_updated', params).catch((error: unknown) => {
		Logger.debug({error}, 'Message activity event could not be built');
	});
}
