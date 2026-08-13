// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import * as GuildMemberCommands from '@app/features/member/commands/GuildMemberCommands';
import GuildMembers from '@app/features/member/state/GuildMembers';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {Message} from '@app/features/messaging/models/MessagingMessage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {FLUXERBOT_ID} from '@fluxer/constants/src/AppConstants';
import {MessageStates, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const USER_MENTION_REGEX = /<@!?(\d+)>/;
const logger = new Logger('CommandUtils');
const localCurrentUserNicknames = new Map<string, string | null>();
const UNKNOWN_USER_DESCRIPTOR = msg({
	message: 'Unknown user',
	comment: 'Fallback name in local command system messages when the current user is unavailable.',
});
const NICKNAME_CHANGED_DESCRIPTOR = msg({
	message: 'You changed your nickname in this community from **{previousNickname}** to **{newNickname}**.',
	comment:
		'Local system message after the user changes their community nickname with a slash command. The bold markdown markers should remain around the two names.',
});

export type ParsedCommand =
	| {
			type: 'nick';
			nickname: string;
	  }
	| {
			type: 'kick';
			userId: string;
			reason?: string;
	  }
	| {
			type: 'ban';
			userId: string;
			deleteMessageDays: number;
			duration: number;
			reason?: string;
	  }
	| {
			type: 'msg';
			userId: string;
			message: string;
	  }
	| {
			type: 'me';
			content: string;
	  }
	| {
			type: 'spoiler';
			content: string;
	  }
	| {
			type: 'tts';
			content: string;
	  }
	| {
			type: 'saved' | 'sticker' | 'gif';
			query: string;
	  }
	| {
			type: 'unknown';
	  };

export function parseCommand(content: string): ParsedCommand {
	const trimmed = content.trim();
	if (trimmed === '/nick') {
		return {type: 'nick', nickname: ''};
	}
	if (trimmed.startsWith('/nick ')) {
		const nickname = trimmed.slice(6).trim();
		return {type: 'nick', nickname};
	}
	if (trimmed.startsWith('/kick ')) {
		const rest = trimmed.slice(6).trim();
		const userMatch = rest.match(USER_MENTION_REGEX);
		if (!userMatch) {
			return {type: 'unknown'};
		}
		const userId = userMatch[1];
		const afterMention = rest.slice(userMatch[0].length).trim();
		const reason = afterMention || undefined;
		return {type: 'kick', userId, reason};
	}
	if (trimmed.startsWith('/ban ')) {
		const rest = trimmed.slice(5).trim();
		const userMatch = rest.match(USER_MENTION_REGEX);
		if (!userMatch) {
			return {type: 'unknown'};
		}
		const userId = userMatch[1];
		const afterMention = rest.slice(userMatch[0].length).trim();
		const parts = afterMention.length === 0 ? [] : afterMention.split(/\s+/);
		let deleteMessageDays = 1;
		let reasonStart = 0;
		const firstPart = parts[0];
		if (firstPart !== undefined && /^[0-7]$/.test(firstPart)) {
			deleteMessageDays = Number(firstPart);
			reasonStart = 1;
		} else if (firstPart !== undefined && /^\d+$/.test(firstPart)) {
			return {type: 'unknown'};
		}
		const duration = 0;
		const reasonParts = parts.slice(reasonStart);
		const reasonText = reasonParts.join(' ').trim();
		const reason = reasonText || undefined;
		return {type: 'ban', userId, deleteMessageDays, duration, reason};
	}
	if (trimmed.startsWith('/msg ')) {
		const rest = trimmed.slice(5).trim();
		const userMatch = rest.match(USER_MENTION_REGEX);
		if (!userMatch) {
			return {type: 'unknown'};
		}
		const userId = userMatch[1];
		const message = rest.slice(userMatch[0].length).trim();
		if (!message) {
			return {type: 'unknown'};
		}
		return {type: 'msg', userId, message};
	}
	if (trimmed.startsWith('/me ')) {
		const content = trimmed.slice(4).trim();
		if (!content) {
			return {type: 'unknown'};
		}
		return {type: 'me', content};
	}
	if (trimmed.startsWith('/spoiler ')) {
		const content = trimmed.slice(9).trim();
		if (!content) {
			return {type: 'unknown'};
		}
		return {type: 'spoiler', content};
	}
	if (trimmed.startsWith('/tts ')) {
		const content = trimmed.slice(5).trim();
		if (!content) {
			return {type: 'unknown'};
		}
		return {type: 'tts', content};
	}
	for (const type of ['saved', 'sticker', 'gif'] as const) {
		const prefix = `/${type}`;
		if (trimmed === prefix || trimmed.startsWith(`${prefix} `)) {
			const query = trimmed.slice(prefix.length).trim();
			return {type, query};
		}
	}
	return {type: 'unknown'};
}

export function transformWrappingCommands(content: string): string {
	const trimmed = content.trim();
	if (trimmed.startsWith('/me ')) {
		const messageContent = trimmed.slice(4).trim();
		if (messageContent) {
			return `_${messageContent}_`;
		}
	}
	if (trimmed.startsWith('/spoiler ')) {
		const messageContent = trimmed.slice(9).trim();
		if (messageContent) {
			return `||${messageContent}||`;
		}
	}
	return content;
}

export function isCommand(content: string): boolean {
	const trimmed = content.trim();
	return (
		trimmed === '/nick' ||
		trimmed.startsWith('/nick ') ||
		trimmed.startsWith('/kick ') ||
		trimmed.startsWith('/ban ') ||
		trimmed.startsWith('/msg ') ||
		trimmed.startsWith('/me ') ||
		trimmed.startsWith('/spoiler ') ||
		trimmed.startsWith('/tts ') ||
		trimmed === '/saved' ||
		trimmed.startsWith('/saved ') ||
		trimmed === '/sticker' ||
		trimmed.startsWith('/sticker ') ||
		trimmed === '/gif' ||
		trimmed.startsWith('/gif ') ||
		(trimmed.startsWith('_') && trimmed.endsWith('_') && trimmed.length > 2)
	);
}

export function doesCommandSendCurrentChannelMessage(command: ParsedCommand): boolean {
	return command.type === 'me' || command.type === 'spoiler' || command.type === 'tts' || command.type === 'unknown';
}

export function createSystemMessage(channelId: string, content: string): Message {
	const fluxerbotUser = new User({
		id: FLUXERBOT_ID,
		username: 'Fluxerbot',
		discriminator: '0000',
		global_name: null,
		avatar: null,
		avatar_color: null,
		bot: true,
		system: true,
		flags: 0,
	});
	const nonce = SnowflakeUtils.fromTimestamp(Date.now());
	return new Message({
		id: nonce,
		channel_id: channelId,
		author: fluxerbotUser.toJSON(),
		type: MessageTypes.CLIENT_SYSTEM,
		flags: 0,
		pinned: false,
		mention_everyone: false,
		content,
		timestamp: new Date().toISOString(),
		state: MessageStates.SENT,
		nonce,
		attachments: [],
	});
}

function getCurrentUserNicknameLabels(guildId: string, i18n: I18n): {previous: string; fallback: string} {
	const currentUserId = Authentication.currentUserId;
	const currentMember = GuildMembers.getMember(guildId, currentUserId);
	const currentUser = Users.getCurrentUser();
	const fallback = currentUser
		? NicknameUtils.getDisplayName(currentUser)
		: currentMember
			? NicknameUtils.getDisplayName(currentMember.user)
			: i18n._(UNKNOWN_USER_DESCRIPTOR);
	const memberNickname = currentMember?.nick?.trim();
	if (!currentMember && localCurrentUserNicknames.has(guildId)) {
		const localNickname = localCurrentUserNicknames.get(guildId)?.trim();
		return {
			previous: localNickname || fallback,
			fallback,
		};
	}
	return {
		previous: memberNickname || fallback,
		fallback,
	};
}

function updateCurrentUserNickname(guildId: string, nickname: string | null): void {
	const currentUserId = Authentication.currentUserId;
	const currentMember = GuildMembers.getMember(guildId, currentUserId);
	if (!currentMember) {
		localCurrentUserNicknames.set(guildId, nickname);
		return;
	}
	localCurrentUserNicknames.delete(guildId);
	GuildMembers.handleMemberAdd(guildId, {
		...currentMember.toJSON(),
		nick: nickname,
	});
}

export async function executeCommand(
	command: ParsedCommand,
	channelId: string,
	guildId: string | undefined,
	i18n: I18n,
): Promise<void> {
	switch (command.type) {
		case 'nick': {
			if (!guildId) {
				throw new Error('Cannot change nickname outside of a guild');
			}
			const labels = getCurrentUserNicknameLabels(guildId, i18n);
			const prevNickname = labels.previous;
			const newNickname = command.nickname || labels.fallback;
			await GuildMemberCommands.updateProfile(guildId, {
				nick: command.nickname || null,
			});
			const systemMessage = createSystemMessage(
				channelId,
				i18n._(NICKNAME_CHANGED_DESCRIPTOR, {
					previousNickname: prevNickname,
					newNickname,
				}),
			);
			updateCurrentUserNickname(guildId, command.nickname || null);
			MessageCommands.createOptimistic(channelId, systemMessage.toJSON());
			break;
		}
		case 'kick': {
			if (!guildId) {
				throw new Error('Cannot kick members outside of a guild');
			}
			await GuildMemberCommands.kick(guildId, command.userId);
			break;
		}
		case 'ban': {
			if (!guildId) {
				throw new Error('Cannot ban members outside of a guild');
			}
			await GuildCommands.banMember(
				guildId,
				command.userId,
				command.deleteMessageDays,
				command.reason,
				command.duration,
			);
			break;
		}
		case 'msg': {
			let dmChannelId: string;
			try {
				dmChannelId = await PrivateChannelCommands.ensureDMChannel(command.userId);
			} catch {
				const user = Users.getUser(command.userId);
				const username = user ? NicknameUtils.getDisplayName(user) : command.userId;
				const systemMessage = createSystemMessage(
					channelId,
					`Failed to send a message to **${username}**. They may have DMs disabled or you may be blocked.`,
				);
				MessageCommands.createOptimistic(channelId, systemMessage.toJSON());
				break;
			}
			try {
				const result = await MessageCommands.send(dmChannelId, {
					content: command.message,
					nonce: SnowflakeUtils.fromTimestamp(Date.now()),
					hasAttachments: false,
					flags: 0,
				});
				if (result) {
					await PrivateChannelCommands.openDMChannel(command.userId);
				}
			} catch (error) {
				logger.error('Failed to dispatch /msg command DM', error);
			}
			break;
		}
		case 'me': {
			break;
		}
		case 'spoiler': {
			break;
		}
		case 'saved':
		case 'sticker':
		case 'gif': {
			throw new Error(`Select a ${command.type} result before submitting the command`);
		}
		default:
			break;
	}
}
