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

import * as GuildActionCreators from '~/actions/GuildActionCreators';
import * as GuildMemberActionCreators from '~/actions/GuildMemberActionCreators';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import {FLUXERBOT_ID, MessageStates, MessageTypes} from '~/Constants';
import {MessageRecord} from '~/records/MessageRecord';
import {UserRecord} from '~/records/UserRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import UserStore from '~/stores/UserStore';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';

const USER_MENTION_REGEX = /<@!?(\d+)>/;

type ParsedCommand =
	| {type: 'nick'; nickname: string}
	| {type: 'kick'; userId: string; reason?: string}
	| {type: 'ban'; userId: string; deleteMessageDays: number; duration: number; reason?: string}
	| {type: 'msg'; userId: string; message: string}
	| {type: 'me'; content: string}
	| {type: 'spoiler'; content: string}
	| {type: 'tts'; content: string}
	| {type: 'unknown'};

export function parseCommand(content: string): ParsedCommand {
	const trimmed = content.trim();

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

		// TODO: Parse these from the command
		const deleteMessageDays = 1;
		const duration = 0;
		const reason = afterMention || undefined;

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
		trimmed.startsWith('/nick ') ||
		trimmed.startsWith('/kick ') ||
		trimmed.startsWith('/ban ') ||
		trimmed.startsWith('/msg ') ||
		trimmed.startsWith('/me ') ||
		trimmed.startsWith('/spoiler ') ||
		trimmed.startsWith('/tts ') ||
		(trimmed.startsWith('_') && trimmed.endsWith('_') && trimmed.length > 2)
	);
}

export function createSystemMessage(channelId: string, content: string): MessageRecord {
	const fluxerbotUser = new UserRecord({
		id: FLUXERBOT_ID,
		username: 'Fluxerbot',
		discriminator: '0000',
		avatar: null,
		bot: true,
		system: true,
		flags: 0,
	});

	const nonce = SnowflakeUtils.fromTimestamp(Date.now());

	return new MessageRecord({
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

export async function executeCommand(command: ParsedCommand, channelId: string, guildId?: string): Promise<void> {
	const currentUserId = AuthenticationStore.currentUserId;

	switch (command.type) {
		case 'nick': {
			if (!guildId) {
				throw new Error('Cannot change nickname outside of a guild');
			}

			const currentMember = GuildMemberStore.getMember(guildId, currentUserId);
			const prevNickname = currentMember?.nick || UserStore.getCurrentUser()?.username || 'Unknown';
			const newNickname = command.nickname || UserStore.getCurrentUser()?.username || 'Unknown';

			await GuildMemberActionCreators.updateProfile(guildId, {
				nick: command.nickname || null,
			});

			const systemMessage = createSystemMessage(
				channelId,
				`You changed your nickname in this community from **${prevNickname}** to **${newNickname}**.`,
			);

			MessageActionCreators.createOptimistic(channelId, systemMessage.toJSON());
			break;
		}

		case 'kick': {
			if (!guildId) {
				throw new Error('Cannot kick members outside of a guild');
			}

			await GuildMemberActionCreators.kick(guildId, command.userId);
			break;
		}

		case 'ban': {
			if (!guildId) {
				throw new Error('Cannot ban members outside of a guild');
			}

			await GuildActionCreators.banMember(
				guildId,
				command.userId,
				command.deleteMessageDays,
				command.reason,
				command.duration,
			);
			break;
		}

		case 'msg': {
			try {
				const dmChannelId = await PrivateChannelActionCreators.ensureDMChannel(command.userId);

				await MessageActionCreators.send(dmChannelId, {
					content: command.message,
					nonce: SnowflakeUtils.fromTimestamp(Date.now()),
					hasAttachments: false,
					flags: 0,
				});

				await PrivateChannelActionCreators.openDMChannel(command.userId);
			} catch (_error) {
				const user = UserStore.getUser(command.userId);
				const username = user?.username || 'user';

				const systemMessage = createSystemMessage(
					channelId,
					`Failed to send a message to **${username}**. They may have DMs disabled or you may be blocked.`,
				);

				MessageActionCreators.createOptimistic(channelId, systemMessage.toJSON());
			}
			break;
		}

		case 'me': {
			break;
		}

		case 'spoiler': {
			break;
		}

		default:
			break;
	}
}
