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

import {createRoleID, createUserID, type GuildID, type RoleID, type UserID} from '~/BrandedTypes';
import {
	ChannelTypes,
	GuildOperations,
	MessageTypes,
	ROLE_MENTION_REGEX,
	SENDABLE_MESSAGE_FLAGS,
	USER_MENTION_REGEX,
} from '~/Constants';
import {ALLOWED_MENTIONS_PARSE, type AllowedMentionsRequest} from '~/channel/ChannelModel';
import {InputValidationError} from '~/Errors';
import type {GuildResponse} from '~/guild/GuildModel';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {Channel, Message} from '~/Models';
import type {IUserRepository} from '~/user/IUserRepository';
import type {IWorkerService} from '~/worker/IWorkerService';
import {cleanTextForMentions, isOperationDisabled, isPersonalNotesChannel} from './MessageHelpers';

interface MentionData {
	userMentions: Set<UserID>;
	roleMentions: Set<RoleID>;
	flags: number;
	mentionsEveryone: boolean;
	mentionsHere: boolean;
}

export class MessageMentionService {
	constructor(
		private userRepository: IUserRepository,
		private guildRepository: IGuildRepository,
		private workerService: IWorkerService,
	) {}

	extractMentions({
		content,
		referencedMessage,
		message,
		channelType,
		allowedMentions,
		guild,
		canMentionEveryone = true,
	}: {
		content: string;
		referencedMessage: Message | null;
		message: Message;
		channelType: number;
		allowedMentions: AllowedMentionsRequest | null;
		guild?: GuildResponse | null;
		canMentionEveryone?: boolean;
	}): MentionData {
		const cleanText = cleanTextForMentions(content);
		let mentionsEveryone = cleanText.includes('@everyone');
		let mentionsHere = cleanText.includes('@here');

		if (guild && isOperationDisabled(guild, GuildOperations.EVERYONE_MENTIONS)) {
			mentionsEveryone = false;
			mentionsHere = false;
		}

		const userMentions = new Set(
			[...content.matchAll(USER_MENTION_REGEX)]
				.map((m) => createUserID(BigInt(m.groups?.userId ?? '0')))
				.filter((id) => id !== 0n),
		);

		const roleMentions = new Set(
			[...content.matchAll(ROLE_MENTION_REGEX)]
				.map((m) => createRoleID(BigInt(m.groups?.roleId ?? '0')))
				.filter((id) => id !== createRoleID(0n)),
		);

		const isDMChannel = channelType === ChannelTypes.DM || channelType === ChannelTypes.DM_PERSONAL_NOTES;
		const shouldAddReferencedUser =
			referencedMessage?.authorId &&
			referencedMessage.authorId !== message.authorId &&
			!isDMChannel &&
			(!allowedMentions || allowedMentions.replied_user !== false);

		if (shouldAddReferencedUser) {
			userMentions.add(referencedMessage!.authorId!);
		}

		const sendableFlags = message.flags & SENDABLE_MESSAGE_FLAGS;
		let flags = message.flags & ~SENDABLE_MESSAGE_FLAGS;

		if (allowedMentions) {
			const result = this.applyAllowedMentions({
				allowedMentions,
				userMentions,
				roleMentions,
				mentionsEveryone,
				mentionsHere,
				flags,
				referencedMessage,
			});
			flags = result.flags;
			mentionsEveryone = result.mentionsEveryone;
			mentionsHere = result.mentionsHere;
		}

		if (!canMentionEveryone && (mentionsEveryone || mentionsHere)) {
			mentionsEveryone = false;
			mentionsHere = false;
		}

		return {userMentions, roleMentions, flags: flags | sendableFlags, mentionsEveryone, mentionsHere};
	}

	private applyAllowedMentions({
		allowedMentions,
		userMentions,
		roleMentions,
		mentionsEveryone,
		mentionsHere,
		flags,
		referencedMessage,
	}: {
		allowedMentions: AllowedMentionsRequest;
		userMentions: Set<UserID>;
		roleMentions: Set<RoleID>;
		mentionsEveryone: boolean;
		mentionsHere: boolean;
		flags: number;
		referencedMessage?: Message | null;
	}): {flags: number; mentionsEveryone: boolean; mentionsHere: boolean} {
		const parse = allowedMentions.parse ?? ALLOWED_MENTIONS_PARSE;
		const users = allowedMentions.users ?? [];
		const roles = allowedMentions.roles ?? [];
		const hasExplicitParse = allowedMentions.parse !== undefined;

		if (hasExplicitParse && parse.length > 0 && (users.length > 0 || roles.length > 0)) {
			throw InputValidationError.create(
				'allowed_mentions',
				'"parse" and ("users" or "roles") cannot be used together.',
			);
		}

		const repliedUserId = referencedMessage?.authorId;
		const shouldPreserveRepliedUser = repliedUserId && allowedMentions.replied_user !== false;
		let preservedRepliedUser = null;

		if (shouldPreserveRepliedUser && userMentions.has(repliedUserId)) {
			preservedRepliedUser = repliedUserId;
			userMentions.delete(repliedUserId);
		}

		this.filterMentions({
			mentions: userMentions,
			allowedList: users.map(createUserID),
			shouldParse: parse.includes('users'),
		});

		this.filterMentions({
			mentions: roleMentions,
			allowedList: roles.map(createRoleID),
			shouldParse: parse.includes('roles'),
		});

		if (preservedRepliedUser) {
			userMentions.add(preservedRepliedUser);
		}

		const preserveEveryone = parse.includes('everyone');
		return {
			flags,
			mentionsEveryone: preserveEveryone && mentionsEveryone,
			mentionsHere: preserveEveryone && mentionsHere,
		};
	}

	private filterMentions<T extends UserID | RoleID>({
		mentions,
		allowedList,
		shouldParse,
	}: {
		mentions: Set<T>;
		allowedList: Array<T>;
		shouldParse: boolean;
	}): void {
		const shouldClear = shouldParse ? allowedList.length === 0 : allowedList.length > 0;

		if (shouldClear) {
			if (allowedList.length === 0) {
				mentions.clear();
			} else {
				for (const id of Array.from(mentions)) {
					if (!allowedList.includes(id)) {
						mentions.delete(id);
					}
				}
			}
		}
	}

	async validateMentions({
		userMentions,
		roleMentions,
		channel,
		canMentionRoles = true,
	}: {
		userMentions: Set<UserID>;
		roleMentions: Set<RoleID>;
		channel: Channel;
		canMentionRoles?: boolean;
	}): Promise<{validUserIds: Array<UserID>; validRoleIds: Array<RoleID>}> {
		if (channel.guildId) {
			const [users, roles] = await Promise.all([
				this.userRepository.listUsers(Array.from(userMentions)),
				this.guildRepository.listRolesByIds(Array.from(roleMentions), channel.guildId),
			]);

			const filteredRoles = canMentionRoles ? roles : roles.filter((role) => role.isMentionable);
			return {
				validUserIds: users.map((u) => u.id),
				validRoleIds: filteredRoles.map((r) => r.id),
			};
		}

		const recipients = Array.from(channel.recipientIds || []);
		const validUserIds = recipients.filter((r) => userMentions.has(r));

		return {validUserIds, validRoleIds: []};
	}

	async handleMentionTasks(params: {
		guildId: GuildID | null;
		message: Message;
		authorId: UserID;
		mentionHere?: boolean;
	}): Promise<void> {
		const {guildId, message, authorId, mentionHere = false} = params;

		if (isPersonalNotesChannel({userId: authorId, channelId: message.channelId})) return;

		const taskData = {
			guildId: guildId?.toString(),
			channelId: message.channelId.toString(),
			messageId: message.id.toString(),
			authorId: authorId.toString(),
			mentionHere,
		};

		const hasMentions =
			message.mentionedUserIds?.size > 0 ||
			message.mentionedRoleIds?.size > 0 ||
			message.mentionEveryone ||
			(message.reference && message.type === MessageTypes.REPLY);

		if (hasMentions) {
			await this.workerService.addJob('handleMentions', taskData);
		}
	}
}
