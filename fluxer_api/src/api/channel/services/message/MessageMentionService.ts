// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes, MessageTypes, SENDABLE_MESSAGE_FLAGS} from '@fluxer/constants/src/ChannelConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {RelationshipTypes, UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {
	ALLOWED_MENTIONS_PARSE,
	type AllowedMentionsRequest,
} from '@fluxer/schema/src/domains/message/SharedMessageSchemas';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {
	type ChannelID,
	createChannelID,
	createRoleID,
	createUserID,
	type GuildID,
	type RoleID,
	type UserID,
} from '../../../BrandedTypes';
import type {IGuildRepositoryAggregate} from '../../../guild/repositories/IGuildRepositoryAggregate';
import type {GatewayChannelMention, IGatewayService} from '../../../infrastructure/IGatewayService';
import {Logger} from '../../../Logger';
import type {Channel} from '../../../models/Channel';
import type {Message} from '../../../models/Message';
import type {IUserRepository} from '../../../user/IUserRepository';
import type {WorkerTaskName} from '../../../worker/WorkerLaneConfig';
import {WorkerQueueOverflowError} from '../../../worker/WorkerQueueOverflowError';
import {isOperationDisabled, isPersonalNotesChannel} from './MessageHelpers';
import type {MessageResponseDataService} from './MessageResponseDataService';

interface MentionData {
	userMentions: Set<UserID>;
	roleMentions: Set<RoleID>;
	channelMentions: Set<ChannelID>;
	flags: number;
	mentionsEveryone: boolean;
	mentionsHere: boolean;
}

interface MentionReadRepairData {
	userMentions: Set<UserID>;
	roleMentions: Set<RoleID>;
	channelMentions: Set<ChannelID>;
	mentionsEveryone: boolean;
	changed: boolean;
}

interface MentionableUser {
	id: UserID;
	flags: bigint;
}

export class MessageMentionService {
	constructor(
		private userRepository: IUserRepository,
		private guildRepository: IGuildRepositoryAggregate,
		private gatewayService: IGatewayService,
		private workerService: IWorkerService<WorkerTaskName>,
		private responseDataService: MessageResponseDataService,
	) {}

	async extractMentions({
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
	}): Promise<MentionData> {
		const allMentions = await this.extractAllMentions(content);
		let mentionsEveryone = allMentions.everyone;
		let mentionsHere = allMentions.here;
		if (guild && isOperationDisabled(guild, GuildOperations.EVERYONE_MENTIONS)) {
			mentionsEveryone = false;
			mentionsHere = false;
		}
		const userMentions = allMentions.users;
		const roleMentions = allMentions.roles;
		const channelMentions = allMentions.channels;
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
		return {userMentions, roleMentions, channelMentions, flags: flags | sendableFlags, mentionsEveryone, mentionsHere};
	}

	async buildReadRepairMentionData({
		message,
		referencedMessage,
	}: {
		message: Message;
		referencedMessage: Message | null;
	}): Promise<MentionReadRepairData> {
		const allMentions = await this.extractAllMentions(message.content ?? '');
		const contentUserMentions = allMentions.users;
		const contentRoleMentions = allMentions.roles;
		const contentChannelMentions = allMentions.channels;
		const referencedAuthorId = referencedMessage?.authorId ?? null;
		const preserveUnknownReplyMentions = message.reference != null && referencedMessage == null;
		const userMentions = new Set<UserID>();
		for (const id of message.mentionedUserIds) {
			if (contentUserMentions.has(id) || id === referencedAuthorId || preserveUnknownReplyMentions) {
				userMentions.add(id);
			}
		}
		const roleMentions = new Set<RoleID>();
		for (const id of message.mentionedRoleIds) {
			if (contentRoleMentions.has(id)) {
				roleMentions.add(id);
			}
		}
		const mentionsEveryone = message.mentionEveryone && (allMentions.everyone || allMentions.here);
		return {
			userMentions,
			roleMentions,
			channelMentions: contentChannelMentions,
			mentionsEveryone,
			changed:
				!this.setsEqual(message.mentionedUserIds, userMentions) ||
				!this.setsEqual(message.mentionedRoleIds, roleMentions) ||
				!this.setsEqual(message.mentionedChannelIds, contentChannelMentions) ||
				message.mentionEveryone !== mentionsEveryone,
		};
	}

	private async extractAllMentions(content: string): Promise<{
		users: Set<UserID>;
		roles: Set<RoleID>;
		channels: Set<ChannelID>;
		everyone: boolean;
		here: boolean;
	}> {
		const results = await this.responseDataService.extractMentions([content]);
		const rustResult = results[0];
		if (!rustResult) {
			throw new Error('Message mention extraction service returned no result for non-empty content');
		}
		return {
			users: new Set(rustResult.users.filter((id) => id !== '0').map((id) => createUserID(BigInt(id)))),
			roles: new Set(rustResult.roles.filter((id) => id !== '0').map((id) => createRoleID(BigInt(id)))),
			channels: new Set(rustResult.channels.filter((id) => id !== '0').map((id) => createChannelID(BigInt(id)))),
			everyone: rustResult.everyone,
			here: rustResult.here,
		};
	}

	private setsEqual<T>(left: ReadonlySet<T>, right: ReadonlySet<T>): boolean {
		if (left.size !== right.size) return false;
		for (const value of left) {
			if (!right.has(value)) return false;
		}
		return true;
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
	}): {
		flags: number;
		mentionsEveryone: boolean;
		mentionsHere: boolean;
	} {
		const isExplicitlyEmptyObject =
			allowedMentions.parse === undefined &&
			allowedMentions.users === undefined &&
			allowedMentions.roles === undefined &&
			allowedMentions.replied_user === undefined;
		const hasExplicitLists = allowedMentions.users !== undefined || allowedMentions.roles !== undefined;
		const parse = isExplicitlyEmptyObject
			? []
			: (allowedMentions.parse ?? (hasExplicitLists ? [] : ALLOWED_MENTIONS_PARSE));
		const users = allowedMentions.users ?? [];
		const roles = allowedMentions.roles ?? [];
		const hasExplicitParse = allowedMentions.parse !== undefined;
		if (hasExplicitParse && parse.length > 0 && (users.length > 0 || roles.length > 0)) {
			throw InputValidationError.fromCode(
				'allowed_mentions',
				ValidationErrorCodes.PARSE_AND_USERS_OR_ROLES_CANNOT_BE_USED_TOGETHER,
			);
		}
		const repliedUserId = referencedMessage?.authorId;
		const shouldPreserveRepliedUser =
			repliedUserId && !isExplicitlyEmptyObject && allowedMentions.replied_user !== false;
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
		if (shouldParse) {
			return;
		}
		if (allowedList.length === 0) {
			mentions.clear();
			return;
		}
		for (const id of Array.from(mentions)) {
			if (!allowedList.includes(id)) {
				mentions.delete(id);
			}
		}
	}

	async validateMentions({
		userMentions,
		roleMentions,
		channelMentions,
		channel,
		message,
		guild,
		canMentionRoles = true,
	}: {
		userMentions: Set<UserID>;
		roleMentions: Set<RoleID>;
		channelMentions: Set<ChannelID>;
		channel: Channel;
		message: Pick<Message, 'authorId' | 'webhookId'>;
		guild?: GuildResponse | null;
		canMentionRoles?: boolean;
	}): Promise<{
		validUserIds: Array<UserID>;
		validRoleIds: Array<RoleID>;
		validChannelMentions: Array<GatewayChannelMention>;
	}> {
		const channelMentionPromise = this.validateChannelMentions({channelMentions, channel});
		if (channel.guildId) {
			const [users, roles, validChannelMentions] = await Promise.all([
				userMentions.size > 0 ? this.userRepository.listUsers(Array.from(userMentions)) : Promise.resolve([]),
				this.resolveMentionRoles({roleMentions, guildId: channel.guildId, guild}),
				channelMentionPromise,
			]);
			const filteredRoles = canMentionRoles ? roles : roles.filter((role) => role.mentionable);
			const blockFilteredMentions = await this.filterBlockedUserMentions({
				validUsers: users,
				validUserIds: users.map((u) => u.id),
				message,
			});
			const validUserIds = await this.filterSuppressedSelfMentions({
				validUsers: blockFilteredMentions.validUsers,
				validUserIds: blockFilteredMentions.validUserIds,
				message,
			});
			return {
				validUserIds,
				validRoleIds: filteredRoles.map((r) => r.id),
				validChannelMentions,
			};
		}
		const recipients = Array.from(channel.recipientIds || []);
		const validUserIds = recipients.filter((r) => userMentions.has(r));
		const [validUsers, validChannelMentions] = await Promise.all([
			validUserIds.length > 0 ? this.userRepository.listUsers(validUserIds) : Promise.resolve([]),
			channelMentionPromise,
		]);
		const blockFilteredMentions = await this.filterBlockedUserMentions({
			validUsers,
			validUserIds,
			message,
		});
		return {
			validUserIds: await this.filterSuppressedSelfMentions({
				validUsers: blockFilteredMentions.validUsers,
				validUserIds: blockFilteredMentions.validUserIds,
				message,
			}),
			validRoleIds: [],
			validChannelMentions,
		};
	}

	private async resolveMentionRoles({
		roleMentions,
		guildId,
		guild,
	}: {
		roleMentions: Set<RoleID>;
		guildId: GuildID;
		guild?: GuildResponse | null;
	}): Promise<Array<{id: RoleID; mentionable: boolean}>> {
		if (roleMentions.size === 0) {
			return [];
		}
		if (guild?.roles) {
			const mentionedRoleIds = new Set(Array.from(roleMentions, String));
			return guild.roles
				.filter((role) => mentionedRoleIds.has(role.id))
				.map((role) => ({
					id: createRoleID(BigInt(role.id)),
					mentionable: role.mentionable,
				}));
		}
		const roles = await this.guildRepository.listRolesByIds(Array.from(roleMentions), guildId);
		return roles.map((role) => ({id: role.id, mentionable: role.isMentionable}));
	}

	private async filterBlockedUserMentions({
		validUsers,
		validUserIds,
		message,
	}: {
		validUsers: Array<MentionableUser>;
		validUserIds: Array<UserID>;
		message: Pick<Message, 'authorId' | 'webhookId'>;
	}): Promise<{
		validUsers: Array<MentionableUser>;
		validUserIds: Array<UserID>;
	}> {
		if (validUserIds.length === 0 || message.webhookId != null || message.authorId == null) {
			return {validUsers, validUserIds};
		}
		const authorId = message.authorId;
		const blockedUserIds = new Set(
			(
				await Promise.all(
					validUserIds.map(async (targetUserId) => {
						if (targetUserId === authorId) {
							return null;
						}
						const block = await this.userRepository.getRelationship(targetUserId, authorId, RelationshipTypes.BLOCKED);
						return block ? targetUserId : null;
					}),
				)
			).filter((targetUserId): targetUserId is UserID => targetUserId != null),
		);
		if (blockedUserIds.size === 0) {
			return {validUsers, validUserIds};
		}
		return {
			validUsers: validUsers.filter((user) => !blockedUserIds.has(user.id)),
			validUserIds: validUserIds.filter((userId) => !blockedUserIds.has(userId)),
		};
	}

	async validateChannelMentions({
		channelMentions,
		channel,
	}: {
		channelMentions: Set<ChannelID>;
		channel: Channel;
	}): Promise<Array<GatewayChannelMention>> {
		if (!channel.guildId || channelMentions.size === 0) {
			return [];
		}
		return this.gatewayService.resolveChannelMentions({
			guildId: channel.guildId,
			channelIds: Array.from(channelMentions),
		});
	}

	private async filterSuppressedSelfMentions({
		validUsers,
		validUserIds,
		message,
	}: {
		validUsers: Array<MentionableUser>;
		validUserIds: Array<UserID>;
		message: Pick<Message, 'authorId' | 'webhookId'>;
	}): Promise<Array<UserID>> {
		if (validUserIds.length === 0 || message.webhookId != null || message.authorId == null) {
			return validUserIds;
		}
		const author = await this.userRepository.findUnique(message.authorId);
		if (!author || author.isBot) {
			return validUserIds;
		}
		const authorHasStaffFlag = (author.flags & UserFlags.STAFF) === UserFlags.STAFF;
		if (authorHasStaffFlag) {
			return validUserIds;
		}
		const staffTargets = validUsers.filter((user) => (user.flags & UserFlags.STAFF) === UserFlags.STAFF);
		if (staffTargets.length === 0) {
			return validUserIds;
		}
		const suppressedUserIds = new Set<UserID>();
		const targetSettings = await Promise.all(
			staffTargets.map(async (targetUser) => ({
				targetUserId: targetUser.id,
				settings: await this.userRepository.findSettings(targetUser.id),
			})),
		);
		for (const {targetUserId, settings} of targetSettings) {
			if (!settings?.suppressUnprivilegedSelfMentions) {
				continue;
			}
			if (settings.suppressUnprivilegedSelfMentionBypassUserIds.has(author.id)) {
				continue;
			}
			suppressedUserIds.add(targetUserId);
		}
		if (suppressedUserIds.size === 0) {
			return validUserIds;
		}
		return validUserIds.filter((userId) => !suppressedUserIds.has(userId));
	}

	async handleMentionTasks(params: {
		guildId: GuildID | null;
		message: Message;
		authorId: UserID;
		mentionHere?: boolean;
	}): Promise<void> {
		const {guildId, message, authorId, mentionHere = false} = params;
		if (isPersonalNotesChannel({userId: authorId, channelId: message.channelId})) return;
		const mentionUserIds = Array.from(message.mentionedUserIds ?? []);
		const mentionRoleIds = Array.from(message.mentionedRoleIds ?? []);
		const mentionEveryone = message.mentionEveryone && !mentionHere;
		const taskData = {
			guildId: guildId?.toString(),
			channelId: message.channelId.toString(),
			messageId: message.id.toString(),
			authorId: authorId.toString(),
			mentionHere,
			mentionEveryone,
			mentionUserIds: mentionUserIds.map((userId) => userId.toString()),
			mentionRoleIds: mentionRoleIds.map((roleId) => roleId.toString()),
		};
		const hasMentions =
			mentionUserIds.length > 0 ||
			mentionRoleIds.length > 0 ||
			mentionHere ||
			mentionEveryone ||
			(message.reference && message.type === MessageTypes.REPLY);
		if (!hasMentions) {
			return;
		}
		try {
			await this.workerService.addJob('handleMentions', taskData, {skipLedger: true});
		} catch (error) {
			if (!(error instanceof WorkerQueueOverflowError)) {
				throw error;
			}
			Logger.warn(
				{channelId: message.channelId.toString(), messageId: message.id.toString()},
				'Dropped mention fanout, jobs stream is at its limit',
			);
		}
	}
}
