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

import type {I18n} from '@lingui/core';
import React from 'react';
import * as DraftActionCreators from '~/actions/DraftActionCreators';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import {Permissions, StatusTypes} from '~/Constants';
import type {MessageRecord} from '~/records/MessageRecord';
import ChannelStore from '~/stores/ChannelStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import MessageStore from '~/stores/MessageStore';
import PermissionStore from '~/stores/PermissionStore';
import PresenceStore from '~/stores/PresenceStore';
import * as CommandUtils from '~/utils/CommandUtils';
import * as ReplaceCommandUtils from '~/utils/ReplaceCommandUtils';
import {TypingUtils} from '~/utils/TypingUtils';

const MENTION_EVERYONE_THRESHOLD = import.meta.env.DEV ? 0 : 50;
const ROLE_MENTION_PATTERN = /<@&(\d+)>/g;
type MentionType = '@everyone' | '@here' | 'role';
const mentionTypePriority: Record<MentionType, number> = {
	'@everyone': 3,
	'@here': 2,
	role: 1,
};

export interface MentionConfirmationInfo {
	mentionType: MentionType;
	memberCount: number;
	content: string;
	tts?: boolean;
	roleId?: string;
	roleName?: string;
}

interface UseTextareaSubmitOptions {
	channelId: string;
	guildId: string | null;
	value: string;
	setValue: React.Dispatch<React.SetStateAction<string>>;
	displayToActual: (text: string) => string;
	clearSegments: () => void;
	isSlowmodeActive: boolean;
	editingMessage: MessageRecord | null;
	isMobileEditMode: boolean;
	uploadAttachmentsLength: number;
	hasPendingSticker: boolean;
	handleSendMessage: (content: string, hasAttachments: boolean, tts?: boolean) => void;
	onMentionConfirmationNeeded?: (info: MentionConfirmationInfo) => void;
	i18n: I18n;
}

export const useTextareaSubmit = ({
	channelId,
	guildId,
	value,
	setValue,
	displayToActual,
	clearSegments,
	isSlowmodeActive,
	editingMessage,
	isMobileEditMode,
	uploadAttachmentsLength,
	hasPendingSticker,
	handleSendMessage,
	onMentionConfirmationNeeded,
	i18n,
}: UseTextareaSubmitOptions) => {
	const checkMentionConfirmation = React.useCallback(
		(content: string, tts?: boolean): boolean => {
			if (!guildId || !onMentionConfirmationNeeded) {
				return false;
			}

			const channel = ChannelStore.getChannel(channelId);
			const canMentionEveryone = Boolean(channel && PermissionStore.can(Permissions.MENTION_EVERYONE, channel));

			const mentionCandidates: Array<{
				mentionType: MentionType;
				memberIds: Set<string>;
				roleId?: string;
				roleName?: string;
			}> = [];

			const guildMemberCount = GuildMemberStore.getMemberCount(guildId);
			const guildMembers = GuildMemberStore.getMembers(guildId);
			const guildMemberIds = new Set(guildMembers.map((member) => member.user.id));

			if (guildMemberCount > MENTION_EVERYONE_THRESHOLD && canMentionEveryone) {
				if (content.includes('@everyone')) {
					mentionCandidates.push({
						mentionType: '@everyone',
						memberIds: guildMemberIds,
					});
				}
				if (content.includes('@here')) {
					const hereMemberIds = new Set<string>();
					for (const member of guildMembers) {
						const status = PresenceStore.getStatus(member.user.id);
						if (status === StatusTypes.OFFLINE || status === StatusTypes.INVISIBLE) {
							continue;
						}
						hereMemberIds.add(member.user.id);
					}

					if (hereMemberIds.size > 0) {
						mentionCandidates.push({
							mentionType: '@here',
							memberIds: hereMemberIds,
						});
					}
				}
			}

			const guild = GuildStore.getGuild(guildId);
			if (guild) {
				ROLE_MENTION_PATTERN.lastIndex = 0;
				const mentionedRoles = new Set<string>();
				let match: RegExpExecArray | null = null;

				while ((match = ROLE_MENTION_PATTERN.exec(content))) {
					mentionedRoles.add(match[1]);
				}

				if (mentionedRoles.size > 0) {
					for (const roleId of mentionedRoles) {
						if (roleId === guild.id) {
							continue;
						}

						const role = guild.roles[roleId];
						if (!role) {
							continue;
						}

						const roleMemberIds = new Set<string>();
						for (const member of guildMembers) {
							if (member.roles.has(roleId)) {
								roleMemberIds.add(member.user.id);
							}
						}

						if (roleMemberIds.size <= MENTION_EVERYONE_THRESHOLD) {
							continue;
						}

						const canMentionRole = canMentionEveryone || role.mentionable;
						if (!canMentionRole) {
							continue;
						}

						mentionCandidates.push({
							mentionType: 'role',
							memberIds: roleMemberIds,
							roleId,
							roleName: role.name,
						});
					}
				}
			}

			if (mentionCandidates.length === 0) {
				return false;
			}

			const uniqueMemberIds = new Set<string>();
			for (const candidate of mentionCandidates) {
				candidate.memberIds.forEach((memberId) => uniqueMemberIds.add(memberId));
			}
			if (uniqueMemberIds.size === 0) {
				return false;
			}

			mentionCandidates.sort((a, b) => {
				if (b.memberIds.size !== a.memberIds.size) {
					return b.memberIds.size - a.memberIds.size;
				}
				return mentionTypePriority[b.mentionType] - mentionTypePriority[a.mentionType];
			});

			const highestImpact = mentionCandidates[0];

			onMentionConfirmationNeeded({
				mentionType: highestImpact.mentionType,
				memberCount: uniqueMemberIds.size,
				content,
				tts,
				roleId: highestImpact.roleId,
				roleName: highestImpact.roleName,
			});
			return true;
		},
		[channelId, guildId, onMentionConfirmationNeeded],
	);

	const onSubmit = React.useCallback(async () => {
		if (isSlowmodeActive && !editingMessage) {
			return;
		}

		const actualContent = displayToActual(value).trim();

		if (editingMessage && isMobileEditMode) {
			if (!actualContent) {
				MessageActionCreators.showDeleteConfirmation(i18n, {
					message: editingMessage,
					onDelete: () => MessageActionCreators.stopEditMobile(channelId),
				});
			} else {
				MessageActionCreators.edit(channelId, editingMessage.id, actualContent).then(() => {
					MessageActionCreators.stopEditMobile(channelId);
				});
			}
			setValue('');
			clearSegments();
			return;
		}

		if (!actualContent && uploadAttachmentsLength === 0 && !hasPendingSticker) {
			return;
		}

		const replaceCommand = ReplaceCommandUtils.parseReplaceCommand(actualContent);
		if (replaceCommand) {
			const lastMessage = MessageStore.getLastEditableMessage(channelId);
			if (lastMessage) {
				const newContent = ReplaceCommandUtils.executeReplaceCommand(lastMessage.content, replaceCommand);
				if (newContent !== lastMessage.content) {
					MessageActionCreators.edit(lastMessage.channelId, lastMessage.id, newContent);
				}
			}
			setValue('');
			clearSegments();
			return;
		}

		if (CommandUtils.isCommand(actualContent)) {
			const parsedCommand = CommandUtils.parseCommand(actualContent);
			if (parsedCommand.type !== 'unknown') {
				if (parsedCommand.type === 'me' || parsedCommand.type === 'spoiler') {
					const transformedContent = CommandUtils.transformWrappingCommands(actualContent);
					if (!checkMentionConfirmation(transformedContent)) {
						handleSendMessage(transformedContent, false);
					}
				} else if (parsedCommand.type === 'tts') {
					if (!checkMentionConfirmation(parsedCommand.content, true)) {
						handleSendMessage(parsedCommand.content, false, true);
					}
				} else {
					try {
						await CommandUtils.executeCommand(parsedCommand, channelId, guildId ?? undefined);
						setValue('');
						clearSegments();
						DraftActionCreators.deleteDraft(channelId);
						TypingUtils.clear(channelId);
						if (parsedCommand.type !== 'msg') {
							MessageActionCreators.stopReply(channelId);
						}
					} catch (error) {
						console.error('Failed to execute command:', error);
						const errorMessage = CommandUtils.createSystemMessage(
							channelId,
							`Failed to execute command: ${error instanceof Error ? error.message : 'Unknown error'}`,
						);
						MessageActionCreators.createOptimistic(channelId, errorMessage.toJSON());
					}
				}
				return;
			}
		}

		if (!checkMentionConfirmation(actualContent)) {
			handleSendMessage(actualContent, false);
		}
	}, [
		channelId,
		value,
		uploadAttachmentsLength,
		displayToActual,
		clearSegments,
		editingMessage,
		isMobileEditMode,
		isSlowmodeActive,
		guildId,
		handleSendMessage,
		hasPendingSticker,
		setValue,
		checkMentionConfirmation,
	]);

	return {onSubmit};
};
