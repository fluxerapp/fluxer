// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import * as ChannelStickerCommands from '@app/features/channel/commands/ChannelStickerCommands';
import ChannelSticker from '@app/features/channel/state/ChannelSticker';
import Channels from '@app/features/channel/state/Channels';
import * as CommandUtils from '@app/features/devtools/utils/CommandUtils';
import Emoji from '@app/features/emoji/state/Emoji';
import {checkEmojiAvailabilityWithGuildFallback} from '@app/features/expressions/utils/ExpressionPermissionUtils';
import ChannelMemberCount from '@app/features/guild/state/ChannelMemberCount';
import Guilds from '@app/features/guild/state/Guilds';
import type {ComposerHandle} from '@app/features/lexical/composer/ComposerHandle';
import {
	type LexicalMessageCommandResolution,
	LexicalMessageCommandResolutionStatus,
	LexicalMessageCommandResolver,
} from '@app/features/lexical/composer/LexicalMessageCommand';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberSidebar from '@app/features/member/state/MemberSidebar';
import * as DraftCommands from '@app/features/messaging/commands/DraftCommands';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import type {MentionConfirmationInfo, MentionType} from '@app/features/messaging/state/MentionConfirmationStateMachine';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {
	buildExistingAttachmentEditReferences,
	canSubmitEmptyMessageEdit,
} from '@app/features/messaging/utils/MessageEditContentUtils';
import {hasVisibleMessageContent} from '@app/features/messaging/utils/MessageRequestUtils';
import * as ReplaceCommandUtils from '@app/features/messaging/utils/ReplaceCommandUtils';
import {resolveTypedEmojiShortcodes} from '@app/features/messaging/utils/TypedEmojiShortcodeUtils';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Presence from '@app/features/presence/state/Presence';
import {TypingUtils} from '@app/features/typing/utils/TypingUtils';
import * as FormUtils from '@app/lib/forms';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {StatusTypes} from '@fluxer/constants/src/StatusConstants';
import type {MessageStickerItem} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import type React from 'react';
import {useCallback} from 'react';

const COULDN_T_RUN_THAT_COMMAND_DESCRIPTOR = msg({
	message: "Couldn't run that command: {detail}",
	comment: 'Error message in the use textarea submit hook. Preserve {detail}; it is inserted by code.',
});
const logger = new Logger('useTextareaSubmit');
const MENTION_EVERYONE_THRESHOLD = import.meta.env.DEV ? 0 : 50;
const MENTION_COUNT_LOAD_TIMEOUT_MS = 1500;
const ROLE_MENTION_PATTERN = /<@&(\d+)>/g;
const CUSTOM_EMOJI_MARKDOWN_PATTERN = /<a?:[a-zA-Z0-9_+-]{2,}:([0-9]+)>/g;

const mentionTypePriority: Record<MentionType, number> = {
	'@everyone': 3,
	'@here': 2,
	role: 1,
};
const pendingMentionCountLoads = new Map<string, Promise<void>>();

interface UseTextareaSubmitOptions {
	channelId: string;
	guildId: string | null;
	value: string;
	setValue: React.Dispatch<React.SetStateAction<string>>;
	displayToActual: (text: string) => string;
	composerHandleRef?: React.RefObject<ComposerHandle | null>;
	clearSegments: () => void;
	isSlowmodeActive: boolean;
	editingMessage: Message | null;
	isMobileEditMode: boolean;
	uploadAttachmentsLength: number;
	hasPendingSticker: boolean;
	handleSendMessage: (
		content: string,
		hasAttachments: boolean,
		stickersOrTts?: Array<MessageStickerItem> | boolean,
		favoriteMemeIdOrStickers?: string | Array<MessageStickerItem>,
		maybeFavoriteMemeId?: string,
	) => boolean;
	onMentionConfirmationNeeded?: (info: MentionConfirmationInfo) => void;
	i18n: I18n;
}

interface MentionCountResolutionParams {
	mentionType: MentionType;
	guildId: string;
	channelId: string;
	fallbackCount: number;
}

function getMentionCountCacheKey(guildId: string, channelId: string): string {
	return `${guildId}:${channelId}`;
}

function hasAuthoritativeMentionCounts(guildId: string, channelId: string): boolean {
	if (ChannelMemberCount.getCounts(guildId, channelId) != null) {
		return true;
	}
	const listState = MemberSidebar.getList(guildId, channelId);
	if (!listState) {
		return false;
	}
	return listState.hasReceivedInitialPayload;
}

function getAuthoritativeMentionCounts(
	guildId: string,
	channelId: string,
): {
	memberCount: number;
	onlineCount: number;
} | null {
	const channelCounts = ChannelMemberCount.getCounts(guildId, channelId);
	if (channelCounts) {
		return {
			memberCount: channelCounts.memberCount,
			onlineCount: channelCounts.onlineCount,
		};
	}
	if (!hasAuthoritativeMentionCounts(guildId, channelId)) {
		return null;
	}
	const listState = MemberSidebar.getList(guildId, channelId);
	if (!listState) {
		return null;
	}
	return {
		memberCount: listState.memberCount,
		onlineCount: listState.onlineCount,
	};
}

async function waitForAuthoritativeMentionCounts(guildId: string, channelId: string): Promise<void> {
	if (hasAuthoritativeMentionCounts(guildId, channelId)) {
		return;
	}
	await new Promise<void>((resolve) => {
		const deadline = Date.now() + MENTION_COUNT_LOAD_TIMEOUT_MS;
		const poll = () => {
			if (hasAuthoritativeMentionCounts(guildId, channelId) || Date.now() >= deadline) {
				resolve();
				return;
			}
			window.setTimeout(poll, 50);
		};
		poll();
	});
}

async function ensureMentionCountsLoaded(guildId: string, channelId: string): Promise<void> {
	if (hasAuthoritativeMentionCounts(guildId, channelId)) {
		return;
	}
	const guild = Guilds.getGuild(guildId);
	if (!guild || (guild.disabledOperations & GuildOperations.MEMBER_LIST_UPDATES) !== 0) {
		return;
	}
	const cacheKey = getMentionCountCacheKey(guildId, channelId);
	const existingLoad = pendingMentionCountLoads.get(cacheKey);
	if (existingLoad) {
		await existingLoad;
		return;
	}
	const loadPromise = (async () => {
		try {
			ChannelMemberCount.requestCounts(guildId, channelId, {force: true});
			await waitForAuthoritativeMentionCounts(guildId, channelId);
		} finally {
			pendingMentionCountLoads.delete(cacheKey);
		}
	})();
	pendingMentionCountLoads.set(cacheKey, loadPromise);
	await loadPromise;
}

export function getResolvedMentionCount({
	mentionType,
	guildId,
	channelId,
	fallbackCount,
}: MentionCountResolutionParams): number {
	const authoritativeCounts = getAuthoritativeMentionCounts(guildId, channelId);
	if (mentionType === '@everyone') {
		if (authoritativeCounts !== null) {
			return authoritativeCounts.memberCount;
		}
		const guild = Guilds.getGuild(guildId);
		if (guild !== undefined) {
			return guild.memberCount;
		}
		return fallbackCount;
	}
	if (mentionType === '@here') {
		if (authoritativeCounts !== null) {
			return authoritativeCounts.onlineCount;
		}
		return fallbackCount;
	}
	return fallbackCount;
}

function shouldBlockSubmissionForSlowmode(
	isSlowmodeActive: boolean,
	editingMessage: Message | null,
	parsedCommand: CommandUtils.ParsedCommand | null,
	lexicalCommand: LexicalMessageCommandResolution | null,
	hasReplaceCommand: boolean,
): boolean {
	if (!isSlowmodeActive || editingMessage !== null) {
		return false;
	}
	if (hasReplaceCommand) {
		return false;
	}
	if (lexicalCommand !== null) {
		if (lexicalCommand.status === LexicalMessageCommandResolutionStatus.NO_COMMAND) {
			return true;
		}
		if (lexicalCommand.status === LexicalMessageCommandResolutionStatus.INVALID_COMMAND) {
			return false;
		}
		return CommandUtils.doesCommandSendCurrentChannelMessage(lexicalCommand.command);
	}
	if (parsedCommand === null || parsedCommand.type === 'unknown') {
		return parsedCommand === null;
	}
	return CommandUtils.doesCommandSendCurrentChannelMessage(parsedCommand);
}

export function shouldShowMentionConfirmation(params: MentionCountResolutionParams): boolean {
	return getResolvedMentionCount(params) > MENTION_EVERYONE_THRESHOLD;
}

export const useTextareaSubmit = ({
	channelId,
	guildId,
	value,
	setValue,
	displayToActual,
	composerHandleRef,
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
	const checkMentionConfirmation = useCallback(
		async (content: string, sourceContent: string, tts?: boolean): Promise<boolean> => {
			if (!guildId || !onMentionConfirmationNeeded) {
				return false;
			}
			const channel = Channels.getChannel(channelId);
			const canMentionEveryone = Boolean(channel && Permission.can(Permissions.MENTION_EVERYONE, channel));
			const includesEveryoneMention = content.includes('@everyone');
			const includesHereMention = content.includes('@here');
			if (canMentionEveryone && (includesEveryoneMention || includesHereMention)) {
				await ensureMentionCountsLoaded(guildId, channelId);
			}
			const mentionCandidates: Array<{
				mentionType: MentionType;
				memberIds: Set<string>;
				memberCount: number;
				roleId?: string;
				roleName?: string;
			}> = [];
			const guildMembers = GuildMembers.getMembers(guildId);
			const guildMemberIds = new Set(guildMembers.map((member) => member.user.id));
			if (canMentionEveryone) {
				if (
					includesEveryoneMention &&
					shouldShowMentionConfirmation({
						mentionType: '@everyone',
						guildId,
						channelId,
						fallbackCount: guildMemberIds.size,
					})
				) {
					mentionCandidates.push({
						mentionType: '@everyone',
						memberIds: guildMemberIds,
						memberCount: getResolvedMentionCount({
							mentionType: '@everyone',
							guildId,
							channelId,
							fallbackCount: guildMemberIds.size,
						}),
					});
				}
				if (includesHereMention) {
					const hereMemberIds = new Set<string>();
					for (const member of guildMembers) {
						const status = Presence.getStatus(member.user.id);
						if (status === StatusTypes.OFFLINE || status === StatusTypes.INVISIBLE) {
							continue;
						}
						hereMemberIds.add(member.user.id);
					}
					if (
						shouldShowMentionConfirmation({
							mentionType: '@here',
							guildId,
							channelId,
							fallbackCount: hereMemberIds.size,
						})
					) {
						mentionCandidates.push({
							mentionType: '@here',
							memberIds: hereMemberIds,
							memberCount: getResolvedMentionCount({
								mentionType: '@here',
								guildId,
								channelId,
								fallbackCount: hereMemberIds.size,
							}),
						});
					}
				}
			}
			const guild = Guilds.getGuild(guildId);
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
							memberCount: roleMemberIds.size,
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
			if (uniqueMemberIds.size === 0 && mentionCandidates.every((candidate) => candidate.memberCount === 0)) {
				return false;
			}
			mentionCandidates.sort((a, b) => {
				if (b.memberCount !== a.memberCount) {
					return b.memberCount - a.memberCount;
				}
				return mentionTypePriority[b.mentionType] - mentionTypePriority[a.mentionType];
			});
			const highestImpact = mentionCandidates[0];
			let resolvedMemberCount = Math.max(uniqueMemberIds.size, highestImpact.memberCount);
			if (highestImpact.mentionType === '@everyone' || mentionCandidates.length === 1) {
				resolvedMemberCount = highestImpact.memberCount;
			}
			onMentionConfirmationNeeded({
				mentionType: highestImpact.mentionType,
				memberCount: resolvedMemberCount,
				content,
				sourceContent,
				tts,
				roleId: highestImpact.roleId,
				roleName: highestImpact.roleName,
			});
			return true;
		},
		[channelId, guildId, onMentionConfirmationNeeded],
	);
	const ttsCommandEnabled = Accessibility.enableTTSCommand;
	const checkCustomEmojiAvailability = useCallback(
		(content: string): boolean => {
			const storedChannel = Channels.getChannel(channelId);
			const channel = storedChannel === undefined ? null : storedChannel;
			CUSTOM_EMOJI_MARKDOWN_PATTERN.lastIndex = 0;
			let match: RegExpExecArray | null = null;
			while ((match = CUSTOM_EMOJI_MARKDOWN_PATTERN.exec(content))) {
				const emojiId = match[1];
				const emoji = Emoji.getEmojiById(emojiId);
				if (!emoji) {
					continue;
				}
				const availability = checkEmojiAvailabilityWithGuildFallback(i18n, emoji, channel, guildId);
				if (availability.canUse) {
					continue;
				}
				if (availability.lockReason) {
					const errorMessage = CommandUtils.createSystemMessage(channelId, availability.lockReason);
					MessageCommands.createOptimistic(channelId, errorMessage.toJSON());
				}
				return true;
			}
			return false;
		},
		[channelId, guildId, i18n],
	);
	const resolveTypedEmojiContent = useCallback(
		(content: string): string => {
			const storedChannel = Channels.getChannel(channelId);
			const channel = storedChannel === undefined ? null : storedChannel;
			return resolveTypedEmojiShortcodes({
				content,
				channel,
				guildIdFallback: guildId,
				i18n,
			});
		},
		[channelId, guildId, i18n],
	);
	const onSubmit = useCallback(async () => {
		let composerHandle: ComposerHandle | null = null;
		if (composerHandleRef !== undefined) {
			composerHandle = composerHandleRef.current;
		}
		let lexicalCommand: LexicalMessageCommandResolution | null = null;
		let actualContent = displayToActual(value).trim();
		if (composerHandle !== null) {
			lexicalCommand = LexicalMessageCommandResolver.resolve(composerHandle);
			actualContent = composerHandle.getWireValue().trim();
		}
		const resolvedContent = resolveTypedEmojiContent(actualContent);
		let parsedCommand: CommandUtils.ParsedCommand | null = null;
		if (lexicalCommand === null) {
			parsedCommand = CommandUtils.isCommand(actualContent) ? CommandUtils.parseCommand(actualContent) : null;
		} else if (lexicalCommand.status === LexicalMessageCommandResolutionStatus.VALID_COMMAND) {
			parsedCommand = lexicalCommand.command;
		}
		const replaceCommand = ReplaceCommandUtils.parseReplaceCommand(actualContent);
		if (
			shouldBlockSubmissionForSlowmode(
				isSlowmodeActive,
				editingMessage,
				parsedCommand,
				lexicalCommand,
				replaceCommand !== null,
			)
		)
			return;
		if (lexicalCommand !== null && lexicalCommand.status === LexicalMessageCommandResolutionStatus.INVALID_COMMAND) {
			return;
		}
		if (editingMessage && isMobileEditMode) {
			const finishMobileEdit = () => {
				MessageCommands.stopEditMobile(channelId);
				setValue('');
				clearSegments();
			};
			if (!hasVisibleMessageContent(resolvedContent)) {
				if (canSubmitEmptyMessageEdit(editingMessage)) {
					if (editingMessage.content.length === 0) {
						finishMobileEdit();
						return;
					}
					MessageCommands.edit(
						channelId,
						editingMessage.id,
						'',
						undefined,
						editingMessage._allowedMentions,
						buildExistingAttachmentEditReferences(editingMessage),
					).then((result) => {
						if (result) {
							finishMobileEdit();
						}
					});
					return;
				}
				MessageCommands.showDeleteConfirmation(i18n, {
					message: editingMessage,
					onDelete: () => MessageCommands.stopEditMobile(channelId),
				});
				setValue('');
				clearSegments();
				return;
			}
			if (checkCustomEmojiAvailability(resolvedContent)) {
				return;
			}
			MessageCommands.edit(
				channelId,
				editingMessage.id,
				resolvedContent,
				undefined,
				editingMessage._allowedMentions,
			).then((result) => {
				if (result) {
					finishMobileEdit();
				}
			});
			return;
		}
		if (!hasVisibleMessageContent(resolvedContent) && uploadAttachmentsLength === 0 && !hasPendingSticker) {
			return;
		}
		if (replaceCommand) {
			const lastMessage = Messages.getLastEditableMessage(channelId);
			if (lastMessage) {
				const newContent = ReplaceCommandUtils.executeReplaceCommand(lastMessage.content, replaceCommand);
				if (newContent !== lastMessage.content) {
					MessageCommands.edit(
						lastMessage.channelId,
						lastMessage.id,
						newContent,
						undefined,
						lastMessage._allowedMentions,
					);
				}
			}
			setValue('');
			clearSegments();
			DraftCommands.deleteDraft(channelId);
			TypingUtils.clear(channelId);
			return;
		}
		const sendWithPendingSticker = (content: string, hasAttachments: boolean, tts?: boolean): boolean => {
			const pendingSticker = ChannelSticker.getPendingSticker(channelId);
			const stickerItems = pendingSticker ? [pendingSticker.toJSON()] : undefined;
			let didSend = false;
			if (tts) {
				didSend = handleSendMessage(content, hasAttachments, true, stickerItems);
			} else if (stickerItems) {
				didSend = handleSendMessage(content, hasAttachments, stickerItems);
			} else {
				didSend = handleSendMessage(content, hasAttachments);
			}
			if (didSend && pendingSticker) {
				ChannelStickerCommands.removePendingSticker(channelId);
			}
			return didSend;
		};
		if (parsedCommand) {
			if (parsedCommand.type !== 'unknown') {
				if (!ttsCommandEnabled && parsedCommand.type === 'tts') {
				} else if (parsedCommand.type === 'me' || parsedCommand.type === 'spoiler') {
					let transformedContent = resolveTypedEmojiContent(actualContent);
					if (lexicalCommand !== null) {
						const commandContent = resolveTypedEmojiContent(parsedCommand.content);
						transformedContent = parsedCommand.type === 'me' ? `_${commandContent}_` : `||${commandContent}||`;
					} else {
						transformedContent = CommandUtils.transformWrappingCommands(transformedContent);
					}
					if (checkCustomEmojiAvailability(transformedContent)) {
						return;
					}
					if (!(await checkMentionConfirmation(transformedContent, actualContent))) {
						sendWithPendingSticker(transformedContent, false);
						return;
					}
				} else if (parsedCommand.type === 'tts') {
					const ttsContent = resolveTypedEmojiContent(parsedCommand.content);
					if (checkCustomEmojiAvailability(ttsContent)) {
						return;
					}
					if (!(await checkMentionConfirmation(ttsContent, actualContent, true))) {
						sendWithPendingSticker(ttsContent, false, true);
						return;
					}
				} else {
					try {
						let commandGuildId: string | undefined;
						if (guildId !== null) {
							commandGuildId = guildId;
						}
						await CommandUtils.executeCommand(parsedCommand, channelId, commandGuildId, i18n);
						setValue('');
						clearSegments();
						DraftCommands.deleteDraft(channelId);
						TypingUtils.clear(channelId);
						if (parsedCommand.type !== 'msg') {
							MessageCommands.stopReply(channelId);
						}
						return;
					} catch (error) {
						logger.error('Failed to execute command', error);
						const detail = FormUtils.extractErrorMessage(i18n, error);
						const errorMessage = CommandUtils.createSystemMessage(
							channelId,
							i18n._(COULDN_T_RUN_THAT_COMMAND_DESCRIPTOR, {detail}),
						);
						MessageCommands.createOptimistic(channelId, errorMessage.toJSON());
						return;
					}
				}
			}
		}
		if (checkCustomEmojiAvailability(resolvedContent)) {
			return;
		}
		if (!(await checkMentionConfirmation(resolvedContent, actualContent))) {
			sendWithPendingSticker(resolvedContent, false);
		}
	}, [
		channelId,
		value,
		uploadAttachmentsLength,
		displayToActual,
		composerHandleRef,
		clearSegments,
		editingMessage,
		isMobileEditMode,
		isSlowmodeActive,
		i18n,
		guildId,
		handleSendMessage,
		hasPendingSticker,
		setValue,
		checkCustomEmojiAvailability,
		checkMentionConfirmation,
		resolveTypedEmojiContent,
		ttsCommandEnabled,
	]);
	return {onSubmit};
};
