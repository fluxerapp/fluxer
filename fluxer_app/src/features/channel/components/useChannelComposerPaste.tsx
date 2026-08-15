// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import Emoji from '@app/features/emoji/state/Emoji';
import type {GuildEmoji} from '@app/features/expressions/models/GuildEmoji';
import GuildGuilds from '@app/features/guild/state/Guilds';
import {
	$insertComposerClipboardSlice,
	type ComposerClipboardSlice,
	FLUXER_COMPOSER_CLIPBOARD_MIME,
	getComposerClipboardTextPlain,
	parseComposerClipboardSlice,
} from '@app/features/lexical/composer/ComposerClipboard';
import type {ComposerHandle} from '@app/features/lexical/composer/ComposerHandle';
import {showAttachmentPermissionDeniedModal} from '@app/features/messaging/components/alerts/AttachmentPermissionDeniedModal';
import {FileSizeTooLargeModal} from '@app/features/messaging/components/alerts/FileSizeTooLargeModal';
import {TooManyAttachmentsModal} from '@app/features/messaging/components/alerts/TooManyAttachmentsModal';
import ChatInputSettings from '@app/features/messaging/state/ChatInputSettings';
import MessageFocus from '@app/features/messaging/state/MessageFocus';
import type {CloudAttachment} from '@app/features/messaging/upload/CloudUpload';
import {canAttachFilesInChannel} from '@app/features/messaging/utils/AttachmentPermissionUtils';
import {getClipboardDataFiles, readClipboardImageFiles} from '@app/features/messaging/utils/ClipboardFilePasteUtils';
import * as FileUploadUtils from '@app/features/messaging/utils/FileUploadUtils';
import {detectPastedSegments, type LookupFunctions} from '@app/features/messaging/utils/PasteSegmentUtils';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import {isDialogPasteTarget} from '@app/features/messaging/utils/TextInputEditUtils';
import {canFocusTextarea, safeFocus} from '@app/features/platform/utils/InputFocusManager';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import ContextMenuState from '@app/features/ui/state/ContextMenu';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import Users from '@app/features/user/state/Users';
import {COMMAND_PRIORITY_HIGH, PASTE_COMMAND} from 'lexical';
import type React from 'react';
import {useCallback, useEffect} from 'react';

interface UseChannelComposerPasteParams {
	channel: Channel;
	handleRef: React.RefObject<ComposerHandle | null>;
	editableRef: React.RefObject<HTMLDivElement | null>;
	isFocused: boolean;
	maxMessageLength: number;
	maxAttachments: number;
	uploadAttachments: ReadonlyArray<CloudAttachment>;
	textareaInputDisabled: boolean;
}

function createComposerPasteSlice(pastedText: string, segments: ReadonlyArray<MentionSegment>): ComposerClipboardSlice {
	const displayParts: Array<string> = [];
	const projectedSegments: Array<MentionSegment> = [];
	let sourceCursor = 0;
	let displayLength = 0;
	for (const segment of segments) {
		const plainText = pastedText.slice(sourceCursor, segment.start);
		displayParts.push(plainText, segment.displayText);
		displayLength += plainText.length;
		projectedSegments.push({
			...segment,
			start: displayLength,
			end: displayLength + segment.displayText.length,
		});
		displayLength += segment.displayText.length;
		sourceCursor = segment.end;
	}
	displayParts.push(pastedText.slice(sourceCursor));
	return {display: displayParts.join(''), segments: projectedSegments};
}

export function useChannelComposerPaste({
	channel,
	handleRef,
	editableRef,
	isFocused,
	maxMessageLength,
	maxAttachments,
	uploadAttachments,
	textareaInputDisabled,
}: UseChannelComposerPasteParams): void {
	const handlePasteExceedsLimit = useCallback(
		async (pastedText: string) => {
			if (!canAttachFilesInChannel(channel)) {
				showAttachmentPermissionDeniedModal(channel);
				return;
			}
			const result = await FileUploadUtils.convertTextToFile(
				channel.id,
				pastedText,
				uploadAttachments.length,
				maxAttachments,
			);
			if (!result.success) {
				if (result.error === 'too_many_attachments') {
					ModalCommands.push(modal(() => <TooManyAttachmentsModal />));
				} else if (result.error === 'file_size_too_large') {
					ModalCommands.push(modal(() => <FileSizeTooLargeModal oversizedFileCount={result.oversizedFileCount} />));
				}
			}
		},
		[channel, uploadAttachments.length, maxAttachments],
	);
	const handlePasteFiles = useCallback(
		async (files: Array<File>) => {
			if (!canAttachFilesInChannel(channel)) {
				showAttachmentPermissionDeniedModal(channel);
				return;
			}
			const result = await FileUploadUtils.handleFileUpload(
				channel.id,
				files,
				uploadAttachments.length,
				maxAttachments,
			);
			if (!result.success) {
				if (result.error === 'too_many_attachments') {
					ModalCommands.push(modal(() => <TooManyAttachmentsModal />));
				} else if (result.error === 'file_size_too_large') {
					ModalCommands.push(modal(() => <FileSizeTooLargeModal oversizedFileCount={result.oversizedFileCount} />));
				}
			}
		},
		[channel, uploadAttachments.length, maxAttachments],
	);
	const insertPastedText = useCallback(
		(pastedText: string): boolean => {
			const guildId = channel.guildId;
			const lookups: LookupFunctions = {
				userById: (id: string) => {
					const user = Users.getUser(id);
					return user ? {id: user.id, tag: user.tag} : null;
				},
				channelById: (id: string) => {
					const foundChannel = Channels.getChannel(id);
					if (foundChannel === null || foundChannel === undefined || !foundChannel.name) return null;
					return {id: foundChannel.id, name: foundChannel.name};
				},
				roleById: (id: string) => {
					if (!guildId) return null;
					const roles = GuildGuilds.getGuildRoles(guildId);
					const role = roles.find((r) => r.id === id);
					return role ? {id: role.id, name: role.name} : null;
				},
				emojiById: (id: string) => {
					const guilds = GuildGuilds.getGuilds();
					for (const guild of guilds) {
						const emojis = Emoji.getGuildEmoji(guild.id);
						const emoji = emojis.find((e: GuildEmoji) => e.id === id);
						if (emoji) return {id: emoji.id, name: emoji.name, uniqueName: emoji.uniqueName};
					}
					return null;
				},
			};
			const segments = detectPastedSegments(pastedText, 0, lookups);
			const richSlice = createComposerPasteSlice(pastedText, segments);
			const plainText = getComposerClipboardTextPlain(richSlice);
			const slice = plainText === null || plainText === undefined ? {display: pastedText, segments: []} : richSlice;
			return $insertComposerClipboardSlice(slice, ChatInputSettings.renderComposerAsPlainText);
		},
		[channel.guildId],
	);
	useEffect(() => {
		const handle = handleRef.current;
		if (handle === null) {
			return;
		}
		const editor = handle.getEditor();
		const asClipboardEvent = (event: unknown): ClipboardEvent | null =>
			typeof ClipboardEvent !== 'undefined' && event instanceof ClipboardEvent ? event : null;
		const handlePaste = (event: ClipboardEvent): boolean => {
			if (textareaInputDisabled) {
				event.preventDefault();
				return true;
			}
			const clipboardData = event.clipboardData;
			if (clipboardData === null) return false;
			const pastedFiles = getClipboardDataFiles(clipboardData);
			if (pastedFiles.length > 0) {
				event.preventDefault();
				void handlePasteFiles(pastedFiles);
				return true;
			}
			const rawPastedText = clipboardData.getData('text/plain');
			if (!rawPastedText) {
				void readClipboardImageFiles().then((files) => {
					if (files.length > 0) {
						void handlePasteFiles(files);
					}
				});
				return false;
			}
			const serializedComposerSlice = clipboardData.getData(FLUXER_COMPOSER_CLIPBOARD_MIME);
			if (serializedComposerSlice) {
				const composerSlice = parseComposerClipboardSlice(serializedComposerSlice);
				const composerWire = composerSlice == null ? null : getComposerClipboardTextPlain(composerSlice);
				if (composerWire != null) {
					if (composerWire.length > maxMessageLength) {
						event.preventDefault();
						void handlePasteExceedsLimit(composerWire);
						return true;
					}
					return false;
				}
			}
			const pastedText = rawPastedText.replace(/\t/g, '    ');
			if (pastedText.length > maxMessageLength) {
				event.preventDefault();
				void handlePasteExceedsLimit(pastedText);
				return true;
			}
			if (!insertPastedText(pastedText)) {
				return false;
			}
			event.preventDefault();
			return true;
		};
		const handleWindowPaste = (event: ClipboardEvent) => {
			const element = editableRef.current;
			if (event.defaultPrevented || textareaInputDisabled || isDialogPasteTarget(event.target)) {
				return;
			}
			if (!canFocusTextarea(element === null ? undefined : element)) {
				return;
			}
			if (isFocused) {
				return;
			}
			if (QuickSwitcher.getIsOpen()) {
				return;
			}
			if (ContextMenuState.contextMenu) {
				return;
			}
			if (KeyboardMode.keyboardModeEnabled && MessageFocus.focusedMessageId) {
				return;
			}
			const clipboardData = event.clipboardData;
			if (clipboardData === null) {
				return;
			}
			if (getClipboardDataFiles(clipboardData).length > 0) {
				return;
			}
			if (!clipboardData.getData('text/plain')) {
				return;
			}
			if (!element) {
				return;
			}
			safeFocus(element, true);
			const currentHandle = handleRef.current;
			if (currentHandle !== null) currentHandle.focus();
			editor.dispatchCommand(PASTE_COMMAND, event);
		};
		window.addEventListener('paste', handleWindowPaste);
		const unregisterPaste = editor.registerCommand(
			PASTE_COMMAND,
			(event) => {
				const clipboardEvent = asClipboardEvent(event);
				return clipboardEvent != null && handlePaste(clipboardEvent);
			},
			COMMAND_PRIORITY_HIGH,
		);
		return () => {
			window.removeEventListener('paste', handleWindowPaste);
			unregisterPaste();
		};
	}, [handlePasteExceedsLimit, handlePasteFiles, insertPastedText, isFocused, maxMessageLength, textareaInputDisabled]);
}
