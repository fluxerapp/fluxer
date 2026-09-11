// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import Emoji from '@app/features/emoji/state/Emoji';
import type {GuildEmoji} from '@app/features/expressions/models/GuildEmoji';
import Guilds from '@app/features/guild/state/Guilds';
import {getClipboardDataFiles, readClipboardImageFiles} from '@app/features/messaging/utils/ClipboardFilePasteUtils';
import {detectPastedSegments, type LookupFunctions} from '@app/features/messaging/utils/PasteSegmentUtils';
import {
	applyTextareaTextChange,
	type PrepareTextareaTextChange,
} from '@app/features/messaging/utils/TextareaNativeEditUtils';
import {type MentionSegment, TextareaSegmentManager} from '@app/features/messaging/utils/TextareaSegmentManager';
import {isDialogPasteTarget, isDocumentPasteTarget} from '@app/features/messaging/utils/TextInputEditUtils';
import {canFocusTextarea, safeFocus} from '@app/features/platform/utils/InputFocusManager';
import Modal from '@app/features/ui/state/Modal';
import Users from '@app/features/user/state/Users';
import {useCallback, useEffect} from 'react';

interface UseTextareaPasteParams {
	channel?: Channel | null;
	textareaRef: React.RefObject<HTMLTextAreaElement | null>;
	segmentManagerRef: React.RefObject<TextareaSegmentManager>;
	setValue: React.Dispatch<React.SetStateAction<string>>;
	previousValueRef: React.RefObject<string>;
	prepareTextChange: PrepareTextareaTextChange;
	maxMessageLength?: number;
	onPasteExceedsLimit?: (pastedText: string) => void;
	onPasteFiles?: (files: Array<File>) => void | Promise<void>;
	allowExceedingLimit?: boolean;
	disabled?: boolean;
}

export function useTextareaPaste({
	channel,
	textareaRef,
	segmentManagerRef,
	setValue,
	previousValueRef,
	prepareTextChange,
	maxMessageLength,
	onPasteExceedsLimit,
	onPasteFiles,
	allowExceedingLimit,
	disabled,
}: UseTextareaPasteParams) {
	const handlePasteFiles = useCallback(
		(files: Array<File>): boolean => {
			if (!onPasteFiles || files.length === 0) {
				return false;
			}
			void onPasteFiles(files);
			return true;
		},
		[onPasteFiles],
	);
	const tryReadAsyncClipboardImageFiles = useCallback(() => {
		if (!onPasteFiles) {
			return;
		}
		void readClipboardImageFiles().then((files) => {
			if (files.length > 0) {
				void onPasteFiles(files);
			}
		});
	}, [onPasteFiles]);
	const applyPastedText = useCallback(
		(
			pastedText: string,
			opts: {
				forceHandlePlainText: boolean;
			},
		): boolean => {
			const textarea = textareaRef.current;
			if (!textarea) return false;
			const currentValue = textarea.value;
			const rawSelectionStart = textarea.selectionStart ?? 0;
			const rawSelectionEnd = textarea.selectionEnd ?? 0;
			const selectionStart = Math.min(rawSelectionStart, rawSelectionEnd);
			const selectionEnd = Math.max(rawSelectionStart, rawSelectionEnd);
			const beforeSelection = currentValue.slice(0, selectionStart);
			const afterSelection = currentValue.slice(selectionEnd);
			const maxLength = maxMessageLength ?? null;
			const handleExceedsLimit = onPasteExceedsLimit ?? null;
			const guildId = channel?.guildId;
			const lookups: LookupFunctions = {
				userById: (id: string) => {
					const user = Users.getUser(id);
					return user ? {id: user.id, tag: user.tag} : null;
				},
				channelById: (id: string) => {
					const foundChannel = Channels.getChannel(id);
					return foundChannel?.name ? {id: foundChannel.id, name: foundChannel.name} : null;
				},
				roleById: (id: string) => {
					if (!guildId) return null;
					const roles = Guilds.getGuildRoles(guildId);
					const role = roles.find((r) => r.id === id);
					return role ? {id: role.id, name: role.name} : null;
				},
				emojiById: (id: string) => {
					if (guildId) {
						const guildEmojis = Emoji.getGuildEmoji(guildId);
						const emoji = guildEmojis.find((e: GuildEmoji) => e.id === id);
						if (emoji) return {id: emoji.id, name: emoji.name, uniqueName: emoji.uniqueName};
					}
					const guilds = Guilds.getGuilds();
					for (const guild of guilds) {
						const emojis = Emoji.getGuildEmoji(guild.id);
						const emoji = emojis.find((e: GuildEmoji) => e.id === id);
						if (emoji) return {id: emoji.id, name: emoji.name, uniqueName: emoji.uniqueName};
					}
					return null;
				},
			};
			const pasteSegments = detectPastedSegments(pastedText, 0, lookups);
			if (pasteSegments.length === 0) {
				if (!opts.forceHandlePlainText) {
					return false;
				}
				const newText = beforeSelection + pastedText + afterSelection;
				const segmentManager = new TextareaSegmentManager();
				segmentManager.setSegments(segmentManagerRef.current.getSegmentsCopy());
				const previousSegments = segmentManager.getSegmentsCopy();
				segmentManager.updateSegmentsForTextChange(selectionStart, selectionEnd, pastedText.length);
				if (maxLength != null && handleExceedsLimit) {
					const exceedsLimit = allowExceedingLimit
						? pastedText.length > maxLength
						: segmentManager.displayToActual(newText).length > maxLength;
					if (exceedsLimit) {
						segmentManager.setSegments(previousSegments);
						handleExceedsLimit(pastedText);
						return true;
					}
				}
				const newCursorPosition = selectionStart + pastedText.length;
				applyTextareaTextChange({
					textareaRef,
					setValue,
					segmentManagerRef,
					previousValueRef,
					prepareTextChange,
					nextValue: newText,
					nextSegments: segmentManager.getSegmentsCopy(),
					replacementText: pastedText,
					rangeStart: selectionStart,
					rangeEnd: selectionEnd,
					selectionStart: newCursorPosition,
				});
				return true;
			}
			let displayPastedText = pastedText;
			let offset = 0;
			const newSegments: Array<{
				start: number;
				displayText: string;
				actualText: string;
				type: MentionSegment['type'];
				id: string;
			}> = [];
			for (const seg of pasteSegments) {
				const originalLength = seg.end - seg.start;
				const adjustedStart = seg.start + offset;
				displayPastedText =
					displayPastedText.slice(0, adjustedStart) +
					seg.displayText +
					displayPastedText.slice(adjustedStart + originalLength);
				newSegments.push({
					start: selectionStart + adjustedStart,
					displayText: seg.displayText,
					actualText: seg.actualText,
					type: seg.type,
					id: seg.id,
				});
				offset += seg.displayText.length - originalLength;
			}
			const newText = beforeSelection + displayPastedText + afterSelection;
			const segmentManager = new TextareaSegmentManager();
			segmentManager.setSegments(segmentManagerRef.current.getSegmentsCopy());
			const previousSegments = segmentManager.getSegmentsCopy();
			segmentManager.updateSegmentsForTextChange(selectionStart, selectionEnd, displayPastedText.length);
			const updatedSegments = segmentManager.getSegments();
			const mappedSegments = newSegments.map((seg) => ({
				...seg,
				end: seg.start + seg.displayText.length,
			}));
			segmentManager.setSegments([...updatedSegments, ...mappedSegments]);
			if (maxLength != null && handleExceedsLimit) {
				const candidateActualText = segmentManager.displayToActual(newText);
				if (candidateActualText.length > maxLength) {
					segmentManager.setSegments(previousSegments);
					handleExceedsLimit(pastedText);
					return true;
				}
			}
			const newCursorPosition = selectionStart + displayPastedText.length;
			applyTextareaTextChange({
				textareaRef,
				setValue,
				segmentManagerRef,
				previousValueRef,
				prepareTextChange,
				nextValue: newText,
				nextSegments: segmentManager.getSegmentsCopy(),
				replacementText: displayPastedText,
				rangeStart: selectionStart,
				rangeEnd: selectionEnd,
				selectionStart: newCursorPosition,
			});
			return true;
		},
		[
			channel,
			textareaRef,
			segmentManagerRef,
			setValue,
			previousValueRef,
			prepareTextChange,
			maxMessageLength,
			onPasteExceedsLimit,
			allowExceedingLimit,
		],
	);
	const handleCopy = useCallback(
		(event: ClipboardEvent) => {
			const textarea = textareaRef.current;
			if (!textarea) return;
			const selectionStart = textarea.selectionStart ?? 0;
			const selectionEnd = textarea.selectionEnd ?? 0;
			if (selectionStart === selectionEnd) {
				return;
			}
			const actualText = segmentManagerRef.current.displayToActualSubstring(
				textarea.value,
				selectionStart,
				selectionEnd,
			);
			event.preventDefault();
			event.clipboardData?.setData('text/plain', actualText);
		},
		[textareaRef, segmentManagerRef],
	);
	const handleCut = useCallback(
		(event: ClipboardEvent) => {
			const textarea = textareaRef.current;
			if (!textarea) return;
			const rawSelectionStart = textarea.selectionStart ?? 0;
			const rawSelectionEnd = textarea.selectionEnd ?? 0;
			const selectionStart = Math.min(rawSelectionStart, rawSelectionEnd);
			const selectionEnd = Math.max(rawSelectionStart, rawSelectionEnd);
			if (selectionStart === selectionEnd) {
				return;
			}
			const displayText = textarea.value.slice(selectionStart, selectionEnd);
			const actualText = segmentManagerRef.current.displayToActualSubstring(
				textarea.value,
				selectionStart,
				selectionEnd,
			);
			event.clipboardData?.setData('text/plain', actualText);
			if (actualText === displayText) {
				return;
			}
			event.preventDefault();
			const nextText = textarea.value.slice(0, selectionStart) + textarea.value.slice(selectionEnd);
			const segmentManager = new TextareaSegmentManager();
			segmentManager.setSegments(segmentManagerRef.current.getSegmentsCopy());
			segmentManager.updateSegmentsForTextChange(selectionStart, selectionEnd, 0);
			applyTextareaTextChange({
				textareaRef,
				setValue,
				segmentManagerRef,
				previousValueRef,
				prepareTextChange,
				nextValue: nextText,
				nextSegments: segmentManager.getSegmentsCopy(),
				replacementText: '',
				rangeStart: selectionStart,
				rangeEnd: selectionEnd,
				selectionStart,
			});
		},
		[textareaRef, segmentManagerRef, setValue, previousValueRef, prepareTextChange],
	);
	const handlePaste = useCallback(
		(event: ClipboardEvent) => {
			if (disabled) {
				event.preventDefault();
				return;
			}
			if (Modal.hasModalOpen() && !isDialogPasteTarget(event.target)) {
				event.preventDefault();
				return;
			}
			const pastedFiles = getClipboardDataFiles(event.clipboardData);
			if (handlePasteFiles(pastedFiles)) {
				event.preventDefault();
				return;
			}
			const rawPastedText = event.clipboardData?.getData('text/plain');
			if (!rawPastedText) {
				tryReadAsyncClipboardImageFiles();
				return;
			}
			const pastedText = rawPastedText.replace(/\t/g, '    ');
			const hadTabs = pastedText !== rawPastedText;
			if (maxMessageLength != null && onPasteExceedsLimit) {
				if (allowExceedingLimit) {
					if (pastedText.length > maxMessageLength) {
						event.preventDefault();
						onPasteExceedsLimit(pastedText);
						return;
					}
				} else {
					const textarea = textareaRef.current;
					if (textarea) {
						const currentValue = textarea.value;
						const selectionStart = textarea.selectionStart ?? 0;
						const selectionEnd = textarea.selectionEnd ?? 0;
						const currentActualText = segmentManagerRef.current.displayToActual(currentValue);
						const selectedActualText = segmentManagerRef.current.displayToActualSubstring(
							currentValue,
							Math.min(selectionStart, selectionEnd),
							Math.max(selectionStart, selectionEnd),
						);
						const resultLength = currentActualText.length - selectedActualText.length + pastedText.length;
						if (resultLength > maxMessageLength) {
							event.preventDefault();
							onPasteExceedsLimit(pastedText);
							return;
						}
					}
				}
			}
			const handled = applyPastedText(pastedText, {forceHandlePlainText: hadTabs});
			if (handled) {
				event.preventDefault();
			}
		},
		[
			applyPastedText,
			disabled,
			handlePasteFiles,
			maxMessageLength,
			onPasteExceedsLimit,
			allowExceedingLimit,
			textareaRef,
			segmentManagerRef,
			tryReadAsyncClipboardImageFiles,
		],
	);
	useEffect(() => {
		const textarea = textareaRef.current;
		if (!textarea) return;
		textarea.addEventListener('copy', handleCopy);
		textarea.addEventListener('paste', handlePaste);
		textarea.addEventListener('cut', handleCut);
		return () => {
			textarea.removeEventListener('copy', handleCopy);
			textarea.removeEventListener('paste', handlePaste);
			textarea.removeEventListener('cut', handleCut);
		};
	}, [handleCopy, handlePaste, handleCut, textareaRef]);
	useEffect(() => {
		const handleWindowPaste = (event: ClipboardEvent) => {
			if (disabled) return;
			if (event.defaultPrevented || Modal.hasModalOpen() || isDialogPasteTarget(event.target)) return;
			const textarea = textareaRef.current;
			if (!textarea) return;
			if (document.activeElement === textarea) {
				return;
			}
			if (!isDocumentPasteTarget(document.activeElement)) {
				return;
			}
			const pastedFiles = getClipboardDataFiles(event.clipboardData);
			if (pastedFiles.length > 0) {
				if (!canFocusTextarea(textarea)) {
					return;
				}
				if (handlePasteFiles(pastedFiles)) {
					event.preventDefault();
					safeFocus(textarea, true);
					return;
				}
			}
			const rawPastedText = event.clipboardData?.getData('text/plain');
			if (!rawPastedText) {
				if (canFocusTextarea(textarea)) {
					tryReadAsyncClipboardImageFiles();
				}
				return;
			}
			const pastedText = rawPastedText.replace(/\t/g, '    ');
			if (!canFocusTextarea(textarea)) {
				return;
			}
			if (maxMessageLength != null && onPasteExceedsLimit) {
				if (allowExceedingLimit) {
					if (pastedText.length > maxMessageLength) {
						event.preventDefault();
						onPasteExceedsLimit(pastedText);
						return;
					}
				} else {
					const currentValue = textarea.value;
					const currentActualText = segmentManagerRef.current.displayToActual(currentValue);
					const resultLength = currentActualText.length + pastedText.length;
					if (resultLength > maxMessageLength) {
						event.preventDefault();
						onPasteExceedsLimit(pastedText);
						return;
					}
				}
			}
			event.preventDefault();
			safeFocus(textarea, true);
			applyPastedText(pastedText, {forceHandlePlainText: true});
		};
		window.addEventListener('paste', handleWindowPaste);
		return () => {
			window.removeEventListener('paste', handleWindowPaste);
		};
	}, [
		textareaRef,
		applyPastedText,
		disabled,
		handlePasteFiles,
		maxMessageLength,
		onPasteExceedsLimit,
		allowExceedingLimit,
		segmentManagerRef,
		tryReadAsyncClipboardImageFiles,
	]);
}
