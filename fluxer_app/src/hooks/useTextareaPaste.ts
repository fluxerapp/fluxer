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

import React from 'react';
import {canFocusTextarea, safeFocus} from '~/lib/InputFocusManager';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildEmojiRecord} from '~/records/GuildEmojiRecord';
import ChannelStore from '~/stores/ChannelStore';
import EmojiStore from '~/stores/EmojiStore';
import GuildStore from '~/stores/GuildStore';
import UserStore from '~/stores/UserStore';
import {detectPastedSegments, type LookupFunctions} from '~/utils/PasteSegmentUtils';
import type {TextareaSegmentManager} from '~/utils/TextareaSegmentManager';

interface UseTextareaPasteParams {
	channel?: ChannelRecord | null;
	textareaRef: React.RefObject<HTMLTextAreaElement | null>;
	segmentManagerRef: React.MutableRefObject<TextareaSegmentManager>;
	setValue: React.Dispatch<React.SetStateAction<string>>;
	previousValueRef: React.MutableRefObject<string>;
}

export function useTextareaPaste({
	channel,
	textareaRef,
	segmentManagerRef,
	setValue,
	previousValueRef,
}: UseTextareaPasteParams) {
	const applyPastedText = React.useCallback(
		(pastedText: string, opts: {forceHandlePlainText: boolean}): boolean => {
			const textarea = textareaRef.current;
			if (!textarea) return false;

			const currentValue = textarea.value;
			const selectionStart = textarea.selectionStart ?? 0;
			const selectionEnd = textarea.selectionEnd ?? 0;

			const hasMarkdownMention = /<(@|#|@&)\d+>/.test(pastedText);
			const hasEmojiMarkdown = /<(a)?:[^:]+:[a-zA-Z0-9]+>/.test(pastedText);

			const shouldUseSegments = hasMarkdownMention || hasEmojiMarkdown;

			const beforeSelection = currentValue.slice(0, selectionStart);
			const afterSelection = currentValue.slice(selectionEnd);

			if (!shouldUseSegments && opts.forceHandlePlainText) {
				const newText = beforeSelection + pastedText + afterSelection;

				setValue(newText);
				previousValueRef.current = newText;

				const newCursorPosition = beforeSelection.length + pastedText.length;
				setTimeout(() => {
					const t = textareaRef.current;
					if (t) {
						t.setSelectionRange(newCursorPosition, newCursorPosition);
					}
				}, 0);

				return true;
			}

			if (!shouldUseSegments) {
				return false;
			}

			const guildId = channel?.guildId;

			const lookups: LookupFunctions = {
				userById: (id: string) => {
					const user = UserStore.getUser(id);
					return user ? {id: user.id, tag: user.tag} : null;
				},
				channelById: (id: string) => {
					const foundChannel = ChannelStore.getChannel(id);
					return foundChannel?.name ? {id: foundChannel.id, name: foundChannel.name} : null;
				},
				roleById: (id: string) => {
					if (!guildId) return null;
					const roles = GuildStore.getGuildRoles(guildId);
					const role = roles.find((r) => r.id === id);
					return role ? {id: role.id, name: role.name} : null;
				},
				emojiById: (id: string) => {
					if (guildId) {
						const guildEmojis = EmojiStore.getGuildEmoji(guildId);
						const emoji = guildEmojis.find((e: GuildEmojiRecord) => e.id === id);
						if (emoji) return {id: emoji.id, name: emoji.name, uniqueName: emoji.uniqueName};
					}

					const guilds = GuildStore.getGuilds();
					for (const guild of guilds) {
						const emojis = EmojiStore.getGuildEmoji(guild.id);
						const emoji = emojis.find((e: GuildEmojiRecord) => e.id === id);
						if (emoji) return {id: emoji.id, name: emoji.name, uniqueName: emoji.uniqueName};
					}

					return null;
				},
			};

			const segments = detectPastedSegments(pastedText, beforeSelection.length, lookups);

			let newText = beforeSelection;
			let lastEnd = 0;

			for (const segment of segments) {
				const relativeStart = segment.start - beforeSelection.length;
				newText += pastedText.slice(lastEnd, relativeStart);

				const insertPos = newText.length;
				segmentManagerRef.current.insertSegment(
					newText,
					insertPos,
					segment.displayText,
					segment.actualText,
					segment.type,
					segment.id,
				);
				newText += segment.displayText;

				lastEnd = segment.end - beforeSelection.length;
			}

			newText += pastedText.slice(lastEnd);
			newText += afterSelection;

			setValue(newText);
			previousValueRef.current = newText;

			const newCursorPosition = beforeSelection.length + pastedText.length;
			setTimeout(() => {
				const t = textareaRef.current;
				if (t) {
					t.setSelectionRange(newCursorPosition, newCursorPosition);
				}
			}, 0);

			return true;
		},
		[channel, textareaRef, segmentManagerRef, setValue, previousValueRef],
	);

	const handleCopy = React.useCallback(
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

	const handleCut = React.useCallback(
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

			const currentValue = textarea.value;
			const newValue = currentValue.slice(0, selectionStart) + currentValue.slice(selectionEnd);

			segmentManagerRef.current.updateSegmentsForTextChange(selectionStart, selectionEnd, 0);

			setValue(newValue);
			previousValueRef.current = newValue;

			setTimeout(() => {
				const t = textareaRef.current;
				if (t) {
					t.setSelectionRange(selectionStart, selectionStart);
				}
			}, 0);
		},
		[textareaRef, segmentManagerRef, setValue, previousValueRef],
	);

	const handlePaste = React.useCallback(
		(event: ClipboardEvent) => {
			const pastedText = event.clipboardData?.getData('text/plain');
			if (!pastedText) return;

			const handled = applyPastedText(pastedText, {forceHandlePlainText: false});

			if (handled) {
				event.preventDefault();
			}
		},
		[applyPastedText],
	);

	React.useEffect(() => {
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

	React.useEffect(() => {
		const handleWindowPaste = (event: ClipboardEvent) => {
			const textarea = textareaRef.current;
			if (!textarea) return;

			if (document.activeElement === textarea) {
				return;
			}

			const pastedText = event.clipboardData?.getData('text/plain');
			if (!pastedText) return;

			if (!canFocusTextarea(textarea)) {
				return;
			}

			event.preventDefault();
			safeFocus(textarea, true);

			applyPastedText(pastedText, {forceHandlePlainText: true});
		};

		window.addEventListener('paste', handleWindowPaste);
		return () => {
			window.removeEventListener('paste', handleWindowPaste);
		};
	}, [textareaRef, applyPastedText]);
}
