// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ExpressionPickerCommands from '@app/features/emoji/commands/ExpressionPickerCommands';
import Emoji from '@app/features/emoji/state/Emoji';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {
	applyTextareaTextChange,
	type PrepareTextareaTextChange,
} from '@app/features/messaging/utils/TextareaNativeEditUtils';
import {TextareaSegmentManager} from '@app/features/messaging/utils/TextareaSegmentManager';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import {useCallback} from 'react';

interface UseTextareaEmojiPickerReturn {
	handleEmojiSelect: (emoji: FlatEmoji, shiftKey?: boolean) => boolean;
}

interface UseTextareaEmojiPickerParams {
	setValue: React.Dispatch<React.SetStateAction<string>>;
	textareaRef: React.RefObject<HTMLTextAreaElement | null>;
	segmentManagerRef: React.RefObject<TextareaSegmentManager>;
	previousValueRef: React.RefObject<string>;
	prepareTextChange: PrepareTextareaTextChange;
	channelId?: string;
	maxActualLength?: number;
	onExceedMaxLength?: () => void;
}

export function useTextareaEmojiPicker({
	setValue,
	textareaRef,
	segmentManagerRef,
	previousValueRef,
	prepareTextChange,
	channelId,
	maxActualLength,
	onExceedMaxLength,
}: UseTextareaEmojiPickerParams): UseTextareaEmojiPickerReturn {
	const handleEmojiSelect = useCallback(
		(emoji: FlatEmoji, shiftKey?: boolean) => {
			const actualText = Emoji.getEmojiMarkdown(emoji);
			const displayText = `:${emoji.name}:`;
			const textarea = textareaRef.current;
			const rawSelectionStart = textarea?.selectionStart ?? null;
			const rawSelectionEnd = textarea?.selectionEnd ?? null;
			let nextCursorPosition: number | null = null;
			let didInsert = false;
			let didRejectForMaxLength = false;
			const prevValue = textarea?.value ?? previousValueRef.current;
			let nextValue = prevValue;
			let nextSegments = segmentManagerRef.current.getSegmentsCopy();
			const segmentManager = new TextareaSegmentManager();
			segmentManager.setSegments(segmentManagerRef.current.getSegmentsCopy());
			{
				const rawStart = rawSelectionStart ?? prevValue.length;
				const rawEnd = rawSelectionEnd ?? rawStart;
				const clampedStart = Math.min(prevValue.length, Math.max(0, Math.min(rawStart, rawEnd)));
				const clampedEnd = Math.min(prevValue.length, Math.max(0, Math.max(rawStart, rawEnd)));
				const beforeSelection = prevValue.slice(0, clampedStart);
				const afterSelection = prevValue.slice(clampedEnd);
				const needsSpaceBefore = beforeSelection.length > 0 && !/\s$/.test(beforeSelection);
				const needsSpaceAfter = afterSelection.length === 0 || !/^\s/.test(afterSelection);
				const prefix = needsSpaceBefore ? ' ' : '';
				const suffix = needsSpaceAfter ? ' ' : '';
				const displayInsertText = `${prefix}${displayText}${suffix}`;
				const segmentsBefore = maxActualLength != null ? segmentManager.getSegmentsCopy() : null;
				segmentManager.updateSegmentsForTextChange(clampedStart, clampedEnd, prefix.length);
				const withoutSelection = beforeSelection + prefix + afterSelection;
				const insertPosition = clampedStart + prefix.length;
				const {newText} = segmentManager.insertSegment(
					withoutSelection,
					insertPosition,
					displayText,
					actualText,
					'emoji',
					emoji.id ?? emoji.uniqueName,
				);
				const suffixInsertPosition = insertPosition + displayText.length;
				segmentManager.updateSegmentsForTextChange(suffixInsertPosition, suffixInsertPosition, suffix.length);
				const nextText = newText.slice(0, suffixInsertPosition) + suffix + newText.slice(suffixInsertPosition);
				if (maxActualLength != null) {
					const candidateActualText = segmentManager.displayToActual(nextText);
					if (candidateActualText.length > maxActualLength) {
						didRejectForMaxLength = true;
						if (segmentsBefore) {
							segmentManager.setSegments(segmentsBefore);
						}
						nextSegments = segmentManager.getSegmentsCopy();
						nextValue = prevValue;
					} else {
						nextValue = nextText;
						nextSegments = segmentManager.getSegmentsCopy();
						nextCursorPosition = clampedStart + displayInsertText.length;
						didInsert = true;
					}
				} else {
					nextValue = nextText;
					nextSegments = segmentManager.getSegmentsCopy();
					nextCursorPosition = clampedStart + displayInsertText.length;
					didInsert = true;
				}
			}
			if (didRejectForMaxLength) {
				onExceedMaxLength?.();
				return false;
			}
			applyTextareaTextChange({
				textareaRef,
				setValue,
				segmentManagerRef,
				previousValueRef,
				prepareTextChange,
				nextValue,
				nextSegments,
				selectionStart: nextCursorPosition ?? nextValue.length,
			});
			if (didInsert && !shiftKey && channelId) {
				ExpressionPickerCommands.close();
				PopoutCommands.close(`expression-picker-${channelId}`);
			}
			return didInsert;
		},
		[
			segmentManagerRef,
			setValue,
			textareaRef,
			previousValueRef,
			prepareTextChange,
			channelId,
			maxActualLength,
			onExceedMaxLength,
		],
	);
	return {
		handleEmojiSelect,
	};
}
