// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MentionSegment, TextareaSegmentManager} from '@app/features/messaging/utils/TextareaSegmentManager';
import {replaceTextRange, setTextSelectionSoon} from '@app/features/messaging/utils/TextInputEditUtils';
import type React from 'react';

export type PrepareTextareaTextChange = (nextValue: string, nextSegments: ReadonlyArray<MentionSegment>) => void;

export interface ApplyTextareaTextChangeOptions {
	textareaRef: React.RefObject<HTMLTextAreaElement | null>;
	setValue: React.Dispatch<React.SetStateAction<string>>;
	segmentManagerRef: React.RefObject<TextareaSegmentManager>;
	previousValueRef: React.RefObject<string>;
	prepareTextChange: PrepareTextareaTextChange;
	nextValue: string;
	nextSegments?: ReadonlyArray<MentionSegment>;
	replacementText?: string;
	rangeStart?: number;
	rangeEnd?: number;
	selectionStart?: number;
	selectionEnd?: number;
	focus?: boolean;
}

function cloneSegments(segments: ReadonlyArray<MentionSegment>): Array<MentionSegment> {
	return segments.map((segment) => ({...segment}));
}

export function applyTextareaTextChange({
	textareaRef,
	setValue,
	segmentManagerRef,
	previousValueRef,
	prepareTextChange,
	nextValue,
	nextSegments,
	replacementText,
	rangeStart,
	rangeEnd,
	selectionStart,
	selectionEnd,
	focus = true,
}: ApplyTextareaTextChangeOptions): boolean {
	const textarea = textareaRef.current;
	const resolvedNextSegments = cloneSegments(nextSegments ?? segmentManagerRef.current.getSegments());
	prepareTextChange(nextValue, resolvedNextSegments);
	if (textarea) {
		const start = rangeStart ?? 0;
		const end = rangeEnd ?? textarea.value.length;
		const text = replacementText ?? nextValue;
		const appliedNativeEdit = replaceTextRange(textarea, text, start, end);
		if (appliedNativeEdit) {
			if (focus) {
				textarea.focus({preventScroll: true});
			}
			if (selectionStart != null) {
				setTextSelectionSoon(textarea, selectionStart, selectionEnd ?? selectionStart);
			}
			return true;
		}
	}
	segmentManagerRef.current.setSegments(resolvedNextSegments);
	previousValueRef.current = nextValue;
	setValue(nextValue);
	if (textarea) {
		if (focus) {
			textarea.focus({preventScroll: true});
		}
		if (selectionStart != null) {
			setTextSelectionSoon(textarea, selectionStart, selectionEnd ?? selectionStart);
		}
	}
	return false;
}
