// SPDX-License-Identifier: AGPL-3.0-or-later

import Emoji from '@app/features/emoji/state/Emoji';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import * as EmojiImageUtils from '@app/features/expressions/utils/EmojiUtils';
import {getSkinTonedSurrogate} from '@app/features/expressions/utils/SkinToneUtils';
import type {ComposerHandle, ComposerSelectionRange} from '@app/features/lexical/composer/ComposerHandle';
import type {ComposerInsertPayload, ComposerInsertSpacing} from '@app/features/lexical/composer/composerOffsets';
import {type MentionSegment, TextareaSegmentManager} from '@app/features/messaging/utils/TextareaSegmentManager';

export interface ComposerReplacementPlan {
	start: number;
	end: number;
	payload: ComposerInsertPayload;
	spacing: ComposerInsertSpacing;
	display: string;
	wire: string;
	segments: Array<MentionSegment>;
	selectionAfter: number;
}

export interface ComposerReplacementLimit {
	maxWireLength?: number;
	onExceedMaxLength?: () => void;
}

interface ComposerPayloadSegment {
	type: MentionSegment['type'];
	id: string;
	display: string;
	wire: string;
}

function clampOffset(value: number, fallback: number, maximum: number): number {
	const finiteValue = Number.isFinite(value) ? Math.trunc(value) : fallback;
	return Math.min(maximum, Math.max(0, finiteValue));
}

function normalizeSelection(display: string, selection: ComposerSelectionRange | null): ComposerSelectionRange {
	const fallback = display.length;
	const rawStart = selection == null || selection.start == null ? fallback : selection.start;
	const rawEnd = selection == null || selection.end == null ? rawStart : selection.end;
	const start = clampOffset(Math.min(rawStart, rawEnd), fallback, display.length);
	const end = clampOffset(Math.max(rawStart, rawEnd), start, display.length);
	return {start, end};
}

function getPayloadDisplay(payload: ComposerInsertPayload): string {
	return payload.kind === 'text' ? payload.text : payload.display;
}

function getPayloadSegment(payload: ComposerInsertPayload): ComposerPayloadSegment | null {
	switch (payload.kind) {
		case 'mention':
			return {type: payload.mentionType, id: payload.id, display: payload.display, wire: payload.wire};
		case 'customEmoji':
			return {type: 'emoji', id: payload.emojiId, display: payload.display, wire: payload.wire};
		default:
			return null;
	}
}

export function createComposerReplacementPlan(
	display: string,
	segments: ReadonlyArray<MentionSegment>,
	selection: ComposerSelectionRange | null,
	payload: ComposerInsertPayload,
	spacing: ComposerInsertSpacing = {},
): ComposerReplacementPlan {
	const {start, end} = normalizeSelection(display, selection);
	const leading = spacing.leading == null ? false : spacing.leading;
	const trailing = spacing.trailing == null ? payload.kind !== 'text' : spacing.trailing;
	const prefix = leading ? ' ' : '';
	const suffix = trailing ? ' ' : '';
	const payloadDisplay = getPayloadDisplay(payload);
	const manager = new TextareaSegmentManager();
	manager.setSegments(segments.map((segment) => ({...segment})));
	manager.updateSegmentsForTextChange(start, end, prefix.length);
	const before = display.slice(0, start);
	const after = display.slice(end);
	let nextDisplay = `${before}${prefix}${after}`;
	const insertPosition = start + prefix.length;
	const payloadSegment = getPayloadSegment(payload);
	if (payloadSegment == null) {
		manager.updateSegmentsForTextChange(insertPosition, insertPosition, payloadDisplay.length);
		nextDisplay = `${nextDisplay.slice(0, insertPosition)}${payloadDisplay}${nextDisplay.slice(insertPosition)}`;
	} else {
		nextDisplay = manager.insertSegment(
			nextDisplay,
			insertPosition,
			payloadSegment.display,
			payloadSegment.wire,
			payloadSegment.type,
			payloadSegment.id,
		).newText;
	}
	const suffixPosition = insertPosition + payloadDisplay.length;
	manager.updateSegmentsForTextChange(suffixPosition, suffixPosition, suffix.length);
	nextDisplay = `${nextDisplay.slice(0, suffixPosition)}${suffix}${nextDisplay.slice(suffixPosition)}`;
	return {
		start,
		end,
		payload,
		spacing: {leading, trailing},
		display: nextDisplay,
		wire: manager.displayToActual(nextDisplay),
		segments: manager.getSegmentsCopy(),
		selectionAfter: suffixPosition + suffix.length,
	};
}

export function createComposerEmojiPayload(emoji: FlatEmoji): ComposerInsertPayload {
	const wire = Emoji.getEmojiMarkdown(emoji);
	if (emoji.id) {
		return {
			kind: 'customEmoji',
			emojiId: emoji.id,
			animated: emoji.animated == null ? false : emoji.animated,
			display: `:${emoji.name}:`,
			wire,
		};
	}
	const surrogate = getSkinTonedSurrogate(emoji);
	if (surrogate) {
		return {
			kind: 'standardEmoji',
			name: emoji.name,
			surrogate,
			url: EmojiImageUtils.getEmojiURL(surrogate),
			display: wire,
		};
	}
	return {kind: 'text', text: wire};
}

export function applyComposerReplacement(
	handle: ComposerHandle,
	selection: ComposerSelectionRange | null,
	payload: ComposerInsertPayload,
	spacing: ComposerInsertSpacing,
	limit: ComposerReplacementLimit = {},
): boolean {
	const plan = createComposerReplacementPlan(
		handle.getDisplayValue(),
		handle.getSegments(),
		selection,
		payload,
		spacing,
	);
	if (limit.maxWireLength != null && plan.wire.length > limit.maxWireLength) {
		if (limit.onExceedMaxLength != null) {
			limit.onExceedMaxLength();
		}
		return false;
	}
	handle.replaceRange(plan.start, plan.end, plan.payload, plan.spacing);
	return true;
}

export function insertComposerEmoji(
	handle: ComposerHandle | null,
	emoji: FlatEmoji,
	limit: ComposerReplacementLimit = {},
): boolean {
	if (handle == null) {
		return false;
	}
	const display = handle.getDisplayValue();
	const selection = normalizeSelection(display, handle.getSelection());
	const charBefore = selection.start > 0 ? display[selection.start - 1] : '';
	const charAfter = selection.end < display.length ? display[selection.end] : '';
	return applyComposerReplacement(
		handle,
		selection,
		createComposerEmojiPayload(emoji),
		{
			leading: charBefore !== '' && !/\s/.test(charBefore),
			trailing: charAfter === '' || !/\s/.test(charAfter),
		},
		limit,
	);
}
