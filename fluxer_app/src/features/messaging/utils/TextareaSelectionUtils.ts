// SPDX-License-Identifier: AGPL-3.0-or-later

type TextareaSelectionDirection = 'forward' | 'backward' | 'none';

export interface TextareaSelectionSnapshot {
	selectionStart: number;
	selectionEnd: number;
	selectionDirection: TextareaSelectionDirection;
}

function clampPosition(position: number, max: number): number {
	if (!Number.isFinite(position)) {
		return max;
	}
	return Math.min(Math.max(Math.trunc(position), 0), max);
}

export function cloneTextareaSelectionSnapshot(snapshot: TextareaSelectionSnapshot): TextareaSelectionSnapshot {
	return {
		selectionStart: snapshot.selectionStart,
		selectionEnd: snapshot.selectionEnd,
		selectionDirection: snapshot.selectionDirection,
	};
}

function normalizeTextareaSelectionSnapshot(
	snapshot: TextareaSelectionSnapshot,
	valueLength: number,
): TextareaSelectionSnapshot {
	const max = clampPosition(valueLength, Number.MAX_SAFE_INTEGER);
	const selectionStart = clampPosition(snapshot.selectionStart, max);
	const selectionEnd = clampPosition(snapshot.selectionEnd, max);
	return {
		selectionStart: Math.min(selectionStart, selectionEnd),
		selectionEnd: Math.max(selectionStart, selectionEnd),
		selectionDirection: snapshot.selectionDirection,
	};
}

function restoreTextareaSelection(textarea: HTMLTextAreaElement, snapshot: TextareaSelectionSnapshot): void {
	const normalized = normalizeTextareaSelectionSnapshot(snapshot, textarea.value.length);
	textarea.setSelectionRange(normalized.selectionStart, normalized.selectionEnd, normalized.selectionDirection);
}

export function focusTextareaWithSelection(
	textarea: HTMLTextAreaElement,
	snapshot: TextareaSelectionSnapshot | null,
	fallbackPosition = textarea.value.length,
): void {
	textarea.focus({preventScroll: true});
	if (snapshot) {
		restoreTextareaSelection(textarea, snapshot);
		return;
	}
	const position = clampPosition(fallbackPosition, textarea.value.length);
	textarea.setSelectionRange(position, position, 'none');
}
