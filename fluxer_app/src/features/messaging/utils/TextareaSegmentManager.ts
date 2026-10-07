// SPDX-License-Identifier: AGPL-3.0-or-later

export interface MentionSegment {
	type: 'user' | 'role' | 'channel' | 'emoji' | 'special';
	id: string;
	displayText: string;
	actualText: string;
	start: number;
	end: number;
}

export interface TextareaTextChange {
	changeStart: number;
	changeEnd: number;
	replacementLength: number;
}

export interface TextareaTextChangeHint {
	selectionStart: number;
	selectionEnd: number;
	inputType?: string;
}

export class TextareaSegmentManager {
	private segments: Array<MentionSegment> = [];

	getSegments(): Array<MentionSegment> {
		return this.segments;
	}

	getSegmentsCopy(): Array<MentionSegment> {
		return this.segments.slice();
	}

	setSegments(segments: Array<MentionSegment>): void {
		this.segments = segments;
	}

	clear(): void {
		this.segments = [];
	}

	displayToActual(displayText: string): string {
		const segments = this.segments;
		if (segments.length === 0) {
			return displayText;
		}
		const sorted = sortedByStart(segments);
		const pieces: Array<string> = [];
		let cursor = 0;
		for (let i = 0; i < sorted.length; i++) {
			const segment = sorted[i];
			if (segment.start > cursor) {
				pieces.push(displayText.slice(cursor, segment.start));
			}
			pieces.push(segment.actualText);
			cursor = segment.end;
		}
		if (cursor < displayText.length) {
			pieces.push(displayText.slice(cursor));
		}
		return pieces.join('');
	}

	updateSegmentsForTextChange(
		changeStart: number,
		changeEnd: number,
		replacementLength: number,
	): Array<MentionSegment> {
		const lengthDelta = replacementLength - (changeEnd - changeStart);
		const previous = this.segments;
		const updated = new Array<MentionSegment>(previous.length);
		let count = 0;
		for (let i = 0; i < previous.length; i++) {
			const segment = previous[i];
			if (segment.end <= changeStart) {
				updated[count++] = segment;
			} else if (segment.start >= changeEnd) {
				if (lengthDelta === 0) {
					updated[count++] = segment;
				} else {
					updated[count++] = {
						...segment,
						start: segment.start + lengthDelta,
						end: segment.end + lengthDelta,
					};
				}
			}
		}
		updated.length = count;
		this.segments = updated;
		return updated;
	}

	insertSegment(
		currentText: string,
		insertPosition: number,
		displayText: string,
		actualText: string,
		type: MentionSegment['type'],
		id: string,
	): {
		newText: string;
		newSegments: Array<MentionSegment>;
	} {
		const newText = currentText.slice(0, insertPosition) + displayText + currentText.slice(insertPosition);
		const newSegment: MentionSegment = {
			type,
			id,
			displayText,
			actualText,
			start: insertPosition,
			end: insertPosition + displayText.length,
		};
		const previous = this.segments;
		const length = displayText.length;
		const next = new Array<MentionSegment>(previous.length + 1);
		for (let i = 0; i < previous.length; i++) {
			const segment = previous[i];
			if (segment.start >= insertPosition) {
				next[i] = {
					...segment,
					start: segment.start + length,
					end: segment.end + length,
				};
			} else {
				next[i] = segment;
			}
		}
		next[previous.length] = newSegment;
		this.segments = next;
		return {newText, newSegments: this.segments};
	}

	static detectChange(oldText: string, newText: string, hint?: TextareaTextChangeHint): TextareaTextChange {
		const hintedChange = hint ? detectChangeFromHint(oldText, newText, hint) : null;
		if (hintedChange) {
			return hintedChange;
		}
		const oldLength = oldText.length;
		const newLength = newText.length;
		let changeStart = 0;
		const minLength = oldLength < newLength ? oldLength : newLength;
		while (changeStart < minLength && oldText.charCodeAt(changeStart) === newText.charCodeAt(changeStart)) {
			changeStart++;
		}
		let oldEnd = oldLength;
		let newEnd = newLength;
		while (
			oldEnd > changeStart &&
			newEnd > changeStart &&
			oldText.charCodeAt(oldEnd - 1) === newText.charCodeAt(newEnd - 1)
		) {
			oldEnd--;
			newEnd--;
		}
		const replacementLength = newEnd - changeStart;
		return {changeStart, changeEnd: oldEnd, replacementLength};
	}
}

function detectChangeFromHint(
	oldText: string,
	newText: string,
	hint: TextareaTextChangeHint,
): TextareaTextChange | null {
	if (isHistoryInputType(hint.inputType)) {
		return null;
	}
	const oldLength = oldText.length;
	const newLength = newText.length;
	const selectionStart = clamp(hint.selectionStart, 0, oldLength);
	const selectionEnd = clamp(hint.selectionEnd, selectionStart, oldLength);
	const selectedLength = selectionEnd - selectionStart;
	const lengthDelta = newLength - oldLength;
	if (selectedLength > 0) {
		const replacementLength = selectedLength + lengthDelta;
		if (replacementLength < 0) {
			return null;
		}
		return normalizeHintedChange(oldText, newText, {
			changeStart: selectionStart,
			changeEnd: selectionEnd,
			replacementLength,
		});
	}
	if (isDeleteInputType(hint.inputType)) {
		const deletedLength = oldLength - newLength;
		if (deletedLength < 0) {
			return null;
		}
		const isBackwardDelete = hint.inputType?.includes('Backward') ?? false;
		return normalizeHintedChange(oldText, newText, {
			changeStart: isBackwardDelete ? selectionStart - deletedLength : selectionStart,
			changeEnd: isBackwardDelete ? selectionStart : selectionStart + deletedLength,
			replacementLength: 0,
		});
	}
	if (isInsertInputType(hint.inputType) || lengthDelta >= 0) {
		return normalizeHintedChange(oldText, newText, {
			changeStart: selectionStart,
			changeEnd: selectionStart,
			replacementLength: lengthDelta,
		});
	}
	return null;
}

function normalizeHintedChange(
	oldText: string,
	newText: string,
	change: TextareaTextChange,
): TextareaTextChange | null {
	const oldLength = oldText.length;
	const newLength = newText.length;
	const changeStart = clamp(change.changeStart, 0, oldLength);
	const changeEnd = clamp(change.changeEnd, changeStart, oldLength);
	const replacementLength = change.replacementLength;
	if (replacementLength < 0) {
		return null;
	}
	const expectedNewLength = oldLength - (changeEnd - changeStart) + replacementLength;
	if (expectedNewLength !== newLength) {
		return null;
	}
	if (oldText.slice(0, changeStart) !== newText.slice(0, changeStart)) {
		return null;
	}
	if (oldText.slice(changeEnd) !== newText.slice(changeStart + replacementLength)) {
		return null;
	}
	return {changeStart, changeEnd, replacementLength};
}

function isHistoryInputType(inputType: string | undefined): boolean {
	return inputType === 'historyUndo' || inputType === 'historyRedo';
}

function isDeleteInputType(inputType: string | undefined): boolean {
	return inputType?.startsWith('delete') ?? false;
}

function isInsertInputType(inputType: string | undefined): boolean {
	return inputType?.startsWith('insert') ?? false;
}

function clamp(value: number, min: number, max: number): number {
	return Math.min(max, Math.max(min, value));
}

function sortedByStart(segments: Array<MentionSegment>): Array<MentionSegment> {
	for (let i = 1; i < segments.length; i++) {
		if (segments[i - 1].start > segments[i].start) {
			return segments.slice().sort(byStart);
		}
	}
	return segments;
}

function byStart(a: MentionSegment, b: MentionSegment): number {
	return a.start - b.start;
}
