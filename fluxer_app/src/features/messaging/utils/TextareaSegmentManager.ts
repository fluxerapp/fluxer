// SPDX-License-Identifier: AGPL-3.0-or-later

export interface MentionSegment {
	type: 'user' | 'role' | 'channel' | 'emoji' | 'special';
	id: string;
	displayText: string;
	actualText: string;
	start: number;
	end: number;
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
