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
		return [...this.segments];
	}

	setSegments(segments: Array<MentionSegment>): void {
		this.segments = [...segments];
	}

	clear(): void {
		this.segments = [];
	}

	displayToActual(displayText: string): string {
		let result = displayText;
		const sortedSegments = [...this.segments].sort((a, b) => b.start - a.start);
		for (const segment of sortedSegments) {
			result = result.slice(0, segment.start) + segment.actualText + result.slice(segment.end);
		}
		return result;
	}

	displayToActualSubstring(displayText: string, start: number, end: number): string {
		const relevantSegments = this.segments
			.filter((segment) => segment.start >= start && segment.end <= end)
			.sort((a, b) => b.start - a.start);

		let result = displayText.slice(start, end);

		for (const segment of relevantSegments) {
			const relativeStart = segment.start - start;
			const relativeEnd = segment.end - start;
			result = result.slice(0, relativeStart) + segment.actualText + result.slice(relativeEnd);
		}

		return result;
	}

	updateSegmentsForTextChange(
		changeStart: number,
		changeEnd: number,
		replacementLength: number,
	): Array<MentionSegment> {
		const lengthDelta = replacementLength - (changeEnd - changeStart);
		const updated = this.segments
			.map((segment) => {
				if (segment.end <= changeStart) {
					return segment;
				} else if (segment.start >= changeEnd) {
					return {...segment, start: segment.start + lengthDelta, end: segment.end + lengthDelta};
				} else {
					return null;
				}
			})
			.filter((s): s is MentionSegment => s !== null);

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
	): {newText: string; newSegments: Array<MentionSegment>} {
		const newText = currentText.slice(0, insertPosition) + displayText + currentText.slice(insertPosition);
		const newSegment: MentionSegment = {
			type,
			id,
			displayText,
			actualText,
			start: insertPosition,
			end: insertPosition + displayText.length,
		};

		const updatedSegments = this.segments.map((segment) => {
			if (segment.start >= insertPosition) {
				return {
					...segment,
					start: segment.start + displayText.length,
					end: segment.end + displayText.length,
				};
			}
			return segment;
		});

		this.segments = [...updatedSegments, newSegment];
		return {newText, newSegments: this.segments};
	}

	static detectChange(
		oldText: string,
		newText: string,
	): {changeStart: number; changeEnd: number; replacementLength: number} {
		let changeStart = 0;
		while (
			changeStart < oldText.length &&
			changeStart < newText.length &&
			oldText[changeStart] === newText[changeStart]
		) {
			changeStart++;
		}

		let oldEnd = oldText.length;
		let newEnd = newText.length;
		while (oldEnd > changeStart && newEnd > changeStart && oldText[oldEnd - 1] === newText[newEnd - 1]) {
			oldEnd--;
			newEnd--;
		}

		const replacementLength = newEnd - changeStart;

		return {changeStart, changeEnd: oldEnd, replacementLength};
	}
}
