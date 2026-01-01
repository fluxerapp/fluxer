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

type SearchSegmentType = 'user' | 'channel' | 'value';

export interface SearchSegment {
	type: SearchSegmentType;
	filterKey: string;
	id: string;
	displayText: string;
	start: number;
	end: number;
}

export class SearchSegmentManager {
	private segments: Array<SearchSegment> = [];

	getSegments(): Array<SearchSegment> {
		return [...this.segments];
	}

	setSegments(segments: Array<SearchSegment>): void {
		this.segments = [...segments];
	}

	clear(): void {
		this.segments = [];
	}

	getSegmentAt(position: number): SearchSegment | null {
		return this.segments.find((seg) => seg.start <= position && seg.end >= position) || null;
	}

	updateSegmentsForTextChange(changeStart: number, changeEnd: number, replacementLength: number): Array<SearchSegment> {
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
			.filter((s): s is SearchSegment => s !== null);

		this.segments = updated;
		return updated;
	}

	insertSegment(
		currentText: string,
		insertPosition: number,
		filterKey: string,
		filterSyntax: string,
		displayText: string,
		id: string,
		type: SearchSegmentType,
	): {newText: string; newSegments: Array<SearchSegment>} {
		const fullDisplayText = `${filterSyntax}${displayText}`;
		const newText = `${currentText.slice(0, insertPosition) + fullDisplayText} ${currentText.slice(insertPosition)}`;

		const newSegment: SearchSegment = {
			type,
			filterKey,
			id,
			displayText: fullDisplayText,
			start: insertPosition,
			end: insertPosition + fullDisplayText.length,
		};

		const updatedSegments = this.segments.map((segment) => {
			if (segment.start >= insertPosition) {
				const offset = fullDisplayText.length + 1;
				return {
					...segment,
					start: segment.start + offset,
					end: segment.end + offset,
				};
			}
			return segment;
		});

		this.segments = [...updatedSegments, newSegment];
		return {newText, newSegments: this.segments};
	}

	replaceWithSegment(
		currentText: string,
		replaceStart: number,
		replaceEnd: number,
		filterKey: string,
		filterSyntax: string,
		displayText: string,
		id: string,
		type: SearchSegmentType,
	): {newText: string; newSegments: Array<SearchSegment>} {
		const fullDisplayText = `${filterSyntax}${displayText}`;
		const lengthDelta = fullDisplayText.length + 1 - (replaceEnd - replaceStart);

		const newText = `${currentText.slice(0, replaceStart) + fullDisplayText} ${currentText.slice(replaceEnd)}`;

		const newSegment: SearchSegment = {
			type,
			filterKey,
			id,
			displayText: fullDisplayText,
			start: replaceStart,
			end: replaceStart + fullDisplayText.length,
		};

		const updatedSegments = this.segments
			.map((segment) => {
				if (segment.end <= replaceStart) {
					return segment;
				} else if (segment.start >= replaceEnd) {
					return {...segment, start: segment.start + lengthDelta, end: segment.end + lengthDelta};
				} else {
					return null;
				}
			})
			.filter((s): s is SearchSegment => s !== null);

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
