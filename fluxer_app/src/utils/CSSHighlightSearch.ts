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

const HIGHLIGHT_NAME = 'settings-search-highlight';

export function isHighlightAPISupported(): boolean {
	return typeof CSS !== 'undefined' && 'highlights' in CSS;
}

export function clearHighlights(): void {
	if (!isHighlightAPISupported()) return;
	CSS.highlights.clear();
}

function findAllTextNodes(container: HTMLElement): Array<Text> {
	const textNodes: Array<Text> = [];
	const walker = document.createTreeWalker(container, NodeFilter.SHOW_TEXT);

	let currentNode = walker.nextNode();
	while (currentNode) {
		const textNode = currentNode as Text;
		if (textNode.textContent && textNode.textContent.trim().length > 0) {
			textNodes.push(textNode);
		}
		currentNode = walker.nextNode();
	}

	return textNodes;
}

function createRangesForMatches(textNodes: Array<Text>, query: string): Array<Range> {
	const ranges: Array<Range> = [];
	const cleanQuery = query.trim().toLowerCase();

	if (!cleanQuery) return ranges;

	textNodes.forEach((textNode) => {
		const text = textNode.textContent || '';
		const lowerText = text.toLowerCase();

		let startPos = 0;
		while (startPos < lowerText.length) {
			const index = lowerText.indexOf(cleanQuery, startPos);
			if (index === -1) break;

			const range = new Range();
			range.setStart(textNode, index);
			range.setEnd(textNode, index + cleanQuery.length);
			ranges.push(range);

			startPos = index + cleanQuery.length;
		}
	});

	return ranges;
}

export function createRangesForSection(container: HTMLElement, query: string): Array<Range> {
	if (!isHighlightAPISupported()) {
		return [];
	}

	const cleanQuery = query.trim();
	if (!cleanQuery) {
		return [];
	}

	const textNodes = findAllTextNodes(container);
	return createRangesForMatches(textNodes, cleanQuery);
}

export function setHighlightRanges(ranges: Array<Range>): void {
	if (!isHighlightAPISupported()) return;

	CSS.highlights.clear();

	if (ranges.length === 0) {
		return;
	}

	const highlight = new Highlight(...ranges);
	CSS.highlights.set(HIGHLIGHT_NAME, highlight);
}
