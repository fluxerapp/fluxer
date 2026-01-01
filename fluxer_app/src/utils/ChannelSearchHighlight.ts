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

const HIGHLIGHT_NAME = 'channel-search-highlight';

function isHighlightAPISupported(): boolean {
	return typeof CSS !== 'undefined' && 'highlights' in CSS;
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

function createRangesForSearchTerms(textNodes: Array<Text>, searchTerms: Array<string>): Array<Range> {
	const ranges: Array<Range> = [];

	const cleanTerms = searchTerms.map((term) => term.trim().toLowerCase()).filter((term) => term.length > 0);

	if (cleanTerms.length === 0) return ranges;

	for (const textNode of textNodes) {
		const text = textNode.textContent || '';
		const lowerText = text.toLowerCase();

		for (const term of cleanTerms) {
			let startPos = 0;
			while (startPos < lowerText.length) {
				const index = lowerText.indexOf(term, startPos);
				if (index === -1) break;

				const range = new Range();
				range.setStart(textNode, index);
				range.setEnd(textNode, index + term.length);
				ranges.push(range);

				startPos = index + term.length;
			}
		}
	}

	return ranges;
}

export function applyChannelSearchHighlight(container: HTMLElement, searchTerms: Array<string>): void {
	if (!isHighlightAPISupported()) return;

	CSS.highlights.delete(HIGHLIGHT_NAME);

	if (searchTerms.length === 0) return;

	const textNodes = findAllTextNodes(container);
	const ranges = createRangesForSearchTerms(textNodes, searchTerms);

	if (ranges.length === 0) return;

	const highlight = new Highlight(...ranges);
	CSS.highlights.set(HIGHLIGHT_NAME, highlight);
}

export function clearChannelSearchHighlight(): void {
	if (!isHighlightAPISupported()) return;
	CSS.highlights.delete(HIGHLIGHT_NAME);
}
