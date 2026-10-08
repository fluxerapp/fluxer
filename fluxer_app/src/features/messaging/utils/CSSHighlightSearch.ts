// SPDX-License-Identifier: AGPL-3.0-or-later

const HIGHLIGHT_NAME = 'search-highlight';

export function isHighlightAPISupported(): boolean {
	return typeof CSS !== 'undefined' && 'highlights' in CSS;
}

export function clearHighlights(): void {
	if (!isHighlightAPISupported()) return;
	CSS.highlights.clear();
}

export function findAllTextNodes(container: HTMLElement): Array<Text> {
	const textNodes: Array<Text> = [];
	const walker = container.ownerDocument.createTreeWalker(container, NodeFilter.SHOW_TEXT, {
		acceptNode: (node) =>
			node.textContent && node.textContent.trim().length > 0 ? NodeFilter.FILTER_ACCEPT : NodeFilter.FILTER_REJECT,
	});
	let currentNode = walker.nextNode();
	while (currentNode) {
		textNodes.push(currentNode as Text);
		currentNode = walker.nextNode();
	}
	return textNodes;
}

function normaliseSearchTerms(terms: ReadonlyArray<string>): Array<string> {
	const cleanTerms = new Set<string>();
	for (const term of terms) {
		const cleanTerm = term.trim().toLowerCase();
		if (cleanTerm) {
			cleanTerms.add(cleanTerm);
		}
	}
	return Array.from(cleanTerms);
}

export function createRangesForSearchTerms(textNodes: Array<Text>, searchTerms: ReadonlyArray<string>): Array<Range> {
	const ranges: Array<Range> = [];
	const cleanTerms = normaliseSearchTerms(searchTerms);
	if (cleanTerms.length === 0) return ranges;
	textNodes.forEach((textNode) => {
		const text = textNode.textContent || '';
		if (!text) return;
		const lowerText = text.toLowerCase();
		for (const term of cleanTerms) {
			if (term.length > lowerText.length) continue;
			let startPos = 0;
			while (startPos < lowerText.length) {
				const index = lowerText.indexOf(term, startPos);
				if (index === -1) break;
				const range = textNode.ownerDocument.createRange();
				range.setStart(textNode, index);
				range.setEnd(textNode, index + term.length);
				ranges.push(range);
				startPos = index + term.length;
			}
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
	return createRangesForSearchTerms(textNodes, [cleanQuery]);
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
