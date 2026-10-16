// SPDX-License-Identifier: AGPL-3.0-or-later

import {escapeRegex} from '@app/features/messaging/utils/RegexUtils';

const RANGE_START_TO_START = 0;
const RANGE_START_TO_END = 1;
const RANGE_END_TO_END = 2;
const MESSAGE_ROW_SELECTOR = '[data-message-id][data-is-group-start]';
const MESSAGE_SELECTION_ROOT_SELECTOR = '[data-message-selection-root="true"]';
const COPY_HIDDEN_SELECTOR = '[data-message-copy-hidden="true"]';
const COPY_TEXT_BLOCK_SELECTOR = '[data-message-copy-block="true"][data-message-copy-text]';

interface MessageSelectionCopyOptions {
	rootElement: HTMLElement;
	selection: Selection | null;
	getMessagePlaintext?: (messageId: string) => string | null;
}

interface MessageSelectionCopyRangeOptions {
	rootElement: HTMLElement;
	selectionRange: Range;
	getMessagePlaintext?: (messageId: string) => string | null;
}

interface PreparedRowText {
	text: string;
	isGroupStart: boolean;
	canOverrideSingleRowCopy: boolean;
}

interface RowSelectionInfo {
	range: Range;
	isEntireRowSelected: boolean;
}

interface HeaderInfo {
	headerElements: Array<HTMLElement>;
	username: string;
	timestamp: string;
	style: 'block' | 'inline';
}

interface SelectedCopyMetadata {
	copyBlocks: Array<HTMLElement>;
	hiddenElements: Array<HTMLElement>;
}

export function buildMessageSelectionCopyText(options: MessageSelectionCopyOptions): string | null {
	const {rootElement, selection, getMessagePlaintext} = options;
	if (!isSelectionInsideRoot(selection, rootElement)) {
		return null;
	}
	const selectionRange = getSelectionRange(selection);
	if (!selectionRange) {
		return null;
	}
	return buildMessageSelectionCopyTextForRange({
		rootElement,
		selectionRange,
		getMessagePlaintext,
	});
}

export function buildMessageSelectionCopyTextForRange(options: MessageSelectionCopyRangeOptions): string | null {
	const {rootElement, selectionRange, getMessagePlaintext} = options;
	const selectedRows = getSelectedMessageRows(rootElement, selectionRange);
	if (selectedRows.length === 0) {
		return null;
	}
	const preparedRows: Array<PreparedRowText> = [];
	for (const row of selectedRows) {
		const preparedRow = prepareRowText(row, selectionRange, getMessagePlaintext);
		if (preparedRow && preparedRow.text.length > 0) {
			preparedRows.push(preparedRow);
		}
	}
	if (preparedRows.length === 0) {
		return '';
	}
	if (preparedRows.length < 2) {
		const [singleRow] = preparedRows;
		return singleRow?.canOverrideSingleRowCopy ? singleRow.text : null;
	}
	return joinPreparedRows(preparedRows);
}

export function getMessageSelectionRoot(node: Node | null): HTMLElement | null {
	const element = node?.nodeType === Node.ELEMENT_NODE ? (node as HTMLElement) : node?.parentElement;
	return element?.closest<HTMLElement>(MESSAGE_SELECTION_ROOT_SELECTOR) ?? null;
}

function isSelectionInsideRoot(selection: Selection | null, rootElement: HTMLElement): boolean {
	if (!selection) {
		return false;
	}
	const anchorNode = selection.anchorNode;
	const focusNode = selection.focusNode;
	if (!anchorNode || !focusNode) {
		return false;
	}
	return rootElement.contains(anchorNode) && rootElement.contains(focusNode);
}

function getSelectionRange(selection: Selection | null): Range | null {
	if (!selection || selection.rangeCount === 0 || selection.isCollapsed) {
		return null;
	}
	const range = selection.getRangeAt(0);
	if (range.collapsed) {
		return null;
	}
	return range;
}

function getSelectedMessageRows(rootElement: HTMLElement, selectionRange: Range): Array<HTMLElement> {
	const startRow = getBoundaryMessageRow(rootElement, selectionRange, 'start');
	const endRow = getBoundaryMessageRow(rootElement, selectionRange, 'end');
	if (!startRow || !endRow) {
		return getSelectedMessageRowsByTreeWalk(rootElement, selectionRange);
	}
	const orderedStartRow = compareNodeOrder(startRow, endRow) <= 0 ? startRow : endRow;
	const orderedEndRow = orderedStartRow === startRow ? endRow : startRow;
	const selectedRows: Array<HTMLElement> = [];
	walkMessageRowsFrom(rootElement, orderedStartRow, (row) => {
		if (rangeIntersectsNode(selectionRange, row)) {
			selectedRows.push(row);
		}
		return row !== orderedEndRow;
	});
	return selectedRows;
}

function getSelectedMessageRowsByTreeWalk(rootElement: HTMLElement, selectionRange: Range): Array<HTMLElement> {
	const selectedRows: Array<HTMLElement> = [];
	walkMessageRowsFrom(rootElement, rootElement, (row) => {
		if (rangeIntersectsNode(selectionRange, row)) {
			selectedRows.push(row);
			return true;
		}
		return !isNodeAfterRange(row, selectionRange);
	});
	return selectedRows;
}

function getBoundaryMessageRow(
	rootElement: HTMLElement,
	selectionRange: Range,
	boundary: 'start' | 'end',
): HTMLElement | null {
	const container = boundary === 'start' ? selectionRange.startContainer : selectionRange.endContainer;
	const offset = boundary === 'start' ? selectionRange.startOffset : selectionRange.endOffset;
	const containingRow = getContainingMessageRow(rootElement, container);
	if (containingRow) {
		return containingRow;
	}
	if (container.nodeType !== Node.ELEMENT_NODE) {
		return null;
	}
	const containerElement = container as HTMLElement;
	if (!rootElement.contains(containerElement)) {
		return null;
	}
	const boundaryNode =
		boundary === 'start' ? container.childNodes[offset] : container.childNodes[Math.max(0, offset - 1)];
	if (!boundaryNode) {
		return null;
	}
	return boundary === 'start' ? findFirstMessageRowInSubtree(boundaryNode) : findLastMessageRowInSubtree(boundaryNode);
}

function getContainingMessageRow(rootElement: HTMLElement, node: Node): HTMLElement | null {
	const element = node.nodeType === Node.ELEMENT_NODE ? (node as HTMLElement) : node.parentElement;
	const row = element?.closest<HTMLElement>(MESSAGE_ROW_SELECTOR) ?? null;
	return row && rootElement.contains(row) ? row : null;
}

function findFirstMessageRowInSubtree(node: Node): HTMLElement | null {
	if (node.nodeType !== Node.ELEMENT_NODE) {
		return null;
	}
	const element = node as HTMLElement;
	if (element.matches(MESSAGE_ROW_SELECTOR)) {
		return element;
	}
	return element.querySelector<HTMLElement>(MESSAGE_ROW_SELECTOR);
}

function findLastMessageRowInSubtree(node: Node): HTMLElement | null {
	if (node.nodeType !== Node.ELEMENT_NODE) {
		return null;
	}
	const element = node as HTMLElement;
	if (element.matches(MESSAGE_ROW_SELECTOR)) {
		return element;
	}
	const rows = element.querySelectorAll<HTMLElement>(MESSAGE_ROW_SELECTOR);
	return rows.length > 0 ? (rows[rows.length - 1] ?? null) : null;
}

function walkMessageRowsFrom(
	rootElement: HTMLElement,
	startNode: HTMLElement,
	visit: (row: HTMLElement) => boolean,
): void {
	const treeWalker = rootElement.ownerDocument.createTreeWalker(rootElement, NodeFilter.SHOW_ELEMENT, {
		acceptNode: (node) =>
			node instanceof HTMLElement && node.matches(MESSAGE_ROW_SELECTOR)
				? NodeFilter.FILTER_ACCEPT
				: NodeFilter.FILTER_SKIP,
	});
	treeWalker.currentNode = startNode;
	if (startNode.matches(MESSAGE_ROW_SELECTOR) && !visit(startNode)) {
		return;
	}
	let nextNode = treeWalker.nextNode();
	while (nextNode) {
		if (!visit(nextNode as HTMLElement)) {
			return;
		}
		nextNode = treeWalker.nextNode();
	}
}

function compareNodeOrder(first: Node, second: Node): number {
	if (first === second) {
		return 0;
	}
	return first.compareDocumentPosition(second) & Node.DOCUMENT_POSITION_PRECEDING ? 1 : -1;
}

function isNodeAfterRange(node: Node, range: Range): boolean {
	const ownerDocument = node.ownerDocument;
	if (!ownerDocument) {
		return false;
	}
	const nodeRange = ownerDocument.createRange();
	nodeRange.selectNodeContents(node);
	return nodeRange.compareBoundaryPoints(RANGE_START_TO_END, range) > 0;
}

function rangeIntersectsNode(range: Range, node: Node): boolean {
	try {
		return range.intersectsNode(node);
	} catch {
		return false;
	}
}

function prepareRowText(
	row: HTMLElement,
	selectionRange: Range,
	getMessagePlaintext?: (messageId: string) => string | null,
): PreparedRowText | null {
	const rowSelection = getRowSelectionInfo(row, selectionRange);
	if (!rowSelection) {
		return null;
	}
	const copyMetadata = getSelectedCopyMetadata(rowSelection.range);
	const hasExplicitCopyText = copyMetadata.copyBlocks.length > 0;
	const hasHiddenCopyText = copyMetadata.hiddenElements.length > 0;
	const rawRowText = sanitiseCopiedText(getCopyableRangeText(rowSelection.range, copyMetadata));
	if (!rawRowText.trim()) {
		return null;
	}
	const isGroupStart = isGroupStartRow(row);
	const messagePlaintext = getResolvedMessagePlaintext(row, rowSelection.isEntireRowSelected, getMessagePlaintext);
	const headerInfo = isGroupStart ? getHeaderInfo(row) : getInlineHeaderInfo(row);
	const headerIncluded = headerInfo ? isHeaderIncluded(rowSelection.range, headerInfo) : false;
	if (headerInfo && headerIncluded) {
		const rowBodyWithoutHeader = messagePlaintext ?? removeHeaderPrefix(rawRowText, headerInfo);
		const normalisedHeader = formatHeaderInfo(headerInfo);
		const rowText = formatHeaderWithBody(headerInfo, normalisedHeader, rowBodyWithoutHeader);
		return {
			text: rowText,
			isGroupStart,
			canOverrideSingleRowCopy: true,
		};
	}
	return {
		text: messagePlaintext ?? rawRowText,
		isGroupStart,
		canOverrideSingleRowCopy: rowSelection.isEntireRowSelected || hasExplicitCopyText || hasHiddenCopyText,
	};
}

function getSelectedCopyMetadata(range: Range): SelectedCopyMetadata {
	return {
		copyBlocks: getSelectedCopyBlocks(range),
		hiddenElements: getSelectedElements(range, COPY_HIDDEN_SELECTOR),
	};
}

function getCopyableRangeText(range: Range, metadata = getSelectedCopyMetadata(range)): string {
	const {copyBlocks: selectedCopyBlocks, hiddenElements} = metadata;
	if (selectedCopyBlocks.length > 0 && !hasSelectedVisibleTextOutsideElements(range, selectedCopyBlocks)) {
		return selectedCopyBlocks.map(getCopyBlockText).filter(Boolean).join('\n\n');
	}
	let text = range.toString();
	for (const element of hiddenElements) {
		text = removeHiddenCopyText(text, getSelectedTextForNode(range, element));
	}
	for (const element of selectedCopyBlocks) {
		text = replaceSelectedCopyText(text, getSelectedTextForNode(range, element), getCopyBlockText(element));
	}
	return text;
}

function removeHiddenCopyText(text: string, hiddenText: string): string {
	const candidates = [hiddenText, hiddenText.trim()].filter((candidate) => candidate.length > 0);
	for (const candidate of candidates) {
		const index = text.lastIndexOf(candidate);
		if (index >= 0) {
			return text.slice(0, index) + text.slice(index + candidate.length);
		}
	}
	return text;
}

function replaceSelectedCopyText(text: string, selectedText: string, copyText: string): string {
	if (!selectedText || !copyText) {
		return text;
	}
	const candidates = [selectedText, selectedText.trim()].filter((candidate) => candidate.length > 0);
	for (const candidate of candidates) {
		const index = text.indexOf(candidate);
		if (index >= 0) {
			return text.slice(0, index) + copyText + text.slice(index + candidate.length);
		}
	}
	return text;
}

function getCopyBlockText(element: HTMLElement): string {
	return element.dataset.messageCopyText?.trim() ?? '';
}

function getSelectedCopyBlocks(range: Range): Array<HTMLElement> {
	const selectedElements = getSelectedElements(range, COPY_TEXT_BLOCK_SELECTOR).filter((element) =>
		isCopyBlockSelected(range, element),
	);
	const selectedElementSet = new Set(selectedElements);
	return selectedElements.filter((element) => !hasSelectedCopyBlockAncestor(element, selectedElementSet));
}

function hasSelectedCopyBlockAncestor(element: HTMLElement, selectedElements: ReadonlySet<HTMLElement>): boolean {
	let current = element.parentElement?.closest<HTMLElement>(COPY_TEXT_BLOCK_SELECTOR) ?? null;
	while (current) {
		if (selectedElements.has(current)) {
			return true;
		}
		current = current.parentElement?.closest<HTMLElement>(COPY_TEXT_BLOCK_SELECTOR) ?? null;
	}
	return false;
}

function getSelectedElements(range: Range, selector: string): Array<HTMLElement> {
	const candidates = new Set<HTMLElement>();
	const startElement = getElementForNode(range.startContainer);
	const endElement = getElementForNode(range.endContainer);
	const commonElement = getElementForNode(range.commonAncestorContainer);
	const scope =
		startElement?.closest<HTMLElement>(MESSAGE_ROW_SELECTOR) ??
		endElement?.closest<HTMLElement>(MESSAGE_ROW_SELECTOR) ??
		commonElement;
	for (const element of [startElement, endElement, commonElement]) {
		let current: HTMLElement | null = element;
		while (current && (!scope || scope.contains(current) || current === scope)) {
			if (current.matches(selector)) {
				candidates.add(current);
			}
			current = current.parentElement;
		}
	}
	if (scope) {
		for (const element of scope.querySelectorAll<HTMLElement>(selector)) {
			candidates.add(element);
		}
	}
	return Array.from(candidates).filter((element) => rangeIntersectsNode(range, element));
}

function getElementForNode(node: Node): HTMLElement | null {
	return node.nodeType === Node.ELEMENT_NODE ? (node as HTMLElement) : node.parentElement;
}

function isCopyBlockSelected(range: Range, element: HTMLElement): boolean {
	const selectedText = collapseWhitespace(getSelectedTextForNode(range, element));
	if (!selectedText && getCopyBlockText(element)) {
		return isNodeEntirelySelected(range, element);
	}
	if (isNodeEntirelySelected(range, element)) {
		return true;
	}
	if (element.dataset.messageCopyTable === 'true' && isTableCopyBlockSelected(range, element)) {
		return true;
	}
	return selectedText.length > 0 && selectedText === collapseWhitespace(getTextContentWithoutAriaHidden(element));
}

function isTableCopyBlockSelected(range: Range, element: HTMLElement): boolean {
	const cells = element.querySelectorAll<HTMLElement>('th, td');
	if (cells.length === 0) {
		return false;
	}
	for (const cell of cells) {
		if (!rangeIntersectsNode(range, cell)) {
			return false;
		}
		const fullCellText = collapseWhitespace(getTextContentWithoutAriaHidden(cell));
		if (!fullCellText) {
			continue;
		}
		if (collapseWhitespace(getSelectedTextForNode(range, cell)) !== fullCellText) {
			return false;
		}
	}
	return true;
}

function isNodeEntirelySelected(range: Range, node: Node): boolean {
	const ownerDocument = node.ownerDocument;
	if (!ownerDocument) {
		return false;
	}
	const nodeRange = ownerDocument.createRange();
	nodeRange.selectNodeContents(node);
	return (
		range.compareBoundaryPoints(RANGE_START_TO_START, nodeRange) <= 0 &&
		range.compareBoundaryPoints(RANGE_END_TO_END, nodeRange) >= 0
	);
}

function getSelectedTextForNode(range: Range, node: Node): string {
	const intersectionRange = getIntersectionRange(range, node);
	return intersectionRange?.toString() ?? '';
}

function getIntersectionRange(range: Range, node: Node): Range | null {
	if (!rangeIntersectsNode(range, node)) {
		return null;
	}
	const ownerDocument = node.ownerDocument;
	if (!ownerDocument) {
		return null;
	}
	const nodeRange = ownerDocument.createRange();
	nodeRange.selectNodeContents(node);
	const intersectionRange = range.cloneRange();
	if (range.compareBoundaryPoints(RANGE_START_TO_START, nodeRange) <= 0) {
		intersectionRange.setStart(nodeRange.startContainer, nodeRange.startOffset);
	}
	if (range.compareBoundaryPoints(RANGE_END_TO_END, nodeRange) >= 0) {
		intersectionRange.setEnd(nodeRange.endContainer, nodeRange.endOffset);
	}
	return intersectionRange.collapsed ? null : intersectionRange;
}

function hasSelectedVisibleTextOutsideElements(range: Range, excludedElements: Array<HTMLElement>): boolean {
	const root =
		getElementForNode(range.startContainer)?.closest<HTMLElement>(MESSAGE_ROW_SELECTOR) ??
		getElementForNode(range.endContainer)?.closest<HTMLElement>(MESSAGE_ROW_SELECTOR) ??
		getElementForNode(range.commonAncestorContainer)?.closest<HTMLElement>(MESSAGE_ROW_SELECTOR);
	if (!root) {
		return false;
	}
	const excludedElementSet = new Set(excludedElements);
	const treeWalker = root.ownerDocument.createTreeWalker(root, NodeFilter.SHOW_TEXT, {
		acceptNode: (node) => {
			if (!rangeIntersectsNode(range, node)) {
				return NodeFilter.FILTER_REJECT;
			}
			const parentElement = node.parentElement;
			if (!parentElement) {
				return NodeFilter.FILTER_REJECT;
			}
			if (parentElement.closest(COPY_HIDDEN_SELECTOR)) {
				return NodeFilter.FILTER_REJECT;
			}
			if (hasAncestorInSet(parentElement, excludedElementSet)) {
				return NodeFilter.FILTER_REJECT;
			}
			return NodeFilter.FILTER_ACCEPT;
		},
	});
	let current = treeWalker.nextNode();
	while (current) {
		if (getSelectedTextForNode(range, current).trim()) {
			return true;
		}
		current = treeWalker.nextNode();
	}
	return false;
}

function hasAncestorInSet(element: HTMLElement, ancestors: ReadonlySet<HTMLElement>): boolean {
	let current: HTMLElement | null = element;
	while (current) {
		if (ancestors.has(current)) {
			return true;
		}
		current = current.parentElement;
	}
	return false;
}

function getResolvedMessagePlaintext(
	row: HTMLElement,
	isEntireRowSelected: boolean,
	getMessagePlaintext?: (messageId: string) => string | null,
): string | null {
	if (!getMessagePlaintext || !isEntireRowSelected) {
		return null;
	}
	const messageId = row.dataset.messageId;
	if (!messageId) {
		return null;
	}
	const plaintext = getMessagePlaintext(messageId);
	return plaintext == null ? null : plaintext;
}

function getRowSelectionInfo(row: HTMLElement, selectionRange: Range): RowSelectionInfo | null {
	const rowRange = row.ownerDocument.createRange();
	rowRange.selectNodeContents(row);
	const startsBeforeOrAtRow = selectionRange.compareBoundaryPoints(RANGE_START_TO_START, rowRange) <= 0;
	const endsAfterOrAtRow = selectionRange.compareBoundaryPoints(RANGE_END_TO_END, rowRange) >= 0;
	if (startsBeforeOrAtRow && endsAfterOrAtRow) {
		return {
			range: rowRange,
			isEntireRowSelected: true,
		};
	}
	const intersectionRange = selectionRange.cloneRange();
	if (startsBeforeOrAtRow) {
		intersectionRange.setStart(rowRange.startContainer, rowRange.startOffset);
	}
	if (endsAfterOrAtRow) {
		intersectionRange.setEnd(rowRange.endContainer, rowRange.endOffset);
	}
	if (intersectionRange.collapsed) {
		return null;
	}
	return {
		range: intersectionRange,
		isEntireRowSelected: false,
	};
}

function sanitiseCopiedText(text: string): string {
	return normaliseLineEndings(text)
		.replace(/\u00a0/gu, ' ')
		.replace(/[ \t]+\n/gu, '\n')
		.trimEnd();
}

function isGroupStartRow(row: HTMLElement): boolean {
	return row.dataset.isGroupStart === 'true';
}

function getHeaderInfo(row: HTMLElement): HeaderInfo | null {
	return getBlockHeaderInfo(row) ?? getInlineHeaderInfo(row);
}

function getBlockHeaderInfo(row: HTMLElement): HeaderInfo | null {
	const headerElement = findHeaderElement(row);
	if (!headerElement) {
		return null;
	}
	const usernameElement = headerElement.querySelector<HTMLElement>('[data-user-id]');
	const timestampElement = headerElement.querySelector<HTMLElement>('time');
	if (!usernameElement || !timestampElement) {
		return null;
	}
	const username = collapseWhitespace(getTextContentWithoutAriaHidden(usernameElement));
	const timestamp = collapseWhitespace(getTextContentWithoutAriaHidden(timestampElement));
	if (!username || !timestamp) {
		return null;
	}
	return {
		headerElements: [headerElement],
		username,
		timestamp,
		style: 'block',
	};
}

function getInlineHeaderInfo(row: HTMLElement): HeaderInfo | null {
	const prefixElement = row.querySelector<HTMLElement>('[data-compact-message-prefix="true"]');
	if (!prefixElement) {
		return null;
	}
	const usernameElement = prefixElement.querySelector<HTMLElement>('[data-user-id]');
	const timestampElement = findInlineTimestampElement(row, prefixElement);
	if (!usernameElement || !timestampElement) {
		return null;
	}
	const username = collapseWhitespace(getTextContentWithoutAriaHidden(usernameElement));
	const timestamp = collapseWhitespace(getTextContentWithoutAriaHidden(timestampElement));
	if (!username || !timestamp) {
		return null;
	}
	return {
		headerElements: [timestampElement, prefixElement],
		username,
		timestamp,
		style: 'inline',
	};
}

function findInlineTimestampElement(row: HTMLElement, prefixElement: HTMLElement): HTMLElement | null {
	let firstTimestamp: HTMLElement | null = null;
	for (const timestampElement of row.querySelectorAll<HTMLElement>('time')) {
		firstTimestamp ??= timestampElement;
		if (compareNodeOrder(timestampElement, prefixElement) < 0) {
			return timestampElement;
		}
	}
	return firstTimestamp;
}

function isHeaderIncluded(range: Range, headerInfo: HeaderInfo): boolean {
	return headerInfo.headerElements.some((element) => rangeIntersectsNode(range, element));
}

function getTextContentWithoutAriaHidden(element: HTMLElement): string {
	let text = '';
	for (const childNode of element.childNodes) {
		if (childNode.nodeType === Node.TEXT_NODE) {
			text += childNode.textContent ?? '';
			continue;
		}
		if (childNode.nodeType !== Node.ELEMENT_NODE) {
			continue;
		}
		const childElement = childNode as HTMLElement;
		if (childElement.getAttribute('aria-hidden') === 'true') {
			continue;
		}
		text += getTextContentWithoutAriaHidden(childElement);
	}
	return text;
}

function findHeaderElement(row: HTMLElement): HTMLElement | null {
	const headings = row.querySelectorAll<HTMLElement>('h3');
	for (const heading of headings) {
		if (heading.querySelector('[data-user-id]') && heading.querySelector('time')) {
			return heading;
		}
	}
	return null;
}

function removeHeaderPrefix(rowText: string, headerInfo: HeaderInfo): string {
	if (headerInfo.style === 'inline') {
		return removeInlineStyleHeaderPrefix(rowText, headerInfo);
	}
	const normalisedRowText = normaliseLineEndings(rowText);
	const lines = normalisedRowText.split('\n');
	let firstTextLineIndex = 0;
	while (firstTextLineIndex < lines.length && collapseWhitespace(lines[firstTextLineIndex]).length === 0) {
		firstTextLineIndex += 1;
	}
	if (firstTextLineIndex >= lines.length) {
		return '';
	}
	const username = collapseWhitespace(headerInfo.username);
	const timestamp = collapseWhitespace(headerInfo.timestamp);
	const firstLine = collapseWhitespace(lines[firstTextLineIndex]);
	const secondLine = firstTextLineIndex + 1 < lines.length ? collapseWhitespace(lines[firstTextLineIndex + 1]) : '';
	if (lineContainsHeader(firstLine, username, timestamp)) {
		const firstLineWithoutHeader = removeInlineHeaderPrefix(lines[firstTextLineIndex], username, timestamp);
		const bodyText = [firstLineWithoutHeader, ...lines.slice(firstTextLineIndex + 1)].join('\n').replace(/^\n+/u, '');
		return stripLeadingHeaderSeparator(bodyText);
	}
	if (firstLine.includes(username) && lineContainsTimestamp(secondLine, timestamp)) {
		const secondLineWithoutTimestamp = removeTimestampLinePrefix(lines[firstTextLineIndex + 1] ?? '', timestamp);
		const bodyText = [secondLineWithoutTimestamp, ...lines.slice(firstTextLineIndex + 2)]
			.join('\n')
			.replace(/^\n+/u, '');
		return stripLeadingHeaderSeparator(bodyText);
	}
	const headerPrefixPattern = createHeaderPrefixPattern(username, timestamp);
	return stripLeadingHeaderSeparator(normalisedRowText.replace(headerPrefixPattern, '').replace(/^\n+/u, ''));
}

function removeInlineStyleHeaderPrefix(rowText: string, headerInfo: HeaderInfo): string {
	const normalisedRowText = normaliseLineEndings(rowText);
	const username = collapseWhitespace(headerInfo.username);
	const timestampAlternatives = createTimestampAlternatives(headerInfo.timestamp).map(escapeRegex).join('|');
	const escapedUsername = escapeRegex(username);
	const headerPrefixPattern = new RegExp(
		`^\\s*(?:${timestampAlternatives})\\s*(?:\\n\\s*)*[^\\n]*?${escapedUsername}\\s*(?::\\s*)?`,
		'u',
	);
	return stripLeadingInlineHeaderSeparator(normalisedRowText.replace(headerPrefixPattern, '').replace(/^\n+/u, ''));
}

function createTimestampAlternatives(timestamp: string): Array<string> {
	const collapsed = collapseWhitespace(timestamp);
	const withoutBrackets = collapsed.replace(/^\[(.*)\]$/u, '$1');
	return Array.from(new Set([collapsed, withoutBrackets, `[${withoutBrackets}]`].filter(Boolean)));
}

function formatHeaderInfo(headerInfo: HeaderInfo): string {
	if (headerInfo.style === 'inline') {
		return `${headerInfo.timestamp} ${headerInfo.username}:`;
	}
	return `${headerInfo.username} — ${headerInfo.timestamp}`;
}

function formatHeaderWithBody(headerInfo: HeaderInfo, header: string, body: string): string {
	if (!body) {
		return header;
	}
	if (headerInfo.style === 'inline') {
		return `${header} ${body}`;
	}
	return `${header}\n${body}`;
}

function lineContainsHeader(line: string, username: string, timestamp: string): boolean {
	return line.includes(username) && lineContainsTimestamp(line, timestamp);
}

function lineContainsTimestamp(line: string, timestamp: string): boolean {
	const withoutLeadingDash = line.replace(/^(?:[-—]\s*)+/u, '');
	return withoutLeadingDash.includes(timestamp);
}

function createHeaderPrefixPattern(username: string, timestamp: string): RegExp {
	const escapedUsername = escapeRegex(username);
	const escapedTimestamp = escapeRegex(timestamp);
	return new RegExp(`^\\s*${escapedUsername}[^\\n]*?(?:\\n\\s*)?(?:[-—]\\s*)*${escapedTimestamp}\\s*`, 'u');
}

function removeInlineHeaderPrefix(line: string, username: string, timestamp: string): string {
	return line.replace(createHeaderPrefixPattern(username, timestamp), '');
}

function removeTimestampLinePrefix(line: string, timestamp: string): string {
	const escapedTimestamp = escapeRegex(timestamp);
	const timestampPrefixPattern = new RegExp(`^\\s*(?:[-—]\\s*)*${escapedTimestamp}\\s*`, 'u');
	return line.replace(timestampPrefixPattern, '');
}

function stripLeadingHeaderSeparator(value: string): string {
	return value.replace(/^\s*(?:[-—]\s*)+/u, '');
}

function stripLeadingInlineHeaderSeparator(value: string): string {
	return value.replace(/^\s*(?::\s*)+/u, '');
}

function collapseWhitespace(value: string): string {
	return value.replace(/\s+/gu, ' ').trim();
}

function normaliseLineEndings(value: string): string {
	return value.replace(/\r\n/gu, '\n');
}

function joinPreparedRows(rows: Array<PreparedRowText>): string {
	let result = '';
	for (const row of rows) {
		if (!row.text.trim()) {
			continue;
		}
		if (result.length > 0) {
			result += row.isGroupStart ? '\n\n' : '\n';
		}
		result += row.text;
	}
	return result;
}
