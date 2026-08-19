// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	$createComposerSegmentNodes,
	$projectComposer,
	isValidComposerSegment,
} from '@app/features/lexical/composer/ComposerSerialization';
import {$captureSelectionOffsets, $selectComposerOffset} from '@app/features/lexical/composer/composerOffsets';
import {$isSlashSlotNode, type SlashSlotNode} from '@app/features/lexical/composer/nodes/SlashSlotNode';
import {COMPOSER_SLASH_SLOT_STATE_MAX_ID_LENGTH} from '@app/features/lexical/composer/SlashSlotPersistence';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import {mergeRegister} from '@lexical/utils';
import {
	$addUpdateTag,
	$createLineBreakNode,
	$createTextNode,
	$getRoot,
	$getSelection,
	$isElementNode,
	$isLineBreakNode,
	$isNodeSelection,
	$isRangeSelection,
	$isTextNode,
	COMMAND_PRIORITY_NORMAL,
	COPY_COMMAND,
	CUT_COMMAND,
	CUT_TAG,
	type LexicalEditor,
	type LexicalNode,
	PASTE_COMMAND,
	PASTE_TAG,
} from 'lexical';

export const FLUXER_COMPOSER_CLIPBOARD_MIME = 'application/x-fluxer-composer+json';
export const COMPOSER_CLIPBOARD_VERSION = 1;
export const COMPOSER_CLIPBOARD_MAX_PAYLOAD_LENGTH = 262_144;
export const COMPOSER_CLIPBOARD_MAX_DISPLAY_LENGTH = 65_536;
export const COMPOSER_CLIPBOARD_MAX_SEGMENTS = 512;
export const COMPOSER_CLIPBOARD_MAX_TRUSTED_PAYLOADS = 32;

const COMPOSER_CLIPBOARD_MAX_SEGMENT_ID_LENGTH = COMPOSER_SLASH_SLOT_STATE_MAX_ID_LENGTH;
const COMPOSER_CLIPBOARD_MAX_SEGMENT_TEXT_LENGTH = 65_536;
const COMPOSER_CLIPBOARD_MAX_TRUSTED_PAYLOAD_LENGTH = 1_048_576;
const SEGMENT_TYPES = new Set<MentionSegment['type']>(['user', 'role', 'channel', 'emoji', 'special']);
const PAYLOAD_KEYS = ['version', 'token', 'display', 'segments'] as const;
const SEGMENT_KEYS = ['type', 'id', 'displayText', 'actualText', 'start', 'end'] as const;
const TRUST_TOKEN_RE = /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/;
const trustedPayloads = new Map<string, string>();
let trustedPayloadLength = 0;

export interface ComposerClipboardSlice {
	display: string;
	segments: Array<MentionSegment>;
}

export interface ComposerClipboardSelection extends ComposerClipboardSlice {
	textPlain: string;
}

export interface ComposerClipboardCommandState {
	getPlainText(): boolean;
	isEditable(): boolean;
}

interface SerializedComposerClipboardPayload {
	version: typeof COMPOSER_CLIPBOARD_VERSION;
	token: string;
	display: string;
	segments: Array<MentionSegment>;
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value != null && !Array.isArray(value);
}

function hasExactKeys<const T extends ReadonlyArray<string>>(value: Record<string, unknown>, keys: T): boolean {
	const actualKeys = Object.keys(value);
	return actualKeys.length === keys.length && keys.every((key) => Object.hasOwn(value, key));
}

function isBoundedString(value: unknown, minimumLength: number, maximumLength: number): value is string {
	return typeof value === 'string' && value.length >= minimumLength && value.length <= maximumLength;
}

function parseSegment(value: unknown): MentionSegment | null {
	if (!isRecord(value) || !hasExactKeys(value, SEGMENT_KEYS)) {
		return null;
	}
	if (typeof value.type !== 'string' || !SEGMENT_TYPES.has(value.type as MentionSegment['type'])) {
		return null;
	}
	if (!isBoundedString(value.id, 1, COMPOSER_CLIPBOARD_MAX_SEGMENT_ID_LENGTH)) {
		return null;
	}
	if (!isBoundedString(value.displayText, 1, COMPOSER_CLIPBOARD_MAX_SEGMENT_TEXT_LENGTH)) {
		return null;
	}
	if (value.displayText.includes('\n') || value.displayText.includes('\r')) {
		return null;
	}
	if (!isBoundedString(value.actualText, 1, COMPOSER_CLIPBOARD_MAX_SEGMENT_TEXT_LENGTH)) {
		return null;
	}
	if (!Number.isInteger(value.start) || !Number.isInteger(value.end)) {
		return null;
	}
	return {
		type: value.type as MentionSegment['type'],
		id: value.id,
		displayText: value.displayText,
		actualText: value.actualText,
		start: value.start as number,
		end: value.end as number,
	};
}

function validateSlice(display: unknown, rawSegments: unknown): ComposerClipboardSlice | null {
	if (!isBoundedString(display, 1, COMPOSER_CLIPBOARD_MAX_DISPLAY_LENGTH)) {
		return null;
	}
	if (!Array.isArray(rawSegments) || rawSegments.length > COMPOSER_CLIPBOARD_MAX_SEGMENTS) {
		return null;
	}
	const segments: Array<MentionSegment> = [];
	let minimumStart = 0;
	for (const rawSegment of rawSegments) {
		const segment = parseSegment(rawSegment);
		if (segment == null || !isValidComposerSegment(display, segment, minimumStart)) {
			return null;
		}
		segments.push(segment);
		minimumStart = segment.end;
	}
	return {display, segments};
}

function sliceToWire(slice: ComposerClipboardSlice): string {
	const chunks: Array<string> = [];
	let cursor = 0;
	for (const segment of slice.segments) {
		if (segment.start > cursor) {
			chunks.push(slice.display.slice(cursor, segment.start));
		}
		chunks.push(segment.actualText);
		cursor = segment.end;
	}
	if (cursor < slice.display.length) {
		chunks.push(slice.display.slice(cursor));
	}
	return chunks.join('');
}

function $getNodeDisplayRange(targetNode: LexicalNode): {start: number; end: number} | null {
	let result: {start: number; end: number} | null = null;
	let offset = 0;
	const visit = (node: LexicalNode) => {
		const nodeStart = offset;
		if ($isElementNode(node)) {
			for (const child of node.getChildren()) {
				visit(child);
			}
		} else {
			offset += $isLineBreakNode(node) ? 1 : node.getTextContent().length;
		}
		if (node.is(targetNode)) {
			result = {start: nodeStart, end: offset};
		}
	};
	const blocks = $getRoot().getChildren();
	for (let index = 0; index < blocks.length; index += 1) {
		if (index > 0) {
			offset += 1;
		}
		visit(blocks[index]!);
	}
	return result;
}

function $getNodeSelectionOffsets(selectedNodes: ReadonlyArray<LexicalNode>): {anchor: number; focus: number} | null {
	if (selectedNodes.length !== 1) {
		return null;
	}
	const range = $getNodeDisplayRange(selectedNodes[0]!);
	return range != null && range.end > range.start ? {anchor: range.start, focus: range.end} : null;
}

function $getSelectionOffsets(): {anchor: number; focus: number} | null {
	const selection = $getSelection();
	if ($isNodeSelection(selection)) {
		return $getNodeSelectionOffsets(selection.getNodes());
	}
	return $captureSelectionOffsets();
}

export function getComposerClipboardTextPlain(slice: ComposerClipboardSlice): string | null {
	const validated = validateSlice(slice.display, slice.segments);
	return validated == null ? null : sliceToWire(validated);
}

export function $getComposerClipboardSelection(): ComposerClipboardSelection | null {
	const offsets = $getSelectionOffsets();
	if (offsets == null) {
		return null;
	}
	const start = Math.min(offsets.anchor, offsets.focus);
	const end = Math.max(offsets.anchor, offsets.focus);
	if (start === end) {
		return null;
	}
	const projection = $projectComposer();
	const display = projection.display.slice(start, end);
	const segments = projection.segments
		.filter((segment) => segment.start >= start && segment.end <= end)
		.map((segment) => ({...segment, start: segment.start - start, end: segment.end - start}));
	const validated = validateSlice(display, segments);
	if (validated == null) {
		return null;
	}
	return {...validated, textPlain: sliceToWire(validated)};
}

export function serializeComposerClipboardSlice(slice: ComposerClipboardSlice): string | null {
	const validated = validateSlice(slice.display, slice.segments);
	if (validated == null) {
		return null;
	}
	const token = globalThis.crypto.randomUUID();
	const payload: SerializedComposerClipboardPayload = {
		version: COMPOSER_CLIPBOARD_VERSION,
		token,
		display: validated.display,
		segments: validated.segments,
	};
	const serialized = JSON.stringify(payload);
	if (serialized.length > COMPOSER_CLIPBOARD_MAX_PAYLOAD_LENGTH) {
		return null;
	}
	trustedPayloads.set(token, serialized);
	trustedPayloadLength += serialized.length;
	while (
		trustedPayloads.size > COMPOSER_CLIPBOARD_MAX_TRUSTED_PAYLOADS ||
		trustedPayloadLength > COMPOSER_CLIPBOARD_MAX_TRUSTED_PAYLOAD_LENGTH
	) {
		const oldest = trustedPayloads.entries().next().value;
		if (oldest == null) {
			break;
		}
		trustedPayloads.delete(oldest[0]);
		trustedPayloadLength -= oldest[1].length;
	}
	return serialized;
}

export function parseComposerClipboardSlice(serialized: string): ComposerClipboardSlice | null {
	if (serialized.length === 0 || serialized.length > COMPOSER_CLIPBOARD_MAX_PAYLOAD_LENGTH) {
		return null;
	}
	let value: unknown;
	try {
		value = JSON.parse(serialized);
	} catch {
		return null;
	}
	if (!isRecord(value) || !hasExactKeys(value, PAYLOAD_KEYS)) {
		return null;
	}
	if (value.version !== COMPOSER_CLIPBOARD_VERSION) {
		return null;
	}
	if (typeof value.token !== 'string' || !TRUST_TOKEN_RE.test(value.token)) {
		return null;
	}
	if (trustedPayloads.get(value.token) !== serialized) {
		return null;
	}
	const validated = validateSlice(value.display, value.segments);
	return validated;
}

function appendTextNodes(nodes: Array<LexicalNode>, text: string): void {
	const lines = text.split('\n');
	for (let index = 0; index < lines.length; index += 1) {
		if (index > 0) {
			nodes.push($createLineBreakNode());
		}
		const line = lines[index]!;
		if (line.length > 0) {
			nodes.push($createTextNode(line));
		}
	}
}

function $createSliceNodes(slice: ComposerClipboardSlice, plainText: boolean): Array<LexicalNode> {
	const nodes: Array<LexicalNode> = [];
	let cursor = 0;
	for (const segment of slice.segments) {
		if (segment.start > cursor) {
			appendTextNodes(nodes, slice.display.slice(cursor, segment.start));
		}
		nodes.push(...$createComposerSegmentNodes(segment, plainText));
		cursor = segment.end;
	}
	if (cursor < slice.display.length) {
		appendTextNodes(nodes, slice.display.slice(cursor));
	}
	return nodes;
}

function $findEnclosingSlashSlot(node: LexicalNode): SlashSlotNode | null {
	let current: LexicalNode | null = node;
	while (current != null) {
		if ($isSlashSlotNode(current)) {
			return current;
		}
		current = current.getParent();
	}
	return null;
}

function $selectSlashSlotOffset(slot: SlashSlotNode, offset: number): void {
	let consumed = 0;
	const children = slot.getChildren();
	for (let index = 0; index < children.length; index += 1) {
		const child = children[index]!;
		if ($isLineBreakNode(child)) {
			if (offset <= consumed) {
				slot.select(index, index);
				return;
			}
			consumed += 1;
			if (offset <= consumed) {
				slot.select(index + 1, index + 1);
				return;
			}
			continue;
		}
		if ($isTextNode(child)) {
			const length = child.getTextContentSize();
			if (offset <= consumed + length) {
				const textOffset = Math.max(0, offset - consumed);
				child.select(textOffset, textOffset);
				return;
			}
			consumed += length;
		}
	}
	slot.selectValueEnd();
}

function $replaceSlashSlotRange(
	slot: SlashSlotNode,
	globalStart: number,
	globalEnd: number,
	replacement: string,
): boolean {
	const slotRange = $getNodeDisplayRange(slot);
	if (slotRange == null) {
		return false;
	}
	const slotText = slot.getTextContent();
	const start = Math.min(Math.max(0, globalStart - slotRange.start), slotText.length);
	const end = Math.min(Math.max(start, globalEnd - slotRange.start), slotText.length);
	const nextText = `${slotText.slice(0, start)}${replacement}${slotText.slice(end)}`;
	const nodes: Array<LexicalNode> = [];
	appendTextNodes(nodes, nextText);
	slot.clear();
	slot.append(...nodes);
	slot.setResolvedWire(null).setValidity('neutral').setValidationError(null).setTouched(true);
	$selectSlashSlotOffset(slot, start + replacement.length);
	return true;
}

export function $insertComposerClipboardSlice(slice: ComposerClipboardSlice, plainText: boolean): boolean {
	const validated = validateSlice(slice.display, slice.segments);
	if (validated == null) {
		return false;
	}
	const selection = $getSelection();
	if ($isNodeSelection(selection)) {
		const selectedNodes = selection.getNodes();
		if (selectedNodes.length !== 1) {
			return false;
		}
		const selectedNode = selectedNodes[0]!;
		const slot = $findEnclosingSlashSlot(selectedNode);
		if (slot != null) {
			const selectedRange = $getNodeDisplayRange(selectedNode);
			return (
				selectedRange != null &&
				$replaceSlashSlotRange(slot, selectedRange.start, selectedRange.end, sliceToWire(validated))
			);
		}
		selection.insertNodes($createSliceNodes(validated, plainText));
		return true;
	}
	if (!$isRangeSelection(selection)) {
		return false;
	}
	const offsets = $captureSelectionOffsets();
	if (offsets == null) {
		return false;
	}
	const start = Math.min(offsets.anchor, offsets.focus);
	const anchorSlot = $findEnclosingSlashSlot(selection.anchor.getNode());
	const focusSlot = $findEnclosingSlashSlot(selection.focus.getNode());
	if (anchorSlot != null && focusSlot != null && anchorSlot.is(focusSlot)) {
		const end = Math.max(offsets.anchor, offsets.focus);
		return $replaceSlashSlotRange(anchorSlot, start, end, sliceToWire(validated));
	}
	selection.insertNodes($createSliceNodes(validated, plainText));
	$selectComposerOffset(start + validated.display.length);
	return true;
}

function getClipboardEvent(event: unknown): ClipboardEvent | null {
	if (
		typeof event !== 'object' ||
		event == null ||
		!('clipboardData' in event) ||
		!('preventDefault' in event) ||
		event.clipboardData == null ||
		typeof event.preventDefault !== 'function'
	) {
		return null;
	}
	return event as ClipboardEvent;
}

function writeComposerClipboardSelection(event: ClipboardEvent): boolean {
	const selection = $getComposerClipboardSelection();
	if (selection == null || event.clipboardData == null) {
		return false;
	}
	const serialized = serializeComposerClipboardSlice(selection);
	event.clipboardData.setData('text/plain', selection.textPlain);
	if (serialized != null) {
		event.clipboardData.setData(FLUXER_COMPOSER_CLIPBOARD_MIME, serialized);
	}
	return true;
}

function $deleteComposerClipboardSelection(): boolean {
	const selection = $getSelection();
	if ($isRangeSelection(selection)) {
		if (selection.isCollapsed()) {
			return false;
		}
		selection.removeText();
		return true;
	}
	if ($isNodeSelection(selection) && selection.getNodes().length > 0) {
		selection.deleteNodes();
		return true;
	}
	return false;
}

export function registerComposerClipboardCommands(
	editor: LexicalEditor,
	state: ComposerClipboardCommandState,
): () => void {
	return mergeRegister(
		editor.registerCommand(
			COPY_COMMAND,
			(event) => {
				const clipboardEvent = getClipboardEvent(event);
				if (clipboardEvent == null || !writeComposerClipboardSelection(clipboardEvent)) {
					return false;
				}
				clipboardEvent.preventDefault();
				return true;
			},
			COMMAND_PRIORITY_NORMAL,
		),
		editor.registerCommand(
			CUT_COMMAND,
			(event) => {
				if (!state.isEditable()) {
					return false;
				}
				const clipboardEvent = getClipboardEvent(event);
				if (clipboardEvent == null || !writeComposerClipboardSelection(clipboardEvent)) {
					return false;
				}
				if (!$deleteComposerClipboardSelection()) {
					return false;
				}
				$addUpdateTag(CUT_TAG);
				clipboardEvent.preventDefault();
				return true;
			},
			COMMAND_PRIORITY_NORMAL,
		),
		editor.registerCommand(
			PASTE_COMMAND,
			(event) => {
				if (!state.isEditable()) {
					return false;
				}
				const clipboardEvent = getClipboardEvent(event);
				const serialized =
					clipboardEvent == null || clipboardEvent.clipboardData == null
						? null
						: clipboardEvent.clipboardData.getData(FLUXER_COMPOSER_CLIPBOARD_MIME);
				if (clipboardEvent == null || serialized == null || serialized.length === 0) {
					return false;
				}
				const slice = parseComposerClipboardSlice(serialized);
				if (slice == null || !$insertComposerClipboardSlice(slice, state.getPlainText())) {
					return false;
				}
				$addUpdateTag(PASTE_TAG);
				clipboardEvent.preventDefault();
				return true;
			},
			COMMAND_PRIORITY_NORMAL,
		),
	);
}
