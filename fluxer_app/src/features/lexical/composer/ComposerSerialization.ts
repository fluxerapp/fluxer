// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	$createComposerCommandNode,
	$isComposerCommandNode,
} from '@app/features/lexical/composer/nodes/ComposerCommandNode';
import {
	$createComposerCustomEmojiNode,
	$isComposerCustomEmojiNode,
} from '@app/features/lexical/composer/nodes/ComposerCustomEmojiNode';
import {
	$createComposerMentionNode,
	$isComposerMentionNode,
	type ComposerMentionType,
} from '@app/features/lexical/composer/nodes/ComposerMentionNode';
import {
	$createComposerPlainSegmentNode,
	$isComposerPlainSegmentNode,
} from '@app/features/lexical/composer/nodes/ComposerPlainSegmentNode';
import {$createSlashSeparatorNode} from '@app/features/lexical/composer/nodes/SlashSeparatorNode';
import {
	$createSlashSlotNode,
	$isSlashSlotNode,
	type SlashSlotNode,
} from '@app/features/lexical/composer/nodes/SlashSlotNode';
import {
	createSlashCommandStateSegmentId,
	createSlashSlotStateSegmentId,
	type PersistedSlashSlotState,
	parseSlashCommandStateSegment,
	parseSlashSlotStateSegment,
	parseSlashSlotStateSegmentId,
} from '@app/features/lexical/composer/SlashSlotPersistence';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import {
	$createLineBreakNode,
	$createParagraphNode,
	$createTextNode,
	$getRoot,
	$isElementNode,
	$isLineBreakNode,
	type LexicalNode,
} from 'lexical';
import invariant from 'tiny-invariant';

export interface ComposerProjection {
	display: string;
	segments: Array<MentionSegment>;
	wire: string;
}

const CUSTOM_EMOJI_WIRE_RE = /^<(a)?:([A-Za-z0-9_+~-]+):(\d+)>$/;
const SLOT_USER_WIRE_RE = /^<@!?(\d+)>$/;
const SLOT_ROLE_WIRE_RE = /^<@&(\d+)>$/;
const SLOT_CHANNEL_WIRE_RE = /^<#(\d+)>$/;
export const COMPOSER_SLASH_SLOT_SEGMENT_PREFIX = 'slash-slot:';
const SLASH_SLOT_OPTION_NAME_RE = /^[a-z0-9_-]{1,32}$/;

function isValidSegmentWire(segment: MentionSegment): boolean {
	if (parseSlashCommandStateSegment(segment) != null) {
		return true;
	}
	if (parseSlashSlotStateSegment(segment) != null) {
		return true;
	}
	if (segment.id.startsWith(COMPOSER_SLASH_SLOT_SEGMENT_PREFIX)) {
		return (
			segment.type === 'special' &&
			SLASH_SLOT_OPTION_NAME_RE.test(segment.id.slice(COMPOSER_SLASH_SLOT_SEGMENT_PREFIX.length)) &&
			segment.actualText.length > 0 &&
			!segment.actualText.includes('\n') &&
			!segment.actualText.includes('\r')
		);
	}
	switch (segment.type) {
		case 'emoji': {
			const match = CUSTOM_EMOJI_WIRE_RE.exec(segment.actualText);
			return match != null && match[3] === segment.id;
		}
		case 'user': {
			const match = SLOT_USER_WIRE_RE.exec(segment.actualText);
			return match != null && match[1] === segment.id;
		}
		case 'role': {
			const match = SLOT_ROLE_WIRE_RE.exec(segment.actualText);
			return match != null && match[1] === segment.id;
		}
		case 'channel': {
			const match = SLOT_CHANNEL_WIRE_RE.exec(segment.actualText);
			return match != null && match[1] === segment.id;
		}
		case 'special':
			return (
				(segment.id === 'everyone' || segment.id === 'here') &&
				segment.actualText === `@${segment.id}` &&
				segment.displayText === segment.actualText
			);
	}
}

export function isValidComposerSegment(display: string, segment: MentionSegment, minimumStart = 0): boolean {
	return (
		Number.isInteger(segment.start) &&
		Number.isInteger(segment.end) &&
		segment.start >= minimumStart &&
		segment.start >= 0 &&
		segment.end > segment.start &&
		segment.end <= display.length &&
		display.slice(segment.start, segment.end) === segment.displayText &&
		isValidSegmentWire(segment)
	);
}

function $slashSlotState(node: SlashSlotNode): PersistedSlashSlotState {
	return {
		version: 2,
		optionName: node.getOptionName(),
		optionType: node.getOptionType(),
		required: node.isRequired(),
		allowEmpty: node.allowsEmpty(),
		description: node.getDescription(),
		choices: node.getChoices(),
		validity: node.getValidity(),
		validationError: node.getValidationError(),
		touched: node.isTouched(),
	};
}

function normalizeEditedSlashSlotState(state: PersistedSlashSlotState, displayText: string): PersistedSlashSlotState {
	const emptyRequired = state.required && state.allowEmpty !== true && displayText.slice(1).length === 0;
	return {
		...state,
		validity: emptyRequired ? 'invalid' : 'neutral',
		validationError: emptyRequired ? 'required' : null,
		touched: true,
	};
}

function createSlashSlotStateSegment(
	state: PersistedSlashSlotState,
	displayText: string,
	actualText: string,
	start: number,
): MentionSegment | null {
	const segment: MentionSegment = {
		type: 'special',
		id: createSlashSlotStateSegmentId(state),
		displayText,
		actualText,
		start,
		end: start + displayText.length,
	};
	return parseSlashSlotStateSegment(segment) == null ? null : segment;
}

function createLegacySlashSlotSegment(
	node: SlashSlotNode,
	displayText: string,
	actualText: string,
	start: number,
): MentionSegment | null {
	if (displayText.length === 0 || actualText === displayText) {
		return null;
	}
	const userMatch = SLOT_USER_WIRE_RE.exec(actualText);
	if (userMatch) {
		return {type: 'user', id: userMatch[1]!, displayText, actualText, start, end: start + displayText.length};
	}
	const roleMatch = SLOT_ROLE_WIRE_RE.exec(actualText);
	if (roleMatch) {
		return {type: 'role', id: roleMatch[1]!, displayText, actualText, start, end: start + displayText.length};
	}
	const channelMatch = SLOT_CHANNEL_WIRE_RE.exec(actualText);
	if (channelMatch) {
		return {type: 'channel', id: channelMatch[1]!, displayText, actualText, start, end: start + displayText.length};
	}
	return {
		type: 'special',
		id: `${COMPOSER_SLASH_SLOT_SEGMENT_PREFIX}${node.getOptionName()}`,
		displayText,
		actualText,
		start,
		end: start + displayText.length,
	};
}

function $createSlashSlotValueNode(
	optionType: PersistedSlashSlotState['optionType'],
	displayText: string,
	wireText: string,
): LexicalNode {
	if (optionType === 'user') {
		const match = SLOT_USER_WIRE_RE.exec(wireText);
		if (match) {
			return $createComposerMentionNode('user', match[1]!, displayText, wireText);
		}
	}
	if (optionType === 'role') {
		const match = SLOT_ROLE_WIRE_RE.exec(wireText);
		if (match) {
			return $createComposerMentionNode('role', match[1]!, displayText, wireText);
		}
	}
	if (optionType === 'channel') {
		const match = SLOT_CHANNEL_WIRE_RE.exec(wireText);
		if (match) {
			return $createComposerMentionNode('channel', match[1]!, displayText, wireText);
		}
	}
	return $createTextNode(displayText);
}

export function $projectComposer(): ComposerProjection {
	const root = $getRoot();
	const blocks = root.getChildren();
	let display = '';
	let wire = '';
	const segments: Array<MentionSegment> = [];
	for (let b = 0; b < blocks.length; b += 1) {
		if (b > 0) {
			display += '\n';
			wire += '\n';
		}
		const block = blocks[b]!;
		if (!$isElementNode(block)) {
			display += block.getTextContent();
			wire += block.getTextContent();
			continue;
		}
		for (const child of block.getChildren()) {
			if ($isLineBreakNode(child)) {
				display += '\n';
				wire += '\n';
			} else if ($isComposerCommandNode(child)) {
				const start = display.length;
				const displayText = child.getTextContent();
				const segmentId = createSlashCommandStateSegmentId(displayText, child.getOptionalOptions());
				invariant(segmentId != null, 'Composer slash command metadata violates persistence bounds');
				display += displayText;
				wire += displayText;
				segments.push({
					type: 'special',
					id: segmentId,
					displayText,
					actualText: displayText,
					start,
					end: display.length,
				});
			} else if ($isComposerPlainSegmentNode(child)) {
				const start = display.length;
				const displayText = child.getTextContent();
				display += displayText;
				const slashState = parseSlashSlotStateSegmentId(child.getSegmentId());
				if (slashState != null && displayText.startsWith(' ') && !displayText.includes('\n')) {
					const segment = child.isSegmentValid()
						? createSlashSlotStateSegment(slashState, displayText, child.getWireText(), start)
						: createSlashSlotStateSegment(
								normalizeEditedSlashSlotState(slashState, displayText),
								displayText,
								displayText,
								start,
							);
					if (segment != null) {
						wire += segment.actualText;
						segments.push(segment);
					} else {
						wire += displayText;
					}
				} else if (child.isSegmentValid()) {
					const actualText = child.getWireText();
					wire += actualText;
					segments.push({
						type: child.getSegmentType(),
						id: child.getSegmentId(),
						displayText,
						actualText,
						start,
						end: display.length,
					});
				} else {
					wire += displayText;
				}
			} else if ($isComposerMentionNode(child)) {
				const start = display.length;
				const displayText = child.getTextContent();
				display += displayText;
				wire += child.getWireText();
				segments.push({
					type: child.getSegmentType(),
					id: child.getMentionId(),
					displayText,
					actualText: child.getWireText(),
					start,
					end: display.length,
				});
			} else if ($isComposerCustomEmojiNode(child)) {
				const start = display.length;
				const displayText = child.getTextContent();
				display += displayText;
				wire += child.getWireText();
				segments.push({
					type: 'emoji',
					id: child.getEmojiId(),
					displayText,
					actualText: child.getWireText(),
					start,
					end: display.length,
				});
			} else if ($isSlashSlotNode(child)) {
				const displayText = child.getTextContent();
				const start = display.length;
				let slotWire = child.getWireText();
				display += displayText;
				const previousSibling = child.getPreviousSibling();
				const previousSegment = segments.at(-1);
				const previousSegmentEnd = previousSegment == null ? 0 : previousSegment.end;
				let persistedStructure = false;
				if (
					start > 0 &&
					display[start - 1] === ' ' &&
					wire[wire.length - 1] === ' ' &&
					previousSibling != null &&
					previousSibling.getTextContent().endsWith(' ') === true &&
					previousSegmentEnd <= start - 1
				) {
					const stateDisplay = ` ${displayText}`;
					let segment = createSlashSlotStateSegment($slashSlotState(child), stateDisplay, ` ${slotWire}`, start - 1);
					if (segment == null) {
						slotWire = displayText;
						segment = createSlashSlotStateSegment(
							normalizeEditedSlashSlotState($slashSlotState(child), stateDisplay),
							stateDisplay,
							stateDisplay,
							start - 1,
						);
					}
					if (segment != null) {
						segments.push(segment);
						persistedStructure = true;
					}
				}
				if (!persistedStructure) {
					const legacySegment = createLegacySlashSlotSegment(child, displayText, slotWire, start);
					if (legacySegment != null) {
						segments.push(legacySegment);
					}
				}
				wire += slotWire;
			} else {
				display += child.getTextContent();
				wire += child.getTextContent();
			}
		}
	}
	return {display, segments, wire};
}

export function $createComposerSegmentNodes(segment: MentionSegment, plainText: boolean): Array<LexicalNode> {
	const commandState = parseSlashCommandStateSegment(segment);
	if (commandState != null) {
		if (plainText) {
			return [$createComposerPlainSegmentNode(segment.type, segment.id, segment.displayText, segment.actualText)];
		}
		return [$createComposerCommandNode(segment.displayText, commandState.optionalOptions)];
	}
	const slashState = parseSlashSlotStateSegment(segment);
	if (slashState != null) {
		if (plainText) {
			return [$createComposerPlainSegmentNode(segment.type, segment.id, segment.displayText, segment.actualText)];
		}
		const displayText = segment.displayText.slice(1);
		const wireText = segment.actualText.slice(1);
		const slot = $createSlashSlotNode(
			slashState.optionName,
			slashState.optionType,
			slashState.required,
			slashState.choices,
			slashState.description,
		)
			.setAllowEmpty(slashState.allowEmpty == null ? false : slashState.allowEmpty)
			.setTouched(slashState.touched)
			.setValidity(slashState.validity)
			.setValidationError(slashState.validationError);
		if (slashState.validity === 'valid') {
			if (displayText.length > 0) {
				slot.append($createSlashSlotValueNode(slashState.optionType, displayText, wireText));
			}
			slot.setResolvedWire(wireText, displayText);
		} else if (displayText.length > 0) {
			slot.append($createTextNode(displayText));
		}
		return [$createSlashSeparatorNode(), slot];
	}
	if (plainText || segment.id.startsWith(COMPOSER_SLASH_SLOT_SEGMENT_PREFIX)) {
		return [$createComposerPlainSegmentNode(segment.type, segment.id, segment.displayText, segment.actualText)];
	}
	if (segment.type === 'emoji') {
		const match = CUSTOM_EMOJI_WIRE_RE.exec(segment.actualText);
		if (match) {
			return [$createComposerCustomEmojiNode(match[3]!, Boolean(match[1]), segment.displayText, segment.actualText)];
		}
		return [$createTextNode(segment.displayText)];
	}
	return [
		$createComposerMentionNode(
			segment.type as ComposerMentionType,
			segment.id,
			segment.displayText,
			segment.actualText,
		),
	];
}

export function $hydrateComposerFromDraft(
	display: string,
	segments: ReadonlyArray<MentionSegment>,
	plainText = false,
): void {
	const root = $getRoot();
	root.clear();
	const paragraph = $createParagraphNode();
	root.append(paragraph);
	const appendText = (text: string) => {
		if (text.length === 0) {
			return;
		}
		const parts = text.split('\n');
		for (let i = 0; i < parts.length; i += 1) {
			if (i > 0) {
				paragraph.append($createLineBreakNode());
			}
			if (parts[i]!.length > 0) {
				paragraph.append($createTextNode(parts[i]!));
			}
		}
	};
	const sorted = [...segments].sort((a, b) => a.start - b.start);
	let cursor = 0;
	for (const segment of sorted) {
		if (!isValidComposerSegment(display, segment, cursor)) {
			continue;
		}
		if (segment.start > cursor) {
			appendText(display.slice(cursor, segment.start));
		}
		paragraph.append(...$createComposerSegmentNodes(segment, plainText));
		cursor = segment.end;
	}
	if (cursor < display.length) {
		appendText(display.slice(cursor));
	}
}
