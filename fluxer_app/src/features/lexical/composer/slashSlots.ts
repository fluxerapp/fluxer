// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CommandOption} from '@app/features/devtools/hooks/useCommands';
import type {ComposerTypeaheadActiveState} from '@app/features/lexical/composer/ComposerTypeaheadModifierGuard';
import {
	$createComposerInsertNode,
	$replaceComposerRange,
	type ComposerInsertPayload,
} from '@app/features/lexical/composer/composerOffsets';
import {
	$createComposerCommandNode,
	$isComposerCommandNode,
	type ComposerCommandNode,
} from '@app/features/lexical/composer/nodes/ComposerCommandNode';
import {
	$createSlashOptionalHintNode,
	$isSlashOptionalHintNode,
	type SlashOptionalHintNode,
} from '@app/features/lexical/composer/nodes/SlashOptionalHintNode';
import {
	$createSlashSeparatorNode,
	$isSlashSeparatorNode,
} from '@app/features/lexical/composer/nodes/SlashSeparatorNode';
import {
	$createSlashSlotNode,
	$isSlashSlotNode,
	SlashSlotNode,
} from '@app/features/lexical/composer/nodes/SlashSlotNode';
import {
	$createSlashSlotPlaceholderNode,
	$isSlashSlotPlaceholderNode,
} from '@app/features/lexical/composer/nodes/SlashSlotPlaceholderNode';
import {
	computeAbsentOptionalOptions,
	partitionSlashCommandOptions,
} from '@app/features/lexical/composer/slashCommandOptions';
import {
	type SlashSlotResolvers,
	type SlashSlotType,
	validateSlot,
} from '@app/features/lexical/composer/slashSlotValidation';
import {isIMEComposing} from '@app/features/messaging/utils/IMECompositionUtils';
import {mergeRegister} from '@lexical/utils';
import {
	$createParagraphNode,
	$createTextNode,
	$getAdjacentNode,
	$getNodeByKey,
	$getRoot,
	$getSelection,
	$isElementNode,
	$isRangeSelection,
	$isTextNode,
	BEFORE_INPUT_COMMAND,
	COMMAND_PRIORITY_HIGH,
	type ElementNode,
	HISTORY_MERGE_TAG,
	KEY_ARROW_LEFT_COMMAND,
	KEY_ARROW_RIGHT_COMMAND,
	KEY_BACKSPACE_COMMAND,
	KEY_DELETE_COMMAND,
	KEY_TAB_COMMAND,
	type LexicalEditor,
	type LexicalNode,
	type NodeKey,
	ParagraphNode,
	type PointType,
	type TextNode,
} from 'lexical';

function $createSlotForOption(option: CommandOption): SlashSlotNode {
	return $createSlashSlotNode(
		option.name,
		option.type,
		option.required,
		option.choices == null ? [] : option.choices,
		option.description,
	).setAllowEmpty(option.allowEmpty == null ? false : option.allowEmpty);
}

export function $insertSlashCommand(
	name: string,
	options: ReadonlyArray<CommandOption>,
	start: number,
	end: number,
): void {
	$replaceComposerRange(start, end, {kind: 'text', text: ''}, {leading: false, trailing: false});
	const selection = $getSelection();
	if (!$isRangeSelection(selection)) {
		return;
	}
	const {required, optional} = partitionSlashCommandOptions(options);
	const nodes: Array<LexicalNode> = [$createComposerCommandNode(name, optional)];
	const slots: Array<SlashSlotNode> = [];
	for (const option of required) {
		nodes.push($createSlashSeparatorNode());
		const slot = $createSlotForOption(option);
		slots.push(slot);
		nodes.push(slot);
	}
	let optionalAnchor: LexicalNode | null = null;
	if (optional.length > 0) {
		optionalAnchor = $createTextNode(' ');
		nodes.push(optionalAnchor);
		nodes.push($createSlashOptionalHintNode(optional.length));
	}
	selection.insertNodes(nodes);
	const firstSlot = slots[0];
	if (firstSlot != null) {
		firstSlot.selectValueEnd();
	} else if (optionalAnchor != null && $isTextNode(optionalAnchor)) {
		optionalAnchor.selectEnd();
	}
}

function $collectSlots(): Array<SlashSlotNode> {
	const slots: Array<SlashSlotNode> = [];
	for (const block of $getRoot().getChildren()) {
		if (!$isElementNode(block)) {
			continue;
		}
		for (const child of block.getChildren()) {
			if ($isSlashSlotNode(child)) {
				slots.push(child);
			}
		}
	}
	return slots;
}

function $enclosingSlot(node: LexicalNode): SlashSlotNode | null {
	let current: LexicalNode | null = node;
	while (current != null) {
		if ($isSlashSlotNode(current)) {
			return current;
		}
		current = current.getParent();
	}
	return null;
}

function $isPointAtSlotBoundary(slot: SlashSlotNode, point: PointType, boundary: 'start' | 'end'): boolean {
	const node = point.getNode();
	if (point.type === 'element') {
		return node.is(slot) && point.offset === (boundary === 'start' ? 0 : slot.getChildrenSize());
	}
	const enclosing = $enclosingSlot(node);
	if (enclosing == null || !enclosing.is(slot)) {
		return false;
	}
	if (boundary === 'start') {
		return point.offset === 0 && node.getPreviousSibling() == null;
	}
	return point.offset === node.getTextContentSize() && node.getNextSibling() == null;
}

function $advanceToNextSlot(includeOptionalAnchor: boolean): boolean {
	const selection = $getSelection();
	if (!$isRangeSelection(selection)) {
		return false;
	}
	const slots = $collectSlots();
	if (slots.length === 0) {
		return false;
	}
	const currentSlot = $enclosingSlot(selection.anchor.getNode());
	if (currentSlot == null) {
		return false;
	}
	const currentIndex = slots.findIndex((slot) => slot.is(currentSlot));
	const nextSlot = slots[currentIndex + 1];
	if (nextSlot != null) {
		nextSlot.selectValueEnd();
		return true;
	}
	return includeOptionalAnchor ? $focusOptionalAnchorInParagraph(currentSlot.getParent()) : false;
}

export function $focusNextSlot(): boolean {
	return $advanceToNextSlot(true);
}

function $focusPreviousSlot(): boolean {
	const selection = $getSelection();
	if (!$isRangeSelection(selection)) {
		return false;
	}
	const slots = $collectSlots();
	if (slots.length === 0) {
		return false;
	}
	const currentSlot = $enclosingSlot(selection.anchor.getNode());
	if (currentSlot != null) {
		const currentIndex = slots.findIndex((slot) => slot.is(currentSlot));
		const previousSlot = slots[currentIndex - 1];
		if (previousSlot != null) {
			previousSlot.selectValueEnd();
			return true;
		}
		return false;
	}
	const paragraph = $slashCommandParagraph(selection.anchor.getNode());
	if (paragraph == null) {
		return false;
	}
	const target = $nearestSlotForCaret(paragraph);
	if (target != null) {
		target.selectValueEnd();
		return true;
	}
	return false;
}

function $findOptionalHint(paragraph: ElementNode): SlashOptionalHintNode | null {
	for (const child of paragraph.getChildren()) {
		if ($isSlashOptionalHintNode(child)) {
			return child;
		}
	}
	return null;
}

function $focusAnchorBeforeHint(hint: SlashOptionalHintNode): void {
	const previous = hint.getPreviousSibling();
	if ($isTextNode(previous)) {
		previous.selectEnd();
		return;
	}
	const anchor = $createTextNode(' ');
	hint.insertBefore(anchor);
	anchor.selectEnd();
}

function $focusOptionalAnchorInParagraph(paragraph: LexicalNode | null): boolean {
	if (paragraph == null || !$isElementNode(paragraph)) {
		return false;
	}
	const hint = $findOptionalHint(paragraph);
	if (hint == null) {
		return false;
	}
	$focusAnchorBeforeHint(hint);
	return true;
}

export function $focusFirstInvalidSlashSlot(): boolean {
	const invalidSlot = $collectSlots().find((slot) => {
		if (slot.getValidity() === 'invalid') {
			return true;
		}
		if (slot.isSubmitRequired() && !slot.isFilled()) {
			slot.setTouched(true).setValidity('invalid').setValidationError('required').setResolvedWire(null);
			return true;
		}
		return false;
	});
	if (invalidSlot == null) {
		return false;
	}
	invalidSlot.selectValueEnd();
	return true;
}

function $findCommandNode(paragraph: ElementNode): ComposerCommandNode | null {
	for (const child of paragraph.getChildren()) {
		if ($isComposerCommandNode(child)) {
			return child;
		}
	}
	return null;
}

function $reconcileOptionalHint(paragraph: ParagraphNode): void {
	let hint: SlashOptionalHintNode | null = null;
	let commandNode: ComposerCommandNode | null = null;
	const presentNames = new Set<string>();
	for (const child of paragraph.getChildren()) {
		if ($isSlashOptionalHintNode(child)) {
			hint = child;
		} else if ($isSlashSlotNode(child)) {
			presentNames.add(child.getOptionName());
		} else if ($isComposerCommandNode(child)) {
			commandNode = child;
		}
	}
	const optionals = commandNode == null ? [] : commandNode.getOptionalOptions();
	const remaining = optionals.length === 0 ? 0 : computeAbsentOptionalOptions(optionals, presentNames).length;
	if (remaining === 0) {
		if (hint != null) {
			const previous = hint.getPreviousSibling();
			hint.remove();
			if ($isTextNode(previous) && previous.getTextContent().trim().length === 0) {
				previous.remove();
			}
		}
		return;
	}
	if (hint == null) {
		hint = $createSlashOptionalHintNode(remaining);
		paragraph.append(hint);
	} else if (hint.getRemaining() !== remaining) {
		hint.setRemaining(remaining);
	}
	if (!$isTextNode(hint.getPreviousSibling())) {
		hint.insertBefore($createTextNode(' '));
	}
}

function $slashCommandParagraph(node: LexicalNode): ElementNode | null {
	const top = node.getTopLevelElement();
	return top != null && $isElementNode(top) && $findCommandNode(top) != null ? top : null;
}

function $lastRequiredSlotIndex(paragraph: ElementNode): number {
	let index = 0;
	const children = paragraph.getChildren();
	for (let i = 0; i < children.length; i += 1) {
		const child = children[i]!;
		if ($isComposerCommandNode(child) || ($isSlashSlotNode(child) && child.isRequired())) {
			index = i;
		}
	}
	return index;
}

function $findTypeTargetSlot(): SlashSlotNode | null {
	const selection = $getSelection();
	if (!$isRangeSelection(selection) || !selection.isCollapsed()) {
		return null;
	}
	const {anchor} = selection;
	const enclosing = $enclosingSlot(anchor.getNode());
	if (enclosing != null) {
		return enclosing.getTextContentSize() === 0 ? enclosing : null;
	}
	const before = $getAdjacentNode(anchor, true);
	if ($isSlashSlotNode(before) && before.getTextContentSize() === 0) {
		return before;
	}
	const after = $getAdjacentNode(anchor, false);
	if ($isSlashSlotNode(after) && after.getTextContentSize() === 0) {
		return after;
	}
	return null;
}

function $nearestSlotForCaret(paragraph: ElementNode): SlashSlotNode | null {
	const selection = $getSelection();
	if (!$isRangeSelection(selection) || !selection.isCollapsed()) {
		return null;
	}
	const {anchor} = selection;
	const before = $getAdjacentNode(anchor, true);
	if ($isSlashSlotNode(before)) {
		return before;
	}
	const after = $getAdjacentNode(anchor, false);
	if ($isSlashSlotNode(after)) {
		return after;
	}
	const anchorNode = anchor.getNode();
	const caretIndex = anchorNode.is(paragraph) ? anchor.offset : anchorNode.getIndexWithinParent();
	let candidate: SlashSlotNode | null = null;
	for (const child of paragraph.getChildren()) {
		if (!$isSlashSlotNode(child)) {
			continue;
		}
		if (candidate == null || child.getIndexWithinParent() < caretIndex) {
			candidate = child;
		}
	}
	return candidate;
}

function $insertTextIntoSlot(slot: SlashSlotNode, text: string): void {
	const writable = slot.getWritable();
	const hadText = writable.getTextContentSize() > 0;
	for (const child of writable.getChildren()) {
		if ($isSlashSlotPlaceholderNode(child)) {
			child.remove();
		}
	}
	if (!hadText) {
		const node = $createTextNode(text);
		writable.append(node);
		node.selectEnd();
		return;
	}
	slot.selectValueEnd();
	const selection = $getSelection();
	if ($isRangeSelection(selection)) {
		selection.insertText(text);
	}
}

function $clearSlashComposer(): void {
	const root = $getRoot();
	root.clear();
	const paragraph = $createParagraphNode();
	root.append(paragraph);
	paragraph.select();
}

function $removeOptionalSlot(slot: SlashSlotNode): void {
	const separator = slot.getPreviousSibling();
	const beforeSeparator = separator == null ? null : separator.getPreviousSibling();
	slot.remove();
	let caretTarget: LexicalNode | null = separator;
	if ($isSlashSeparatorNode(separator) || ($isTextNode(separator) && separator.getTextContent() === ' ')) {
		separator.remove();
		caretTarget = beforeSeparator;
	}
	if ($isSlashSlotNode(caretTarget)) {
		caretTarget.selectValueEnd();
	} else if ($isTextNode(caretTarget)) {
		caretTarget.selectEnd();
	} else if (caretTarget != null) {
		if (caretTarget.isAttached() && $isElementNode(caretTarget)) {
			caretTarget.selectEnd();
		}
	}
}

function $nextOptionalSlot(slot: SlashSlotNode): SlashSlotNode | null {
	let node: LexicalNode | null = slot.getNextSibling();
	while (node != null && !$isSlashSlotNode(node)) {
		if ($isSlashOptionalHintNode(node)) {
			return null;
		}
		node = node.getNextSibling();
	}
	return $isSlashSlotNode(node) && !node.isRequired() ? node : null;
}

function $handleSlashStructuralDelete(event: KeyboardEvent, isBackward: boolean): boolean {
	const selection = $getSelection();
	if (!$isRangeSelection(selection)) {
		return false;
	}
	const paragraph = $slashCommandParagraph(selection.anchor.getNode());
	if (paragraph == null) {
		return false;
	}
	if (!selection.isCollapsed()) {
		const anchorSlot = $enclosingSlot(selection.anchor.getNode());
		const focusSlot = $enclosingSlot(selection.focus.getNode());
		if (anchorSlot != null) {
			if (anchorSlot.is(focusSlot)) {
				return false;
			}
		}
		event.preventDefault();
		$clearSlashComposer();
		return true;
	}
	const activeOptional = $activeOptionalHint();
	if (activeOptional != null) {
		if (isBackward) {
			if (selection.anchor.offset > 1) {
				return false;
			}
			event.preventDefault();
			if (activeOptional.anchor.getTextContent() !== ' ') {
				activeOptional.anchor.setTextContent(' ');
			}
			$focusPreviousSlot();
			return true;
		}
		event.preventDefault();
		return true;
	}
	const anchorNode = selection.anchor.getNode();
	const slot = $enclosingSlot(anchorNode);
	if (slot != null) {
		const hasValue = slot.getTextContentSize() > 0;
		const boundary = isBackward ? 'start' : 'end';
		const withinValue = hasValue && !$isPointAtSlotBoundary(slot, selection.anchor, boundary);
		if (withinValue) {
			return false;
		}
		event.preventDefault();
		if (isBackward) {
			if (slot.isRequired()) {
				$clearSlashComposer();
			} else {
				$removeOptionalSlot(slot);
			}
		} else {
			const nextOptional = $nextOptionalSlot(slot);
			if (nextOptional != null) {
				$removeOptionalSlot(nextOptional);
			}
		}
		return true;
	}
	const lastRequiredIndex = $lastRequiredSlotIndex(paragraph);
	if (
		$isComposerCommandNode(anchorNode) ||
		($isTextNode(anchorNode) && anchorNode.getIndexWithinParent() <= lastRequiredIndex)
	) {
		event.preventDefault();
		if (isBackward) {
			$clearSlashComposer();
		}
		return true;
	}
	const adjacent = $getAdjacentNode(selection.anchor, isBackward);
	if (adjacent == null) {
		return false;
	}
	const adjacentSlot = $isSlashSeparatorNode(adjacent)
		? isBackward
			? adjacent.getPreviousSibling()
			: adjacent.getNextSibling()
		: adjacent;
	if ($isSlashSlotNode(adjacentSlot)) {
		event.preventDefault();
		if (adjacentSlot.isRequired()) {
			if (isBackward) {
				$clearSlashComposer();
			}
		} else {
			$removeOptionalSlot(adjacentSlot);
		}
		return true;
	}
	if ($isSlashOptionalHintNode(adjacent)) {
		event.preventDefault();
		return true;
	}
	if ($isComposerCommandNode(adjacent) || adjacent.getIndexWithinParent() <= lastRequiredIndex) {
		event.preventDefault();
		if (isBackward) {
			$clearSlashComposer();
		}
		return true;
	}
	return false;
}

export function registerSlashSlotPlugin(
	editor: LexicalEditor,
	getResolvers: () => SlashSlotResolvers,
	typeaheadActiveRef: ComposerTypeaheadActiveState,
): () => void {
	return mergeRegister(
		editor.registerNodeTransform(SlashSlotNode, (node) => {
			if (editor.isComposing()) {
				return;
			}
			if (node.getTextContentSize() > 0) {
				for (const child of node.getChildren()) {
					if ($isSlashSlotPlaceholderNode(child)) {
						child.remove();
					}
				}
			} else if (!node.getChildren().some($isSlashSlotPlaceholderNode)) {
				node.append($createSlashSlotPlaceholderNode());
			}
			if (
				node.getValidity() === 'valid' &&
				node.getResolvedWire() != null &&
				node.getResolvedDisplay() === node.getTextContent()
			) {
				return;
			}
			const resolvers = getResolvers();
			const result = validateSlot(node.getOptionType(), node.getTextContent(), {
				...resolvers,
				required: node.isSubmitRequired(),
				choices: node.getChoices(),
			});
			const text = node.getTextContent();
			const resolvedDisplay = node.getResolvedDisplay();
			const nextTouched = node.isTouched() || text.length > 0 || (resolvedDisplay != null && resolvedDisplay !== text);
			const nextValidity =
				text.length === 0
					? node.isSubmitRequired() && nextTouched
						? 'invalid'
						: 'neutral'
					: result.valid
						? 'valid'
						: 'invalid';
			const nextWire = result.resolvedWire == null ? null : result.resolvedWire;
			if (node.isTouched() !== nextTouched) {
				node.setTouched(nextTouched);
			}
			if (node.getValidity() !== nextValidity) {
				node.setValidity(nextValidity);
			}
			const nextError = result.error == null ? null : result.error;
			if (node.getValidationError() !== nextError) {
				node.setValidationError(nextError);
			}
			if (node.getResolvedWire() !== nextWire || node.getResolvedDisplay() !== (nextWire == null ? null : text)) {
				node.setResolvedWire(nextWire, text);
			}
		}),
		editor.registerNodeTransform(ParagraphNode, (paragraph) => {
			$reconcileOptionalHint(paragraph);
		}),
		editor.registerCommand(
			KEY_TAB_COMMAND,
			(event: KeyboardEvent) => {
				if (isIMEComposing(event) || event.altKey || event.ctrlKey || event.metaKey) {
					return false;
				}
				if (typeaheadActiveRef.current) {
					return false;
				}
				if (event.shiftKey) {
					if ($focusPreviousSlot()) {
						event.preventDefault();
						return true;
					}
					return false;
				}
				if ($focusNextSlot()) {
					event.preventDefault();
					return true;
				}
				return false;
			},
			COMMAND_PRIORITY_HIGH,
		),
		editor.registerCommand(
			KEY_ARROW_RIGHT_COMMAND,
			(event: KeyboardEvent | null) => {
				if (
					event != null &&
					(isIMEComposing(event) || event.shiftKey || event.altKey || event.ctrlKey || event.metaKey)
				) {
					return false;
				}
				const selection = $getSelection();
				if (!$isRangeSelection(selection) || !selection.isCollapsed()) {
					return false;
				}
				const slot = $enclosingSlot(selection.anchor.getNode());
				if (slot == null) {
					return false;
				}
				if (!$isPointAtSlotBoundary(slot, selection.anchor, 'end')) {
					return false;
				}
				if ($focusNextSlot()) {
					if (event != null) {
						event.preventDefault();
					}
					return true;
				}
				return false;
			},
			COMMAND_PRIORITY_HIGH,
		),
		editor.registerCommand(
			KEY_ARROW_LEFT_COMMAND,
			(event: KeyboardEvent | null) => {
				if (
					event != null &&
					(isIMEComposing(event) || event.shiftKey || event.altKey || event.ctrlKey || event.metaKey)
				) {
					return false;
				}
				const selection = $getSelection();
				if (!$isRangeSelection(selection) || !selection.isCollapsed()) {
					return false;
				}
				const slot = $enclosingSlot(selection.anchor.getNode());
				if (slot == null) {
					return false;
				}
				if (!$isPointAtSlotBoundary(slot, selection.anchor, 'start')) {
					return false;
				}
				if (event != null) {
					event.preventDefault();
				}
				$focusPreviousSlot();
				return true;
			},
			COMMAND_PRIORITY_HIGH,
		),
		editor.registerCommand(
			BEFORE_INPUT_COMMAND,
			(event: InputEvent) => {
				if (editor.isComposing() || event.isComposing) {
					return false;
				}
				if (event.inputType !== 'insertText' && event.inputType !== 'insertReplacementText') {
					return false;
				}
				const data = event.data;
				if (data == null || data.length === 0) {
					return false;
				}
				const target = $findTypeTargetSlot();
				if (target != null) {
					event.preventDefault();
					$insertTextIntoSlot(target, data);
					return true;
				}
				const selection = $getSelection();
				if (!$isRangeSelection(selection)) {
					return false;
				}
				const paragraph = $slashCommandParagraph(selection.anchor.getNode());
				if (paragraph == null) {
					return false;
				}
				const anchorSlot = $enclosingSlot(selection.anchor.getNode());
				const focusSlot = $enclosingSlot(selection.focus.getNode());
				if (anchorSlot != null) {
					if (anchorSlot.is(focusSlot)) {
						return false;
					}
				}
				if (selection.isCollapsed() && $activeOptionalHint() != null) {
					return false;
				}
				event.preventDefault();
				if (selection.isCollapsed()) {
					const fallbackSlot = $nearestSlotForCaret(paragraph);
					if (fallbackSlot != null) {
						$insertTextIntoSlot(fallbackSlot, data);
					}
				}
				return true;
			},
			COMMAND_PRIORITY_HIGH,
		),
		editor.registerCommand(
			KEY_BACKSPACE_COMMAND,
			(event: KeyboardEvent) => $handleSlashStructuralDelete(event, true),
			COMMAND_PRIORITY_HIGH,
		),
		editor.registerCommand(
			KEY_DELETE_COMMAND,
			(event: KeyboardEvent) => $handleSlashStructuralDelete(event, false),
			COMMAND_PRIORITY_HIGH,
		),
		editor.registerUpdateListener(({editorState}) => {
			if (editor.isComposing()) {
				return;
			}
			let targetKey: NodeKey | null = null;
			editorState.read(() => {
				const selection = $getSelection();
				if (!$isRangeSelection(selection) || !selection.isCollapsed()) {
					return;
				}
				const anchorNode = selection.anchor.getNode();
				const paragraph = $slashCommandParagraph(anchorNode);
				if (paragraph == null) {
					return;
				}
				if ($enclosingSlot(anchorNode) != null) {
					return;
				}
				if ($activeOptionalHint() != null) {
					return;
				}
				const targetSlot = $nearestSlotForCaret(paragraph);
				targetKey = targetSlot == null ? null : targetSlot.getKey();
			});
			if (targetKey == null) {
				return;
			}
			const key = targetKey;
			queueMicrotask(() => {
				editor.update(
					() => {
						const selection = $getSelection();
						if (!$isRangeSelection(selection) || !selection.isCollapsed()) {
							return;
						}
						const anchorNode = selection.anchor.getNode();
						const paragraph = $slashCommandParagraph(anchorNode);
						if (paragraph == null || $enclosingSlot(anchorNode) != null || $activeOptionalHint() != null) {
							return;
						}
						const node = $nearestSlotForCaret(paragraph);
						if ($isSlashSlotNode(node) && node.getKey() === key) {
							node.selectValueEnd();
						}
					},
					{tag: HISTORY_MERGE_TAG},
				);
			});
		}),
	);
}

export interface ActiveSlotInfo {
	nodeKey: NodeKey;
	optionName: string;
	description: string;
	required: boolean;
	optionType: SlashSlotType;
	isRequiredError: boolean;
}

export interface SlashCommandComposerState {
	hasSlots: boolean;
	activeSlot: ActiveSlotInfo | null;
}

const EMPTY_SLASH_COMMAND_STATE: SlashCommandComposerState = {hasSlots: false, activeSlot: null};

function $getActiveSlot(): SlashSlotNode | null {
	const selection = $getSelection();
	if (!$isRangeSelection(selection)) {
		return null;
	}
	const enclosing = $enclosingSlot(selection.anchor.getNode());
	if (enclosing != null) {
		return enclosing;
	}
	if (!selection.isCollapsed()) {
		return null;
	}
	const before = $getAdjacentNode(selection.anchor, true);
	if ($isSlashSlotNode(before) && before.getTextContentSize() === 0) {
		return before;
	}
	const after = $getAdjacentNode(selection.anchor, false);
	if ($isSlashSlotNode(after) && after.getTextContentSize() === 0) {
		return after;
	}
	return null;
}

function buildActiveSlotInfo(slot: SlashSlotNode): ActiveSlotInfo {
	const isEmpty = slot.getTextContent().trim().length === 0;
	return {
		nodeKey: slot.getKey(),
		optionName: slot.getOptionName(),
		description: slot.getDescription(),
		required: slot.isSubmitRequired(),
		optionType: slot.getOptionType(),
		isRequiredError: slot.isSubmitRequired() && slot.isTouched() && isEmpty,
	};
}

function slashCommandStateEquals(a: SlashCommandComposerState, b: SlashCommandComposerState): boolean {
	if (a.hasSlots !== b.hasSlots) {
		return false;
	}
	const slotA = a.activeSlot;
	const slotB = b.activeSlot;
	if (slotA == null || slotB == null) {
		return slotA === slotB;
	}
	return (
		slotA.nodeKey === slotB.nodeKey &&
		slotA.optionName === slotB.optionName &&
		slotA.description === slotB.description &&
		slotA.required === slotB.required &&
		slotA.optionType === slotB.optionType &&
		slotA.isRequiredError === slotB.isRequiredError
	);
}

export function registerSlashSlotFocus(
	editor: LexicalEditor,
	getListener: () => ((state: SlashCommandComposerState) => void) | undefined,
): () => void {
	let previousActiveKey: NodeKey | null = null;
	let reportedState: SlashCommandComposerState = EMPTY_SLASH_COMMAND_STATE;
	return editor.registerUpdateListener(({editorState}) => {
		let nextState: SlashCommandComposerState = EMPTY_SLASH_COMMAND_STATE;
		let activeKey: NodeKey | null = null;
		let slotToTouch: NodeKey | null = null;
		editorState.read(() => {
			const slots = $collectSlots();
			const activeSlot = $getActiveSlot();
			activeKey = activeSlot == null ? null : activeSlot.getKey();
			for (const slot of slots) {
				const element = editor.getElementByKey(slot.getKey());
				if (element == null) {
					continue;
				}
				if (slot.getKey() === activeKey) {
					element.setAttribute('data-slot-focused', 'true');
				} else {
					element.removeAttribute('data-slot-focused');
				}
			}
			if (previousActiveKey != null && previousActiveKey !== activeKey) {
				const previousSlot = slots.find((slot) => slot.getKey() === previousActiveKey);
				if (previousSlot != null) {
					const isBlurredRequiredEmpty =
						previousSlot.isSubmitRequired() &&
						!previousSlot.isTouched() &&
						previousSlot.getTextContent().trim().length === 0;
					if (isBlurredRequiredEmpty) {
						slotToTouch = previousSlot.getKey();
					}
				}
			}
			nextState = {
				hasSlots: slots.length > 0,
				activeSlot: activeSlot == null ? null : buildActiveSlotInfo(activeSlot),
			};
		});
		previousActiveKey = activeKey;
		if (slotToTouch != null) {
			const key = slotToTouch;
			queueMicrotask(() => {
				editor.update(
					() => {
						const node = $getNodeByKey(key);
						if (
							$isSlashSlotNode(node) &&
							($getActiveSlot() == null || $getActiveSlot()!.getKey() !== key) &&
							node.isSubmitRequired() &&
							node.getTextContent().trim().length === 0
						) {
							node.setTouched(true).setValidity('invalid').setValidationError('required').setResolvedWire(null);
						}
					},
					{tag: HISTORY_MERGE_TAG},
				);
			});
		}
		if (!slashCommandStateEquals(reportedState, nextState)) {
			reportedState = nextState;
			const listener = getListener();
			if (listener != null) {
				listener(nextState);
			}
		}
	});
}

export interface SlashSlotChoiceContext {
	choices: ReadonlyArray<{name: string; value: string}>;
	query: string;
}

export interface SlashSlotAutocompleteContext {
	commandName: string | null;
	optionName: string;
	optionType: SlashSlotType;
	choices: ReadonlyArray<{name: string; value: string}>;
	query: string;
}

export function $getActiveSlotAutocompleteContext(): SlashSlotAutocompleteContext | null {
	const slot = $getActiveSlot();
	if (slot == null) {
		return null;
	}
	const paragraph = slot.getParent();
	const commandNode = paragraph != null && $isElementNode(paragraph) ? $findCommandNode(paragraph) : null;
	const commandName = commandNode == null ? null : commandNode.getTextContent();
	const optionType = slot.getOptionType();
	if (
		optionType !== 'choice' &&
		optionType !== 'user' &&
		optionType !== 'channel' &&
		optionType !== 'role' &&
		optionType !== 'boolean'
	) {
		return null;
	}
	const text = slot.getTextContent();
	if (slot.getValidity() === 'valid' && slot.getResolvedWire() != null && slot.getResolvedDisplay() === text) {
		return null;
	}
	return {
		commandName: commandName == null ? null : commandName.replace(/^\//, ''),
		optionName: slot.getOptionName(),
		optionType,
		choices: slot.getChoices(),
		query: text,
	};
}

export function $getActiveSlotChoiceContext(): SlashSlotChoiceContext | null {
	const context = $getActiveSlotAutocompleteContext();
	if (context == null || context.optionType !== 'choice') {
		return null;
	}
	if (context.choices.length === 0) {
		return null;
	}
	return {choices: context.choices, query: context.query};
}

export function $applySlotChoice(name: string): void {
	const slot = $getActiveSlot();
	if (slot == null) {
		return;
	}
	for (const child of slot.getChildren()) {
		child.remove();
	}
	if (name.length > 0) {
		slot.append($createTextNode(name));
	}
	slot.setResolvedWire(null).setTouched(true);
	slot.selectValueEnd();
	$advanceToNextSlot(false);
}

export function $applySlotPayload(payload: ComposerInsertPayload): void {
	const slot = $getActiveSlot();
	if (slot == null) {
		return;
	}
	for (const child of slot.getChildren()) {
		child.remove();
	}
	slot.append($createComposerInsertNode(payload, false));
	if (payload.kind === 'mention' || payload.kind === 'customEmoji') {
		slot.setResolvedWire(payload.wire, payload.display);
	} else {
		slot.setResolvedWire(null).setTouched(true);
	}
	slot.selectValueEnd();
	$advanceToNextSlot(false);
}

export interface SlashOptionalContext {
	options: ReadonlyArray<CommandOption>;
	query: string;
}

function $activeOptionalHint(): {hint: SlashOptionalHintNode; anchor: TextNode} | null {
	const selection = $getSelection();
	if (!$isRangeSelection(selection) || !selection.isCollapsed()) {
		return null;
	}
	const anchor = selection.anchor.getNode();
	if (!$isTextNode(anchor)) {
		return null;
	}
	const next = anchor.getNextSibling();
	if (!$isSlashOptionalHintNode(next)) {
		return null;
	}
	return {hint: next, anchor};
}

function $paragraphOptionals(paragraph: ElementNode): {
	optionals: ReadonlyArray<CommandOption>;
	presentNames: Set<string>;
} {
	const commandNode = $findCommandNode(paragraph);
	const presentNames = new Set<string>();
	for (const child of paragraph.getChildren()) {
		if ($isSlashSlotNode(child)) {
			presentNames.add(child.getOptionName());
		}
	}
	return {optionals: commandNode == null ? [] : commandNode.getOptionalOptions(), presentNames};
}

export function $getActiveOptionalContext(): SlashOptionalContext | null {
	const active = $activeOptionalHint();
	if (active == null) {
		return null;
	}
	const {hint, anchor} = active;
	const paragraph = hint.getParent();
	if (paragraph == null || !$isElementNode(paragraph)) {
		return null;
	}
	const {optionals, presentNames} = $paragraphOptionals(paragraph);
	const absent = computeAbsentOptionalOptions(optionals, presentNames);
	if (absent.length === 0) {
		return null;
	}
	return {options: absent, query: anchor.getTextContent().trim()};
}

export function $applyOptionalChoice(optionName: string): void {
	const active = $activeOptionalHint();
	if (active == null) {
		return;
	}
	const {hint, anchor} = active;
	const paragraph = hint.getParent();
	if (paragraph == null || !$isElementNode(paragraph)) {
		return;
	}
	const {optionals} = $paragraphOptionals(paragraph);
	const option = optionals.find((candidate) => candidate.name === optionName);
	if (option == null) {
		return;
	}
	if (anchor.getTextContent() !== ' ') {
		anchor.setTextContent(' ');
	}
	const slot = $createSlotForOption(option);
	anchor.insertBefore(slot);
	slot.insertBefore($createSlashSeparatorNode());
	slot.selectValueEnd();
}
