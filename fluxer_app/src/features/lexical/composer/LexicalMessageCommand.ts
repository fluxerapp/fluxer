// SPDX-License-Identifier: AGPL-3.0-or-later

import type * as CommandUtils from '@app/features/devtools/utils/CommandUtils';
import type {ComposerHandle} from '@app/features/lexical/composer/ComposerHandle';
import {
	$isComposerCommandNode,
	type ComposerCommandNode,
} from '@app/features/lexical/composer/nodes/ComposerCommandNode';
import {$isSlashOptionalHintNode} from '@app/features/lexical/composer/nodes/SlashOptionalHintNode';
import {$isSlashSeparatorNode} from '@app/features/lexical/composer/nodes/SlashSeparatorNode';
import {$isSlashSlotNode, type SlashSlotNode} from '@app/features/lexical/composer/nodes/SlashSlotNode';
import {$getRoot, $isElementNode, $isTextNode, type LexicalNode} from 'lexical';

export const LexicalMessageCommandResolutionStatus = Object.freeze({
	NO_COMMAND: 'no-command',
	INVALID_COMMAND: 'invalid-command',
	VALID_COMMAND: 'valid-command',
} as const);

export type LexicalMessageCommandResolutionStatus =
	(typeof LexicalMessageCommandResolutionStatus)[keyof typeof LexicalMessageCommandResolutionStatus];

interface NoCommandResolution {
	status: typeof LexicalMessageCommandResolutionStatus.NO_COMMAND;
}

interface InvalidCommandResolution {
	status: typeof LexicalMessageCommandResolutionStatus.INVALID_COMMAND;
}

interface ValidCommandResolution {
	status: typeof LexicalMessageCommandResolutionStatus.VALID_COMMAND;
	command: Exclude<CommandUtils.ParsedCommand, {type: 'unknown'}>;
}

export type LexicalMessageCommandResolution = NoCommandResolution | InvalidCommandResolution | ValidCommandResolution;

interface CommandStructure {
	commandNode: ComposerCommandNode;
	slots: Map<string, SlashSlotNode>;
}

const NO_COMMAND: NoCommandResolution = Object.freeze({
	status: LexicalMessageCommandResolutionStatus.NO_COMMAND,
});
const INVALID_COMMAND: InvalidCommandResolution = Object.freeze({
	status: LexicalMessageCommandResolutionStatus.INVALID_COMMAND,
});
const USER_WIRE_PATTERN = /^<@!?(\d+)>$/;
const DELETE_MESSAGE_DAYS_PATTERN = /^[0-7]$/;

function hasOnlySlots(slots: Map<string, SlashSlotNode>, allowed: ReadonlyArray<string>): boolean {
	for (const name of slots.keys()) {
		if (allowed.indexOf(name) < 0) {
			return false;
		}
	}
	return true;
}

function readStringSlot(slots: Map<string, SlashSlotNode>, name: string, allowEmpty: boolean): string | null {
	const slot = slots.get(name);
	if (slot == null || slot.getOptionType() !== 'string') {
		return null;
	}
	const text = slot.getTextContent().trim();
	if (text.length === 0) {
		return allowEmpty ? '' : null;
	}
	if (slot.getValidity() === 'invalid') {
		return null;
	}
	const wire = slot.getWireText().trim();
	return wire.length === 0 ? null : wire;
}

function readOptionalStringSlot(slots: Map<string, SlashSlotNode>, name: string): string | undefined | null {
	const slot = slots.get(name);
	if (slot == null) {
		return undefined;
	}
	if (slot.getOptionType() !== 'string') {
		return null;
	}
	if (slot.getTextContent().trim().length === 0) {
		return undefined;
	}
	return readStringSlot(slots, name, false);
}

function readUserSlot(slots: Map<string, SlashSlotNode>, name: string): string | null {
	const slot = slots.get(name);
	if (slot == null || slot.getOptionType() !== 'user' || slot.getValidity() === 'invalid') {
		return null;
	}
	const wire = slot.getWireText().trim();
	const match = USER_WIRE_PATTERN.exec(wire);
	return match == null ? null : match[1];
}

function readChoiceSlot(slots: Map<string, SlashSlotNode>, name: string): string | null {
	const slot = slots.get(name);
	if (slot == null || slot.getOptionType() !== 'choice' || slot.getValidity() === 'invalid') {
		return null;
	}
	const wire = slot.getResolvedWire();
	if (wire == null) {
		return null;
	}
	for (const choice of slot.getChoices()) {
		if (choice.value === wire) {
			return wire;
		}
	}
	return null;
}

function readStructure(): CommandStructure | null {
	const blocks = $getRoot().getChildren();
	if (blocks.length !== 1) {
		return null;
	}
	const block = blocks[0];
	if (block == null || !$isElementNode(block)) {
		return null;
	}
	const children = block.getChildren();
	const first = children[0];
	if (first == null || !$isComposerCommandNode(first)) {
		return null;
	}
	const slots = new Map<string, SlashSlotNode>();
	for (let index = 1; index < children.length; index += 1) {
		const child = children[index];
		if (child == null) {
			return null;
		}
		if ($isSlashSlotNode(child)) {
			if (slots.has(child.getOptionName())) {
				return null;
			}
			slots.set(child.getOptionName(), child);
			continue;
		}
		if ($isSlashSeparatorNode(child) || $isSlashOptionalHintNode(child)) {
			continue;
		}
		if ($isTextNode(child) && child.getTextContent().trim().length === 0) {
			continue;
		}
		return null;
	}
	return {commandNode: first, slots};
}

function containsCommandNode(nodes: ReadonlyArray<LexicalNode>): boolean {
	const pending = nodes.slice();
	while (pending.length > 0) {
		const node = pending.pop();
		if (node == null) {
			continue;
		}
		if ($isComposerCommandNode(node)) {
			return true;
		}
		if ($isElementNode(node)) {
			pending.push(...node.getChildren());
		}
	}
	return false;
}

function resolveCommand(structure: CommandStructure): Exclude<CommandUtils.ParsedCommand, {type: 'unknown'}> | null {
	const name = structure.commandNode.getTextContent().trim();
	const slots = structure.slots;
	if (name === '/nick') {
		if (!hasOnlySlots(slots, ['nickname'])) return null;
		const nickname = readStringSlot(slots, 'nickname', true);
		return nickname == null ? null : {type: 'nick', nickname};
	}
	if (name === '/kick') {
		if (!hasOnlySlots(slots, ['user', 'reason'])) return null;
		const userId = readUserSlot(slots, 'user');
		const reason = readOptionalStringSlot(slots, 'reason');
		if (userId == null || reason === null) return null;
		if (reason === undefined) return {type: 'kick', userId};
		return {type: 'kick', userId, reason};
	}
	if (name === '/ban') {
		if (!hasOnlySlots(slots, ['user', 'delete_messages', 'reason'])) return null;
		const userId = readUserSlot(slots, 'user');
		const deleteMessageDaysText = readChoiceSlot(slots, 'delete_messages');
		const reason = readOptionalStringSlot(slots, 'reason');
		if (
			userId == null ||
			deleteMessageDaysText == null ||
			!DELETE_MESSAGE_DAYS_PATTERN.test(deleteMessageDaysText) ||
			reason === null
		) {
			return null;
		}
		if (reason === undefined) {
			return {
				type: 'ban',
				userId,
				deleteMessageDays: Number(deleteMessageDaysText),
				duration: 0,
			};
		}
		return {
			type: 'ban',
			userId,
			deleteMessageDays: Number(deleteMessageDaysText),
			duration: 0,
			reason,
		};
	}
	if (name === '/msg') {
		if (!hasOnlySlots(slots, ['user', 'message'])) return null;
		const userId = readUserSlot(slots, 'user');
		const message = readStringSlot(slots, 'message', false);
		if (userId == null || message == null) return null;
		return {type: 'msg', userId, message};
	}
	if (name === '/me' || name === '/spoiler' || name === '/tts') {
		if (!hasOnlySlots(slots, ['message'])) return null;
		const content = readStringSlot(slots, 'message', false);
		if (content == null) return null;
		if (name === '/me') return {type: 'me', content};
		if (name === '/spoiler') return {type: 'spoiler', content};
		return {type: 'tts', content};
	}
	return null;
}

export const LexicalMessageCommandResolver = Object.freeze({
	resolve(handle: ComposerHandle | null): LexicalMessageCommandResolution {
		if (handle == null) return NO_COMMAND;
		const editor = handle.getEditor();
		let resolution: LexicalMessageCommandResolution = NO_COMMAND;
		editor.getEditorState().read(() => {
			const structure = readStructure();
			if (structure == null) {
				resolution = containsCommandNode($getRoot().getChildren()) ? INVALID_COMMAND : NO_COMMAND;
				return;
			}
			const command = resolveCommand(structure);
			resolution =
				command == null ? INVALID_COMMAND : {status: LexicalMessageCommandResolutionStatus.VALID_COMMAND, command};
		});
		return resolution;
	},
});

export function isLexicalMessageCommandCurrentChannelMessage(resolution: LexicalMessageCommandResolution): boolean {
	if (resolution.status !== LexicalMessageCommandResolutionStatus.VALID_COMMAND) return false;
	return resolution.command.type === 'me' || resolution.command.type === 'spoiler' || resolution.command.type === 'tts';
}
