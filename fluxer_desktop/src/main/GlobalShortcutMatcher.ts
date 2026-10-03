// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	hookKeyNameForLayoutKey,
	hookKeyNameToDomCode,
	MODIFIER_KEY_CODES,
	type ModifierKind,
	modifierKindForCode,
	shouldPreferLayoutKey,
	usLayoutKeyToDomCode,
} from '@electron/main/GlobalShortcutKeys';

export interface ShortcutModifiers {
	ctrl: boolean;
	alt: boolean;
	shift: boolean;
	meta: boolean;
}

export type ShortcutTrigger =
	| {kind: 'key'; layoutKey: string | null; code: string | null}
	| {kind: 'mouse'; button: number}
	| {kind: 'modifier-only'; modifiers: Array<ModifierKind>; bothSides: boolean};

export interface ShortcutBinding {
	sourceId: string;
	action: string;
	trigger: ShortcutTrigger;
	modifiers: ShortcutModifiers;
}

export interface ShortcutComboInput {
	key: string;
	code?: string;
	ctrl: boolean;
	alt: boolean;
	shift: boolean;
	meta: boolean;
	mouseButton?: number;
	modifierOnly?: boolean;
	bothSides?: boolean;
}

export interface HookKeyEvent {
	type: 'keydown' | 'keyup';
	code: string | null;
	key: string | null;
	rawKeycode: number;
	ctrlKey: boolean;
	altKey: boolean;
	shiftKey: boolean;
	metaKey: boolean;
	unflaggedModifierCodes?: ReadonlyArray<string>;
}

export interface HookMouseEvent {
	type: 'mousedown' | 'mouseup';
	button: number;
	ctrlKey: boolean;
	altKey: boolean;
	shiftKey: boolean;
	metaKey: boolean;
	unflaggedModifierCodes?: ReadonlyArray<string>;
}

export interface ShortcutTransition {
	sourceId: string;
	action: string;
	phase: 'press' | 'release';
}

type PressRecord = {kind: 'key'; rawKeycode: number} | {kind: 'mouse'; button: number} | {kind: 'modifier-only'};

const MODIFIER_ORDER: ReadonlyArray<ModifierKind> = ['ctrl', 'alt', 'shift', 'meta'];
const SINGLE_CHARACTER_KEY = /^[A-Z0-9]$/;

export function bindingFromCombo(sourceId: string, action: string, combo: ShortcutComboInput): ShortcutBinding | null {
	const modifiers = {ctrl: combo.ctrl, alt: combo.alt, shift: combo.shift, meta: combo.meta};
	if (combo.mouseButton !== undefined) {
		return {sourceId, action, trigger: {kind: 'mouse', button: combo.mouseButton}, modifiers};
	}
	const code = combo.code ?? usLayoutKeyToDomCode(combo.key);
	if (combo.modifierOnly) {
		const codeKind = modifierKindForCode(code);
		const kinds = MODIFIER_ORDER.filter((kind) => modifiers[kind] || kind === codeKind);
		if (kinds.length === 0) return null;
		const bothSides = combo.bothSides === true && kinds.length === 1;
		return {sourceId, action, trigger: {kind: 'modifier-only', modifiers: kinds, bothSides}, modifiers};
	}
	const layoutName = shouldPreferLayoutKey(combo) ? hookKeyNameForLayoutKey(combo.key) : null;
	const layoutKey = layoutName !== null && SINGLE_CHARACTER_KEY.test(layoutName) ? layoutName : null;
	if (layoutKey === null && code === null) return null;
	return {sourceId, action, trigger: {kind: 'key', layoutKey, code}, modifiers};
}

function bindingSignature(binding: ShortcutBinding): string {
	return JSON.stringify([binding.action, binding.trigger, binding.modifiers]);
}

function modifiersMatch(expected: ShortcutModifiers, event: HookKeyEvent | HookMouseEvent): boolean {
	if (!expected.ctrl && !expected.alt && !expected.shift && !expected.meta) return true;
	return (
		expected.ctrl === event.ctrlKey &&
		expected.alt === event.altKey &&
		expected.shift === event.shiftKey &&
		expected.meta === event.metaKey
	);
}

function keyTriggerMatches(trigger: {layoutKey: string | null; code: string | null}, event: HookKeyEvent): boolean {
	if (trigger.layoutKey !== null && event.key !== null) {
		if (SINGLE_CHARACTER_KEY.test(event.key)) return event.key === trigger.layoutKey;
		if (SINGLE_CHARACTER_KEY.test(trigger.layoutKey) && hookKeyNameToDomCode(event.key) !== null) return false;
	}
	return trigger.code !== null && event.code === trigger.code;
}

function heldModifierCode(event: HookKeyEvent): string | null {
	const identity = event.key ?? event.code;
	return modifierKindForCode(identity) !== null ? identity : null;
}

function modifierFlag(kind: ModifierKind, event: HookKeyEvent | HookMouseEvent): boolean {
	switch (kind) {
		case 'ctrl':
			return event.ctrlKey;
		case 'alt':
			return event.altKey;
		case 'shift':
			return event.shiftKey;
		case 'meta':
			return event.metaKey;
	}
}

export class GlobalShortcutMatcher {
	private readonly bindings = new Map<string, ShortcutBinding>();
	private readonly signatures = new Map<string, string>();
	private readonly pressed = new Map<string, PressRecord>();
	private readonly heldModifierCodes = new Set<string>();

	hasBindings(): boolean {
		return this.bindings.size > 0;
	}

	setBindings(next: ReadonlyArray<ShortcutBinding>): Array<ShortcutTransition> {
		const nextBindings = new Map<string, ShortcutBinding>();
		for (const binding of next) {
			if (!nextBindings.has(binding.sourceId)) nextBindings.set(binding.sourceId, binding);
		}
		const released: Array<ShortcutTransition> = [];
		for (const sourceId of [...this.pressed.keys()]) {
			const previous = this.bindings.get(sourceId);
			const replacement = nextBindings.get(sourceId);
			if (previous && replacement && bindingSignature(replacement) === this.signatures.get(sourceId)) continue;
			this.pressed.delete(sourceId);
			if (previous) released.push({sourceId, action: previous.action, phase: 'release'});
		}
		this.bindings.clear();
		this.signatures.clear();
		for (const [sourceId, binding] of nextBindings) {
			this.bindings.set(sourceId, binding);
			this.signatures.set(sourceId, bindingSignature(binding));
		}
		return released;
	}

	handleKey(event: HookKeyEvent, allowPress: boolean): Array<ShortcutTransition> {
		const heldCode = heldModifierCode(event);
		this.reconcileModifierFlags(event, event.type === 'keydown' ? heldCode : null);
		if (event.type === 'keydown') {
			const transitions = this.releaseUnsatisfiedModifierOnly();
			const modifierOnlyBefore = this.satisfiedModifierOnlyBindings();
			if (heldCode !== null) this.heldModifierCodes.add(heldCode);
			if (!allowPress) return transitions;
			for (const binding of this.bindings.values()) {
				if (this.pressed.has(binding.sourceId)) continue;
				const {trigger} = binding;
				if (trigger.kind === 'modifier-only') {
					if (modifierOnlyBefore.has(binding.sourceId) || !this.isModifierOnlySatisfied(trigger)) continue;
					this.pressed.set(binding.sourceId, {kind: 'modifier-only'});
				} else if (trigger.kind === 'key') {
					if (!keyTriggerMatches(trigger, event) || !modifiersMatch(binding.modifiers, event)) continue;
					this.pressed.set(binding.sourceId, {kind: 'key', rawKeycode: event.rawKeycode});
				} else {
					continue;
				}
				transitions.push({sourceId: binding.sourceId, action: binding.action, phase: 'press'});
			}
			return transitions;
		}
		if (heldCode !== null) this.heldModifierCodes.delete(heldCode);
		const transitions: Array<ShortcutTransition> = [];
		for (const [sourceId, record] of [...this.pressed]) {
			if (record.kind !== 'key' || record.rawKeycode !== event.rawKeycode) continue;
			const binding = this.bindings.get(sourceId);
			this.pressed.delete(sourceId);
			if (binding) transitions.push({sourceId, action: binding.action, phase: 'release'});
		}
		transitions.push(...this.releaseUnsatisfiedModifierOnly());
		return transitions;
	}

	handleMouse(event: HookMouseEvent, allowPress: boolean): Array<ShortcutTransition> {
		this.reconcileModifierFlags(event, null);
		const transitions = this.releaseUnsatisfiedModifierOnly();
		if (event.type === 'mouseup') {
			for (const [sourceId, record] of [...this.pressed]) {
				if (record.kind !== 'mouse' || record.button !== event.button) continue;
				const binding = this.bindings.get(sourceId);
				this.pressed.delete(sourceId);
				if (binding) transitions.push({sourceId, action: binding.action, phase: 'release'});
			}
			return transitions;
		}
		if (!allowPress) return transitions;
		for (const binding of this.bindings.values()) {
			const {trigger} = binding;
			if (trigger.kind !== 'mouse' || trigger.button !== event.button) continue;
			if (this.pressed.has(binding.sourceId) || !modifiersMatch(binding.modifiers, event)) continue;
			this.pressed.set(binding.sourceId, {kind: 'mouse', button: event.button});
			transitions.push({sourceId: binding.sourceId, action: binding.action, phase: 'press'});
		}
		return transitions;
	}

	releaseAll(): Array<ShortcutTransition> {
		const transitions: Array<ShortcutTransition> = [];
		for (const sourceId of this.pressed.keys()) {
			const binding = this.bindings.get(sourceId);
			if (binding) transitions.push({sourceId, action: binding.action, phase: 'release'});
		}
		this.pressed.clear();
		this.heldModifierCodes.clear();
		return transitions;
	}

	private reconcileModifierFlags(event: HookKeyEvent | HookMouseEvent, pressingCode: string | null): void {
		const unflagged = event.unflaggedModifierCodes ?? [];
		for (const kind of MODIFIER_ORDER) {
			if (modifierFlag(kind, event)) continue;
			for (const code of MODIFIER_KEY_CODES[kind]) {
				if (code !== pressingCode && !unflagged.includes(code)) this.heldModifierCodes.delete(code);
			}
		}
	}

	private releaseUnsatisfiedModifierOnly(): Array<ShortcutTransition> {
		const transitions: Array<ShortcutTransition> = [];
		for (const [sourceId, record] of [...this.pressed]) {
			if (record.kind !== 'modifier-only') continue;
			const binding = this.bindings.get(sourceId);
			if (binding?.trigger.kind === 'modifier-only' && this.isModifierOnlySatisfied(binding.trigger)) continue;
			this.pressed.delete(sourceId);
			if (binding) transitions.push({sourceId, action: binding.action, phase: 'release'});
		}
		return transitions;
	}

	private satisfiedModifierOnlyBindings(): Set<string> {
		const satisfied = new Set<string>();
		for (const binding of this.bindings.values()) {
			if (binding.trigger.kind === 'modifier-only' && this.isModifierOnlySatisfied(binding.trigger)) {
				satisfied.add(binding.sourceId);
			}
		}
		return satisfied;
	}

	private isModifierOnlySatisfied(trigger: {modifiers: Array<ModifierKind>; bothSides: boolean}): boolean {
		return trigger.modifiers.every((kind) => {
			const [left, right] = MODIFIER_KEY_CODES[kind];
			const leftHeld = this.heldModifierCodes.has(left);
			const rightHeld = this.heldModifierCodes.has(right);
			return trigger.bothSides ? leftHeld && rightHeld : leftHeld || rightHeld;
		});
	}
}
