// SPDX-License-Identifier: AGPL-3.0-or-later

import Keybind, {
	type CustomKeybindEntry,
	type KeybindCommand,
	type KeyCombo,
} from '@app/features/input/state/InputKeybind';
import {resolveKeybindCommand} from '@app/features/input/state/input_keybind/KeybindCommands';
import {useEffect} from 'react';

interface FormattingShortcut {
	combo: Partial<KeyCombo>;
	wrapper: string;
}

interface MarkdownKeybindScopeOptions {
	preserveEditableFocusActions?: boolean;
}

const MARKDOWN_FORMATTING_SHORTCUTS: ReadonlyArray<FormattingShortcut> = [
	{combo: {key: 'b', ctrlOrMeta: true}, wrapper: '**'},
	{combo: {key: 'i', ctrlOrMeta: true}, wrapper: '*'},
	{combo: {key: 'u', ctrlOrMeta: true}, wrapper: '__'},
	{combo: {key: 's', ctrlOrMeta: true, shift: true}, wrapper: '~~'},
];
const normalizeKeyName = (key?: string, code?: string): string => {
	const candidate = key?.length ? key : code;
	return candidate ? candidate.toLowerCase() : '';
};
const modifiersMatch = (
	source: {
		ctrlOrMeta: boolean;
		ctrl: boolean;
		meta: boolean;
		alt: boolean;
		shift: boolean;
	},
	target: Partial<KeyCombo>,
): boolean => {
	if (target.ctrlOrMeta !== undefined && source.ctrlOrMeta !== target.ctrlOrMeta) {
		return false;
	}
	if (target.ctrl !== undefined) {
		if (source.ctrl !== target.ctrl) return false;
	} else if (target.ctrlOrMeta === undefined && source.ctrl) {
		return false;
	}
	if (target.meta !== undefined) {
		if (source.meta !== target.meta) return false;
	} else if (target.ctrlOrMeta === undefined && source.meta) {
		return false;
	}
	if (target.alt !== undefined) {
		if (source.alt !== target.alt) return false;
	} else if (source.alt) {
		return false;
	}
	if (target.shift !== undefined) {
		if (source.shift !== target.shift) return false;
	} else if (source.shift) {
		return false;
	}
	return true;
};
const doesStoredComboMatchShortcut = (combo: KeyCombo, target: Partial<KeyCombo>): boolean => {
	const comboKey = normalizeKeyName(combo.key, combo.code);
	const targetKey = normalizeKeyName(target.key, target.code);
	if (targetKey && targetKey !== comboKey) {
		return false;
	}
	return modifiersMatch(
		{
			ctrlOrMeta: Boolean(combo.ctrlOrMeta || combo.ctrl || combo.meta),
			ctrl: Boolean(combo.ctrl),
			meta: Boolean(combo.meta),
			alt: Boolean(combo.alt),
			shift: Boolean(combo.shift),
		},
		target,
	);
};
const shouldPreserveConflictingAction = (action: KeybindCommand, options: MarkdownKeybindScopeOptions): boolean => {
	if (!options.preserveEditableFocusActions) return false;
	const behavior = Keybind.getDefaultByAction(action)?.editableFocusBehavior;
	return behavior === 'allow' || behavior === 'allow_when_empty';
};
const getConflictingKeybindActions = (options: MarkdownKeybindScopeOptions = {}): Set<KeybindCommand> => {
	const actions = new Set<KeybindCommand>();
	for (const {combo, action} of Keybind.getAll()) {
		if (shouldPreserveConflictingAction(action, options)) continue;
		for (const {combo: shortcutCombo} of MARKDOWN_FORMATTING_SHORTCUTS) {
			if (doesStoredComboMatchShortcut(combo, shortcutCombo)) {
				actions.add(action);
				break;
			}
		}
	}
	for (const entry of Keybind.getCustomKeybinds() as ReadonlyArray<CustomKeybindEntry>) {
		const entryAction = resolveKeybindCommand(entry.action);
		if (entryAction === null || !entry.enabled) continue;
		if (shouldPreserveConflictingAction(entryAction, options)) continue;
		for (const {combo: shortcutCombo} of MARKDOWN_FORMATTING_SHORTCUTS) {
			if (doesStoredComboMatchShortcut(entry.combo, shortcutCombo)) {
				actions.add(entryAction);
				break;
			}
		}
	}
	return actions;
};

class MarkdownKeybindScope {
	private mutedActions = new Set<KeybindCommand>();
	private activeScopes = new Map<symbol, MarkdownKeybindScopeOptions>();

	acquire(options: MarkdownKeybindScopeOptions = {}): () => void {
		const token = Symbol('markdown-keybind-scope');
		this.activeScopes.set(token, options);
		this.refreshMutedKeybinds();
		let released = false;
		return () => {
			if (released) return;
			released = true;
			this.activeScopes.delete(token);
			this.refreshMutedKeybinds();
		};
	}

	private refreshMutedKeybinds(): void {
		this.restoreMutedKeybinds();
		if (this.activeScopes.size === 0) return;
		const preserveEditableFocusActions = [...this.activeScopes.values()].some(
			(options) => options.preserveEditableFocusActions,
		);
		const actions = getConflictingKeybindActions({preserveEditableFocusActions});
		for (const action of actions) {
			this.mutedActions.add(action);
		}
		Keybind.muteActions(this.mutedActions);
	}

	private restoreMutedKeybinds(): void {
		if (!this.mutedActions.size) return;
		Keybind.unmuteActions(this.mutedActions);
		this.mutedActions.clear();
	}
}

const markdownKeybindScope = new MarkdownKeybindScope();
export const useMarkdownKeybinds = (active: boolean, options: MarkdownKeybindScopeOptions = {}): void => {
	useEffect(() => {
		if (!active) {
			return;
		}
		const release = markdownKeybindScope.acquire(options);
		return release;
	}, [active, options.preserveEditableFocusActions]);
};
