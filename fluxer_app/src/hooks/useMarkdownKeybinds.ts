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

import React from 'react';
import KeybindStore, {type KeybindAction, type KeyCombo} from '~/stores/KeybindStore';

interface FormattingShortcut {
	combo: Partial<KeyCombo>;
	wrapper: string;
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
	source: {ctrlOrMeta: boolean; ctrl: boolean; meta: boolean; alt: boolean; shift: boolean},
	target: Partial<KeyCombo>,
): boolean => {
	if (target.ctrlOrMeta !== undefined && source.ctrlOrMeta !== target.ctrlOrMeta) {
		return false;
	}
	if (target.ctrl !== undefined && source.ctrl !== target.ctrl) {
		return false;
	}
	if (target.meta !== undefined && source.meta !== target.meta) {
		return false;
	}
	if (target.alt !== undefined && source.alt !== target.alt) {
		return false;
	}
	if (target.shift !== undefined && source.shift !== target.shift) {
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

const doesEventMatchShortcut = (event: React.KeyboardEvent, target: Partial<KeyCombo>): boolean => {
	const eventKey = event.key ? event.key.toLowerCase() : '';
	const targetKey = normalizeKeyName(target.key, target.code);
	if (!eventKey || (targetKey && targetKey !== eventKey)) {
		return false;
	}
	return modifiersMatch(
		{
			ctrlOrMeta: event.ctrlKey || event.metaKey,
			ctrl: event.ctrlKey,
			meta: event.metaKey,
			alt: event.altKey,
			shift: event.shiftKey,
		},
		target,
	);
};

const getConflictingKeybindActions = (): Set<KeybindAction> => {
	const actions = new Set<KeybindAction>();
	for (const {combo, action} of KeybindStore.getAll()) {
		for (const {combo: shortcutCombo} of MARKDOWN_FORMATTING_SHORTCUTS) {
			if (doesStoredComboMatchShortcut(combo, shortcutCombo)) {
				actions.add(action);
				break;
			}
		}
	}
	return actions;
};

class MarkdownKeybindScope {
	private disabledKeybinds = new Map<KeybindAction, KeyCombo>();
	private activeCount = 0;

	acquire(): () => void {
		this.activeCount += 1;
		if (this.activeCount === 1) {
			this.disableConflictingKeybinds();
		}

		let released = false;
		return () => {
			if (released) return;
			released = true;
			this.release();
		};
	}

	private release(): void {
		this.activeCount = Math.max(0, this.activeCount - 1);
		if (this.activeCount === 0) {
			this.restoreConflictingKeybinds();
		}
	}

	private disableConflictingKeybinds(): void {
		if (this.disabledKeybinds.size > 0) {
			return;
		}

		const actions = getConflictingKeybindActions();
		for (const action of actions) {
			const {combo} = KeybindStore.getByAction(action);
			if (combo.enabled === false) {
				continue;
			}
			this.disabledKeybinds.set(action, {...combo});
			KeybindStore.setKeybind(action, {...combo, enabled: false});
		}
	}

	private restoreConflictingKeybinds(): void {
		if (!this.disabledKeybinds.size) {
			return;
		}

		for (const [action, combo] of this.disabledKeybinds) {
			KeybindStore.setKeybind(action, combo);
		}
		this.disabledKeybinds.clear();
	}
}

const markdownKeybindScope = new MarkdownKeybindScope();

export const useMarkdownKeybinds = (active: boolean): void => {
	React.useEffect(() => {
		if (!active) {
			return;
		}
		const release = markdownKeybindScope.acquire();
		return release;
	}, [active]);
};

export {MARKDOWN_FORMATTING_SHORTCUTS, doesEventMatchShortcut};
