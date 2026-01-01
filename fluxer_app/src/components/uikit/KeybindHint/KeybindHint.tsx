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

import {observer} from 'mobx-react-lite';
import React from 'react';
import AccessibilityStore from '~/stores/AccessibilityStore';
import type {KeybindAction, KeyCombo} from '~/stores/KeybindStore';
import KeybindStore from '~/stores/KeybindStore';
import {SHIFT_KEY_SYMBOL} from '~/utils/KeyboardUtils';
import styles from './KeybindHint.module.css';

const isMac = () => /Mac|iPod|iPhone|iPad/.test(navigator.platform);

interface KeyPart {
	label: string;
	isSymbol?: boolean;
}

const formatKeyParts = (combo: KeyCombo): Array<KeyPart> => {
	const parts: Array<KeyPart> = [];
	const mac = isMac();

	if (combo.ctrl) {
		parts.push(mac ? {label: '⌃', isSymbol: true} : {label: 'Ctrl'});
	} else if (combo.ctrlOrMeta) {
		parts.push(mac ? {label: '⌘', isSymbol: true} : {label: 'Ctrl'});
	}
	if (combo.meta) {
		parts.push(mac ? {label: '⌘', isSymbol: true} : {label: 'Win'});
	}
	if (combo.shift) {
		parts.push({label: SHIFT_KEY_SYMBOL, isSymbol: true});
	}
	if (combo.alt) {
		parts.push(mac ? {label: '⌥', isSymbol: true} : {label: 'Alt'});
	}

	const key = combo.code ?? combo.key ?? '';
	if (key === ' ') {
		parts.push({label: 'Space'});
	} else if (key === 'ArrowUp') {
		parts.push({label: '↑', isSymbol: true});
	} else if (key === 'ArrowDown') {
		parts.push({label: '↓', isSymbol: true});
	} else if (key === 'ArrowLeft') {
		parts.push({label: '←', isSymbol: true});
	} else if (key === 'ArrowRight') {
		parts.push({label: '→', isSymbol: true});
	} else if (key === 'Enter') {
		parts.push(mac ? {label: '↵', isSymbol: true} : {label: 'Enter'});
	} else if (key === 'Escape') {
		parts.push({label: 'Esc'});
	} else if (key === 'Tab') {
		parts.push({label: 'Tab'});
	} else if (key === 'Backspace') {
		parts.push(mac ? {label: '⌫', isSymbol: true} : {label: 'Backspace'});
	} else if (key === 'PageUp') {
		parts.push({label: 'PgUp'});
	} else if (key === 'PageDown') {
		parts.push({label: 'PgDn'});
	} else if (key.length === 1) {
		parts.push({label: key.toUpperCase()});
	} else if (key) {
		parts.push({label: key});
	}

	return parts;
};

export interface KeybindHintProps {
	action?: KeybindAction;
	combo?: KeyCombo;
}

export const KeybindHint = ({action, combo}: KeybindHintProps) => {
	const resolvedCombo = React.useMemo(() => {
		if (combo) return combo;
		if (action) return KeybindStore.getByAction(action).combo;
		return null;
	}, [action, combo]);

	if (!resolvedCombo || (!resolvedCombo.key && !resolvedCombo.code)) {
		return null;
	}

	const parts = formatKeyParts(resolvedCombo);

	if (parts.length === 0) {
		return null;
	}

	return (
		<span className={styles.keybindHint}>
			{parts.map((part, index) => (
				<kbd key={index} className={part.isSymbol ? styles.keySymbol : styles.key}>
					{part.label}
				</kbd>
			))}
		</span>
	);
};

export interface TooltipWithKeybindProps {
	label: string;
	action?: KeybindAction;
	combo?: KeyCombo;
}

export const TooltipWithKeybind = observer(({label, action, combo}: TooltipWithKeybindProps) => {
	const resolvedCombo = React.useMemo(() => {
		if (combo) return combo;
		if (action) return KeybindStore.getByAction(action).combo;
		return null;
	}, [action, combo]);

	const hasKeybind = resolvedCombo && (resolvedCombo.key || resolvedCombo.code);
	const shouldShowKeybind = hasKeybind && !AccessibilityStore.hideKeyboardHints;

	return (
		<div className={styles.tooltipContent}>
			<span className={styles.label}>{label}</span>
			{shouldShowKeybind && <KeybindHint combo={resolvedCombo} />}
		</div>
	);
});
