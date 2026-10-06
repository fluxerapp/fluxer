// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import type {KeybindCommand, KeyCombo} from '@app/features/input/state/InputKeybind';
import Keybind from '@app/features/input/state/InputKeybind';
import {
	formatPrimaryKeySymbol,
	getPrimaryKeyForComboDisplay,
	isPrimaryKeyAlreadyRepresentedByModifier,
} from '@app/features/input/utils/KeybindUtils';
import {SHIFT_KEY_DESCRIPTOR} from '@app/features/input/utils/KeyboardUtils';
import styles from '@app/features/ui/keybind_hint/KeybindHint.module.css';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

export const CTRL_DESCRIPTOR = msg({
	message: 'Ctrl',
	comment: 'Keyboard shortcut keycap label for Control on non-Mac platforms. Keep it short.',
});
const WIN_DESCRIPTOR = msg({
	message: 'Win',
	comment: 'Keyboard shortcut keycap label for the Windows key on non-Mac platforms. Keep it short.',
});
const ALT_DESCRIPTOR = msg({
	message: 'Alt',
	comment: 'Keyboard shortcut keycap label for Alt. Keep it short.',
});
const SPACE_DESCRIPTOR = msg({
	message: 'Space',
	comment: 'Keyboard shortcut keycap label for the spacebar. Use a compact key name.',
});
const ENTER_DESCRIPTOR = msg({
	message: 'Enter',
	comment: 'Keyboard shortcut keycap label for Enter. Keep it short.',
});
const ESC_DESCRIPTOR = msg({
	message: 'Esc',
	comment: 'Keyboard shortcut keycap label for Escape. Keep the abbreviation compact.',
});
const TAB_DESCRIPTOR = msg({
	message: 'Tab',
	comment: 'Keyboard shortcut keycap label for Tab. Keep it short.',
});
const BACKSPACE_DESCRIPTOR = msg({
	message: 'Backspace',
	comment: 'Keyboard shortcut keycap label for Backspace. Use the standard key name.',
});
const PGUP_DESCRIPTOR = msg({
	message: 'PgUp',
	comment: 'Keyboard shortcut keycap label for Page Up. Keep the abbreviation compact.',
});
const PGDN_DESCRIPTOR = msg({
	message: 'PgDn',
	comment: 'Keyboard shortcut keycap label for Page Down. Keep the abbreviation compact.',
});
const isMac = () => /Mac|iPod|iPhone|iPad/.test(navigator.platform);

interface KeyPart {
	label: string;
	isSymbol?: boolean;
}

const isShiftKeyName = (key: string): boolean => key === 'Shift' || key === 'ShiftLeft' || key === 'ShiftRight';

const formatKeyParts = (i18n: I18n, combo: KeyCombo): Array<KeyPart> => {
	const parts: Array<KeyPart> = [];
	const mac = isMac();
	if (combo.ctrl) {
		parts.push(mac ? {label: '⌃', isSymbol: true} : {label: i18n._(CTRL_DESCRIPTOR)});
	} else if (combo.ctrlOrMeta) {
		parts.push(mac ? {label: '⌘', isSymbol: true} : {label: i18n._(CTRL_DESCRIPTOR)});
	}
	if (combo.meta) {
		parts.push(mac ? {label: '⌘', isSymbol: true} : {label: i18n._(WIN_DESCRIPTOR)});
	}
	if (combo.shift) {
		parts.push({label: i18n._(SHIFT_KEY_DESCRIPTOR)});
	}
	if (combo.alt) {
		parts.push(mac ? {label: '⌥', isSymbol: true} : {label: i18n._(ALT_DESCRIPTOR)});
	}
	const key = getPrimaryKeyForComboDisplay(combo);
	if (isPrimaryKeyAlreadyRepresentedByModifier(combo)) {
		return parts;
	}
	if (key === ' ') {
		parts.push({label: i18n._(SPACE_DESCRIPTOR)});
	} else if (key === 'Spacebar' || key === 'Space') {
		parts.push({label: i18n._(SPACE_DESCRIPTOR)});
	} else if (key === 'ArrowUp') {
		parts.push({label: '↑', isSymbol: true});
	} else if (key === 'ArrowDown') {
		parts.push({label: '↓', isSymbol: true});
	} else if (key === 'ArrowLeft') {
		parts.push({label: '←', isSymbol: true});
	} else if (key === 'ArrowRight') {
		parts.push({label: '→', isSymbol: true});
	} else if (key === 'Enter') {
		parts.push(mac ? {label: '↵', isSymbol: true} : {label: i18n._(ENTER_DESCRIPTOR)});
	} else if (key === 'Escape') {
		parts.push({label: i18n._(ESC_DESCRIPTOR)});
	} else if (key === 'Tab') {
		parts.push({label: i18n._(TAB_DESCRIPTOR)});
	} else if (key === 'Backspace') {
		parts.push(mac ? {label: '⌫', isSymbol: true} : {label: i18n._(BACKSPACE_DESCRIPTOR)});
	} else if (key === 'PageUp') {
		parts.push({label: i18n._(PGUP_DESCRIPTOR)});
	} else if (key === 'PageDown') {
		parts.push({label: i18n._(PGDN_DESCRIPTOR)});
	} else if (isShiftKeyName(key)) {
		parts.push({label: i18n._(SHIFT_KEY_DESCRIPTOR)});
	} else if (/^Key[A-Z]$/.test(key)) {
		parts.push({label: key.slice(3)});
	} else if (/^Digit[0-9]$/.test(key)) {
		parts.push({label: key.slice(5)});
	} else if (/^Numpad[0-9]$/.test(key)) {
		parts.push({label: formatPrimaryKeySymbol(i18n, key)});
	} else if (key.length === 1) {
		parts.push({label: key.toUpperCase()});
	} else if (key) {
		parts.push({label: formatPrimaryKeySymbol(i18n, key)});
	}
	return parts;
};

export interface KeybindHintProps {
	action?: KeybindCommand;
	combo?: KeyCombo;
}

export const KeybindHint = ({action, combo}: KeybindHintProps) => {
	const {i18n} = useLingui();
	const resolvedCombo = combo ?? (action ? Keybind.getByAction(action).combo : null);
	if (!resolvedCombo || (!resolvedCombo.key && !resolvedCombo.code)) {
		return null;
	}
	const parts = formatKeyParts(i18n, resolvedCombo);
	if (parts.length === 0) {
		return null;
	}
	return (
		<span className={styles.keybindHint} data-flx="ui.keybind-hint.keybind-hint.keybind-hint">
			{parts.map((part, index) => (
				<kbd
					key={index}
					className={part.isSymbol ? styles.keySymbol : styles.key}
					data-flx="ui.keybind-hint.keybind-hint.key-symbol"
				>
					{part.label}
				</kbd>
			))}
		</span>
	);
};

export interface TooltipWithKeybindProps {
	label: string;
	action?: KeybindCommand;
	combo?: KeyCombo;
}

export const TooltipWithKeybind = observer(({label, action, combo}: TooltipWithKeybindProps) => {
	const resolvedCombo = combo ?? (action ? Keybind.getByAction(action).combo : null);
	const hasKeybind = resolvedCombo && (resolvedCombo.key || resolvedCombo.code);
	const shouldShowKeybind = hasKeybind && !Accessibility.hideKeyboardHints;
	return (
		<div className={styles.tooltipContent} data-flx="ui.keybind-hint.keybind-hint.tooltip-with-keybind.tooltip-content">
			<span className={styles.label} data-flx="ui.keybind-hint.keybind-hint.tooltip-with-keybind.label">
				{label}
			</span>
			{shouldShowKeybind && (
				<KeybindHint combo={resolvedCombo} data-flx="ui.keybind-hint.keybind-hint.tooltip-with-keybind.keybind-hint" />
			)}
		</div>
	);
});
