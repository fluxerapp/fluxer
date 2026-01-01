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

import type {KeyCombo} from '~/stores/KeybindStore';
import {SHIFT_KEY_SYMBOL} from './KeyboardUtils';

const isMac = () => /Mac|iPod|iPhone|iPad/.test(navigator.platform);
const CONTROL_KEY_SYMBOL = '⌃';

export const formatKeyCombo = (combo: KeyCombo): string => {
	const parts: Array<string> = [];
	if (combo.ctrl) {
		parts.push(isMac() ? CONTROL_KEY_SYMBOL : 'Ctrl');
	} else if (combo.ctrlOrMeta) {
		parts.push(isMac() ? '⌘' : 'Ctrl');
	}
	if (combo.meta) {
		parts.push(isMac() ? '⌘' : 'Win');
	}
	if (combo.shift) {
		const shiftLabel = isMac() ? SHIFT_KEY_SYMBOL : 'Shift';
		parts.push(shiftLabel);
	}
	if (combo.alt) parts.push(isMac() ? '⌥' : 'Alt');
	const key = combo.code ?? combo.key ?? '';
	if (key === ' ') {
		parts.push('Space');
	} else if (key.length === 1) {
		parts.push(key.toUpperCase());
	} else {
		parts.push(key);
	}
	return parts.join(' + ');
};
