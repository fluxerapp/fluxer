// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KeyCombo} from '@app/features/input/state/InputKeybind';
import {
	ACCOUNT_SWITCHER_SLOT_ACTIONS,
	type KeybindCommand,
} from '@app/features/input/state/input_keybind/KeybindCommands';

export interface AccountKeybindDefault {
	readonly action: KeybindCommand;
	readonly combo: KeyCombo;
}

export function getAccountKeybindDefaults(applePlatform: boolean): ReadonlyArray<AccountKeybindDefault> {
	return [
		{action: 'system_open_account_switcher', combo: {key: '', code: 'KeyO', ctrlOrMeta: true, shift: true}},
		{action: 'system_switch_account_next', combo: {key: '', code: 'BracketRight', ctrlOrMeta: true, shift: true}},
		{action: 'system_switch_account_prev', combo: {key: '', code: 'BracketLeft', ctrlOrMeta: true, shift: true}},
		{action: 'system_switch_account_recent', combo: {key: '', code: 'KeyJ', ctrlOrMeta: true}},
		{action: 'system_jump_account_mention', combo: {key: '', code: 'KeyJ', ctrlOrMeta: true, shift: true}},
		{action: 'nav_quick_switcher_accounts', combo: {key: '', code: 'KeyK', ctrlOrMeta: true, shift: true}},
		...ACCOUNT_SWITCHER_SLOT_ACTIONS.map((action, index) => ({
			action,
			combo: applePlatform
				? {key: '', code: `Digit${index + 1}`, ctrlOrMeta: true, alt: true}
				: {key: '', code: `Digit${index + 1}`, ctrlOrMeta: true, shift: true},
		})),
	];
}
