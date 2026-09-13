// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CustomKeybindEntry, KeybindCommand, KeybindConfig, KeyCombo} from '@app/features/input/state/InputKeybind';
import {
	getActiveCombosForResolvedAction,
	getDisplayKeybindForResolvedAction,
} from '@app/features/input/state/KeybindResolution';
import {describe, expect, it} from 'vitest';

const action: KeybindCommand = 'voice_toggle_deafen';
const keybind = (combo: KeyCombo, overrides: Partial<KeybindConfig> = {}): KeybindConfig => ({
	action,
	label: 'Deafen',
	combo,
	section: 'voice_and_video',
	...overrides,
});
const custom = (combo: KeyCombo, enabled: boolean): CustomKeybindEntry => ({
	id: 'custom-1',
	action,
	combo,
	enabled,
});

describe('keybind action resolution', () => {
	it('uses the default combo when there is no custom override', () => {
		const base = keybind({key: 'd', ctrlOrMeta: true, shift: true});
		expect(getActiveCombosForResolvedAction(base, [])).toEqual([base.combo]);
		expect(getDisplayKeybindForResolvedAction(base, []).combo).toEqual(base.combo);
	});
	it('suppresses the default combo when an override is disabled and empty', () => {
		const base = keybind({key: 'd', ctrlOrMeta: true, shift: true});
		const bindings = [custom({key: ''}, false)];
		expect(getActiveCombosForResolvedAction(base, bindings)).toEqual([]);
		expect(getDisplayKeybindForResolvedAction(base, bindings).combo).toEqual({key: ''});
	});
	it('suppresses the default combo when an override is enabled but empty', () => {
		const base = keybind({key: 'd', ctrlOrMeta: true, shift: true});
		const bindings = [custom({key: ''}, true)];
		expect(getActiveCombosForResolvedAction(base, bindings)).toEqual([]);
		expect(getDisplayKeybindForResolvedAction(base, bindings).combo).toEqual({key: ''});
	});
	it('uses enabled custom combos instead of the default combo', () => {
		const base = keybind({key: 'd', ctrlOrMeta: true, shift: true});
		const override = custom({key: 'm', ctrlOrMeta: true}, true);
		expect(getActiveCombosForResolvedAction(base, [override])).toEqual([override.combo]);
		expect(getDisplayKeybindForResolvedAction(base, [override]).combo).toEqual(override.combo);
	});
});
