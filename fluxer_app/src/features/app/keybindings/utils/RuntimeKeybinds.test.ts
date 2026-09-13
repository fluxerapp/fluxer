// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildCustomRuntimeKeybinds,
	buildDefaultRuntimeKeybinds,
	getCustomActionOverrides,
} from '@app/features/app/keybindings/utils/RuntimeKeybinds';
import type {CustomKeybindEntry, KeybindCommand, KeybindConfig, KeyCombo} from '@app/features/input/state/InputKeybind';
import {describe, expect, it} from 'vitest';

const action: KeybindCommand = 'voice_toggle_deafen';
const keybind = (combo: KeyCombo): KeybindConfig => ({
	action,
	label: 'Deafen',
	combo,
	section: 'voice_and_video',
});
const custom = (combo: KeyCombo, enabled: boolean): CustomKeybindEntry => ({
	id: 'custom-1',
	action,
	combo,
	enabled,
});
const resolver =
	(defaults: ReadonlyArray<KeybindConfig>) =>
	(candidate: KeybindCommand): KeybindConfig | null =>
		defaults.find((entry) => entry.action === candidate) ?? null;

describe('runtime keybind resolution', () => {
	it('treats disabled custom bindings as default action overrides', () => {
		const defaults = [keybind({key: 'd', ctrlOrMeta: true, shift: true})];
		const customs = [custom({key: ''}, false)];
		const overriddenActions = getCustomActionOverrides(customs);
		expect(buildDefaultRuntimeKeybinds(defaults, overriddenActions)).toEqual([]);
		expect(buildCustomRuntimeKeybinds(customs, resolver(defaults))).toEqual([]);
	});
	it('builds enabled custom runtime keybinds from the default action metadata', () => {
		const defaults = [keybind({key: 'd', ctrlOrMeta: true, shift: true})];
		const override = custom({key: 'm', ctrlOrMeta: true, global: true}, true);
		expect(buildCustomRuntimeKeybinds([override], resolver(defaults))).toEqual([
			{
				...defaults[0],
				combo: override.combo,
			},
		]);
	});
	it('keeps a local-only default local while an enabled custom replacement can be global', () => {
		const defaults = [{...keybind({key: 'd', ctrlOrMeta: true, shift: true, global: false}), allowGlobal: true}];
		const override = custom({key: 'F13', code: 'F13', global: true}, true);

		expect(buildDefaultRuntimeKeybinds(defaults, new Set())).toEqual(defaults);
		expect(buildDefaultRuntimeKeybinds(defaults, getCustomActionOverrides([override]))).toEqual([]);
		expect(buildCustomRuntimeKeybinds([override], resolver(defaults))).toEqual([
			{
				...defaults[0],
				combo: override.combo,
			},
		]);
	});
});
