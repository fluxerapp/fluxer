// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KeybindCommand, KeyCombo} from '@app/features/input/state/InputKeybind';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';

type ZoomKeybindAction = Extract<KeybindCommand, 'system_zoom_in' | 'system_zoom_out' | 'system_zoom_reset'>;

const ZOOM_ACTIONS = new Set<ZoomKeybindAction>(['system_zoom_in', 'system_zoom_out', 'system_zoom_reset']);
const ZOOM_IN_KEYS = new Set(['=', '+', 'equal', 'numpadadd']);
const ZOOM_OUT_KEYS = new Set(['-', '_', 'minus', 'numpadsubtract']);
const ZOOM_RESET_KEYS = new Set(['0', ')', 'digit0', 'numpad0']);

interface ZoomKeybindEnvironment {
	isDesktop?: boolean;
	isFirefoxBrowser?: boolean;
}

const normalizeKey = (rawKey: string | undefined): string => {
	return (rawKey ?? '').trim().toLowerCase();
};
const usesBrowserZoomModifier = (combo: KeyCombo): boolean => {
	return !!(combo.ctrlOrMeta || combo.ctrl || combo.meta);
};

export function isWebReservedZoomShortcut(
	action: KeybindCommand,
	combo: KeyCombo,
	environment: Pick<ZoomKeybindEnvironment, 'isDesktop'> = {},
): boolean {
	if (!ZOOM_ACTIONS.has(action as ZoomKeybindAction)) {
		return false;
	}
	const desktop = environment.isDesktop ?? isDesktop();
	if (desktop) {
		return false;
	}
	if (!usesBrowserZoomModifier(combo) || combo.alt) {
		return false;
	}
	const normalizedKey = normalizeKey(combo.code ?? combo.key);
	switch (action) {
		case 'system_zoom_in':
			return ZOOM_IN_KEYS.has(normalizedKey);
		case 'system_zoom_out':
			return ZOOM_OUT_KEYS.has(normalizedKey);
		case 'system_zoom_reset':
			return ZOOM_RESET_KEYS.has(normalizedKey);
		default:
			return false;
	}
}
