// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KeyCombo} from '@app/features/input/state/InputKeybind';

export function isKeybindModifierKey(key: string | undefined | null): boolean {
	if (!key) return false;
	return key === 'Shift' || key === 'Control' || key === 'Alt' || key === 'AltGraph' || key === 'Meta';
}

export function combosLooseEqual(a: KeyCombo | null | undefined, b: KeyCombo | null | undefined): boolean {
	if (!a && !b) return true;
	if (!a || !b) return false;
	if ((a.key ?? '') !== (b.key ?? '')) return false;
	if ((a.code ?? '') !== (b.code ?? '')) return false;
	if (!!a.ctrlOrMeta !== !!b.ctrlOrMeta) return false;
	if (!!a.ctrl !== !!b.ctrl) return false;
	if (!!a.alt !== !!b.alt) return false;
	if (!!a.shift !== !!b.shift) return false;
	if (!!a.meta !== !!b.meta) return false;
	if (!!a.modifierOnly !== !!b.modifierOnly) return false;
	if (!!a.bothSides !== !!b.bothSides) return false;
	if ((a.mouseButton ?? null) !== (b.mouseButton ?? null)) return false;
	if ((a.gamepadButton ?? null) !== (b.gamepadButton ?? null)) return false;
	return true;
}
