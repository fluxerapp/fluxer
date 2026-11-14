// SPDX-License-Identifier: AGPL-3.0-or-later

import {DesktopLegacyImportPhase} from '@fluxer/desktop_ipc/src/StorageContract';

let bootPhase: DesktopLegacyImportPhase | null = null;

export function recordDesktopLegacyImportPhase(phase: DesktopLegacyImportPhase | null): void {
	bootPhase = phase;
}

export function desktopLegacyImportIsComplete(): boolean {
	return bootPhase === DesktopLegacyImportPhase.DONE;
}
