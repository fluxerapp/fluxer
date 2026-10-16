// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ElectronAPI} from '@app/types/electron.d';

export function isElectron(): boolean {
	if (typeof window === 'undefined') return false;
	return (
		(
			window as {
				electron?: ElectronAPI;
			}
		).electron !== undefined
	);
}

export function hasUnavailableElectronNativeContext(): boolean {
	if (typeof window === 'undefined' || typeof navigator === 'undefined') return false;
	return (
		(
			window as {
				electron?: ElectronAPI | null;
			}
		).electron == null && /\bElectron\/\d+(?:\.\d+)*/.test(navigator.userAgent)
	);
}
