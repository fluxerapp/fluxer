// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ElectronAPI} from '@app/types/electron.d';

export interface DesktopStoreAccessOptions {
	readonly allowUnavailable?: boolean;
}

export function resolveDesktopStoreAPI<T extends object>(
	select: (electron: ElectronAPI) => T | undefined,
	methods: ReadonlyArray<keyof T>,
	options?: DesktopStoreAccessOptions,
): T | null {
	const electron = globalThis.window?.electron;
	if (electron == null) {
		return null;
	}
	if (options?.allowUnavailable !== true && electron.capabilities?.appStore === false) {
		return null;
	}
	const candidate = select(electron);
	if (candidate == null) {
		return null;
	}
	return methods.every((method) => typeof candidate[method] === 'function') ? candidate : null;
}
