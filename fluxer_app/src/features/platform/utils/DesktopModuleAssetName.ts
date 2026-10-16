// SPDX-License-Identifier: AGPL-3.0-or-later

import {isDesktopModuleName} from '@fluxer/desktop_ipc/src/ModuleContract';

const ASSETS_PATH_SEGMENT = 'assets';

export function desktopModuleNameForAsset(assetUrl: string): string | null {
	let pathname: string;
	try {
		pathname = new URL(assetUrl, 'http://localhost').pathname;
	} catch {
		return null;
	}
	const segments = pathname.split('/').filter(Boolean);
	const assetsIndex = segments.lastIndexOf(ASSETS_PATH_SEGMENT);
	if (assetsIndex < 0 || segments.length - assetsIndex !== 3) {
		return null;
	}
	const moduleName = segments[assetsIndex + 1];
	return isDesktopModuleName(moduleName) ? moduleName : null;
}
