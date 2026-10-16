// SPDX-License-Identifier: AGPL-3.0-or-later

import {desktopModuleNameForAsset} from '@app/features/platform/utils/DesktopModuleAssetName';
import {createDesktopModuleRequest, type DesktopModuleRequest} from '@app/features/platform/utils/DesktopModuleRequest';

const requests = new Map<string, DesktopModuleRequest>();

export async function ensureDesktopModule(moduleName: string): Promise<boolean> {
	let request = requests.get(moduleName);
	if (request == null) {
		request = createDesktopModuleRequest(moduleName);
		requests.set(moduleName, request);
	}
	if (request.isSettled()) {
		return true;
	}
	await request.ensure();
	return request.isSettled();
}

export function ensureDesktopModuleForAsset(assetUrl: string): Promise<boolean> {
	const moduleName = desktopModuleNameForAsset(assetUrl);
	if (moduleName == null) {
		return Promise.resolve(true);
	}
	return ensureDesktopModule(moduleName);
}
