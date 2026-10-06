// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {DesktopTroubleshootingSettings} from '@app/types/electron.d';

const logger = new Logger('DesktopTroubleshootingUtils');

let cachedSettings: DesktopTroubleshootingSettings | null = null;
let pendingPromise: Promise<DesktopTroubleshootingSettings | null> | null = null;

function fetchSettings(): Promise<DesktopTroubleshootingSettings | null> {
	const electronApi = getElectronAPI();
	if (!electronApi?.getDesktopTroubleshootingSettings) return Promise.resolve(null);
	return electronApi
		.getDesktopTroubleshootingSettings()
		.then((settings) => {
			cachedSettings = settings;
			return settings;
		})
		.catch((error) => {
			logger.error('Failed to read desktop troubleshooting settings', error);
			return null;
		});
}

export function getCachedDesktopTroubleshootingSettings(): DesktopTroubleshootingSettings | null {
	return cachedSettings;
}

export function getDesktopTroubleshootingSettings(): Promise<DesktopTroubleshootingSettings | null> {
	if (cachedSettings) return Promise.resolve(cachedSettings);
	if (pendingPromise) return pendingPromise;
	pendingPromise = fetchSettings().finally(() => {
		pendingPromise = null;
	});
	return pendingPromise;
}

export async function setDesktopDisableHardwareAcceleration(
	disable: boolean,
	options?: {restart?: boolean},
): Promise<DesktopTroubleshootingSettings | null> {
	const electronApi = getElectronAPI();
	if (!electronApi?.setDesktopDisableHardwareAcceleration) return null;
	try {
		const next = await electronApi.setDesktopDisableHardwareAcceleration({
			disable,
			restart: options?.restart ?? false,
		});
		cachedSettings = next;
		return next;
	} catch (error) {
		logger.error('Failed to update desktop hardware-acceleration setting', error);
		return null;
	}
}
