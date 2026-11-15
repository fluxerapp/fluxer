// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import type {DesktopLegacyHarvest} from '@fluxer/desktop_ipc/src/LegacyHarvestContract';

const logger = new Logger('LegacyOriginHarvestSource');

export async function loadLegacyOriginHarvest(): Promise<DesktopLegacyHarvest | null> {
	const harvestAPI = globalThis.window?.electron?.desktopLegacyHarvest;
	if (harvestAPI == null || typeof harvestAPI.read !== 'function') {
		return null;
	}
	try {
		return await harvestAPI.read();
	} catch (error) {
		logger.error('The staged legacy harvest could not be read, deferring desktop authority', error);
		throw error;
	}
}
