/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {getElectronAPI} from './NativeUtils';

export const setAutostartEnabled = async (enabled: boolean): Promise<boolean | null> => {
	const electronApi = getElectronAPI();
	if (!electronApi) return null;

	try {
		if (enabled) {
			await electronApi.autostartEnable();
		} else {
			await electronApi.autostartDisable();
		}
		return await electronApi.autostartIsEnabled();
	} catch (error) {
		console.error('Failed to update autostart status', error);
		return null;
	}
};

export const getAutostartStatus = async (): Promise<boolean | null> => {
	const electronApi = getElectronAPI();
	if (!electronApi) return null;

	try {
		return await electronApi.autostartIsEnabled();
	} catch (error) {
		console.error('Failed to read autostart status', error);
		return null;
	}
};

export const ensureAutostartDefaultEnabled = async (): Promise<boolean | null> => {
	const electronApi = getElectronAPI();
	if (!electronApi) return null;

	try {
		if (electronApi.platform !== 'darwin') {
			return await electronApi.autostartIsEnabled();
		}

		const initialized = await electronApi.autostartIsInitialized();
		let enabled = await electronApi.autostartIsEnabled();

		if (!initialized && !enabled) {
			await electronApi.autostartEnable();
			enabled = await electronApi.autostartIsEnabled();
		}

		if (!initialized) {
			await electronApi.autostartMarkInitialized();
		}

		return enabled;
	} catch (error) {
		console.error('Failed to ensure default autostart', error);
		return null;
	}
};
