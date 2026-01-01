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

import {configurePersistable, makePersistable, stopPersisting} from 'mobx-persist-store';
import {Logger} from './Logger';

const logger = new Logger('MobXPersistence');

const persistedStores = new Set<string>();

const getStorage = () => {
	return 'localStorage' in window ? window.localStorage : undefined;
};

configurePersistable({
	storage: getStorage(),
	expireIn: undefined,
	removeOnExpiration: false,
	stringify: true,
	debugMode: false,
});

export const makePersistent = async <T extends object>(
	store: T,
	storageKey: string,
	properties: Array<keyof T>,
	options?: {
		expireIn?: number;
		removeOnExpiration?: boolean;
		version?: number;
	},
): Promise<void> => {
	try {
		if (persistedStores.has(storageKey)) {
			logger.debug(`Store ${storageKey} is already being persisted, skipping...`);
			return;
		}

		await makePersistable(store, {
			name: storageKey,
			properties: properties as Array<keyof T & string>,
			storage: getStorage(),
			expireIn: options?.expireIn,
			removeOnExpiration: options?.removeOnExpiration,
			stringify: true,
			version: options?.version ?? 1,
		});

		persistedStores.add(storageKey);
		logger.debug(`Store ${storageKey} hydrated from localStorage and is now persisting.`);
	} catch (error) {
		logger.error(`Failed to hydrate store ${storageKey}:`, error);
	}
};

export const stopPersistent = (storageKey: string, store: object): void => {
	try {
		stopPersisting(store);
		persistedStores.delete(storageKey);
		logger.debug(`Stopped persisting store: ${storageKey}`);
	} catch (error) {
		logger.error(`Failed to stop persisting store ${storageKey}:`, error);
	}
};
