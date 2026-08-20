// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '../Config';
import type {IStorageService} from './IStorageService';
import {StorageService} from './StorageService';

export function createStorageService(): IStorageService {
	return new StorageService();
}

export function createDownloadsStorageService(): IStorageService | null {
	if (!Config.s3Downloads.isOverridden) {
		return null;
	}
	return new StorageService(Config.s3Downloads.settings);
}
