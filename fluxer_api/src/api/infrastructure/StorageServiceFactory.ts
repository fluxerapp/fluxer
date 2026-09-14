// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {StorageService} from '@app/api/infrastructure/StorageService';

export function createStorageService(): IStorageService {
	return new StorageService();
}

export function createDownloadsStorageService(): IStorageService | null {
	if (!Config.s3Downloads.isOverridden) {
		return null;
	}
	return new StorageService(Config.s3Downloads.settings);
}
