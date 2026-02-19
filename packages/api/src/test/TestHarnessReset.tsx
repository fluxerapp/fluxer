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

import {clearSqliteStore} from '@fluxer/api/src/database/SqliteKV';
import {Logger} from '@fluxer/api/src/Logger';
import {resetSearchServices} from '@fluxer/api/src/SearchFactory';
import type {IKVProvider} from '@fluxer/kv_client/src/IKVProvider';
import type {S3Service} from '@fluxer/s3/src/s3/S3Service';

export type TestHarnessResetHandler = () => Promise<void>;

let registeredHandler: TestHarnessResetHandler | null = null;

export function registerTestHarnessReset(handler: TestHarnessResetHandler | null): void {
	registeredHandler = handler;
}

export async function resetTestHarnessState(): Promise<void> {
	if (!registeredHandler) {
		throw new Error('Test harness reset handler not registered');
	}
	await registeredHandler();
}

interface CreateTestHarnessResetOptions {
	kvProvider?: IKVProvider;
	s3Service?: S3Service;
}

export function createTestHarnessResetHandler(options: CreateTestHarnessResetOptions): TestHarnessResetHandler {
	return async () => {
		Logger.info('Resetting test harness state');

		clearSqliteStore();

		if (options.kvProvider) {
			Logger.info('Clearing KV storage');
			const keys = await options.kvProvider.scan('*', 100000);
			if (keys.length > 0) {
				await options.kvProvider.del(...keys);
			}
		}

		if (options.s3Service) {
			Logger.info('Wiping S3 storage');
			await options.s3Service.clearAll();
		}

		resetSearchServices();

		Logger.info('Test harness state reset complete');
	};
}
