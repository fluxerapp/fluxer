// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('StoreInitialization');

export function initializeStore(store: object, initialize: () => Promise<void>): Promise<void> {
	const report = (error: unknown): void => {
		logger.error(`${store.constructor.name} failed to initialize:`, error);
	};
	try {
		return initialize().catch(report);
	} catch (error) {
		report(error);
		return Promise.resolve();
	}
}
