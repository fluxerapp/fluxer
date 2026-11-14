// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {loadLazyModule} from '@app/features/platform/utils/LazyModuleLoader';

const logger = new Logger('DesktopDeferredCommand');

export function createBootDeferredCommand<Args extends Array<unknown>>(load: () => Promise<(...args: Args) => void>) {
	let loaded: ((...args: Args) => void) | null = null;
	let pending: ReturnType<typeof load> | null = null;
	const loadOnce = () => {
		pending ??= loadLazyModule(load)
			.then((command) => {
				loaded = command;
				return command;
			})
			.catch((error: unknown) => {
				pending = null;
				throw error;
			});
		return pending;
	};
	const reportError = (error: unknown) => logger.error('Failed to load a desktop command', error);
	return (...args: Args): void => {
		if (loaded !== null) {
			loaded(...args);
			return;
		}
		void loadOnce()
			.then((command) => command(...args))
			.catch(reportError);
	};
}
