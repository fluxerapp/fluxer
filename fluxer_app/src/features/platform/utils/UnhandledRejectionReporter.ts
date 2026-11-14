// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {isBrowserOffline, isLazyModuleLoadError} from '@app/features/platform/utils/LazyModuleLoader';

const logger = new Logger('UnhandledRejection');

let detach: (() => void) | null = null;

function handleUnhandledRejection(event: PromiseRejectionEvent): void {
	const reason: unknown = event.reason;
	if (isLazyModuleLoadError(reason)) {
		event.preventDefault();
		logger.warn(
			isBrowserOffline()
				? 'A module could not be loaded while offline, so the feature stays unavailable'
				: 'A module could not be loaded, so the feature stays unavailable',
			reason,
		);
		return;
	}
	logger.error('Unhandled promise rejection', reason);
}

export function installUnhandledRejectionReporter(): () => void {
	if (typeof window === 'undefined') {
		return () => {};
	}
	if (detach) {
		return detach;
	}
	window.addEventListener('unhandledrejection', handleUnhandledRejection);
	detach = () => {
		window.removeEventListener('unhandledrejection', handleUnhandledRejection);
		detach = null;
	};
	return detach;
}
