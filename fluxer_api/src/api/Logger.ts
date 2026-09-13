// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ILogger} from '@app/api/ILogger';

let _logger: ILogger | null = null;

export function initializeLogger(logger: ILogger): void {
	if (_logger !== null) {
		return;
	}
	_logger = logger;
}

export const Logger: ILogger = new Proxy({} as ILogger, {
	get(_target, prop: keyof ILogger | symbol) {
		if (_logger === null) {
			throw new Error('Logger has not been initialized. Call initializeLogger() first.');
		}
		const value = _logger[prop as keyof ILogger];
		if (typeof value === 'function') {
			return value.bind(_logger);
		}
		return value;
	},
	set() {
		throw new Error('Cannot modify Logger directly. Use initializeLogger() instead.');
	},
});
