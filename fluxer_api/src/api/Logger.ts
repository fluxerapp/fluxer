// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';

let _logger: LoggerInterface | null = null;

export function initializeLogger(logger: LoggerInterface): void {
	if (_logger !== null) {
		return;
	}
	_logger = logger;
}

export const Logger: LoggerInterface = new Proxy({} as LoggerInterface, {
	get(_target, prop: keyof LoggerInterface | symbol) {
		if (_logger === null) {
			throw new Error('Logger has not been initialized. Call initializeLogger() first.');
		}
		const value = _logger[prop as keyof LoggerInterface];
		if (typeof value === 'function') {
			return value.bind(_logger);
		}
		return value;
	},
	set() {
		throw new Error('Cannot modify Logger directly. Use initializeLogger() instead.');
	},
});
