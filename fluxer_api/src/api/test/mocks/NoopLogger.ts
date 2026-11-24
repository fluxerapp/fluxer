// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';

export class NoopLogger implements LoggerInterface {
	trace(..._args: Array<unknown>): void {}
	debug(..._args: Array<unknown>): void {}
	info(..._args: Array<unknown>): void {}
	warn(..._args: Array<unknown>): void {}
	error(..._args: Array<unknown>): void {}
	fatal(..._args: Array<unknown>): void {}

	child(): this {
		return this;
	}
}
