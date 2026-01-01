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

import AppStorage from '~/lib/AppStorage';
import {IS_DEV} from '~/lib/env';
import {HttpError} from '~/lib/HttpError';

export const LogLevel = {
	Trace: 0,
	Debug: 1,
	Info: 2,
	Warn: 3,
	Error: 4,
	Fatal: 5,
	Silent: 6,
} as const;
export type LogLevel = (typeof LogLevel)[keyof typeof LogLevel];

const LEVEL_NAME_BY_VALUE: Record<number, keyof typeof LogLevel> = {
	[LogLevel.Trace]: 'Trace',
	[LogLevel.Debug]: 'Debug',
	[LogLevel.Info]: 'Info',
	[LogLevel.Warn]: 'Warn',
	[LogLevel.Error]: 'Error',
	[LogLevel.Fatal]: 'Fatal',
	[LogLevel.Silent]: 'Silent',
};

const DEFAULT_STYLES = {
	Trace: {color: '#6c757d', fontWeight: 'normal'},
	Debug: {color: '#17a2b8', fontWeight: 'normal'},
	Info: {color: '#28a745', fontWeight: 'normal'},
	Warn: {color: '#ffc107', fontWeight: 'normal'},
	Error: {color: '#dc3545', fontWeight: 'normal'},
	Fatal: {color: '#dc3545', fontWeight: 'bold'},
};

const pad2 = (value: number): string => (value < 10 ? `0${value}` : `${value}`);
const formatTimestamp = (date: Date): string =>
	`${pad2(date.getHours())}:${pad2(date.getMinutes())}:${pad2(date.getSeconds())}`;

const resolveDefaultMinLevel = (): LogLevel =>
	AppStorage.getItem('debugLoggingEnabled') === 'true' || IS_DEV ? LogLevel.Debug : LogLevel.Info;

export class Logger {
	private name: string;
	private minLevelOverride?: LogLevel;
	private static globalMinLevel: LogLevel = resolveDefaultMinLevel();

	constructor(name = 'default', minLevelOverride?: LogLevel) {
		this.name = name;
		this.minLevelOverride = minLevelOverride;
	}

	static refreshGlobalLogLevel(): void {
		Logger.globalMinLevel = resolveDefaultMinLevel();
	}

	private getCurrentLogLevel(): LogLevel {
		if (this.minLevelOverride !== undefined) {
			return this.minLevelOverride;
		}
		return Logger.globalMinLevel;
	}

	child(suffix: string): Logger {
		return new Logger(`${this.name}:${suffix}`, this.minLevelOverride);
	}

	trace(...args: Array<unknown>): void {
		this.log(LogLevel.Trace, ...args);
	}
	debug(...args: Array<unknown>): void {
		this.log(LogLevel.Debug, ...args);
	}
	info(...args: Array<unknown>): void {
		this.log(LogLevel.Info, ...args);
	}
	warn(...args: Array<unknown>): void {
		this.log(LogLevel.Warn, ...args);
	}
	error(...args: Array<unknown>): void {
		if (this.shouldDemoteHttp404(args)) {
			this.log(LogLevel.Debug, ...args);
			return;
		}

		this.log(LogLevel.Error, ...args);
	}

	private shouldDemoteHttp404(args: Array<unknown>): boolean {
		return args.some((value) => value instanceof HttpError && value.status === 404);
	}
	fatal(...args: Array<unknown>): void {
		this.log(LogLevel.Fatal, ...args);
	}

	private log(level: LogLevel, ...args: Array<unknown>): void {
		const minLevel = this.getCurrentLogLevel();
		if (level < minLevel) return;

		const levelName = LEVEL_NAME_BY_VALUE[level] || 'Unknown';
		const timestamp = formatTimestamp(new Date());
		const prefix = `[${timestamp}] [${this.name}] [${levelName}]`;

		const style = DEFAULT_STYLES[levelName as keyof typeof DEFAULT_STYLES];
		const consoleMethod = this.getConsoleMethod(level);

		if (style) {
			console[consoleMethod](
				`%c${prefix}`,
				`color:${style.color};font-weight:${style.fontWeight || 'normal'}`,
				...args,
			);
		} else {
			console[consoleMethod](prefix, ...args);
		}
	}

	private getConsoleMethod(level: LogLevel): 'log' | 'debug' | 'info' | 'warn' | 'error' {
		switch (level) {
			case LogLevel.Trace:
			case LogLevel.Debug:
				return 'debug';
			case LogLevel.Info:
				return 'info';
			case LogLevel.Warn:
				return 'warn';
			case LogLevel.Error:
			case LogLevel.Fatal:
				return 'error';
			default:
				return 'log';
		}
	}
}
