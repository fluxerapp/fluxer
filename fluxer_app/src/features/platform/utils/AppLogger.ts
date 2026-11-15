// SPDX-License-Identifier: AGPL-3.0-or-later

import {isAccountTransitionAbortError} from '@app/features/platform/state/AccountTransitionAbort';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {IS_DEV} from '@app/features/platform/types/Env';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const LogLevel = {
	Trace: 0,
	Debug: 1,
	Info: 2,
	Warn: 3,
	Error: 4,
	Fatal: 5,
	Silent: 6,
} as const;

export type LogLevel = ValueOf<typeof LogLevel>;

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
const ACCOUNT_TRANSITION_ABORT_DESCRIPTION = '(cancelled by an account switch or logout)';
const hasDOMException = typeof DOMException !== 'undefined';
const describeLogArgument = (value: unknown): unknown => {
	if (hasDOMException && value instanceof DOMException) {
		return `${value.name}: ${value.message}`;
	}
	return value;
};
const describeAccountTransitionAbort = (value: unknown): unknown =>
	isAccountTransitionAbortError(value) ? ACCOUNT_TRANSITION_ABORT_DESCRIPTION : value;
const pad2 = (value: number): string => (value < 10 ? `0${value}` : `${value}`);
const formatTimestamp = (date: Date): string =>
	`${pad2(date.getHours())}:${pad2(date.getMinutes())}:${pad2(date.getSeconds())}`;
const resolveDefaultMinLevel = (): LogLevel => {
	if (IS_DEV) {
		return LogLevel.Debug;
	}
	try {
		return AppStorage.getItem('debugLoggingEnabled') === 'true' ? LogLevel.Debug : LogLevel.Info;
	} catch {
		return LogLevel.Info;
	}
};

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

	static create(name: string, minLevelOverride?: LogLevel): Logger {
		return new Logger(name, minLevelOverride);
	}

	private getCurrentLogLevel(): LogLevel {
		if (this.minLevelOverride !== undefined) {
			return this.minLevelOverride;
		}
		return Logger.globalMinLevel;
	}

	isLevelEnabled(level: LogLevel): boolean {
		return level >= this.getCurrentLogLevel();
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
		if (args.some(isAccountTransitionAbortError)) {
			this.log(LogLevel.Debug, ...args.map(describeAccountTransitionAbort));
			return;
		}
		this.log(LogLevel.Warn, ...args);
	}

	error(...args: Array<unknown>): void {
		if (args.some(isAccountTransitionAbortError)) {
			this.log(LogLevel.Debug, ...args.map(describeAccountTransitionAbort));
			return;
		}
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
		const levelName = LEVEL_NAME_BY_VALUE[level] || String(level);
		const timestamp = formatTimestamp(new Date());
		const prefix = `[${timestamp}] [${this.name}] [${levelName}]`;
		const style = DEFAULT_STYLES[levelName as keyof typeof DEFAULT_STYLES];
		const consoleMethod = this.getConsoleMethod(level);
		const described = args.map(describeLogArgument);
		if (style) {
			console[consoleMethod](
				`%c${prefix}`,
				`color:${style.color};font-weight:${style.fontWeight || 'normal'}`,
				...described,
			);
		} else {
			console[consoleMethod](prefix, ...described);
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
