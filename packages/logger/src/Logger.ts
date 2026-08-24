// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import pino, {type Logger as PinoLogger} from 'pino';

interface LoggerOptions {
	level?: pino.Level;
	environment?: string;
	serviceVersion?: string;
	baseProperties?: Record<string, unknown>;
}

interface PinoTransportOptions {
	target: string;
	options?: Record<string, unknown>;
	caller?: Array<string>;
	worker?: {
		autoEnd?: boolean;
		endTimeout?: number;
	};
}

const PINO_LEVELS: ReadonlyArray<pino.Level> = ['fatal', 'error', 'warn', 'info', 'debug', 'trace'];

function isDevelopment(environment: string): boolean {
	return environment === 'development';
}

function isPinoLevel(value: string): value is pino.Level {
	return (PINO_LEVELS as ReadonlyArray<string>).includes(value);
}

function resolveEnvironment(options: LoggerOptions): string {
	return options.environment ?? process.env.FLUXER_ENV ?? 'production';
}

function resolveLevel(options: LoggerOptions, isDev: boolean): pino.Level {
	if (options.level) {
		return options.level;
	}
	const configured = process.env.LOG_LEVEL?.trim().toLowerCase();
	if (configured && isPinoLevel(configured)) {
		return configured;
	}
	return isDev ? 'debug' : 'info';
}

function createPinoLogger(serviceName: string, options: LoggerOptions = {}): PinoLogger {
	const environment = resolveEnvironment(options);
	const isDev = isDevelopment(environment);
	const level = resolveLevel(options, isDev);
	const streams: Array<pino.StreamEntry> = [];
	if (isDev) {
		try {
			const require = createRequire(import.meta.url);
			const pinoPrettyTarget = require.resolve('pino-pretty');
			streams.push({
				level: 'trace',
				stream: pino.transport({
					target: pinoPrettyTarget,
					options: {
						colorize: true,
						translateTime: 'HH:MM:ss.l',
						ignore: 'pid,hostname',
						messageFormat: '{msg}',
					},
					sync: true,
				} as PinoTransportOptions),
			});
		} catch (error) {
			console.warn('pino-pretty not available, falling back to stdout', error);
			streams.push({
				level: 'trace',
				stream: pino.destination({dest: 1, sync: true}),
			});
		}
	} else {
		streams.push({
			level: 'trace',
			stream: pino.destination({dest: 1, sync: false}),
		});
	}
	const destination =
		streams.length === 1 && streams[0] ? streams[0].stream : pino.multistream(streams, {dedupe: true});
	const pinoOptions: pino.LoggerOptions = {
		level,
		formatters: {
			level: (label) => ({level: label}),
		},
		errorKey: 'error',
		serializers: {
			reason: (value) => {
				if (value instanceof Error) {
					return pino.stdSerializers.errWithCause(value);
				}
				return value;
			},
			err: pino.stdSerializers.errWithCause,
			error: pino.stdSerializers.errWithCause,
		},
		timestamp: pino.stdTimeFunctions.isoTime,
		base: {
			service: serviceName,
			env: environment,
			...options.baseProperties,
		},
	};
	return pino(pinoOptions, destination);
}

export class Logger {
	private logger: PinoLogger;

	constructor(serviceName: string, options: LoggerOptions = {}) {
		this.logger = createPinoLogger(serviceName, options);
	}

	getPinoLogger(): PinoLogger {
		return this.logger;
	}

	setPinoLogger(logger: PinoLogger): void {
		this.logger = logger;
	}

	static createWithLogger(logger: PinoLogger): Logger {
		const childLogger = new Logger('', {});
		childLogger.setPinoLogger(logger);
		return childLogger;
	}

	trace(obj: Record<string, unknown>, msg?: string): void;
	trace(msg: string): void;
	trace(objOrMsg: Record<string, unknown> | string, msg?: string): void {
		if (typeof objOrMsg === 'string') {
			this.logger.trace(objOrMsg);
		} else if (msg) {
			this.logger.trace(objOrMsg, msg);
		} else {
			this.logger.trace(objOrMsg);
		}
	}

	debug(obj: Record<string, unknown>, msg?: string): void;
	debug(msg: string): void;
	debug(objOrMsg: Record<string, unknown> | string, msg?: string): void {
		if (typeof objOrMsg === 'string') {
			this.logger.debug(objOrMsg);
		} else if (msg) {
			this.logger.debug(objOrMsg, msg);
		} else {
			this.logger.debug(objOrMsg);
		}
	}

	info(obj: Record<string, unknown>, msg?: string): void;
	info(msg: string): void;
	info(objOrMsg: Record<string, unknown> | string, msg?: string): void {
		if (typeof objOrMsg === 'string') {
			this.logger.info(objOrMsg);
		} else if (msg) {
			this.logger.info(objOrMsg, msg);
		} else {
			this.logger.info(objOrMsg);
		}
	}

	warn(obj: Record<string, unknown>, msg?: string): void;
	warn(msg: string): void;
	warn(objOrMsg: Record<string, unknown> | string, msg?: string): void {
		if (typeof objOrMsg === 'string') {
			this.logger.warn(objOrMsg);
		} else if (msg) {
			this.logger.warn(objOrMsg, msg);
		} else {
			this.logger.warn(objOrMsg);
		}
	}

	error(obj: Record<string, unknown>, msg?: string): void;
	error(msg: string): void;
	error(objOrMsg: Record<string, unknown> | string, msg?: string): void {
		if (typeof objOrMsg === 'string') {
			this.logger.error(objOrMsg);
		} else if (msg) {
			this.logger.error(objOrMsg, msg);
		} else {
			this.logger.error(objOrMsg);
		}
	}

	fatal(obj: Record<string, unknown>, msg?: string): void;
	fatal(msg: string): void;
	fatal(objOrMsg: Record<string, unknown> | string, msg?: string): void {
		if (typeof objOrMsg === 'string') {
			this.logger.fatal(objOrMsg);
		} else if (msg) {
			this.logger.fatal(objOrMsg, msg);
		} else {
			this.logger.fatal(objOrMsg);
		}
	}

	child(bindings: Record<string, unknown>): Logger {
		const childPinoLogger = this.logger.child(bindings);
		return Logger.createWithLogger(childPinoLogger);
	}

	get pino(): PinoLogger {
		return this.logger;
	}
}

export function createLogger(serviceName: string, options?: LoggerOptions): Logger {
	return new Logger(serviceName, options);
}
