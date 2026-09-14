// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import pino, {type Logger as PinoLogger} from 'pino';

interface LoggerOptions {
	level?: pino.Level;
	environment?: string;
	baseProperties?: Record<string, unknown>;
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
	let destination: pino.DestinationStream;
	if (isDev) {
		try {
			const require = createRequire(import.meta.url);
			const pinoPrettyTarget = require.resolve('pino-pretty');
			const transportOptions: pino.TransportSingleOptions & {sync: boolean} = {
				target: pinoPrettyTarget,
				options: {
					colorize: true,
					translateTime: 'HH:MM:ss.l',
					ignore: 'pid,hostname',
					messageFormat: '{msg}',
				},
				sync: true,
			};
			destination = pino.transport(transportOptions);
		} catch (error) {
			console.warn('pino-pretty not available, falling back to stdout', error);
			destination = pino.destination({dest: 1, sync: true});
		}
	} else {
		destination = pino.destination({dest: 1, sync: false});
	}
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

	constructor(serviceName: string, options?: LoggerOptions);
	constructor(pinoLogger: PinoLogger);
	constructor(serviceNameOrPinoLogger: string | PinoLogger, options: LoggerOptions = {}) {
		this.logger =
			typeof serviceNameOrPinoLogger === 'string'
				? createPinoLogger(serviceNameOrPinoLogger, options)
				: serviceNameOrPinoLogger;
	}

	getPinoLogger(): PinoLogger {
		return this.logger;
	}

	setPinoLogger(logger: PinoLogger): void {
		this.logger = logger;
	}

	static createWithLogger(logger: PinoLogger): Logger {
		return new Logger(logger);
	}

	trace(obj: Record<string, unknown>, msg?: string): void;
	trace(msg: string): void;
	trace(objOrMsg: Record<string, unknown> | string, msg?: string): void {
		this.log('trace', objOrMsg, msg);
	}

	debug(obj: Record<string, unknown>, msg?: string): void;
	debug(msg: string): void;
	debug(objOrMsg: Record<string, unknown> | string, msg?: string): void {
		this.log('debug', objOrMsg, msg);
	}

	info(obj: Record<string, unknown>, msg?: string): void;
	info(msg: string): void;
	info(objOrMsg: Record<string, unknown> | string, msg?: string): void {
		this.log('info', objOrMsg, msg);
	}

	warn(obj: Record<string, unknown>, msg?: string): void;
	warn(msg: string): void;
	warn(objOrMsg: Record<string, unknown> | string, msg?: string): void {
		this.log('warn', objOrMsg, msg);
	}

	error(obj: Record<string, unknown>, msg?: string): void;
	error(msg: string): void;
	error(objOrMsg: Record<string, unknown> | string, msg?: string): void {
		this.log('error', objOrMsg, msg);
	}

	fatal(obj: Record<string, unknown>, msg?: string): void;
	fatal(msg: string): void;
	fatal(objOrMsg: Record<string, unknown> | string, msg?: string): void {
		this.log('fatal', objOrMsg, msg);
	}

	private log(level: pino.Level, objOrMsg: Record<string, unknown> | string, msg?: string): void {
		if (typeof objOrMsg === 'string') {
			this.logger[level](objOrMsg);
		} else if (msg) {
			this.logger[level](objOrMsg, msg);
		} else {
			this.logger[level](objOrMsg);
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
