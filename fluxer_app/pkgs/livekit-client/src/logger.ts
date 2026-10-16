// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import * as log from 'loglevel';

export enum LogLevel {
	trace = 0,
	debug = 1,
	info = 2,
	warn = 3,
	error = 4,
	silent = 5,
}

export enum LoggerNames {
	Default = 'livekit',
	Room = 'livekit-room',
	TokenSource = 'livekit-token-source',
	Participant = 'livekit-participant',
	Track = 'livekit-track',
	Publication = 'livekit-track-publication',
	Engine = 'livekit-engine',
	Signal = 'livekit-signal',
	PCManager = 'livekit-pc-manager',
	PCTransport = 'livekit-pc-transport',
	E2EE = 'lk-e2ee',
	DataTracks = 'livekit-data-tracks',
	Region = 'livekit-region',
	ICE = 'livekit-ice',
	Stats = 'livekit-stats',
}

type LogLevelString = keyof typeof LogLevel;

export type StructuredLogger = log.Logger & {
	trace: (msg: string, context?: object) => void;
	debug: (msg: string, context?: object) => void;
	info: (msg: string, context?: object) => void;
	warn: (msg: string, context?: object) => void;
	error: (msg: string, context?: object) => void;
	setDefaultLevel: (level: log.LogLevelDesc) => void;
	setLevel: (level: log.LogLevelDesc) => void;
	getLevel: () => number;
};

export type ContextProvider = () => object | undefined;

const livekitLogger = log.getLogger(LoggerNames.Default);
const livekitLoggers = Object.values(LoggerNames).map((name) => log.getLogger(name));

livekitLogger.setDefaultLevel(LogLevel.info);

export default livekitLogger as StructuredLogger;

export function getLogger(name: string, ctxFn?: ContextProvider) {
	const logger = log.getLogger(name);
	logger.setDefaultLevel(livekitLogger.getLevel());
	if (!ctxFn) {
		return logger as StructuredLogger;
	}
	return wrapWithContext(logger as StructuredLogger, ctxFn);
}

function wrapWithContext(base: StructuredLogger, ctxFn: ContextProvider): StructuredLogger {
	type LogMethod = 'trace' | 'debug' | 'info' | 'warn' | 'error';
	const wrap = (method: LogMethod) => (msg: string, extra?: object) => {
		const ctx = ctxFn();
		const merged = ctx || extra ? {...ctx, ...extra} : undefined;
		base[method](msg, merged);
	};

	const proxy = Object.create(base) as StructuredLogger;
	proxy.trace = wrap('trace');
	proxy.debug = wrap('debug');
	proxy.info = wrap('info');
	proxy.warn = wrap('warn');
	proxy.error = wrap('error');
	return proxy;
}

export function setLogLevel(level: LogLevel | LogLevelString, loggerName?: LoggerNames) {
	if (loggerName) {
		log.getLogger(loggerName).setLevel(level);
	} else {
		for (const logger of livekitLoggers) {
			logger.setLevel(level);
		}
	}
}

export type LogExtension = (level: LogLevel, msg: string, context?: object) => void;

export function setLogExtension(extension: LogExtension, logger?: StructuredLogger) {
	const loggers = logger ? [logger] : livekitLoggers;

	loggers.forEach((logR) => {
		const originalFactory = logR.methodFactory;

		logR.methodFactory = (methodName, configLevel, loggerName) => {
			const rawMethod = originalFactory(methodName, configLevel, loggerName);

			const logLevel = LogLevel[methodName as LogLevelString];
			const needLog = logLevel >= configLevel && logLevel < LogLevel.silent;

			return (msg, context?: [msg: string, context: object]) => {
				if (context) rawMethod(msg, context);
				else rawMethod(msg);
				if (needLog) {
					extension(logLevel, msg, context);
				}
			};
		};
		logR.setLevel(logR.getLevel());
	});
}

export const workerLogger = log.getLogger(LoggerNames.E2EE) as StructuredLogger;

const workerLogLevelListeners = new Set<(level: LogLevel) => void>();

const originalWorkerSetLevel = workerLogger.setLevel.bind(workerLogger);
workerLogger.setLevel = ((level: log.LogLevelDesc, persist?: boolean) => {
	originalWorkerSetLevel(level, persist);
	const numeric = workerLogger.getLevel() as LogLevel;
	workerLogLevelListeners.forEach((cb) => cb(numeric));
}) as typeof workerLogger.setLevel;

export function onWorkerLogLevelChanged(cb: (level: LogLevel) => void): () => void {
	workerLogLevelListeners.add(cb);
	return () => {
		workerLogLevelListeners.delete(cb);
	};
}

export function getWorkerLogLevelListenerCount(): number {
	return workerLogLevelListeners.size;
}
