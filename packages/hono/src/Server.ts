// SPDX-License-Identifier: AGPL-3.0-or-later

import {applyFluxerVersionHeader} from '@fluxer/hono/src/middleware/VersionHeader';
import {type Http2Bindings, type HttpBindings, type ServerType, serve} from '@hono/node-server';
import type {Env, Hono} from 'hono';

const DEFAULT_HEADERS_TIMEOUT_MS = 30_000;
const DEFAULT_REQUEST_TIMEOUT_MS = 120_000;
const DEFAULT_KEEP_ALIVE_TIMEOUT_MS = 125_000;
const DEFAULT_MAX_REQUESTS_PER_SOCKET = 1_000;

interface ServerOptions {
	port: number;
	hostname?: string;
	onListen?: (info: {address: string; port: number}) => void;
	headersTimeoutMs?: number;
	requestTimeoutMs?: number;
	keepAliveTimeoutMs?: number;
	maxRequestsPerSocket?: number;
}

type NodeFetchCallback = (request: Request, env: HttpBindings | Http2Bindings) => Promise<unknown> | unknown;

function createVersionedFetch<E extends Env>(app: Hono<E>): NodeFetchCallback {
	return async (request, env) => applyFluxerVersionHeader(await app.fetch(request, env as E['Bindings']));
}

export function createServer<E extends Env = Env>(app: Hono<E>, options: ServerOptions): ServerType {
	const {
		port,
		hostname,
		onListen,
		headersTimeoutMs = DEFAULT_HEADERS_TIMEOUT_MS,
		requestTimeoutMs = DEFAULT_REQUEST_TIMEOUT_MS,
		keepAliveTimeoutMs = DEFAULT_KEEP_ALIVE_TIMEOUT_MS,
		maxRequestsPerSocket = DEFAULT_MAX_REQUESTS_PER_SOCKET,
	} = options;
	const server = serve(
		{
			fetch: createVersionedFetch(app),
			port,
			...(hostname !== undefined && {hostname}),
			serverOptions: {
				keepAliveTimeout: keepAliveTimeoutMs,
				headersTimeout: requestTimeoutMs > 0 ? Math.min(headersTimeoutMs, requestTimeoutMs) : headersTimeoutMs,
				requestTimeout: requestTimeoutMs,
			},
		},
		onListen,
	);
	if ('maxRequestsPerSocket' in server) {
		server.maxRequestsPerSocket = maxRequestsPerSocket;
	}
	return server;
}

type CleanupFunction = () => void | Promise<void>;

interface ShutdownLogger {
	info(msg: string): void;
	info(obj: Record<string, unknown>, msg: string): void;
	error(msg: string): void;
	error(obj: Record<string, unknown>, msg: string): void;
}

const defaultShutdownLogger: ShutdownLogger = {
	info(objOrMsg: Record<string, unknown> | string, msg?: string): void {
		if (typeof objOrMsg === 'string') {
			process.stdout.write(`[info] ${objOrMsg}\n`);
		} else if (msg) {
			process.stdout.write(`[info] ${msg} ${JSON.stringify(objOrMsg)}\n`);
		}
	},
	error(objOrMsg: Record<string, unknown> | string, msg?: string): void {
		if (typeof objOrMsg === 'string') {
			process.stderr.write(`[error] ${objOrMsg}\n`);
		} else if (msg) {
			process.stderr.write(`[error] ${msg} ${JSON.stringify(objOrMsg)}\n`);
		}
	},
};

interface GracefulShutdownOptions {
	logger?: ShutdownLogger;
	timeoutMs?: number;
}

export function setupGracefulShutdown(cleanupFn: CleanupFunction, options?: GracefulShutdownOptions): void {
	const logger = options?.logger ?? defaultShutdownLogger;
	const timeoutMs = options?.timeoutMs;
	let isShuttingDown = false;
	const shutdown = async (signal: string): Promise<void> => {
		if (isShuttingDown) {
			return;
		}
		isShuttingDown = true;
		logger.info({signal}, `Received ${signal}, shutting down gracefully...`);
		let timeoutHandle: NodeJS.Timeout | undefined;
		if (timeoutMs && timeoutMs > 0) {
			timeoutHandle = setTimeout(() => {
				logger.error({timeoutMs}, 'Forcing shutdown after timeout');
				process.exit(1);
			}, timeoutMs);
		}
		try {
			await cleanupFn();
			if (timeoutHandle) {
				clearTimeout(timeoutHandle);
			}
		} catch (err) {
			logger.error({err: err instanceof Error ? err : new Error(String(err))}, 'Error during shutdown');
			process.exit(1);
		}
		process.exit(0);
	};
	process.on('SIGINT', () => shutdown('SIGINT'));
	process.on('SIGTERM', () => shutdown('SIGTERM'));
}
