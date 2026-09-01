// SPDX-License-Identifier: AGPL-3.0-or-later

import {ms} from 'itty-time';
import {isTransientDatabaseError} from '../database/TransientDatabaseError';
import type {ILogger} from '../ILogger';

export type WorkerProcessErrorSource = 'uncaughtException' | 'unhandledRejection';

type WorkerProcessErrorHandler = (source: WorkerProcessErrorSource, error: unknown) => Promise<void>;

interface WorkerProcessErrorHandlerOptions {
	logger: Pick<ILogger, 'error' | 'warn'>;
	shutdown: () => Promise<void>;
	exit: (code: number) => void;
	forceExitDelayMs?: number;
}

const FATAL_MESSAGE: Record<WorkerProcessErrorSource, string> = {
	uncaughtException: 'Uncaught Exception',
	unhandledRejection: 'Unhandled Rejection at Promise',
};

export function createWorkerProcessErrorHandler(options: WorkerProcessErrorHandlerOptions): WorkerProcessErrorHandler {
	const forceExitDelayMs = options.forceExitDelayMs ?? ms('5 seconds');
	return async (source, error) => {
		if (isTransientDatabaseError(error)) {
			options.logger.warn(
				{err: error, source},
				'Transient database connection error reached the worker process, keeping the worker running',
			);
			return;
		}
		options.logger.error({err: error, source}, FATAL_MESSAGE[source]);
		const forceExit = setTimeout(() => options.exit(1), forceExitDelayMs);
		forceExit.unref();
		try {
			await options.shutdown();
		} catch (shutdownError) {
			options.logger.error({err: shutdownError, source}, 'Worker shutdown failed while handling a fatal error');
		}
		clearTimeout(forceExit);
		options.exit(1);
	};
}
