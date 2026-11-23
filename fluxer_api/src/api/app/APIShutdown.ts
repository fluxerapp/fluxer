// SPDX-License-Identifier: AGPL-3.0-or-later

import {settleWithin} from '@app/api/utils/ConcurrencyUtils';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';

export const API_SHUTDOWN_TIMEOUT_MS = 55_000;
export const API_HTTP_DRAIN_MS = 25_000;

interface ClosableServer {
	close(callback: (error?: Error) => void): unknown;
	closeAllConnections?(): void;
}

function closeServer(server: ClosableServer): Promise<void> {
	return new Promise<void>((resolve, reject) => {
		server.close((error) => {
			if (error !== undefined) {
				reject(error);
				return;
			}
			resolve();
		});
	});
}

export async function closeHttpServers(
	servers: ReadonlyArray<ClosableServer>,
	budgetMs: number,
	logger: LoggerInterface,
): Promise<boolean> {
	const closed = Promise.all(servers.map(closeServer)).catch((error: unknown) => {
		logger.error({error}, 'Error closing API servers at shutdown');
	});
	const drained = await settleWithin(closed, budgetMs);
	if (drained) {
		return true;
	}
	logger.warn({budgetMs}, 'API requests still open at shutdown, closing their connections');
	for (const server of servers) {
		server.closeAllConnections?.();
	}
	return false;
}
