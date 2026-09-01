// SPDX-License-Identifier: AGPL-3.0-or-later

const TRANSIENT_POSTGRES_SQLSTATES: ReadonlySet<string> = new Set([
	'08000',
	'08001',
	'08003',
	'08004',
	'08006',
	'08007',
	'57P01',
	'57P02',
	'57P03',
]);

const TRANSIENT_SOCKET_CODES: ReadonlySet<string> = new Set([
	'EAI_AGAIN',
	'ECONNABORTED',
	'ECONNREFUSED',
	'ECONNRESET',
	'EHOSTUNREACH',
	'ENETDOWN',
	'ENETUNREACH',
	'ENOTFOUND',
	'EPIPE',
	'ETIMEDOUT',
]);

const TRANSIENT_DRIVER_MESSAGES: ReadonlySet<string> = new Set([
	'Client has encountered a connection error and is not queryable',
	'Client was closed and is not queryable',
	'Connection terminated',
	'Connection terminated due to connection timeout',
	'Connection terminated unexpectedly',
	'timeout exceeded when trying to connect',
]);

const MAX_ERROR_CHAIN_DEPTH = 8;

type ErrorNode = Record<string, unknown>;

function readString(value: unknown): string | null {
	return typeof value === 'string' ? value : null;
}

function collectErrorChain(error: unknown): Array<ErrorNode> {
	const nodes: Array<ErrorNode> = [];
	const queue: Array<unknown> = [error];
	const seen = new Set<unknown>();
	while (queue.length > 0 && nodes.length < MAX_ERROR_CHAIN_DEPTH) {
		const current = queue.shift();
		if (typeof current !== 'object' || current === null || seen.has(current)) {
			continue;
		}
		seen.add(current);
		const node = current as ErrorNode;
		nodes.push(node);
		if ('cause' in node) {
			queue.push(node['cause']);
		}
		const aggregated = node['errors'];
		if (Array.isArray(aggregated)) {
			for (const nested of aggregated) {
				queue.push(nested);
			}
		}
	}
	return nodes;
}

function carriesPostgresClient(node: ErrorNode): boolean {
	const client = node['client'];
	return typeof client === 'object' && client !== null;
}

function hasTransientSqlState(node: ErrorNode): boolean {
	const code = readString(node['code']);
	return code !== null && TRANSIENT_POSTGRES_SQLSTATES.has(code);
}

function hasTransientSocketCode(node: ErrorNode): boolean {
	const code = readString(node['code']);
	return code !== null && TRANSIENT_SOCKET_CODES.has(code);
}

function hasTransientDriverMessage(node: ErrorNode): boolean {
	const message = readString(node['message']);
	return message !== null && TRANSIENT_DRIVER_MESSAGES.has(message);
}

export function isTransientDatabaseError(error: unknown): boolean {
	const nodes = collectErrorChain(error);
	if (nodes.some(hasTransientSqlState)) {
		return true;
	}
	if (nodes.some(carriesPostgresClient) && nodes.some(hasTransientSocketCode)) {
		return true;
	}
	return nodes.some(hasTransientDriverMessage);
}
