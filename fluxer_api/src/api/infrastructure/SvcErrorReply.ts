// SPDX-License-Identifier: AGPL-3.0-or-later

import {isJsonRecord, parseJsonRecord, parseJsonWithGuard} from '@app/api/utils/JsonBoundaryUtils';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';

const OVERLOADED = 'overloaded';

function svcErrorReplyReason(decoded: unknown): string | null {
	if (!isJsonRecord(decoded)) {
		return null;
	}
	const reason = decoded.error;
	return typeof reason === 'string' && reason.length > 0 ? reason : null;
}

function throwForSvcErrorReply(service: string, decoded: unknown): void {
	const reason = svcErrorReplyReason(decoded);
	if (reason === null) {
		return;
	}
	if (reason === OVERLOADED) {
		throw new ServiceUnavailableError({
			message: `[${service}] shard rejected the request because it is at its concurrency limit`,
			headers: {'Retry-After': '1'},
		});
	}
	throw new ServiceUnavailableError({message: `[${service}] ${reason}`});
}

const textEncoder = new TextEncoder();
const textDecoder = new TextDecoder();

export async function requestSvcJson<T>(
	connectionManager: INatsConnectionManager,
	subject: string,
	payload: unknown,
	options: {
		timeoutMs: number;
		guard: (value: unknown) => value is T;
		service: string;
		invalidPayloadError: () => Error;
	},
): Promise<T> {
	if (connectionManager.isClosed()) await connectionManager.connect();
	const response = await connectionManager
		.getConnection()
		.request(subject, textEncoder.encode(JSON.stringify(payload)), {timeout: options.timeoutMs});
	const decoded = textDecoder.decode(response.data);
	const parsed = parseJsonWithGuard(decoded, options.guard);
	if (parsed === null) {
		throwForSvcErrorReply(options.service, parseJsonRecord(decoded));
		throw options.invalidPayloadError();
	}
	return parsed;
}
