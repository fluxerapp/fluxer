// SPDX-License-Identifier: AGPL-3.0-or-later

import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {isJsonRecord} from '../utils/JsonBoundaryUtils';

const OVERLOADED = 'overloaded';

export function svcErrorReplyReason(decoded: unknown): string | null {
	if (!isJsonRecord(decoded)) {
		return null;
	}
	const reason = decoded.error;
	return typeof reason === 'string' && reason.length > 0 ? reason : null;
}

export function throwForSvcErrorReply(service: string, decoded: unknown): void {
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
