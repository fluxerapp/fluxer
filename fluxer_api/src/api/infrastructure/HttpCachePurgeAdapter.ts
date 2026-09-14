// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APICachePurgeConfig} from '@app/api/config/APIConfig';
import type {CachePurgeAdapter, CachePurgeBatch, CachePurgeOutcome} from '@app/api/infrastructure/CachePurgeAdapter';
import * as FetchUtils from '@app/api/utils/FetchUtils';

function stripOneTrailingAsterisk(entry: string): string {
	return entry.endsWith('*') ? entry.slice(0, -1) : entry;
}

export function createHttpCachePurgeAdapter(config: APICachePurgeConfig): CachePurgeAdapter {
	return {
		async purge(batch: CachePurgeBatch): Promise<CachePurgeOutcome> {
			const headers: Record<string, string> = {'Content-Type': 'application/json'};
			if (config.http.token !== '') {
				headers.Authorization = `Bearer ${config.http.token}`;
			}
			let response: Response;
			try {
				response = await fetch(config.http.endpoint, {
					method: 'POST',
					headers,
					body: JSON.stringify({exact: batch.exact, prefix: batch.prefix.map(stripOneTrailingAsterisk)}),
					redirect: 'manual',
					signal: AbortSignal.timeout(config.http.timeoutMs),
				});
			} catch (error) {
				return {kind: 'failed', status: null, error};
			}
			FetchUtils.discardResponseBody(response.body, response.status);
			if (response.ok) {
				return {kind: 'purged'};
			}
			if (response.status === 400 || response.status === 422) {
				return {kind: 'invalid_entries', status: response.status};
			}
			return {kind: 'failed', status: response.status, error: null};
		},
	};
}
