// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APICachePurgeConfig} from '@app/api/config/APIConfig';
import type {CachePurgeAdapter, CachePurgeOutcome} from '@app/api/infrastructure/CachePurgeAdapter';
import * as FetchUtils from '@app/api/utils/FetchUtils';

export function createHttpCachePurgeAdapter(config: APICachePurgeConfig): CachePurgeAdapter {
	return {
		async purge(prefixes: ReadonlyArray<string>): Promise<CachePurgeOutcome> {
			let response: Response;
			try {
				response = await fetch(config.http.endpoint, {
					method: 'POST',
					headers: {'Content-Type': 'application/json', Authorization: `Bearer ${config.http.token}`},
					body: JSON.stringify({prefixes}),
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
				return {kind: 'rejected', status: response.status};
			}
			return {kind: 'failed', status: response.status, error: null};
		},
	};
}
