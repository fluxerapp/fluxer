// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APICachePurgeConfig} from '@app/api/config/APIConfig';
import type {CachePurgeAdapter, CachePurgeOutcome} from '@app/api/infrastructure/CachePurgeAdapter';
import * as FetchUtils from '@app/api/utils/FetchUtils';

const MAX_PATTERNS_PER_REQUEST = 30;

export function createControllerCachePurgeAdapter(config: APICachePurgeConfig): CachePurgeAdapter {
	async function purgeChunk(patterns: ReadonlyArray<string>): Promise<CachePurgeOutcome> {
		let response: Response;
		try {
			response = await fetch(config.controller.endpoint, {
				method: 'POST',
				headers: {'Content-Type': 'application/json', Authorization: `Bearer ${config.controller.token}`},
				body: JSON.stringify({patterns, soft: false}),
				redirect: 'manual',
				signal: AbortSignal.timeout(config.controller.timeoutMs),
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
	}

	return {
		async purge(prefixes: ReadonlyArray<string>): Promise<CachePurgeOutcome> {
			const patterns = prefixes.map((prefix) => `${prefix}*`);
			for (let start = 0; start < patterns.length; start += MAX_PATTERNS_PER_REQUEST) {
				const outcome = await purgeChunk(patterns.slice(start, start + MAX_PATTERNS_PER_REQUEST));
				if (outcome.kind !== 'purged') {
					return outcome;
				}
			}
			return {kind: 'purged'};
		},
	};
}
