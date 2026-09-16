// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APICachePurgeConfig} from '@app/api/config/APIConfig';
import {createHttpCachePurgeAdapter} from '@app/api/infrastructure/HttpCachePurgeAdapter';
import {createNoneCachePurgeAdapter} from '@app/api/infrastructure/NoneCachePurgeAdapter';
import type {CachePurgeAdapterName} from '@fluxer/config/src/MasterConfig';

export type CachePurgeOutcome =
	| {readonly kind: 'purged'}
	| {readonly kind: 'rejected'; readonly status: number}
	| {readonly kind: 'failed'; readonly status: number | null; readonly error: unknown};

export interface CachePurgeAdapter {
	purge(prefixes: ReadonlyArray<string>): Promise<CachePurgeOutcome>;
}

const CACHE_PURGE_ADAPTERS = {
	none: createNoneCachePurgeAdapter,
	http: createHttpCachePurgeAdapter,
} satisfies Record<CachePurgeAdapterName, (config: APICachePurgeConfig) => CachePurgeAdapter>;

export function createCachePurgeAdapter(config: APICachePurgeConfig): CachePurgeAdapter {
	return CACHE_PURGE_ADAPTERS[config.adapter](config);
}
