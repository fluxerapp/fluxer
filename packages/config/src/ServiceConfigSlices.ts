// SPDX-License-Identifier: AGPL-3.0-or-later

import {getBuildMetadata} from '@fluxer/config/src/BuildMetadata';
import type {MasterConfig} from '@fluxer/config/src/MasterConfig';

export function extractBaseServiceConfig(master: MasterConfig) {
	return {
		env: master.env,
	};
}

export function extractKVClientConfig(master: MasterConfig) {
	if (!master.internal) {
		throw new Error('internal configuration is required for KV client access');
	}
	return {
		kvUrl: master.internal.kv,
		kvMode: master.internal.kv_mode,
		kvClusterNodes: master.internal.kv_cluster_nodes,
		kvClusterNatMap: master.internal.kv_cluster_nat_map,
	};
}

export function extractBuildInfoConfig() {
	const metadata = getBuildMetadata();
	return {
		releaseChannel: metadata.releaseChannel,
		buildVersion: metadata.buildVersion,
	};
}

export function extractRateLimit(
	rawRateLimit:
		| {
				limit?: number | null;
				window_ms?: number | null;
		  }
		| null
		| undefined,
):
	| {
			limit: number;
			windowMs: number;
	  }
	| undefined {
	if (!rawRateLimit || rawRateLimit.limit == null || rawRateLimit.window_ms == null) {
		return undefined;
	}
	return {
		limit: rawRateLimit.limit,
		windowMs: rawRateLimit.window_ms,
	};
}
