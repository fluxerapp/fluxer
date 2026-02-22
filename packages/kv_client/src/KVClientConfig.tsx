/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {DEFAULT_KV_TIMEOUT_MS} from '@fluxer/constants/src/Timeouts';

export interface IKVLogger {
	debug(obj: object, msg?: string): void;
	error(obj: object, msg?: string): void;
}

export type KVClientMode = 'standalone' | 'cluster';

export interface KVClusterNode {
	host: string;
	port: number;
}

export interface KVClientConfig {
	url: string;
	mode?: KVClientMode;
	clusterNodes?: Array<KVClusterNode>;
	clusterNatMap?: Record<string, KVClusterNode>;
	timeoutMs?: number;
	logger?: IKVLogger;
}

export interface ResolvedKVClientConfig {
	url: string;
	mode: KVClientMode;
	clusterNodes: Array<KVClusterNode>;
	clusterNatMap: Record<string, KVClusterNode>;
	timeoutMs: number;
	logger: IKVLogger;
}

const noopLogger: IKVLogger = {
	debug() {},
	error() {},
};

export function resolveKVClientConfig(config: KVClientConfig | string): ResolvedKVClientConfig {
	if (typeof config === 'string') {
		return {
			url: normalizeUrl(config),
			mode: 'standalone' as const,
			clusterNodes: [],
			clusterNatMap: {},
			timeoutMs: DEFAULT_KV_TIMEOUT_MS,
			logger: noopLogger,
		};
	}

	return {
		url: normalizeUrl(config.url),
		mode: config.mode ?? 'standalone',
		clusterNodes: config.clusterNodes ?? [],
		clusterNatMap: config.clusterNatMap ?? {},
		timeoutMs: config.timeoutMs ?? DEFAULT_KV_TIMEOUT_MS,
		logger: config.logger ?? noopLogger,
	};
}

function normalizeUrl(url: string): string {
	const trimmed = url.trim();
	if (trimmed.length === 0) {
		throw new Error('KV client URL must not be empty');
	}
	return trimmed;
}
