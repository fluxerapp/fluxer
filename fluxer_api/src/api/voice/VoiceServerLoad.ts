// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IGatewayService} from '../infrastructure/IGatewayService';
import {Logger} from '../Logger';

const DEFAULT_REFRESH_INTERVAL_MS = 15000;
const STALE_REFRESH_INTERVALS = 4;
const EMPTY_CONNECTION_COUNTS: ReadonlyMap<string, number> = new Map();

export interface VoiceServerLoadSource {
	getConnectionCounts(): ReadonlyMap<string, number>;
}

export class VoiceServerLoadTracker implements VoiceServerLoadSource {
	private readonly gatewayService: IGatewayService;
	private readonly refreshIntervalMs: number;
	private readonly staleAfterMs: number;
	private readonly now: () => number;
	private connectionCounts: ReadonlyMap<string, number> = EMPTY_CONNECTION_COUNTS;
	private lastAttemptAt = 0;
	private lastSuccessAt = 0;
	private refreshing: Promise<void> | null = null;

	constructor(options: {
		gatewayService: IGatewayService;
		refreshIntervalMs?: number;
		now?: () => number;
	}) {
		this.gatewayService = options.gatewayService;
		this.refreshIntervalMs = options.refreshIntervalMs ?? DEFAULT_REFRESH_INTERVAL_MS;
		this.staleAfterMs = this.refreshIntervalMs * STALE_REFRESH_INTERVALS;
		this.now = options.now ?? Date.now;
	}

	getConnectionCounts(): ReadonlyMap<string, number> {
		const now = this.now();
		if (now - this.lastAttemptAt >= this.refreshIntervalMs) {
			void this.refresh();
		}
		if (this.lastSuccessAt === 0 || now - this.lastSuccessAt > this.staleAfterMs) {
			return EMPTY_CONNECTION_COUNTS;
		}
		return this.connectionCounts;
	}

	async refresh(): Promise<void> {
		if (this.refreshing) {
			return this.refreshing;
		}
		this.lastAttemptAt = this.now();
		this.refreshing = this.gatewayService
			.getVoiceStateCounts()
			.then((counts) => {
				const nextCounts = new Map<string, number>();
				for (const server of counts.servers) {
					nextCounts.set(server.server_id, server.voice_state_count);
				}
				this.connectionCounts = nextCounts;
				this.lastSuccessAt = this.now();
			})
			.catch((error) => {
				Logger.warn({error}, 'Failed to refresh voice server connection counts');
			})
			.finally(() => {
				this.refreshing = null;
			});
		return this.refreshing;
	}
}
