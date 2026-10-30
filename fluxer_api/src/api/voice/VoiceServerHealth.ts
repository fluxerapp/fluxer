// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import type {VoiceServerRecord} from '@app/api/voice/VoiceModel';
import {
	probeLiveKit,
	resolveRoomServiceEndpoint,
	usesPublicEndpoint,
	VoiceEndpointError,
} from '@app/api/voice/VoiceServerEndpoints';
import type {VoiceTopology} from '@app/api/voice/VoiceTopology';

export type VoiceServerHealthStatus = 'up' | 'down' | 'unknown';

export interface VoiceServerHealthState {
	status: VoiceServerHealthStatus;
	checkedAt: Date | null;
	error: string | null;
}

export interface VoiceServerHealthSource {
	get(server: Pick<VoiceServerRecord, 'regionId' | 'serverId'>): VoiceServerHealthState;
}

const FAILURES_BEFORE_DOWN = 2;
const PROBE_TIMEOUT_MS = 3000;
const UNKNOWN: VoiceServerHealthState = {status: 'unknown', checkedAt: null, error: null};

type Probe = (server: VoiceServerRecord) => Promise<void>;

const probeVoiceServer: Probe = (server) =>
	probeLiveKit(resolveRoomServiceEndpoint(server), {
		checkAddress: usesPublicEndpoint(server),
		allowPrivate: Config.voice.joinAllowPrivateEndpoints,
		timeoutMs: PROBE_TIMEOUT_MS,
	});

interface Entry extends VoiceServerHealthState {
	failures: number;
}

function key(server: Pick<VoiceServerRecord, 'regionId' | 'serverId'>): string {
	return `${server.regionId}\u0000${server.serverId}`;
}

export class VoiceServerHealthMonitor implements VoiceServerHealthSource {
	private entries = new Map<string, Entry>();
	private timer: NodeJS.Timeout | null = null;
	private running = false;

	constructor(
		private readonly topology: Pick<VoiceTopology, 'getAllRegions' | 'getServersForRegion'>,
		private readonly probe: Probe = probeVoiceServer,
		private readonly intervalMs: number = Config.voice.healthIntervalMs,
	) {}

	get(server: Pick<VoiceServerRecord, 'regionId' | 'serverId'>): VoiceServerHealthState {
		const entry = this.entries.get(key(server));
		return entry ? {status: entry.status, checkedAt: entry.checkedAt, error: entry.error} : UNKNOWN;
	}

	start(): void {
		if (this.timer || this.intervalMs <= 0) return;
		this.timer = setInterval(() => void this.checkAll(), this.intervalMs);
		this.timer.unref();
		void this.checkAll();
	}

	stop(): void {
		if (this.timer) clearInterval(this.timer);
		this.timer = null;
	}

	async checkAll(): Promise<void> {
		if (this.running) return;
		this.running = true;
		try {
			const servers = this.topology
				.getAllRegions()
				.flatMap((region) => this.topology.getServersForRegion(region.id))
				.filter((server) => server.isActive);
			const live = new Set(servers.map(key));
			for (const k of this.entries.keys()) {
				if (!live.has(k)) this.entries.delete(k);
			}
			await Promise.all(servers.map((server) => this.check(server)));
		} finally {
			this.running = false;
		}
	}

	private async check(server: VoiceServerRecord): Promise<void> {
		const k = key(server);
		const previous = this.entries.get(k) ?? {...UNKNOWN, failures: 0};
		const checkedAt = new Date();
		try {
			await this.probe(server);
			this.entries.set(k, {status: 'up', checkedAt, error: null, failures: 0});
		} catch (error) {
			const message = error instanceof Error ? error.message : 'probe failed';
			if (error instanceof VoiceEndpointError && error.reason === 'private') {
				this.entries.set(k, {status: 'unknown', checkedAt, error: message, failures: 0});
				return;
			}
			const failures = previous.failures + 1;
			const status = failures >= FAILURES_BEFORE_DOWN ? 'down' : previous.status;
			if (status === 'down' && previous.status !== 'down') {
				Logger.warn({regionId: server.regionId, serverId: server.serverId, error: message}, 'Voice server is down');
			}
			this.entries.set(k, {status, checkedAt, error: message, failures});
		}
	}
}

export function toHealthResponse(state: VoiceServerHealthState): {
	status: VoiceServerHealthStatus;
	checked_at: string | null;
	error: string | null;
} {
	return {status: state.status, checked_at: state.checkedAt?.toISOString() ?? null, error: state.error};
}

export function healthOf(
	source: VoiceServerHealthSource | null,
	server: Pick<VoiceServerRecord, 'regionId' | 'serverId'>,
): VoiceServerHealthState {
	return source?.get(server) ?? UNKNOWN;
}
