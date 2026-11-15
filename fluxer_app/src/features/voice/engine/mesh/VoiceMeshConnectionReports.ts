// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('VoiceMeshConnectionReports');

const FLUSH_DELAY_MS = 10_000;
const MAX_REPORTS_PER_REQUEST = 8;
const CANDIDATE_TYPES = ['host', 'srflx', 'prflx', 'relay'] as const;
const PROTOCOLS = ['udp', 'tcp'] as const;
const IPV4_ADDRESS = /^\d{1,3}(?:\.\d{1,3}){3}$/u;

type VoiceMeshCandidateType = (typeof CANDIDATE_TYPES)[number];

export interface VoiceMeshConnectionPath {
	local_candidate_type: VoiceMeshCandidateType | null;
	remote_candidate_type: VoiceMeshCandidateType | null;
	ip_family: 'ipv4' | 'ipv6' | null;
	protocol: (typeof PROTOCOLS)[number] | null;
}

export interface VoiceMeshConnectionReport extends VoiceMeshConnectionPath {
	channel_id: string;
	guild_id: string | null;
	participant_count: number;
	outcome: 'connected' | 'failed';
	setup_ms: number | null;
	ice_restarted: boolean;
}

export const UNKNOWN_VOICE_MESH_CONNECTION_PATH: VoiceMeshConnectionPath = {
	local_candidate_type: null,
	remote_candidate_type: null,
	ip_family: null,
	protocol: null,
};

let pending: Array<VoiceMeshConnectionReport> = [];
let flushTimer: ReturnType<typeof setTimeout> | null = null;

function readEnum<T extends string>(values: ReadonlyArray<T>, value: unknown): T | null {
	return values.find((candidate) => candidate === value) ?? null;
}

function readIpFamily(address: unknown): VoiceMeshConnectionPath['ip_family'] {
	if (typeof address !== 'string') return null;
	if (IPV4_ADDRESS.test(address)) return 'ipv4';
	return address.includes(':') ? 'ipv6' : null;
}

export function readVoiceMeshConnectionPath(report: RTCStatsReport): VoiceMeshConnectionPath {
	const stats = new Map<string, Record<string, unknown>>();
	report.forEach((stat: Record<string, unknown>) => stats.set(String(stat.id), stat));
	const entries = Array.from(stats.values());
	const transport = entries.find(
		(stat) => stat.type === 'transport' && typeof stat.selectedCandidatePairId === 'string',
	);
	const pair = transport
		? stats.get(String(transport.selectedCandidatePairId))
		: entries.find((stat) => stat.type === 'candidate-pair' && stat.nominated === true && stat.state === 'succeeded');
	if (!pair) return UNKNOWN_VOICE_MESH_CONNECTION_PATH;
	const local = stats.get(String(pair.localCandidateId));
	const remote = stats.get(String(pair.remoteCandidateId));
	return {
		local_candidate_type: readEnum(CANDIDATE_TYPES, local?.candidateType),
		remote_candidate_type: readEnum(CANDIDATE_TYPES, remote?.candidateType),
		ip_family: readIpFamily(local?.address) ?? readIpFamily(remote?.address),
		protocol: readEnum(PROTOCOLS, local?.protocol),
	};
}

export function queueVoiceMeshConnectionReport(report: VoiceMeshConnectionReport): void {
	pending.push(report);
	if (pending.length >= MAX_REPORTS_PER_REQUEST) {
		flushVoiceMeshConnectionReports();
		return;
	}
	flushTimer ??= setTimeout(flushVoiceMeshConnectionReports, FLUSH_DELAY_MS);
}

export function flushVoiceMeshConnectionReports(): void {
	if (flushTimer) clearTimeout(flushTimer);
	flushTimer = null;
	const reports = pending;
	pending = [];
	if (reports.length === 0) return;
	http.post(Endpoints.VOICE_P2P_CONNECTION_REPORTS, {body: {reports}}).catch((error: unknown) => {
		logger.debug('Dropped P2P connection reports', {count: reports.length, error});
	});
}
