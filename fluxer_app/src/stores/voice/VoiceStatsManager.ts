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

import type {ConnectionQuality, Room} from 'livekit-client';
import {makeAutoObservable, runInAction} from 'mobx';
import {Logger} from '~/lib/Logger';
import {voiceStatsDB} from '~/lib/VoiceStatsDB';

const logger = new Logger('VoiceStatsManager');

export interface VoiceStats {
	audioSendBitrate: number;
	audioRecvBitrate: number;
	videoSendBitrate: number;
	videoRecvBitrate: number;
	audioPacketLoss: number;
	videoPacketLoss: number;
	rtt: number;
	jitter: number;
	participantCount: number;
	duration: number;
}

export interface LatencyDataPoint {
	timestamp: number;
	latency: number;
}

type RoomWithEngine = Room & {
	engine?: {
		client?: {
			rtt?: number;
		};
		pcManager?: {
			publisher?: RTCPeerConnection;
			subscriber?: RTCPeerConnection;
		};
	};
};

interface RTCStatsReport {
	type: string;
	id: string;
	kind?: string;
	bytesSent?: number;
	bytesReceived?: number;
	packetsLost?: number;
	packetsReceived?: number;
	jitter?: number;
	state?: string;
	currentRoundTripTime?: number;
}

const LATENCY_UPDATE_INTERVAL_MS = 2000;
const STATS_UPDATE_INTERVAL_MS = 1000;
const MAX_LATENCY_HISTORY = 60;
const STATS_CLEANUP_INTERVAL_MS = 60000;
const MAX_STATS_AGE_MS = 300000;

const initialVoiceStats: VoiceStats = {
	audioSendBitrate: 0,
	audioRecvBitrate: 0,
	videoSendBitrate: 0,
	videoRecvBitrate: 0,
	audioPacketLoss: 0,
	videoPacketLoss: 0,
	rtt: 0,
	jitter: 0,
	participantCount: 0,
	duration: 0,
};

export class VoiceStatsManager {
	private room: Room | null = null;
	private latencyIntervalId: ReturnType<typeof setInterval> | null = null;
	private statsIntervalId: ReturnType<typeof setInterval> | null = null;
	private cleanupIntervalId: ReturnType<typeof setInterval> | null = null;

	currentLatency: number | null = null;
	averageLatency: number | null = null;
	latencyHistory: Array<LatencyDataPoint> = [];
	voiceStats: VoiceStats = initialVoiceStats;
	connectionStartTime: number | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get estimatedLatency(): number | null {
		if (!this.room) return null;
		const localParticipant = this.room.localParticipant;
		if (!localParticipant) return null;

		const quality: ConnectionQuality = localParticipant.connectionQuality;
		switch (quality) {
			case 'excellent':
				return 30;
			case 'good':
				return 60;
			case 'poor':
				return 120;
			default:
				return null;
		}
	}

	get displayLatency(): number | null {
		const measured = this.currentLatency;
		return measured !== null ? measured : this.estimatedLatency;
	}

	get duration(): number {
		return this.connectionStartTime ? Math.floor((Date.now() - this.connectionStartTime) / 1000) : 0;
	}

	setRoom(room: Room | null): void {
		this.room = room;
		if (room) {
			this.connectionStartTime = Date.now();
		} else {
			this.connectionStartTime = null;
		}
	}

	startLatencyTracking(): void {
		this.stopLatencyTracking();
		logger.debug('[VoiceStatsManager] Starting latency tracking');

		this.latencyIntervalId = setInterval(() => {
			const engineWrap = this.room as RoomWithEngine;

			if (!engineWrap?.engine?.client?.rtt) {
				logger.debug('[VoiceStatsManager] No RTT available', {
					hasEngine: !!engineWrap?.engine,
					hasClient: !!engineWrap?.engine?.client,
				});
				return;
			}

			const rtt = Math.round(engineWrap.engine.client.rtt);
			logger.debug('[VoiceStatsManager] RTT measured', {rtt});

			logger.debug('[VoiceStatsManager] Updating latency state', {rtt});
			const timestamp = Date.now();
			const newPoint: LatencyDataPoint = {timestamp, latency: rtt};

			runInAction(() => {
				this.latencyHistory.push(newPoint);

				if (this.latencyHistory.length > MAX_LATENCY_HISTORY) {
					this.latencyHistory.shift();
				}

				this.currentLatency = rtt;
				this.averageLatency = this.latencyHistory.length
					? Math.round(this.latencyHistory.reduce((a, b) => a + b.latency, 0) / this.latencyHistory.length)
					: null;
			});
		}, LATENCY_UPDATE_INTERVAL_MS);
	}

	stopLatencyTracking(): void {
		if (this.latencyIntervalId !== null) {
			clearInterval(this.latencyIntervalId);
			this.latencyIntervalId = null;
		}
	}

	startStatsTracking(): void {
		this.stopStatsTracking();
		logger.debug('[VoiceStatsManager] Starting stats tracking');

		this.statsIntervalId = setInterval(async () => {
			if (!this.room) return;

			try {
				const room = this.room as RoomWithEngine;
				const engine = room?.engine;
				if (!engine?.pcManager) return;

				const publisher = engine.pcManager.publisher;
				const subscriber = engine.pcManager.subscriber;
				if (!publisher && !subscriber) return;

				let audioSendBitrate = 0;
				let audioRecvBitrate = 0;
				let videoSendBitrate = 0;
				let videoRecvBitrate = 0;
				let audioPacketLoss = 0;
				let videoPacketLoss = 0;
				let rtt = 0;
				let jitter = 0;
				let audioRecvCount = 0;
				let videoRecvCount = 0;

				const now = Date.now();

				if (publisher) {
					const stats = await publisher.getStats();
					for (const report of stats.values()) {
						if (report.type === 'outbound-rtp') {
							const id = `pub-${report.id}`;
							const current = (report as RTCStatsReport).bytesSent || 0;
							const prev = await voiceStatsDB.get(id);
							if (prev) {
								const dt = (now - prev.timestamp) / 1000;
								const db = current - prev.bytes;
								const br = dt > 0 ? (db * 8) / 1000 / dt : 0;
								const kind = (report as RTCStatsReport).kind;
								if (kind === 'audio') audioSendBitrate += br;
								else if (kind === 'video') videoSendBitrate += br;
							}
							await voiceStatsDB.set(id, current, now);
						} else if (report.type === 'candidate-pair' && (report as RTCStatsReport).state === 'succeeded') {
							const crt = (report as RTCStatsReport).currentRoundTripTime;
							if (crt) rtt = Math.max(rtt, crt * 1000);
						}
					}
				}

				if (subscriber) {
					const stats = await subscriber.getStats();
					for (const report of stats.values()) {
						if (report.type === 'inbound-rtp') {
							const id = `sub-${report.id}`;
							const current = (report as RTCStatsReport).bytesReceived || 0;
							const prev = await voiceStatsDB.get(id);
							if (prev) {
								const dt = (now - prev.timestamp) / 1000;
								const db = current - prev.bytes;
								const br = dt > 0 ? (db * 8) / 1000 / dt : 0;
								const kind = (report as RTCStatsReport).kind;
								if (kind === 'audio') {
									audioRecvBitrate += br;
									audioRecvCount++;
									const j = (report as RTCStatsReport).jitter;
									if (j) jitter += j * 1000;
								} else if (kind === 'video') {
									videoRecvBitrate += br;
									videoRecvCount++;
								}
							}
							await voiceStatsDB.set(id, current, now);

							const lost = (report as RTCStatsReport).packetsLost;
							const recvd = (report as RTCStatsReport).packetsReceived;
							if (lost && recvd) {
								const total = recvd + lost;
								const loss = lost / total;
								const kind = (report as RTCStatsReport).kind;
								if (kind === 'audio') audioPacketLoss += loss;
								else if (kind === 'video') videoPacketLoss += loss;
							}
						} else if (report.type === 'candidate-pair' && (report as RTCStatsReport).state === 'succeeded') {
							const crt = (report as RTCStatsReport).currentRoundTripTime;
							if (crt) rtt = Math.max(rtt, crt * 1000);
						}
					}
				}

				if (rtt === 0 && engine.client?.rtt) rtt = engine.client.rtt;

				if (audioRecvCount > 0) {
					audioPacketLoss = (audioPacketLoss / audioRecvCount) * 100;
					jitter = jitter / audioRecvCount;
				}
				if (videoRecvCount > 0) videoPacketLoss = (videoPacketLoss / videoRecvCount) * 100;

				const duration = this.connectionStartTime ? Math.floor((Date.now() - this.connectionStartTime) / 1000) : 0;

				const stats: VoiceStats = {
					audioSendBitrate: Math.round(audioSendBitrate),
					audioRecvBitrate: Math.round(audioRecvBitrate),
					videoSendBitrate: Math.round(videoSendBitrate),
					videoRecvBitrate: Math.round(videoRecvBitrate),
					audioPacketLoss: Math.round(audioPacketLoss * 10) / 10,
					videoPacketLoss: Math.round(videoPacketLoss * 10) / 10,
					rtt: Math.round(rtt),
					jitter: Math.round(jitter),
					participantCount: this.room?.numParticipants ?? 0,
					duration,
				};

				runInAction(() => {
					this.voiceStats = stats;
				});
			} catch (error) {
				logger.debug('[VoiceStatsManager] Error collecting stats', error);
			}
		}, STATS_UPDATE_INTERVAL_MS);

		this.cleanupIntervalId = setInterval(async () => {
			try {
				await voiceStatsDB.clearOldEntries(MAX_STATS_AGE_MS);
			} catch (error) {
				logger.debug('[VoiceStatsManager] Error cleaning up old entries', error);
			}
		}, STATS_CLEANUP_INTERVAL_MS);
	}

	stopStatsTracking(): void {
		if (this.statsIntervalId !== null) {
			clearInterval(this.statsIntervalId);
			this.statsIntervalId = null;
		}
		if (this.cleanupIntervalId !== null) {
			clearInterval(this.cleanupIntervalId);
			this.cleanupIntervalId = null;
		}
	}

	cleanup(): void {
		logger.debug('[VoiceStatsManager] Cleaning up');
		this.stopLatencyTracking();
		this.stopStatsTracking();

		this.room = null;
		this.currentLatency = null;
		this.averageLatency = null;
		this.latencyHistory = [];
		this.voiceStats = initialVoiceStats;
		this.connectionStartTime = null;
	}

	reset(): void {
		this.currentLatency = null;
		this.averageLatency = null;
		this.latencyHistory = [];
		this.voiceStats = initialVoiceStats;
	}
}
