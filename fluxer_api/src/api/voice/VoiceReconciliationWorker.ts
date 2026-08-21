// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {BadGatewayError} from '@fluxer/errors/src/domains/core/BadGatewayError';
import {GatewayTimeoutError} from '@fluxer/errors/src/domains/core/GatewayTimeoutError';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {ChannelID, GuildID, UserID} from '../BrandedTypes';
import {createUserID} from '../BrandedTypes';
import type {ILogger} from '../ILogger';
import type {GatewayVoiceStateEntry, IGatewayService} from '../infrastructure/IGatewayService';
import type {ILiveKitService, LiveKitRoomLocation} from '../infrastructure/ILiveKitService';
import type {IVoiceRoomStore} from '../infrastructure/IVoiceRoomStore';
import {parseParticipantIdentity, parseRoomName} from '../infrastructure/VoiceRoomContext';
import {type VoicePresenceHeartbeatState, VoicePresenceHeartbeatStore} from './VoicePresenceHeartbeatStore';

interface GatewayPendingJoinEntry {
	readonly connectionId: string;
	readonly userId: string;
	readonly tokenNonce: string;
	readonly expiresAt: number;
}

interface VoiceReconciliationWorkerOptions {
	gatewayService: IGatewayService;
	liveKitService: ILiveKitService;
	voiceRoomStore: IVoiceRoomStore;
	kvClient: IKVProvider;
	logger: ILogger;
	voicePresenceHeartbeatStore?: VoicePresenceHeartbeatStore;
	intervalMs?: number;
	staggerDelayMs?: number;
	lockTtlSeconds?: number;
	cadenceTtlSeconds?: number;
	gatewayOnlyGraceMs?: number;
	liveKitOnlyGraceMs?: number;
}

interface DiscoveredRoom {
	readonly roomName: string;
	readonly guildId?: GuildID;
	readonly channelId: ChannelID;
	readonly fromGateway: boolean;
	readonly fromLiveKit: boolean;
	readonly gatewayVoiceStateCount: number;
}

interface RoomDiscovery {
	readonly rooms: Array<DiscoveredRoom>;
	readonly liveKitLocationsByRoom: Map<string, Array<LiveKitRoomLocation>>;
	readonly liveKitDiscoveryComplete: boolean;
	readonly liveKitDiscoveryErrors: number;
	readonly liveKitServersSearched: number;
	readonly gatewayRoomsDiscovered: number;
	readonly liveKitRoomsDiscovered: number;
	readonly gatewayDiscoveryFailed: boolean;
}

interface LiveKitParticipantEntry {
	readonly identity: string;
	readonly userId: UserID;
	readonly connectionId: string;
	readonly regionId: string;
	readonly serverId: string;
}

interface ResolvedLiveKitLocation extends LiveKitRoomLocation {
	readonly authoritativeForGatewayState: boolean;
}

interface LiveKitRoomSnapshot {
	readonly participants: Array<LiveKitParticipantEntry>;
	readonly completed: boolean;
	readonly gatewayStateRemovalComplete: boolean;
	readonly errors: number;
	readonly searchedLocations: number;
}

interface RoomReconciliationResult {
	readonly roomName: string;
	readonly livekitOnlyConfirmed: number;
	readonly livekitOnlyRepaired: number;
	readonly livekitOnlyDisconnected: number;
	readonly livekitOnlyDeferred: number;
	readonly gatewayOnlyRemoved: number;
	readonly gatewayOnlyDeferred: number;
	readonly gatewayOnlySkipped: number;
	readonly consistent: number;
	readonly transientSkip?: boolean;
}

type LiveKitOnlyRepairResult = 'repaired' | 'not_repairable' | 'defer';

const DEFAULT_INTERVAL_MS = 15000;
const DEFAULT_STAGGER_DELAY_MS = 25;
const DEFAULT_LOCK_TTL_SECONDS = 180;
const DEFAULT_GATEWAY_ONLY_GRACE_MS = 10000;
const DEFAULT_LIVEKIT_ONLY_GRACE_MS = 60000;
const MIN_CANDIDATE_TTL_SECONDS = 300;
const MAX_CANDIDATE_TTL_SECONDS = 3600;
const CANDIDATE_TTL_SWEEP_MULTIPLIER = 3;
const LAST_SWEEP_KEY_TTL_SECONDS = 86400;
const ROOM_KEY_PREFIX = 'voice:room:server:';
export function candidateTtlSecondsFor(input: {
	intervalMs: number;
	observedSweepSpacingMs: number;
	graceMs: number;
}): number {
	const spacingMs = Math.max(input.intervalMs, input.observedSweepSpacingMs);
	const ttlSeconds = Math.ceil((spacingMs * CANDIDATE_TTL_SWEEP_MULTIPLIER + input.graceMs * 2) / 1000);
	return Math.min(MAX_CANDIDATE_TTL_SECONDS, Math.max(MIN_CANDIDATE_TTL_SECONDS, ttlSeconds));
}

const VOICE_RECONCILIATION_LOCK_KEY = 'voice:reconcile:lock';
const VOICE_RECONCILIATION_CADENCE_KEY = 'voice:reconcile:cadence';
const VOICE_RECONCILIATION_LAST_SWEEP_KEY = 'voice:reconcile:last-sweep-at';
const GATEWAY_ONLY_CANDIDATE_KEY_PREFIX = 'voice:reconcile:gateway-only:';
const LIVEKIT_ONLY_CANDIDATE_KEY_PREFIX = 'voice:reconcile:livekit-only:';

export class VoiceReconciliationWorker {
	private readonly gatewayService: IGatewayService;
	private readonly liveKitService: ILiveKitService;
	private readonly voiceRoomStore: IVoiceRoomStore;
	private readonly kvClient: IKVProvider;
	private readonly voicePresenceHeartbeatStore: VoicePresenceHeartbeatStore;
	private readonly logger: ILogger;
	private readonly intervalMs: number;
	private readonly staggerDelayMs: number;
	private readonly lockTtlSeconds: number;
	private readonly cadenceTtlSeconds: number;
	private readonly gatewayOnlyGraceMs: number;
	private readonly liveKitOnlyGraceMs: number;
	private observedSweepSpacingMs = 0;
	private intervalHandle: NodeJS.Timeout | null = null;
	private reconciling = false;
	private reconciliationLockLost = false;

	constructor(options: VoiceReconciliationWorkerOptions) {
		this.gatewayService = options.gatewayService;
		this.liveKitService = options.liveKitService;
		this.voiceRoomStore = options.voiceRoomStore;
		this.kvClient = options.kvClient;
		this.voicePresenceHeartbeatStore =
			options.voicePresenceHeartbeatStore ?? new VoicePresenceHeartbeatStore(this.kvClient);
		this.logger = options.logger.child({worker: 'VoiceReconciliationWorker'});
		this.intervalMs = options.intervalMs ?? DEFAULT_INTERVAL_MS;
		this.staggerDelayMs = options.staggerDelayMs ?? DEFAULT_STAGGER_DELAY_MS;
		this.lockTtlSeconds =
			options.lockTtlSeconds ?? Math.max(DEFAULT_LOCK_TTL_SECONDS, Math.ceil((this.intervalMs * 3) / 1000));
		this.cadenceTtlSeconds = options.cadenceTtlSeconds ?? Math.max(1, Math.ceil((this.intervalMs * 3) / 1000));
		this.gatewayOnlyGraceMs = options.gatewayOnlyGraceMs ?? DEFAULT_GATEWAY_ONLY_GRACE_MS;
		this.liveKitOnlyGraceMs = options.liveKitOnlyGraceMs ?? DEFAULT_LIVEKIT_ONLY_GRACE_MS;
	}

	start(): void {
		if (this.intervalHandle) {
			this.logger.warn('VoiceReconciliationWorker is already running');
			return;
		}
		this.logger.info(
			{
				intervalMs: this.intervalMs,
				gatewayOnlyGraceMs: this.gatewayOnlyGraceMs,
				liveKitOnlyGraceMs: this.liveKitOnlyGraceMs,
				gatewayOnlyCandidateTtlSeconds: this.candidateTtlSeconds(this.gatewayOnlyGraceMs),
			},
			'Starting VoiceReconciliationWorker',
		);
		void this.runReconciliation();
		this.intervalHandle = setInterval(() => {
			void this.runReconciliation();
		}, this.intervalMs);
	}

	stop(): void {
		if (this.intervalHandle) {
			clearInterval(this.intervalHandle);
			this.intervalHandle = null;
			this.logger.info('Stopped VoiceReconciliationWorker');
		}
	}

	async reconcile(): Promise<void> {
		const startTime = Date.now();
		await this.recordSweepSpacing(startTime);
		const discovery = await this.discoverActiveRooms();
		this.logger.info(
			{
				roomCount: discovery.rooms.length,
				gatewayRoomsDiscovered: discovery.gatewayRoomsDiscovered,
				liveKitRoomsDiscovered: discovery.liveKitRoomsDiscovered,
				liveKitDiscoveryComplete: discovery.liveKitDiscoveryComplete,
				liveKitDiscoveryErrors: discovery.liveKitDiscoveryErrors,
				liveKitServersSearched: discovery.liveKitServersSearched,
				gatewayDiscoveryFailed: discovery.gatewayDiscoveryFailed,
			},
			'Starting reconciliation sweep',
		);
		let roomsChecked = 0;
		let totalConfirmed = 0;
		let totalRepaired = 0;
		let totalDisconnected = 0;
		let totalLiveKitOnlyDeferred = 0;
		let totalGatewayRemoved = 0;
		let totalGatewayDeferred = 0;
		let totalGatewaySkipped = 0;
		let totalConsistent = 0;
		let totalErrors = 0;
		let totalTransientSkips = 0;
		for (const room of discovery.rooms) {
			if (this.reconciliationLockLost) {
				this.logger.warn('Stopping reconciliation sweep because the cluster lock was lost');
				break;
			}
			try {
				const result = await this.reconcileRoom(
					room,
					discovery.liveKitLocationsByRoom.get(room.roomName) ?? [],
					discovery.liveKitDiscoveryComplete,
				);
				roomsChecked++;
				totalConfirmed += result.livekitOnlyConfirmed;
				totalRepaired += result.livekitOnlyRepaired;
				totalDisconnected += result.livekitOnlyDisconnected;
				totalLiveKitOnlyDeferred += result.livekitOnlyDeferred;
				totalGatewayRemoved += result.gatewayOnlyRemoved;
				totalGatewayDeferred += result.gatewayOnlyDeferred;
				totalGatewaySkipped += result.gatewayOnlySkipped;
				totalConsistent += result.consistent;
				if (result.transientSkip) {
					totalTransientSkips++;
				}
			} catch (error) {
				totalErrors++;
				this.logger.error({error, roomName: room.roomName}, 'Unexpected reconciliation failure; skipping room');
			}
			if (this.staggerDelayMs > 0) {
				await new Promise((resolve) => setTimeout(resolve, this.staggerDelayMs));
			}
		}
		const durationMs = Date.now() - startTime;
		this.logger.info(
			{
				observedSweepSpacingMs: this.observedSweepSpacingMs,
				gatewayOnlyCandidateTtlSeconds: this.candidateTtlSeconds(this.gatewayOnlyGraceMs),
				roomsChecked,
				totalConfirmed,
				totalRepaired,
				totalDisconnected,
				totalLiveKitOnlyDeferred,
				totalGatewayRemoved,
				totalGatewayDeferred,
				totalGatewaySkipped,
				totalConsistent,
				totalErrors,
				totalTransientSkips,
				durationMs,
			},
			'Reconciliation sweep complete',
		);
	}

	private async runReconciliation(): Promise<void> {
		if (this.reconciling) {
			this.logger.warn('Skipping reconciliation sweep; previous sweep still in progress');
			return;
		}
		this.reconciling = true;
		let lockToken: string | null = null;
		let lockRenewalHandle: NodeJS.Timeout | null = null;
		try {
			lockToken = await this.acquireReconciliationLock();
			if (lockToken === null) {
				this.logger.debug('Skipping reconciliation sweep; lock held by another worker');
				return;
			}
			this.reconciliationLockLost = false;
			lockRenewalHandle = this.startReconciliationLockRenewal(lockToken);
			const shouldRun = await this.acquireCadenceLease();
			if (!shouldRun) {
				this.logger.debug('Skipping reconciliation sweep; cadence lease held by another worker');
				return;
			}
			await this.reconcile();
		} catch (error) {
			this.logger.error({error}, 'Reconciliation sweep failed unexpectedly');
		} finally {
			if (lockRenewalHandle !== null) {
				clearInterval(lockRenewalHandle);
			}
			if (lockToken !== null) {
				await this.releaseReconciliationLock(lockToken);
			}
			this.reconciliationLockLost = false;
			this.reconciling = false;
		}
	}

	private async acquireReconciliationLock(): Promise<string | null> {
		const token = randomUUID();
		try {
			const acquired = await this.kvClient.acquireLock(VOICE_RECONCILIATION_LOCK_KEY, token, this.lockTtlSeconds);
			if (!acquired) {
				return null;
			}
			return token;
		} catch (error) {
			this.logger.error({error}, 'Failed to acquire voice reconciliation lock');
			return null;
		}
	}

	private candidateTtlSeconds(graceMs: number): number {
		return candidateTtlSecondsFor({
			intervalMs: this.intervalMs,
			observedSweepSpacingMs: this.observedSweepSpacingMs,
			graceMs,
		});
	}

	private async recordSweepSpacing(startedAt: number): Promise<void> {
		try {
			const previous = await this.kvClient.get(VOICE_RECONCILIATION_LAST_SWEEP_KEY);
			const previousAt = previous === null ? Number.NaN : Number(previous);
			if (Number.isFinite(previousAt) && startedAt > previousAt) {
				this.observedSweepSpacingMs = Math.max(this.observedSweepSpacingMs, startedAt - previousAt);
				const gatewayOnlyCandidateTtlSeconds = this.candidateTtlSeconds(this.gatewayOnlyGraceMs);
				if (this.observedSweepSpacingMs >= gatewayOnlyCandidateTtlSeconds * 1000) {
					this.logger.warn(
						{
							observedSweepSpacingMs: this.observedSweepSpacingMs,
							gatewayOnlyCandidateTtlSeconds,
						},
						'Reconciliation sweeps are further apart than the candidate TTL; divergent voice states will be deferred forever',
					);
				}
			}
			await this.kvClient.setex(VOICE_RECONCILIATION_LAST_SWEEP_KEY, LAST_SWEEP_KEY_TTL_SECONDS, String(startedAt));
		} catch (error) {
			this.logger.warn({error}, 'Failed to record reconciliation sweep spacing');
		}
	}

	private async acquireCadenceLease(): Promise<boolean> {
		try {
			return await this.kvClient.setnx(VOICE_RECONCILIATION_CADENCE_KEY, '1', this.cadenceTtlSeconds);
		} catch (error) {
			this.logger.error({error}, 'Failed to acquire voice reconciliation cadence lease');
			return false;
		}
	}

	private startReconciliationLockRenewal(token: string): NodeJS.Timeout {
		const intervalMs = Math.max(1000, Math.floor((this.lockTtlSeconds * 1000) / 3));
		return setInterval(() => {
			void this.renewReconciliationLock(token);
		}, intervalMs);
	}

	private async renewReconciliationLock(token: string): Promise<void> {
		try {
			const renewed = await this.kvClient.extendLock(VOICE_RECONCILIATION_LOCK_KEY, token, this.lockTtlSeconds);
			if (!renewed) {
				this.reconciliationLockLost = true;
				this.logger.warn('Voice reconciliation lock token no longer matched on renewal');
			}
		} catch (error) {
			this.logger.error({error}, 'Failed to renew voice reconciliation lock');
		}
	}

	private async releaseReconciliationLock(token: string): Promise<void> {
		try {
			const released = await this.kvClient.releaseLock(VOICE_RECONCILIATION_LOCK_KEY, token);
			if (!released) {
				this.logger.warn('Voice reconciliation lock token no longer matched on release');
			}
		} catch (error) {
			this.logger.error({error}, 'Failed to release voice reconciliation lock');
		}
	}

	private async discoverActiveRooms(): Promise<RoomDiscovery> {
		const roomsByName = new Map<string, DiscoveredRoom>();
		const liveKitLocationsByRoom = new Map<string, Array<LiveKitRoomLocation>>();
		let gatewayRoomsDiscovered = 0;
		let liveKitRoomsDiscovered = 0;
		let gatewayDiscoveryFailed = false;
		try {
			const gatewayRooms = await this.gatewayService.getActiveVoiceRooms();
			for (const room of gatewayRooms.rooms) {
				if (room.voiceStateCount <= 0) {
					continue;
				}
				this.addDiscoveredRoom(roomsByName, {
					roomName: buildRoomName(room.guildId, room.channelId),
					guildId: room.guildId,
					channelId: room.channelId,
					fromGateway: true,
					fromLiveKit: false,
					gatewayVoiceStateCount: room.voiceStateCount,
				});
				gatewayRoomsDiscovered++;
			}
		} catch (error) {
			gatewayDiscoveryFailed = true;
			this.logger.warn(
				{error: errorMessage(error)},
				'Failed to discover active voice rooms from gateway; using compatibility fallback',
			);
			await this.discoverPinnedRoomsFallback(roomsByName);
		}
		let liveKitDiscoveryComplete = false;
		let liveKitDiscoveryErrors = 0;
		let liveKitServersSearched = 0;
		try {
			const liveKitRooms = await this.liveKitService.listActiveRooms();
			liveKitDiscoveryComplete = liveKitRooms.completed;
			liveKitDiscoveryErrors = liveKitRooms.errors.length;
			liveKitServersSearched = liveKitRooms.searchedServers;
			for (const location of liveKitRooms.rooms) {
				const parsed = parseRoomName(location.roomName);
				if (!parsed) {
					this.logger.warn({roomName: location.roomName}, 'Skipping LiveKit room with unrecognized voice room name');
					continue;
				}
				const guildId = parsed.type === 'guild' ? parsed.guildId : undefined;
				this.addDiscoveredRoom(roomsByName, {
					roomName: location.roomName,
					guildId,
					channelId: parsed.channelId,
					fromGateway: false,
					fromLiveKit: true,
					gatewayVoiceStateCount: 0,
				});
				const locations = liveKitLocationsByRoom.get(location.roomName) ?? [];
				if (!locations.some((existing) => sameLiveKitLocation(existing, location))) {
					locations.push(location);
					liveKitLocationsByRoom.set(location.roomName, locations);
				}
				liveKitRoomsDiscovered++;
			}
		} catch (error) {
			this.logger.warn({error: errorMessage(error)}, 'Failed to list active LiveKit rooms');
		}
		return {
			rooms: Array.from(roomsByName.values()).sort((left, right) => left.roomName.localeCompare(right.roomName)),
			liveKitLocationsByRoom,
			liveKitDiscoveryComplete,
			liveKitDiscoveryErrors,
			liveKitServersSearched,
			gatewayRoomsDiscovered,
			liveKitRoomsDiscovered,
			gatewayDiscoveryFailed,
		};
	}

	private addDiscoveredRoom(roomsByName: Map<string, DiscoveredRoom>, room: DiscoveredRoom): void {
		const existing = roomsByName.get(room.roomName);
		if (!existing) {
			roomsByName.set(room.roomName, room);
			return;
		}
		roomsByName.set(room.roomName, {
			...existing,
			fromGateway: existing.fromGateway || room.fromGateway,
			fromLiveKit: existing.fromLiveKit || room.fromLiveKit,
			gatewayVoiceStateCount: existing.gatewayVoiceStateCount + room.gatewayVoiceStateCount,
		});
	}

	private async discoverPinnedRoomsFallback(roomsByName: Map<string, DiscoveredRoom>): Promise<void> {
		try {
			const rooms = await this.voiceRoomStore.listPinnedRooms();
			if (rooms.length > 0) {
				for (const room of rooms) {
					this.addDiscoveredRoom(roomsByName, {
						roomName: buildRoomName(room.guildId, room.channelId),
						guildId: room.guildId,
						channelId: room.channelId,
						fromGateway: false,
						fromLiveKit: false,
						gatewayVoiceStateCount: 0,
					});
				}
				return;
			}
		} catch (error) {
			this.logger.warn({error}, 'Failed to discover voice rooms from voice room store; falling back to KV scan');
		}
		const keys = await this.kvClient.scan(`${ROOM_KEY_PREFIX}*`, 1000);
		for (const key of keys) {
			const suffix = key.slice(ROOM_KEY_PREFIX.length);
			const parsed = parsePinnedRoomKey(suffix);
			if (!parsed) {
				continue;
			}
			this.addDiscoveredRoom(roomsByName, {
				roomName: buildRoomName(parsed.guildId, parsed.channelId),
				guildId: parsed.guildId,
				channelId: parsed.channelId,
				fromGateway: false,
				fromLiveKit: false,
				gatewayVoiceStateCount: 0,
			});
		}
	}

	private async reconcileRoom(
		room: DiscoveredRoom,
		discoveredLiveKitLocations: Array<LiveKitRoomLocation>,
		liveKitDiscoveryComplete: boolean,
	): Promise<RoomReconciliationResult> {
		let voiceStates: Array<GatewayVoiceStateEntry>;
		let pendingJoins: Array<GatewayPendingJoinEntry>;
		try {
			const [voiceStatesResult, pendingJoinsResult] = await Promise.all([
				this.gatewayService.getVoiceStatesForChannel({guildId: room.guildId, channelId: room.channelId}),
				this.gatewayService.getPendingJoinsForChannel({guildId: room.guildId, channelId: room.channelId}),
			]);
			voiceStates = voiceStatesResult.voiceStates;
			pendingJoins = pendingJoinsResult.pendingJoins;
		} catch (error) {
			if (VoiceReconciliationWorker.isTransientGatewayError(error)) {
				this.logger.warn(
					{roomName: room.roomName, error: errorMessage(error)},
					'Skipping room this sweep — gateway RPC temporarily unavailable',
				);
				return emptyRoomResult(room.roomName, {transientSkip: true});
			}
			throw error;
		}
		const liveKitLocations = await this.resolveLiveKitLocations(
			room,
			discoveredLiveKitLocations,
			voiceStates,
			liveKitDiscoveryComplete,
		);
		const liveKitSnapshot = await this.getLiveKitRoomSnapshot(room, liveKitLocations, liveKitDiscoveryComplete);
		const liveKitConnectionIds = new Set(liveKitSnapshot.participants.map((participant) => participant.connectionId));
		const gatewayConnectionIds = new Set(voiceStates.map((voiceState) => voiceState.connectionId));
		const pendingJoinByConnectionId = new Map<string, GatewayPendingJoinEntry>();
		for (const pendingJoin of pendingJoins) {
			pendingJoinByConnectionId.set(pendingJoin.connectionId, pendingJoin);
		}
		let livekitOnlyConfirmed = 0;
		let livekitOnlyRepaired = 0;
		let livekitOnlyDisconnected = 0;
		let livekitOnlyDeferred = 0;
		for (const participant of liveKitSnapshot.participants) {
			await this.clearGatewayOnlyCandidate(room.guildId, room.channelId, participant.connectionId);
			if (gatewayConnectionIds.has(participant.connectionId)) {
				await this.clearLiveKitOnlyCandidate(room.guildId, room.channelId, participant);
				continue;
			}
			const pendingJoin = pendingJoinByConnectionId.get(participant.connectionId);
			if (pendingJoin && pendingJoin.expiresAt > Date.now()) {
				await this.confirmPendingJoin(
					room.guildId,
					room.channelId,
					participant.connectionId,
					pendingJoin,
					room.roomName,
				);
				await this.clearLiveKitOnlyCandidate(room.guildId, room.channelId, participant);
				livekitOnlyConfirmed++;
			} else {
				const repairResult = await this.repairLiveKitOnlyParticipant(room, participant);
				if (repairResult === 'repaired') {
					await this.clearLiveKitOnlyCandidate(room.guildId, room.channelId, participant);
					livekitOnlyRepaired++;
					continue;
				}
				if (repairResult === 'defer') {
					livekitOnlyDeferred++;
					continue;
				}
				const heartbeatState = await this.getVoicePresenceHeartbeatState(room.channelId, participant);
				if (heartbeatState === 'active') {
					await this.clearLiveKitOnlyCandidate(room.guildId, room.channelId, participant);
					this.logger.debug(
						{
							roomName: room.roomName,
							userId: participant.userId.toString(),
							connectionId: participant.connectionId,
						},
						'Deferring LiveKit-only participant because v2 voice presence heartbeat is active',
					);
					livekitOnlyDeferred++;
					continue;
				}
				if (heartbeatState === 'expired') {
					await this.clearLiveKitOnlyCandidate(room.guildId, room.channelId, participant);
					this.logger.warn(
						{
							roomName: room.roomName,
							userId: participant.userId.toString(),
							connectionId: participant.connectionId,
						},
						'Disconnecting LiveKit-only participant because v2 voice presence heartbeat expired',
					);
					await this.disconnectLiveKitOnlyParticipant(room, participant);
					livekitOnlyDisconnected++;
					continue;
				}
				if (!liveKitSnapshot.completed) {
					this.logger.warn(
						{
							roomName: room.roomName,
							userId: participant.userId.toString(),
							connectionId: participant.connectionId,
							regionId: participant.regionId,
							serverId: participant.serverId,
						},
						'Deferring LiveKit-only participant because LiveKit snapshot was incomplete',
					);
					livekitOnlyDeferred++;
					continue;
				}
				const shouldDisconnect = await this.confirmLiveKitOnlyCandidate(room, participant);
				if (!shouldDisconnect) {
					this.logger.warn(
						{
							roomName: room.roomName,
							userId: participant.userId.toString(),
							connectionId: participant.connectionId,
							regionId: participant.regionId,
							serverId: participant.serverId,
						},
						'Deferring LiveKit-only participant; gateway state may be temporarily incomplete',
					);
					livekitOnlyDeferred++;
					continue;
				}
				await this.disconnectLiveKitOnlyParticipant(room, participant);
				livekitOnlyDisconnected++;
			}
		}
		let consistent = 0;
		let gatewayOnlyRemoved = 0;
		let gatewayOnlyDeferred = 0;
		let gatewayOnlySkipped = 0;
		for (const voiceState of voiceStates) {
			if (liveKitConnectionIds.has(voiceState.connectionId)) {
				consistent++;
				continue;
			}
			if (!liveKitSnapshot.gatewayStateRemovalComplete) {
				gatewayOnlySkipped++;
				continue;
			}
			const shouldRemove = await this.shouldRemoveGatewayOnlyState(room, voiceState);
			if (!shouldRemove) {
				gatewayOnlyDeferred++;
				continue;
			}
			await this.removeGhostState(room.guildId, voiceState, room.channelId, room.roomName);
			gatewayOnlyRemoved++;
		}
		if (
			livekitOnlyConfirmed > 0 ||
			livekitOnlyRepaired > 0 ||
			livekitOnlyDisconnected > 0 ||
			livekitOnlyDeferred > 0 ||
			gatewayOnlyRemoved > 0 ||
			gatewayOnlyDeferred > 0 ||
			gatewayOnlySkipped > 0
		) {
			this.logger.info(
				{
					roomName: room.roomName,
					livekitOnlyConfirmed,
					livekitOnlyRepaired,
					livekitOnlyDisconnected,
					livekitOnlyDeferred,
					gatewayOnlyRemoved,
					gatewayOnlyDeferred,
					gatewayOnlySkipped,
					consistent,
					liveKitLocations: liveKitSnapshot.searchedLocations,
					liveKitErrors: liveKitSnapshot.errors,
					gatewayStateRemovalComplete: liveKitSnapshot.gatewayStateRemovalComplete,
					fromGateway: room.fromGateway,
					fromLiveKit: room.fromLiveKit,
				},
				'Room reconciliation found divergence',
			);
		}
		return {
			roomName: room.roomName,
			livekitOnlyConfirmed,
			livekitOnlyRepaired,
			livekitOnlyDisconnected,
			livekitOnlyDeferred,
			gatewayOnlyRemoved,
			gatewayOnlyDeferred,
			gatewayOnlySkipped,
			consistent,
		};
	}

	private async resolveLiveKitLocations(
		room: DiscoveredRoom,
		discoveredLocations: Array<LiveKitRoomLocation>,
		voiceStates: Array<GatewayVoiceStateEntry>,
		liveKitDiscoveryComplete: boolean,
	): Promise<Array<ResolvedLiveKitLocation>> {
		const locations: Array<ResolvedLiveKitLocation> = discoveredLocations.map((location) => ({
			...location,
			authoritativeForGatewayState: false,
		}));
		if (liveKitDiscoveryComplete) {
			return locations;
		}
		for (const voiceState of voiceStates) {
			if (voiceState.regionId === undefined || voiceState.serverId === undefined) {
				continue;
			}
			if (this.liveKitService.getServer(voiceState.regionId, voiceState.serverId) === null) {
				continue;
			}
			addResolvedLiveKitLocation(locations, {
				roomName: room.roomName,
				regionId: voiceState.regionId,
				serverId: voiceState.serverId,
				authoritativeForGatewayState: true,
			});
		}
		try {
			const pinned = await this.voiceRoomStore.getPinnedRoomServer(room.guildId, room.channelId);
			if (pinned && this.liveKitService.getServer(pinned.regionId, pinned.serverId) !== null) {
				addResolvedLiveKitLocation(locations, {
					roomName: room.roomName,
					regionId: pinned.regionId,
					serverId: pinned.serverId,
					authoritativeForGatewayState: true,
				});
			}
		} catch (error) {
			this.logger.warn({error, roomName: room.roomName}, 'Failed to read pinned room server as fallback hint');
		}
		return locations;
	}

	private async getLiveKitRoomSnapshot(
		room: DiscoveredRoom,
		locations: Array<ResolvedLiveKitLocation>,
		liveKitDiscoveryComplete: boolean,
	): Promise<LiveKitRoomSnapshot> {
		const participants: Array<LiveKitParticipantEntry> = [];
		let errors = 0;
		for (const location of locations) {
			const listResult = await this.liveKitService.listParticipants({
				guildId: room.guildId,
				channelId: room.channelId,
				regionId: location.regionId,
				serverId: location.serverId,
			});
			if (listResult.status === 'error') {
				errors++;
				this.logger.warn(
					{
						roomName: room.roomName,
						regionId: location.regionId,
						serverId: location.serverId,
						errorCode: listResult.errorCode,
						retryable: listResult.retryable,
					},
					'Skipping LiveKit location this sweep — listParticipants failed',
				);
				continue;
			}
			for (const participant of listResult.participants) {
				const parsed = parseParticipantIdentity(participant.identity);
				if (!parsed) {
					this.logger.warn(
						{roomName: room.roomName, identity: participant.identity},
						'Could not parse participant identity; skipping',
					);
					continue;
				}
				participants.push({
					identity: participant.identity,
					userId: parsed.userId,
					connectionId: parsed.connectionId,
					regionId: location.regionId,
					serverId: location.serverId,
				});
			}
		}
		const searchedAuthoritativeGatewayLocation = locations.some((location) => location.authoritativeForGatewayState);
		const noRoomSpecificErrors = errors === 0;
		return {
			participants,
			completed: liveKitDiscoveryComplete && noRoomSpecificErrors,
			gatewayStateRemovalComplete:
				noRoomSpecificErrors && (liveKitDiscoveryComplete || searchedAuthoritativeGatewayLocation),
			errors,
			searchedLocations: locations.length,
		};
	}

	private async confirmPendingJoin(
		guildId: GuildID | undefined,
		channelId: ChannelID,
		connectionId: string,
		pendingJoin: GatewayPendingJoinEntry,
		roomName: string,
	): Promise<void> {
		try {
			const result = await this.gatewayService.confirmVoiceConnection({
				guildId,
				channelId,
				connectionId,
				tokenNonce: pendingJoin.tokenNonce,
			});
			if (!result.success) {
				this.logger.warn(
					{roomName, connectionId, error: result.error},
					'Gateway rejected voice connection confirmation',
				);
			}
		} catch (error) {
			if (VoiceReconciliationWorker.isTransientGatewayError(error)) {
				this.logger.warn(
					{roomName, connectionId, error: errorMessage(error)},
					'Deferring pending-join confirmation — gateway temporarily unavailable',
				);
				return;
			}
			this.logger.error({error, roomName, connectionId}, 'Failed to confirm pending voice connection');
		}
	}

	private async repairLiveKitOnlyParticipant(
		room: DiscoveredRoom,
		participant: LiveKitParticipantEntry,
	): Promise<LiveKitOnlyRepairResult> {
		if (room.guildId === undefined) {
			return 'not_repairable';
		}
		try {
			const result = await this.gatewayService.repairVoiceStateFromCache({
				guildId: room.guildId,
				channelId: room.channelId,
				userId: participant.userId,
				connectionId: participant.connectionId,
			});
			if (result.success) {
				this.logger.info(
					{
						roomName: room.roomName,
						userId: participant.userId.toString(),
						connectionId: participant.connectionId,
						repaired: result.repaired ?? false,
					},
					'Repaired LiveKit-only participant from gateway cache',
				);
				return 'repaired';
			}
			this.logger.debug(
				{
					roomName: room.roomName,
					userId: participant.userId.toString(),
					connectionId: participant.connectionId,
					error: result.error,
				},
				'LiveKit-only participant could not be repaired from gateway cache',
			);
			if (VoiceReconciliationWorker.isDefinitiveVoiceRepairMiss(result.error)) {
				return 'not_repairable';
			}
			return 'defer';
		} catch (error) {
			if (VoiceReconciliationWorker.isTransientGatewayError(error)) {
				this.logger.warn(
					{
						roomName: room.roomName,
						userId: participant.userId.toString(),
						connectionId: participant.connectionId,
						error: errorMessage(error),
					},
					'Deferring LiveKit-only repair because gateway is temporarily unavailable',
				);
				return 'defer';
			}
			this.logger.error(
				{error, roomName: room.roomName, userId: participant.userId.toString(), connectionId: participant.connectionId},
				'Failed to repair LiveKit-only participant from gateway cache',
			);
			return 'defer';
		}
	}

	private async disconnectLiveKitOnlyParticipant(
		room: DiscoveredRoom,
		participant: LiveKitParticipantEntry,
	): Promise<void> {
		this.logger.warn(
			{
				roomName: room.roomName,
				userId: participant.userId.toString(),
				connectionId: participant.connectionId,
				regionId: participant.regionId,
				serverId: participant.serverId,
			},
			'Disconnecting confirmed orphan LiveKit participant',
		);
		await this.liveKitService.disconnectParticipant({
			guildId: room.guildId,
			channelId: room.channelId,
			userId: participant.userId,
			connectionId: participant.connectionId,
			regionId: participant.regionId,
			serverId: participant.serverId,
		});
	}

	private async removeGhostState(
		guildId: GuildID | undefined,
		voiceState: GatewayVoiceStateEntry,
		channelId: ChannelID,
		roomName: string,
	): Promise<void> {
		try {
			this.logger.info(
				{roomName, userId: voiceState.userId, connectionId: voiceState.connectionId},
				'Removing confirmed ghost voice state from gateway',
			);
			const result = await this.gatewayService.disconnectVoiceUserIfInChannel({
				guildId,
				channelId,
				userId: createUserID(BigInt(voiceState.userId)),
				connectionId: voiceState.connectionId,
			});
			if (result.ignored) {
				this.logger.debug(
					{roomName, userId: voiceState.userId, connectionId: voiceState.connectionId},
					'Gateway ignored ghost state removal (user may have moved)',
				);
			}
		} catch (error) {
			if (VoiceReconciliationWorker.isTransientGatewayError(error)) {
				this.logger.warn(
					{roomName, userId: voiceState.userId, connectionId: voiceState.connectionId, error: errorMessage(error)},
					'Deferring ghost state removal — gateway temporarily unavailable',
				);
				return;
			}
			this.logger.error(
				{error, roomName, userId: voiceState.userId, connectionId: voiceState.connectionId},
				'Failed to remove ghost voice state from gateway',
			);
		}
	}

	private async shouldRemoveGatewayOnlyState(
		room: DiscoveredRoom,
		voiceState: GatewayVoiceStateEntry,
	): Promise<boolean> {
		let userId: UserID;
		try {
			userId = createUserID(BigInt(voiceState.userId));
		} catch (error) {
			this.logger.warn(
				{error, roomName: room.roomName, userId: voiceState.userId, connectionId: voiceState.connectionId},
				'Falling back to legacy gateway-only reconciliation because voice state user id was invalid',
			);
			return this.confirmGatewayOnlyCandidate(room.guildId, room.channelId, voiceState);
		}
		const heartbeatState = await this.getVoicePresenceHeartbeatState(room.channelId, {
			userId,
			connectionId: voiceState.connectionId,
		});
		if (heartbeatState === 'active') {
			await this.clearGatewayOnlyCandidate(room.guildId, room.channelId, voiceState.connectionId);
			this.logger.debug(
				{roomName: room.roomName, userId: voiceState.userId, connectionId: voiceState.connectionId},
				'Deferring gateway-only state because v2 voice presence heartbeat is active',
			);
			return false;
		}
		if (heartbeatState === 'expired') {
			await this.clearGatewayOnlyCandidate(room.guildId, room.channelId, voiceState.connectionId);
			this.logger.warn(
				{roomName: room.roomName, userId: voiceState.userId, connectionId: voiceState.connectionId},
				'Removing gateway-only state because v2 voice presence heartbeat expired',
			);
			return true;
		}
		return this.confirmGatewayOnlyCandidate(room.guildId, room.channelId, voiceState);
	}

	private async getVoicePresenceHeartbeatState(
		channelId: ChannelID,
		connection: {userId: UserID; connectionId: string},
	): Promise<VoicePresenceHeartbeatState> {
		try {
			return await this.voicePresenceHeartbeatStore.getHeartbeatState({
				channelId,
				userId: connection.userId,
				connectionId: connection.connectionId,
			});
		} catch (error) {
			this.logger.warn(
				{
					error,
					channelId: channelId.toString(),
					userId: connection.userId.toString(),
					connectionId: connection.connectionId,
				},
				'Falling back to legacy reconciliation because v2 voice presence heartbeat lookup failed',
			);
			return 'legacy';
		}
	}

	private async confirmGatewayOnlyCandidate(
		guildId: GuildID | undefined,
		channelId: ChannelID,
		voiceState: GatewayVoiceStateEntry,
	): Promise<boolean> {
		if (this.gatewayOnlyGraceMs <= 0) {
			return true;
		}
		const key = gatewayOnlyCandidateKey(guildId, channelId, voiceState.connectionId);
		const now = Date.now();
		try {
			const existing = await this.kvClient.get(key);
			const firstSeen = existing === null ? Number.NaN : Number(existing);
			if (Number.isFinite(firstSeen) && now - firstSeen >= this.gatewayOnlyGraceMs) {
				await this.kvClient.del(key);
				return true;
			}
			await this.kvClient.setex(
				key,
				this.candidateTtlSeconds(this.gatewayOnlyGraceMs),
				Number.isFinite(firstSeen) ? String(firstSeen) : String(now),
			);
			return false;
		} catch (error) {
			this.logger.warn(
				{error, guildId, channelId, connectionId: voiceState.connectionId},
				'Deferring gateway-only removal because candidate state could not be recorded',
			);
			return false;
		}
	}

	private async confirmLiveKitOnlyCandidate(
		room: DiscoveredRoom,
		participant: LiveKitParticipantEntry,
	): Promise<boolean> {
		if (this.liveKitOnlyGraceMs <= 0) {
			return true;
		}
		const key = liveKitOnlyCandidateKey(room.guildId, room.channelId, participant);
		const now = Date.now();
		try {
			const existing = await this.kvClient.get(key);
			const firstSeen = existing === null ? Number.NaN : Number(existing);
			if (Number.isFinite(firstSeen) && now - firstSeen >= this.liveKitOnlyGraceMs) {
				await this.kvClient.del(key);
				return true;
			}
			await this.kvClient.setex(
				key,
				this.candidateTtlSeconds(this.liveKitOnlyGraceMs),
				Number.isFinite(firstSeen) ? String(firstSeen) : String(now),
			);
			return false;
		} catch (error) {
			this.logger.warn(
				{
					error,
					guildId: room.guildId,
					channelId: room.channelId,
					connectionId: participant.connectionId,
					regionId: participant.regionId,
					serverId: participant.serverId,
				},
				'Deferring LiveKit-only removal because candidate state could not be recorded',
			);
			return false;
		}
	}

	private async clearGatewayOnlyCandidate(
		guildId: GuildID | undefined,
		channelId: ChannelID,
		connectionId: string,
	): Promise<void> {
		try {
			await this.kvClient.del(gatewayOnlyCandidateKey(guildId, channelId, connectionId));
		} catch (error) {
			this.logger.debug({error, guildId, channelId, connectionId}, 'Failed to clear gateway-only candidate marker');
		}
	}

	private async clearLiveKitOnlyCandidate(
		guildId: GuildID | undefined,
		channelId: ChannelID,
		participant: LiveKitParticipantEntry,
	): Promise<void> {
		try {
			await this.kvClient.del(liveKitOnlyCandidateKey(guildId, channelId, participant));
		} catch (error) {
			this.logger.debug(
				{
					error,
					guildId,
					channelId,
					connectionId: participant.connectionId,
					regionId: participant.regionId,
					serverId: participant.serverId,
				},
				'Failed to clear LiveKit-only candidate marker',
			);
		}
	}

	private static isTransientGatewayError(error: unknown): boolean {
		return (
			error instanceof ServiceUnavailableError ||
			error instanceof GatewayTimeoutError ||
			error instanceof BadGatewayError
		);
	}

	private static isDefinitiveVoiceRepairMiss(error: string | undefined): boolean {
		return (
			error === undefined ||
			error === 'connection_not_found' ||
			error === 'voice_state_mismatch' ||
			error === 'voice_invalid_state'
		);
	}
}

function emptyRoomResult(roomName: string, extra: Partial<RoomReconciliationResult> = {}): RoomReconciliationResult {
	return {
		roomName,
		livekitOnlyConfirmed: 0,
		livekitOnlyRepaired: 0,
		livekitOnlyDisconnected: 0,
		livekitOnlyDeferred: 0,
		gatewayOnlyRemoved: 0,
		gatewayOnlyDeferred: 0,
		gatewayOnlySkipped: 0,
		consistent: 0,
		...extra,
	};
}

function parsePinnedRoomKey(suffix: string): {guildId?: GuildID; channelId: ChannelID} | null {
	if (suffix.startsWith('guild:')) {
		const parts = suffix.split(':');
		if (parts.length !== 3) {
			return null;
		}
		try {
			return {guildId: BigInt(parts[1]) as GuildID, channelId: BigInt(parts[2]) as ChannelID};
		} catch {
			return null;
		}
	}
	if (suffix.startsWith('dm:')) {
		try {
			return {channelId: BigInt(suffix.slice(3)) as ChannelID};
		} catch {
			return null;
		}
	}
	return null;
}

function buildRoomName(guildId: GuildID | undefined, channelId: ChannelID): string {
	if (guildId === undefined) {
		return `dm_channel_${channelId.toString()}`;
	}
	return `guild_${guildId.toString()}_channel_${channelId.toString()}`;
}

function sameLiveKitLocation(left: LiveKitRoomLocation, right: LiveKitRoomLocation): boolean {
	return left.regionId === right.regionId && left.serverId === right.serverId && left.roomName === right.roomName;
}

function addResolvedLiveKitLocation(
	locations: Array<ResolvedLiveKitLocation>,
	location: ResolvedLiveKitLocation,
): void {
	const index = locations.findIndex((existing) => sameLiveKitLocation(existing, location));
	if (index === -1) {
		locations.push(location);
		return;
	}
	if (location.authoritativeForGatewayState && !locations[index].authoritativeForGatewayState) {
		locations[index] = {...locations[index], authoritativeForGatewayState: true};
	}
}

function gatewayOnlyCandidateKey(guildId: GuildID | undefined, channelId: ChannelID, connectionId: string): string {
	const scope = guildId === undefined ? 'dm' : `guild:${guildId.toString()}`;
	return `${GATEWAY_ONLY_CANDIDATE_KEY_PREFIX}${scope}:channel:${channelId.toString()}:connection:${encodeURIComponent(
		connectionId,
	)}`;
}

function liveKitOnlyCandidateKey(
	guildId: GuildID | undefined,
	channelId: ChannelID,
	participant: LiveKitParticipantEntry,
): string {
	const scope = guildId === undefined ? 'dm' : `guild:${guildId.toString()}`;
	return `${LIVEKIT_ONLY_CANDIDATE_KEY_PREFIX}${scope}:channel:${channelId.toString()}:connection:${encodeURIComponent(
		participant.connectionId,
	)}:region:${encodeURIComponent(participant.regionId)}:server:${encodeURIComponent(participant.serverId)}`;
}

function errorMessage(error: unknown): string {
	return error instanceof Error ? error.message : String(error);
}
