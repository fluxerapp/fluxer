// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {isElectronPlatform} from '@app/features/platform/types/Platform';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as VoicePresenceHeartbeatCommands from '@app/features/voice/commands/VoicePresenceHeartbeatCommands';
import {Store} from '@app/features/voice/engine/Store';
import {sendVoiceStateDisconnect} from '@app/features/voice/engine/VoiceChannelConnector';
import {
	createVoiceConnectionSnapshot,
	getVoiceConnectionFailedTarget,
	getVoiceConnectionFailureReason,
	isLatestVoiceConnectionAttempt,
	isVoiceConnectionFailed,
	selectVoiceConnectionServerUpdateDecision,
	transitionVoiceConnectionSnapshot,
	type VoiceConnectionEvent,
	type VoiceConnectionFailureReason,
	type VoiceConnectionLocalDisconnectReason,
	type VoiceConnectionSnapshot,
} from '@app/features/voice/engine/VoiceConnectionStateMachine';
import {VoiceConnectionThrottle} from '@app/features/voice/engine/VoiceConnectionThrottle';
import {createE2EEKeyProvider, createE2EEWorker} from '@app/features/voice/engine/VoiceE2EEKeyProvider';
import {getSharedVoiceAudioContext} from '@app/features/voice/engine/VoiceSharedAudioContext';
import {selectLocalMediaPublicationsForConnectionRepublish} from '@app/features/voice/engine/VoiceTrackPublicationUtils';
import {
	assertDisconnectReason,
	assertNonEmptyString,
	assertObjectLike,
	assertOptionalNonEmptyString,
	assertVoiceServerUpdateShape,
	hasAnyTerminalTransport,
	isPresenceConnectionReady,
	isReadyToRepublishTrack,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppAdapterAssertions';
import {VoiceEngineV2AppReconnectPolicy} from '@app/features/voice/engine/v2/VoiceEngineV2AppReconnectPolicy';
import VoiceRegionTeleport from '@app/features/voice/state/VoiceRegionTeleport';
import {
	getVideoDecoderExclusionsSync,
	loadVideoDecoderExclusions,
} from '@app/features/voice/utils/VideoDecoderCapabilities';
import type {
	ExternalE2EEKeyProvider,
	LocalTrack,
	Room,
	RoomConnectOptions,
	RoomOptions,
	TrackPublishOptions,
} from 'livekit-client';
import {Room as LiveKitRoom, RoomEvent, Track} from 'livekit-client';
import {makeObservable, observable} from 'mobx';
import type {Subscription} from 'rxjs';
import {timer} from 'rxjs';

const logger = new Logger('VoiceEngineV2AppConnectionHostAdapter');
const VOICE_SERVER_TIMEOUT_MS = 5000;
const VIDEO_DECODER_EXCLUSION_TIMEOUT_MS = 500;
const VOICE_PRESENCE_HEARTBEAT_INTERVAL_MS = 15000;

export interface VoiceServerUpdateData {
	token: string;
	endpoint: string;
	connection_id: string;
	guild_id?: string;
	channel_id?: string;
	e2ee_key?: string | null;
}

export interface VoiceConnectionState {
	room: Room | null;
	guildId: string | null;
	channelId: string | null;
	connecting: boolean;
	connected: boolean;
	reconnecting: boolean;
	voiceServerEndpoint: string | null;
	connectionId: string | null;
}

export interface RegionHotSwapState {
	pendingRoom: Room | null;
	previousRoom: Room | null;
	inProgress: boolean;
}

export type HotSwapQueuedOperation = () => void | Promise<void>;
export type VoiceConnectFailureHandler = (
	guildId: string | null,
	channelId: string,
	connectionId: string | null,
	attemptId: number,
	error: unknown,
) => void | Promise<void>;

const initialConnectionState: VoiceConnectionState = {
	room: null,
	guildId: null,
	channelId: null,
	connecting: false,
	connected: false,
	reconnecting: false,
	voiceServerEndpoint: null,
	connectionId: null,
};
const initialHotSwapState: RegionHotSwapState = {
	pendingRoom: null,
	previousRoom: null,
	inProgress: false,
};
const REGION_HOT_SWAP_TIMEOUT_MS = 10000;

async function getRoomVideoDecoderExclusions(): Promise<RoomOptions['subscriberVideoCodecExclusions']> {
	const cached = getVideoDecoderExclusionsSync();
	if (cached) return cached.length > 0 ? cached : undefined;
	let timeoutId: NodeJS.Timeout | undefined;
	const timeout = new Promise<null>((resolve) => {
		timeoutId = setTimeout(() => resolve(null), VIDEO_DECODER_EXCLUSION_TIMEOUT_MS);
	});
	try {
		const exclusions = await Promise.race([loadVideoDecoderExclusions(), timeout]);
		if (exclusions && exclusions.length > 0) return exclusions;
		const latest = getVideoDecoderExclusionsSync();
		return latest && latest.length > 0 ? latest : undefined;
	} finally {
		if (timeoutId !== undefined) {
			clearTimeout(timeoutId);
		}
	}
}

function isSamePresenceConnection(
	activeSub: Subscription | null,
	activeConnection: {channelId: string; connectionId: string} | null,
	channelId: string,
	connectionId: string,
): boolean {
	if (!activeSub) return false;
	if (!activeConnection) return false;
	if (activeConnection.channelId !== channelId) return false;
	if (activeConnection.connectionId !== connectionId) return false;
	return true;
}

function createWebAudioMixOption(): RoomOptions['webAudioMix'] {
	const audioContext = getSharedVoiceAudioContext();
	if (audioContext) {
		assert.notEqual(audioContext.state, 'closed', 'shared voice AudioContext handed to LiveKit must not be closed');
		return {audioContext};
	}
	return true;
}

function createRoomOptions(
	e2eeKey: string | null,
	subscriberVideoCodecExclusions: RoomOptions['subscriberVideoCodecExclusions'],
): {
	roomOptions: RoomOptions;
	e2eeKeyProvider: ExternalE2EEKeyProvider | null;
} {
	const roomOptions: RoomOptions = {
		adaptiveStream: false,
		dynacast: true,
		webAudioMix: createWebAudioMixOption(),
		subscriberVideoCodecExclusions,
	};
	let e2eeKeyProvider: ExternalE2EEKeyProvider | null = null;
	if (e2eeKey) {
		try {
			e2eeKeyProvider = createE2EEKeyProvider();
			const worker = createE2EEWorker();
			roomOptions.e2ee = {keyProvider: e2eeKeyProvider, worker};
		} catch (error) {
			logger.error('Failed to construct E2EE key provider/worker', error);
			e2eeKeyProvider = null;
		}
	}
	return {roomOptions, e2eeKeyProvider};
}

function createRoomConnectOptions(): RoomConnectOptions {
	const connectOptions: RoomConnectOptions = {
		autoSubscribe: false,
	};
	assert.equal(connectOptions.autoSubscribe, false, 'LiveKit connect options must not auto-subscribe');
	if (isElectronPlatform()) {
		connectOptions.rtcConfig = {iceTransportPolicy: 'relay'};
		assert.equal(
			connectOptions.rtcConfig.iceTransportPolicy,
			'relay',
			'Electron LiveKit connects must force relay ICE',
		);
	}
	return connectOptions;
}

export class VoiceEngineV2AppConnectionHostAdapter extends Store {
	connectionState: VoiceConnectionState = initialConnectionState;
	hotSwapState: RegionHotSwapState = initialHotSwapState;
	private connectionSnapshot: VoiceConnectionSnapshot = createVoiceConnectionSnapshot();
	private throttle = new VoiceConnectionThrottle();
	private reconnect = new VoiceEngineV2AppReconnectPolicy();
	private voiceServerTimeoutSub: Subscription | null = null;
	private hotSwapTimeoutSub: Subscription | null = null;
	private voicePresenceHeartbeatSub: Subscription | null = null;
	private voicePresenceHeartbeatConnection: {channelId: string; connectionId: string} | null = null;
	private isLocalDisconnecting = false;
	private hotSwapOperationQueue: Array<HotSwapQueuedOperation> = [];

	constructor() {
		super();
		makeObservable(this, {
			connectionState: observable.ref,
			hotSwapState: observable.ref,
		});
		this.throttle.subscribe(() => this.emitChange());
		this.reconnect.subscribe(() => this.emitChange());
	}

	get room(): Room | null {
		return this.connectionState.room;
	}

	get guildId(): string | null {
		return this.connectionState.guildId;
	}

	get channelId(): string | null {
		return this.connectionState.channelId;
	}

	get connected(): boolean {
		return this.connectionState.connected;
	}

	get connecting(): boolean {
		return this.connectionState.connecting;
	}

	get reconnecting(): boolean {
		return this.connectionState.reconnecting;
	}

	get connectionId(): string | null {
		return this.connectionState.connectionId;
	}

	get voiceServerEndpoint(): string | null {
		return this.connectionState.voiceServerEndpoint;
	}

	get shouldAutoReconnect(): boolean {
		return this.reconnect.shouldAutoReconnect;
	}

	get reconnectAttempts(): number {
		return this.reconnect.reconnectAttempts;
	}

	get disconnecting(): boolean {
		return this.isLocalDisconnecting;
	}

	get localDisconnectReason(): VoiceConnectionLocalDisconnectReason {
		return this.connectionSnapshot.context.localDisconnectReason;
	}

	get connectFailed(): boolean {
		return isVoiceConnectionFailed(this.connectionSnapshot);
	}

	get connectFailureReason(): VoiceConnectionFailureReason {
		return getVoiceConnectionFailureReason(this.connectionSnapshot);
	}

	get connectFailedTarget(): {guildId: string | null; channelId: string} | null {
		return getVoiceConnectionFailedTarget(this.connectionSnapshot);
	}

	get regionHotSwapInProgress(): boolean {
		return this.hotSwapState.inProgress;
	}

	enqueueOrRun(operation: HotSwapQueuedOperation): void {
		assert.equal(typeof operation, 'function', 'enqueueOrRun.operation must be a function');
		assert.ok(this.hotSwapOperationQueue.length <= 4096, 'enqueueOrRun pre-condition: queue under cap');
		if (!this.hotSwapState.inProgress) {
			try {
				const result = operation();
				if (result && typeof (result as Promise<void>).catch === 'function') {
					void (result as Promise<void>).catch((error) => {
						logger.error('Immediate operation failed', {error});
					});
				}
			} catch (error) {
				logger.error('Immediate operation failed', {error});
			}
			return;
		}
		logger.debug('Queueing operation during hot-swap', {queueLength: this.hotSwapOperationQueue.length + 1});
		this.update(() => {
			this.transitionConnection({type: 'hotSwap.queueOperation'});
		});
		this.hotSwapOperationQueue.push(operation);
	}

	private async drainHotSwapQueue(): Promise<void> {
		const ops = this.hotSwapOperationQueue.splice(0);
		if (ops.length === 0) return;
		this.update(() => {
			this.transitionConnection({type: 'hotSwap.drainQueue'});
		});
		logger.info('Draining hot-swap operation queue', {count: ops.length});
		for (const op of ops) {
			try {
				await op();
			} catch (error) {
				logger.warn('Queued hot-swap operation failed during drain', {error});
			}
		}
	}

	private clearHotSwapQueue(): void {
		if (this.hotSwapOperationQueue.length > 0) {
			logger.info('Clearing hot-swap operation queue', {discarded: this.hotSwapOperationQueue.length});
		}
		this.hotSwapOperationQueue.length = 0;
		this.update(() => {
			this.transitionConnection({type: 'hotSwap.clearQueue'});
		});
	}

	private startVoicePresenceHeartbeatForCurrentConnection(): void {
		const {channelId, connectionId, connected} = this.connectionState;
		if (!isPresenceConnectionReady(connected, channelId, connectionId)) {
			this.stopVoicePresenceHeartbeat();
			return;
		}
		const presenceChannelId = channelId as string;
		const presenceConnectionId = connectionId as string;
		if (
			isSamePresenceConnection(
				this.voicePresenceHeartbeatSub,
				this.voicePresenceHeartbeatConnection,
				presenceChannelId,
				presenceConnectionId,
			)
		) {
			return;
		}
		this.stopVoicePresenceHeartbeat();
		this.voicePresenceHeartbeatConnection = {channelId: presenceChannelId, connectionId: presenceConnectionId};
		this.voicePresenceHeartbeatSub = timer(0, VOICE_PRESENCE_HEARTBEAT_INTERVAL_MS).subscribe(() => {
			void this.sendVoicePresenceHeartbeat(presenceChannelId, presenceConnectionId, {requireConnected: true});
		});
	}

	private stopVoicePresenceHeartbeat(options: {markEnded?: boolean} = {}): void {
		const connection = this.voicePresenceHeartbeatConnection;
		if (this.voicePresenceHeartbeatSub) {
			this.voicePresenceHeartbeatSub.unsubscribe();
			this.voicePresenceHeartbeatSub = null;
		}
		this.voicePresenceHeartbeatConnection = null;
		if (options.markEnded && connection) {
			void this.markVoicePresenceHeartbeatEnded(connection);
		}
	}

	private async sendVoicePresenceHeartbeat(
		channelId: string,
		connectionId: string,
		options: {requireConnected: boolean},
	): Promise<void> {
		const current = this.connectionState;
		if (
			current.channelId !== channelId ||
			current.connectionId !== connectionId ||
			(options.requireConnected && !current.connected)
		) {
			return;
		}
		try {
			await VoicePresenceHeartbeatCommands.heartbeat({channelId, connectionId});
		} catch (error) {
			logger.warn('Voice presence heartbeat failed', {channelId, connectionId, error});
		}
	}

	private async markVoicePresenceHeartbeatEnded(connection: {channelId: string; connectionId: string}): Promise<void> {
		try {
			await VoicePresenceHeartbeatCommands.end(connection);
		} catch (error) {
			logger.warn('Voice presence heartbeat end failed', {...connection, error});
		}
	}

	get lastConnectedChannel(): {
		guildId: string;
		channelId: string;
	} | null {
		return this.reconnect.lastConnectedChannel;
	}

	private transitionConnection(event: VoiceConnectionEvent): void {
		const wasTeleporting = this.hotSwapState.inProgress;
		this.connectionSnapshot = transitionVoiceConnectionSnapshot(this.connectionSnapshot, event);
		const {context} = this.connectionSnapshot;
		this.connectionState = {
			room: context.room as Room | null,
			guildId: context.guildId,
			channelId: context.channelId,
			connecting: context.connecting,
			connected: context.connected,
			reconnecting: context.reconnecting,
			voiceServerEndpoint: context.voiceServerEndpoint,
			connectionId: context.connectionId,
		};
		this.hotSwapState = {
			pendingRoom: context.hotSwap.pendingRoom as Room | null,
			previousRoom: context.hotSwap.previousRoom as Room | null,
			inProgress: context.hotSwap.inProgress,
		};
		const isTeleporting = this.hotSwapState.inProgress;
		if (isTeleporting && !wasTeleporting) {
			VoiceRegionTeleport.beginTeleport();
		}
		if (!isTeleporting && wasTeleporting) {
			VoiceRegionTeleport.endTeleport();
		}
	}

	private isLatestConnectionAttempt(attemptId: number): boolean {
		return (
			isLatestVoiceConnectionAttempt(this.connectionSnapshot, attemptId) && this.throttle.isLatestAttempt(attemptId)
		);
	}

	private syncThrottleToCurrentAttempt(): void {
		this.throttle.setLatestAttemptId(this.connectionSnapshot.context.connectionAttemptId);
	}

	private invalidateThrottleAttempt(): void {
		this.throttle.setLatestAttemptId(this.connectionSnapshot.context.connectionAttemptId + 1);
	}

	startConnection(guildId: string | null, channelId: string): boolean {
		assertOptionalNonEmptyString(guildId, 'startConnection.guildId');
		assertNonEmptyString(channelId, 'startConnection.channelId');
		if (this.throttle.shouldThrottle()) {
			logger.warn('Connection throttled');
			return false;
		}
		this.throttle.recordConnectRequest();
		this.update(() => {
			this.transitionConnection({type: 'connection.start', guildId, channelId});
		});
		this.syncThrottleToCurrentAttempt();
		this.throttle.setInFlightConnect(true);
		this.scheduleVoiceServerTimeout(guildId, channelId);
		logger.info('Connection started', {guildId, channelId});
		return true;
	}

	recoverConnectionExpectation(guildId: string | null, channelId: string): void {
		assertOptionalNonEmptyString(guildId, 'recoverConnectionExpectation.guildId');
		assertNonEmptyString(channelId, 'recoverConnectionExpectation.channelId');
		this.throttle.recordConnectRequest();
		this.update(() => {
			this.transitionConnection({type: 'connection.recoverExpectation', guildId, channelId});
		});
		this.syncThrottleToCurrentAttempt();
		this.throttle.setInFlightConnect(true);
		this.scheduleVoiceServerTimeout(guildId, channelId);
		logger.info('Connection expectation recovered', {guildId, channelId});
	}

	handleVoiceServerUpdate(
		raw: VoiceServerUpdateData,
		onRoomCreated: (room: Room, attemptId: number, guildId: string | null, channelId: string) => void,
		onBeforeReconnect?: (isChannelMove: boolean, previousRoom: Room) => boolean | undefined,
		onRoomClosed?: (room: Room, attemptId: number) => void,
		onHotSwapComplete?: (newRoom: Room, attemptId: number, guildId: string | null, channelId: string) => void,
		onConnectFailed?: VoiceConnectFailureHandler,
	): void {
		assertVoiceServerUpdateShape(raw, 'handleVoiceServerUpdate.raw');
		assert.equal(typeof onRoomCreated, 'function', 'handleVoiceServerUpdate.onRoomCreated must be a function');
		void this.handleVoiceServerUpdateAsync(
			raw,
			onRoomCreated,
			onBeforeReconnect,
			onRoomClosed,
			onHotSwapComplete,
			onConnectFailed,
		);
	}

	private async handleVoiceServerUpdateAsync(
		raw: VoiceServerUpdateData,
		onRoomCreated: (room: Room, attemptId: number, guildId: string | null, channelId: string) => void,
		onBeforeReconnect?: (isChannelMove: boolean, previousRoom: Room) => boolean | undefined,
		onRoomClosed?: (room: Room, attemptId: number) => void,
		onHotSwapComplete?: (newRoom: Room, attemptId: number, guildId: string | null, channelId: string) => void,
		onConnectFailed?: VoiceConnectFailureHandler,
	): Promise<void> {
		const decision = selectVoiceConnectionServerUpdateDecision(this.connectionSnapshot, raw);
		const {
			guildId: expectedGuildId,
			channelId: expectedChannelId,
			connected,
			room: existingRoom,
			voiceServerEndpoint: currentEndpoint,
		} = this.connectionState;
		const guildId = raw.guild_id ?? null;
		const endpoint = raw.endpoint ?? null;
		const token = raw.token ?? null;
		const connectionId = raw.connection_id ?? null;
		const incomingChannelId = raw.channel_id ?? null;
		const attemptId = decision.attemptId;
		logger.debug('handleVoiceServerUpdate called', {
			incomingGuildId: guildId,
			expectedGuildId,
			incomingChannelId,
			expectedChannelId,
			endpoint,
			hasToken: !!token,
			connectionId,
			attemptId,
		});
		if (decision.type === 'ignore' && decision.reason === 'guild-or-channel-mismatch') {
			logger.warn('Ignoring VOICE_SERVER_UPDATE: guild or channel mismatch', {
				expectedGuildId: decision.expectedGuildId,
				incomingGuildId: decision.incomingGuildId,
				expectedChannelId: decision.expectedChannelId,
				incomingChannelId: decision.incomingChannelId,
			});
			return;
		}
		if (decision.type === 'ignore' && decision.reason === 'stale-channel-update') {
			logger.warn('Ignoring VOICE_SERVER_UPDATE: stale channel update', {
				expectedChannelId: decision.expectedChannelId,
				incomingChannelId: decision.incomingChannelId,
				connectionId,
			});
			return;
		}
		if (decision.type === 'ignore' && decision.reason === 'stale-attempt') {
			logger.warn('Ignoring VOICE_SERVER_UPDATE: not latest attempt', {attemptId});
			return;
		}
		if (decision.type !== 'accept') return;
		const isChannelMove = decision.isChannelMove;
		if (isChannelMove) {
			if (connected) {
				logger.info('VOICE_SERVER_UPDATE: server-initiated channel move', {
					expectedChannelId,
					incomingChannelId,
					connectionId,
				});
			}
		}
		if (decision.isRegionChange) {
			logger.info('VOICE_SERVER_UPDATE: region change detected, attempting hot-swap', {
				previousEndpoint: currentEndpoint,
				newEndpoint: endpoint,
				guildId,
				channelId: decision.resolvedChannelId,
			});
			await this.handleRegionHotSwap(
				raw,
				decision.currentRoom as Room,
				attemptId,
				decision.guildId,
				decision.resolvedChannelId,
				onRoomCreated,
				onRoomClosed,
				onHotSwapComplete,
			);
			return;
		}
		const resolvedChannelId = decision.resolvedChannelId;
		this.clearVoiceServerTimeout();
		let previousRoom = connected && existingRoom ? existingRoom : null;
		let shouldStopPreviousRoomTracks = true;
		if (previousRoom) {
			shouldStopPreviousRoomTracks = isChannelMove || onBeforeReconnect?.(isChannelMove, previousRoom) !== true;
			previousRoom.removeAllListeners();
			if (isChannelMove) {
				this.disconnectPreviousRoom(previousRoom);
				previousRoom = null;
			}
		}
		this.update(() => {
			this.transitionConnection({
				type: 'voiceServer.accepted',
				guildId,
				channelId: resolvedChannelId,
				endpoint,
				connectionId,
				isChannelMove,
			});
		});
		this.throttle.setInFlightConnect(true);
		const e2eeKey = raw.e2ee_key ?? null;
		const subscriberVideoCodecExclusions = await getRoomVideoDecoderExclusions();
		if (
			!this.isLatestConnectionAttempt(attemptId) ||
			this.connectionState.guildId !== guildId ||
			this.connectionState.channelId !== resolvedChannelId
		) {
			logger.warn('Aborting LiveKit room creation after codec probing because attempt is stale', {attemptId});
			return;
		}
		const {roomOptions, e2eeKeyProvider} = createRoomOptions(e2eeKey, subscriberVideoCodecExclusions);
		const room = new LiveKitRoom(roomOptions);
		let roomClosed = false;
		const closeRoom = () => {
			if (roomClosed) return;
			roomClosed = true;
			onRoomClosed?.(room, attemptId);
		};
		const failConnectBeforeRoomConnect = (message: string, error?: unknown) => {
			logger.error(message, error);
			closeRoom();
			this.disconnectPreviousRoom(previousRoom);
			if (!this.isLatestConnectionAttempt(attemptId)) return;
			this.update(() => {
				this.transitionConnection({type: 'connection.failed', reason: 'error'});
			});
			this.stopVoicePresenceHeartbeat({markEnded: true});
			this.throttle.setInFlightConnect(false);
			this.reconnect.setReconnectState('error');
			void onConnectFailed?.(guildId, resolvedChannelId, connectionId, attemptId, error ?? new Error(message));
		};
		const connectRoom = () => {
			if (!this.isLatestConnectionAttempt(attemptId)) {
				closeRoom();
				return;
			}
			onRoomCreated(room, attemptId, guildId, resolvedChannelId);
			if (!endpoint || !token) {
				failConnectBeforeRoomConnect('Missing endpoint or token', {endpoint, hasToken: !!token});
				return;
			}
			logger.info('Attempting to connect to LiveKit', {endpoint, guildId, channelId: resolvedChannelId});
			const connectOptions = createRoomConnectOptions();
			room
				.connect(endpoint, token, connectOptions)
				.then(() => {
					this.disconnectPreviousRoom(previousRoom, shouldStopPreviousRoomTracks);
					logger.info('LiveKit connection succeeded');
					const connectionState = this.connectionState;
					const connectedEventAlreadyApplied = connectionState.connected && connectionState.room == null;
					if (
						!this.isLatestConnectionAttempt(attemptId) ||
						connectionState.guildId !== guildId ||
						connectionState.channelId !== resolvedChannelId ||
						(!connectionState.connecting && !connectedEventAlreadyApplied)
					) {
						logger.warn('Connection succeeded but attempt is stale, disconnecting');
						closeRoom();
						try {
							room.removeAllListeners();
							room.disconnect();
						} catch (error) {
							logger.warn('Failed to disconnect stale room', error);
						}
						this.stopVoicePresenceHeartbeat({markEnded: true});
						return;
					}
					logger.info('Initializing voice connection');
					this.update(() => {
						this.transitionConnection({type: 'connection.roomReady', room, attemptId});
					});
				})
				.catch((error) => {
					closeRoom();
					this.disconnectPreviousRoom(previousRoom);
					logger.error('LiveKit connection failed', {error, endpoint});
					if (this.isLatestConnectionAttempt(attemptId)) {
						this.update(() => {
							this.transitionConnection({type: 'connection.failed', reason: 'error'});
						});
						this.stopVoicePresenceHeartbeat({markEnded: true});
						this.throttle.setInFlightConnect(false);
						this.reconnect.setReconnectState('error');
						void onConnectFailed?.(guildId, resolvedChannelId, connectionId, attemptId, error);
					}
				});
		};
		if (e2eeKey) {
			if (!e2eeKeyProvider) {
				failConnectBeforeRoomConnect('Cannot join E2EE voice channel because E2EE setup failed');
				return;
			}
			void e2eeKeyProvider
				.setKey(e2eeKey)
				.then(() => room.setE2EEEnabled(true))
				.then(() => {
					connectRoom();
				})
				.catch((error) => {
					failConnectBeforeRoomConnect(
						'Cannot join E2EE voice channel because the E2EE key failed to initialize',
						error,
					);
				});
			return;
		}
		connectRoom();
	}

	private handleRegionHotSwap(
		raw: VoiceServerUpdateData,
		existingRoom: Room,
		attemptId: number,
		guildId: string | null,
		channelId: string,
		onRoomCreated: (room: Room, attemptId: number, guildId: string | null, channelId: string) => void,
		onRoomClosed?: (room: Room, attemptId: number) => void,
		onHotSwapComplete?: (newRoom: Room, attemptId: number, guildId: string | null, channelId: string) => void,
	): Promise<void> {
		return this.handleRegionHotSwapAsync(
			raw,
			existingRoom,
			attemptId,
			guildId,
			channelId,
			onRoomCreated,
			onRoomClosed,
			onHotSwapComplete,
		);
	}

	private async handleRegionHotSwapAsync(
		raw: VoiceServerUpdateData,
		existingRoom: Room,
		attemptId: number,
		guildId: string | null,
		channelId: string,
		onRoomCreated: (room: Room, attemptId: number, guildId: string | null, channelId: string) => void,
		onRoomClosed?: (room: Room, attemptId: number) => void,
		onHotSwapComplete?: (newRoom: Room, attemptId: number, guildId: string | null, channelId: string) => void,
	): Promise<void> {
		const endpoint = raw.endpoint!;
		const token = raw.token!;
		const connectionId = raw.connection_id ?? null;
		this.abortHotSwap();
		const cachedExclusions = getVideoDecoderExclusionsSync();
		const roomOptions: RoomOptions = {
			adaptiveStream: false,
			dynacast: true,
			webAudioMix: createWebAudioMixOption(),
			subscriberVideoCodecExclusions: cachedExclusions && cachedExclusions.length > 0 ? cachedExclusions : undefined,
		};
		if (!this.isLatestConnectionAttempt(attemptId) || this.connectionState.room !== existingRoom) {
			logger.warn('Region hot-swap: aborted before room creation because attempt is stale', {attemptId});
			return;
		}
		const newRoom = new LiveKitRoom(roomOptions);
		this.update(() => {
			this.transitionConnection({type: 'hotSwap.start', pendingRoom: newRoom, previousRoom: existingRoom});
		});
		this.clearHotSwapTimeout();
		this.hotSwapTimeoutSub = timer(REGION_HOT_SWAP_TIMEOUT_MS).subscribe(() => {
			if (this.hotSwapState.inProgress && this.hotSwapState.pendingRoom === newRoom) {
				logger.warn('Region hot-swap timed out, aborting', {endpoint});
				this.abortHotSwap();
			}
		});
		const connectOptions = createRoomConnectOptions();
		logger.info('Region hot-swap: connecting to new endpoint', {endpoint, guildId, channelId});
		newRoom
			.connect(endpoint, token, connectOptions)
			.then(async () => {
				if (
					!this.hotSwapState.inProgress ||
					this.hotSwapState.pendingRoom !== newRoom ||
					!this.isLatestConnectionAttempt(attemptId)
				) {
					logger.warn('Region hot-swap: new room connected but hot-swap was cancelled');
					try {
						newRoom.removeAllListeners();
						newRoom.disconnect(false);
					} catch (error) {
						logger.warn('Region hot-swap: cancelled cleanup disconnect failed', {error});
					}
					return;
				}
				logger.info('Region hot-swap: new room connected, republishing tracks');
				try {
					await this.republishLocalTracks(existingRoom, newRoom);
				} catch (error) {
					logger.warn('Region hot-swap: failed to republish tracks, aborting', {error});
					if (
						!this.hotSwapState.inProgress ||
						this.hotSwapState.pendingRoom !== newRoom ||
						!this.isLatestConnectionAttempt(attemptId)
					) {
						return;
					}
					this.abortHotSwap();
					return;
				}
				if (
					!this.hotSwapState.inProgress ||
					this.hotSwapState.pendingRoom !== newRoom ||
					!this.isLatestConnectionAttempt(attemptId)
				) {
					logger.warn('Region hot-swap: cancelled during track republishing');
					try {
						newRoom.removeAllListeners();
						newRoom.disconnect(false);
					} catch (error) {
						logger.warn('Region hot-swap: cancelled-during-republish disconnect failed', {error});
					}
					return;
				}
				logger.info('Region hot-swap: swapping room pointer');
				const previousRoom = existingRoom;
				onRoomCreated(newRoom, attemptId, guildId, channelId);
				previousRoom.removeAllListeners();
				this.update(() => {
					this.transitionConnection({type: 'hotSwap.complete', room: newRoom, endpoint, connectionId});
				});
				this.clearHotSwapTimeout();
				this.startVoicePresenceHeartbeatForCurrentConnection();
				onHotSwapComplete?.(newRoom, attemptId, guildId, channelId);
				await this.drainHotSwapQueue();
				try {
					previousRoom.disconnect(false);
				} catch (error) {
					logger.warn('Region hot-swap: failed to disconnect old room', {error});
				}
				logger.info('Region hot-swap: complete', {
					previousEndpoint: this.connectionState.voiceServerEndpoint,
					newEndpoint: endpoint,
				});
			})
			.catch((error) => {
				logger.error('Region hot-swap: failed to connect to new endpoint', {error, endpoint});
				onRoomClosed?.(newRoom, attemptId);
				try {
					newRoom.removeAllListeners();
					newRoom.disconnect();
				} catch (cleanupError) {
					logger.warn('Region hot-swap: failed disconnect after connect failure', {cleanupError});
				}
				if (
					!this.hotSwapState.inProgress ||
					this.hotSwapState.pendingRoom !== newRoom ||
					!this.isLatestConnectionAttempt(attemptId)
				) {
					return;
				}
				this.update(() => {
					this.transitionConnection({type: 'hotSwap.reset'});
				});
				this.clearHotSwapTimeout();
				this.clearHotSwapQueue();
				logger.info('Region hot-swap: keeping old room on previous endpoint');
			});
	}

	private async republishLocalTracks(oldRoom: Room, newRoom: Room): Promise<void> {
		const oldParticipant = oldRoom.localParticipant;
		const newParticipant = newRoom.localParticipant;
		if (!oldParticipant || !newParticipant) {
			logger.warn('Region hot-swap: missing participant for track republishing');
			return;
		}
		const publications = selectLocalMediaPublicationsForConnectionRepublish(
			Array.from(oldParticipant.trackPublications.values()),
		);
		const errors: Array<{source: string; error: unknown}> = [];
		for (const publication of publications) {
			const track = publication.track as LocalTrack | undefined;
			if (!isReadyToRepublishTrack(track)) {
				logger.debug('Region hot-swap: skipping ended or missing track', {
					source: publication.source,
					trackSid: publication.trackSid,
				});
				continue;
			}
			try {
				logger.debug('Region hot-swap: republishing track', {
					source: publication.source,
					kind: track.kind,
				});
				const publishOptions = {
					...((publication as {options?: TrackPublishOptions}).options ?? {}),
					source: publication.source,
					name: publication.trackName,
				};
				await newParticipant.publishTrack(track.mediaStreamTrack, publishOptions);
			} catch (error) {
				errors.push({source: publication.source ?? 'unknown', error});
				logger.warn('Region hot-swap: failed to republish track', {
					source: publication.source,
					error,
				});
			}
		}
		const screenShareFailure = errors.find(
			(error) => error.source === Track.Source.ScreenShare || error.source === Track.Source.ScreenShareAudio,
		);
		if (screenShareFailure) {
			throw new Error(`Screen share track republish failed for ${screenShareFailure.source}`);
		}
		if (errors.length > 0 && errors.length === publications.length) {
			throw new Error(`All ${errors.length} track republications failed`);
		}
		if (errors.length > 0) {
			logger.warn('Region hot-swap: some tracks failed to republish', {
				total: publications.length,
				failed: errors.length,
			});
		}
	}

	abortHotSwap(): void {
		assert.ok(this.hotSwapOperationQueue.length <= 4096, 'abortHotSwap pre-condition: queue under cap');
		assert.ok(this.connectionSnapshot !== null, 'abortHotSwap pre-condition: connection snapshot present');
		if (!this.hotSwapState.inProgress) return;
		const {pendingRoom} = this.hotSwapState;
		logger.info('Aborting region hot-swap');
		if (pendingRoom) {
			try {
				pendingRoom.removeAllListeners();
				pendingRoom.disconnect(false);
			} catch (error) {
				logger.warn('Failed to disconnect pending hot-swap room', {error});
			}
		}
		this.update(() => {
			this.transitionConnection({type: 'hotSwap.abort'});
		});
		this.clearHotSwapTimeout();
		this.clearHotSwapQueue();
	}

	markConnected(): void {
		assert.ok(this.connectionSnapshot !== null, 'markConnected pre-condition: connection snapshot present');
		const {guildId, channelId} = this.connectionState;
		assertNonEmptyString(channelId, 'markConnected pre-condition: channelId present');
		this.update(() => {
			this.transitionConnection({type: 'connection.connected'});
		});
		this.reconnect.setLastConnectedChannel(guildId, channelId);
		this.throttle.setInFlightConnect(false);
		this.reconnect.resetOnConnection();
		this.startVoicePresenceHeartbeatForCurrentConnection();
		assert.ok(this.connectionState.connected, 'markConnected post-condition: connection state reflects connected');
		logger.info('Connection established');
	}

	markDisconnected(reason: 'user' | 'error' | 'server' = 'user'): void {
		assertDisconnectReason(reason, 'markDisconnected.reason');
		this.update(() => {
			this.transitionConnection({type: 'connection.disconnected', reason});
		});
		this.invalidateThrottleAttempt();
		this.throttle.setInFlightConnect(false);
		this.reconnect.setReconnectState(reason);
		this.stopVoicePresenceHeartbeat({markEnded: true});
		logger.info('Connection terminated', {reason});
	}

	markReconnecting(): void {
		assert.ok(this.connectionSnapshot !== null, 'markReconnecting pre-condition: connection snapshot present');
		this.update(() => {
			this.transitionConnection({type: 'connection.reconnecting'});
		});
		logger.info('Connection reconnecting');
	}

	markReconnected(): void {
		assert.ok(this.connectionSnapshot !== null, 'markReconnected pre-condition: connection snapshot present');
		this.update(() => {
			this.transitionConnection({type: 'connection.reconnected'});
		});
		this.reconnect.resetOnConnection();
		this.startVoicePresenceHeartbeatForCurrentConnection();
		logger.info('Connection reconnected');
	}

	disconnectFromVoiceChannel(reason: 'user' | 'error' | 'server' = 'user'): void {
		assertDisconnectReason(reason, 'disconnectFromVoiceChannel.reason');
		const {room} = this.connectionState;
		this.update(() => {
			this.isLocalDisconnecting = reason === 'user';
		});
		this.clearVoiceServerTimeout();
		this.abortHotSwap();
		if (room) {
			room.removeAllListeners();
			room.disconnect();
		}
		this.update(() => {
			this.transitionConnection({type: 'connection.disconnected', reason});
		});
		this.invalidateThrottleAttempt();
		this.reconnect.setReconnectState(reason);
		this.stopVoicePresenceHeartbeat({markEnded: true});
		this.update(() => {
			this.isLocalDisconnecting = false;
		});
		logger.info('Disconnected from voice channel', {reason});
	}

	disconnectForChannelMove(): void {
		assert.ok(this.connectionSnapshot !== null, 'disconnectForChannelMove pre-condition: connection snapshot present');
		const {room} = this.connectionState;
		this.clearVoiceServerTimeout();
		this.abortHotSwap();
		if (room) {
			room.removeAllListeners();
			room.disconnect();
		}
		this.update(() => {
			this.transitionConnection({type: 'connection.disconnectForChannelMove'});
		});
		this.invalidateThrottleAttempt();
		this.stopVoicePresenceHeartbeat({markEnded: true});
		logger.info('Disconnected for channel move (preserving connectionId)');
	}

	private disconnectRoomForTerminalUnload(room: Room | null, label: string): void {
		if (!room) return;
		try {
			room.removeAllListeners();
			room.disconnect();
			logger.debug('Terminal unload LiveKit room disconnect requested', {label});
		} catch (error) {
			logger.warn('Terminal unload LiveKit room disconnect failed', {label, error});
		}
	}

	hasTerminalUnloadTransports(): boolean {
		assert.ok(this.connectionSnapshot !== null, 'hasTerminalUnloadTransports pre-condition: snapshot present');
		return hasAnyTerminalTransport({current: this.connectionState, hotSwap: this.hotSwapState});
	}

	disconnectTransportsForTerminalUnload(): void {
		assert.ok(
			this.connectionSnapshot !== null,
			'disconnectTransportsForTerminalUnload pre-condition: snapshot present',
		);
		const {room} = this.connectionState;
		const {pendingRoom, previousRoom} = this.hotSwapState;
		this.clearVoiceServerTimeout();
		this.clearHotSwapTimeout();
		this.clearHotSwapQueue();
		this.disconnectRoomForTerminalUnload(pendingRoom, 'pending-hot-swap');
		if (previousRoom && previousRoom !== room && previousRoom !== pendingRoom) {
			this.disconnectRoomForTerminalUnload(previousRoom, 'previous-hot-swap');
		}
		this.disconnectRoomForTerminalUnload(room, 'current');
		this.stopVoicePresenceHeartbeat({markEnded: true});
		this.throttle.setInFlightConnect(false);
		this.update(() => {
			this.isLocalDisconnecting = false;
			this.transitionConnection({type: 'connection.cleanup'});
		});
		this.invalidateThrottleAttempt();
	}

	scheduleReconnect(callback: () => void): boolean {
		assert.equal(typeof callback, 'function', 'scheduleReconnect.callback must be a function');
		return this.reconnect.scheduleReconnect(callback);
	}

	markReconnectionAttempted(): void {
		assert.ok(this.reconnect !== null, 'markReconnectionAttempted pre-condition: reconnect policy present');
		this.reconnect.markAttempted();
	}

	resetReconnectState(): void {
		assert.ok(this.reconnect !== null, 'resetReconnectState pre-condition: reconnect policy present');
		this.reconnect.reset();
	}

	forgetReconnectChannel(channelId: string): void {
		assertNonEmptyString(channelId, 'forgetReconnectChannel.channelId');
		this.reconnect.forgetChannel(channelId);
	}

	updateChannelId(channelId: string): void {
		assertNonEmptyString(channelId, 'updateChannelId.channelId');
		this.update(() => {
			this.transitionConnection({type: 'connection.updateChannel', channelId});
		});
		logger.info('Channel updated', {channelId});
	}

	acceptServerChannelChange(channelId: string): void {
		assertNonEmptyString(channelId, 'acceptServerChannelChange.channelId');
		const previousChannelId = this.connectionState.channelId;
		this.update(() => {
			this.transitionConnection({type: 'connection.acceptServerChannelChange', channelId});
		});
		this.reconnect.setLastConnectedChannel(this.connectionState.guildId, channelId);
		logger.info('Accepted server channel change', {previousChannelId, newChannelId: channelId});
	}

	createGuardedHandler<T extends ReadonlyArray<unknown>>(
		attemptId: number,
		handler: (...args: T) => void | Promise<void>,
	): (...args: T) => void {
		assert.equal(typeof attemptId, 'number', 'createGuardedHandler.attemptId must be a number');
		assert.ok(Number.isFinite(attemptId), 'createGuardedHandler.attemptId must be finite');
		assert.equal(typeof handler, 'function', 'createGuardedHandler.handler must be a function');
		return (...args: T) => {
			if (!this.isLatestConnectionAttempt(attemptId)) {
				return;
			}
			try {
				const result = handler(...args);
				if (result && typeof (result as Promise<void>).catch === 'function') {
					void (result as Promise<void>).catch((error) => {
						logger.error('Guarded voice handler failed', {attemptId, error});
					});
				}
			} catch (error) {
				logger.error('Guarded voice handler failed', {attemptId, error});
			}
		};
	}

	bindConnectionEvents(
		room: Room,
		attemptId: number,
		handlers: {
			onConnected: () => void;
			onDisconnected: (reason?: unknown) => void;
			onReconnecting: () => void;
			onReconnected: () => void;
		},
	): void {
		assertObjectLike<Room>(room, 'bindConnectionEvents.room');
		assert.equal(typeof attemptId, 'number', 'bindConnectionEvents.attemptId must be a number');
		assertObjectLike<typeof handlers>(handlers, 'bindConnectionEvents.handlers');
		assert.equal(
			typeof handlers.onConnected,
			'function',
			'bindConnectionEvents.handlers.onConnected must be a function',
		);
		assert.equal(
			typeof handlers.onDisconnected,
			'function',
			'bindConnectionEvents.handlers.onDisconnected must be a function',
		);
		assert.equal(
			typeof handlers.onReconnecting,
			'function',
			'bindConnectionEvents.handlers.onReconnecting must be a function',
		);
		assert.equal(
			typeof handlers.onReconnected,
			'function',
			'bindConnectionEvents.handlers.onReconnected must be a function',
		);
		room.on(RoomEvent.Connected, this.createGuardedHandler(attemptId, handlers.onConnected));
		room.on(RoomEvent.Disconnected, this.createGuardedHandler(attemptId, handlers.onDisconnected));
		room.on(RoomEvent.Reconnecting, this.createGuardedHandler(attemptId, handlers.onReconnecting));
		room.on(RoomEvent.Reconnected, this.createGuardedHandler(attemptId, handlers.onReconnected));
	}

	resetConnectionState(): void {
		assert.ok(this.connectionSnapshot !== null, 'resetConnectionState pre-condition: snapshot present');
		this.abortHotSwap();
		this.update(() => {
			this.isLocalDisconnecting = false;
			this.transitionConnection({type: 'connection.reset'});
		});
		this.invalidateThrottleAttempt();
		this.stopVoicePresenceHeartbeat({markEnded: true});
		this.throttle.setInFlightConnect(false);
	}

	clearInFlightConnect(): void {
		assert.ok(this.throttle !== null, 'clearInFlightConnect pre-condition: throttle present');
		this.throttle.setInFlightConnect(false);
	}

	abortConnection(): void {
		assert.ok(this.connectionSnapshot !== null, 'abortConnection pre-condition: snapshot present');
		this.clearVoiceServerTimeout();
		this.abortHotSwap();
		this.update(() => {
			this.isLocalDisconnecting = false;
			this.transitionConnection({type: 'connection.abort'});
		});
		this.invalidateThrottleAttempt();
		this.stopVoicePresenceHeartbeat({markEnded: true});
		this.throttle.setInFlightConnect(false);
		logger.info('Connection aborted due to gateway error');
	}

	private scheduleVoiceServerTimeout(guildId: string | null, channelId: string): void {
		this.clearVoiceServerTimeout();
		this.voiceServerTimeoutSub = timer(VOICE_SERVER_TIMEOUT_MS).subscribe(() => {
			let didTimeout = false;
			let abandonedConnectionId: string | null = null;
			this.update(() => {
				if (
					this.connectionState.guildId === guildId &&
					this.connectionState.channelId === channelId &&
					!this.connectionState.connected
				) {
					logger.warn('Voice server timeout', {guildId, channelId});
					didTimeout = true;
					abandonedConnectionId = this.connectionState.connectionId;
					this.transitionConnection({type: 'voiceServer.timeout', guildId, channelId});
					this.invalidateThrottleAttempt();
					this.throttle.setInFlightConnect(false);
					this.reconnect.setReconnectState('error');
				}
			});
			if (didTimeout) {
				logger.info('Sending voice state disconnect for abandoned voice connection after timeout', {
					guildId,
					channelId,
					connectionId: abandonedConnectionId,
				});
				sendVoiceStateDisconnect(guildId, abandonedConnectionId);
			}
		});
	}

	private disconnectPreviousRoom(previousRoom: Room | null, stopTracks = true): void {
		if (!previousRoom) return;
		try {
			if (previousRoom.state === 'connected') {
				const tracksToStop = stopTracks ? [] : this.getPreviousRoomNonScreenShareTracks(previousRoom);
				previousRoom.disconnect(stopTracks);
				this.stopPreviousRoomTracks(tracksToStop);
				logger.debug('Previous room disconnected', {stopTracks});
			}
		} catch (error) {
			logger.warn('Failed to disconnect previous room', error);
		}
	}

	private getPreviousRoomNonScreenShareTracks(previousRoom: Room): Array<LocalTrack> {
		const tracks: Array<LocalTrack> = [];
		previousRoom.localParticipant.trackPublications.forEach((publication) => {
			if (publication.source === Track.Source.ScreenShare || publication.source === Track.Source.ScreenShareAudio) {
				return;
			}
			if (publication.track) {
				tracks.push(publication.track);
			}
		});
		return tracks;
	}

	private stopPreviousRoomTracks(tracks: Array<LocalTrack>): void {
		for (const track of tracks) {
			try {
				track.stop();
			} catch (error) {
				logger.warn('Failed to stop previous room local track', {error});
			}
		}
	}

	private clearVoiceServerTimeout(): void {
		this.voiceServerTimeoutSub?.unsubscribe();
		this.voiceServerTimeoutSub = null;
	}

	private clearHotSwapTimeout(): void {
		this.hotSwapTimeoutSub?.unsubscribe();
		this.hotSwapTimeoutSub = null;
	}

	cleanup(): void {
		assert.ok(this.connectionSnapshot !== null, 'cleanup pre-condition: snapshot present');
		assert.ok(this.hotSwapOperationQueue.length <= 4096, 'cleanup pre-condition: queue under cap');
		const {room} = this.connectionState;
		this.clearVoiceServerTimeout();
		this.abortHotSwap();
		if (room) {
			room.removeAllListeners();
			room.disconnect();
		}
		this.update(() => {
			this.isLocalDisconnecting = false;
			this.transitionConnection({type: 'connection.cleanup'});
		});
		this.throttle.reset();
		this.invalidateThrottleAttempt();
		this.reconnect.cleanup();
		logger.info('Cleanup complete');
	}
}

export default new VoiceEngineV2AppConnectionHostAdapter();
