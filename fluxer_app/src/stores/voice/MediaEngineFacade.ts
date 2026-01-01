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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import type {Participant, Room, ScreenShareCaptureOptions, TrackPublishOptions} from 'livekit-client';
import {computed, makeObservable} from 'mobx';
import * as SoundActionCreators from '~/actions/SoundActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {type GatewayErrorCode, GatewayErrorCodes} from '~/Constants';
import type {GatewayErrorData} from '~/lib/GatewaySocket';
import {Logger} from '~/lib/Logger';
import {voiceStatsDB} from '~/lib/VoiceStatsDB';
import type {GuildReadyData} from '~/records/GuildRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import CallMediaPrefsStore from '~/stores/CallMediaPrefsStore';
import ConnectionStore from '~/stores/ConnectionStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import IdleStore from '~/stores/IdleStore';
import LocalVoiceStateStore from '~/stores/LocalVoiceStateStore';
import UserStore from '~/stores/UserStore';
import {SoundType} from '~/utils/SoundUtils';
import {
	checkChannelLimit,
	checkMultipleConnections,
	sendVoiceStateConnect,
	sendVoiceStateDisconnect,
} from './VoiceChannelConnector';
import type {VoiceServerUpdateData} from './VoiceConnectionManager';
import VoiceConnectionManager from './VoiceConnectionManager';
import VoiceMediaManager from './VoiceMediaManager';
import type {LivekitParticipantSnapshot} from './VoiceParticipantManager';
import VoiceParticipantManager from './VoiceParticipantManager';
import VoicePermissionManager from './VoicePermissionManager';
import {bindRoomEvents} from './VoiceRoomEventBinder';
import type {VoiceState} from './VoiceStateManager';
import VoiceStateManager from './VoiceStateManager';
import {VoiceStateSyncManager, type VoiceStateSyncPayload} from './VoiceStateSyncManager';
import type {LatencyDataPoint, VoiceStats} from './VoiceStatsManager';
import {VoiceStatsManager} from './VoiceStatsManager';
import VoiceSubscriptionManager from './VoiceSubscriptionManager';

const logger = new Logger('MediaEngineFacade');
const AFK_CHECK_INTERVAL_MS = 10000;

class MediaEngineFacade {
	private statsManager: VoiceStatsManager;
	private afkIntervalId: ReturnType<typeof setInterval> | null = null;
	private voiceStateSync = new VoiceStateSyncManager();
	private i18n: I18n | null = null;

	constructor() {
		this.statsManager = new VoiceStatsManager();
		makeObservable(this, {
			room: computed,
			guildId: computed,
			channelId: computed,
			connectionId: computed,
			connected: computed,
			connecting: computed,
			voiceServerEndpoint: computed,
			participants: computed,
			currentLatency: computed,
			averageLatency: computed,
			latencyHistory: computed,
			voiceStats: computed,
			displayLatency: computed,
			estimatedLatency: computed,
		});

		(window as typeof window & {_mediaEngineStore?: MediaEngineFacade})._mediaEngineStore = this;
		logger.debug('MediaEngineFacade initialized');
	}

	setI18n(i18n: I18n): void {
		this.i18n = i18n;
	}

	get room(): Room | null {
		return VoiceConnectionManager.room;
	}
	get guildId(): string | null {
		return VoiceConnectionManager.guildId;
	}
	get channelId(): string | null {
		return VoiceConnectionManager.channelId;
	}
	get connectionId(): string | null {
		return VoiceConnectionManager.connectionId;
	}
	get connected(): boolean {
		return VoiceConnectionManager.connected;
	}
	get connecting(): boolean {
		return VoiceConnectionManager.connecting;
	}
	get voiceServerEndpoint(): string | null {
		return VoiceConnectionManager.voiceServerEndpoint;
	}

	get participants(): Readonly<Record<string, LivekitParticipantSnapshot>> {
		return VoiceParticipantManager.participants;
	}

	get currentLatency(): number | null {
		return this.statsManager.currentLatency;
	}
	get averageLatency(): number | null {
		return this.statsManager.averageLatency;
	}
	get latencyHistory(): Array<LatencyDataPoint> {
		return this.statsManager.latencyHistory;
	}
	get voiceStats(): VoiceStats {
		return this.statsManager.voiceStats;
	}
	get estimatedLatency(): number | null {
		return this.statsManager.estimatedLatency;
	}
	get displayLatency(): number | null {
		return this.statsManager.displayLatency;
	}

	async connectToVoiceChannel(guildId: string | null, channelId: string): Promise<void> {
		const currentUserId = AuthenticationStore.currentUserId;
		const isTimedOut =
			guildId && currentUserId ? (GuildMemberStore.getMember(guildId, currentUserId)?.isTimedOut() ?? false) : false;
		if (isTimedOut) {
			if (!this.i18n) {
				throw new Error('MediaEngineFacade: i18n not initialized');
			}
			ToastActionCreators.createToast({
				type: 'error',
				children: this.i18n._(msg`You can't join while you're on timeout.`),
			});
			return;
		}
		if (!ConnectionStore.socket) {
			logger.warn('[connectToVoiceChannel] No socket');
			return;
		}

		if (!checkChannelLimit(guildId, channelId)) return;

		const shouldProceed = checkMultipleConnections(
			guildId,
			channelId,
			async () => this.connectDirectly(guildId, channelId),
			() => void this.connectDirectly(guildId, channelId),
			() => VoiceConnectionManager.clearInFlightConnect(),
		);
		if (!shouldProceed) return;

		if (VoiceConnectionManager.connected || VoiceConnectionManager.connecting) {
			if (VoiceConnectionManager.channelId !== channelId || VoiceConnectionManager.guildId !== guildId) {
				await this.disconnectFromVoiceChannel('user');
			}
		}

		VoiceConnectionManager.startConnection(guildId, channelId);
		sendVoiceStateConnect(guildId, channelId);
	}

	private async connectDirectly(guildId: string | null, channelId: string): Promise<void> {
		if (VoiceConnectionManager.connected || VoiceConnectionManager.connecting) {
			if (VoiceConnectionManager.channelId !== channelId || VoiceConnectionManager.guildId !== guildId) {
				SoundActionCreators.playSound(SoundType.UserMove);
				await this.disconnectFromVoiceChannel('user');
			}
		}
		VoiceConnectionManager.startConnection(guildId, channelId);
		sendVoiceStateConnect(guildId, channelId);
	}

	async disconnectFromVoiceChannel(reason: 'user' | 'error' | 'server' = 'user'): Promise<void> {
		const {guildId, connectionId, connected, connecting, channelId} = VoiceConnectionManager.connectionState;
		if (!connected && !connecting && !channelId) return;

		this.stopTracking();

		if (reason !== 'server' && connectionId) {
			sendVoiceStateDisconnect(guildId, connectionId);
		}

		if (reason === 'user') {
			SoundActionCreators.playSound(SoundType.VoiceDisconnect);
		}

		LocalVoiceStateStore.updateSelfVideo(false);
		LocalVoiceStateStore.updateSelfStream(false);
		VoiceMediaManager.resetStreamTracking();
		VoiceParticipantManager.clear();
		this.voiceStateSync.reset();
		VoiceConnectionManager.disconnectFromVoiceChannel(reason);
		if (connectionId) CallMediaPrefsStore.clearForCall(connectionId);
		logger.info('[disconnectFromVoiceChannel] Disconnected', {reason});
	}

	handleVoiceServerUpdate(raw: VoiceServerUpdateData): void {
		VoiceConnectionManager.handleVoiceServerUpdate(raw, (room, attemptId, guildId, channelId) => {
			bindRoomEvents(room, attemptId, guildId, channelId, {
				onConnected: async () => this.startTracking(room),
				onDisconnected: () => this.stopTracking(),
				onReconnecting: () => {
					this.statsManager.stopLatencyTracking();
					this.statsManager.stopStatsTracking();
				},
				onReconnected: () => {
					this.statsManager.startLatencyTracking();
					this.statsManager.startStatsTracking();
				},
			});
		});
	}

	handleConnectionOpen(guilds: Array<GuildReadyData>): void {
		VoiceStateManager.handleConnectionOpen(guilds);
	}
	handleGuildCreate(guild: GuildReadyData): void {
		VoiceStateManager.handleGuildCreate(guild);
	}
	handleGuildDelete(guildId: string): void {
		VoiceStateManager.handleGuildDelete(guildId);
		if (VoiceConnectionManager.connected && VoiceConnectionManager.guildId === guildId) {
			void this.disconnectFromVoiceChannel('server');
		}
	}
	handleGatewayVoiceStateUpdate(guildId: string | null, voiceState: VoiceState): void {
		VoiceStateManager.handleGatewayVoiceStateUpdate(guildId, voiceState);
		const user = UserStore.getCurrentUser();
		const isLocalUser =
			user && voiceState.user_id === user.id && voiceState.connection_id === VoiceConnectionManager.connectionId;

		if (isLocalUser) {
			const serverPayload =
				voiceState.channel_id && voiceState.connection_id
					? {
							guild_id: guildId,
							channel_id: voiceState.channel_id,
							connection_id: voiceState.connection_id,
							self_mute: voiceState.self_mute,
							self_deaf: voiceState.self_deaf,
							self_video: voiceState.self_video,
							self_stream: voiceState.self_stream,
							viewer_stream_key: voiceState.viewer_stream_key ?? null,
						}
					: null;
			this.voiceStateSync.confirmServerState(serverPayload);
			if (voiceState.channel_id === null && (VoiceConnectionManager.connected || VoiceConnectionManager.connecting)) {
				void this.disconnectFromVoiceChannel('server');
			}
		}
	}
	handleGatewayVoiceStateDelete(guildId: string, userId: string): void {
		VoiceStateManager.handleGatewayVoiceStateDelete(guildId, userId);
	}
	getCurrentUserVoiceState(guildId?: string | null): VoiceState | null {
		return VoiceStateManager.getCurrentUserVoiceState(guildId, UserStore.getCurrentUser()?.id);
	}
	getVoiceState(guildId: string | null, userId?: string): VoiceState | null {
		return VoiceStateManager.getVoiceState(guildId, userId, UserStore.getCurrentUser()?.id);
	}
	getVoiceStateByConnectionId(connectionId: string): VoiceState | null {
		return VoiceStateManager.getVoiceStateByConnectionId(connectionId);
	}
	getAllVoiceStatesInChannel(guildId: string, channelId: string): Readonly<Record<string, VoiceState>> {
		return VoiceStateManager.getAllVoiceStatesInChannel(guildId, channelId);
	}
	getAllVoiceStates(): Readonly<Record<string, Readonly<Record<string, Readonly<Record<string, VoiceState>>>>>> {
		return VoiceStateManager.getAllVoiceStates();
	}

	syncLocalVoiceStateWithServer(partial?: {
		self_video?: boolean;
		self_stream?: boolean;
		self_mute?: boolean;
		self_deaf?: boolean;
		viewer_stream_key?: string | null;
	}): void {
		LocalVoiceStateStore.ensurePermissionMute();
		const {guildId, channelId, connectionId} = VoiceConnectionManager.connectionState;
		if (!channelId || !connectionId) return;

		const payload: VoiceStateSyncPayload = {
			guild_id: guildId,
			channel_id: channelId,
			connection_id: connectionId,
			self_mute: partial?.self_mute ?? LocalVoiceStateStore.getSelfMute(),
			self_deaf: partial?.self_deaf ?? LocalVoiceStateStore.getSelfDeaf(),
			self_video: partial?.self_video ?? LocalVoiceStateStore.getSelfVideo(),
			self_stream: partial?.self_stream ?? LocalVoiceStateStore.getSelfStream(),
			viewer_stream_key: partial?.viewer_stream_key ?? LocalVoiceStateStore.getViewerStreamKey(),
		};

		this.voiceStateSync.requestState(payload);
	}

	getParticipantByUserIdAndConnectionId(
		userId: string,
		connectionId: string | null,
	): LivekitParticipantSnapshot | undefined {
		return VoiceParticipantManager.getParticipantByUserIdAndConnectionId(userId, connectionId);
	}
	upsertParticipant(participant: Participant): void {
		VoiceParticipantManager.upsertParticipant(participant);
	}

	async setCameraEnabled(enabled: boolean, options?: {deviceId?: string; sendUpdate?: boolean}): Promise<void> {
		await VoiceMediaManager.setCameraEnabled(enabled, options);
	}
	async setScreenShareEnabled(
		enabled: boolean,
		options?: ScreenShareCaptureOptions & {sendUpdate?: boolean},
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		await VoiceMediaManager.setScreenShareEnabled(enabled, options, publishOptions);
	}
	applyLocalAudioPreferencesForUser(userId: string): void {
		VoiceMediaManager.applyLocalAudioPreferencesForUser(userId, this.room);
	}
	applyAllLocalAudioPreferences(): void {
		VoiceMediaManager.applyAllLocalAudioPreferences(this.room);
	}
	setLocalVideoDisabled(identity: string, disabled: boolean): void {
		VoiceMediaManager.setLocalVideoDisabled(identity, disabled, this.room, this.connectionId);
	}
	applyPushToTalkHold(held: boolean): void {
		VoiceMediaManager.applyPushToTalkHold(held, this.room, () => this.getCurrentUserVoiceState());
	}
	handlePushToTalkModeChange(): void {
		VoiceMediaManager.handlePushToTalkModeChange(this.room, () => this.getCurrentUserVoiceState());
	}
	getMuteReason(voiceState: VoiceState | null): 'guild' | 'push_to_talk' | 'self' | null {
		return VoiceMediaManager.getMuteReason(voiceState);
	}
	async toggleCameraFromKeybind(): Promise<void> {
		await VoiceMediaManager.toggleCameraFromKeybind();
	}
	async toggleScreenShareFromKeybind(): Promise<void> {
		await VoiceMediaManager.toggleScreenShareFromKeybind();
	}

	private startTracking(roomOverride?: Room | null): void {
		const room = roomOverride ?? VoiceConnectionManager.room;
		if (!room) {
			logger.warn('[startTracking] No room available');
			return;
		}

		this.statsManager.setRoom(room);
		this.statsManager.startLatencyTracking();
		this.statsManager.startStatsTracking();
		VoiceSubscriptionManager.setRoom(room);
		VoicePermissionManager.initializeSubscriptions(room);
		this.startAfkTracking();
		logger.info('[startTracking] All tracking started');
	}

	private stopTracking(): void {
		this.statsManager.stopLatencyTracking();
		this.statsManager.stopStatsTracking();
		VoiceSubscriptionManager.cleanup();
		this.stopAfkTracking();
		logger.info('[stopTracking] All tracking stopped');
	}

	private startAfkTracking(): void {
		this.stopAfkTracking();
		this.afkIntervalId = setInterval(() => {
			if (!VoiceConnectionManager.connected || !VoiceConnectionManager.guildId || !VoiceConnectionManager.channelId)
				return;
			if (!IdleStore.isIdle()) return;
			const idleSince = IdleStore.getIdleSince();
			if (!idleSince) return;
			const guild = GuildStore.getGuild(VoiceConnectionManager.guildId);
			if (!guild?.afkChannelId || !guild.afkTimeout) return;
			if (VoiceConnectionManager.channelId === guild.afkChannelId) return;
			if (Math.floor((Date.now() - idleSince) / 1000) >= guild.afkTimeout) {
				void this.moveToAfkChannel();
			}
		}, AFK_CHECK_INTERVAL_MS);
	}

	private stopAfkTracking(): void {
		if (this.afkIntervalId !== null) {
			clearInterval(this.afkIntervalId);
			this.afkIntervalId = null;
		}
	}

	async moveToAfkChannel(): Promise<void> {
		const {guildId, channelId, connected} = VoiceConnectionManager.connectionState;
		if (!connected || !guildId || !channelId) return;
		const guild = GuildStore.getGuild(guildId);
		if (!guild?.afkChannelId || channelId === guild.afkChannelId) return;
		SoundActionCreators.playSound(SoundType.UserMove);
		await this.connectToVoiceChannel(guildId, guild.afkChannelId);
	}

	getLastConnectedChannel(): {guildId: string; channelId: string} | null {
		return VoiceConnectionManager.lastConnectedChannel;
	}
	getShouldReconnect(): boolean {
		return VoiceConnectionManager.shouldAutoReconnect;
	}
	markReconnectionAttempted(): void {
		VoiceConnectionManager.markReconnectionAttempted();
	}

	handleLogout(): void {
		this.stopTracking();
		VoiceConnectionManager.cleanup();
		VoiceStateManager.clearAllVoiceStates();
		VoiceParticipantManager.clear();
		VoicePermissionManager.reset();
		VoiceMediaManager.resetStreamTracking();
		LocalVoiceStateStore.updateSelfVideo(false);
		LocalVoiceStateStore.updateSelfStream(false);
		this.voiceStateSync.reset();
		voiceStatsDB.clear().catch(() => {});
		logger.info('[handleLogout] Cleanup complete');
	}

	handleGatewayError(error: GatewayErrorData): void {
		const voiceErrorCodes = new Set<GatewayErrorCode>([
			GatewayErrorCodes.VOICE_CONNECTION_NOT_FOUND,
			GatewayErrorCodes.VOICE_CHANNEL_NOT_FOUND,
			GatewayErrorCodes.VOICE_INVALID_CHANNEL_TYPE,
			GatewayErrorCodes.VOICE_MEMBER_NOT_FOUND,
			GatewayErrorCodes.VOICE_MEMBER_TIMED_OUT,
			GatewayErrorCodes.VOICE_USER_NOT_IN_VOICE,
			GatewayErrorCodes.VOICE_GUILD_NOT_FOUND,
			GatewayErrorCodes.VOICE_PERMISSION_DENIED,
			GatewayErrorCodes.VOICE_CHANNEL_FULL,
			GatewayErrorCodes.VOICE_MISSING_CONNECTION_ID,
			GatewayErrorCodes.VOICE_TOKEN_FAILED,
		]);

		if (!voiceErrorCodes.has(error.code)) {
			return;
		}

		logger.warn(`Voice-related gateway error: [${error.code}] ${error.message}`);

		if (error.code === GatewayErrorCodes.VOICE_CONNECTION_NOT_FOUND) {
			if (VoiceConnectionManager.connecting) {
				logger.info('[handleGatewayError] Connection not found while connecting, aborting');
				VoiceConnectionManager.abortConnection();
			}
		} else if (
			error.code === GatewayErrorCodes.VOICE_PERMISSION_DENIED ||
			error.code === GatewayErrorCodes.VOICE_CHANNEL_FULL ||
			error.code === GatewayErrorCodes.VOICE_MEMBER_TIMED_OUT
		) {
			if (VoiceConnectionManager.connecting && !VoiceConnectionManager.connected) {
				logger.info('[handleGatewayError] Permission denied, channel full, or timeout while connecting, aborting');
				VoiceConnectionManager.abortConnection();
			}
			if (error.code === GatewayErrorCodes.VOICE_MEMBER_TIMED_OUT) {
				if (!this.i18n) {
					throw new Error('MediaEngineFacade: i18n not initialized');
				}
				ToastActionCreators.createToast({
					type: 'error',
					children: this.i18n._(msg`You can't join while you're on timeout.`),
				});
			}
		} else if (error.code === GatewayErrorCodes.VOICE_TOKEN_FAILED) {
			if (VoiceConnectionManager.connecting) {
				logger.info('[handleGatewayError] Token failed while connecting, aborting');
				VoiceConnectionManager.abortConnection();
			}
		}
	}

	cleanup(): void {
		this.stopTracking();
		this.statsManager.cleanup();
		VoiceSubscriptionManager.cleanup();
		VoicePermissionManager.reset();
		VoiceMediaManager.resetStreamTracking();
		VoiceConnectionManager.cleanup();
		VoiceStateManager.clearAllVoiceStates();
		VoiceParticipantManager.clear();
		this.voiceStateSync.reset();
	}

	reset(): void {
		this.statsManager.reset();
		VoiceConnectionManager.resetConnectionState();
		VoiceConnectionManager.resetReconnectState();
		VoicePermissionManager.reset();
		VoiceMediaManager.resetStreamTracking();
		VoiceParticipantManager.clear();
		this.voiceStateSync.reset();
	}
}

export type {LivekitParticipantSnapshot, VoiceStats, VoiceState, LatencyDataPoint};

const instance = new MediaEngineFacade();
(window as typeof window & {_mediaEngineFacade?: MediaEngineFacade})._mediaEngineFacade = instance;
export default instance;
