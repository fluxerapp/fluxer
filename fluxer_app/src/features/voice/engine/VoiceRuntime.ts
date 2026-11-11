// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayErrorData} from '@app/features/gateway/transport/GatewaySocket';
import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import {setSoundOutputDeviceIdResolver} from '@app/features/notification/utils/SoundUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {loadLazyModule} from '@app/features/platform/utils/LazyModuleLoader';
import Sound from '@app/features/ui/state/Sound';
import Users from '@app/features/user/state/Users';
import {Store, useStoreVersion} from '@app/features/voice/engine/Store';
import {getVoiceAudioControlsFromAppState} from '@app/features/voice/engine/VoiceAudioControls';
import {getEffectiveAudioState} from '@app/features/voice/engine/VoiceEffectiveAudioState';
import type {NormalizedVoiceState} from '@app/features/voice/engine/VoiceGatewayStateMachine';
import type {LivekitParticipantSnapshot} from '@app/features/voice/engine/VoiceParticipantStateMachine';
import type * as VoiceRuntimeModule from '@app/features/voice/engine/VoiceRuntimeModule';
import {
	selectVoiceEngineV2AppMuteReason,
	type VoiceEngineV2AppVoiceMuteReason,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppSelectors';
import voiceStates from '@app/features/voice/engine/v2/VoiceEngineV2AppVoiceStateAdapter';
import VoiceSessionRestore from '@app/features/voice/state/VoiceSessionRestore';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {isVoicePermissionMuteActive} from '@app/features/voice/utils/VoicePermissionUtils';
import type {VoiceEngineV2Model} from '@fluxer/voice_engine_v2/src/protocol/types';
import type {Room} from 'livekit-client';
import {makeObservable, observableRef, runInAction} from 'mobx';

const logger = new Logger('VoiceRuntime');

const IDLE_LOAD_TIMEOUT_MS = 10_000;
const CHANNEL_RENDER_WAIT_MS = 15_000;

type LoadedVoiceRuntime = typeof VoiceRuntimeModule;
export type MediaEngine = LoadedVoiceRuntime['MediaEngine'];

export type VoiceRuntimeLoadReason =
	| 'current-user-voice-state'
	| 'call-ring'
	| 'join'
	| 'session-restore'
	| 'action'
	| 'idle'
	| 'engine-created';

export interface VoiceRuntimeConnection {
	readonly guildId: string | null;
	readonly channelId: string | null;
	readonly connectionId: string | null;
	readonly connected: boolean;
	readonly connecting: boolean;
	readonly reconnecting: boolean;
}

const NO_VOICE_CONNECTION: VoiceRuntimeConnection = Object.freeze({
	guildId: null,
	channelId: null,
	connectionId: null,
	connected: false,
	connecting: false,
	reconnecting: false,
});

const NO_PARTICIPANTS: Readonly<Record<string, LivekitParticipantSnapshot>> = Object.freeze({});

type IdleLoadState =
	| {readonly kind: 'waiting-for-ready'; readonly channelRendered: boolean}
	| {readonly kind: 'waiting-for-channel'; readonly timer: number}
	| {readonly kind: 'waiting-for-idle'; readonly cancel: () => void}
	| {readonly kind: 'done'};

function runWhenIdle(callback: () => void): () => void {
	if (typeof window.requestIdleCallback === 'function') {
		const handle = window.requestIdleCallback(callback, {timeout: IDLE_LOAD_TIMEOUT_MS});
		return () => window.cancelIdleCallback(handle);
	}
	const handle = window.setTimeout(callback, 1000);
	return () => window.clearTimeout(handle);
}

class VoiceRuntime extends Store {
	private loaded: LoadedVoiceRuntime | null = null;
	private registeredEngine: MediaEngine | null = null;
	private loading: Promise<LoadedVoiceRuntime> | null = null;
	private idleLoad: IdleLoadState = {kind: 'waiting-for-ready', channelRendered: false};
	private prewarmed = false;

	constructor() {
		super();
		makeObservable<this, 'loaded' | 'registeredEngine'>(this, {loaded: observableRef, registeredEngine: observableRef});
		Sound.setSelfDeafenedResolver(() => getEffectiveAudioState().effectiveDeaf);
		setSoundOutputDeviceIdResolver(() => VoiceSettings.getOutputDeviceId());
	}

	get module(): LoadedVoiceRuntime | null {
		return this.loaded;
	}

	get engine(): MediaEngine | null {
		return this.registeredEngine;
	}

	get connection(): VoiceRuntimeConnection {
		const engine = this.engine;
		if (engine === null) return NO_VOICE_CONNECTION;
		return engine.connectionContext;
	}

	get voiceEngineV2Model(): VoiceEngineV2Model | null {
		const engine = this.engine;
		return engine === null ? null : engine.voiceEngineV2Model;
	}

	get room(): Room | null {
		const engine = this.engine;
		return engine === null ? null : engine.room;
	}

	get guildId(): string | null {
		return this.connection.guildId;
	}

	get channelId(): string | null {
		return this.connection.channelId;
	}

	get connectionId(): string | null {
		return this.connection.connectionId;
	}

	get connected(): boolean {
		return this.connection.connected;
	}

	get connecting(): boolean {
		return this.connection.connecting;
	}

	get reconnecting(): boolean {
		return this.connection.reconnecting;
	}

	isMicrophoneFailureLatched(): boolean {
		return this.engine?.isMicrophoneFailureLatched() === true;
	}

	getMuteReason(voiceState: VoiceState | null): VoiceEngineV2AppVoiceMuteReason {
		return selectVoiceEngineV2AppMuteReason({
			voiceState,
			permissionMuted: isVoicePermissionMuteActive(voiceState, this.guildId, this.channelId),
			audio: getVoiceAudioControlsFromAppState(this.isMicrophoneFailureLatched()),
		});
	}

	get connectFailed(): boolean {
		return this.engine?.connectFailed === true;
	}

	get participants(): Readonly<Record<string, LivekitParticipantSnapshot>> {
		const engine = this.engine;
		return engine === null ? NO_PARTICIPANTS : engine.participants;
	}

	getParticipantByUserIdAndConnectionId(
		userId: string,
		connectionId: string | null,
	): LivekitParticipantSnapshot | undefined {
		const engine = this.engine;
		if (engine === null) return undefined;
		return engine.getParticipantByUserIdAndConnectionId(userId, connectionId);
	}

	get connectionVoiceStates(): Readonly<Record<string, NormalizedVoiceState>> {
		return voiceStates.getConnectionVoiceStates();
	}

	getCurrentUserVoiceState(guildId?: string | null): NormalizedVoiceState | null {
		return voiceStates.getCurrentUserVoiceState(guildId, Users.getCurrentUser()?.id, this.connectionId);
	}

	getVoiceState(guildId: string | null, userId?: string): NormalizedVoiceState | null {
		const currentUserId = Users.getCurrentUser()?.id;
		const connectionId = !userId || userId === currentUserId ? this.connectionId : null;
		return voiceStates.getVoiceState(guildId, userId, currentUserId, connectionId);
	}

	getVoiceStateByConnectionId(connectionId: string): NormalizedVoiceState | null {
		return voiceStates.getVoiceStateByConnectionId(connectionId);
	}

	isVoiceConnectionIgnored(connectionId: string | null | undefined): boolean {
		return voiceStates.isConnectionIgnored(connectionId);
	}

	getAllVoiceStatesInChannel(guildId: string, channelId: string): Readonly<Record<string, NormalizedVoiceState>> {
		return voiceStates.getAllVoiceStatesInChannel(guildId, channelId);
	}

	getAllVoiceStatesInGuild(
		guildId: string,
	): Readonly<Record<string, Readonly<Record<string, NormalizedVoiceState>>>> | undefined {
		return voiceStates.getAllVoiceStatesInGuild(guildId);
	}

	getAllVoiceStates(): Readonly<
		Record<string, Readonly<Record<string, Readonly<Record<string, NormalizedVoiceState>>>>>
	> {
		return voiceStates.getAllVoiceStates();
	}

	load(reason: VoiceRuntimeLoadReason): Promise<LoadedVoiceRuntime> {
		if (this.loaded !== null) return Promise.resolve(this.loaded);
		if (this.loading !== null) return this.loading;
		logger.debug('Loading the voice runtime', {reason});
		this.cancelIdleLoad();
		const loading = loadLazyModule(
			() => import(/* webpackChunkName: "voice-runtime" */ '@app/features/voice/engine/VoiceRuntimeModule'),
		).then(
			(loaded) => {
				if (this.loading !== loading) return loaded;
				this.loading = null;
				runInAction(() => {
					this.loaded = loaded;
				});
				this.emitChange();
				return loaded;
			},
			(error: unknown) => {
				if (this.loading === loading) this.loading = null;
				logger.error('Failed to load the voice runtime', {reason, error});
				throw error;
			},
		);
		this.loading = loading;
		return loading;
	}

	registerEngine(engine: MediaEngine): void {
		if (this.registeredEngine === engine) return;
		if (this.registeredEngine !== null)
			throw new Error('A second media engine tried to register with the voice runtime');
		runInAction(() => {
			this.registeredEngine = engine;
		});
		engine.subscribe(() => this.emitChange());
		this.emitChange();
		this.prewarmWhenReady();
		if (this.loaded === null) void this.load('engine-created');
	}

	async withEngine<T>(reason: VoiceRuntimeLoadReason, run: (engine: MediaEngine) => T | Promise<T>): Promise<T> {
		const loaded = await this.load(reason);
		return run(loaded.MediaEngine);
	}

	noteChannelContentRendered(): void {
		const state = this.idleLoad;
		if (state.kind === 'waiting-for-ready') {
			this.idleLoad = {kind: 'waiting-for-ready', channelRendered: true};
			return;
		}
		if (state.kind !== 'waiting-for-channel') return;
		window.clearTimeout(state.timer);
		this.scheduleIdleLoad();
	}

	handleGatewayReady(guilds: Array<GuildReadyData>): void {
		const engine = this.engine;
		if (engine !== null) {
			engine.handleGatewayReady(guilds);
		} else {
			voiceStates.handleGatewayReady(guilds);
		}
		this.prewarmWhenReady();
		if (this.loaded !== null) return;
		if (this.hasCurrentUserVoiceState()) {
			void this.load('current-user-voice-state');
			return;
		}
		if (VoiceSessionRestore.getSnapshotForUser(Users.currentUserId) !== null) {
			void this.load('session-restore');
			return;
		}
		if (this.idleLoad.kind !== 'waiting-for-ready') return;
		if (this.idleLoad.channelRendered) {
			this.scheduleIdleLoad();
			return;
		}
		this.idleLoad = {
			kind: 'waiting-for-channel',
			timer: window.setTimeout(() => this.scheduleIdleLoad(), CHANNEL_RENDER_WAIT_MS),
		};
	}

	handleGuildCreate(guild: GuildReadyData): void {
		const engine = this.engine;
		if (engine !== null) {
			engine.handleGuildCreate(guild);
			return;
		}
		voiceStates.handleGuildCreate(guild);
		this.loadForCurrentUserVoiceStates(guild.voice_states);
	}

	handleGuildDelete(guildId: string): void {
		const engine = this.engine;
		if (engine !== null) {
			engine.handleGuildDelete(guildId);
			return;
		}
		voiceStates.handleGuildDelete(guildId);
	}

	handleChannelDelete(channelId: string): void {
		const engine = this.engine;
		if (engine !== null) {
			engine.handleChannelDelete(channelId);
			return;
		}
		VoiceSessionRestore.clearSnapshotForChannel(channelId);
	}

	handlePassiveVoiceStates(guildId: string, passiveVoiceStates: Array<VoiceState>): void {
		const engine = this.engine;
		if (engine !== null) {
			engine.handlePassiveVoiceStates(guildId, passiveVoiceStates);
			return;
		}
		for (const voiceState of passiveVoiceStates) {
			voiceStates.handleGatewayVoiceStateUpdate(guildId, {...voiceState, guild_id: voiceState.guild_id ?? guildId});
		}
		this.loadForCurrentUserVoiceStates(passiveVoiceStates);
	}

	handleGatewayVoiceStateUpdate(guildId: string | null, voiceState: VoiceState): void {
		const engine = this.engine;
		if (engine !== null) {
			engine.handleGatewayVoiceStateUpdate(guildId, voiceState);
			return;
		}
		voiceStates.handleGatewayVoiceStateUpdate(guildId, voiceState);
		this.loadForCurrentUserVoiceStates([voiceState]);
	}

	handleGatewayError(error: GatewayErrorData): void {
		const engine = this.engine;
		if (engine === null) {
			logger.debug('Ignoring a gateway error while the voice runtime is not loaded', {code: error.code});
			return;
		}
		engine.handleGatewayError(error);
	}

	handleCallRing(): void {
		if (this.loaded !== null) return;
		void this.load('call-ring');
	}

	cleanup(): void {
		const engine = this.engine;
		if (engine !== null) {
			engine.cleanup();
			return;
		}
		voiceStates.clearAllVoiceStates();
	}

	private hasCurrentUserVoiceState(): boolean {
		const userId = Users.currentUserId;
		if (userId === null) return false;
		return voiceStates.getCurrentUserVoiceState(undefined, userId, null) !== null;
	}

	private loadForCurrentUserVoiceStates(candidates: ReadonlyArray<VoiceState> | undefined): void {
		if (this.loaded !== null || candidates === undefined) return;
		const userId = Users.currentUserId;
		if (userId === null) return;
		if (candidates.some((voiceState) => voiceState.user_id === userId && voiceState.channel_id != null)) {
			void this.load('current-user-voice-state');
		}
	}

	private scheduleIdleLoad(): void {
		if (this.loaded !== null || this.loading !== null) return;
		if (this.idleLoad.kind === 'waiting-for-idle' || this.idleLoad.kind === 'done') return;
		this.idleLoad = {
			kind: 'waiting-for-idle',
			cancel: runWhenIdle(() => {
				this.idleLoad = {kind: 'done'};
				void this.load('idle');
			}),
		};
	}

	private cancelIdleLoad(): void {
		const state = this.idleLoad;
		if (state.kind === 'waiting-for-channel') window.clearTimeout(state.timer);
		if (state.kind === 'waiting-for-idle') state.cancel();
		this.idleLoad = {kind: 'done'};
	}

	private prewarmWhenReady(): void {
		const engine = this.engine;
		if (engine === null || this.prewarmed || Users.currentUserId === null) return;
		this.prewarmed = true;
		runWhenIdle(() => engine.prewarmInBackground('startup'));
	}
}

const instance = new VoiceRuntime();

export function useVoiceRuntimeVersion(): number {
	return useStoreVersion(instance);
}

export default instance;
