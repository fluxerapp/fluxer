// SPDX-License-Identifier: AGPL-3.0-or-later

import MediaPermission from '@app/features/permissions/system/state/MediaPermission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import VoiceDevicePermissionState from '@app/features/voice/engine/VoiceDevicePermissionState';
import {syncLocalVoiceStateWithServer} from '@app/features/voice/engine/VoiceMediaEngineBridge';
import {
	createLocalVoiceStateSnapshot,
	type LocalVoiceConnectionState,
	type LocalVoiceStateContext,
	type LocalVoiceStateEvent,
	type LocalVoiceStateSnapshot,
	transitionLocalVoiceStateSnapshot,
} from '@app/features/voice/state/LocalVoiceStateMachine';
import type {VoiceDeviceState, VoiceMediaPermissionStatus} from '@app/features/voice/utils/VoiceDeviceManager';
import {makeAutoObservable, observable, reaction, runInAction} from 'mobx';

const logger = new Logger('LocalVoiceState');

interface VoiceConnectionContextAccess {
	connectionId?: string | null;
}

function getActiveMediaEngineConnectionId(): string | null {
	try {
		const mediaEngine =
			(
				window as typeof window & {
					_mediaEngineFacade?: VoiceConnectionContextAccess;
					_mediaEngine?: VoiceConnectionContextAccess;
				}
			)._mediaEngineFacade ??
			(window as typeof window & {_mediaEngine?: VoiceConnectionContextAccess})._mediaEngine ??
			null;
		return mediaEngine?.connectionId ?? null;
	} catch (error) {
		logger.error('Failed to read active voice connection id from media engine', {error});
		return null;
	}
}

class LocalVoiceState implements LocalVoiceConnectionState {
	selfMute = !MediaPermission.isMicrophoneGranted();
	selfDeaf = false;
	selfVideo = false;
	selfStream = false;
	selfStreamAudio = false;
	selfStreamAudioMute = false;
	noiseSuppressionEnabled = true;
	viewerStreamKeys: Array<string> = [];
	hasUserSetMute = false;
	hasUserSetDeaf = false;
	shouldUnmuteOnUndeafen = false;
	private microphonePermissionGranted: boolean | null = MediaPermission.isMicrophoneGranted();
	mutedByPermission = !MediaPermission.isMicrophoneGranted();
	persistedSelfMute = !MediaPermission.isMicrophoneGranted();
	persistedSelfDeaf = false;
	persistedHasUserSetMute = false;
	persistedHasUserSetDeaf = false;
	private persistenceHydrationPromise: Promise<void>;
	private _disposers: Array<() => void> = [];
	private listeners = new Set<() => void>();
	private lastDeviceAudioPermissionStatus: VoiceMediaPermissionStatus | null =
		VoiceDevicePermissionState.getState().permissionStatus.audio;
	private isNotifyingServerOfPermissionMute = false;
	private connectionStates = observable.object<Record<string, LocalVoiceConnectionState>>({});
	private machineSnapshot: LocalVoiceStateSnapshot;

	constructor() {
		this.machineSnapshot = createLocalVoiceStateSnapshot({
			microphonePermissionGranted: this.microphonePermissionGranted,
			persistedDefaults: this.getPersistedAudioDefaults(),
			fallback: this.getFallbackStateSeed(),
		});
		makeAutoObservable<
			this,
			| 'microphonePermissionGranted'
			| 'mutedByPermission'
			| '_disposers'
			| 'listeners'
			| 'isNotifyingServerOfPermissionMute'
			| 'shouldUnmuteOnUndeafen'
			| 'connectionStates'
			| 'machineSnapshot'
			| 'getFallbackStateSeed'
			| 'transitionLocalState'
			| 'applyMachineSnapshotToObservableState'
			| 'notifyListeners'
			| 'syncObservableConnectionStates'
			| 'copyConnectionState'
			| 'getConnectionState'
			| 'getActiveConnectionId'
			| 'getActiveStateForRead'
		>(
			this,
			{
				microphonePermissionGranted: false,
				mutedByPermission: false,
				_disposers: false,
				listeners: false,
				isNotifyingServerOfPermissionMute: false,
				shouldUnmuteOnUndeafen: false,
				connectionStates: false,
				machineSnapshot: false,
				getFallbackStateSeed: false,
				transitionLocalState: false,
				applyMachineSnapshotToObservableState: false,
				notifyListeners: false,
				syncObservableConnectionStates: false,
				copyConnectionState: false,
				getConnectionState: false,
				getActiveConnectionId: false,
				getActiveStateForRead: false,
				getSelfMute: false,
				getSelfDeaf: false,
				getSelfVideo: false,
				getSelfStream: false,
				getSelfStreamAudio: false,
				getSelfStreamAudioMute: false,
				getViewerStreamKeys: false,
				hasViewerStreamKey: false,
				getNoiseSuppressionEnabled: false,
				getHasUserSetMute: false,
				getHasUserSetDeaf: false,
				getMutedByPermission: false,
			},
			{autoBind: true},
		);
		this._disposers = [];
		this.persistenceHydrationPromise = this.initPersistence().catch((error) => {
			logger.error('Failed to hydrate LocalVoiceState from persistence', error);
		});
		this.initializePersistedDefaultSync();
		this.initializePermissionSync();
		this.initializeDevicePermissionSync();
	}

	private getFallbackStateSeed(): LocalVoiceConnectionState {
		return {
			selfMute: this.selfMute,
			selfDeaf: this.selfDeaf,
			selfVideo: this.selfVideo,
			selfStream: this.selfStream,
			viewerStreamKeys: this.viewerStreamKeys,
			hasUserSetMute: this.hasUserSetMute,
			hasUserSetDeaf: this.hasUserSetDeaf,
			mutedByPermission: this.mutedByPermission,
			shouldUnmuteOnUndeafen: this.shouldUnmuteOnUndeafen,
		};
	}

	private getPersistedAudioDefaults(): Pick<
		LocalVoiceConnectionState,
		'selfMute' | 'selfDeaf' | 'hasUserSetMute' | 'hasUserSetDeaf'
	> {
		return {
			selfMute: this.persistedSelfMute,
			selfDeaf: this.persistedSelfDeaf,
			hasUserSetMute: this.persistedHasUserSetMute,
			hasUserSetDeaf: this.persistedHasUserSetDeaf,
		};
	}

	private transitionLocalState(event: LocalVoiceStateEvent): void {
		this.machineSnapshot = transitionLocalVoiceStateSnapshot(this.machineSnapshot, event);
		this.applyMachineSnapshotToObservableState();
		this.notifyListeners();
	}

	subscribe(listener: () => void): () => void {
		this.listeners.add(listener);
		return () => {
			this.listeners.delete(listener);
		};
	}

	private notifyListeners(): void {
		for (const listener of [...this.listeners]) {
			listener();
		}
	}

	private applyMachineSnapshotToObservableState(): void {
		const {fallback, persistedDefaults, microphonePermissionGranted, connections} = this.machineSnapshot.context;
		this.selfMute = fallback.selfMute;
		this.selfDeaf = fallback.selfDeaf;
		this.selfVideo = fallback.selfVideo;
		this.selfStream = fallback.selfStream;
		this.viewerStreamKeys = [...fallback.viewerStreamKeys];
		this.hasUserSetMute = fallback.hasUserSetMute;
		this.hasUserSetDeaf = fallback.hasUserSetDeaf;
		this.mutedByPermission = fallback.mutedByPermission;
		this.shouldUnmuteOnUndeafen = fallback.shouldUnmuteOnUndeafen;
		this.persistedSelfMute = persistedDefaults.selfMute;
		this.persistedSelfDeaf = persistedDefaults.selfDeaf;
		this.persistedHasUserSetMute = persistedDefaults.hasUserSetMute;
		this.persistedHasUserSetDeaf = persistedDefaults.hasUserSetDeaf;
		this.microphonePermissionGranted = microphonePermissionGranted;
		this.syncObservableConnectionStates(connections);
	}

	private syncObservableConnectionStates(connections: LocalVoiceStateContext['connections']): void {
		for (const connectionId of Object.keys(this.connectionStates)) {
			if (!connections[connectionId]) {
				delete this.connectionStates[connectionId];
			}
		}
		for (const [connectionId, state] of Object.entries(connections)) {
			const existingState = this.connectionStates[connectionId];
			if (existingState) {
				this.copyConnectionState(existingState, state);
			} else {
				this.connectionStates[connectionId] = observable.object<LocalVoiceConnectionState>({
					...state,
					viewerStreamKeys: [...state.viewerStreamKeys],
				});
			}
		}
	}

	private copyConnectionState(target: LocalVoiceConnectionState, source: LocalVoiceConnectionState): void {
		target.selfMute = source.selfMute;
		target.selfDeaf = source.selfDeaf;
		target.selfVideo = source.selfVideo;
		target.selfStream = source.selfStream;
		target.viewerStreamKeys = [...source.viewerStreamKeys];
		target.hasUserSetMute = source.hasUserSetMute;
		target.hasUserSetDeaf = source.hasUserSetDeaf;
		target.mutedByPermission = source.mutedByPermission;
		target.shouldUnmuteOnUndeafen = source.shouldUnmuteOnUndeafen;
	}

	private getActiveConnectionId(): string | null {
		return getActiveMediaEngineConnectionId();
	}

	private getConnectionState(connectionId: string | null): LocalVoiceConnectionState | null {
		if (!connectionId) {
			return null;
		}
		return this.connectionStates[connectionId] ?? null;
	}

	private getActiveStateForRead(): LocalVoiceConnectionState {
		return this.getConnectionState(this.getActiveConnectionId()) ?? this;
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(
			this,
			'LocalVoiceState',
			[
				'persistedSelfMute',
				'persistedSelfDeaf',
				'noiseSuppressionEnabled',
				'persistedHasUserSetMute',
				'persistedHasUserSetDeaf',
				'selfStreamAudio',
			],
			{
				syncAcrossTabs: true,
			},
		);
		runInAction(() => {
			this.applyPersistedDefaultsToFallbackState();
		});
		logger.debug('LocalVoiceState hydrated from localStorage on reload');
	}

	private initializePersistedDefaultSync(): void {
		const disposer = reaction(
			() => ({
				activeConnectionId: this.getActiveConnectionId(),
				selfMute: this.persistedSelfMute,
				selfDeaf: this.persistedSelfDeaf,
				hasUserSetMute: this.persistedHasUserSetMute,
				hasUserSetDeaf: this.persistedHasUserSetDeaf,
			}),
			({activeConnectionId}) => {
				if (activeConnectionId) {
					return;
				}
				runInAction(() => {
					this.applyPersistedDefaultsToFallbackState();
				});
			},
			{
				name: 'LocalVoiceState-persistedDefaultSync',
			},
		);
		this._disposers.push(disposer);
	}

	private applyPersistedDefaultsToFallbackState(): void {
		this.transitionLocalState({
			type: 'defaults.apply',
			activeConnectionId: this.getActiveConnectionId(),
			persistedDefaults: this.getPersistedAudioDefaults(),
		});
	}

	dispose(): void {
		this._disposers.forEach((disposer) => disposer());
		this._disposers = [];
	}

	private async initializePermissionSync(): Promise<void> {
		try {
			let defaultMuteInitialized = false;
			await this.persistenceHydrationPromise;
			const syncWithPermission = (source: 'init' | 'change') => {
				if (!MediaPermission.isInitialized()) {
					return;
				}
				const isMicGranted = MediaPermission.isMicrophoneGranted();
				const permissionState = MediaPermission.getMicrophonePermissionState();
				logger.debug(source === 'init' ? 'Checking microphone permission for sync' : 'Microphone permission changed', {
					isMicGranted,
					permissionState,
					currentMute: this.getSelfMute(),
					hasUserSetMute: this.getHasUserSetMute(),
					mutedByPermission: this.getActiveStateForRead().mutedByPermission,
				});
				if (!isMicGranted) {
					this.applyTransientPermissionMute();
					return;
				}
				const state = this.getActiveStateForRead();
				const wasMutedByPermission = state.mutedByPermission;
				const shouldApplyDefaultUnmute =
					!defaultMuteInitialized && !state.hasUserSetMute && state.selfMute && !wasMutedByPermission;
				runInAction(() => {
					this.transitionLocalState({
						type: 'permission.sync',
						activeConnectionId: this.getActiveConnectionId(),
						microphoneGranted: true,
						defaultMuteInitialized,
					});
					if (wasMutedByPermission) {
						logger.info('Microphone permission granted, restoring persisted mute preference', {
							permissionState,
							persistedSelfMute: this.persistedSelfMute,
							persistedHasUserSetMute: this.persistedHasUserSetMute,
						});
					} else if (shouldApplyDefaultUnmute) {
						logger.info('Microphone permission granted, defaulting to unmuted state', {permissionState});
					}
				});
				defaultMuteInitialized = true;
			};
			syncWithPermission('init');
			const disposer = MediaPermission.addChangeListener(() => {
				syncWithPermission('change');
			});
			this._disposers.push(disposer);
		} catch (err) {
			logger.error('Failed to initialize permission sync', err);
		}
	}

	private initializeDevicePermissionSync(): void {
		const disposer = VoiceDevicePermissionState.subscribe((state) => {
			this.handleDevicePermissionStatus(state.permissionStatus);
		});
		this._disposers.push(disposer);
	}

	private handleDevicePermissionStatus(status: VoiceDeviceState['permissionStatus']): void {
		const audioStatus = status.audio;
		if (audioStatus === this.lastDeviceAudioPermissionStatus) {
			return;
		}
		this.lastDeviceAudioPermissionStatus = audioStatus;
		if (audioStatus === 'granted') {
			void this.applyPermissionGrant();
		} else if (audioStatus === 'denied') {
			this.applyTransientPermissionMute();
		}
	}

	private enforcePermissionMuteIfNeeded(): void {
		const devicePermission = VoiceDevicePermissionState.getState().permissionStatus;
		const granted = MediaPermission.isMicrophoneGranted() || devicePermission.audio === 'granted';
		if (granted) {
			runInAction(() => {
				this.transitionLocalState({type: 'permission.grant', activeConnectionId: this.getActiveConnectionId()});
			});
			return;
		}
		this.applyTransientPermissionMute();
	}

	private applyTransientPermissionMute(): void {
		const shouldNotify = !this.isNotifyingServerOfPermissionMute;
		runInAction(() => {
			this.transitionLocalState({type: 'permission.deny', activeConnectionId: this.getActiveConnectionId()});
		});
		if (shouldNotify) {
			void this.notifyServerOfPermissionMute();
		}
	}

	private async applyPermissionGrant(): Promise<void> {
		await this.persistenceHydrationPromise;
		runInAction(() => {
			this.transitionLocalState({type: 'permission.grant', activeConnectionId: this.getActiveConnectionId()});
		});
	}

	private notifyServerOfPermissionMute(): void {
		if (this.isNotifyingServerOfPermissionMute) {
			logger.debug('Skipping recursive notifyServerOfPermissionMute call');
			return;
		}
		try {
			this.isNotifyingServerOfPermissionMute = true;
			syncLocalVoiceStateWithServer({self_mute: true});
		} catch (error) {
			logger.error('Failed to sync permission-mute to server', {error});
			throw error;
		} finally {
			this.isNotifyingServerOfPermissionMute = false;
		}
	}

	getSelfMute(): boolean {
		return this.getActiveStateForRead().selfMute;
	}

	ensurePermissionMute(): void {
		this.enforcePermissionMuteIfNeeded();
	}

	getSelfDeaf(): boolean {
		return this.getActiveStateForRead().selfDeaf;
	}

	getSelfVideo(): boolean {
		return this.getActiveStateForRead().selfVideo;
	}

	getSelfStream(): boolean {
		return this.getActiveStateForRead().selfStream;
	}

	getSelfStreamAudio(): boolean {
		return this.selfStreamAudio;
	}

	getSelfStreamAudioMute(): boolean {
		return this.selfStreamAudioMute;
	}

	getViewerStreamKeys(): Array<string> {
		return this.getActiveStateForRead().viewerStreamKeys;
	}

	updateViewerStreamKeys(keys: Array<string>): void {
		runInAction(() => {
			this.transitionLocalState({
				type: 'viewer.replace',
				activeConnectionId: this.getActiveConnectionId(),
				keys,
			});
		});
	}

	hasViewerStreamKey(key: string): boolean {
		return this.getActiveStateForRead().viewerStreamKeys.includes(key);
	}

	getNoiseSuppressionEnabled(): boolean {
		return this.noiseSuppressionEnabled;
	}

	getHasUserSetMute(): boolean {
		return this.getActiveStateForRead().hasUserSetMute;
	}

	getHasUserSetDeaf(): boolean {
		return this.getActiveStateForRead().hasUserSetDeaf;
	}

	getMutedByPermission(): boolean {
		return this.getActiveStateForRead().mutedByPermission;
	}

	toggleSelfMute(): void {
		runInAction(() => {
			this.transitionLocalState({type: 'mute.toggle', activeConnectionId: this.getActiveConnectionId()});
			logger.debug('User toggled self mute', {newSelfMute: this.getSelfMute(), hasUserSetMute: true});
		});
	}

	toggleSelfDeaf(): void {
		runInAction(() => {
			this.transitionLocalState({type: 'deaf.toggle', activeConnectionId: this.getActiveConnectionId()});
			logger.debug('User toggled self deaf', {newSelfDeaf: this.getSelfDeaf(), hasUserSetDeaf: true});
		});
	}

	toggleSelfVideo(): void {
		runInAction(() => {
			this.transitionLocalState({type: 'video.toggle', activeConnectionId: this.getActiveConnectionId()});
			logger.debug('User toggled self video', {selfVideo: this.getSelfVideo()});
		});
	}

	toggleSelfStream(): void {
		runInAction(() => {
			this.transitionLocalState({type: 'stream.toggle', activeConnectionId: this.getActiveConnectionId()});
			logger.debug('User toggled self stream', {selfStream: this.getSelfStream()});
		});
	}

	toggleSelfStreamAudio(): void {
		runInAction(() => {
			this.selfStreamAudio = !this.selfStreamAudio;
			logger.debug('User toggled self stream audio', {selfStreamAudio: this.selfStreamAudio});
		});
	}

	toggleSelfStreamAudioMute(): void {
		runInAction(() => {
			this.selfStreamAudioMute = !this.selfStreamAudioMute;
			logger.debug('User toggled self stream audio mute', {selfStreamAudioMute: this.selfStreamAudioMute});
		});
	}

	toggleNoiseSuppression(): void {
		runInAction(() => {
			this.noiseSuppressionEnabled = !this.noiseSuppressionEnabled;
			logger.debug('User toggled noise suppression', {enabled: this.noiseSuppressionEnabled});
		});
	}

	clearUserSetMute(): void {
		runInAction(() => {
			if (!this.getActiveStateForRead().hasUserSetMute) return;
			this.transitionLocalState({type: 'mute.clearUserSet', activeConnectionId: this.getActiveConnectionId()});
			logger.debug('Cleared hasUserSetMute flag');
		});
	}

	updateSelfMute(muted: boolean): void {
		runInAction(() => {
			this.transitionLocalState({type: 'mute.update', activeConnectionId: this.getActiveConnectionId(), muted});
			logger.debug('Self mute updated', {muted});
		});
	}

	updateSelfDeaf(deafened: boolean): void {
		runInAction(() => {
			this.transitionLocalState({type: 'deaf.update', activeConnectionId: this.getActiveConnectionId(), deafened});
			logger.debug('Self deaf updated', {deafened});
		});
	}

	updateSelfVideo(video: boolean): void {
		runInAction(() => {
			this.transitionLocalState({type: 'video.update', activeConnectionId: this.getActiveConnectionId(), video});
			logger.debug('Self video updated', {video});
		});
	}

	updateSelfStream(streaming: boolean): void {
		runInAction(() => {
			this.transitionLocalState({type: 'stream.update', activeConnectionId: this.getActiveConnectionId(), streaming});
			logger.debug('Self stream updated', {streaming});
		});
	}

	updateSelfStreamAudio(enabled: boolean): void {
		runInAction(() => {
			this.selfStreamAudio = enabled;
			logger.debug('Self stream audio updated', {enabled});
		});
	}

	updateSelfStreamAudioMute(muted: boolean): void {
		runInAction(() => {
			this.selfStreamAudioMute = muted;
			logger.debug('Self stream audio mute updated', {muted});
		});
	}

	resetUserPreferences(): void {
		runInAction(() => {
			this.transitionLocalState({type: 'preferences.reset'});
			this.selfStreamAudio = false;
			this.selfStreamAudioMute = false;
			this.noiseSuppressionEnabled = true;
		});
		logger.info('Reset user voice preferences');
	}

	seedConnectionState(
		connectionId: string,
		seed: Partial<
			Pick<LocalVoiceConnectionState, 'selfMute' | 'selfDeaf' | 'selfVideo' | 'selfStream' | 'viewerStreamKeys'>
		>,
	): void {
		if (this.connectionStates[connectionId]) {
			return;
		}
		runInAction(() => {
			this.transitionLocalState({type: 'connection.seed', connectionId, seed});
		});
	}

	syncConnectionState(
		connectionId: string,
		seed: Partial<
			Pick<
				LocalVoiceConnectionState,
				| 'selfMute'
				| 'selfDeaf'
				| 'selfVideo'
				| 'selfStream'
				| 'viewerStreamKeys'
				| 'hasUserSetMute'
				| 'hasUserSetDeaf'
				| 'mutedByPermission'
				| 'shouldUnmuteOnUndeafen'
			>
		>,
	): void {
		runInAction(() => {
			this.transitionLocalState({type: 'connection.sync', connectionId, seed});
		});
	}

	clearConnectionState(connectionId: string | null): void {
		if (!connectionId) {
			return;
		}
		runInAction(() => {
			this.transitionLocalState({type: 'connection.clear', connectionId});
		});
	}
}

export default new LocalVoiceState();
