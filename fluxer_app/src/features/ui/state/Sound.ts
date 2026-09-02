// SPDX-License-Identifier: AGPL-3.0-or-later

import * as SoundUtils from '@app/features/notification/utils/SoundUtils';
import {SoundType} from '@app/features/notification/utils/SoundUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {SoundSettingsSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/pickers_pb';
import {makeAutoObservable, reaction, runInAction} from 'mobx';

export interface SoundSettings {
	allSoundsDisabled: boolean;
	disabledSounds: Partial<Record<SoundType, boolean>>;
	masterVolume: number;
	soundOverrides: Partial<Record<SoundType, number>>;
}

export interface SoundPlayOptions {
	bypassSelfDeafened?: boolean;
	ignoreGroupCooldown?: boolean;
}

interface OneShotSoundPolicy {
	group: string;
	debounceMs: number;
	cooldownMs: number;
	maxWaitMs: number;
	priority: number;
}

interface PendingOneShotSound {
	sound: SoundType;
	volume: number;
	bypassSelfDeafened: boolean;
	queuedAt: number;
	lastRequestedAt: number;
	playAfter: number;
}

interface PendingAutoplayBlockedOneShot {
	sound: SoundType;
	volume: number;
	bypassSelfDeafened: boolean;
	expiresAt: number;
}

const DEFAULT_MASTER_VOLUME = 100;
const MAX_VOLUME_PERCENT = 200;
const GLOBAL_ONE_SHOT_MIN_INTERVAL_MS = 260;
export const AUTOPLAY_BLOCKED_ONE_SHOT_TTL_MS = 3000;
export const DEAFEN_SUPPRESSED_SOUND_TYPES: ReadonlySet<SoundType> = new Set([
	SoundType.UserJoin,
	SoundType.UserLeave,
	SoundType.UserMove,
	SoundType.ViewerJoin,
	SoundType.ViewerLeave,
]);
const DEFAULT_ONE_SHOT_SOUND_POLICY: OneShotSoundPolicy = {
	group: 'default',
	debounceMs: 100,
	cooldownMs: 450,
	maxWaitMs: 900,
	priority: 10,
};
const MESSAGE_ONE_SHOT_SOUND_POLICY: OneShotSoundPolicy = {
	group: 'message-notification',
	debounceMs: 120,
	cooldownMs: 900,
	maxWaitMs: 1000,
	priority: 20,
};
const VOICE_PRESENCE_ONE_SHOT_SOUND_POLICY: OneShotSoundPolicy = {
	group: 'voice-presence',
	debounceMs: 180,
	cooldownMs: 900,
	maxWaitMs: 1100,
	priority: 30,
};
const STREAM_VIEWER_ONE_SHOT_SOUND_POLICY: OneShotSoundPolicy = {
	group: 'stream-viewer-presence',
	debounceMs: 180,
	cooldownMs: 900,
	maxWaitMs: 1100,
	priority: 30,
};
const VOICE_CONTROL_ONE_SHOT_SOUND_POLICY: OneShotSoundPolicy = {
	group: 'voice-control',
	debounceMs: 80,
	cooldownMs: 250,
	maxWaitMs: 700,
	priority: 25,
};
const VOICE_MEDIA_ONE_SHOT_SOUND_POLICY: OneShotSoundPolicy = {
	group: 'voice-media',
	debounceMs: 100,
	cooldownMs: 350,
	maxWaitMs: 800,
	priority: 25,
};
const VOICE_DISCONNECT_ONE_SHOT_SOUND_POLICY: OneShotSoundPolicy = {
	group: 'voice-disconnect',
	debounceMs: 40,
	cooldownMs: 750,
	maxWaitMs: 500,
	priority: 60,
};
const DEFAULT_DISABLED_SOUNDS: Partial<Record<SoundType, boolean>> = {
	[SoundType.SameChannelMessage]: true,
};
const ONE_SHOT_SOUND_POLICIES: Partial<Record<SoundType, OneShotSoundPolicy>> = {
	[SoundType.Message]: MESSAGE_ONE_SHOT_SOUND_POLICY,
	[SoundType.DirectMessage]: MESSAGE_ONE_SHOT_SOUND_POLICY,
	[SoundType.SameChannelMessage]: MESSAGE_ONE_SHOT_SOUND_POLICY,
	[SoundType.UserJoin]: VOICE_PRESENCE_ONE_SHOT_SOUND_POLICY,
	[SoundType.UserLeave]: VOICE_PRESENCE_ONE_SHOT_SOUND_POLICY,
	[SoundType.UserMove]: VOICE_PRESENCE_ONE_SHOT_SOUND_POLICY,
	[SoundType.ViewerJoin]: STREAM_VIEWER_ONE_SHOT_SOUND_POLICY,
	[SoundType.ViewerLeave]: STREAM_VIEWER_ONE_SHOT_SOUND_POLICY,
	[SoundType.Mute]: VOICE_CONTROL_ONE_SHOT_SOUND_POLICY,
	[SoundType.Unmute]: VOICE_CONTROL_ONE_SHOT_SOUND_POLICY,
	[SoundType.Deaf]: VOICE_CONTROL_ONE_SHOT_SOUND_POLICY,
	[SoundType.Undeaf]: VOICE_CONTROL_ONE_SHOT_SOUND_POLICY,
	[SoundType.CameraOn]: VOICE_MEDIA_ONE_SHOT_SOUND_POLICY,
	[SoundType.CameraOff]: VOICE_MEDIA_ONE_SHOT_SOUND_POLICY,
	[SoundType.ScreenShareStart]: VOICE_MEDIA_ONE_SHOT_SOUND_POLICY,
	[SoundType.ScreenShareStop]: VOICE_MEDIA_ONE_SHOT_SOUND_POLICY,
	[SoundType.VoiceDisconnect]: VOICE_DISCONNECT_ONE_SHOT_SOUND_POLICY,
};
const clampVolume = (value: number): number => Math.max(0, Math.min(MAX_VOLUME_PERCENT, Math.round(value)));
const isDefaultDisabledSound = (soundType: SoundType): boolean => DEFAULT_DISABLED_SOUNDS[soundType] === true;
const getOneShotSoundPolicy = (soundType: SoundType): OneShotSoundPolicy => {
	const policy = ONE_SHOT_SOUND_POLICIES[soundType];
	if (policy) return policy;
	return {
		...DEFAULT_ONE_SHOT_SOUND_POLICY,
		group: soundType,
	};
};

class Sound {
	private logger = new Logger('Sound');
	private inFlightLoopSounds = new Set<SoundType>();
	private pendingLoopSounds = new Set<SoundType>();
	private loopPlayTokens = new Map<SoundType, number>();
	private pendingOneShotSounds = new Map<string, PendingOneShotSound>();
	private oneShotDrainTimer: NodeJS.Timeout | null = null;
	private lastOneShotPlayedAt = 0;
	private lastOneShotPlayedAtByGroup = new Map<string, number>();
	private unlockListenersAttached = false;
	private selfDeafenedResolver: (() => boolean) | null = null;
	private pendingAutoplayBlockedOneShot: PendingAutoplayBlockedOneShot | null = null;
	private autoplayBlockedWarnedThisSession = false;
	currentlyPlaying = new Set<SoundType>();
	volume = 0.4;
	incomingCallActive = false;
	settings: SoundSettings = {
		allSoundsDisabled: false,
		disabledSounds: {},
		masterVolume: DEFAULT_MASTER_VOLUME,
		soundOverrides: {},
	};
	syncAcrossDevices = true;

	constructor() {
		makeAutoObservable<
			Sound,
			| 'inFlightLoopSounds'
			| 'pendingLoopSounds'
			| 'loopPlayTokens'
			| 'pendingOneShotSounds'
			| 'oneShotDrainTimer'
			| 'lastOneShotPlayedAt'
			| 'lastOneShotPlayedAtByGroup'
			| 'unlockListenersAttached'
			| 'selfDeafenedResolver'
			| 'pendingAutoplayBlockedOneShot'
			| 'autoplayBlockedWarnedThisSession'
		>(
			this,
			{
				currentlyPlaying: false,
				inFlightLoopSounds: false,
				pendingLoopSounds: false,
				loopPlayTokens: false,
				pendingOneShotSounds: false,
				oneShotDrainTimer: false,
				lastOneShotPlayedAt: false,
				lastOneShotPlayedAtByGroup: false,
				unlockListenersAttached: false,
				selfDeafenedResolver: false,
				pendingAutoplayBlockedOneShot: false,
				autoplayBlockedWarnedThisSession: false,
			},
			{autoBind: true},
		);
		this.initPersistence();
		reaction(
			() => StreamerMode.shouldDisableSounds,
			(disabled) => {
				if (disabled) {
					this.stopAllSounds();
				}
			},
		);
	}

	private async initPersistence(): Promise<void> {
		await makeSyncedField(this, {
			field: 'sound',
			schema: SoundSettingsSchema,
			persist: ['settings', 'syncAcrossDevices'],
			enabled: () => this.syncAcrossDevices,
			toMessage: (s) => {
				const init: {
					allSoundsDisabled: boolean;
					masterVolume?: number;
					disabledSounds: Record<string, boolean>;
					soundOverrides: Record<string, number>;
				} = {
					allSoundsDisabled: s.settings.allSoundsDisabled,
					disabledSounds: {...s.settings.disabledSounds} as Record<string, boolean>,
					soundOverrides: {...s.settings.soundOverrides} as Record<string, number>,
				};
				if (s.settings.masterVolume !== DEFAULT_MASTER_VOLUME) {
					init.masterVolume = s.settings.masterVolume;
				}
				return init;
			},
			applyMessage: (s, m) => {
				s.settings = {
					allSoundsDisabled: m.allSoundsDisabled,
					masterVolume: m.masterVolume ?? DEFAULT_MASTER_VOLUME,
					disabledSounds: {...m.disabledSounds} as Partial<Record<SoundType, boolean>>,
					soundOverrides: {...m.soundOverrides} as Partial<Record<SoundType, number>>,
				};
			},
		});
	}

	setSelfDeafenedResolver(resolver: () => boolean): void {
		this.selfDeafenedResolver = resolver;
	}

	private isSuppressedWhileSelfDeafened(sound: SoundType, options: SoundPlayOptions = {}): boolean {
		if (options.bypassSelfDeafened === true) return false;
		if (!DEAFEN_SUPPRESSED_SOUND_TYPES.has(sound)) return false;
		return this.selfDeafenedResolver?.() === true;
	}

	playSound(sound: SoundType, loop = false, options: SoundPlayOptions = {}): void {
		if (StreamerMode.shouldDisableSounds) {
			return;
		}
		if (this.isSuppressedWhileSelfDeafened(sound, options)) {
			return;
		}
		if (!this.isSoundEnabled(sound)) {
			return;
		}
		const effectiveVolume = this.volume * this.getEffectiveMultiplier(sound);
		if (!loop) {
			this.queueOneShotSound(sound, effectiveVolume, options.bypassSelfDeafened === true);
			return;
		}
		if (loop) {
			if (this.isPlayingSound(sound) || this.inFlightLoopSounds.has(sound) || this.pendingLoopSounds.has(sound)) {
				return;
			}
			this.inFlightLoopSounds.add(sound);
		}
		const token = loop ? this.bumpLoopToken(sound) : 0;
		SoundUtils.playSound(sound, loop, effectiveVolume, () => {
			if (loop) {
				this.queuePendingLoopSound(sound);
			}
		})
			.then((result) => {
				if (loop) {
					this.inFlightLoopSounds.delete(sound);
				}
				if (result) {
					if (loop && this.getLoopToken(sound) !== token) {
						void SoundUtils.stopSound(sound);
						return;
					}
					this.clearPendingLoopSound(sound);
					this.addCurrentlyPlayingSound(sound);
				}
			})
			.catch((error) => {
				if (loop) {
					this.inFlightLoopSounds.delete(sound);
				}
				this.logger.warn('Failed to play sound:', error);
			});
	}

	async playOneShotSoundImmediately(
		sound: SoundType,
		options: SoundPlayOptions = {},
		signal?: AbortSignal,
	): Promise<boolean> {
		if (StreamerMode.shouldDisableSounds) return false;
		if (this.isSuppressedWhileSelfDeafened(sound, options)) return false;
		if (!this.isSoundEnabled(sound)) return false;
		const policy = getOneShotSoundPolicy(sound);
		if (options.ignoreGroupCooldown !== true) {
			const lastGroupPlayedAt = this.lastOneShotPlayedAtByGroup.get(policy.group);
			if (lastGroupPlayedAt !== undefined && Date.now() < lastGroupPlayedAt + policy.cooldownMs) return false;
		}
		this.clearPendingOneShotSound(sound);
		const effectiveVolume = this.volume * this.getEffectiveMultiplier(sound);
		const played = await this.playOneShotSoundNow(
			sound,
			effectiveVolume,
			options.bypassSelfDeafened === true,
			signal,
			false,
		);
		if (played) {
			const playedAt = Date.now();
			this.lastOneShotPlayedAt = playedAt;
			this.lastOneShotPlayedAtByGroup.set(policy.group, playedAt);
		}
		return played;
	}

	previewSound(sound: SoundType): void {
		if (StreamerMode.shouldDisableSounds) {
			return;
		}
		const effectiveVolume = this.volume * this.getEffectiveMultiplier(sound);
		SoundUtils.playSound(sound, false, effectiveVolume, () => {}).catch((error) => {
			this.logger.warn('Failed to preview sound:', error);
		});
	}

	stopSound(sound: SoundType): void {
		this.bumpLoopToken(sound);
		this.inFlightLoopSounds.delete(sound);
		this.clearPendingLoopSound(sound);
		this.clearPendingOneShotSound(sound);
		if (this.pendingAutoplayBlockedOneShot?.sound === sound) {
			this.pendingAutoplayBlockedOneShot = null;
			this.detachUnlockListenersIfIdle();
		}
		SoundUtils.stopSound(sound);
		const newPlaying = new Set(this.currentlyPlaying);
		newPlaying.delete(sound);
		this.currentlyPlaying = newPlaying;
	}

	stopAllSounds(): void {
		this.pendingAutoplayBlockedOneShot = null;
		this.clearPendingOneShotSounds();
		this.clearPendingLoopSounds();
		this.inFlightLoopSounds.clear();
		this.loopPlayTokens.clear();
		SoundUtils.stopAllSounds();
		this.currentlyPlaying = new Set();
		this.incomingCallActive = false;
	}

	startIncomingRing(): void {
		this.incomingCallActive = true;
		if (this.isPlayingSound(SoundType.IncomingRing) || this.inFlightLoopSounds.has(SoundType.IncomingRing)) {
			return;
		}
		if (this.pendingLoopSounds.has(SoundType.IncomingRing)) {
			return;
		}
		this.playSound(SoundType.IncomingRing, true);
	}

	stopIncomingRing(): void {
		this.stopSound(SoundType.IncomingRing);
		this.incomingCallActive = false;
	}

	private isSoundEnabled(soundType: SoundType): boolean {
		return !StreamerMode.shouldDisableSounds && !this.settings.allSoundsDisabled && !this.isSoundDisabled(soundType);
	}

	private isSoundDisabled(soundType: SoundType): boolean {
		const configured = this.settings.disabledSounds?.[soundType];
		if (configured !== undefined) {
			return configured;
		}
		return isDefaultDisabledSound(soundType);
	}

	toggleEnabled(): void {
		this.settings = {
			...this.settings,
			allSoundsDisabled: !this.settings.allSoundsDisabled,
		};
		if (this.settings.allSoundsDisabled) {
			this.stopAllSounds();
		}
	}

	updateSettings(settings: {allSoundsDisabled?: boolean; soundType?: SoundType; enabled?: boolean}): void {
		const {soundType, enabled, allSoundsDisabled} = settings;
		if (allSoundsDisabled !== undefined) {
			this.settings = {
				...this.settings,
				allSoundsDisabled,
			};
			if (allSoundsDisabled) {
				this.stopAllSounds();
			}
		} else if (soundType && enabled !== undefined) {
			const newDisabledSounds = {...this.settings.disabledSounds};
			if (enabled) {
				if (isDefaultDisabledSound(soundType)) {
					newDisabledSounds[soundType] = false;
				} else {
					delete newDisabledSounds[soundType];
				}
			} else {
				newDisabledSounds[soundType] = true;
			}
			this.settings = {
				...this.settings,
				disabledSounds: newDisabledSounds,
			};
			if (!enabled) {
				this.clearPendingLoopSound(soundType);
				this.clearPendingOneShotSound(soundType);
			}
		}
	}

	setVolume(volume: number): void {
		this.volume = Math.max(0, Math.min(1, volume));
	}

	getSyncAcrossDevices(): boolean {
		return this.syncAcrossDevices;
	}

	setSyncAcrossDevices(value: boolean): void {
		this.syncAcrossDevices = value;
	}

	getMasterVolume(): number {
		return this.settings.masterVolume ?? DEFAULT_MASTER_VOLUME;
	}

	setMasterVolume(value: number): void {
		this.settings = {
			...this.settings,
			masterVolume: clampVolume(value),
			soundOverrides: this.settings.soundOverrides ?? {},
		};
	}

	getSoundOverride(soundType: SoundType): number | undefined {
		return this.settings.soundOverrides?.[soundType];
	}

	setSoundOverride(soundType: SoundType, value: number): void {
		const overrides = {...(this.settings.soundOverrides ?? {})};
		overrides[soundType] = clampVolume(value);
		this.settings = {
			...this.settings,
			masterVolume: this.settings.masterVolume ?? DEFAULT_MASTER_VOLUME,
			soundOverrides: overrides,
		};
	}

	clearSoundOverride(soundType: SoundType): void {
		const overrides = {...(this.settings.soundOverrides ?? {})};
		if (!(soundType in overrides)) {
			return;
		}
		delete overrides[soundType];
		this.settings = {
			...this.settings,
			masterVolume: this.settings.masterVolume ?? DEFAULT_MASTER_VOLUME,
			soundOverrides: overrides,
		};
	}

	clearAllSoundOverrides(): void {
		if (Object.keys(this.settings.soundOverrides ?? {}).length === 0) {
			return;
		}
		this.settings = {
			...this.settings,
			masterVolume: this.settings.masterVolume ?? DEFAULT_MASTER_VOLUME,
			soundOverrides: {},
		};
	}

	hasAnyOverride(): boolean {
		return Object.keys(this.settings.soundOverrides ?? {}).length > 0;
	}

	private getEffectiveMultiplier(soundType: SoundType): number {
		const override = this.settings.soundOverrides?.[soundType];
		const percent = override !== undefined ? override : (this.settings.masterVolume ?? DEFAULT_MASTER_VOLUME);
		return Math.max(0, Math.min(MAX_VOLUME_PERCENT, percent)) / 100;
	}

	private queueOneShotSound(sound: SoundType, volume: number, bypassSelfDeafened: boolean): void {
		const policy = getOneShotSoundPolicy(sound);
		const now = Date.now();
		const existing = this.pendingOneShotSounds.get(policy.group);
		if (existing) {
			const existingPolicy = getOneShotSoundPolicy(existing.sound);
			if (policy.priority >= existingPolicy.priority) {
				existing.sound = sound;
				existing.bypassSelfDeafened = bypassSelfDeafened;
			}
			existing.volume = Math.max(existing.volume, volume);
			existing.lastRequestedAt = now;
			existing.playAfter = this.getOneShotPlayAfter(getOneShotSoundPolicy(existing.sound), existing.queuedAt, now);
			this.scheduleOneShotDrain();
			return;
		}
		this.pendingOneShotSounds.set(policy.group, {
			sound,
			volume,
			bypassSelfDeafened,
			queuedAt: now,
			lastRequestedAt: now,
			playAfter: this.getOneShotPlayAfter(policy, now, now),
		});
		this.scheduleOneShotDrain();
	}

	private getOneShotPlayAfter(policy: OneShotSoundPolicy, queuedAt: number, now: number): number {
		const debounceAt = Math.min(now + policy.debounceMs, queuedAt + policy.maxWaitMs);
		const lastGroupPlayedAt = this.lastOneShotPlayedAtByGroup.get(policy.group);
		const groupCooldownAt = lastGroupPlayedAt === undefined ? 0 : lastGroupPlayedAt + policy.cooldownMs;
		const globalCooldownAt =
			this.lastOneShotPlayedAt === 0 ? 0 : this.lastOneShotPlayedAt + GLOBAL_ONE_SHOT_MIN_INTERVAL_MS;
		return Math.max(debounceAt, groupCooldownAt, globalCooldownAt);
	}

	private scheduleOneShotDrain(): void {
		if (this.oneShotDrainTimer) {
			clearTimeout(this.oneShotDrainTimer);
			this.oneShotDrainTimer = null;
		}
		let nextPlayAfter: number | null = null;
		for (const pending of this.pendingOneShotSounds.values()) {
			pending.playAfter = Math.max(
				pending.playAfter,
				this.getOneShotPlayAfter(getOneShotSoundPolicy(pending.sound), pending.queuedAt, pending.lastRequestedAt),
			);
			if (nextPlayAfter === null || pending.playAfter < nextPlayAfter) {
				nextPlayAfter = pending.playAfter;
			}
		}
		if (nextPlayAfter === null) return;
		this.oneShotDrainTimer = setTimeout(this.drainOneShotSounds, Math.max(0, nextPlayAfter - Date.now()));
	}

	private drainOneShotSounds(): void {
		this.oneShotDrainTimer = null;
		const now = Date.now();
		let selectedGroup: string | null = null;
		let selectedSound: PendingOneShotSound | null = null;
		for (const [group, pending] of this.pendingOneShotSounds) {
			if (pending.playAfter > now) continue;
			if (
				!selectedSound ||
				getOneShotSoundPolicy(pending.sound).priority > getOneShotSoundPolicy(selectedSound.sound).priority
			) {
				selectedGroup = group;
				selectedSound = pending;
			}
		}
		if (!selectedSound || !selectedGroup) {
			this.scheduleOneShotDrain();
			return;
		}
		this.pendingOneShotSounds.delete(selectedGroup);
		if (
			this.isSoundEnabled(selectedSound.sound) &&
			!this.isSuppressedWhileSelfDeafened(selectedSound.sound, selectedSound)
		) {
			this.lastOneShotPlayedAt = now;
			this.lastOneShotPlayedAtByGroup.set(selectedGroup, now);
			void this.playOneShotSoundNow(selectedSound.sound, selectedSound.volume, selectedSound.bypassSelfDeafened);
		}
		this.scheduleOneShotDrain();
	}

	private handleOneShotAutoplayBlocked(sound: SoundType, volume: number, bypassSelfDeafened: boolean): void {
		if (!this.autoplayBlockedWarnedThisSession) {
			this.autoplayBlockedWarnedThisSession = true;
			this.logger.warn('Autoplay blocked a one-shot sound; queueing it for the next user gesture', {sound});
		}
		this.pendingAutoplayBlockedOneShot = {
			sound,
			volume,
			bypassSelfDeafened,
			expiresAt: Date.now() + AUTOPLAY_BLOCKED_ONE_SHOT_TTL_MS,
		};
		this.attachUnlockListeners();
	}

	private flushPendingAutoplayBlockedOneShot(): void {
		const pending = this.pendingAutoplayBlockedOneShot;
		if (!pending) return;
		this.pendingAutoplayBlockedOneShot = null;
		if (Date.now() > pending.expiresAt) return;
		if (!this.isSoundEnabled(pending.sound)) return;
		if (this.isSuppressedWhileSelfDeafened(pending.sound, pending)) return;
		void this.playOneShotSoundNow(pending.sound, pending.volume, pending.bypassSelfDeafened);
	}

	private async playOneShotSoundNow(
		sound: SoundType,
		volume: number,
		bypassSelfDeafened: boolean,
		signal?: AbortSignal,
		retryOnAutoplayBlocked = true,
	): Promise<boolean> {
		try {
			const result = await SoundUtils.playSound(
				sound,
				false,
				volume,
				retryOnAutoplayBlocked ? () => this.handleOneShotAutoplayBlocked(sound, volume, bypassSelfDeafened) : undefined,
				signal,
			);
			if (!result) return false;
			this.addCurrentlyPlayingSound(sound);
			const remove = (): void => {
				signal?.removeEventListener('abort', remove);
				this.removeCurrentlyPlayingSound(sound);
			};
			result.addEventListener('ended', remove, {once: true});
			result.addEventListener('error', remove, {once: true});
			signal?.addEventListener('abort', remove, {once: true});
			return true;
		} catch (error) {
			this.logger.warn('Failed to play sound:', error);
			return false;
		}
	}

	private addCurrentlyPlayingSound(sound: SoundType): void {
		runInAction(() => {
			const newPlaying = new Set(this.currentlyPlaying);
			newPlaying.add(sound);
			this.currentlyPlaying = newPlaying;
		});
	}

	private removeCurrentlyPlayingSound(sound: SoundType): void {
		runInAction(() => {
			const newPlaying = new Set(this.currentlyPlaying);
			newPlaying.delete(sound);
			this.currentlyPlaying = newPlaying;
		});
	}

	private clearPendingOneShotSound(soundType: SoundType): void {
		let changed = false;
		for (const [group, pending] of this.pendingOneShotSounds) {
			if (pending.sound !== soundType) continue;
			this.pendingOneShotSounds.delete(group);
			changed = true;
		}
		if (!changed) return;
		this.scheduleOneShotDrain();
	}

	private clearPendingOneShotSounds(): void {
		this.pendingOneShotSounds.clear();
		if (!this.oneShotDrainTimer) return;
		clearTimeout(this.oneShotDrainTimer);
		this.oneShotDrainTimer = null;
	}

	getSoundEnabled(): boolean {
		return !this.settings.allSoundsDisabled;
	}

	getSoundSettings(): SoundSettings {
		return this.settings;
	}

	isSoundTypeEnabled(soundType: SoundType): boolean {
		return this.isSoundEnabled(soundType);
	}

	getVolume(): number {
		return this.volume;
	}

	isIncomingCallActive(): boolean {
		return this.incomingCallActive;
	}

	isPlayingSound(sound: SoundType): boolean {
		return this.currentlyPlaying.has(sound);
	}

	private bumpLoopToken(sound: SoundType): number {
		const nextToken = (this.loopPlayTokens.get(sound) ?? 0) + 1;
		this.loopPlayTokens.set(sound, nextToken);
		return nextToken;
	}

	private getLoopToken(sound: SoundType): number {
		return this.loopPlayTokens.get(sound) ?? 0;
	}

	private queuePendingLoopSound(sound: SoundType): void {
		if (this.pendingLoopSounds.has(sound)) {
			return;
		}
		this.pendingLoopSounds.add(sound);
		this.attachUnlockListeners();
	}

	private clearPendingLoopSound(sound: SoundType): void {
		if (this.pendingLoopSounds.delete(sound)) {
			this.detachUnlockListenersIfIdle();
		}
	}

	private clearPendingLoopSounds(): void {
		this.pendingLoopSounds.clear();
		this.detachUnlockListeners();
	}

	private attachUnlockListeners(): void {
		if (typeof window === 'undefined') {
			return;
		}
		if (this.unlockListenersAttached) {
			return;
		}
		this.unlockListenersAttached = true;
		window.addEventListener('pointerdown', this.handleAutoplayUnlock, {passive: true});
		window.addEventListener('keydown', this.handleAutoplayUnlock);
	}

	private detachUnlockListenersIfIdle(): void {
		if (this.pendingLoopSounds.size > 0 || this.pendingAutoplayBlockedOneShot) {
			return;
		}
		this.detachUnlockListeners();
	}

	private detachUnlockListeners(): void {
		if (!this.unlockListenersAttached) {
			return;
		}
		window.removeEventListener('pointerdown', this.handleAutoplayUnlock);
		window.removeEventListener('keydown', this.handleAutoplayUnlock);
		this.unlockListenersAttached = false;
	}

	private handleAutoplayUnlock(): void {
		this.flushPendingAutoplayBlockedOneShot();
		this.retryPendingLoopSounds();
	}

	private retryPendingLoopSounds(): void {
		if (this.pendingLoopSounds.size === 0) {
			this.detachUnlockListenersIfIdle();
			return;
		}
		for (const sound of Array.from(this.pendingLoopSounds)) {
			if (!this.isSoundEnabled(sound)) {
				this.pendingLoopSounds.delete(sound);
				continue;
			}
			if (sound === SoundType.IncomingRing && !this.incomingCallActive) {
				this.pendingLoopSounds.delete(sound);
				continue;
			}
			if (this.isPlayingSound(sound) || this.inFlightLoopSounds.has(sound)) {
				this.pendingLoopSounds.delete(sound);
				continue;
			}
			this.pendingLoopSounds.delete(sound);
			this.playSound(sound, true);
		}
		this.detachUnlockListenersIfIdle();
	}
}

export default new Sound();
