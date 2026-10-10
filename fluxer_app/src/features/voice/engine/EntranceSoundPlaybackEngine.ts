// SPDX-License-Identifier: AGPL-3.0-or-later

import {wrapDesktopLocalResourceURL} from '@app/features/messaging/utils/DesktopResourceUrl';
import {Logger} from '@app/features/platform/utils/AppLogger';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import Sound from '@app/features/ui/state/Sound';
import {getEffectiveAudioState} from '@app/features/voice/engine/VoiceEffectiveAudioState';
import {
	getVoiceEngineV2SnapshotFromMediaEngine,
	setVoiceEngineV2ParticipantAudioLevelSpeaking,
} from '@app/features/voice/engine/VoiceMediaEngineBridge';
import {selectVoiceEngineV2AppParticipants} from '@app/features/voice/engine/v2/VoiceEngineV2AppSelectors';
import EntranceSoundListenerPrefs from '@app/features/voice/state/EntranceSoundListenerPrefs';
import VoiceRegionTeleport from '@app/features/voice/state/VoiceRegionTeleport';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';

const logger = new Logger('EntranceSoundPlaybackEngine');

const BUFFER_CACHE_LIMIT = 16;
const SPEAKING_RELEASE_PADDING_MS = 80;
const IDENTITY_RE = /^user_(\d+)(?:_(.+))?$/;
const MAX_MASTER_VOLUME_PERCENT = 200;

interface PlayParams {
	userId: string;
	hash: string;
	url: string;
	durationMs: number;
}

class EntranceSoundPlaybackEngine {
	private audioContext: AudioContext | null = null;
	private bufferCache: Map<string, AudioBuffer> = new Map();
	private speakingTimers: Map<string, NodeJS.Timeout> = new Map();
	private lastAppliedSinkId: string | null = null;

	private ensureContext(): AudioContext | null {
		if (typeof window === 'undefined') return null;
		if (this.audioContext && this.audioContext.state !== 'closed') {
			if (this.audioContext.state === 'suspended') {
				void this.audioContext.resume().catch((error) => {
					logger.debug('Entrance sound AudioContext resume rejected', {error});
				});
			}
			return this.audioContext;
		}
		const Ctor =
			window.AudioContext || (window as typeof window & {webkitAudioContext?: typeof AudioContext}).webkitAudioContext;
		if (!Ctor) return null;
		try {
			this.audioContext = new Ctor({latencyHint: 'interactive'});
			this.lastAppliedSinkId = null;
			return this.audioContext;
		} catch (error) {
			logger.warn('Failed to create AudioContext', {error});
			return null;
		}
	}

	private async fetchAndDecode(url: string, hash: string): Promise<AudioBuffer | null> {
		const cached = this.bufferCache.get(hash);
		if (cached) {
			this.bufferCache.delete(hash);
			this.bufferCache.set(hash, cached);
			return cached;
		}
		const ctx = this.ensureContext();
		if (!ctx) return null;
		try {
			const response = await fetch(wrapDesktopLocalResourceURL(url), {cache: 'force-cache'});
			if (!response.ok) {
				logger.warn('Entrance sound fetch failed', {url, status: response.status});
				return null;
			}
			const bytes = await response.arrayBuffer();
			const buffer = await ctx.decodeAudioData(bytes);
			this.bufferCache.set(hash, buffer);
			while (this.bufferCache.size > BUFFER_CACHE_LIMIT) {
				const firstKey = this.bufferCache.keys().next().value;
				if (!firstKey) break;
				this.bufferCache.delete(firstKey);
			}
			return buffer;
		} catch (error) {
			logger.warn('Entrance sound decode failed', {url, error});
			return null;
		}
	}

	private findParticipantIdentities(userId: string): Array<string> {
		const snapshot = getVoiceEngineV2SnapshotFromMediaEngine();
		if (!snapshot) return [];
		const identities: Array<string> = [];
		for (const {identity} of selectVoiceEngineV2AppParticipants(snapshot).participants) {
			const match = IDENTITY_RE.exec(identity);
			if (match && match[1] === userId) {
				identities.push(identity);
			}
		}
		return identities;
	}

	private setSpeakingFor(userId: string, speaking: boolean): void {
		for (const identity of this.findParticipantIdentities(userId)) {
			setVoiceEngineV2ParticipantAudioLevelSpeaking(identity, speaking);
		}
	}

	private scheduleSpeakingRelease(userId: string, durationMs: number): void {
		const existing = this.speakingTimers.get(userId);
		if (existing) clearTimeout(existing);
		const timer = setTimeout(() => {
			this.speakingTimers.delete(userId);
			this.setSpeakingFor(userId, false);
		}, Math.max(0, durationMs) + SPEAKING_RELEASE_PADDING_MS);
		this.speakingTimers.set(userId, timer);
	}

	private getMasterVolumeMultiplier(): number {
		return Math.max(0, Math.min(MAX_MASTER_VOLUME_PERCENT, Sound.getMasterVolume())) / 100;
	}

	private applyOutputDevice(ctx: AudioContext): void {
		const deviceId = VoiceSettings.getOutputDeviceId();
		const sinkId = !deviceId || deviceId === 'default' ? '' : deviceId;
		if (sinkId === this.lastAppliedSinkId) return;
		if (sinkId === '' && this.lastAppliedSinkId === null) return;
		const sinkableContext = ctx as AudioContext & {setSinkId?: (sinkId: string) => Promise<void>};
		if (typeof sinkableContext.setSinkId !== 'function') return;
		const previousSinkId = this.lastAppliedSinkId;
		this.lastAppliedSinkId = sinkId;
		void sinkableContext.setSinkId(sinkId).catch((error) => {
			this.lastAppliedSinkId = previousSinkId;
			logger.debug('Failed to apply output device to entrance sound context', {sinkId, error});
		});
	}

	async play(params: PlayParams): Promise<void> {
		const {userId, hash, url, durationMs} = params;
		if (VoiceRegionTeleport.shouldSuppressRejoinSounds()) return;
		if (EntranceSoundListenerPrefs.isMuted(userId)) return;
		if (getEffectiveAudioState().effectiveDeaf) return;
		if (StreamerMode.shouldDisableSounds) return;
		if (!Sound.getSoundEnabled()) return;
		const ctx = this.ensureContext();
		if (!ctx) return;
		this.applyOutputDevice(ctx);
		const buffer = await this.fetchAndDecode(url, hash);
		if (!buffer) return;
		const listenerVolumePct = EntranceSoundListenerPrefs.getVolume(userId);
		const outputVolumePct = VoiceSettings.getOutputVolume();
		const gainValue = Math.max(
			0,
			Math.min(3, (listenerVolumePct / 100) * (outputVolumePct / 100) * this.getMasterVolumeMultiplier()),
		);
		try {
			const source = ctx.createBufferSource();
			source.buffer = buffer;
			const gain = ctx.createGain();
			gain.gain.value = gainValue;
			source.connect(gain).connect(ctx.destination);
			this.setSpeakingFor(userId, true);
			this.scheduleSpeakingRelease(userId, durationMs);
			source.onended = () => {
				const timer = this.speakingTimers.get(userId);
				if (timer) {
					clearTimeout(timer);
					this.speakingTimers.delete(userId);
				}
				this.setSpeakingFor(userId, false);
			};
			source.start();
		} catch (error) {
			logger.warn('Failed to play entrance sound', {userId, hash, error});
			this.setSpeakingFor(userId, false);
		}
	}

	playPreview(buffer: AudioBuffer): void {
		const ctx = this.ensureContext();
		if (!ctx) return;
		try {
			const source = ctx.createBufferSource();
			source.buffer = buffer;
			const gain = ctx.createGain();
			gain.gain.value = Math.max(0, Math.min(3, VoiceSettings.getOutputVolume() / 100));
			source.connect(gain).connect(ctx.destination);
			source.start();
		} catch (error) {
			logger.warn('Failed to play preview', {error});
		}
	}

	async fetchBuffer(url: string, hash: string): Promise<AudioBuffer | null> {
		return this.fetchAndDecode(url, hash);
	}
}

const instance = new EntranceSoundPlaybackEngine();

declare global {
	interface Window {
		_entranceSoundPlaybackEngine?: EntranceSoundPlaybackEngine;
	}
}

if (typeof window !== 'undefined') {
	window._entranceSoundPlaybackEngine = instance;
}

export default instance;
