// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getNoiseSuppressionBackendDescriptor,
	type VoiceNoiseSuppressionBackend,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionBackends';
import {makeAutoObservable} from 'mobx';

export type DeepFilterAssetState = 'idle' | 'loading' | 'ready';

export type NoiseSuppressionFailureReason = 'assets' | 'model_encoding' | 'build' | 'runtime' | 'sample_rate';

export type NoiseSuppressionFallback = 'loading' | 'failed';

export interface NoiseSuppressionFailure {
	count: number;
	retryAtMs: number;
	reason: NoiseSuppressionFailureReason;
	active: boolean;
}

const FAILURE_BACKOFF_BASE_MS = 30_000;
const FAILURE_BACKOFF_MAX_MS = 15 * 60_000;

class NoiseSuppressionAvailability {
	deepFilterAssets: DeepFilterAssetState = 'idle';
	failures = new Map<VoiceNoiseSuppressionBackend, NoiseSuppressionFailure>();
	inputContextRate: number | null = null;
	voiceInputGraphUnavailable = false;
	runningBackend: VoiceNoiseSuppressionBackend | null = null;
	captureSettings: MediaTrackSettings | null = null;
	deepFilterLevels: {inputRms: number; outputRms: number; contextTime: number} | null = null;
	private runtimeOwner: object | null = null;
	private runtimeContext: BaseAudioContext | null = null;
	private listeners = new Set<() => void>();

	constructor() {
		makeAutoObservable<this, 'listeners' | 'notifyListeners' | 'runtimeOwner' | 'runtimeContext'>(
			this,
			{listeners: false, notifyListeners: false, runtimeOwner: false, runtimeContext: false},
			{autoBind: true},
		);
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

	isBackendUsable(backend: VoiceNoiseSuppressionBackend): boolean {
		if (this.failures.get(backend)?.active) return false;
		const descriptor = getNoiseSuppressionBackendDescriptor(backend);
		if (descriptor.engine === 'worklet' || descriptor.engine === 'deep_filter') {
			if (this.voiceInputGraphUnavailable) return false;
			const rates = descriptor.supportedSampleRates;
			if (this.inputContextRate !== null && rates !== null && !rates.includes(this.inputContextRate)) return false;
		}
		return backend !== 'deep_filter' || this.deepFilterAssets === 'ready';
	}

	resolveEffectiveBackend(requested: VoiceNoiseSuppressionBackend): VoiceNoiseSuppressionBackend {
		return this.isBackendUsable(requested) ? requested : 'standard';
	}

	getFallback(requested: VoiceNoiseSuppressionBackend): NoiseSuppressionFallback | null {
		if (this.failures.get(requested)?.active) return 'failed';
		if (requested === 'deep_filter' && this.deepFilterAssets !== 'ready') return 'loading';
		if (!this.isBackendUsable(requested)) return 'failed';
		return null;
	}

	setVoiceInputRuntime(
		owner: object,
		backend: VoiceNoiseSuppressionBackend,
		captureSettings: MediaTrackSettings,
		context: BaseAudioContext | null,
	): void {
		if (backend !== 'deep_filter' || this.runtimeContext !== context) this.deepFilterLevels = null;
		this.runtimeOwner = owner;
		this.runtimeContext = context;
		this.runningBackend = backend;
		this.captureSettings = captureSettings;
	}

	clearVoiceInputRuntime(owner: object): void {
		if (this.runtimeOwner !== owner) return;
		this.runtimeOwner = null;
		this.runtimeContext = null;
		this.runningBackend = null;
		this.captureSettings = null;
		this.deepFilterLevels = null;
	}

	setDeepFilterLevels(context: BaseAudioContext, inputRms: number, outputRms: number, contextTime: number): void {
		if (this.runtimeContext !== context || this.runningBackend !== 'deep_filter') return;
		this.deepFilterLevels = {inputRms, outputRms, contextTime};
	}

	setDeepFilterAssetState(state: DeepFilterAssetState): void {
		if (this.deepFilterAssets === state) return;
		this.deepFilterAssets = state;
		this.notifyListeners();
	}

	setInputContextRate(rate: number): void {
		if (this.inputContextRate === rate) return;
		this.inputContextRate = rate;
		this.notifyListeners();
	}

	setVoiceInputGraphUnavailable(unavailable: boolean): void {
		if (this.voiceInputGraphUnavailable === unavailable) return;
		this.voiceInputGraphUnavailable = unavailable;
		this.notifyListeners();
	}

	markBackendFailed(backend: VoiceNoiseSuppressionBackend, reason: NoiseSuppressionFailureReason, nowMs: number): void {
		const count = (this.failures.get(backend)?.count ?? 0) + 1;
		const backoffMs = Math.min(FAILURE_BACKOFF_BASE_MS * 2 ** (count - 1), FAILURE_BACKOFF_MAX_MS);
		this.failures.set(backend, {count, retryAtMs: nowMs + backoffMs, reason, active: true});
		this.notifyListeners();
	}

	clearBackendFailure(backend: VoiceNoiseSuppressionBackend): void {
		if (!this.failures.delete(backend)) return;
		this.notifyListeners();
	}

	releaseExpiredFailures(nowMs: number): void {
		let changed = false;
		for (const [backend, failure] of this.failures) {
			if (!failure.active || failure.retryAtMs > nowMs) continue;
			this.failures.set(backend, {...failure, active: false});
			changed = true;
		}
		if (changed) this.notifyListeners();
	}
}

export default new NoiseSuppressionAvailability();
