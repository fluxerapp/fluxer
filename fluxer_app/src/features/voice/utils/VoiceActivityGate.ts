import {
	LOCAL_AUTO_MIN_RMS,
	LOCAL_MAX_RMS,
	SPEAKING_LOCAL_RELEASE_MS,
} from '@app/features/voice/engine/VoiceSpeakingThreshold';

const GATE_STARTUP_TIMEOUT_MS = 8000;
const modules = new WeakMap<AudioContext, Promise<void>>();

export interface VoiceActivityGateConfig {
	apply: boolean;
	auto: boolean;
	manualThresholdRms: number;
}

export interface VoiceInputLevel {
	rms: number;
	floorRms: number;
	thresholdRms: number;
	speaking: boolean;
	nonFinite: boolean;
	frame: number;
	contextTime: number;
}

export function loadVoiceActivityGate(context: AudioContext, signal: AbortSignal): Promise<void> {
	let pending = modules.get(context);
	if (!pending) {
		pending = import('@app/features/voice/utils/noise_suppression/NoiseSuppressionWorkletAssets').then((assets) =>
			context.audioWorklet.addModule(assets.VOICE_GATE_WORKLET_URL),
		);
		modules.set(context, pending);
		void pending.catch(() => {
			if (modules.get(context) === pending) modules.delete(context);
		});
	}
	const loading = pending;
	return new Promise((resolve, reject) => {
		const finish = (error?: unknown): void => {
			clearTimeout(timer);
			signal.removeEventListener('abort', onAbort);
			if (error === undefined) resolve();
			else reject(error);
		};
		const onAbort = (): void => finish(signal.reason);
		const timer = setTimeout(
			() => finish(new Error('Voice activity gate did not load in time')),
			GATE_STARTUP_TIMEOUT_MS,
		);
		signal.addEventListener('abort', onAbort, {once: true});
		if (signal.aborted) onAbort();
		void loading.then(() => finish(), finish);
	});
}

export function createVoiceActivityGate(
	context: AudioContext,
	channelCount: 1 | 2,
	config: VoiceActivityGateConfig,
	onLevel: (level: VoiceInputLevel) => void,
	onFailure: () => void,
): AudioWorkletNode {
	const node = new AudioWorkletNode(context, 'fluxer-voice-gate', {
		numberOfInputs: 1,
		numberOfOutputs: 1,
		outputChannelCount: [channelCount],
		channelCount,
		channelCountMode: 'explicit',
		channelInterpretation: 'speakers',
		processorOptions: {
			...config,
			channelCount,
			minRms: LOCAL_AUTO_MIN_RMS,
			maxRms: LOCAL_MAX_RMS,
			releaseMs: SPEAKING_LOCAL_RELEASE_MS,
		},
	});
	node.port.onmessage = ({data}: MessageEvent<VoiceInputLevel & {type: string}>) => {
		if (data.type === 'level') onLevel(data);
	};
	node.onprocessorerror = onFailure;
	return node;
}
