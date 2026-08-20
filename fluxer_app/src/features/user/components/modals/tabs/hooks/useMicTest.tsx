// SPDX-License-Identifier: AGPL-3.0-or-later

import {handleMediaPermissionBlocked} from '@app/features/permissions/system/commands/MacPermissionsModalCommands';
import MediaPermission from '@app/features/permissions/system/state/MediaPermission';
import {ensureMacPermission} from '@app/features/permissions/system/utils/MacPermissionGate';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	createMicTestAudioGraph,
	type MicTestAudioGraph,
} from '@app/features/user/components/modals/tabs/hooks/MicTestAudioGraph';
import {
	applyContentHintToTrack,
	resolveVoiceProcessing,
	type VoiceProcessingMode,
} from '@app/features/voice/utils/VoiceProcessingProfile';
import {
	boostedVoiceVolumePercentToTrackVolume,
	inputVoiceVolumePercentToGain,
} from '@app/features/voice/utils/VoiceVolumeUtils';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const logger = new Logger('useMicTest');

interface SinkableAudioContext extends AudioContext {
	setSinkId?: (sinkId: string | {type: 'none'}) => Promise<void>;
}

const TEST_AUDIO_CONTEXT_SAMPLE_RATE = 48000;
const MIC_TEST_MONITOR_DELAY_SECONDS = 0.9;

export interface MicTestSettings {
	inputDeviceId: string;
	outputDeviceId: string;
	inputVolume: number;
	outputVolume: number;
	echoCancellation: boolean;
	noiseSuppression: boolean;
	autoGainControl: boolean;
	deepFilterNoiseSuppression: boolean;
	deepFilterNoiseSuppressionLevel: number;
	voiceProcessingMode: VoiceProcessingMode;
}

function normalizeOutputDeviceId(deviceId: string): string {
	return deviceId === 'default' ? '' : deviceId;
}

export const useMicTest = (settings: MicTestSettings) => {
	const [isTesting, setIsTesting] = useState(false);
	const [isStarting, setIsStarting] = useState(false);
	const [level, setLevel] = useState(0);
	const [peakLevel, setPeakLevel] = useState(0);
	const audioContextRef = useRef<AudioContext | null>(null);
	const graphRef = useRef<MicTestAudioGraph | null>(null);
	const playbackDestinationRef = useRef<MediaStreamAudioDestinationNode | null>(null);
	const audioElementRef = useRef<HTMLAudioElement | null>(null);
	const micStreamRef = useRef<MediaStream | null>(null);
	const animationFrameRef = useRef<number | null>(null);
	const timeDomainDataRef = useRef<Float32Array<ArrayBuffer> | null>(null);
	const peakLevelRef = useRef(0);
	const isStartingRef = useRef(false);
	const restartPendingRef = useRef(false);
	const activeCaptureSignatureRef = useRef<string | null>(null);
	const micExplicitlyDenied = MediaPermission.microphoneExplicitlyDenied;
	const captureSignature = useMemo(
		() =>
			JSON.stringify({
				inputDeviceId: settings.inputDeviceId,
				outputDeviceId: settings.outputDeviceId,
				echoCancellation: settings.echoCancellation,
				noiseSuppression: settings.noiseSuppression,
				autoGainControl: settings.autoGainControl,
				deepFilterNoiseSuppression: settings.deepFilterNoiseSuppression,
				deepFilterNoiseSuppressionLevel: settings.deepFilterNoiseSuppressionLevel,
				voiceProcessingMode: settings.voiceProcessingMode,
			}),
		[
			settings.autoGainControl,
			settings.deepFilterNoiseSuppression,
			settings.deepFilterNoiseSuppressionLevel,
			settings.echoCancellation,
			settings.inputDeviceId,
			settings.noiseSuppression,
			settings.outputDeviceId,
			settings.voiceProcessingMode,
		],
	);
	const updateLevel = useCallback(() => {
		if (!graphRef.current || !timeDomainDataRef.current) {
			animationFrameRef.current = requestAnimationFrame(updateLevel);
			return;
		}
		graphRef.current.analyser.getFloatTimeDomainData(timeDomainDataRef.current);
		let sumOfSquares = 0;
		for (let i = 0; i < timeDomainDataRef.current.length; i++) {
			const sample = timeDomainDataRef.current[i];
			sumOfSquares += sample * sample;
		}
		const rms = Math.sqrt(sumOfSquares / timeDomainDataRef.current.length);
		const MIN_DB = -60;
		const MAX_DB = 0;
		const db = 20 * Math.log10(Math.max(rms, 1e-10));
		const normalized = Math.max(0, Math.min(1, (db - MIN_DB) / (MAX_DB - MIN_DB)));
		const nextPeak = Math.max(normalized, Math.max(0, peakLevelRef.current - 0.006));
		peakLevelRef.current = nextPeak;
		setLevel(normalized);
		setPeakLevel(nextPeak);
		animationFrameRef.current = requestAnimationFrame(updateLevel);
	}, []);
	const stop = useCallback(() => {
		if (animationFrameRef.current) {
			cancelAnimationFrame(animationFrameRef.current);
			animationFrameRef.current = null;
		}
		if (audioElementRef.current) {
			audioElementRef.current.pause();
			audioElementRef.current.srcObject = null;
			audioElementRef.current = null;
		}
		const graph = graphRef.current;
		graphRef.current = null;
		if (graph) {
			void graph.dispose().catch((error) => {
				logger.warn('Failed to dispose mic test graph', error);
			});
		}
		const playbackDestination = playbackDestinationRef.current;
		playbackDestinationRef.current = null;
		if (playbackDestination) {
			playbackDestination.disconnect();
			playbackDestination.stream.getTracks().forEach((track) => track.stop());
		}
		if (micStreamRef.current) {
			micStreamRef.current.getTracks().forEach((track) => track.stop());
			micStreamRef.current = null;
		}
		const audioContext = audioContextRef.current;
		audioContextRef.current = null;
		if (audioContext && audioContext.state !== 'closed') {
			void audioContext.close();
			audioContextRef.current = null;
		}
		timeDomainDataRef.current = null;
		peakLevelRef.current = 0;
		setIsTesting(false);
		setLevel(0);
		setPeakLevel(0);
	}, []);
	const start = useCallback(async () => {
		if (isStartingRef.current) return;
		if (micExplicitlyDenied) {
			handleMediaPermissionBlocked('microphone');
			return;
		}
		isStartingRef.current = true;
		setIsStarting(true);
		try {
			stop();
			const nativeResult = await ensureMacPermission('microphone', {behavior: 'interactive'});
			switch (nativeResult) {
				case 'granted':
				case 'unsupported-platform':
					break;
				case 'denied':
				case 'declined':
					MediaPermission.markMicrophoneExplicitlyDenied();
					handleMediaPermissionBlocked('microphone');
					return;
				default: {
					const exhaustive: never = nativeResult;
					return exhaustive;
				}
			}
			const profile = resolveVoiceProcessing(settings);
			const baseAudioConstraints: MediaTrackConstraints & {voiceIsolation?: boolean} = {
				echoCancellation: profile.echoCancellation,
				noiseSuppression: profile.browserNoiseSuppression,
				autoGainControl: profile.autoGainControl,
				voiceIsolation: false,
			};
			const useExactDeviceId = settings.inputDeviceId !== 'default';
			const buildAudioConstraints = (exact: boolean): MediaTrackConstraints =>
				useExactDeviceId
					? {
							...baseAudioConstraints,
							deviceId: exact ? {exact: settings.inputDeviceId} : {ideal: settings.inputDeviceId},
						}
					: baseAudioConstraints;
			let stream: MediaStream;
			try {
				stream = await navigator.mediaDevices.getUserMedia({audio: buildAudioConstraints(true)});
			} catch (error) {
				if (!useExactDeviceId || !(error instanceof Error) || error.name !== 'OverconstrainedError') {
					throw error;
				}
				stream = await navigator.mediaDevices.getUserMedia({audio: buildAudioConstraints(false)});
			}
			micStreamRef.current = stream;
			const sourceTrack = stream.getAudioTracks()[0];
			if (!sourceTrack) {
				throw new Error('getUserMedia returned no audio tracks for mic test');
			}
			applyContentHintToTrack(sourceTrack, profile.contentHint);
			const audioContext = new AudioContext({sampleRate: TEST_AUDIO_CONTEXT_SAMPLE_RATE});
			audioContextRef.current = audioContext;
			if (audioContext.state === 'suspended') {
				await audioContext.resume();
			}
			const outputSinkId = normalizeOutputDeviceId(settings.outputDeviceId);
			const playbackDestination = audioContext.createMediaStreamDestination();
			playbackDestinationRef.current = playbackDestination;
			let playbackTarget: AudioNode = playbackDestination;
			graphRef.current = await createMicTestAudioGraph({
				audioContext,
				sourceTrack,
				inputGain: inputVoiceVolumePercentToGain(settings.inputVolume),
				outputGain: boostedVoiceVolumePercentToTrackVolume(settings.outputVolume),
				playbackTarget,
				playbackDelaySeconds: MIC_TEST_MONITOR_DELAY_SECONDS,
				deepFilter: profile.deepFilter,
				deepFilterNoiseReductionLevel: profile.deepFilterNoiseReductionLevel,
			});
			timeDomainDataRef.current = new Float32Array(graphRef.current.analyser.fftSize);
			const audioElement = new Audio();
			audioElementRef.current = audioElement;
			audioElement.autoplay = true;
			audioElement.muted = false;
			audioElement.volume = 1;
			audioElement.srcObject = playbackDestination.stream;
			if (settings.outputDeviceId !== 'default' && typeof audioElement.setSinkId === 'function') {
				try {
					await audioElement.setSinkId(outputSinkId);
				} catch (error) {
					logger.warn('Failed to set mic test media element output device', error);
				}
			}
			try {
				await audioElement.play();
			} catch (error) {
				logger.warn('Failed to start mic test media element playback; falling back to AudioContext destination', error);
				audioElement.pause();
				audioElement.srcObject = null;
				audioElementRef.current = null;
				playbackDestination.disconnect();
				playbackDestination.stream.getTracks().forEach((track) => track.stop());
				playbackDestinationRef.current = null;
				graphRef.current.softClipOutput.disconnect();
				playbackTarget = audioContext.destination;
				const sinkableAudioContext = audioContext as SinkableAudioContext;
				if (settings.outputDeviceId !== 'default' && sinkableAudioContext.setSinkId) {
					try {
						await sinkableAudioContext.setSinkId(outputSinkId);
					} catch (sinkError) {
						logger.warn('Failed to set mic test AudioContext output device', sinkError);
					}
				}
				graphRef.current.softClipOutput.connect(playbackTarget);
				graphRef.current.playbackTarget = playbackTarget;
			}
			setIsTesting(true);
			activeCaptureSignatureRef.current = captureSignature;
			updateLevel();
			if (profile.deepFilter) {
				logger.info('Applied DeepFilterNet3 noise suppression for mic test');
			}
		} catch (error) {
			logger.error('Error starting mic test', error);
			if (error instanceof Error && (error.name === 'NotAllowedError' || error.name === 'PermissionDeniedError')) {
				MediaPermission.markMicrophoneExplicitlyDenied();
				handleMediaPermissionBlocked('microphone');
			}
			stop();
		} finally {
			isStartingRef.current = false;
			setIsStarting(false);
			if (restartPendingRef.current) {
				restartPendingRef.current = false;
				void start();
			}
		}
	}, [captureSignature, settings, updateLevel, stop, micExplicitlyDenied]);
	useEffect(() => {
		if (!isTesting) return;
		if (graphRef.current) {
			graphRef.current.inputGain.gain.value = inputVoiceVolumePercentToGain(settings.inputVolume);
			graphRef.current.outputGain.gain.value = boostedVoiceVolumePercentToTrackVolume(settings.outputVolume);
		}
	}, [isTesting, settings.inputVolume, settings.outputVolume]);
	useEffect(() => {
		if (!isTesting) {
			activeCaptureSignatureRef.current = captureSignature;
			return;
		}
		if (activeCaptureSignatureRef.current === captureSignature) {
			return;
		}
		activeCaptureSignatureRef.current = captureSignature;
		if (isStartingRef.current) {
			restartPendingRef.current = true;
			return;
		}
		void start();
	}, [captureSignature, isTesting, start]);
	useEffect(() => {
		return () => {
			stop();
		};
	}, [stop]);
	return {
		isTesting,
		isStarting,
		level,
		peakLevel,
		monitorDelayMs: Math.round(MIC_TEST_MONITOR_DELAY_SECONDS * 1000),
		start,
		stop,
	};
};
