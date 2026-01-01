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

import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {MicrophonePermissionDeniedModal} from '~/components/alerts/MicrophonePermissionDeniedModal';
import MediaPermissionStore from '~/stores/MediaPermissionStore';
import {ensureNativePermission} from '~/utils/NativePermissions';
import {isDesktop} from '~/utils/NativeUtils';

interface MicTestSettings {
	inputDeviceId: string;
	outputDeviceId: string;
	inputVolume: number;
	outputVolume: number;
	echoCancellation: boolean;
	noiseSuppression: boolean;
	autoGainControl: boolean;
}

export const useMicTest = (settings: MicTestSettings) => {
	const [isTesting, setIsTesting] = React.useState(false);
	const [micLevel, setMicLevel] = React.useState(-Infinity);
	const [peakLevel, setPeakLevel] = React.useState(-Infinity);

	const audioContextRef = React.useRef<AudioContext | null>(null);
	const analyserRef = React.useRef<AnalyserNode | null>(null);
	const gainNodeRef = React.useRef<GainNode | null>(null);
	const destinationRef = React.useRef<MediaStreamAudioDestinationNode | null>(null);
	const audioElementRef = React.useRef<HTMLAudioElement | null>(null);
	const micStreamRef = React.useRef<MediaStream | null>(null);
	const animationFrameRef = React.useRef<number | null>(null);
	const sourceRef = React.useRef<MediaStreamAudioSourceNode | null>(null);
	const canUseFloatTimeDomainRef = React.useRef<boolean>(false);
	const floatSampleBufferRef = React.useRef<Float32Array | null>(null);
	const byteSampleBufferRef = React.useRef<Uint8Array | null>(null);
	const peakDecayRef = React.useRef<number>(-Infinity);
	const peakHoldTimeRef = React.useRef<number>(0);

	const micExplicitlyDenied = MediaPermissionStore.microphoneExplicitlyDenied;

	const calculateLevel = React.useCallback(() => {
		if (!analyserRef.current) return -Infinity;

		const analyser = analyserRef.current;
		const canUseFloat = canUseFloatTimeDomainRef.current;
		const floatBuffer = floatSampleBufferRef.current;
		const byteBuffer = byteSampleBufferRef.current;

		let peakInstantaneousPower = 0;
		let sumOfSquares = 0;
		let sampleCount = 0;

		if (canUseFloat && floatBuffer) {
			analyser.getFloatTimeDomainData(floatBuffer as any);
			for (let i = 0; i < floatBuffer.length; i++) {
				const sample = floatBuffer[i];
				const power = sample * sample;
				peakInstantaneousPower = Math.max(power, peakInstantaneousPower);
				sumOfSquares += power;
				sampleCount++;
			}
		} else if (byteBuffer) {
			analyser.getByteTimeDomainData(byteBuffer as any);
			for (let i = 0; i < byteBuffer.length; i++) {
				const sample = (byteBuffer[i] - 128) / 128;
				const power = sample * sample;
				peakInstantaneousPower = Math.max(power, peakInstantaneousPower);
				sumOfSquares += power;
				sampleCount++;
			}
		}

		const averagePower = sampleCount > 0 ? sumOfSquares / sampleCount : 0;
		const rmsDb = averagePower > 0 ? 10 * Math.log10(averagePower) : -Infinity;
		const peakDb = peakInstantaneousPower > 0 ? 10 * Math.log10(peakInstantaneousPower) : -Infinity;

		const now = Date.now();
		if (peakDb > peakDecayRef.current) {
			peakDecayRef.current = peakDb;
			peakHoldTimeRef.current = now;
		} else if (now - peakHoldTimeRef.current > 1000) {
			peakDecayRef.current = Math.max(peakDb, peakDecayRef.current - 0.5);
		}

		return rmsDb;
	}, []);

	const drawLoop = React.useCallback(() => {
		const level = calculateLevel();
		setMicLevel(level);
		setPeakLevel(peakDecayRef.current);
		animationFrameRef.current = requestAnimationFrame(drawLoop);
	}, [calculateLevel]);

	const stop = React.useCallback(() => {
		if (animationFrameRef.current) {
			cancelAnimationFrame(animationFrameRef.current);
			animationFrameRef.current = null;
		}

		if (audioElementRef.current) {
			audioElementRef.current.pause();
			audioElementRef.current.srcObject = null;
			audioElementRef.current = null;
		}

		if (sourceRef.current) {
			sourceRef.current.disconnect();
			sourceRef.current = null;
		}

		if (analyserRef.current) {
			analyserRef.current.disconnect();
			analyserRef.current = null;
		}

		if (gainNodeRef.current) {
			gainNodeRef.current.disconnect();
			gainNodeRef.current = null;
		}

		if (destinationRef.current) {
			destinationRef.current.disconnect();
			destinationRef.current = null;
		}

		if (micStreamRef.current) {
			micStreamRef.current.getTracks().forEach((track) => track.stop());
			micStreamRef.current = null;
		}

		if (audioContextRef.current && audioContextRef.current.state !== 'closed') {
			void audioContextRef.current.close();
			audioContextRef.current = null;
		}

		floatSampleBufferRef.current = null;
		byteSampleBufferRef.current = null;
		peakDecayRef.current = -Infinity;
		peakHoldTimeRef.current = 0;

		setIsTesting(false);
		setMicLevel(-Infinity);
		setPeakLevel(-Infinity);
	}, []);

	const start = React.useCallback(async () => {
		if (micExplicitlyDenied) {
			ModalActionCreators.push(modal(() => <MicrophonePermissionDeniedModal />));
			return;
		}

		try {
			stop();
			if (isDesktop()) {
				const nativeResult = await ensureNativePermission('microphone');
				if (nativeResult === 'denied') {
					MediaPermissionStore.markMicrophoneExplicitlyDenied();
					ModalActionCreators.push(modal(() => <MicrophonePermissionDeniedModal />));
					return;
				}
			}

			const stream = await navigator.mediaDevices.getUserMedia({
				audio: {
					deviceId: settings.inputDeviceId !== 'default' ? {ideal: settings.inputDeviceId} : undefined,
					echoCancellation: settings.echoCancellation,
					noiseSuppression: settings.noiseSuppression,
					autoGainControl: settings.autoGainControl,
				},
			});

			micStreamRef.current = stream;

			const AudioContextClass = window.AudioContext || (window as any).webkitAudioContext;
			audioContextRef.current = new AudioContextClass();

			if (audioContextRef.current.state === 'suspended') {
				await audioContextRef.current.resume();
			}

			analyserRef.current = audioContextRef.current.createAnalyser();
			analyserRef.current.fftSize = 2048;
			analyserRef.current.smoothingTimeConstant = 0.3;

			canUseFloatTimeDomainRef.current = typeof analyserRef.current.getFloatTimeDomainData === 'function';

			if (canUseFloatTimeDomainRef.current) {
				floatSampleBufferRef.current = new Float32Array(analyserRef.current.fftSize);
			} else {
				byteSampleBufferRef.current = new Uint8Array(analyserRef.current.fftSize);
			}

			sourceRef.current = audioContextRef.current.createMediaStreamSource(stream);
			gainNodeRef.current = audioContextRef.current.createGain();
			gainNodeRef.current.gain.value = settings.inputVolume / 100;
			destinationRef.current = audioContextRef.current.createMediaStreamDestination();

			sourceRef.current.connect(analyserRef.current);
			analyserRef.current.connect(gainNodeRef.current);
			gainNodeRef.current.connect(destinationRef.current);
			gainNodeRef.current.connect(audioContextRef.current.destination);

			audioElementRef.current = new Audio();
			audioElementRef.current.srcObject = destinationRef.current.stream;

			if (settings.outputDeviceId !== 'default' && 'setSinkId' in audioElementRef.current) {
				try {
					await (audioElementRef.current as any).setSinkId(settings.outputDeviceId);
				} catch (error) {
					console.warn('Failed to set output device:', error);
				}
			}

			audioElementRef.current.volume = settings.outputVolume / 100;
			await audioElementRef.current.play();

			setIsTesting(true);
			drawLoop();
		} catch (error) {
			console.error('Error starting mic test:', error);

			if (error instanceof Error && (error.name === 'NotAllowedError' || error.name === 'PermissionDeniedError')) {
				MediaPermissionStore.markMicrophoneExplicitlyDenied();
				ModalActionCreators.push(modal(() => <MicrophonePermissionDeniedModal />));
			}

			stop();
		}
	}, [settings, drawLoop, stop, micExplicitlyDenied]);

	React.useEffect(() => {
		return () => {
			stop();
		};
	}, [stop]);

	return {
		isTesting,
		micLevel,
		peakLevel,
		start,
		stop,
	};
};
