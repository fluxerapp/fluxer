// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import Config from '@app/features/app/config/Config';
import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/features/app/utils/LimitUtils';
import {getCachedDesktopTroubleshootingSettings} from '@app/features/devtools/utils/DesktopTroubleshootingUtils';
import type {AppMetricsSnapshot, DesktopInfo, GpuInfo} from '@app/features/platform/types/Electron';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import AdaptiveScreenShareEngine from '@app/features/voice/engine/AdaptiveScreenShareEngine';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import ScreenShareCodecNegotiation, {
	getScreenShareCodecPreferenceOrder,
} from '@app/features/voice/engine/ScreenShareCodecNegotiation';
import {getScreenShareAudioPumpDiagnostics} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareAudioPump';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {getNativeAudioCaptureDiagnosticState} from '@app/features/voice/utils/NativeAudioCaptureBridge';
import {resolveStreamingModeSettings} from '@app/features/voice/utils/ScreenShareOptions';
import {
	buildVoiceStatsForNerdsPresentation,
	type StatsForNerdsData,
} from '@app/features/voice/utils/VoiceStatsForNerdsPresenter';
import type {VoiceEngineV2PerTrackStats, VoiceEngineV2TransportInfo} from '@fluxer/voice_engine_v2';
import {useEffect, useRef, useState} from 'react';

export type {StatsForNerdsData} from '@app/features/voice/utils/VoiceStatsForNerdsPresenter';

const STATS_FOR_NERDS_POLL_INTERVAL_MS = 2000;
const MAX_STATS_HISTORY_SAMPLES = 60;

export interface UseStatsForNerdsOptions {
	enabled?: boolean;
}

class BoundedNumberHistory {
	private readonly samples: Array<number>;
	private readonly capacity: number;
	private head = 0;
	private count = 0;

	constructor(capacity: number) {
		assert.ok(capacity > 0, 'stats history capacity must be positive');
		this.capacity = capacity;
		this.samples = new Array(capacity).fill(0);
	}

	push(value: number): void {
		assert.ok(Number.isFinite(value), 'stats history sample must be finite');
		if (this.count < this.capacity) {
			this.samples[(this.head + this.count) % this.capacity] = value;
			this.count += 1;
			return;
		}
		this.samples[this.head] = value;
		this.head = (this.head + 1) % this.capacity;
	}

	toArray(): Array<number> {
		const out = new Array<number>(this.count);
		for (let index = 0; index < this.count; index += 1) {
			out[index] = this.samples[(this.head + index) % this.capacity] ?? 0;
		}
		return out;
	}
}

function formatTransportSummary(transport: VoiceEngineV2TransportInfo | null): string {
	if (!transport) return 'n/a';
	const parts: Array<string> = [];
	if (transport.localProtocol) parts.push(transport.localProtocol.toUpperCase());
	if (transport.localCandidateType) parts.push(transport.localCandidateType);
	if (transport.iceState) parts.push(`ICE:${transport.iceState}`);
	if (transport.dtlsState) parts.push(`DTLS:${transport.dtlsState}`);
	return parts.join(' / ') || 'n/a';
}

export function formatResolution(track: VoiceEngineV2PerTrackStats | null): string {
	if (!track || !track.frameWidth || !track.frameHeight) return 'n/a';
	return `${track.frameWidth}x${track.frameHeight}`;
}

export function formatCodec(track: VoiceEngineV2PerTrackStats | null): string {
	if (!track?.codec) return 'n/a';
	return track.codec.replace(/^(audio|video)\//, '');
}

export function formatTransport(transport: VoiceEngineV2TransportInfo | null): string {
	return formatTransportSummary(transport);
}

function getSystemInfo(): StatsForNerdsData['system'] {
	const nav = navigator as Navigator & {deviceMemory?: number};
	const mem = (
		performance as Performance & {memory?: {usedJSHeapSize: number; totalJSHeapSize: number; jsHeapSizeLimit: number}}
	).memory;
	return {
		platform: navigator.platform,
		userAgent: navigator.userAgent.replace(/^Mozilla\/5\.0\s*/, ''),
		hardwareConcurrency: navigator.hardwareConcurrency,
		deviceMemoryGB: nav.deviceMemory ?? null,
		jsHeapUsedMB: mem ? Math.round((mem.usedJSHeapSize / 1048576) * 10) / 10 : null,
		jsHeapTotalMB: mem ? Math.round((mem.totalJSHeapSize / 1048576) * 10) / 10 : null,
		jsHeapLimitMB: mem ? Math.round((mem.jsHeapSizeLimit / 1048576) * 10) / 10 : null,
	};
}

function hasHigherVideoQuality(): boolean {
	return isLimitToggleEnabled(
		{
			feature_higher_video_quality: LimitResolver.resolve({
				key: 'feature_higher_video_quality',
				fallback: 0,
			}),
		},
		'feature_higher_video_quality',
	);
}

export function useStatsForNerds({enabled = true}: UseStatsForNerdsOptions = {}): StatsForNerdsData {
	useMediaEngineVersion();
	const [gpuInfo, setGpuInfo] = useState<GpuInfo | null>(null);
	const [appMetrics, setAppMetrics] = useState<AppMetricsSnapshot | null>(null);
	const [desktopInfo, setDesktopInfo] = useState<DesktopInfo | null>(null);
	const heapHistoryRef = useRef<BoundedNumberHistory | null>(null);
	const cpuHistoryRef = useRef<BoundedNumberHistory | null>(null);
	if (!heapHistoryRef.current) heapHistoryRef.current = new BoundedNumberHistory(MAX_STATS_HISTORY_SAMPLES);
	if (!cpuHistoryRef.current) cpuHistoryRef.current = new BoundedNumberHistory(MAX_STATS_HISTORY_SAMPLES);
	useEffect(() => {
		if (!enabled) return;
		let cancelled = false;
		const electronAPI = getElectronAPI();
		if (electronAPI?.getGpuInfo) {
			void electronAPI.getGpuInfo().then((info) => {
				if (!cancelled) setGpuInfo(info);
			});
		}
		if (electronAPI?.getDesktopInfo) {
			void electronAPI.getDesktopInfo().then((info) => {
				if (!cancelled) setDesktopInfo(info);
			});
		}
		return () => {
			cancelled = true;
		};
	}, [enabled]);
	useEffect(() => {
		if (!enabled) return;
		let cancelled = false;
		const electronAPI = getElectronAPI();
		const mem = (performance as Performance & {memory?: {usedJSHeapSize: number}}).memory;
		if (!electronAPI?.getAppMetrics && !mem) return;
		const poll = (): void => {
			if (mem) {
				heapHistoryRef.current?.push(Math.round((mem.usedJSHeapSize / 1048576) * 10) / 10);
			}
			if (!electronAPI?.getAppMetrics) return;
			void electronAPI.getAppMetrics().then((metrics) => {
				if (cancelled) return;
				setAppMetrics(metrics);
				const mainProcess = metrics.processes.find((p) => p.type === 'Browser');
				if (!mainProcess) return;
				cpuHistoryRef.current?.push(mainProcess.cpu.percentCPUUsage);
			});
		};
		poll();
		const id = setInterval(poll, STATS_FOR_NERDS_POLL_INTERVAL_MS);
		return () => {
			cancelled = true;
			clearInterval(id);
		};
	}, [enabled]);
	const adaptiveQualitySnapshot = AdaptiveScreenShareEngine.qualitySnapshot;
	const effectiveScreenShareSettings = resolveStreamingModeSettings(
		VoiceSettings.getStreamingMode(),
		VoiceSettings.getScreenshareResolution(),
		VoiceSettings.getVideoFrameRate(),
		hasHigherVideoQuality(),
	);
	const localParticipant = MediaEngine.room?.localParticipant ?? null;
	const voicePresentation = buildVoiceStatsForNerdsPresentation({
		connectionId: MediaEngine.connectionId,
		connectionQuality: MediaEngine.localConnectionQuality,
		currentLatency: MediaEngine.currentLatency,
		averageLatency: MediaEngine.averageLatency,
		stats: MediaEngine.voiceStats,
		perTrackStats: MediaEngine.perTrackStats,
		statsTimeSeries: MediaEngine.statsTimeSeries,
		nativeStats: null,
		publisherTransport: MediaEngine.publisherTransport,
		subscriberTransport: MediaEngine.subscriberTransport,
		localParticipant,
		remoteParticipants: MediaEngine.room?.remoteParticipants.values() ?? null,
	});
	return {
		...voicePresentation,
		connection: {
			voiceServerEndpoint: MediaEngine.voiceServerEndpoint ?? 'n/a',
			reconnectionCount: MediaEngine.reconnectionCount,
		},
		audio: {
			echoCancellation: VoiceSettings.echoCancellation,
			noiseSuppression: VoiceSettings.noiseSuppression,
			autoGainControl: VoiceSettings.autoGainControl,
			deepFilterNoiseSuppression: VoiceSettings.deepFilterNoiseSuppression,
			deepFilterNoiseSuppressionLevel: VoiceSettings.deepFilterNoiseSuppressionLevel,
			processingMode: VoiceSettings.voiceProcessingMode,
		},
		screenShareSettings: {
			resolution: effectiveScreenShareSettings.resolution,
			frameRate: effectiveScreenShareSettings.frameRate,
			streamingMode: VoiceSettings.getStreamingMode(),
			preferredCodec: VoiceSettings.getPreferredScreenShareCodec(),
			selectedCodec: ScreenShareCodecNegotiation.getSelectedCodec(),
			codecPreferenceOrder: [...getScreenShareCodecPreferenceOrder()],
			contentHint: VoiceSettings.getScreenShareContentHint(),
			encoderMode: VoiceSettings.getScreenShareEncoderMode(),
			softwareQuality: VoiceSettings.getScreenShareSoftwareQuality(),
			scalabilityMode: VoiceSettings.getScreenShareScalabilityMode(),
			backupCodecMode: VoiceSettings.getScreenShareBackupCodecMode(),
			maxBitrateMbps: VoiceSettings.getScreenShareMaxBitrateMbps(),
			adaptiveQuality: VoiceSettings.getAdaptiveScreenShareQuality(),
			adaptiveQualityAdapted: adaptiveQualitySnapshot.isAdapted,
			adaptiveQualityConfiguredResolution: adaptiveQualitySnapshot.configuredResolution,
			adaptiveQualityConfiguredFrameRate: adaptiveQualitySnapshot.configuredFrameRate,
			adaptiveQualityEffectiveResolution: adaptiveQualitySnapshot.effectiveResolution,
			adaptiveQualityEffectiveFrameRate: adaptiveQualitySnapshot.effectiveFrameRate,
			adaptiveQualityLimitationReason: adaptiveQualitySnapshot.limitationReason,
			audioSourceMode: VoiceSettings.getScreenShareAudioSourceMode(),
			audioIncludeSources: VoiceSettings.getScreenShareAudioIncludeSources(),
			audioExcludeSources: VoiceSettings.getScreenShareAudioExcludeSources(),
			shareDesktopAudio: VoiceSettings.getShareDesktopAudio(),
			shareAppAudio: VoiceSettings.getShareAppAudio(),
			muteStreamAudio: VoiceSettings.getMuteStreamAudio(),
			openH264Enabled: VoiceSettings.getOpenH264Enabled(),
		},
		screenShareAudioCapture: {
			pump: getScreenShareAudioPumpDiagnostics(),
			nativeCapture: getNativeAudioCaptureDiagnosticState(),
		},
		appInfo: {
			appVersion: Config.PUBLIC_BUILD_VERSION ?? 'dev',
			electronVersion: desktopInfo?.electronVersion ?? null,
			chromiumVersion: desktopInfo?.chromeVersion ?? null,
			hardwareAccelerationEnabled:
				desktopInfo != null ? getCachedDesktopTroubleshootingSettings()?.disableHardwareAcceleration !== true : null,
			chromiumRuntime: desktopInfo?.chromiumRuntime ?? null,
		},
		gpu: gpuInfo,
		appMetrics,
		system: getSystemInfo(),
		heapHistory: heapHistoryRef.current.toArray(),
		cpuHistory: cpuHistoryRef.current.toArray(),
	};
}
