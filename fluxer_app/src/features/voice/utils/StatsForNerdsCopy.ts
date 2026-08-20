// SPDX-License-Identifier: AGPL-3.0-or-later

import Config from '@app/features/app/config/Config';
import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/features/app/utils/LimitUtils';
import {
	getCachedDesktopTroubleshootingSettings,
	getDesktopTroubleshootingSettings,
} from '@app/features/devtools/utils/DesktopTroubleshootingUtils';
import {
	getElectronAPI,
	getNativePlatform,
	isDesktop,
	supportsDesktopScreenShareAudioCapture,
} from '@app/features/ui/utils/NativeUtils';
import AdaptiveScreenShareEngine from '@app/features/voice/engine/AdaptiveScreenShareEngine';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import ScreenShareCodecNegotiation, {
	getScreenShareCodecPreferenceOrder,
} from '@app/features/voice/engine/ScreenShareCodecNegotiation';
import {getScreenShareAudioPumpDiagnostics} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareAudioPump';
import {getNativeEngineAudioTrackPumpStats} from '@app/features/voice/engine/voice_screen_share_manager/NativeEngineAudioTrackPump';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	type CodecCapabilityReport,
	getCodecCapabilityReport,
	getLiveKitSupportedCodecs,
	selectOptimalCameraCodec,
	selectOptimalScreenShareCodec,
} from '@app/features/voice/utils/CodecCapabilityDetector';
import {loadGpuEncoderReport} from '@app/features/voice/utils/GpuEncoderCapabilities';
import {
	getNativeAudioBridgeStats,
	getNativeAudioCaptureDiagnosticState,
} from '@app/features/voice/utils/NativeAudioCaptureBridge';
import {getDisplayShareEnvironment} from '@app/features/voice/utils/ScreenShareEnvironment';
import {resolveStreamingModeSettings} from '@app/features/voice/utils/ScreenShareOptions';
import {
	buildVoiceStatsForNerdsPresentation,
	type StatsForNerdsData,
} from '@app/features/voice/utils/VoiceStatsForNerdsPresenter';
import type {NativeAudioApplication, VirtmicNode} from '@app/types/electron.d';

const SNAPSHOT_TIMEOUT_MS = 3000;
const MAX_INCLUDED_TARGETS = 200;

function safeError(error: unknown): Record<string, unknown> {
	if (error instanceof Error) {
		return {
			name: error.name,
			message: error.message,
			stack: error.stack ?? null,
		};
	}
	return {message: String(error)};
}

async function withTimeout<T>(
	label: string,
	task: Promise<T>,
	timeoutMs = SNAPSHOT_TIMEOUT_MS,
): Promise<T | {error: string}> {
	let timeoutId: number | null = null;
	const timeout = new Promise<{error: string}>((resolve) => {
		timeoutId = window.setTimeout(() => resolve({error: `${label} timed out after ${timeoutMs}ms`}), timeoutMs);
	});
	try {
		return await Promise.race([task, timeout]);
	} catch (error) {
		return {error: `${label} failed: ${safeError(error).message ?? 'unknown error'}`};
	} finally {
		if (timeoutId != null) {
			window.clearTimeout(timeoutId);
		}
	}
}

async function hashString(value: string | null | undefined): Promise<string | null> {
	if (!value) return null;
	try {
		const data = new TextEncoder().encode(value);
		const digest = await crypto.subtle.digest('SHA-256', data);
		return Array.from(new Uint8Array(digest))
			.map((byte) => byte.toString(16).padStart(2, '0'))
			.join('');
	} catch {
		let hash = 0;
		for (let index = 0; index < value.length; index += 1) {
			hash = (hash << 5) - hash + value.charCodeAt(index);
			hash |= 0;
		}
		return `fallback-${Math.abs(hash).toString(16)}`;
	}
}

async function queryPermission(name: string): Promise<string> {
	const permissions = navigator.permissions as
		| {query?: (descriptor: {name: string}) => Promise<PermissionStatus>}
		| undefined;
	if (!permissions?.query) return 'unsupported';
	try {
		return (await permissions.query({name})).state;
	} catch (error) {
		return `error:${safeError(error).message ?? 'unknown'}`;
	}
}

async function collectMediaDevices(): Promise<Record<string, unknown>> {
	if (!navigator.mediaDevices?.enumerateDevices) {
		return {supported: false};
	}
	try {
		const devices = await navigator.mediaDevices.enumerateDevices();
		return {
			supported: true,
			count: devices.length,
			devices: await Promise.all(
				devices.map(async (device) => ({
					kind: device.kind,
					label: device.label || '',
					hasLabel: Boolean(device.label),
					deviceIdHash: await hashString(device.deviceId),
					deviceIdLength: device.deviceId?.length ?? 0,
					groupIdHash: await hashString(device.groupId),
					groupIdLength: device.groupId?.length ?? 0,
					isDefaultDeviceId: device.deviceId === 'default',
				})),
			),
		};
	} catch (error) {
		return {supported: true, error: safeError(error)};
	}
}

function collectBrowserMetadata(): Record<string, unknown> {
	const userAgentData = (
		navigator as Navigator & {
			userAgentData?: {
				brands?: Array<{brand: string; version: string}>;
				mobile?: boolean;
				platform?: string;
			};
		}
	).userAgentData;
	const memory = (
		performance as Performance & {
			memory?: {
				jsHeapSizeLimit?: number;
				totalJSHeapSize?: number;
				usedJSHeapSize?: number;
			};
		}
	).memory;
	return {
		userAgent: navigator.userAgent,
		platform: navigator.platform,
		userAgentData: userAgentData
			? {
					brands: userAgentData.brands ?? [],
					mobile: userAgentData.mobile ?? null,
					platform: userAgentData.platform ?? null,
				}
			: null,
		language: navigator.language,
		languages: Array.from(navigator.languages ?? []),
		hardwareConcurrency: navigator.hardwareConcurrency ?? null,
		deviceMemory: (navigator as Navigator & {deviceMemory?: number}).deviceMemory ?? null,
		maxTouchPoints: navigator.maxTouchPoints ?? null,
		cookieEnabled: navigator.cookieEnabled,
		onLine: navigator.onLine,
		doNotTrack: navigator.doNotTrack,
		secureContext: window.isSecureContext,
		visibilityState: document.visibilityState,
		location: {
			origin: window.location.origin,
			pathname: window.location.pathname,
			hash: window.location.hash,
		},
		viewport: {
			width: window.innerWidth,
			height: window.innerHeight,
			devicePixelRatio: window.devicePixelRatio,
		},
		screen: {
			width: window.screen?.width ?? null,
			height: window.screen?.height ?? null,
			availWidth: window.screen?.availWidth ?? null,
			availHeight: window.screen?.availHeight ?? null,
			colorDepth: window.screen?.colorDepth ?? null,
			pixelDepth: window.screen?.pixelDepth ?? null,
		},
		performanceMemory: memory ?? null,
	};
}

function collectWebRtcMetadata(): Record<string, unknown> {
	return {
		mediaDevices: Boolean(navigator.mediaDevices),
		getUserMedia: Boolean(navigator.mediaDevices?.getUserMedia),
		getDisplayMedia: Boolean(navigator.mediaDevices?.getDisplayMedia),
		enumerateDevices: Boolean(navigator.mediaDevices?.enumerateDevices),
		audioContext: typeof AudioContext !== 'undefined',
		webkitAudioContext: typeof (window as Window & {webkitAudioContext?: unknown}).webkitAudioContext !== 'undefined',
		mediaStreamTrackGenerator:
			typeof (window as Window & {MediaStreamTrackGenerator?: unknown}).MediaStreamTrackGenerator !== 'undefined',
		audioData: typeof (window as Window & {AudioData?: unknown}).AudioData !== 'undefined',
		rtcPeerConnection: typeof RTCPeerConnection !== 'undefined',
	};
}

async function collectCodecMetadata(): Promise<Record<string, unknown>> {
	const cameraPreference = VoiceSettings.getPreferredVideoCodec();
	const screenSharePreference = VoiceSettings.getPreferredScreenShareCodec();
	let report: CodecCapabilityReport | null = null;
	let resolvedCameraCodec: string | null = null;
	let resolvedScreenShareCodec: string | null = null;
	let gpuReport: unknown = null;
	try {
		gpuReport = await withTimeout('GPU encoder report', loadGpuEncoderReport());
	} catch (error) {
		gpuReport = {error: safeError(error)};
	}
	try {
		report = getCodecCapabilityReport();
	} catch {
		report = null;
	}
	try {
		resolvedCameraCodec = selectOptimalCameraCodec(cameraPreference);
	} catch {
		resolvedCameraCodec = null;
	}
	try {
		resolvedScreenShareCodec = selectOptimalScreenShareCodec(screenSharePreference);
	} catch {
		resolvedScreenShareCodec = null;
	}
	return {
		liveKitSupported: getLiveKitSupportedCodecs(),
		preferences: {
			camera: cameraPreference,
			screenShare: screenSharePreference,
		},
		resolved: {
			camera: resolvedCameraCodec,
			screenShare: resolvedScreenShareCodec,
		},
		support: report,
		gpuHardwareEncode: gpuReport,
	};
}

async function collectVoiceSettingsMetadata(): Promise<Record<string, unknown>> {
	return {
		inputDeviceIdHash: await hashString(VoiceSettings.getInputDeviceId()),
		outputDeviceIdHash: await hashString(VoiceSettings.getOutputDeviceId()),
		videoDeviceIdHash: await hashString(VoiceSettings.getVideoDeviceId()),
		screenShareAudioDeviceIdHash: await hashString(VoiceSettings.getScreenShareAudioDeviceId()),
		effectiveScreenShareAudioDeviceIdHash: await hashString(VoiceSettings.getEffectiveScreenShareAudioDeviceId()),
		inputVolume: VoiceSettings.getInputVolume(),
		outputVolume: VoiceSettings.getOutputVolume(),
		echoCancellation: VoiceSettings.getEchoCancellation(),
		noiseSuppression: VoiceSettings.getNoiseSuppression(),
		autoGainControl: VoiceSettings.getAutoGainControl(),
		deepFilterNoiseSuppression: VoiceSettings.getDeepFilterNoiseSuppression(),
		deepFilterNoiseSuppressionLevel: VoiceSettings.getDeepFilterNoiseSuppressionLevel(),
		voiceProcessingMode: VoiceSettings.getVoiceProcessingMode(),
		vadThreshold: VoiceSettings.getVadThreshold(),
		vadAutoSensitivity: VoiceSettings.getVadAutoSensitivity(),
		vadEnhanced: VoiceSettings.getVadEnhanced(),
		cameraResolution: VoiceSettings.getCameraResolution(),
		screenshareResolution: VoiceSettings.getScreenshareResolution(),
		videoFrameRate: VoiceSettings.getVideoFrameRate(),
		streamingMode: VoiceSettings.getStreamingMode(),
		hideStreamPreview: VoiceSettings.getHideStreamPreview(),
		muteStreamAudio: VoiceSettings.getMuteStreamAudio(),
		shareAppAudio: VoiceSettings.getShareAppAudio(),
		shareDesktopAudio: VoiceSettings.getShareDesktopAudio(),
		shareDeviceAudio: VoiceSettings.getShareDeviceAudio(),
		screenShareAudioSourceMode: VoiceSettings.getScreenShareAudioSourceMode(),
		screenShareAudioIncludeSources: VoiceSettings.getScreenShareAudioIncludeSources(),
		screenShareAudioExcludeSources: VoiceSettings.getScreenShareAudioExcludeSources(),
		preferredVideoCodec: VoiceSettings.getPreferredVideoCodec(),
		preferredScreenShareCodec: VoiceSettings.getPreferredScreenShareCodec(),
		screenShareContentHint: VoiceSettings.getScreenShareContentHint(),
		screenShareEncoderMode: VoiceSettings.getScreenShareEncoderMode(),
		screenShareSoftwareQuality: VoiceSettings.getScreenShareSoftwareQuality(),
		screenShareScalabilityMode: VoiceSettings.getScreenShareScalabilityMode(),
		screenShareBackupCodecMode: VoiceSettings.getScreenShareBackupCodecMode(),
		screenShareMaxBitrateMbps: VoiceSettings.getScreenShareMaxBitrateMbps(),
		adaptiveScreenShareQuality: VoiceSettings.getAdaptiveScreenShareQuality(),
		openH264Enabled: VoiceSettings.getOpenH264Enabled(),
		linuxAudioCapture: {
			workaround: VoiceSettings.getLinuxAudioCaptureWorkaround(),
			onlySpeakers: VoiceSettings.getLinuxAudioCaptureOnlySpeakers(),
			onlyDefaultSpeakers: VoiceSettings.getLinuxAudioCaptureOnlyDefaultSpeakers(),
			ignoreInputMedia: VoiceSettings.getLinuxAudioCaptureIgnoreInputMedia(),
			ignoreVirtual: VoiceSettings.getLinuxAudioCaptureIgnoreVirtual(),
			ignoreDevices: VoiceSettings.getLinuxAudioCaptureIgnoreDevices(),
			granularSelect: VoiceSettings.getLinuxAudioCaptureGranularSelect(),
			deviceSelect: VoiceSettings.getLinuxAudioCaptureDeviceSelect(),
		},
	};
}

function summarizeRoom(): Record<string, unknown> {
	const room = MediaEngine.room;
	const participant = room?.localParticipant as
		| {
				identity?: string;
				sid?: string;
				isMicrophoneEnabled?: boolean;
				isCameraEnabled?: boolean;
				isScreenShareEnabled?: boolean;
				audioTrackPublications?: Map<unknown, unknown>;
				videoTrackPublications?: Map<unknown, unknown>;
		  }
		| null
		| undefined;
	return {
		connected: MediaEngine.connected,
		connecting: MediaEngine.connecting,
		guildId: MediaEngine.guildId,
		channelId: MediaEngine.channelId,
		connectionId: MediaEngine.connectionId,
		voiceServerEndpointPresent: Boolean(MediaEngine.voiceServerEndpoint),
		roomState: room?.state ?? null,
		localParticipant: participant
			? {
					identity: participant.identity ?? null,
					sid: participant.sid ?? null,
					isMicrophoneEnabled: participant.isMicrophoneEnabled ?? null,
					isCameraEnabled: participant.isCameraEnabled ?? null,
					isScreenShareEnabled: participant.isScreenShareEnabled ?? null,
					audioPublicationCount: participant.audioTrackPublications?.size ?? null,
					videoPublicationCount: participant.videoTrackPublications?.size ?? null,
				}
			: null,
		voiceStats: MediaEngine.voiceStats,
		perTrackStats: MediaEngine.perTrackStats,
		statsTimeSeries: MediaEngine.statsTimeSeries,
		transport: {
			publisher: MediaEngine.publisherTransport,
			subscriber: MediaEngine.subscriberTransport,
		},
	};
}

function summarizeVirtmicNode(node: VirtmicNode): Record<string, string> {
	const result: Record<string, string> = {};
	for (const [key, value] of Object.entries(node)) {
		result[key] = value;
	}
	return result;
}

function summarizeNativeApp(app: NativeAudioApplication): Record<string, unknown> {
	return {
		pid: app.pid,
		identifier: app.identifier,
		name: app.name,
		audible: app.audible ?? null,
	};
}

async function collectDesktopMetadata(): Promise<Record<string, unknown>> {
	const electronApi = getElectronAPI();
	if (!electronApi) {
		return {isDesktop: false};
	}
	const electronRecord = electronApi as typeof electronApi & {
		isWaylandSession?: () => Promise<boolean>;
		getWaylandCapabilities?: () => Promise<unknown>;
	};
	const [
		desktopInfo,
		waylandSession,
		waylandCapabilities,
		virtmicAvailability,
		virtmicTargets,
		nativeAvailability,
		nativeApps,
		troubleshooting,
		streamingPriority,
		appMetrics,
		gpuInfo,
	] = await Promise.all([
		withTimeout('desktop info', electronApi.getDesktopInfo()),
		electronRecord.isWaylandSession
			? withTimeout('wayland session', electronRecord.isWaylandSession())
			: Promise.resolve({error: 'isWaylandSession unavailable'}),
		electronRecord.getWaylandCapabilities
			? withTimeout('wayland capabilities', electronRecord.getWaylandCapabilities())
			: Promise.resolve({error: 'getWaylandCapabilities unavailable'}),
		electronApi.virtmic
			? withTimeout('virtmic availability', electronApi.virtmic.getAvailability())
			: Promise.resolve({error: 'virtmic API unavailable'}),
		electronApi.virtmic
			? withTimeout('virtmic targets', electronApi.virtmic.listTargets({granular: true}))
			: Promise.resolve({error: 'virtmic API unavailable'}),
		electronApi.nativeAudio
			? withTimeout('native audio availability', electronApi.nativeAudio.getAvailability())
			: Promise.resolve({error: 'nativeAudio API unavailable'}),
		electronApi.nativeAudio
			? withTimeout('native audio apps', electronApi.nativeAudio.listAudibleApplications())
			: Promise.resolve({error: 'nativeAudio API unavailable'}),
		(() => {
			const cached = getCachedDesktopTroubleshootingSettings();
			if (cached) return Promise.resolve(cached);
			return withTimeout('desktop troubleshooting', getDesktopTroubleshootingSettings());
		})(),
		electronApi.getStreamingPriorityDiagnostics
			? withTimeout('streaming priority diagnostics', electronApi.getStreamingPriorityDiagnostics())
			: Promise.resolve({error: 'getStreamingPriorityDiagnostics unavailable'}),
		electronApi.getAppMetrics
			? withTimeout('app metrics', electronApi.getAppMetrics())
			: Promise.resolve({error: 'getAppMetrics unavailable'}),
		electronApi.getGpuInfo
			? withTimeout('GPU info', electronApi.getGpuInfo())
			: Promise.resolve({error: 'getGpuInfo unavailable'}),
	]);
	const virtmicTargetSummary =
		virtmicTargets && typeof virtmicTargets === 'object' && 'targets' in virtmicTargets
			? {
					...virtmicTargets,
					targets: Array.isArray(virtmicTargets.targets)
						? virtmicTargets.targets.slice(0, MAX_INCLUDED_TARGETS).map(summarizeVirtmicNode)
						: virtmicTargets.targets,
					truncated: Array.isArray(virtmicTargets.targets) && virtmicTargets.targets.length > MAX_INCLUDED_TARGETS,
				}
			: virtmicTargets;
	return {
		isDesktop: true,
		platform: electronApi.platform,
		desktopInfo,
		waylandSession,
		waylandCapabilities,
		troubleshooting,
		streamingPriority,
		appMetrics,
		gpuInfo,
		virtmic: {
			availability: virtmicAvailability,
			targets: virtmicTargetSummary,
		},
		nativeAudio: {
			availability: nativeAvailability,
			audibleApplications: Array.isArray(nativeApps) ? nativeApps.map(summarizeNativeApp) : nativeApps,
			bridgeStats: getNativeAudioBridgeStats(),
			engineAudioPumpStats: getNativeEngineAudioTrackPumpStats(),
			captureState: getNativeAudioCaptureDiagnosticState(),
		},
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

export function collectStatsForNerdsSnapshot(): StatsForNerdsData {
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
			electronVersion: null,
			chromiumVersion: null,
			hardwareAccelerationEnabled: null,
			chromiumRuntime: null,
		},
		gpu: null,
		appMetrics: null,
		system: {
			platform: navigator.platform,
			userAgent: navigator.userAgent.replace(/^Mozilla\/5\.0\s*/, ''),
			hardwareConcurrency: navigator.hardwareConcurrency,
			deviceMemoryGB: (navigator as Navigator & {deviceMemory?: number}).deviceMemory ?? null,
			jsHeapUsedMB: null,
			jsHeapTotalMB: null,
			jsHeapLimitMB: null,
		},
		heapHistory: [],
		cpuHistory: [],
	};
}

export async function buildStatsForNerdsCopyPayload(data: StatsForNerdsData): Promise<Record<string, unknown>> {
	const [nativePlatform, displayShareEnvironment, permissions, mediaDevices, voiceSettings, desktop, codecs] =
		await Promise.all([
			getNativePlatform(),
			getDisplayShareEnvironment().catch((error) => `error:${safeError(error).message ?? 'unknown'}`),
			Promise.all([
				queryPermission('microphone'),
				queryPermission('camera'),
				queryPermission('display-capture'),
				queryPermission('speaker-selection'),
			]),
			collectMediaDevices(),
			collectVoiceSettingsMetadata(),
			collectDesktopMetadata(),
			collectCodecMetadata(),
		]);
	const [microphone, camera, displayCapture, speakerSelection] = permissions;
	return {
		schemaVersion: 1,
		createdAt: new Date().toISOString(),
		statsForNerds: data,
		app: {
			nativePlatform,
			displayShareEnvironment,
			isDesktop: isDesktop(),
			supportsDesktopScreenShareAudioCapture: supportsDesktopScreenShareAudioCapture(),
		},
		browser: collectBrowserMetadata(),
		webrtc: collectWebRtcMetadata(),
		codecs,
		permissions: {
			microphone,
			camera,
			displayCapture,
			speakerSelection,
		},
		mediaDevices,
		voiceSettings,
		voiceSession: summarizeRoom(),
		desktop,
	};
}
