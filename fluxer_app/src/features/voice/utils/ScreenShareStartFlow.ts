// SPDX-License-Identifier: AGPL-3.0-or-later

import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/features/app/utils/LimitUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI, supportsDesktopScreenShareAudioCapture} from '@app/features/ui/utils/NativeUtils';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import ScreenShareCodecNegotiation from '@app/features/voice/engine/ScreenShareCodecNegotiation';
import ActiveScreenShareSource from '@app/features/voice/state/ActiveScreenShareSource';
import {clearDesktopSourceIntent, setDesktopSourceIntent} from '@app/features/voice/state/DesktopSourceIntent';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	resolveScreenShareContentHintForContext,
	type ScreenShareContentSource,
} from '@app/features/voice/utils/CodecCapabilityDetector';
import {
	LINUX_AUDIO_TARGET_OBJECTS_PATTERN_KEY,
	toNativeLinuxAudioPatterns,
} from '@app/features/voice/utils/LinuxAudioSourceRules';
import {disarmVirtmic} from '@app/features/voice/utils/LinuxScreenShareAudio';
import {
	armNativeAudioForLinuxRouting,
	armNativeAudioForNextCapture,
	armNativeSystemAudioForNextCapture,
	disarmNativeAudio,
	disarmPendingNativeAudio,
	getLastNativeAudioArmFailure,
} from '@app/features/voice/utils/NativeAudioCaptureBridge';
import type {ScreenShareAudioCaptureDebugInfo} from '@app/features/voice/utils/ScreenShareAudioCaptureError';
import {
	type DisplayShareEnvironment,
	getDisplayShareEnvironment,
	usesNativeDisplayShareAudioSelection,
} from '@app/features/voice/utils/ScreenShareEnvironment';
import {
	buildScreenShareOptions,
	normaliseResolutionForContext,
	normaliseStreamingModeForContext,
	resolveStreamingModeSettings,
	type ScreenShareContext,
} from '@app/features/voice/utils/ScreenShareOptions';
import {
	isScreenSharePortalUnavailableError,
	ScreenSharePortalUnavailableError,
} from '@app/features/voice/utils/ScreenSharePortalUnavailableError';
import {executeScreenShareOperation} from '@app/features/voice/utils/ScreenShareUtils';
import type {NativeAudioStartOptions, VirtmicNode} from '@app/types/electron.d';
import type {ScreenShareCaptureOptions, VideoCodec} from 'livekit-client';

const logger = new Logger('ScreenShareStartFlow');

type LinuxNativeAudioRule = NonNullable<NativeAudioStartOptions['linuxRule']>;

interface LinuxAudioLinkOptions {
	workaround: boolean;
	ignoreInputMedia: boolean;
	ignoreVirtual: boolean;
	ignoreDevices: boolean;
}

function getLinkOptions(): LinuxAudioLinkOptions {
	return {
		workaround: VoiceSettings.getLinuxAudioCaptureWorkaround(),
		ignoreInputMedia: VoiceSettings.getLinuxAudioCaptureIgnoreInputMedia(),
		ignoreVirtual: VoiceSettings.getLinuxAudioCaptureIgnoreVirtual(),
		ignoreDevices: VoiceSettings.getLinuxAudioCaptureIgnoreDevices(),
	};
}

function getSystemOptions() {
	return {
		...getLinkOptions(),
		onlySpeakers: VoiceSettings.getLinuxAudioCaptureOnlySpeakers(),
		onlyDefaultSpeakers: VoiceSettings.getLinuxAudioCaptureOnlyDefaultSpeakers(),
	};
}

function withNativeAudioExcludes(exclude: Array<VirtmicNode>, options: LinuxAudioLinkOptions): Array<VirtmicNode> {
	const next = toNativeLinuxAudioPatterns(exclude);
	if (options.ignoreVirtual) {
		next.push({'node.virtual': 'true'});
	}
	return next;
}

function buildLinuxNativeAudioRule(
	sourceMode: 'system' | 'specific',
	userIncludeSources: Array<VirtmicNode>,
	userExcludeSources: Array<VirtmicNode>,
): LinuxNativeAudioRule {
	const linkOptions = getLinkOptions();
	const nativeIncludeSources = toNativeLinuxAudioPatterns(userIncludeSources);
	if (sourceMode === 'specific' && nativeIncludeSources.length > 0) {
		const includesDeviceTarget = nativeIncludeSources.some(
			(source) => LINUX_AUDIO_TARGET_OBJECTS_PATTERN_KEY in source,
		);
		return {
			include: nativeIncludeSources,
			exclude: withNativeAudioExcludes([], linkOptions),
			ignoreDevices: includesDeviceTarget ? false : linkOptions.ignoreDevices,
		};
	}
	const systemOptions = getSystemOptions();
	return {
		include: [],
		exclude: withNativeAudioExcludes(userExcludeSources, linkOptions),
		ignoreDevices: systemOptions.ignoreDevices,
		onlySpeakers: systemOptions.onlySpeakers,
		onlyDefaultSpeakers: systemOptions.onlyDefaultSpeakers,
	};
}

export async function reconfigureActiveLinuxScreenShareAudioLink(): Promise<boolean> {
	const electronApi = getElectronAPI();
	const virtmicApi = electronApi?.virtmic;
	if (!electronApi || electronApi.platform !== 'linux') {
		return false;
	}
	const sourceMode = VoiceSettings.getScreenShareAudioSourceMode();
	const userIncludeSources = VoiceSettings.getScreenShareAudioIncludeSources().map((entry) => ({...entry}));
	const userExcludeSources = VoiceSettings.getScreenShareAudioExcludeSources().map((entry) => ({...entry}));
	if (sourceMode === 'none') {
		disarmVirtmic();
		disarmNativeAudio();
		await virtmicApi?.stop();
		return true;
	}
	const nativeRule = buildLinuxNativeAudioRule(sourceMode, userIncludeSources, userExcludeSources);
	if (await MediaEngine.ensureLinuxScreenShareAudioPublication(nativeRule).catch(() => false)) {
		disarmVirtmic();
		await virtmicApi?.stop();
		return true;
	}
	disarmNativeAudio();
	return false;
}

export async function stopActiveLinuxScreenShareAudioLink(): Promise<boolean> {
	const electronApi = getElectronAPI();
	const virtmicApi = electronApi?.virtmic;
	if (!electronApi || electronApi.platform !== 'linux') {
		return false;
	}
	disarmVirtmic();
	disarmNativeAudio();
	await virtmicApi?.stop();
	return true;
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

function didScreenShareStart(): boolean {
	return Boolean(MediaEngine.room?.localParticipant?.isScreenShareEnabled || LocalVoiceState.getSelfStream());
}

function getScreenShareContentSource(
	shareContext: ScreenShareContext,
	preferredDisplaySurface?: 'window' | 'monitor',
): ScreenShareContentSource {
	if (shareContext === 'device') return 'device';
	if (preferredDisplaySurface === 'window') return 'app';
	return 'display';
}

export function normaliseDeviceScreenShareSettings(): void {
	const nextStreamingMode = normaliseStreamingModeForContext(VoiceSettings.getStreamingMode(), 'device');
	const nextResolution = normaliseResolutionForContext(
		VoiceSettings.getScreenshareResolution(),
		'device',
		hasHigherVideoQuality(),
	);
	if (
		nextStreamingMode === VoiceSettings.getStreamingMode() &&
		nextResolution === VoiceSettings.getScreenshareResolution()
	) {
		return;
	}
	VoiceSettingsCommands.update({
		streamingMode: nextStreamingMode,
		screenshareResolution: nextResolution,
	});
}

function shouldIncludeAudioForShare(
	shareContext: ScreenShareContext,
	displayShareEnvironment: DisplayShareEnvironment,
	sourceId?: string | null,
	preferredDisplaySurface?: 'window' | 'monitor',
): boolean {
	if (shareContext === 'device') {
		return VoiceSettings.getShareDeviceAudio();
	}
	if (sourceId?.startsWith('window:')) {
		return supportsDesktopScreenShareAudioCapture() && VoiceSettings.getShareAppAudio();
	}
	if (sourceId?.startsWith('screen:')) {
		return supportsDesktopScreenShareAudioCapture() && VoiceSettings.getShareDesktopAudio();
	}
	if (preferredDisplaySurface === 'window') {
		return supportsDesktopScreenShareAudioCapture() && VoiceSettings.getShareAppAudio();
	}
	if (shareContext === 'display' && usesNativeDisplayShareAudioSelection(displayShareEnvironment)) {
		return supportsDesktopScreenShareAudioCapture() && VoiceSettings.getShareDesktopAudio();
	}
	return supportsDesktopScreenShareAudioCapture() && VoiceSettings.getShareDesktopAudio();
}

function removeAudioFromCaptureOptions(captureOptions: ScreenShareCaptureOptions): void {
	captureOptions.audio = false;
	captureOptions.systemAudio = 'exclude';
	captureOptions.windowAudio = 'exclude';
}

function buildAudioCaptureFailureDebug(
	overrides: {
		platform?: string | null;
		sourceId?: string | null;
		sourceMode?: string | null;
		reason?: string | null;
		detail?: string | null;
	} = {},
): ScreenShareAudioCaptureDebugInfo {
	return {
		platform: overrides.platform ?? getElectronAPI()?.platform ?? null,
		...getLastNativeAudioArmFailure(),
		...overrides,
	};
}

function degradeAudioToVideoOnly(
	captureOptions: ScreenShareCaptureOptions,
	debugInfo: ScreenShareAudioCaptureDebugInfo,
): void {
	logger.warn('Screen share audio capture unavailable; proceeding with video only', debugInfo);
	removeAudioFromCaptureOptions(captureOptions);
}

function cleanupNativeAudioAfterCaptureDidNotStart(mode: 'start' | 'switch'): void {
	if (mode === 'switch') {
		disarmPendingNativeAudio();
		return;
	}
	disarmNativeAudio();
}

function getConfiguredScreenShareOptions(
	shareContext: ScreenShareContext,
	displayShareEnvironment: DisplayShareEnvironment,
	sourceDimensions?: {
		width: number;
		height: number;
	},
	sourceId?: string | null,
	preferredDisplaySurface?: 'window' | 'monitor',
	videoCodec?: VideoCodec,
) {
	const currentResolution = VoiceSettings.getScreenshareResolution();
	const currentStreamingMode = VoiceSettings.getStreamingMode();
	const higherQuality = hasHigherVideoQuality();
	const normalisedStreamingMode = normaliseStreamingModeForContext(currentStreamingMode, shareContext);
	const normalisedResolution = normaliseResolutionForContext(currentResolution, shareContext, higherQuality);
	const codecPreference = VoiceSettings.getPreferredScreenShareCodec();
	const preferredVideoCodec = videoCodec ?? ScreenShareCodecNegotiation.selectScreenShareCodec(codecPreference);
	const contentHint = resolveScreenShareContentHintForContext(
		VoiceSettings.getScreenShareContentHintOverride(),
		preferredVideoCodec,
		getScreenShareContentSource(shareContext, preferredDisplaySurface),
		normalisedStreamingMode,
	);
	const {resolution, frameRate} = resolveStreamingModeSettings(
		normalisedStreamingMode,
		normalisedResolution,
		VoiceSettings.getVideoFrameRate(),
		higherQuality,
	);
	const includeAudio = shouldIncludeAudioForShare(
		shareContext,
		displayShareEnvironment,
		sourceId,
		preferredDisplaySurface,
	);
	if (!includeAudio && shareContext !== 'device' && supportsDesktopScreenShareAudioCapture()) {
		logger.info('Screen share audio not requested for this surface', {
			sourceId,
			preferredDisplaySurface,
			shareAppAudio: VoiceSettings.getShareAppAudio(),
			shareDesktopAudio: VoiceSettings.getShareDesktopAudio(),
		});
	}
	const {captureOptions, publishOptions} = buildScreenShareOptions({
		resolution,
		frameRate,
		includeAudio,
		streamingMode: normalisedStreamingMode,
		contentHint,
		maxBitrateBps: VoiceSettings.getScreenShareMaxBitrateBpsOverride(),
		sourceDimensions,
		preferredDisplaySurface,
	});
	publishOptions.videoCodec = preferredVideoCodec;
	return {
		captureOptions,
		publishOptions,
		includeAudio,
		audioDeviceId: includeAudio ? VoiceSettings.getEffectiveScreenShareAudioDeviceId() || undefined : undefined,
	};
}

export interface ConfiguredDisplayScreenShareOptions {
	sourceDimensions?: {
		width: number;
		height: number;
	};
	preferredDisplaySurface?: 'window' | 'monitor';
	isOwnWindow?: boolean;
}

async function runConfiguredDisplayScreenShare(
	sourceId?: string | null,
	options?: ConfiguredDisplayScreenShareOptions,
	mode: 'start' | 'switch' = 'start',
): Promise<boolean> {
	const electronApi = getElectronAPI();
	const displayShareEnvironment = await getDisplayShareEnvironment();
	const useWaylandPortal = displayShareEnvironment === 'desktop-wayland';
	const {
		captureOptions,
		publishOptions,
		includeAudio: requestedAudio,
	} = getConfiguredScreenShareOptions(
		'display',
		displayShareEnvironment,
		options?.sourceDimensions,
		sourceId,
		options?.preferredDisplaySurface,
	);
	if (electronApi) {
		const restartWaylandPortalForSwitch = useWaylandPortal && mode === 'switch';
		if (!useWaylandPortal && !sourceId) {
			logger.warn('No desktop source selected for display share');
			return false;
		}
		if (restartWaylandPortalForSwitch) {
			if (!didScreenShareStart()) {
				logger.warn('No active screen share to restart for Wayland portal source switch');
				return false;
			}
			await MediaEngine.setScreenShareEnabled(false, {
				sendUpdate: false,
				playSound: false,
				preserveStreamAudioPreferences: true,
			});
		}
		let nativeAudioArmed = false;
		const isOwnWindowShare = options?.isOwnWindow === true && sourceId?.startsWith('window:');
		if (isOwnWindowShare && requestedAudio) {
			logger.warn('Fluxer-owned window audio is excluded from screen share capture; continuing video-only', {
				sourceId,
				platform: electronApi.platform,
			});
			degradeAudioToVideoOnly(
				captureOptions,
				buildAudioCaptureFailureDebug({
					sourceId,
					platform: electronApi.platform,
					reason: 'self-window-audio-route-unavailable',
				}),
			);
		}
		const requestedAppAudioOnLinux =
			requestedAudio && !isOwnWindowShare && electronApi.platform === 'linux' && sourceId?.startsWith('window:');
		const requestedDesktopAudio =
			requestedAudio && (sourceId?.startsWith('screen:') || (useWaylandPortal && VoiceSettings.getShareDesktopAudio()));
		const requestedNativeDesktopAudio =
			requestedDesktopAudio &&
			(electronApi.platform === 'darwin' || electronApi.platform === 'win32') &&
			sourceId?.startsWith('screen:');
		const requestedNativePickerAudioOnLinux = requestedAudio && electronApi.platform === 'linux' && useWaylandPortal;
		const linuxDesktopAudioSourceMode =
			electronApi.platform === 'linux' && (requestedDesktopAudio || requestedNativePickerAudioOnLinux)
				? VoiceSettings.getScreenShareAudioSourceMode()
				: null;
		if (requestedAppAudioOnLinux) {
			try {
				nativeAudioArmed = await armNativeAudioForNextCapture(sourceId ?? '');
			} catch (error) {
				logger.warn('Failed to arm Linux native per-window audio capture', {
					sourceId,
					error,
				});
			}
			if (!nativeAudioArmed) {
				degradeAudioToVideoOnly(
					captureOptions,
					buildAudioCaptureFailureDebug({
						sourceId,
						reason: getLastNativeAudioArmFailure()?.reason ?? 'linux-window-audio-route-unavailable',
					}),
				);
			}
		} else if (requestedNativeDesktopAudio) {
			try {
				nativeAudioArmed = await armNativeSystemAudioForNextCapture();
			} catch (error) {
				logger.warn('Failed to arm native desktop audio capture', {
					sourceId,
					platform: electronApi.platform,
					error,
				});
			}
			if (!nativeAudioArmed) {
				const debugInfo = buildAudioCaptureFailureDebug({
					sourceId,
					sourceMode: 'system',
					platform: electronApi.platform,
					reason: getLastNativeAudioArmFailure()?.reason ?? 'system-audio-route-unavailable',
				});
				logger.warn('Desktop audio unavailable; aborting screen share because audio was requested', debugInfo);
				degradeAudioToVideoOnly(captureOptions, debugInfo);
			}
		} else if (linuxDesktopAudioSourceMode === 'none') {
			removeAudioFromCaptureOptions(captureOptions);
		} else if ((requestedDesktopAudio || requestedNativePickerAudioOnLinux) && electronApi.platform === 'linux') {
			const sourceMode = linuxDesktopAudioSourceMode ?? 'system';
			const userIncludeSources = VoiceSettings.getScreenShareAudioIncludeSources().map((entry) => ({...entry}));
			const userExcludeSources = VoiceSettings.getScreenShareAudioExcludeSources().map((entry) => ({...entry}));
			try {
				nativeAudioArmed = await armNativeAudioForLinuxRouting(
					buildLinuxNativeAudioRule(sourceMode, userIncludeSources, userExcludeSources),
				);
			} catch (error) {
				logger.warn('Failed to arm Linux native audio-capture link', {
					sourceMode,
					error,
				});
			}
			if (!nativeAudioArmed) {
				degradeAudioToVideoOnly(
					captureOptions,
					buildAudioCaptureFailureDebug({
						sourceMode,
						reason: getLastNativeAudioArmFailure()?.reason ?? 'linux-system-audio-route-unavailable',
					}),
				);
			}
		}
		if (
			requestedAudio &&
			!isOwnWindowShare &&
			sourceId?.startsWith('window:') &&
			(electronApi.platform === 'darwin' || electronApi.platform === 'win32')
		) {
			try {
				nativeAudioArmed = await armNativeAudioForNextCapture(sourceId);
			} catch (error) {
				logger.warn('Failed to arm native per-window audio capture', {
					sourceId,
					error,
				});
			}
			if (!nativeAudioArmed) {
				const debugInfo = buildAudioCaptureFailureDebug({
					sourceId,
					reason: getLastNativeAudioArmFailure()?.reason ?? 'native-window-audio-route-unavailable',
				});
				logger.warn('Per-window audio unavailable; aborting screen share because audio was requested', {
					sourceId,
					platform: electronApi.platform,
					reason: debugInfo.reason,
				});
				degradeAudioToVideoOnly(captureOptions, debugInfo);
			}
		}
		if (
			requestedAudio &&
			sourceId?.startsWith('window:') &&
			!isOwnWindowShare &&
			!nativeAudioArmed &&
			electronApi.platform !== 'darwin' &&
			electronApi.platform !== 'win32' &&
			electronApi.platform !== 'linux'
		) {
			degradeAudioToVideoOnly(
				captureOptions,
				buildAudioCaptureFailureDebug({
					sourceId,
					platform: electronApi.platform,
					reason: getLastNativeAudioArmFailure()?.reason ?? 'window-audio-route-unavailable',
				}),
			);
		}
		if (requestedAudio && (nativeAudioArmed || electronApi.platform === 'linux')) {
			removeAudioFromCaptureOptions(captureOptions);
		}
		try {
			if (useWaylandPortal && options?.preferredDisplaySurface) {
				await electronApi.setDisplayMediaPortalPreference?.(options.preferredDisplaySurface);
			}
			if (!useWaylandPortal && sourceId) {
				setDesktopSourceIntent({sourceId, includeAudio: false});
				ActiveScreenShareSource.setSourceId(sourceId, {isOwnWindow: isOwnWindowShare});
			}
			let operationSucceeded = false;
			if (mode === 'switch' && !restartWaylandPortalForSwitch) {
				operationSucceeded = await MediaEngine.replaceActiveDisplayScreenShare(captureOptions, publishOptions);
			} else {
				await MediaEngine.setScreenShareEnabled(
					true,
					restartWaylandPortalForSwitch ? {...captureOptions, playSound: false} : captureOptions,
					publishOptions,
				);
				operationSucceeded = didScreenShareStart();
			}
			const captured = mode === 'switch' ? operationSucceeded : didScreenShareStart();
			if (nativeAudioArmed && !captured) {
				cleanupNativeAudioAfterCaptureDidNotStart(mode);
			}
			if (!captured) {
				ActiveScreenShareSource.clear();
			}
			if (!captured && useWaylandPortal && mode !== 'switch') {
				logger.warn('Wayland screen share portal did not yield a capturable source', {sourceId});
				throw new ScreenSharePortalUnavailableError('empty');
			}
			if (
				captured &&
				requestedAudio &&
				electronApi.platform === 'linux' &&
				linuxDesktopAudioSourceMode !== null &&
				linuxDesktopAudioSourceMode !== 'none'
			) {
				const audioRelinked = await reconfigureActiveLinuxScreenShareAudioLink().catch((error) => {
					logger.warn('Failed to link Linux screen-share audio after capture start', {mode, error});
					return false;
				});
				if (!audioRelinked) {
					const debugInfo = buildAudioCaptureFailureDebug({
						sourceMode: linuxDesktopAudioSourceMode,
						platform: electronApi.platform,
						reason: getLastNativeAudioArmFailure()?.reason ?? 'linux-system-audio-route-unavailable',
					});
					logger.warn('Linux screen-share capture succeeded, but audio link did not complete', {
						...debugInfo,
						mode,
					});
					degradeAudioToVideoOnly(captureOptions, debugInfo);
				}
			}
			return captured;
		} catch (error) {
			if (isScreenSharePortalUnavailableError(error)) {
				throw error;
			}
			const capturedAfterError = didScreenShareStart();
			if (nativeAudioArmed && !capturedAfterError) {
				cleanupNativeAudioAfterCaptureDidNotStart(mode);
			}
			if (!capturedAfterError) {
				ActiveScreenShareSource.clear();
			}
			if (useWaylandPortal && !capturedAfterError && mode !== 'switch') {
				logger.warn('Wayland screen share portal capture failed to start', {
					sourceId,
					error,
				});
				throw new ScreenSharePortalUnavailableError('error', error instanceof Error ? error.message : undefined);
			}
			throw error;
		} finally {
			if (!useWaylandPortal) {
				clearDesktopSourceIntent();
			}
		}
	}
	let operationSucceeded = false;
	if (mode === 'switch') {
		operationSucceeded = await MediaEngine.replaceActiveDisplayScreenShare(captureOptions, publishOptions);
	} else {
		await MediaEngine.setScreenShareEnabled(true, captureOptions, publishOptions);
		operationSucceeded = didScreenShareStart();
	}
	return mode === 'switch' ? operationSucceeded : didScreenShareStart();
}

export async function startConfiguredDisplayScreenShare(
	sourceId?: string | null,
	options?: ConfiguredDisplayScreenShareOptions,
): Promise<boolean> {
	let didStart = false;
	await executeScreenShareOperation(async () => {
		didStart = await runConfiguredDisplayScreenShare(sourceId, options, 'start');
	});
	return didStart;
}

export async function switchConfiguredDisplayScreenShare(
	sourceId?: string | null,
	options?: ConfiguredDisplayScreenShareOptions,
): Promise<boolean> {
	let didSwitch = false;
	await executeScreenShareOperation(async () => {
		didSwitch = await runConfiguredDisplayScreenShare(sourceId, options, 'switch');
	});
	return didSwitch;
}

export async function startConfiguredDeviceScreenShare(videoDeviceId: string): Promise<boolean> {
	normaliseDeviceScreenShareSettings();
	const {captureOptions, publishOptions, includeAudio, audioDeviceId} = getConfiguredScreenShareOptions(
		'device',
		'desktop-custom',
	);
	try {
		await MediaEngine.startDeviceScreenShare(
			{
				videoDeviceId,
				audioDeviceId: includeAudio ? audioDeviceId : undefined,
				resolution: captureOptions.resolution,
			},
			publishOptions,
		);
	} catch (error) {
		logger.error('Failed to start device screen share', {
			error,
			videoDeviceId,
		});
	}
	return didScreenShareStart();
}

export async function switchConfiguredDeviceScreenShare(videoDeviceId: string): Promise<boolean> {
	normaliseDeviceScreenShareSettings();
	const {captureOptions, publishOptions, includeAudio, audioDeviceId} = getConfiguredScreenShareOptions(
		'device',
		'desktop-custom',
	);
	try {
		return await MediaEngine.replaceActiveDeviceScreenShare(
			{
				videoDeviceId,
				audioDeviceId: includeAudio ? audioDeviceId : undefined,
				resolution: captureOptions.resolution,
			},
			publishOptions,
		);
	} catch (error) {
		logger.error('Failed to switch device screen share source', {
			error,
			videoDeviceId,
		});
		return false;
	}
}
