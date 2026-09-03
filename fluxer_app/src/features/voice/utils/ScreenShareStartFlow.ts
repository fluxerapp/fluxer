// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI, supportsDesktopScreenShareAudioCapture} from '@app/features/ui/utils/NativeUtils';
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
	getNativeAudioAvailabilityCached,
} from '@app/features/voice/utils/NativeAudioCaptureBridge';
import {
	type ScreenShareAudioCaptureDebugInfo,
	ScreenShareAudioCaptureError,
} from '@app/features/voice/utils/ScreenShareAudioCaptureError';
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
import {
	canSelectManualAudioSources,
	routesManualAudioSources,
	type StreamSettingsShareContext,
	supportsManualScreenShareAudioSourceSelection,
} from '@app/features/voice/utils/StreamSettingsUpdatePolicy';
import {hasHigherVideoQuality} from '@app/features/voice/utils/VideoQualityEntitlement';
import type {NativeAudioStartOptions, VirtmicNode} from '@app/types/electron.d';
import type {ScreenShareCaptureOptions, VideoCodec} from 'livekit-client';

const logger = new Logger('ScreenShareStartFlow');

type LinuxNativeAudioRule = NonNullable<NativeAudioStartOptions['linuxRule']>;

interface LinuxAudioLinkOptions {
	ignoreInputMedia: boolean;
	ignoreVirtual: boolean;
	ignoreDevices: boolean;
}

function getLinkOptions(): LinuxAudioLinkOptions {
	return {
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
			ignoreInputMedia: linkOptions.ignoreInputMedia,
			ignoreDevices: includesDeviceTarget ? false : linkOptions.ignoreDevices,
		};
	}
	const systemOptions = getSystemOptions();
	return {
		include: [],
		exclude: withNativeAudioExcludes(userExcludeSources, linkOptions),
		ignoreInputMedia: systemOptions.ignoreInputMedia,
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
	const sourceMode = VoiceSettings.getEffectiveScreenShareAudioSourceMode();
	const userIncludeSources = VoiceSettings.getEffectiveScreenShareAudioIncludeSources().map((entry) => ({...entry}));
	const userExcludeSources = VoiceSettings.getEffectiveScreenShareAudioExcludeSources().map((entry) => ({...entry}));
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

async function getManualAudioSourceSelectionInput(shareContext: StreamSettingsShareContext) {
	const platform = getElectronAPI()?.platform;
	return {
		platform,
		shareContext,
		nativeAudioAvailability: platform === 'linux' ? await getNativeAudioAvailabilityCached() : null,
		manualOptIn: VoiceSettings.getScreenShareManualAudioSourcesOptIn(),
		audioSourceMode: VoiceSettings.getScreenShareAudioSourceMode(),
		selectedSourceCount: VoiceSettings.getScreenShareAudioIncludeSources().length,
	};
}

export async function canSelectManualAudioSourcesForShare(shareContext: StreamSettingsShareContext): Promise<boolean> {
	return canSelectManualAudioSources(await getManualAudioSourceSelectionInput(shareContext));
}

export async function shouldRouteManualAudioSourcesForShare(
	shareContext: StreamSettingsShareContext,
): Promise<boolean> {
	return routesManualAudioSources(await getManualAudioSourceSelectionInput(shareContext));
}

function hasStoredManualAudioSourceSelection(): boolean {
	return (
		VoiceSettings.getScreenShareAudioSourceMode() !== 'system' ||
		VoiceSettings.getScreenShareAudioIncludeSources().length > 0 ||
		VoiceSettings.getScreenShareAudioExcludeSources().length > 0
	);
}

function captureAudioEnabledForShare(shareContext: StreamSettingsShareContext): boolean {
	if (shareContext === 'app') return VoiceSettings.getShareAppAudio();
	if (shareContext === 'device') return VoiceSettings.getShareDeviceAudio();
	return VoiceSettings.getShareDesktopAudio();
}

export async function reapplyActiveScreenShareAudioSources(): Promise<boolean> {
	const shareContext = ActiveScreenShareSource.getShareContext();
	if (shareContext === null) return false;
	if (shareContext === 'app') return false;
	if (!hasStoredManualAudioSourceSelection()) return false;
	if (!captureAudioEnabledForShare(shareContext)) return false;
	const selectionInput = await getManualAudioSourceSelectionInput(shareContext);
	if (!supportsManualScreenShareAudioSourceSelection(selectionInput)) return false;
	const applied = await (shareContext === 'device'
		? reconfigureActiveDeviceShareAudio()
		: reconfigureActiveLinuxScreenShareAudioLink()
	).catch((error) => {
		logger.warn('Failed to reapply the active screen share audio sources', {shareContext, error});
		return false;
	});
	if (!applied) {
		logger.warn('Active screen share audio sources could not be reapplied', {
			shareContext,
			sourceMode: VoiceSettings.getEffectiveScreenShareAudioSourceMode(),
		});
	}
	return applied;
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

function shouldIncludeAudioForShare(
	shareContext: ScreenShareContext,
	displayShareEnvironment: DisplayShareEnvironment,
	sourceId?: string | null,
	preferredDisplaySurface?: 'window' | 'monitor',
): boolean {
	if (shareContext === 'device') {
		return VoiceSettings.getShareDeviceAudio();
	}
	if (displayShareEnvironment === 'web') {
		return supportsDesktopScreenShareAudioCapture();
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

function failRequestedAudioCapture(debugInfo: ScreenShareAudioCaptureDebugInfo): never {
	logger.warn('Screen share audio capture was requested but could not start', debugInfo);
	throw new ScreenShareAudioCaptureError(debugInfo);
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
	includeAudioOverride?: boolean,
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
	const includeAudio =
		includeAudioOverride ??
		shouldIncludeAudioForShare(shareContext, displayShareEnvironment, sourceId, preferredDisplaySurface);
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
		contentHint,
		sourceDimensions,
		preferredDisplaySurface,
		useBrowserAudioPicker: displayShareEnvironment === 'web',
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
	includeAudio?: boolean;
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
		undefined,
		options?.includeAudio,
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
			removeAudioFromCaptureOptions(captureOptions);
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
				? VoiceSettings.getEffectiveScreenShareAudioSourceMode()
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
				failRequestedAudioCapture(
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
				failRequestedAudioCapture(debugInfo);
			}
		} else if (linuxDesktopAudioSourceMode === 'none') {
			removeAudioFromCaptureOptions(captureOptions);
		} else if ((requestedDesktopAudio || requestedNativePickerAudioOnLinux) && electronApi.platform === 'linux') {
			const sourceMode = linuxDesktopAudioSourceMode ?? 'system';
			const userIncludeSources = VoiceSettings.getEffectiveScreenShareAudioIncludeSources().map((entry) => ({
				...entry,
			}));
			const userExcludeSources = VoiceSettings.getEffectiveScreenShareAudioExcludeSources().map((entry) => ({
				...entry,
			}));
			if (requestedNativePickerAudioOnLinux && options?.preferredDisplaySurface === 'window') {
				logger.info(
					'Wayland window share cannot identify the shared window; capturing the desktop mix without Fluxer instead',
					{sourceMode},
				);
			}
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
				failRequestedAudioCapture(
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
				failRequestedAudioCapture(debugInfo);
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
			failRequestedAudioCapture(
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
			}
			let operationSucceeded = false;
			if (mode === 'switch' && !restartWaylandPortalForSwitch) {
				operationSucceeded = await MediaEngine.replaceActiveDisplayScreenShare(captureOptions, publishOptions, {
					sourceId: sourceId ?? null,
					displayShareEnvironment,
					requireAudio: requestedAudio && nativeAudioArmed,
				});
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
			if (captured && !useWaylandPortal && sourceId) {
				ActiveScreenShareSource.setPublishedSource(sourceId.startsWith('window:') ? 'app' : 'display', sourceId, {
					isOwnWindow: isOwnWindowShare,
				});
			}
			if (captured && useWaylandPortal) {
				ActiveScreenShareSource.setPublishedSource('wayland', null);
			}
			if (!captured && mode === 'start' && !didScreenShareStart()) {
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
			if (!capturedAfterError && mode === 'start' && !didScreenShareStart()) {
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
		operationSucceeded = await MediaEngine.replaceActiveDisplayScreenShare(captureOptions, publishOptions, {
			sourceId: null,
			displayShareEnvironment,
			requireAudio: false,
		});
	} else {
		await MediaEngine.setScreenShareEnabled(true, captureOptions, publishOptions);
		operationSucceeded = didScreenShareStart();
	}
	const captured = mode === 'switch' ? operationSucceeded : didScreenShareStart();
	if (captured) {
		ActiveScreenShareSource.setPublishedSource('web', null);
	}
	return captured;
}

interface ConfiguredScreenShareMutationRequest {
	execute: () => Promise<boolean>;
	promise: Promise<boolean>;
	resolve: (result: boolean) => void;
	reject: (error: unknown) => void;
}

let configuredScreenShareMutationActive = false;
let pendingConfiguredScreenShareMutation: ConfiguredScreenShareMutationRequest | null = null;

function createConfiguredScreenShareMutationRequest(
	execute: () => Promise<boolean>,
): ConfiguredScreenShareMutationRequest {
	let resolveRequest: ((result: boolean) => void) | undefined;
	let rejectRequest: ((error: unknown) => void) | undefined;
	const promise = new Promise<boolean>((resolve, reject) => {
		resolveRequest = resolve;
		rejectRequest = reject;
	});
	if (!resolveRequest || !rejectRequest) {
		throw new Error('Configured screen share mutation deferred was not initialized');
	}
	return {execute, promise, resolve: resolveRequest, reject: rejectRequest};
}

async function drainConfiguredScreenShareMutations(
	initialRequest: ConfiguredScreenShareMutationRequest,
): Promise<void> {
	let request: ConfiguredScreenShareMutationRequest | null = initialRequest;
	while (request) {
		try {
			request.resolve(await request.execute());
		} catch (error) {
			request.reject(error);
		}
		request = pendingConfiguredScreenShareMutation;
		pendingConfiguredScreenShareMutation = null;
	}
	configuredScreenShareMutationActive = false;
}

function scheduleConfiguredScreenShareMutation(execute: () => Promise<boolean>): Promise<boolean> {
	const request = createConfiguredScreenShareMutationRequest(execute);
	if (!configuredScreenShareMutationActive) {
		configuredScreenShareMutationActive = true;
		void drainConfiguredScreenShareMutations(request);
		return request.promise;
	}
	pendingConfiguredScreenShareMutation?.resolve(false);
	pendingConfiguredScreenShareMutation = request;
	return request.promise;
}

export async function startConfiguredDisplayScreenShare(
	sourceId?: string | null,
	options?: ConfiguredDisplayScreenShareOptions,
): Promise<boolean> {
	return scheduleConfiguredScreenShareMutation(async () => {
		let didStart = false;
		await executeScreenShareOperation(async () => {
			didStart = await runConfiguredDisplayScreenShare(sourceId, options, 'start');
		});
		return didStart;
	});
}

export async function switchConfiguredDisplayScreenShare(
	sourceId?: string | null,
	options?: ConfiguredDisplayScreenShareOptions,
): Promise<boolean> {
	return scheduleConfiguredScreenShareMutation(async () => {
		let didSwitch = false;
		await executeScreenShareOperation(async () => {
			didSwitch = await runConfiguredDisplayScreenShare(sourceId, options, 'switch');
		});
		return didSwitch;
	});
}

async function linkManualAudioSourcesForDeviceShare(mode: 'start' | 'switch'): Promise<void> {
	const linked = await reconfigureActiveLinuxScreenShareAudioLink().catch((error) => {
		logger.warn('Failed to link the selected application audio to the device share', {mode, error});
		return false;
	});
	if (linked) return;
	logger.warn(
		'Device screen share is running without the selected application audio',
		buildAudioCaptureFailureDebug({
			sourceMode: VoiceSettings.getEffectiveScreenShareAudioSourceMode(),
			reason: getLastNativeAudioArmFailure()?.reason ?? 'manual-audio-route-unavailable',
		}),
	);
}

async function resolveDeviceShareAudioDeviceId(
	videoDeviceId: string,
	configuredAudioDeviceId: string | undefined,
): Promise<string | undefined> {
	if (configuredAudioDeviceId === undefined) return undefined;
	if (VoiceSettings.getScreenShareAudioDeviceId() !== 'default') return configuredAudioDeviceId;
	if (!videoDeviceId || videoDeviceId === 'default') return configuredAudioDeviceId;
	try {
		const devices = await navigator.mediaDevices.enumerateDevices();
		const videoDevice = devices.find((device) => device.kind === 'videoinput' && device.deviceId === videoDeviceId);
		if (!videoDevice?.groupId) return configuredAudioDeviceId;
		const pairedInput = devices.find(
			(device) =>
				device.kind === 'audioinput' &&
				device.groupId === videoDevice.groupId &&
				device.deviceId !== '' &&
				device.deviceId !== 'default' &&
				device.deviceId !== 'communications',
		);
		if (!pairedInput) return configuredAudioDeviceId;
		logger.info('Using the capture device own audio input for the device share', {
			videoDeviceId,
			audioDeviceId: pairedInput.deviceId,
		});
		return pairedInput.deviceId;
	} catch (error) {
		logger.warn('Failed to pair an audio input with the shared video device', {videoDeviceId, error});
		return configuredAudioDeviceId;
	}
}

export async function reconfigureActiveDeviceShareAudio(): Promise<boolean> {
	if (!VoiceSettings.getShareDeviceAudio()) return false;
	if (await shouldRouteManualAudioSourcesForShare('device')) {
		return reconfigureActiveLinuxScreenShareAudioLink();
	}
	await stopActiveLinuxScreenShareAudioLink();
	const configuredAudioDeviceId = VoiceSettings.getEffectiveScreenShareAudioDeviceId();
	const audioDeviceId = await resolveDeviceShareAudioDeviceId(
		MediaEngine.getActiveScreenShareVideoDeviceId(),
		configuredAudioDeviceId,
	);
	return MediaEngine.ensureDeviceScreenShareMicPublication(audioDeviceId ?? configuredAudioDeviceId);
}

async function getConfiguredDeviceScreenShareAudio(videoDeviceId: string): Promise<{
	routeManualAudioSources: boolean;
	audioDeviceId: string | undefined;
}> {
	const {includeAudio, audioDeviceId} = getConfiguredScreenShareOptions('device', 'desktop-custom');
	if (!includeAudio) return {routeManualAudioSources: false, audioDeviceId: undefined};
	if (await shouldRouteManualAudioSourcesForShare('device')) {
		return {routeManualAudioSources: true, audioDeviceId: undefined};
	}
	return {
		routeManualAudioSources: false,
		audioDeviceId: await resolveDeviceShareAudioDeviceId(videoDeviceId, audioDeviceId),
	};
}

export async function startConfiguredDeviceScreenShare(videoDeviceId: string): Promise<boolean> {
	return scheduleConfiguredScreenShareMutation(async () => {
		const {captureOptions, publishOptions} = getConfiguredScreenShareOptions('device', 'desktop-custom');
		const {routeManualAudioSources, audioDeviceId} = await getConfiguredDeviceScreenShareAudio(videoDeviceId);
		try {
			await MediaEngine.startDeviceScreenShare(
				{
					videoDeviceId,
					audioDeviceId,
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
		const didStart = didScreenShareStart();
		if (didStart) ActiveScreenShareSource.setPublishedSource('device', null);
		if (didStart && routeManualAudioSources) await linkManualAudioSourcesForDeviceShare('start');
		return didStart;
	});
}

export async function switchConfiguredDeviceScreenShare(videoDeviceId: string): Promise<boolean> {
	return scheduleConfiguredScreenShareMutation(async () => {
		const {captureOptions, publishOptions} = getConfiguredScreenShareOptions('device', 'desktop-custom');
		const {routeManualAudioSources, audioDeviceId} = await getConfiguredDeviceScreenShareAudio(videoDeviceId);
		try {
			const didSwitch = await MediaEngine.replaceActiveDeviceScreenShare(
				{
					videoDeviceId,
					audioDeviceId,
					resolution: captureOptions.resolution,
				},
				publishOptions,
			);
			if (didSwitch) ActiveScreenShareSource.setPublishedSource('device', null);
			if (didSwitch && routeManualAudioSources) await linkManualAudioSourcesForDeviceShare('switch');
			return didSwitch;
		} catch (error) {
			logger.error('Failed to switch device screen share source', {
				error,
				videoDeviceId,
			});
			return false;
		}
	});
}
