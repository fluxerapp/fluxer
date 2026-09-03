// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {VoiceEngineV2AppSourceLifecycleBridge} from '@app/features/voice/engine/v2/VoiceEngineV2AppSourceLifecycleBridge';
import {getNativeAudioErrorDetail} from '@app/features/voice/utils/NativeAudioFailureUtils';
import {getBridgeStats as getNativeAudioBridgeStats} from '@app/features/voice/utils/native_audio_capture_bridge/bridgeStats';
import {createGeneratorBridge} from '@app/features/voice/utils/native_audio_capture_bridge/createGeneratorBridge';
import {createScriptProcessorBridge} from '@app/features/voice/utils/native_audio_capture_bridge/createScriptProcessorBridge';
import {
	type ActiveNativeAudioBridge,
	type ArmedNativeAudioCapture,
	getArmedCaptureId,
	getAudioDataCtor,
	getGeneratorCtor,
	getNativeAudioApi,
	isNativeAudioDesktopPlatform,
	isValidAudioFrameMessage,
	type NativeAudioBridgeHandle,
	type NativeAudioBridgeStats,
	nativeAudioRoutingRulesEqual,
	patchTrackStopForCleanup,
	replaceStreamAudioTrack,
	shouldMixSelfWindowAudioIntoSystemCapture,
	unsupportedPlatformAvailability,
} from '@app/features/voice/utils/native_audio_capture_bridge/shared';
import type {ScreenShareAudioCaptureDebugInfo} from '@app/features/voice/utils/ScreenShareAudioCaptureError';
import {
	acquireSelfWindowScreenShareAudioTrack,
	releaseSelfWindowScreenShareAudioTrack,
} from '@app/features/voice/utils/SelfWindowScreenShareAudio';
import {
	type MixedSelfWindowAudioTrack,
	mixTrackWithSelfWindowScreenShareAudio,
} from '@app/features/voice/utils/SelfWindowScreenShareAudioMix';
import type {
	NativeAudioApi,
	NativeAudioAvailability,
	NativeAudioFrameMessage,
	NativeAudioStartOptions,
	NativeAudioStartResult,
} from '@app/types/electron.d';

export type {NativeAudioBridgeStats};

const logger = new Logger('NativeAudioCaptureBridge');

let patched = false;
let originalGetDisplayMedia: MediaDevices['getDisplayMedia'] | null = null;
let availabilityCache: Promise<NativeAudioAvailability> | null = null;
let resolvedAvailability: NativeAudioAvailability | null = null;
let armedCapture: ArmedNativeAudioCapture | null = null;
let lastArmFailure: ScreenShareAudioCaptureDebugInfo | null = null;
let activeBridge: ActiveNativeAudioBridge | null = null;
let supersededBridge: ActiveNativeAudioBridge | null = null;

interface NativeAudioStartedCaptureDiagnostic {
	captureId: string;
	startedAtMs: number;
	platform?: string;
	sourceMode?: 'system' | 'specific';
	targetPid?: number;
	linuxRule?: NativeAudioStartOptions['linuxRule'];
	macBackend?: NativeAudioStartOptions['macBackend'];
	macCaptureScope?: NativeAudioStartOptions['macCaptureScope'];
	winCaptureScope?: NativeAudioStartOptions['winCaptureScope'];
	includeSelfWindowAudio?: boolean;
}

let lastStartedCapture: NativeAudioStartedCaptureDiagnostic | null = null;
let sourceLifecycleBridge: VoiceEngineV2AppSourceLifecycleBridge | null = null;
const lifecycleBoundCaptureIds = new Set<string>();

export {getNativeAudioBridgeStats};

export function setNativeAudioCaptureBridgeLifecycleBridge(bridge: VoiceEngineV2AppSourceLifecycleBridge | null): void {
	for (const captureId of lifecycleBoundCaptureIds) {
		sourceLifecycleBridge?.unbind(captureId);
	}
	lifecycleBoundCaptureIds.clear();
	sourceLifecycleBridge = bridge;
}

function bindNativeAudioCaptureLifecycle(captureId: string): void {
	const bridge = sourceLifecycleBridge;
	if (!bridge) return;
	if (typeof captureId !== 'string' || captureId.length === 0) return;
	if (lifecycleBoundCaptureIds.has(captureId)) return;
	const sourceId = `native-audio-tap:${captureId}`;
	const bound = bridge.bind({captureId, sourceId});
	if (bound) {
		lifecycleBoundCaptureIds.add(captureId);
	}
}

function unbindNativeAudioCaptureLifecycle(captureId: string, faulted: boolean): void {
	if (!lifecycleBoundCaptureIds.has(captureId)) return;
	const bridge = sourceLifecycleBridge;
	if (bridge) {
		if (faulted) {
			bridge.reportLifecycle({captureId, kind: 'error', message: 'native-audio-tap-track-ended'});
		}
		bridge.unbind(captureId);
	}
	lifecycleBoundCaptureIds.delete(captureId);
}

function setLastArmFailure(debugInfo: ScreenShareAudioCaptureDebugInfo): void {
	lastArmFailure = debugInfo;
}

function clearLastArmFailure(): void {
	lastArmFailure = null;
}

export function getLastNativeAudioArmFailure(): ScreenShareAudioCaptureDebugInfo | null {
	return lastArmFailure ? {...lastArmFailure} : null;
}

function cloneLinuxRule(rule: NativeAudioStartOptions['linuxRule']): NativeAudioStartOptions['linuxRule'] {
	if (!rule) return undefined;
	const next: NativeAudioStartOptions['linuxRule'] = {
		...rule,
		include: rule.include?.map((entry) => ({...entry})),
		exclude: rule.exclude?.map((entry) => ({...entry})),
	};
	return next;
}

function summarizeArmedCapture(capture: ArmedNativeAudioCapture | null): Record<string, unknown> | null {
	if (!capture) return null;
	if (capture.kind === 'linux-routing') {
		return {
			kind: capture.kind,
			linuxRule: cloneLinuxRule(capture.linuxRule),
			includeSelfWindowAudio: capture.includeSelfWindowAudio,
		};
	}
	if (capture.kind === 'capture') {
		return {
			kind: capture.kind,
			captureId: capture.captureId,
			includeSelfWindowAudio: capture.includeSelfWindowAudio,
		};
	}
	return {kind: capture.kind};
}

function rememberStartedCapture(capture: NativeAudioStartedCaptureDiagnostic): void {
	lastStartedCapture = {
		...capture,
		linuxRule: cloneLinuxRule(capture.linuxRule),
	};
}

export function getNativeAudioCaptureDiagnosticState(): Record<string, unknown> {
	const started = lastStartedCapture
		? {
				...lastStartedCapture,
				linuxRule: cloneLinuxRule(lastStartedCapture.linuxRule),
			}
		: null;
	return {
		armedCapture: summarizeArmedCapture(armedCapture),
		activeBridge: activeBridge ? {captureId: activeBridge.captureId} : null,
		supersededBridge: supersededBridge ? {captureId: supersededBridge.captureId} : null,
		lastStartedCapture: started,
		lastArmFailure: getLastNativeAudioArmFailure(),
		bridgeStats: getNativeAudioBridgeStats(),
	};
}

function cleanupBridgeAsync(bridge: ActiveNativeAudioBridge, stopRemote: boolean, logContext: string): void {
	void bridge.cleanup(stopRemote).catch((error) => {
		logger.warn(`Failed to clean up native audio bridge after ${logContext}`, {
			captureId: bridge.captureId,
			error,
		});
	});
}

function cleanupManagedBridgeById(captureId: string, stopRemote: boolean, logContext: string): boolean {
	if (activeBridge?.captureId === captureId) {
		const bridge = activeBridge;
		activeBridge = supersededBridge;
		supersededBridge = null;
		cleanupBridgeAsync(bridge, stopRemote, logContext);
		return true;
	}
	if (supersededBridge?.captureId === captureId) {
		const bridge = supersededBridge;
		supersededBridge = null;
		cleanupBridgeAsync(bridge, stopRemote, logContext);
		return true;
	}
	return false;
}

async function stopArmedCapture(capture: ArmedNativeAudioCapture, logContext: string): Promise<void> {
	if (capture.kind === 'self-window-web-audio') {
		releaseSelfWindowScreenShareAudioTrack();
		return;
	}
	if (capture.kind !== 'capture') return;
	await getNativeAudioApi()
		?.stop(capture.captureId)
		.catch((error) => {
			logger.warn(`Failed to stop native audio capture after ${logContext}`, {
				captureId: capture.captureId,
				error,
			});
		});
}

function attachNativeAudioCleanup(
	stream: MediaStream,
	captureId: string,
	handle: NativeAudioBridgeHandle,
): ActiveNativeAudioBridge['cleanup'] {
	const tracks = [...stream.getVideoTracks(), handle.track];
	const cleanupListeners: Array<() => void> = [];
	const restoreStops: Array<() => void> = [];
	let cleanedUp = false;
	bindNativeAudioCaptureLifecycle(captureId);
	const cleanup = async (stopRemote: boolean = true): Promise<void> => {
		if (cleanedUp) return;
		cleanedUp = true;
		for (const removeListener of cleanupListeners.splice(0)) {
			removeListener();
		}
		for (const restoreStop of restoreStops.splice(0)) {
			restoreStop();
		}
		if (activeBridge?.captureId === captureId) {
			activeBridge = null;
		}
		unbindNativeAudioCaptureLifecycle(captureId, false);
		await handle.cleanup(stopRemote);
	};
	const requestCleanup = (): void => {
		unbindNativeAudioCaptureLifecycle(captureId, true);
		if (!cleanupManagedBridgeById(captureId, true, 'track ended')) {
			cleanupBridgeAsync({captureId, cleanup}, true, 'track ended');
		}
	};
	for (const track of tracks) {
		const onEnded = (): void => requestCleanup();
		track.addEventListener('ended', onEnded);
		cleanupListeners.push(() => track.removeEventListener('ended', onEnded));
		restoreStops.push(patchTrackStopForCleanup(track, requestCleanup));
	}
	return cleanup;
}

async function createNativeAudioBridge(captureId: string): Promise<NativeAudioBridgeHandle> {
	if (getGeneratorCtor() && getAudioDataCtor()) {
		return createGeneratorBridge(captureId, (id) => cleanupManagedBridgeById(id, false, 'remote end'));
	}
	logger.info('Falling back to ScriptProcessor native-audio bridge');
	return createScriptProcessorBridge(captureId, (id) => cleanupManagedBridgeById(id, false, 'remote end'));
}

async function createNativeAudioBridgeWithSelfWindowAudio(captureId: string): Promise<NativeAudioBridgeHandle> {
	const nativeHandle = await createNativeAudioBridge(captureId);
	let mixedTrack: MixedSelfWindowAudioTrack;
	try {
		mixedTrack = await mixTrackWithSelfWindowScreenShareAudio(nativeHandle.track, {
			sampleRate: 48000,
			unavailableMessage: 'Self-window app-audio tap unavailable for system audio mix',
		});
	} catch (error) {
		await nativeHandle.cleanup(true).catch((cleanupError) => {
			logger.warn('Failed to clean up native audio bridge after system audio mixer creation failed', {
				captureId,
				error: cleanupError,
			});
		});
		throw error;
	}
	let cleanedUp = false;
	const cleanup = async (stopRemote: boolean = true): Promise<void> => {
		if (cleanedUp) return;
		cleanedUp = true;
		await mixedTrack.cleanup();
		await nativeHandle.cleanup(stopRemote);
	};
	return {
		track: mixedTrack.track,
		cleanup,
	};
}

async function startLinuxNativeAudioCapture(
	linuxRule: NonNullable<NativeAudioStartOptions['linuxRule']>,
): Promise<NativeAudioStartResult | null> {
	const electronApi = getElectronAPI();
	const nativeAudioApi = electronApi?.nativeAudio;
	if (!electronApi || electronApi.platform !== 'linux' || !nativeAudioApi) {
		setLastArmFailure({
			platform: electronApi?.platform,
			reason: !electronApi
				? 'electron-api-unavailable'
				: !nativeAudioApi
					? 'native-audio-api-unavailable'
					: 'unsupported-platform',
		});
		logger.warn('Cannot start Linux native audio capture: native audio API unavailable', {
			hasElectronApi: Boolean(electronApi),
			hasNativeAudioApi: Boolean(nativeAudioApi),
			platform: electronApi?.platform,
		});
		return null;
	}
	if (!(await isNativeAudioAvailable())) {
		const availability = await nativeAudioApi.getAvailability().catch(() => null);
		setLastArmFailure({
			platform: electronApi.platform,
			backend: availability?.backend ?? null,
			reason: availability?.reason ?? 'unavailable',
			detail: availability?.detail ?? null,
		});
		logger.warn('Cannot start Linux native audio capture: native audio addon unavailable', {availability});
		return null;
	}
	try {
		const result = await nativeAudioApi.start({linuxRule});
		rememberStartedCapture({
			captureId: result.captureId,
			startedAtMs: Date.now(),
			platform: electronApi.platform,
			sourceMode: linuxRule.include && linuxRule.include.length > 0 ? 'specific' : 'system',
			linuxRule,
		});
		clearLastArmFailure();
		return result;
	} catch (error) {
		const detail = getNativeAudioErrorDetail(error);
		setLastArmFailure({
			platform: electronApi.platform,
			sourceMode: linuxRule.include && linuxRule.include.length > 0 ? 'specific' : 'system',
			reason: 'native-audio-start-failed',
			detail,
		});
		logger.warn('Cannot start Linux native audio capture: native audio start failed', {error});
		return null;
	}
}

export interface NativeAudioFramePump {
	captureId: string;
	sampleRate: number;
	channels: number;
	cleanup: (stopRemote?: boolean) => Promise<void>;
}

export type NativeAudioFramePumpSource =
	| {
			kind: 'system';
	  }
	| {
			kind: 'window';
			targetPid: number;
	  }
	| {
			kind: 'linux';
			linuxRule: NonNullable<NativeAudioStartOptions['linuxRule']>;
	  };

function createDirectNativeAudioFramePump(
	result: NativeAudioStartResult,
	nativeAudioApi: NativeAudioApi,
	onFrame: (message: NativeAudioFrameMessage) => void | Promise<void>,
	onEnd?: (message: {captureId: string; reason?: string; detail?: string}) => void,
): NativeAudioFramePump {
	const {captureId} = result;
	let cleanedUp = false;
	bindNativeAudioCaptureLifecycle(captureId);
	const unsubscribeFrame = nativeAudioApi.onFrame((message) => {
		if (message.captureId !== captureId || cleanedUp) return;
		if (!isValidAudioFrameMessage(message)) {
			logger.warn('Dropping invalid native screen-share audio frame before native-engine publish', {
				captureId,
				sampleRate: message.sampleRate,
				channels: message.channels,
				byteLength: message.samples?.byteLength,
				timestampUs: message.timestampUs,
			});
			return;
		}
		Promise.resolve(onFrame(message)).catch((error) => {
			logger.warn('Failed to forward native screen-share audio frame', {captureId, error});
		});
	});
	const unsubscribeEnd = nativeAudioApi.onEnd((message) => {
		if (message.captureId !== captureId || cleanedUp) return;
		cleanedUp = true;
		unsubscribeFrame();
		unsubscribeEnd();
		unbindNativeAudioCaptureLifecycle(captureId, true);
		onEnd?.(message);
	});
	const cleanup = async (stopRemote: boolean = true): Promise<void> => {
		if (cleanedUp) return;
		cleanedUp = true;
		unsubscribeFrame();
		unsubscribeEnd();
		unbindNativeAudioCaptureLifecycle(captureId, false);
		if (stopRemote) {
			await nativeAudioApi.stop(captureId).catch((error) => {
				logger.warn('Failed to stop native screen-share audio frame pump', {captureId, error});
			});
		}
	};
	return {captureId, sampleRate: result.sampleRate, channels: result.channels, cleanup};
}

export async function startLinuxNativeAudioFramePump(
	linuxRule: NonNullable<NativeAudioStartOptions['linuxRule']>,
	onFrame: (message: NativeAudioFrameMessage) => void | Promise<void>,
	onEnd?: (message: {captureId: string; reason?: string; detail?: string}) => void,
): Promise<NativeAudioFramePump | null> {
	const result = await startLinuxNativeAudioCapture(linuxRule);
	if (!result) return null;
	const nativeAudioApi = getNativeAudioApi();
	if (!nativeAudioApi) {
		setLastArmFailure({
			platform: 'linux',
			reason: 'native-audio-api-unavailable',
		});
		await getElectronAPI()
			?.nativeAudio?.stop(result.captureId)
			.catch((error) => {
				logger.warn('Failed to stop Linux native audio capture after frame pump setup failed', {
					captureId: result.captureId,
					error,
				});
			});
		return null;
	}
	return createDirectNativeAudioFramePump(result, nativeAudioApi, onFrame, onEnd);
}

async function startSystemNativeAudioFramePump(
	onFrame: (message: NativeAudioFrameMessage) => void | Promise<void>,
	onEnd?: (message: {captureId: string; reason?: string; detail?: string}) => void,
): Promise<NativeAudioFramePump | null> {
	const electronApi = getElectronAPI();
	const nativeAudioApi = electronApi?.nativeAudio;
	const supported = electronApi?.platform === 'darwin' || electronApi?.platform === 'win32';
	if (!electronApi || !supported || !nativeAudioApi) {
		setLastArmFailure({
			platform: electronApi?.platform,
			sourceMode: 'system',
			reason: !electronApi
				? 'electron-api-unavailable'
				: !nativeAudioApi
					? 'native-audio-api-unavailable'
					: 'unsupported-platform',
		});
		logger.warn('Cannot start native system audio frame pump: native audio API unavailable', {
			hasElectronApi: Boolean(electronApi),
			hasNativeAudioApi: Boolean(nativeAudioApi),
			platform: electronApi?.platform,
		});
		return null;
	}
	const availability = await getNativeAudioAvailabilityCached();
	if (!nativeAudioSupportsScope(availability, 'system')) {
		setLastArmFailure({
			platform: electronApi.platform,
			sourceMode: 'system',
			backend: availability?.backend ?? null,
			reason: availability?.reason ?? 'unavailable',
			detail: availability?.detail ?? null,
		});
		logger.warn('Cannot start native system audio frame pump: native audio addon unavailable', {availability});
		return null;
	}
	if (!systemAudioExcludesFluxer(availability)) {
		rejectUnverifiedSystemSelfExclusion(
			electronApi.platform,
			availability,
			'Cannot start native system audio frame pump',
		);
		return null;
	}
	const includeSelfWindowAudio = shouldMixSelfWindowAudioIntoSystemCapture(availability);
	if (includeSelfWindowAudio) {
		setLastArmFailure({
			platform: electronApi.platform,
			sourceMode: 'system',
			backend: availability.backend ?? null,
			reason: 'native-frame-pump-self-window-mix-unavailable',
			detail: availability.detail ?? null,
		});
		logger.warn('Cannot start direct native system audio frame pump when self-window mixing is required', {
			availability,
		});
		return null;
	}
	const winCaptureScope =
		electronApi.platform === 'win32' &&
		availability.capabilities?.processExclude === false &&
		availability.capabilities?.sessionMixer === true
			? 'session-mixer'
			: 'system';
	let result: NativeAudioStartResult;
	try {
		result = await nativeAudioApi.start({
			includeProcessTree: electronApi.platform !== 'win32',
			...(electronApi.platform === 'darwin'
				? {
						macBackend: 'sck' as const,
						macCaptureScope: 'system' as const,
					}
				: {
						winCaptureScope,
					}),
		});
		rememberStartedCapture({
			captureId: result.captureId,
			startedAtMs: Date.now(),
			platform: electronApi.platform,
			sourceMode: 'system',
			...(electronApi.platform === 'darwin'
				? {
						macBackend: 'sck' as const,
						macCaptureScope: 'system' as const,
					}
				: {
						winCaptureScope,
					}),
			includeSelfWindowAudio,
		});
	} catch (error) {
		const detail = getNativeAudioErrorDetail(error);
		setLastArmFailure({
			platform: electronApi.platform,
			sourceMode: 'system',
			reason: 'native-audio-start-failed',
			detail,
		});
		logger.warn('Cannot start native system audio frame pump: native audio start failed', {error});
		return null;
	}
	clearLastArmFailure();
	return createDirectNativeAudioFramePump(result, nativeAudioApi, onFrame, onEnd);
}

async function startWindowNativeAudioFramePump(
	targetPid: number,
	onFrame: (message: NativeAudioFrameMessage) => void | Promise<void>,
	onEnd?: (message: {captureId: string; reason?: string; detail?: string}) => void,
): Promise<NativeAudioFramePump | null> {
	const electronApi = getElectronAPI();
	const nativeAudioApi = electronApi?.nativeAudio;
	const supported =
		electronApi?.platform === 'darwin' || electronApi?.platform === 'win32' || electronApi?.platform === 'linux';
	if (!electronApi || !supported || !nativeAudioApi) {
		setLastArmFailure({
			platform: electronApi?.platform,
			sourceMode: 'specific',
			reason: !electronApi
				? 'electron-api-unavailable'
				: !nativeAudioApi
					? 'native-audio-api-unavailable'
					: 'unsupported-platform',
		});
		logger.warn('Cannot start native window audio frame pump: native audio API unavailable', {
			hasElectronApi: Boolean(electronApi),
			hasNativeAudioApi: Boolean(nativeAudioApi),
			platform: electronApi?.platform,
			targetPid,
		});
		return null;
	}
	if (!Number.isInteger(targetPid) || targetPid <= 0) {
		setLastArmFailure({
			platform: electronApi.platform,
			sourceMode: 'specific',
			reason: 'target-pid-not-resolved',
		});
		logger.warn('Cannot start native window audio frame pump: invalid target PID', {targetPid});
		return null;
	}
	const availability = await getNativeAudioAvailabilityCached();
	if (!nativeAudioSupportsScope(availability, 'process')) {
		setLastArmFailure({
			platform: electronApi.platform,
			sourceMode: 'specific',
			backend: availability?.backend ?? null,
			reason: availability?.reason ?? 'unavailable',
			detail: availability?.detail ?? null,
		});
		logger.warn('Cannot start native window audio frame pump: native audio addon unavailable for process scope', {
			availability,
			targetPid,
		});
		return null;
	}
	let result: NativeAudioStartResult;
	try {
		result = await nativeAudioApi.start({
			targetPid,
			includeProcessTree: true,
		});
		rememberStartedCapture({
			captureId: result.captureId,
			startedAtMs: Date.now(),
			platform: electronApi.platform,
			sourceMode: 'specific',
			targetPid,
		});
	} catch (error) {
		const detail = getNativeAudioErrorDetail(error);
		const refusedSelf = /Refusing to capture native audio from (?:own|Fluxer) process/.test(detail);
		setLastArmFailure({
			platform: electronApi.platform,
			sourceMode: 'specific',
			reason: refusedSelf ? 'native-audio-refused-self' : 'native-audio-start-failed',
			detail,
		});
		if (refusedSelf) {
			logger.info('Native window audio frame pump refused for Fluxer-owned process', {targetPid, detail});
		} else {
			logger.warn('Cannot start native window audio frame pump: native audio start failed', {error, targetPid});
		}
		return null;
	}
	clearLastArmFailure();
	return createDirectNativeAudioFramePump(result, nativeAudioApi, onFrame, onEnd);
}

export async function startNativeAudioFramePump(
	source: NativeAudioFramePumpSource,
	onFrame: (message: NativeAudioFrameMessage) => void | Promise<void>,
	onEnd?: (message: {captureId: string; reason?: string; detail?: string}) => void,
): Promise<NativeAudioFramePump | null> {
	if (source.kind === 'linux') {
		return startLinuxNativeAudioFramePump(source.linuxRule, onFrame, onEnd);
	}
	if (source.kind === 'system') {
		return startSystemNativeAudioFramePump(onFrame, onEnd);
	}
	return startWindowNativeAudioFramePump(source.targetPid, onFrame, onEnd);
}

function installPatchIfNeeded(): void {
	if (patched) return;
	if (!isNativeAudioDesktopPlatform()) return;
	if (typeof navigator === 'undefined' || !navigator.mediaDevices?.getDisplayMedia) return;
	originalGetDisplayMedia = navigator.mediaDevices.getDisplayMedia.bind(navigator.mediaDevices);
	navigator.mediaDevices.getDisplayMedia = async function patchedGetDisplayMedia(
		this: MediaDevices,
		constraints?: DisplayMediaStreamOptions,
	): Promise<MediaStream> {
		const pendingCapture = armedCapture;
		let stream: MediaStream;
		try {
			stream = await originalGetDisplayMedia!(constraints);
		} catch (error) {
			if (pendingCapture && armedCapture === pendingCapture) {
				armedCapture = null;
				await stopArmedCapture(pendingCapture, 'display capture failed');
			}
			throw error;
		}
		if (pendingCapture && armedCapture === pendingCapture) {
			armedCapture = null;
			if (pendingCapture.kind === 'self-window-web-audio') {
				const track = acquireSelfWindowScreenShareAudioTrack();
				if (track) {
					const cleanupListeners: Array<() => void> = [];
					const restoreStops: Array<() => void> = [];
					let cleanedUpSelf = false;
					const cleanupSelf = (): void => {
						if (cleanedUpSelf) return;
						cleanedUpSelf = true;
						for (const removeListener of cleanupListeners.splice(0)) {
							removeListener();
						}
						for (const restoreStop of restoreStops.splice(0)) {
							restoreStop();
						}
						releaseSelfWindowScreenShareAudioTrack(track);
					};
					try {
						await replaceStreamAudioTrack(stream, track);
						for (const otherTrack of [...stream.getVideoTracks(), track]) {
							const onEnded = (): void => cleanupSelf();
							otherTrack.addEventListener('ended', onEnded);
							cleanupListeners.push(() => otherTrack.removeEventListener('ended', onEnded));
							restoreStops.push(patchTrackStopForCleanup(otherTrack, cleanupSelf));
						}
					} catch (error) {
						logger.warn('Failed to attach self-window screen-share audio tap; degrading to video-only', {error});
						cleanupSelf();
					}
				} else {
					logger.warn('Self-window screen-share audio tap unavailable; degrading to video-only');
				}
				return stream;
			}
			let handle: NativeAudioBridgeHandle | null = null;
			let captureId = getArmedCaptureId(pendingCapture);
			try {
				if (!captureId && pendingCapture.kind === 'linux-routing') {
					const result = await startLinuxNativeAudioCapture(pendingCapture.linuxRule);
					if (!result) {
						return stream;
					}
					captureId = result.captureId;
				}
				if (!captureId) {
					return stream;
				}
				const previousBridge = activeBridge;
				handle =
					(pendingCapture.kind === 'capture' || pendingCapture.kind === 'linux-routing') &&
					pendingCapture.includeSelfWindowAudio
						? await createNativeAudioBridgeWithSelfWindowAudio(captureId)
						: await createNativeAudioBridge(captureId);
				await replaceStreamAudioTrack(stream, handle.track);
				const cleanup = attachNativeAudioCleanup(stream, captureId, handle);
				activeBridge = {captureId, cleanup};
				if (previousBridge && previousBridge.captureId !== captureId) {
					supersededBridge = previousBridge;
				}
			} catch (error) {
				logger.warn('Failed to substitute native audio capture into display stream; degrading to video-only', {
					captureId,
					error,
				});
				if (handle) {
					await handle.cleanup(true).catch((cleanupError) => {
						logger.warn('Failed to clean up native audio bridge after substitution failure', {
							captureId,
							error: cleanupError,
						});
					});
				} else if (captureId) {
					await getNativeAudioApi()
						?.stop(captureId)
						.catch((stopError) => {
							logger.warn('Failed to stop native audio capture after substitution failure', {
								captureId,
								error: stopError,
							});
						});
				}
			}
		}
		return stream;
	};
	patched = true;
}

export async function getNativeAudioAvailabilityCached(): Promise<NativeAudioAvailability> {
	if (!isNativeAudioDesktopPlatform()) return unsupportedPlatformAvailability();
	if (!availabilityCache) {
		const nativeAudioApi = getNativeAudioApi();
		availabilityCache = nativeAudioApi
			? nativeAudioApi.getAvailability().catch((error): NativeAudioAvailability => {
					logger.warn('Failed to query native audio availability', {error});
					return {available: false, reason: 'load-failed'};
				})
			: Promise.resolve<NativeAudioAvailability>({available: false, reason: 'unsupported-platform'});
	}
	const availability = await availabilityCache;
	resolvedAvailability = availability;
	if (!availability.available) {
		availabilityCache = null;
	}
	return availability;
}

export function getNativeAudioAvailabilitySnapshot(): NativeAudioAvailability | null {
	if (resolvedAvailability == null) {
		void getNativeAudioAvailabilityCached().catch(() => undefined);
	}
	return resolvedAvailability;
}

function nativeAudioSupportsScope(availability: NativeAudioAvailability, scope: 'process' | 'system'): boolean {
	if (!availability.available) return false;
	return availability.capabilities?.[scope] ?? true;
}

function systemAudioExcludesFluxer(availability: NativeAudioAvailability): boolean {
	if (!availability.available) return false;
	return availability.capabilities?.systemExcludesSelf === true;
}

function rejectUnverifiedSystemSelfExclusion(
	platform: string,
	availability: NativeAudioAvailability,
	context: string,
): void {
	setLastArmFailure({
		platform,
		sourceMode: 'system',
		backend: availability.backend ?? null,
		reason: 'system-audio-self-exclusion-unverified',
		detail: availability.detail ?? null,
	});
	logger.warn(`${context}: self-audio exclusion is not verified`, {availability});
}

export async function isNativeAudioAvailable(): Promise<boolean> {
	const availability = await getNativeAudioAvailabilityCached();
	return availability.available;
}

export async function armNativeAudioForNextCapture(sourceId: string): Promise<boolean> {
	const electronApi = getElectronAPI();
	const nativeAudioApi = electronApi?.nativeAudio;
	if (!electronApi || !nativeAudioApi || !isNativeAudioDesktopPlatform()) {
		setLastArmFailure({
			platform: electronApi?.platform,
			sourceId,
			reason: !electronApi
				? 'electron-api-unavailable'
				: !nativeAudioApi
					? 'native-audio-api-unavailable'
					: 'unsupported-platform',
		});
		logger.warn('Cannot arm per-window audio capture: not a supported desktop platform', {
			hasElectronApi: Boolean(electronApi),
			hasNativeAudioApi: Boolean(nativeAudioApi),
			platform: electronApi?.platform,
		});
		return false;
	}
	if (!sourceId.startsWith('window:')) {
		setLastArmFailure({platform: electronApi.platform, sourceId, reason: 'source-is-not-window'});
		logger.warn('Cannot arm per-window audio capture: source is not a window', {sourceId});
		return false;
	}
	const availability = await getNativeAudioAvailabilityCached();
	if (!nativeAudioSupportsScope(availability, 'process')) {
		setLastArmFailure({
			platform: electronApi.platform,
			sourceId,
			backend: availability?.backend ?? null,
			reason: availability?.reason ?? 'process-scope-unsupported',
			detail: availability?.detail ?? null,
		});
		logger.warn('Cannot arm per-window audio capture: native audio addon unavailable', {
			sourceId,
			availability,
		});
		return false;
	}
	const targetPid = await nativeAudioApi.resolveAudioRootPidForSource(sourceId);
	if (!targetPid) {
		setLastArmFailure({platform: electronApi.platform, sourceId, reason: 'target-pid-not-resolved'});
		logger.warn('Per-window audio target PID could not be resolved', {sourceId});
		return false;
	}
	if (armedCapture) {
		await stopArmedCapture(armedCapture, 'new per-window capture arm');
		armedCapture = null;
	}
	installPatchIfNeeded();
	if (!patched) {
		setLastArmFailure({
			platform: electronApi.platform,
			sourceId,
			reason: 'display-capture-unavailable',
		});
		logger.warn('Cannot arm per-window audio capture: display capture patch unavailable', {sourceId});
		return false;
	}
	let result: NativeAudioStartResult;
	try {
		result = await nativeAudioApi.start({
			targetPid,
			includeProcessTree:
				electronApi.platform === 'win32' || electronApi.platform === 'darwin' || electronApi.platform === 'linux',
		});
		rememberStartedCapture({
			captureId: result.captureId,
			startedAtMs: Date.now(),
			platform: electronApi.platform,
			sourceMode: 'specific',
			targetPid,
		});
	} catch (error) {
		const detail = getNativeAudioErrorDetail(error);
		setLastArmFailure({
			platform: electronApi.platform,
			sourceId,
			reason: 'native-audio-start-failed',
			detail,
		});
		logger.warn('Cannot arm per-window audio capture; attempting system fallback', {sourceId, targetPid, error});
		if (electronApi.platform === 'win32') {
			const fallback = await armNativeSystemAudioForNextCapture();
			if (fallback) return true;
		}
		return false;
	}
	clearLastArmFailure();
	armedCapture = {
		kind: 'capture',
		captureId: result.captureId,
	};
	return true;
}

export async function armNativeSystemAudioForNextCapture(): Promise<boolean> {
	const electronApi = getElectronAPI();
	const nativeAudioApi = electronApi?.nativeAudio;
	const supported = electronApi?.platform === 'darwin' || electronApi?.platform === 'win32';
	if (!electronApi || !supported || !nativeAudioApi) {
		setLastArmFailure({
			platform: electronApi?.platform,
			sourceMode: 'system',
			reason: !electronApi
				? 'electron-api-unavailable'
				: !nativeAudioApi
					? 'native-audio-api-unavailable'
					: 'unsupported-platform',
		});
		logger.warn('Cannot arm native system audio capture: native audio API unavailable', {
			hasElectronApi: Boolean(electronApi),
			hasNativeAudioApi: Boolean(nativeAudioApi),
			platform: electronApi?.platform,
		});
		return false;
	}
	const availability = await getNativeAudioAvailabilityCached();
	if (!nativeAudioSupportsScope(availability, 'system')) {
		setLastArmFailure({
			platform: electronApi.platform,
			sourceMode: 'system',
			backend: availability?.backend ?? null,
			reason: availability?.reason ?? 'unavailable',
			detail: availability?.detail ?? null,
		});
		logger.warn('Cannot arm native system audio capture: native audio addon unavailable', {availability});
		return false;
	}
	if (!systemAudioExcludesFluxer(availability)) {
		rejectUnverifiedSystemSelfExclusion(electronApi.platform, availability, 'Cannot arm native system audio capture');
		return false;
	}
	const includeSelfWindowAudio = shouldMixSelfWindowAudioIntoSystemCapture(availability);
	const winCaptureScope =
		electronApi.platform === 'win32' &&
		availability.capabilities?.processExclude === false &&
		availability.capabilities?.sessionMixer === true
			? 'session-mixer'
			: 'system';
	if (armedCapture) {
		await stopArmedCapture(armedCapture, 'new system capture arm');
		armedCapture = null;
	}
	installPatchIfNeeded();
	if (!patched) {
		setLastArmFailure({
			platform: electronApi.platform,
			sourceMode: 'system',
			reason: 'display-capture-unavailable',
		});
		logger.warn('Cannot arm native system audio capture: display capture patch unavailable');
		return false;
	}
	let result: NativeAudioStartResult;
	try {
		result = await nativeAudioApi.start({
			includeProcessTree: electronApi.platform !== 'win32',
			...(electronApi.platform === 'darwin'
				? {
						macBackend: 'sck' as const,
						macCaptureScope: 'system' as const,
					}
				: {
						winCaptureScope,
					}),
		});
		rememberStartedCapture({
			captureId: result.captureId,
			startedAtMs: Date.now(),
			platform: electronApi.platform,
			sourceMode: 'system',
			...(electronApi.platform === 'darwin'
				? {
						macBackend: 'sck' as const,
						macCaptureScope: 'system' as const,
					}
				: {
						winCaptureScope,
					}),
			includeSelfWindowAudio,
		});
	} catch (error) {
		const detail = getNativeAudioErrorDetail(error);
		setLastArmFailure({
			platform: electronApi.platform,
			sourceMode: 'system',
			reason: 'native-audio-start-failed',
			detail,
		});
		logger.warn('Cannot arm native system audio capture: native audio start failed', {error});
		return false;
	}
	clearLastArmFailure();
	armedCapture = {
		kind: 'capture',
		captureId: result.captureId,
		includeSelfWindowAudio,
	};
	return true;
}

export function armSelfWindowScreenShareAudio(): boolean {
	const electronApi = getElectronAPI();
	if (!electronApi) return false;
	if (armedCapture) {
		void stopArmedCapture(armedCapture, 'self-window audio arm');
		armedCapture = null;
	}
	installPatchIfNeeded();
	if (!patched) {
		setLastArmFailure({
			platform: electronApi.platform,
			reason: 'display-capture-unavailable',
		});
		return false;
	}
	clearLastArmFailure();
	armedCapture = {kind: 'self-window-web-audio'};
	return true;
}

export async function armNativeAudioForLinuxRouting(
	linuxRule: NonNullable<NativeAudioStartOptions['linuxRule']>,
	options: {includeSelfWindowAudio?: boolean} = {},
): Promise<boolean> {
	const electronApi = getElectronAPI();
	const nativeAudioApi = electronApi?.nativeAudio;
	if (!electronApi || electronApi.platform !== 'linux' || !nativeAudioApi) {
		setLastArmFailure({
			platform: electronApi?.platform,
			reason: !electronApi
				? 'electron-api-unavailable'
				: !nativeAudioApi
					? 'native-audio-api-unavailable'
					: 'unsupported-platform',
		});
		logger.warn('Cannot arm Linux native audio capture: native audio API unavailable', {
			hasElectronApi: Boolean(electronApi),
			hasNativeAudioApi: Boolean(nativeAudioApi),
			platform: electronApi?.platform,
		});
		return false;
	}
	if (!(await isNativeAudioAvailable())) {
		const availability = await nativeAudioApi.getAvailability().catch(() => null);
		setLastArmFailure({
			platform: electronApi.platform,
			backend: availability?.backend ?? null,
			reason: availability?.reason ?? 'unavailable',
			detail: availability?.detail ?? null,
		});
		logger.warn('Cannot arm Linux native audio capture: native audio addon unavailable', {availability});
		return false;
	}
	if (armedCapture) {
		await stopArmedCapture(armedCapture, 'new Linux routing capture arm');
		armedCapture = null;
	}
	installPatchIfNeeded();
	if (!patched) {
		setLastArmFailure({
			platform: electronApi.platform,
			sourceMode: linuxRule.include && linuxRule.include.length > 0 ? 'specific' : 'system',
			reason: 'display-capture-unavailable',
		});
		logger.warn('Cannot arm Linux native audio capture: display capture patch unavailable');
		return false;
	}
	armedCapture = {
		kind: 'linux-routing',
		linuxRule,
		includeSelfWindowAudio: options.includeSelfWindowAudio,
	};
	return true;
}

export async function captureNativeAudioTrackForSystem(): Promise<MediaStreamTrack | null> {
	const electronApi = getElectronAPI();
	const nativeAudioApi = electronApi?.nativeAudio;
	const supported = electronApi?.platform === 'darwin' || electronApi?.platform === 'win32';
	if (!electronApi || !supported || !nativeAudioApi) {
		setLastArmFailure({
			platform: electronApi?.platform,
			sourceMode: 'system',
			reason: !electronApi
				? 'electron-api-unavailable'
				: !nativeAudioApi
					? 'native-audio-api-unavailable'
					: 'unsupported-platform',
		});
		logger.warn('Cannot capture native system audio track: native audio API unavailable', {
			hasElectronApi: Boolean(electronApi),
			hasNativeAudioApi: Boolean(nativeAudioApi),
			platform: electronApi?.platform,
		});
		return null;
	}
	const availability = await getNativeAudioAvailabilityCached();
	if (!nativeAudioSupportsScope(availability, 'system')) {
		setLastArmFailure({
			platform: electronApi.platform,
			sourceMode: 'system',
			backend: availability?.backend ?? null,
			reason: availability?.reason ?? 'unavailable',
			detail: availability?.detail ?? null,
		});
		logger.warn('Cannot capture native system audio track: native audio addon unavailable', {availability});
		return null;
	}
	if (!systemAudioExcludesFluxer(availability)) {
		rejectUnverifiedSystemSelfExclusion(electronApi.platform, availability, 'Cannot capture native system audio track');
		return null;
	}
	const includeSelfWindowAudio = shouldMixSelfWindowAudioIntoSystemCapture(availability);
	const winCaptureScope =
		electronApi.platform === 'win32' &&
		availability.capabilities?.processExclude === false &&
		availability.capabilities?.sessionMixer === true
			? 'session-mixer'
			: 'system';
	let result: NativeAudioStartResult;
	try {
		result = await nativeAudioApi.start({
			includeProcessTree: electronApi.platform !== 'win32',
			...(electronApi.platform === 'darwin'
				? {
						macBackend: 'sck' as const,
						macCaptureScope: 'system' as const,
					}
				: {
						winCaptureScope,
					}),
		});
		rememberStartedCapture({
			captureId: result.captureId,
			startedAtMs: Date.now(),
			platform: electronApi.platform,
			sourceMode: 'system',
			...(electronApi.platform === 'darwin'
				? {
						macBackend: 'sck' as const,
						macCaptureScope: 'system' as const,
					}
				: {
						winCaptureScope,
					}),
			includeSelfWindowAudio,
		});
	} catch (error) {
		const detail = getNativeAudioErrorDetail(error);
		setLastArmFailure({
			platform: electronApi.platform,
			sourceMode: 'system',
			reason: 'native-audio-start-failed',
			detail,
		});
		logger.warn('Cannot capture native system audio track: native audio start failed', {error});
		return null;
	}
	clearLastArmFailure();
	let handle: NativeAudioBridgeHandle | null = null;
	try {
		const previousBridge = activeBridge;
		handle = includeSelfWindowAudio
			? await createNativeAudioBridgeWithSelfWindowAudio(result.captureId)
			: await createNativeAudioBridge(result.captureId);
		const cleanup = attachNativeAudioCleanup(new MediaStream([handle.track]), result.captureId, handle);
		activeBridge = {captureId: result.captureId, cleanup};
		if (previousBridge && previousBridge.captureId !== result.captureId) {
			supersededBridge = previousBridge;
		}
		return handle.track;
	} catch (error) {
		logger.warn('Failed to create native system audio media track', {
			captureId: result.captureId,
			error,
		});
		if (handle) {
			await handle.cleanup(true).catch((cleanupError) => {
				logger.warn('Failed to clean up native system audio bridge after track creation failure', {
					captureId: result.captureId,
					error: cleanupError,
				});
			});
		} else {
			await nativeAudioApi.stop(result.captureId).catch((stopError) => {
				logger.warn('Failed to stop native system audio capture after track creation failure', {
					captureId: result.captureId,
					error: stopError,
				});
			});
		}
		setLastArmFailure({
			platform: electronApi.platform,
			sourceMode: 'system',
			reason: 'native-audio-substitution-failed',
			detail: error instanceof Error ? error.message : String(error),
		});
		return null;
	}
}

export async function captureNativeAudioTrackForWindowPid(targetPid: number): Promise<MediaStreamTrack | null> {
	const electronApi = getElectronAPI();
	const nativeAudioApi = electronApi?.nativeAudio;
	const supported =
		electronApi?.platform === 'darwin' || electronApi?.platform === 'win32' || electronApi?.platform === 'linux';
	if (!electronApi || !supported || !nativeAudioApi) {
		setLastArmFailure({
			platform: electronApi?.platform,
			sourceMode: 'specific',
			reason: !electronApi
				? 'electron-api-unavailable'
				: !nativeAudioApi
					? 'native-audio-api-unavailable'
					: 'unsupported-platform',
		});
		logger.warn('Cannot capture native window audio track: native audio API unavailable', {
			hasElectronApi: Boolean(electronApi),
			hasNativeAudioApi: Boolean(nativeAudioApi),
			platform: electronApi?.platform,
			targetPid,
		});
		return null;
	}
	if (!Number.isInteger(targetPid) || targetPid <= 0) {
		setLastArmFailure({
			platform: electronApi.platform,
			sourceMode: 'specific',
			reason: 'target-pid-not-resolved',
		});
		logger.warn('Cannot capture native window audio track: invalid target PID', {targetPid});
		return null;
	}
	const availability = await getNativeAudioAvailabilityCached();
	if (!nativeAudioSupportsScope(availability, 'process')) {
		setLastArmFailure({
			platform: electronApi.platform,
			sourceMode: 'specific',
			backend: availability?.backend ?? null,
			reason: availability?.reason ?? 'unavailable',
			detail: availability?.detail ?? null,
		});
		logger.warn('Cannot capture native window audio track: native audio addon unavailable for process scope', {
			availability,
			targetPid,
		});
		return null;
	}
	let result: NativeAudioStartResult;
	try {
		result = await nativeAudioApi.start({
			targetPid,
			includeProcessTree: true,
		});
		rememberStartedCapture({
			captureId: result.captureId,
			startedAtMs: Date.now(),
			platform: electronApi.platform,
			sourceMode: 'specific',
			targetPid,
		});
	} catch (error) {
		const detail = getNativeAudioErrorDetail(error);
		const refusedSelf = /Refusing to capture native audio from (?:own|Fluxer) process/.test(detail);
		setLastArmFailure({
			platform: electronApi.platform,
			sourceMode: 'specific',
			reason: refusedSelf ? 'native-audio-refused-self' : 'native-audio-start-failed',
			detail,
		});
		if (refusedSelf) {
			logger.info('Native window audio capture refused for Fluxer-owned process', {targetPid, detail});
		} else {
			logger.warn('Cannot capture native window audio track: native audio start failed', {error, targetPid});
		}
		return null;
	}
	clearLastArmFailure();
	let handle: NativeAudioBridgeHandle | null = null;
	try {
		const previousBridge = activeBridge;
		handle = await createNativeAudioBridge(result.captureId);
		const cleanup = attachNativeAudioCleanup(new MediaStream([handle.track]), result.captureId, handle);
		activeBridge = {captureId: result.captureId, cleanup};
		if (previousBridge && previousBridge.captureId !== result.captureId) {
			supersededBridge = previousBridge;
		}
		return handle.track;
	} catch (error) {
		logger.warn('Failed to create native window audio media track', {
			captureId: result.captureId,
			error,
			targetPid,
		});
		if (handle) {
			await handle.cleanup(true).catch((cleanupError) => {
				logger.warn('Failed to clean up native window audio bridge after track creation failure', {
					captureId: result.captureId,
					error: cleanupError,
				});
			});
		} else {
			await nativeAudioApi.stop(result.captureId).catch((stopError) => {
				logger.warn('Failed to stop native window audio capture after track creation failure', {
					captureId: result.captureId,
					error: stopError,
				});
			});
		}
		setLastArmFailure({
			platform: electronApi.platform,
			sourceMode: 'specific',
			reason: 'native-audio-substitution-failed',
			detail: error instanceof Error ? error.message : String(error),
		});
		return null;
	}
}

export function captureNativeAudioTrackForSelfWindow(): MediaStreamTrack | null {
	const electronApi = getElectronAPI();
	if (!electronApi) {
		setLastArmFailure({
			platform: null,
			reason: 'electron-api-unavailable',
		});
		return null;
	}
	const track = acquireSelfWindowScreenShareAudioTrack();
	if (!track) {
		setLastArmFailure({
			platform: electronApi.platform,
			reason: 'self-window-audio-track-unavailable',
		});
		logger.warn('Self-window screen-share audio tap unavailable');
		return null;
	}
	clearLastArmFailure();
	const restoreStop = patchTrackStopForCleanup(track, () => {
		releaseSelfWindowScreenShareAudioTrack(track);
	});
	const onEnded = (): void => {
		track.removeEventListener('ended', onEnded);
		restoreStop();
		releaseSelfWindowScreenShareAudioTrack(track);
	};
	track.addEventListener('ended', onEnded);
	return track;
}

export type LinuxNativeAudioReconfigureResult = 'unchanged' | 'updated' | 'unsupported';

export async function reconfigureLinuxNativeAudioRouting(
	linuxRule: NonNullable<NativeAudioStartOptions['linuxRule']>,
	options: {includeSelfWindowAudio?: boolean} = {},
): Promise<LinuxNativeAudioReconfigureResult> {
	const bridge = activeBridge;
	if (!bridge) return 'unsupported';
	if (bridge.linuxRule === undefined) return 'unsupported';
	if (Boolean(bridge.includeSelfWindowAudio) !== (options.includeSelfWindowAudio === true)) return 'unsupported';
	if (nativeAudioRoutingRulesEqual(bridge.linuxRule, linuxRule)) return 'unchanged';
	const nativeAudioApi = getNativeAudioApi();
	if (typeof nativeAudioApi?.setRule !== 'function') return 'unsupported';
	let applied = false;
	try {
		applied = await nativeAudioApi.setRule(bridge.captureId, linuxRule);
	} catch (error) {
		logger.warn('Failed to reconfigure Linux native audio routing in place', {
			captureId: bridge.captureId,
			error,
		});
		return 'unsupported';
	}
	if (!applied) return 'unsupported';
	if (activeBridge !== bridge) return 'unsupported';
	activeBridge = {...bridge, linuxRule};
	return 'updated';
}

export async function captureNativeAudioTrackForLinuxRouting(
	linuxRule: NonNullable<NativeAudioStartOptions['linuxRule']>,
	options: {includeSelfWindowAudio?: boolean} = {},
): Promise<MediaStreamTrack | null> {
	const result = await startLinuxNativeAudioCapture(linuxRule);
	if (!result) return null;
	let handle: NativeAudioBridgeHandle | null = null;
	try {
		const previousBridge = activeBridge;
		handle = options.includeSelfWindowAudio
			? await createNativeAudioBridgeWithSelfWindowAudio(result.captureId)
			: await createNativeAudioBridge(result.captureId);
		const cleanup = attachNativeAudioCleanup(new MediaStream([handle.track]), result.captureId, handle);
		activeBridge = {
			captureId: result.captureId,
			cleanup,
			linuxRule,
			includeSelfWindowAudio: options.includeSelfWindowAudio === true,
		};
		if (previousBridge && previousBridge.captureId !== result.captureId) {
			supersededBridge = previousBridge;
		}
		return handle.track;
	} catch (error) {
		logger.warn('Failed to create Linux native audio media track', {
			captureId: result.captureId,
			error,
		});
		if (handle) {
			await handle.cleanup(true).catch((cleanupError) => {
				logger.warn('Failed to clean up Linux native audio bridge after track creation failure', {
					captureId: result.captureId,
					error: cleanupError,
				});
			});
		} else {
			await getNativeAudioApi()
				?.stop(result.captureId)
				.catch((stopError) => {
					logger.warn('Failed to stop Linux native audio capture after track creation failure', {
						captureId: result.captureId,
						error: stopError,
					});
				});
		}
		setLastArmFailure({
			platform: 'linux',
			sourceMode: linuxRule.include && linuxRule.include.length > 0 ? 'specific' : 'system',
			reason: 'native-audio-substitution-failed',
			detail: error instanceof Error ? error.message : String(error),
		});
		return null;
	}
}

export function disarmPendingNativeAudio(): void {
	const nativeAudioApi = getNativeAudioApi();
	if (!nativeAudioApi || !armedCapture) {
		armedCapture = null;
		return;
	}
	const capture = armedCapture;
	armedCapture = null;
	void stopArmedCapture(capture, 'pending capture cancellation');
}

export function commitNativeAudioBridgeReplacement(): void {
	const bridge = supersededBridge;
	if (!bridge) return;
	supersededBridge = null;
	cleanupBridgeAsync(bridge, true, 'bridge replacement commit');
}

export function disarmNativeAudio(): void {
	const bridge = activeBridge;
	if (bridge) {
		activeBridge = null;
		cleanupBridgeAsync(bridge, true, 'explicit disarm');
	}
	const previousBridge = supersededBridge;
	if (previousBridge) {
		supersededBridge = null;
		cleanupBridgeAsync(previousBridge, true, 'explicit disarm');
	}
	disarmPendingNativeAudio();
	releaseSelfWindowScreenShareAudioTrack();
}

export function resetNativeAudioAvailabilityCache(): void {
	availabilityCache = null;
	resolvedAvailability = null;
}

export function resetNativeAudioCaptureBridgeForTests(): void {
	disarmNativeAudio();
	resetNativeAudioAvailabilityCache();
	if (patched && originalGetDisplayMedia && navigator.mediaDevices) {
		navigator.mediaDevices.getDisplayMedia = originalGetDisplayMedia;
	}
	originalGetDisplayMedia = null;
	patched = false;
	armedCapture = null;
	lastArmFailure = null;
	activeBridge = null;
	supersededBridge = null;
	sourceLifecycleBridge = null;
	lifecycleBoundCaptureIds.clear();
}
