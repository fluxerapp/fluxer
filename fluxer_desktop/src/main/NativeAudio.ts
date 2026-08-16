// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {createRequire} from 'node:module';
import {createChildLogger} from '@electron/common/Logger';
import type {
	NativeAudioApplication,
	NativeAudioAvailability,
	NativeAudioEndReason,
	NativeAudioRoutingGraphResult,
	NativeAudioStartOptions,
	NativeAudioStartResult,
	VirtmicLinkOptions,
	VirtmicNode,
	VirtmicRoutingGraph,
} from '@electron/common/Types';
import {getNativeAudioMode} from '@electron/main/LaunchOptions';
import {ipcMain} from 'electron';
import {buildFluxerAudioExcludePatterns, isKnownFluxerAudioProcessPid} from './FluxerAudioIdentity';
import {resolveVirtmicWindowPid} from './LinuxAudioCapture';
import {parseWindowSourceToken as parseDesktopWindowSourceToken} from './LinuxAudioCaptureHelpers';
import {getTccStatus} from './MacTcc';
import {
	audioFrameDebugDetails,
	isValidAudioFrame,
	isValidLinuxRule,
	isValidTargetPid,
	normalizeTimestampUs,
} from './NativeAudioValidation';

const logger = createChildLogger('NativeAudio');
const requireModule = createRequire(import.meta.url);

interface NativeCaptureInstance {
	on(event: 'frame', listener: (frame: unknown) => void): this;
	on(event: 'error', listener: (error: Error) => void): this;
	on(event: 'closed', listener: () => void): this;
	removeListener(event: 'frame', listener: (frame: unknown) => void): this;
	removeListener(event: 'error', listener: (error: Error) => void): this;
	removeListener(event: 'closed', listener: () => void): this;
	start(): Promise<void> | void;
	stop(): Promise<void> | void;
	routingGraph?: () => VirtmicRoutingGraph | null;
}

interface WindowsNativeAudioModule {
	isSupported: () => boolean;
	getBackendInfo?: () => {
		supported: boolean;
		reason?: string;
		processSupported?: boolean;
		systemSupported?: boolean;
		systemExcludesSelf?: boolean;
		processIncludeSupported?: boolean;
		processExcludeSupported?: boolean;
		sessionMixerSupported?: boolean;
		systemLoopbackMode?: string;
		minWindowsBuild?: number;
		minWindowsVersionLabel?: string;
		detectedWindowsBuild?: number;
	};
	pidFromHwnd: (hwnd: bigint) => number;
	resolveAudioRootPid: (pid: number) => number;
	ProcessLoopback: new (
		pid: number,
		options?: {
			includeProcessTree?: boolean;
			captureScope?: 'process' | 'system' | 'session-mixer';
			winCaptureScope?: 'process' | 'system' | 'session-mixer';
		},
	) => NativeCaptureInstance;
	loadError?: Error | null;
}

interface MacApplicationDescriptor {
	pid: number;
	bundleId?: string;
	name: string;
}

interface MacBackendAvailability {
	sck?: {
		supported: boolean;
		macosVersion?: string;
	};
	coreaudio?: {
		supported: boolean;
	};
	screenPermission?: string;
	audioPermission?: string;
}

interface MacNativeAudioModule {
	listAudibleApplications: () => Promise<Array<MacApplicationDescriptor>>;
	getBackendAvailability: () => Promise<MacBackendAvailability>;
	pidFromWindowId: (windowId: number) => number;
	ProcessLoopback: new (
		pid: number,
		options?: {
			excludeSelf?: boolean;
			includeProcessTree?: boolean;
			backend?: 'sck' | 'coreaudio' | 'auto';
			captureScope?: 'process' | 'system';
			macCaptureScope?: 'process' | 'system';
		},
	) => NativeCaptureInstance;
	loadError?: Error | null;
}

interface LinuxRoutingRule extends VirtmicLinkOptions {
	include?: Array<VirtmicNode>;
	exclude?: Array<VirtmicNode>;
	onlySpeakers?: boolean;
	onlyDefaultSpeakers?: boolean;
}

interface LinuxNativeAudioModule {
	pipeWireAvailable: () => boolean;
	audioBackend?: () => 'pipewire' | 'none';
	ProcessLoopback: new (
		target:
			| number
			| {
					linuxRule: LinuxRoutingRule;
			  },
		options?: {
			includeProcessTree?: boolean;
			ignoreDevices?: boolean;
		},
	) => NativeCaptureInstance;
}

interface NativeAddonLoadResult {
	addon?: LinuxNativeAudioModule | MacNativeAudioModule | WindowsNativeAudioModule;
	availability: NativeAudioAvailability;
	platform: NodeJS.Platform;
}

interface ActiveNativeAudioSession {
	captureId: string;
	capture: NativeCaptureInstance;
	sender: Electron.WebContents;
	onFrame: (frame: unknown) => void;
	onError: (error: Error) => void;
	onClosed: () => void;
	onSenderDestroyed: () => void;
	finalized: boolean;
	stopping?: Promise<void>;
	stoppingReason?: NativeAudioEndReason;
	stoppingDetail?: string;
}

const MAX_NATIVE_AUDIO_SESSIONS_PER_SENDER = 2;

let cachedLoadResult: NativeAddonLoadResult | undefined;
let handlersRegistered = false;

const activeSessions = new Map<string, ActiveNativeAudioSession>();
const activeSessionIdsBySenderId = new Map<number, Set<string>>();

function isModuleNotFoundError(error: unknown): boolean {
	const message = String(
		(
			error as {
				message?: string;
			}
		)?.message ??
			error ??
			'',
	).toLowerCase();
	return message.includes('cannot find module') || message.includes('module_not_found');
}

type LinuxRoutingPattern = Record<string, string>;

function appendUniquePattern(out: Array<LinuxRoutingPattern>, pattern: LinuxRoutingPattern): void {
	if (Object.keys(pattern).length === 0) return;
	const key = JSON.stringify(pattern);
	if (!out.some((existing) => JSON.stringify(existing) === key)) {
		out.push(pattern);
	}
}

function normalizeLinuxRoutingRule(rule: LinuxRoutingRule): LinuxRoutingRule {
	const include = rule.include?.map((pattern) => ({...pattern})) ?? [];
	if (include.length === 0 && rule.onlySpeakers === false) {
		appendUniquePattern(include, {'media.class': 'Stream/Output/Audio'});
	}
	const exclude: Array<LinuxRoutingPattern> = [];
	for (const pattern of rule.exclude ?? []) {
		appendUniquePattern(exclude, {...pattern});
	}
	for (const pattern of buildFluxerAudioExcludePatterns()) {
		appendUniquePattern(exclude, pattern);
	}
	if (rule.ignoreInputMedia ?? true) {
		appendUniquePattern(exclude, {'media.class': 'Stream/Input/Audio'});
	}
	if (rule.ignoreVirtual) {
		appendUniquePattern(exclude, {'node.virtual': 'true'});
	}
	return {
		...rule,
		include,
		exclude,
	};
}

function describeAddonLoadError(error: unknown): string {
	if (error instanceof Error) {
		const code = (error as Error & {code?: string}).code;
		const message = error.message ?? '';
		return code ? `${code}: ${message}` : message;
	}
	return String(error);
}

function loadNativeAudioAddon(): NativeAddonLoadResult {
	if (cachedLoadResult) return cachedLoadResult;
	if (getNativeAudioMode(process.argv) === 'off') {
		cachedLoadResult = {
			platform: process.platform,
			availability: {
				available: false,
				reason: 'disabled-by-launch',
				detail: 'Native audio disabled by launch diagnostics',
			},
		};
		return cachedLoadResult;
	}
	if (process.platform === 'win32') {
		try {
			const addon = requireModule('@fluxer/win-process-loopback') as WindowsNativeAudioModule;
			if (addon.loadError) {
				const detail = describeAddonLoadError(addon.loadError);
				logger.warn('Windows native audio addon reported load error', addon.loadError);
				cachedLoadResult = {
					platform: process.platform,
					availability: {
						available: false,
						backend: 'windows-wasapi-loopback',
						reason: 'load-failed',
						detail,
					},
				};
				return cachedLoadResult;
			}
			const backendInfo = addon.getBackendInfo?.();
			const processInclude =
				backendInfo?.processIncludeSupported ?? backendInfo?.processSupported ?? addon.isSupported();
			const processExclude =
				backendInfo?.processExcludeSupported ?? backendInfo?.systemLoopbackMode === 'process-exclude';
			const sessionMixer = backendInfo?.sessionMixerSupported ?? processInclude;
			const processSupported = backendInfo?.processSupported ?? processInclude;
			const systemSupported = backendInfo?.systemSupported ?? (processExclude || sessionMixer);
			const systemExcludesSelf = backendInfo?.systemExcludesSelf ?? (processExclude || sessionMixer);
			const systemLoopbackMode =
				backendInfo?.systemLoopbackMode ??
				(processExclude ? 'process-exclude' : sessionMixer ? 'session-mixer' : 'unavailable');
			if (!processSupported && !systemSupported) {
				cachedLoadResult = {
					platform: process.platform,
					availability: {
						available: false,
						backend: 'windows-wasapi-loopback',
						reason: 'os-version-too-old',
						detail: backendInfo?.reason,
						capabilities: {
							process: false,
							system: false,
							systemExcludesSelf: false,
							processInclude: false,
							processExclude: false,
							sessionMixer: false,
							systemLoopbackMode: 'unavailable',
						},
					},
				};
				return cachedLoadResult;
			}
			cachedLoadResult = {
				platform: process.platform,
				addon,
				availability: {
					available: true,
					backend: 'windows-wasapi-loopback',
					detail: backendInfo?.reason,
					capabilities: {
						process: processSupported,
						system: systemSupported,
						systemExcludesSelf,
						processInclude,
						processExclude,
						sessionMixer,
						systemLoopbackMode:
							systemLoopbackMode === 'process-exclude' || systemLoopbackMode === 'session-mixer'
								? systemLoopbackMode
								: 'unavailable',
					},
				},
			};
			return cachedLoadResult;
		} catch (error) {
			const detail = describeAddonLoadError(error);
			const reason = isModuleNotFoundError(error) ? 'addon-not-installed' : 'load-failed';
			logger.warn('Failed to load Windows native audio addon', error);
			cachedLoadResult = {
				platform: process.platform,
				availability: {
					available: false,
					backend: 'windows-wasapi-loopback',
					reason,
					detail,
				},
			};
			return cachedLoadResult;
		}
	}
	if (process.platform === 'darwin') {
		try {
			const addon = requireModule('@fluxer/mac-app-audio') as MacNativeAudioModule;
			if (addon.loadError) {
				const detail = describeAddonLoadError(addon.loadError);
				logger.warn('macOS native audio addon reported load error', addon.loadError);
				cachedLoadResult = {
					platform: process.platform,
					availability: {
						available: false,
						backend: 'macos-sck',
						reason: 'load-failed',
						detail,
					},
				};
				return cachedLoadResult;
			}
			cachedLoadResult = {
				platform: process.platform,
				addon,
				availability: {
					available: true,
					backend: 'macos-sck',
				},
			};
			return cachedLoadResult;
		} catch (error) {
			const detail = describeAddonLoadError(error);
			const reason = isModuleNotFoundError(error) ? 'addon-not-installed' : 'load-failed';
			logger.warn('Failed to load macOS native audio addon', error);
			cachedLoadResult = {
				platform: process.platform,
				availability: {
					available: false,
					backend: 'macos-sck',
					reason,
					detail,
				},
			};
			return cachedLoadResult;
		}
	}
	if (process.platform === 'linux') {
		try {
			const addon = requireModule('@fluxer/linux-audio-capture') as LinuxNativeAudioModule;
			if (typeof addon.pipeWireAvailable !== 'function' || typeof addon.ProcessLoopback !== 'function') {
				cachedLoadResult = {
					platform: process.platform,
					availability: {
						available: false,
						backend: 'linux-pipewire',
						reason: 'load-failed',
					},
				};
				return cachedLoadResult;
			}
			const pipewireAvailable = addon.pipeWireAvailable();
			const backend = addon.audioBackend?.() ?? (pipewireAvailable ? 'pipewire' : 'none');
			if (!pipewireAvailable || backend !== 'pipewire') {
				cachedLoadResult = {
					platform: process.platform,
					availability: {
						available: false,
						backend: 'linux-pipewire',
						reason: 'no-pipewire',
					},
				};
				return cachedLoadResult;
			}
			cachedLoadResult = {
				platform: process.platform,
				addon,
				availability: {
					available: true,
					backend: 'linux-pipewire',
					capabilities: {
						process: true,
						system: true,
						systemExcludesSelf: true,
					},
				},
			};
			return cachedLoadResult;
		} catch (error) {
			const detail = describeAddonLoadError(error);
			const reason = isModuleNotFoundError(error) ? 'addon-not-installed' : 'load-failed';
			logger.warn('Failed to load Linux native audio addon', error);
			cachedLoadResult = {
				platform: process.platform,
				availability: {
					available: false,
					backend: 'linux-pipewire',
					reason,
					detail,
				},
			};
			return cachedLoadResult;
		}
	}
	cachedLoadResult = {
		platform: process.platform,
		availability: {
			available: false,
			reason: 'unsupported-platform',
		},
	};
	return cachedLoadResult;
}

function getMacScreenPermissionStatus(): string | null {
	try {
		return getTccStatus('screen-recording');
	} catch (error) {
		logger.debug('Failed to read macOS screen capture status', error);
		return null;
	}
}

async function getNativeAudioAvailability(): Promise<NativeAudioAvailability> {
	const loadResult = loadNativeAudioAddon();
	if (!loadResult.availability.available || !loadResult.addon) {
		return loadResult.availability;
	}
	if (loadResult.platform !== 'darwin') {
		return loadResult.availability;
	}
	try {
		const addon = loadResult.addon as MacNativeAudioModule;
		const backendAvailability = await addon.getBackendAvailability();
		const coreAudioSupported = backendAvailability.coreaudio?.supported === true;
		const sckSupported = backendAvailability.sck?.supported === true;
		const backend = sckSupported ? 'macos-sck' : 'macos-coreaudio';
		const capabilities = {
			process: sckSupported || coreAudioSupported,
			system: sckSupported || coreAudioSupported,
			systemExcludesSelf: sckSupported || coreAudioSupported,
		};
		if (!coreAudioSupported && !sckSupported) {
			return {
				available: false,
				backend,
				reason: 'os-version-too-old',
				detail: backendAvailability.sck?.macosVersion,
				capabilities,
			};
		}
		const screenPermissionStatus = getMacScreenPermissionStatus();
		if (screenPermissionStatus === 'denied' || screenPermissionStatus === 'restricted') {
			return {
				available: false,
				backend,
				reason: 'permission-denied',
				detail: `screen:${screenPermissionStatus}`,
				capabilities,
			};
		}
		return {
			available: true,
			backend,
			detail: backendAvailability.sck?.macosVersion,
			capabilities,
		};
	} catch (error) {
		logger.warn('Failed to query macOS native audio availability', error);
		return {
			available: false,
			backend: 'macos-sck',
			reason: 'load-failed',
		};
	}
}

async function listNativeAudioApplications(): Promise<Array<NativeAudioApplication>> {
	const loadResult = loadNativeAudioAddon();
	if (!loadResult.availability.available || !loadResult.addon) {
		return [];
	}
	if (loadResult.platform !== 'darwin') {
		return [];
	}
	try {
		const addon = loadResult.addon as MacNativeAudioModule;
		const applications = await addon.listAudibleApplications();
		return applications
			.filter((application) => Number.isFinite(application.pid) && application.pid > 0)
			.map((application) => ({
				pid: application.pid,
				identifier: application.bundleId || `pid:${application.pid}`,
				name: application.name || application.bundleId || `PID ${application.pid}`,
			}));
	} catch (error) {
		logger.warn('Failed to list native audio applications', error);
		return [];
	}
}

async function resolveAudioRootPidForSource(sourceId: unknown): Promise<number | null> {
	const windowToken = parseDesktopWindowSourceToken(sourceId);
	if (!windowToken) {
		return null;
	}
	const loadResult = loadNativeAudioAddon();
	if (!loadResult.availability.available || !loadResult.addon) {
		return null;
	}
	try {
		if (loadResult.platform === 'win32') {
			const addon = loadResult.addon as WindowsNativeAudioModule;
			const pid = addon.pidFromHwnd(BigInt(windowToken));
			if (!Number.isFinite(pid) || pid <= 0) return null;
			if (isKnownFluxerAudioProcessPid(pid)) return null;
			const rootPid = addon.resolveAudioRootPid(pid);
			const resolved = Number.isFinite(rootPid) && rootPid > 0 ? rootPid : pid;
			return isKnownFluxerAudioProcessPid(resolved) ? null : resolved;
		}
		if (loadResult.platform === 'darwin') {
			const addon = loadResult.addon as MacNativeAudioModule;
			const pid = addon.pidFromWindowId(Number(windowToken));
			if (!Number.isFinite(pid) || pid <= 0) return null;
			return isKnownFluxerAudioProcessPid(pid) ? null : pid;
		}
		if (loadResult.platform === 'linux') {
			return resolveVirtmicWindowPid(sourceId);
		}
	} catch (error) {
		logger.warn('Failed to resolve native-audio source PID', {sourceId, error});
	}
	return null;
}

function copySamplesBuffer(samples: Float32Array): ArrayBuffer {
	const copy = new Float32Array(samples.length);
	copy.set(samples);
	return copy.buffer;
}

function removeSessionListeners(session: ActiveNativeAudioSession): void {
	session.capture.removeListener('frame', session.onFrame);
	session.capture.removeListener('error', session.onError);
	session.capture.removeListener('closed', session.onClosed);
	session.sender.removeListener('destroyed', session.onSenderDestroyed);
}

function rememberSenderSession(senderId: number, captureId: string): void {
	let senderSessions = activeSessionIdsBySenderId.get(senderId);
	if (!senderSessions) {
		senderSessions = new Set();
		activeSessionIdsBySenderId.set(senderId, senderSessions);
	}
	senderSessions.add(captureId);
}

function forgetSenderSession(senderId: number, captureId: string): void {
	const senderSessions = activeSessionIdsBySenderId.get(senderId);
	if (!senderSessions) return;
	senderSessions.delete(captureId);
	if (senderSessions.size === 0) {
		activeSessionIdsBySenderId.delete(senderId);
	}
}

function finalizeSession(session: ActiveNativeAudioSession, reason: NativeAudioEndReason, detail?: string): void {
	if (session.finalized) return;
	session.finalized = true;
	removeSessionListeners(session);
	activeSessions.delete(session.captureId);
	forgetSenderSession(session.sender.id, session.captureId);
	if (!session.sender.isDestroyed()) {
		try {
			session.sender.send('native-audio:end', {
				captureId: session.captureId,
				reason,
				detail,
			});
		} catch (error) {
			logger.warn('Failed to send native audio end event to renderer', {
				captureId: session.captureId,
				reason,
				error,
			});
		}
	}
}

async function stopActiveSession(
	session: ActiveNativeAudioSession,
	reason: NativeAudioEndReason = 'stopped',
	detail?: string,
): Promise<void> {
	if (session.finalized) return;
	if (session.stopping) return session.stopping;
	session.stoppingReason = reason;
	session.stoppingDetail = detail;
	session.stopping = (async () => {
		try {
			await Promise.resolve(session.capture.stop());
		} catch (error) {
			logger.warn('Failed to stop native audio capture', {captureId: session.captureId, error});
		} finally {
			finalizeSession(session, reason, detail);
		}
	})();
	return session.stopping;
}

async function stopCaptureById(
	captureId: string,
	reason: NativeAudioEndReason = 'stopped',
	detail?: string,
): Promise<void> {
	const session = activeSessions.get(captureId);
	if (!session) return;
	await stopActiveSession(session, reason, detail);
}

async function makeRoomForSenderSession(senderId: number): Promise<void> {
	const senderSessions = activeSessionIdsBySenderId.get(senderId);
	if (!senderSessions) return;
	while (senderSessions.size >= MAX_NATIVE_AUDIO_SESSIONS_PER_SENDER) {
		const oldestCaptureId = senderSessions.values().next().value as string | undefined;
		if (!oldestCaptureId) return;
		await stopCaptureById(oldestCaptureId, 'stopped', 'sender-session-limit');
	}
}

async function startNativeAudioCapture(
	sender: Electron.WebContents,
	options: NativeAudioStartOptions,
): Promise<NativeAudioStartResult> {
	if (sender.isDestroyed()) {
		throw new Error('Cannot start native audio capture for destroyed renderer');
	}
	const loadResult = loadNativeAudioAddon();
	const linuxRule =
		loadResult.platform === 'linux' && isValidLinuxRule(options?.linuxRule)
			? normalizeLinuxRoutingRule(options.linuxRule)
			: null;
	const macCaptureScope =
		loadResult.platform === 'darwin' && options?.macCaptureScope === 'system' ? 'system' : 'process';
	const macSystemCapture = loadResult.platform === 'darwin' && macCaptureScope === 'system';
	const winCaptureScope =
		loadResult.platform === 'win32' && options?.winCaptureScope === 'session-mixer'
			? 'session-mixer'
			: loadResult.platform === 'win32' && options?.winCaptureScope === 'system'
				? 'system'
				: 'process';
	const winSystemCapture =
		loadResult.platform === 'win32' && (winCaptureScope === 'system' || winCaptureScope === 'session-mixer');
	const targetPid = options?.targetPid;
	if (!options || (!linuxRule && !macSystemCapture && !winSystemCapture && !isValidTargetPid(targetPid))) {
		throw new Error('Invalid native audio target PID');
	}
	if (!macSystemCapture && !winSystemCapture && targetPid === process.pid) {
		throw new Error('Refusing to capture native audio from own process');
	}
	if (
		!macSystemCapture &&
		!winSystemCapture &&
		isValidTargetPid(targetPid) &&
		isKnownFluxerAudioProcessPid(targetPid)
	) {
		throw new Error('Refusing to capture native audio from Fluxer process');
	}
	const availability = await getNativeAudioAvailability();
	if (!availability.available) {
		throw new Error(`Native audio capture unavailable (${availability.reason ?? 'unknown'})`);
	}
	const requestedScope: 'process' | 'system' =
		winSystemCapture || macSystemCapture
			? 'system'
			: linuxRule
				? linuxRule.include && linuxRule.include.length > 0
					? 'process'
					: 'system'
				: 'process';
	const capability = availability.capabilities?.[requestedScope];
	if (capability === false) {
		throw new Error(
			`Native ${loadResult.platform} ${requestedScope} audio capture unavailable (${availability.reason ?? 'unsupported-scope'})`,
		);
	}
	if (requestedScope === 'system' && availability.capabilities?.systemExcludesSelf !== true) {
		throw new Error(`Native ${loadResult.platform} system audio capture does not guarantee Fluxer self-exclusion`);
	}
	const effectiveWinCaptureScope =
		loadResult.platform === 'win32' &&
		winCaptureScope === 'system' &&
		availability.capabilities?.processExclude === false &&
		availability.capabilities?.sessionMixer === true
			? 'session-mixer'
			: winCaptureScope;
	if (sender.isDestroyed()) {
		throw new Error('Cannot start native audio capture for destroyed renderer');
	}
	if (!loadResult.addon) {
		throw new Error('Native audio addon unavailable');
	}
	await makeRoomForSenderSession(sender.id);
	if (sender.isDestroyed()) {
		throw new Error('Cannot start native audio capture for destroyed renderer');
	}
	const captureId = randomUUID();
	const targetPidForCapture = macSystemCapture || winSystemCapture ? process.pid : targetPid;
	let capture: NativeCaptureInstance;
	if (loadResult.platform === 'win32') {
		if (!isValidTargetPid(targetPidForCapture)) throw new Error('Invalid native audio target PID');
		capture = new (loadResult.addon as WindowsNativeAudioModule).ProcessLoopback(targetPidForCapture, {
			includeProcessTree: winSystemCapture ? false : (options.includeProcessTree ?? true),
			captureScope: effectiveWinCaptureScope,
		});
	} else if (loadResult.platform === 'darwin') {
		if (!isValidTargetPid(targetPidForCapture)) throw new Error('Invalid native audio target PID');
		capture = new (loadResult.addon as MacNativeAudioModule).ProcessLoopback(targetPidForCapture, {
			excludeSelf: true,
			includeProcessTree: macSystemCapture ? true : (options.includeProcessTree ?? true),
			backend: options.macBackend ?? (macSystemCapture ? 'sck' : 'auto'),
			captureScope: macCaptureScope,
		});
	} else if (loadResult.platform === 'linux') {
		const addon = loadResult.addon as LinuxNativeAudioModule;
		capture = linuxRule
			? new addon.ProcessLoopback({linuxRule})
			: new addon.ProcessLoopback(targetPid as number, {
					includeProcessTree: options.includeProcessTree ?? true,
					ignoreDevices: true,
				});
	} else {
		throw new Error('Native audio addon unavailable for this platform');
	}
	let firstFrameLogged = false;
	const session: ActiveNativeAudioSession = {
		captureId,
		capture,
		sender,
		finalized: false,
		onFrame: (frame) => {
			if (session.finalized) return;
			if (!isValidAudioFrame(frame)) {
				logger.warn('Native audio capture emitted invalid frame', {
					captureId,
					...audioFrameDebugDetails(frame),
				});
				stopActiveSession(session, 'addon-error', 'Native audio addon emitted an invalid PCM frame').catch((error) =>
					logger.warn('stopActiveSession failed after invalid native audio frame', {captureId, error}),
				);
				return;
			}
			if (sender.isDestroyed()) {
				stopActiveSession(session, 'stopped').catch((error) =>
					logger.warn('stopActiveSession failed after sender destroyed', {captureId, error}),
				);
				return;
			}
			try {
				sender.send('native-audio:frame', {
					captureId,
					sampleRate: frame.sampleRate,
					channels: frame.channels,
					timestampUs: normalizeTimestampUs(frame.timestampUs),
					samples: copySamplesBuffer(frame.samples),
				});
			} catch (error) {
				logger.warn('Failed to send native audio frame to renderer', {captureId, error});
				stopActiveSession(session, 'stopped').catch((stopError) =>
					logger.warn('stopActiveSession failed after native audio send failure', {captureId, error: stopError}),
				);
				return;
			}
			if (!firstFrameLogged) {
				firstFrameLogged = true;
			}
		},
		onError: (error) => {
			const detail = error instanceof Error ? error.message : String(error);
			logger.warn('Native audio capture emitted error', {captureId, error});
			stopActiveSession(session, 'addon-error', detail).catch((stopError) =>
				logger.warn('stopActiveSession failed after addon error', {captureId, error: stopError}),
			);
		},
		onClosed: () => {
			const reason = session.stoppingReason ?? 'target-exited';
			finalizeSession(session, reason, session.stoppingDetail);
		},
		onSenderDestroyed: () => {
			stopActiveSession(session, 'stopped').catch((error) =>
				logger.warn('stopActiveSession failed on sender-destroyed', {captureId, error}),
			);
		},
	};
	capture.on('frame', session.onFrame);
	capture.on('error', session.onError);
	capture.on('closed', session.onClosed);
	sender.once('destroyed', session.onSenderDestroyed);
	activeSessions.set(captureId, session);
	rememberSenderSession(sender.id, captureId);
	try {
		await Promise.resolve(capture.start());
	} catch (error) {
		logger.error('Native audio capture failed to start', {captureId, error});
		removeSessionListeners(session);
		try {
			await Promise.resolve(capture.stop());
		} catch (stopError) {
			logger.warn('Failed to stop native audio capture after start failure', {captureId, error: stopError});
		}
		activeSessions.delete(captureId);
		forgetSenderSession(sender.id, captureId);
		throw error;
	}
	if (sender.isDestroyed()) {
		await stopActiveSession(session, 'stopped', 'sender-destroyed-during-start');
		throw new Error('Native audio capture sender was destroyed during startup');
	}
	if (session.finalized || activeSessions.get(captureId) !== session) {
		if (!session.finalized) {
			await stopActiveSession(session, 'stopped', 'session-replaced-during-start');
		}
		throw new Error('Native audio capture ended before startup completed');
	}
	return {
		captureId,
		sampleRate: 48000,
		channels: 2,
	};
}

function getNativeAudioRoutingGraph(sender: Electron.WebContents, captureId?: string): NativeAudioRoutingGraphResult {
	const loadResult = loadNativeAudioAddon();
	const availability = loadResult.availability;
	if (loadResult.platform !== 'linux' || !availability.available) {
		return {ok: false, graphs: [], availability};
	}
	const ownedIds = activeSessionIdsBySenderId.get(sender.id);
	const candidateIds =
		typeof captureId === 'string' && captureId
			? ownedIds?.has(captureId)
				? [captureId]
				: []
			: Array.from(ownedIds ?? []);
	const graphs = candidateIds
		.map((id) => {
			const session = activeSessions.get(id);
			if (!session || session.sender.id !== sender.id) return null;
			return {
				captureId: id,
				graph: session.capture.routingGraph?.() ?? null,
			};
		})
		.filter((entry): entry is {captureId: string; graph: VirtmicRoutingGraph | null} => entry !== null);
	return {ok: true, graphs, availability};
}

export function registerNativeAudioHandlers(): void {
	if (handlersRegistered) return;
	handlersRegistered = true;
	ipcMain.handle('native-audio:get-availability', (): Promise<NativeAudioAvailability> => getNativeAudioAvailability());
	ipcMain.handle(
		'native-audio:list-applications',
		(): Promise<Array<NativeAudioApplication>> => listNativeAudioApplications(),
	);
	ipcMain.handle(
		'native-audio:resolve-root-pid',
		(_event, sourceId: unknown): Promise<number | null> => resolveAudioRootPidForSource(sourceId),
	);
	ipcMain.handle(
		'native-audio:start',
		(event, options: NativeAudioStartOptions): Promise<NativeAudioStartResult> =>
			startNativeAudioCapture(event.sender, options),
	);
	ipcMain.handle('native-audio:stop', async (event, captureId: string): Promise<void> => {
		const session = activeSessions.get(captureId);
		if (!session || session.sender.id !== event.sender.id) {
			return;
		}
		await stopActiveSession(session, 'stopped');
	});
	ipcMain.handle(
		'native-audio:get-routing-graph',
		(event, captureId?: string): NativeAudioRoutingGraphResult => getNativeAudioRoutingGraph(event.sender, captureId),
	);
}

export function cleanupNativeAudio(): void {
	if (!handlersRegistered) return;
	ipcMain.removeHandler('native-audio:get-availability');
	ipcMain.removeHandler('native-audio:list-applications');
	ipcMain.removeHandler('native-audio:resolve-root-pid');
	ipcMain.removeHandler('native-audio:start');
	ipcMain.removeHandler('native-audio:stop');
	ipcMain.removeHandler('native-audio:get-routing-graph');
	handlersRegistered = false;
	for (const session of [...activeSessions.values()]) {
		stopActiveSession(session, 'stopped').catch((error) =>
			logger.warn('stopActiveSession failed during cleanup', {captureId: session.captureId, error}),
		);
	}
}
