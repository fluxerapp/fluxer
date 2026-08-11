// SPDX-License-Identifier: AGPL-3.0-or-later

import {execFile} from 'node:child_process';
import {randomUUID} from 'node:crypto';
import {createRequire} from 'node:module';
import {createChildLogger} from '@electron/common/Logger';
import type {
	NativeScreenCaptureAvailability,
	NativeScreenCaptureDiagnostics,
	NativeScreenCaptureEndReason,
	NativeScreenCaptureLifecycleEventKind,
	NativeScreenCaptureLifecycleMessage,
	NativeScreenCaptureLifecycleSource,
	NativeScreenCaptureSource,
	NativeScreenCaptureStartOptions,
	NativeScreenCaptureStartResult,
	WindowsHagsState,
} from '@electron/common/Types';
import {ipcMain} from 'electron';
import {getTccStatus} from './MacTcc';
import {isValidStartOptions, normalizeScreenCaptureDimension} from './NativeScreenCaptureValidation';
import {createNativeVoiceEngineScreenFrameSinkHandle} from './NativeVoiceEngine';

const logger = createChildLogger('NativeScreenCapture');
const requireModule = createRequire(import.meta.url);

interface NativeScreenCaptureInstance {
	on(event: 'error', listener: (error: Error) => void): this;
	on(event: 'closed', listener: () => void): this;
	on(event: 'stalled', listener: (message?: string) => void): this;
	on(event: 'diagnostic', listener: (message?: string) => void): this;
	removeListener(event: 'error', listener: (error: Error) => void): this;
	removeListener(event: 'closed', listener: () => void): this;
	removeListener(event: 'stalled', listener: (message?: string) => void): this;
	removeListener(event: 'diagnostic', listener: (message?: string) => void): this;
	setLifecycleCallback?(callback: (kind: string, message: string) => void): void;
	start(): Promise<
		| {
				width: number;
				height: number;
				frameRate: number;
				pixelFormat: 'nv12' | 'bgra';
		  }
		| undefined
	>;
	getDiagnostics?(): NativeScreenCaptureDiagnostics | null;
	stop(): Promise<void> | void;
}

interface NativeScreenCaptureSourceDescriptor {
	kind: 'screen' | 'window' | 'game';
	id: string;
	name: string;
	width: number;
	height: number;
	appName?: string;
	bundleId?: string;
	targetPid?: number;
}

type NativeScreenCaptureConstructorOptions = NativeScreenCaptureStartOptions & {
	frameSinkHandle?: unknown;
};

interface MacBackendAvailability {
	sck?: {
		supported: boolean;
		macosVersion?: string;
	};
	screenPermission?: string;
}

interface LinuxBackendAvailability {
	available: boolean;
	backend: string;
	reason?: string;
	detail?: string;
	portalVersion?: number;
	capabilities: {
		process: boolean;
		system: boolean;
	};
}

interface MacNativeScreenCaptureModule {
	listSources: () => Promise<Array<NativeScreenCaptureSourceDescriptor>>;
	getBackendAvailability: () => Promise<MacBackendAvailability>;
	ScreenCapture: new (options: NativeScreenCaptureConstructorOptions) => NativeScreenCaptureInstance;
	loadError?: Error | null;
}

interface LinuxNativeScreenCaptureModule {
	listSources: () => Promise<Array<NativeScreenCaptureSourceDescriptor>>;
	getAvailability: () => Promise<LinuxBackendAvailability>;
	ScreenCapture: new (options: NativeScreenCaptureConstructorOptions) => NativeScreenCaptureInstance;
	loadError?: Error | null;
}

interface WindowsBackendAvailability {
	available: boolean;
	backend: string;
	reason?: string;
}

interface WindowsNativeScreenCaptureModule {
	isSupported: () => boolean;
	getAvailability: () => WindowsBackendAvailability;
	ScreenCapture: new (options: NativeScreenCaptureConstructorOptions) => NativeScreenCaptureInstance;
	listSources: () => Promise<Array<NativeScreenCaptureSourceDescriptor>>;
	loadError?: Error | null;
}

type NativeScreenCaptureModule =
	| MacNativeScreenCaptureModule
	| LinuxNativeScreenCaptureModule
	| WindowsNativeScreenCaptureModule;

interface NativeAddonLoadResult {
	addon?: NativeScreenCaptureModule;
	availability: NativeScreenCaptureAvailability;
	platform: NodeJS.Platform;
}

interface ActiveNativeScreenSession {
	captureId: string;
	capture: NativeScreenCaptureInstance;
	sender: Electron.WebContents;
	sourceId: string;
	sourceKind: NativeScreenCaptureSource['kind'];
	startedAtMs: number;
	onError: (error: Error) => void;
	onClosed: () => void;
	onStalled?: (message?: string) => void;
	onDiagnostic?: (message?: string) => void;
	onSenderDestroyed: () => void;
	finalized: boolean;
	windowsHagsState?: WindowsHagsState;
	windowsHagsDetail?: string;
	stopping?: Promise<void>;
	stoppingReason?: NativeScreenCaptureEndReason;
	stoppingDetail?: string;
}

const MAX_NATIVE_SCREEN_SESSIONS_PER_SENDER = 2;

const GAME_CAPTURE_DETAIL = {
	addonError: 'game-capture-addon-error',
} as const;

const WINDOWS_HAGS_REGISTRY_PATH = 'HKLM\\SYSTEM\\CurrentControlSet\\Control\\GraphicsDrivers';
const WINDOWS_HAGS_REGISTRY_VALUE = 'HwSchMode';
const WINDOWS_HAGS_REGISTRY_READ_TIMEOUT_MS = 1500;
const WINDOWS_HAGS_CACHE_TTL_MS = 10000;
interface WindowsHagsDiagnostic {
	windowsHagsState: WindowsHagsState;
	windowsHagsDetail?: string;
}

let cachedLoadResult: NativeAddonLoadResult | undefined;
let cachedWindowsHagsDiagnostic: {expiresAtMs: number; value: WindowsHagsDiagnostic} | undefined;
let handlersRegistered = false;

const activeSessions = new Map<string, ActiveNativeScreenSession>();
const activeSessionIdsBySenderId = new Map<number, Set<string>>();

function isModuleNotFoundError(error: unknown): boolean {
	const message = String((error as {message?: string})?.message ?? error ?? '').toLowerCase();
	return message.includes('cannot find module') || message.includes('module_not_found');
}

function describeAddonLoadError(error: unknown): string {
	if (error instanceof Error) {
		const code = (error as Error & {code?: string}).code;
		const message = error.message ?? '';
		return code ? `${code}: ${message}` : message;
	}
	return String(error);
}

function parseWindowsHagsRegistryOutput(stdout: string): WindowsHagsDiagnostic {
	const match = stdout.match(/\bHwSchMode\s+REG_DWORD\s+(0x[0-9a-f]+|\d+)/i);
	if (!match) {
		return {windowsHagsState: 'unknown', windowsHagsDetail: 'HwSchMode registry value missing'};
	}
	const rawValue = match[1];
	const value = rawValue.toLowerCase().startsWith('0x') ? Number.parseInt(rawValue, 16) : Number.parseInt(rawValue, 10);
	if (value === 2) {
		return {windowsHagsState: 'enabled', windowsHagsDetail: 'HwSchMode=2'};
	}
	if (value === 1) {
		return {windowsHagsState: 'disabled', windowsHagsDetail: 'HwSchMode=1'};
	}
	return {windowsHagsState: 'unknown', windowsHagsDetail: `HwSchMode=${rawValue}`};
}

function queryWindowsHagsRegistry(): Promise<WindowsHagsDiagnostic> {
	if (process.platform !== 'win32') {
		return Promise.resolve({windowsHagsState: 'unsupported'});
	}
	return new Promise((resolve) => {
		execFile(
			'reg.exe',
			['query', WINDOWS_HAGS_REGISTRY_PATH, '/v', WINDOWS_HAGS_REGISTRY_VALUE],
			{timeout: WINDOWS_HAGS_REGISTRY_READ_TIMEOUT_MS, windowsHide: true},
			(error, stdout, stderr) => {
				if (error) {
					const detail = String(stderr || error.message || error).trim();
					resolve({
						windowsHagsState: 'unknown',
						windowsHagsDetail: detail || 'HwSchMode registry query failed',
					});
					return;
				}
				resolve(parseWindowsHagsRegistryOutput(stdout));
			},
		);
	});
}

async function readWindowsHagsDiagnostic(): Promise<WindowsHagsDiagnostic> {
	if (process.platform !== 'win32') {
		return {windowsHagsState: 'unsupported'};
	}
	const now = Date.now();
	if (cachedWindowsHagsDiagnostic && cachedWindowsHagsDiagnostic.expiresAtMs > now) {
		return cachedWindowsHagsDiagnostic.value;
	}
	const value = await queryWindowsHagsRegistry();
	cachedWindowsHagsDiagnostic = {expiresAtMs: now + WINDOWS_HAGS_CACHE_TTL_MS, value};
	return value;
}

function positiveDimension(value: number | undefined): number | undefined {
	return typeof value === 'number' && Number.isFinite(value) && value > 0 ? value : undefined;
}

function captureEndedBeforeStartupError(detail?: string): Error {
	const message = detail
		? `Native screen capture ended before startup completed: ${detail}`
		: 'Native screen capture ended before startup completed';
	return new Error(message);
}

function loadNativeScreenCaptureAddon(): NativeAddonLoadResult {
	if (cachedLoadResult) return cachedLoadResult;
	if (process.platform === 'darwin') {
		return loadMacNativeScreenCaptureAddon();
	}
	if (process.platform === 'linux') {
		return loadLinuxNativeScreenCaptureAddon();
	}
	if (process.platform === 'win32') {
		return loadWindowsNativeScreenCaptureAddon();
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

function loadMacNativeScreenCaptureAddon(): NativeAddonLoadResult {
	try {
		const addon = requireModule('@fluxer/mac-screen-capture') as MacNativeScreenCaptureModule;
		if (addon.loadError) {
			const detail = describeAddonLoadError(addon.loadError);
			logger.warn('macOS native screen capture addon reported load error', addon.loadError);
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
		logger.warn('Failed to load macOS native screen capture addon', error);
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

function loadLinuxNativeScreenCaptureAddon(): NativeAddonLoadResult {
	try {
		const addon = requireModule('@fluxer/linux-screen-capture') as LinuxNativeScreenCaptureModule;
		if (addon.loadError) {
			const detail = describeAddonLoadError(addon.loadError);
			logger.warn('Linux native screen capture addon reported load error', addon.loadError);
			cachedLoadResult = {
				platform: process.platform,
				availability: {
					available: false,
					backend: 'linux-pipewire',
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
				backend: 'linux-pipewire',
			},
		};
		return cachedLoadResult;
	} catch (error) {
		const detail = describeAddonLoadError(error);
		const reason = isModuleNotFoundError(error) ? 'addon-not-installed' : 'load-failed';
		logger.warn('Failed to load Linux native screen capture addon', error);
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

function loadWindowsNativeScreenCaptureAddon(): NativeAddonLoadResult {
	try {
		const addon = requireModule('@fluxer/win-game-capture') as WindowsNativeScreenCaptureModule;
		if (addon.loadError) {
			const detail = describeAddonLoadError(addon.loadError);
			logger.warn('Windows native screen capture addon reported load error', addon.loadError);
			cachedLoadResult = {
				platform: process.platform,
				availability: {
					available: false,
					backend: 'windows-game-capture',
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
				backend: 'windows-game-capture',
			},
		};
		return cachedLoadResult;
	} catch (error) {
		const detail = describeAddonLoadError(error);
		const reason = isModuleNotFoundError(error) ? 'addon-not-installed' : 'load-failed';
		logger.warn('Failed to load Windows native screen capture addon', error);
		cachedLoadResult = {
			platform: process.platform,
			availability: {
				available: false,
				backend: 'windows-game-capture',
				reason,
				detail,
			},
		};
		return cachedLoadResult;
	}
}

function getMacScreenPermissionStatus(): string | null {
	try {
		return getTccStatus('screen-recording');
	} catch (error) {
		logger.debug('Failed to read macOS screen capture permission', error);
		return null;
	}
}

async function getNativeScreenCaptureAvailability(): Promise<NativeScreenCaptureAvailability> {
	const loadResult = loadNativeScreenCaptureAddon();
	if (!loadResult.availability.available || !loadResult.addon) {
		if (loadResult.platform === 'win32') {
			return {
				...loadResult.availability,
				...(await readWindowsHagsDiagnostic()),
			};
		}
		return loadResult.availability;
	}
	if (loadResult.platform === 'darwin') {
		return getMacNativeScreenCaptureAvailability(loadResult.addon as MacNativeScreenCaptureModule);
	}
	if (loadResult.platform === 'linux') {
		return getLinuxNativeScreenCaptureAvailability(loadResult.addon as LinuxNativeScreenCaptureModule);
	}
	if (loadResult.platform === 'win32') {
		return getWindowsNativeScreenCaptureAvailability(loadResult.addon as WindowsNativeScreenCaptureModule);
	}
	return loadResult.availability;
}

async function getMacNativeScreenCaptureAvailability(
	addon: MacNativeScreenCaptureModule,
): Promise<NativeScreenCaptureAvailability> {
	try {
		const backendAvailability = await addon.getBackendAvailability();
		const sckSupported = backendAvailability.sck?.supported === true;
		const capabilities = {hidesCursor: true, screens: sckSupported, windows: sckSupported};
		if (!sckSupported) {
			return {
				available: false,
				backend: 'macos-sck',
				reason: 'os-version-too-old',
				detail: backendAvailability.sck?.macosVersion,
				capabilities,
			};
		}
		const screenPermissionStatus = getMacScreenPermissionStatus();
		if (screenPermissionStatus === 'denied' || screenPermissionStatus === 'restricted') {
			return {
				available: false,
				backend: 'macos-sck',
				reason: 'permission-denied',
				detail: `screen:${screenPermissionStatus}`,
				capabilities,
			};
		}
		return {
			available: true,
			backend: 'macos-sck',
			detail: backendAvailability.sck?.macosVersion,
			capabilities,
		};
	} catch (error) {
		logger.warn('Failed to query macOS native screen capture availability', error);
		return {
			available: false,
			backend: 'macos-sck',
			reason: 'load-failed',
		};
	}
}

async function getLinuxNativeScreenCaptureAvailability(
	addon: LinuxNativeScreenCaptureModule,
): Promise<NativeScreenCaptureAvailability> {
	try {
		const backendAvailability = await addon.getAvailability();
		const portalSupported = backendAvailability.available === true;
		const capabilities = {
			hidesCursor: true,
			screens: backendAvailability.capabilities?.system === true,
			windows: backendAvailability.capabilities?.system === true,
		};
		if (!portalSupported) {
			const portalReason = backendAvailability.reason ?? 'load-failed';
			const reason: NativeScreenCaptureAvailability['reason'] =
				portalReason === 'portal-too-old' || portalReason === 'pipewire-unreachable'
					? 'os-version-too-old'
					: 'load-failed';
			return {
				available: false,
				backend: 'linux-pipewire',
				reason,
				detail: backendAvailability.detail,
				capabilities,
			};
		}
		return {
			available: true,
			backend: 'linux-pipewire',
			detail: backendAvailability.detail,
			capabilities,
		};
	} catch (error) {
		logger.warn('Failed to query Linux native screen capture availability', error);
		return {
			available: false,
			backend: 'linux-pipewire',
			reason: 'load-failed',
		};
	}
}

async function getWindowsNativeScreenCaptureAvailability(
	addon: WindowsNativeScreenCaptureModule,
): Promise<NativeScreenCaptureAvailability> {
	try {
		const info = addon.getAvailability();
		const windowsHags = await readWindowsHagsDiagnostic();
		const reason: NativeScreenCaptureAvailability['reason'] | undefined = info.available
			? undefined
			: info.reason === 'unsupported-platform' ||
					info.reason === 'addon-not-installed' ||
					info.reason === 'load-failed' ||
					info.reason === 'os-version-too-old' ||
					info.reason === 'permission-denied' ||
					info.reason === 'disabled-by-launch'
				? info.reason
				: 'load-failed';
		return {
			available: info.available,
			backend: info.backend === 'windows-game-capture' ? 'windows-game-capture' : 'windows-dxgi',
			...(reason ? {reason} : {}),
			...windowsHags,
			capabilities: {hidesCursor: false, screens: true, windows: true},
		};
	} catch (error) {
		logger.warn('Failed to query Windows native screen capture availability', error);
		const windowsHags = await readWindowsHagsDiagnostic();
		return {
			available: false,
			backend: 'windows-game-capture',
			reason: 'load-failed',
			...windowsHags,
		};
	}
}

async function listNativeScreenCaptureSources(): Promise<Array<NativeScreenCaptureSource>> {
	const loadResult = loadNativeScreenCaptureAddon();
	if (!loadResult.availability.available || !loadResult.addon) return [];
	if (loadResult.platform === 'win32') {
		return listWindowsNativeScreenCaptureSources(loadResult.addon as WindowsNativeScreenCaptureModule);
	}
	if (loadResult.platform !== 'darwin' && loadResult.platform !== 'linux') {
		return [];
	}
	try {
		const sources = await loadResult.addon.listSources();
		return sources
			.filter(
				(source): source is NativeScreenCaptureSourceDescriptor =>
					typeof source.id === 'string' &&
					source.id.length > 0 &&
					(source.kind === 'screen' || source.kind === 'window' || source.kind === 'game'),
			)
			.map((source) => ({
				kind: source.kind,
				id: source.id,
				name: source.name || `${source.kind} ${source.id}`,
				width: Math.max(0, Math.floor(source.width)),
				height: Math.max(0, Math.floor(source.height)),
				appName: source.appName,
				bundleId: source.bundleId,
				targetPid:
					source.kind === 'window' &&
					typeof source.targetPid === 'number' &&
					Number.isFinite(source.targetPid) &&
					source.targetPid > 0
						? Math.floor(source.targetPid)
						: undefined,
			}));
	} catch (error) {
		logger.warn('Failed to list native screen capture sources', error);
		return [];
	}
}

async function listWindowsNativeScreenCaptureSources(
	addon: WindowsNativeScreenCaptureModule,
): Promise<Array<NativeScreenCaptureSource>> {
	try {
		const sources = await addon.listSources();
		return sources
			.filter(
				(source): source is NativeScreenCaptureSourceDescriptor =>
					typeof source.id === 'string' &&
					source.id.length > 0 &&
					(source.kind === 'screen' || source.kind === 'window' || source.kind === 'game'),
			)
			.map((source) => {
				const kind = source.kind;
				return {
					kind,
					id: source.id,
					name: source.name || `${kind} ${source.id}`,
					width: Math.max(0, Math.floor(source.width)),
					height: Math.max(0, Math.floor(source.height)),
					targetPid:
						kind === 'window' &&
						typeof source.targetPid === 'number' &&
						Number.isFinite(source.targetPid) &&
						source.targetPid > 0
							? Math.floor(source.targetPid)
							: undefined,
				};
			});
	} catch (error) {
		logger.warn('Failed to list Windows native screen capture sources', error);
		return [];
	}
}

function removeSessionListeners(session: ActiveNativeScreenSession): void {
	session.capture.removeListener('error', session.onError);
	session.capture.removeListener('closed', session.onClosed);
	if (session.onStalled) session.capture.removeListener('stalled', session.onStalled);
	if (session.onDiagnostic) session.capture.removeListener('diagnostic', session.onDiagnostic);
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

const LIFECYCLE_EVENT_KINDS: ReadonlySet<NativeScreenCaptureLifecycleEventKind> = new Set([
	'error',
	'closed',
	'closed-clean',
	'stalled',
	'diagnostic',
]);

function normalizeLifecycleEventKind(kind: string): NativeScreenCaptureLifecycleEventKind | null {
	if (typeof kind !== 'string' || kind.length === 0) return null;
	if (!LIFECYCLE_EVENT_KINDS.has(kind as NativeScreenCaptureLifecycleEventKind)) return null;
	return kind as NativeScreenCaptureLifecycleEventKind;
}

function normalizeLifecycleMessage(message: unknown): string {
	if (typeof message !== 'string') return '';
	if (message.length > 4096) return message.slice(0, 4096);
	return message;
}

function sendLifecycleEvent(
	session: ActiveNativeScreenSession,
	kind: string,
	message: string,
	source: NativeScreenCaptureLifecycleSource,
): void {
	const normalizedKind = normalizeLifecycleEventKind(kind);
	if (!normalizedKind) return;
	if (session.finalized) return;
	if (session.sender.isDestroyed()) return;
	const payload: NativeScreenCaptureLifecycleMessage = {
		captureId: session.captureId,
		kind: normalizedKind,
		message: normalizeLifecycleMessage(message),
		source,
	};
	try {
		session.sender.send('native-screen-capture:lifecycle', payload);
	} catch (error) {
		logger.warn('Failed to send native screen capture lifecycle event to renderer', {
			captureId: session.captureId,
			kind: normalizedKind,
			source,
			error,
		});
	}
}

function finalizeSession(
	session: ActiveNativeScreenSession,
	reason: NativeScreenCaptureEndReason,
	detail?: string,
): void {
	if (session.finalized) return;
	session.finalized = true;
	removeSessionListeners(session);
	activeSessions.delete(session.captureId);
	forgetSenderSession(session.sender.id, session.captureId);
	if (!session.sender.isDestroyed()) {
		try {
			session.sender.send('native-screen-capture:end', {
				captureId: session.captureId,
				reason,
				detail,
			});
		} catch (error) {
			logger.warn('Failed to send native screen capture end event to renderer', {
				captureId: session.captureId,
				reason,
				error,
			});
		}
	}
}

async function stopActiveSession(
	session: ActiveNativeScreenSession,
	reason: NativeScreenCaptureEndReason = 'stopped',
	detail?: string,
): Promise<void> {
	if (session.finalized) return;
	if (session.stopping) return session.stopping;
	session.stoppingReason = reason;
	session.stoppingDetail = detail;
	session.stopping = (async () => {
		let stopOk = false;
		try {
			await Promise.resolve(session.capture.stop());
			stopOk = true;
		} catch (error) {
			logger.warn('Failed to stop native screen capture', {captureId: session.captureId, error});
		} finally {
			if (stopOk && reason === 'stopped') {
				sendLifecycleEvent(session, 'closed-clean', detail ?? 'programmatic-stop', 'programmatic');
			}
			finalizeSession(session, reason, detail);
		}
	})();
	return session.stopping;
}

async function stopCaptureById(
	captureId: string,
	reason: NativeScreenCaptureEndReason = 'stopped',
	detail?: string,
): Promise<void> {
	const session = activeSessions.get(captureId);
	if (!session) return;
	await stopActiveSession(session, reason, detail);
}

function getSessionDiagnostics(session: ActiveNativeScreenSession): NativeScreenCaptureDiagnostics {
	return {
		captureId: session.captureId,
		sourceId: session.sourceId,
		sourceKind: session.sourceKind,
		startedAtMs: session.startedAtMs,
		windowsHagsState: session.windowsHagsState,
		windowsHagsDetail: session.windowsHagsDetail,
	};
}

function getCaptureDiagnosticsForSender(
	sender: Electron.WebContents,
	captureId: string,
): NativeScreenCaptureDiagnostics | null {
	const session = activeSessions.get(captureId);
	if (!session || session.sender.id !== sender.id) return null;
	const addonDiagnostics = session.capture.getDiagnostics?.() ?? null;
	return {
		...(addonDiagnostics ?? {}),
		...getSessionDiagnostics(session),
	};
}

async function makeRoomForSenderSession(senderId: number): Promise<void> {
	const senderSessions = activeSessionIdsBySenderId.get(senderId);
	if (!senderSessions) return;
	while (senderSessions.size >= MAX_NATIVE_SCREEN_SESSIONS_PER_SENDER) {
		const oldestCaptureId = senderSessions.values().next().value as string | undefined;
		if (!oldestCaptureId) return;
		await stopCaptureById(oldestCaptureId, 'stopped', 'sender-session-limit');
	}
}

async function startNativeScreenCapture(
	sender: Electron.WebContents,
	options: NativeScreenCaptureStartOptions,
): Promise<NativeScreenCaptureStartResult> {
	if (sender.isDestroyed()) {
		throw new Error('Cannot start native screen capture for destroyed renderer');
	}
	if (!isValidStartOptions(options)) {
		throw new Error('Invalid native screen capture options');
	}
	const availability = await getNativeScreenCaptureAvailability();
	if (!availability.available) {
		throw new Error(`Native screen capture unavailable (${availability.reason ?? 'unknown'})`);
	}
	const loadResult = loadNativeScreenCaptureAddon();
	if (!loadResult.addon) {
		throw new Error('Native screen capture addon unavailable');
	}
	const windowsHags = loadResult.platform === 'win32' ? await readWindowsHagsDiagnostic() : undefined;
	if (windowsHags?.windowsHagsState === 'enabled') {
		logger.warn('Windows Hardware Accelerated GPU Scheduling is enabled during native screen capture startup', {
			sourceKind: options.sourceKind,
			detail: windowsHags.windowsHagsDetail,
		});
	}
	await makeRoomForSenderSession(sender.id);
	if (sender.isDestroyed()) {
		throw new Error('Cannot start native screen capture for destroyed renderer');
	}
	const requestedWidth = normalizeScreenCaptureDimension(options.width);
	const requestedHeight = normalizeScreenCaptureDimension(options.height);
	const captureId = options.captureId?.trim() || randomUUID();
	if (activeSessions.has(captureId)) {
		throw new Error('Native screen capture id is already active');
	}
	const frameSinkHandle = createNativeVoiceEngineScreenFrameSinkHandle(captureId);
	if (!frameSinkHandle) {
		throw new Error('Native screen capture requires a native frame sink handle, but none is active');
	}
	const capture = new loadResult.addon.ScreenCapture({
		sourceId: options.sourceId,
		sourceKind: options.sourceKind,
		width: requestedWidth,
		height: requestedHeight,
		frameRate: options.frameRate ?? 30,
		injectionMethod: options.sourceKind === 'game' ? options.injectionMethod : undefined,
		captureId,
		colorRange: options.colorRange,
		colorSpace: options.colorSpace,
		showCursorClicks: options.showCursorClicks === true,
		captureRect: options.captureRect,
		nativeFrameSinkRequired: true,
		frameSinkHandle,
	});
	const session: ActiveNativeScreenSession = {
		captureId,
		capture,
		sender,
		sourceId: options.sourceId,
		sourceKind: options.sourceKind,
		startedAtMs: Date.now(),
		finalized: false,
		windowsHagsState: windowsHags?.windowsHagsState,
		windowsHagsDetail: windowsHags?.windowsHagsDetail,
		onError: (error) => {
			const message = error instanceof Error ? error.message : String(error);
			const detail = options.sourceKind === 'game' ? `${GAME_CAPTURE_DETAIL.addonError}: ${message}` : message;
			logger.warn('Native screen capture emitted error', {captureId, error});
			stopActiveSession(session, 'addon-error', detail).catch((stopError) =>
				logger.warn('stopActiveSession failed after addon error', {captureId, error: stopError}),
			);
		},
		onClosed: () => {
			const reason = session.stoppingReason ?? 'source-vanished';
			finalizeSession(session, reason, session.stoppingDetail);
		},
		onSenderDestroyed: () => {
			stopActiveSession(session, 'stopped').catch((error) =>
				logger.warn('stopActiveSession failed on sender-destroyed', {captureId, error}),
			);
		},
	};
	session.onStalled = (stallMessage) => {
		if (session.finalized) return;
		logger.info('Native screen capture reported a stall (non-fatal)', {
			captureId,
			sourceKind: options.sourceKind,
			detail: stallMessage,
		});
	};
	session.onDiagnostic = (diagnosticMessage) => {
		if (session.finalized) return;
		logger.debug('Native screen capture diagnostic', {
			captureId,
			sourceKind: options.sourceKind,
			detail: diagnosticMessage,
		});
	};
	capture.on('error', session.onError);
	capture.on('closed', session.onClosed);
	if (session.onStalled) capture.on('stalled', session.onStalled);
	if (session.onDiagnostic) capture.on('diagnostic', session.onDiagnostic);
	if (typeof capture.setLifecycleCallback === 'function') {
		try {
			capture.setLifecycleCallback((kind, message) => sendLifecycleEvent(session, kind, message, 'delegate'));
		} catch (error) {
			logger.warn('Failed to install native screen capture lifecycle callback', {captureId, error});
		}
	}
	sender.once('destroyed', session.onSenderDestroyed);
	activeSessions.set(captureId, session);
	rememberSenderSession(sender.id, captureId);
	let startResult: {width: number; height: number; frameRate: number; pixelFormat: 'nv12' | 'bgra'} | undefined;
	try {
		const resolved = await Promise.resolve(capture.start());
		startResult = resolved ?? undefined;
	} catch (error) {
		logger.warn('Failed to start native screen capture', {captureId, error});
		removeSessionListeners(session);
		try {
			await Promise.resolve(capture.stop());
		} catch (stopError) {
			logger.warn('Failed to stop native screen capture after start failure', {captureId, error: stopError});
		}
		activeSessions.delete(captureId);
		forgetSenderSession(sender.id, captureId);
		throw error;
	}
	if (sender.isDestroyed()) {
		await stopActiveSession(session, 'stopped', 'sender-destroyed-during-start');
		throw new Error('Native screen capture sender was destroyed during startup');
	}
	if (session.finalized || activeSessions.get(captureId) !== session) {
		if (!session.finalized) {
			await stopActiveSession(session, 'stopped', 'session-replaced-during-start');
		}
		throw captureEndedBeforeStartupError(session.stoppingDetail);
	}
	if (session.finalized || activeSessions.get(captureId) !== session) {
		if (!session.finalized) {
			await stopActiveSession(session, 'stopped', 'session-replaced-before-first-frame');
		}
		throw captureEndedBeforeStartupError(session.stoppingDetail);
	}
	const resolvedWidth = positiveDimension(startResult?.width) ?? requestedWidth ?? 0;
	const resolvedHeight = positiveDimension(startResult?.height) ?? requestedHeight ?? 0;
	const resolvedFrameRate = startResult?.frameRate ?? options.frameRate ?? 30;
	const resolvedFormat = startResult?.pixelFormat ?? 'nv12';
	return {
		captureId,
		width: resolvedWidth,
		height: resolvedHeight,
		frameRate: resolvedFrameRate,
		pixelFormat: resolvedFormat,
	};
}

export function registerNativeScreenCaptureHandlers(): void {
	if (handlersRegistered) return;
	handlersRegistered = true;
	ipcMain.handle(
		'native-screen-capture:get-availability',
		(): Promise<NativeScreenCaptureAvailability> => getNativeScreenCaptureAvailability(),
	);
	ipcMain.handle(
		'native-screen-capture:list-sources',
		(): Promise<Array<NativeScreenCaptureSource>> => listNativeScreenCaptureSources(),
	);
	ipcMain.handle(
		'native-screen-capture:start',
		(event, options: NativeScreenCaptureStartOptions): Promise<NativeScreenCaptureStartResult> =>
			startNativeScreenCapture(event.sender, options),
	);
	ipcMain.handle(
		'native-screen-capture:get-diagnostics',
		(event, captureId: string): NativeScreenCaptureDiagnostics | null =>
			getCaptureDiagnosticsForSender(event.sender, captureId),
	);
	ipcMain.handle('native-screen-capture:stop', async (event, captureId: string): Promise<void> => {
		const session = activeSessions.get(captureId);
		if (!session || session.sender.id !== event.sender.id) return;
		await stopActiveSession(session, 'stopped');
	});
}

export function cleanupNativeScreenCapture(): void {
	if (!handlersRegistered) return;
	ipcMain.removeHandler('native-screen-capture:get-availability');
	ipcMain.removeHandler('native-screen-capture:list-sources');
	ipcMain.removeHandler('native-screen-capture:start');
	ipcMain.removeHandler('native-screen-capture:get-diagnostics');
	ipcMain.removeHandler('native-screen-capture:stop');
	handlersRegistered = false;
	for (const session of [...activeSessions.values()]) {
		stopActiveSession(session, 'stopped').catch((error) =>
			logger.warn('stopActiveSession failed during cleanup', {captureId: session.captureId, error}),
		);
	}
}
