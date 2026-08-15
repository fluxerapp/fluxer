// SPDX-License-Identifier: AGPL-3.0-or-later

import {BUILD_CHANNEL} from '@electron/common/BuildChannel';
import type {
	AppMetricsSnapshot,
	ClipboardWriteFileOptions,
	ClipboardWriteFileResult,
	DesktopInfo,
	DesktopSource,
	DesktopTroubleshootingSettings,
	DesktopVoiceDebugEventSinkEntry,
	DesktopWindowBehaviorSettings,
	DisplayMediaPortalSurfacePreference,
	DisplayMediaRequestInfo,
	DownloadFileResult,
	ElectronAPI,
	GetDesktopSourcesOptions,
	GlobalKeybindTriggeredEvent,
	GlobalKeyEvent,
	GlobalKeyHookRegisterOptions,
	GlobalMouseEvent,
	GpuInfo,
	InputMonitoringPermissionStatus,
	LinuxAppearanceSnapshot,
	MediaAccessStatus,
	MediaAccessType,
	NativeAudioApplication,
	NativeAudioAvailability,
	NativeAudioEndMessage,
	NativeAudioFrameMessage,
	NativeAudioRoutingGraphResult,
	NativeAudioStartOptions,
	NativeAudioStartResult,
	NativeScreenCaptureAvailability,
	NativeScreenCaptureDiagnostics,
	NativeScreenCaptureEndMessage,
	NativeScreenCaptureLifecycleEventKind,
	NativeScreenCaptureLifecycleMessage,
	NativeScreenCaptureLifecycleSource,
	NativeScreenCaptureSource,
	NativeScreenCaptureStartOptions,
	NativeScreenCaptureStartResult,
	NotificationOptions,
	NotificationResult,
	OpenH264Status,
	SetDesktopTroubleshootingDisableHardwareAccelerationOptions,
	SpellcheckBundledDictionary,
	SpellcheckResolvedEngineInfo,
	SpellcheckState,
	StreamerModeCaptureAppStatus,
	StreamingPriorityDiagnostics,
	SwitchInstanceUrlOptions,
	TextareaContextMenuParams,
	TrayActionPayload,
	TrayRuntimeStatePayload,
	UpdaterContext,
	UpdaterEvent,
	VirtmicAvailability,
	VirtmicLinkOptions,
	VirtmicNode,
	VirtmicRoutingGraphResult,
	VirtmicSystemLinkOptions,
} from '@electron/common/Types';
import type {
	VoiceEngineV2BridgeApi,
	VoiceEngineV2BridgeEvent,
	VoiceEngineV2BridgeVideoFrame,
} from '@fluxer/voice_engine_v2/bridge';
import {
	VOICE_ENGINE_V2_BRIDGE_VERSION,
	VOICE_ENGINE_V2_EVENT_CHANNELS,
	VOICE_ENGINE_V2_IPC_CHANNELS,
} from '@fluxer/voice_engine_v2/bridge';
import type {
	AuthenticationResponseJSON,
	PublicKeyCredentialCreationOptionsJSON,
	PublicKeyCredentialRequestOptionsJSON,
	RegistrationResponseJSON,
} from '@simplewebauthn/browser';
import {contextBridge, ipcRenderer, webFrame} from 'electron';

const ACCESSIBILITY_STORE_STORAGE_KEY = 'AccessibilityStore';
const ACCESSIBILITY_ZOOM_STORAGE_KEY = 'AccessibilityStore:zoomLevel';
const ACTIVE_ALLOW_TRANSPARENCY_RENDERER_ARG = '--fluxer-active-allow-transparency=1';
const ACTIVE_USE_NATIVE_TITLEBAR_RENDERER_ARG = '--fluxer-active-use-native-titlebar=1';
const CUSTOM_TITLEBAR_HEIGHT = '32px';
const NATIVE_TITLEBAR_HEIGHT = '0px';
const STARTUP_NATIVE_TITLEBAR_ID = 'fluxer-startup-native-titlebar';
const THEME_STUDIO_STANDALONE_PATHNAME = '/theme-studio';
const ZOOM_LEVEL_MIN = 0.5;
const ZOOM_LEVEL_MAX = 2.0;

interface AccessibilityStartupSettings {
	zoomLevel: number;
	syncReducedMotionWithSystem: boolean;
	reducedMotionOverride: boolean | null;
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return value !== null && typeof value === 'object' && !Array.isArray(value);
}

const NATIVE_SCREEN_CAPTURE_LIFECYCLE_KINDS: ReadonlySet<NativeScreenCaptureLifecycleEventKind> = new Set([
	'error',
	'closed',
	'closed-clean',
	'stalled',
	'diagnostic',
]);

const NATIVE_SCREEN_CAPTURE_LIFECYCLE_SOURCES: ReadonlySet<NativeScreenCaptureLifecycleSource> = new Set([
	'delegate',
	'programmatic',
]);

const NATIVE_SCREEN_CAPTURE_LIFECYCLE_MAX_MESSAGE_LENGTH = 4096;
const NATIVE_SCREEN_CAPTURE_LIFECYCLE_MAX_CAPTURE_ID_LENGTH = 256;

function validateNativeScreenCaptureLifecycleMessage(value: unknown): NativeScreenCaptureLifecycleMessage | null {
	if (!isRecord(value)) return null;
	const captureId = value['captureId'];
	if (typeof captureId !== 'string') return null;
	if (captureId.length === 0) return null;
	if (captureId.length > NATIVE_SCREEN_CAPTURE_LIFECYCLE_MAX_CAPTURE_ID_LENGTH) return null;
	const kind = value['kind'];
	if (typeof kind !== 'string') return null;
	if (!NATIVE_SCREEN_CAPTURE_LIFECYCLE_KINDS.has(kind as NativeScreenCaptureLifecycleEventKind)) return null;
	const rawMessage = value['message'];
	const message =
		typeof rawMessage === 'string' ? rawMessage.slice(0, NATIVE_SCREEN_CAPTURE_LIFECYCLE_MAX_MESSAGE_LENGTH) : '';
	const rawSource = value['source'];
	const source =
		typeof rawSource === 'string' &&
		NATIVE_SCREEN_CAPTURE_LIFECYCLE_SOURCES.has(rawSource as NativeScreenCaptureLifecycleSource)
			? (rawSource as NativeScreenCaptureLifecycleSource)
			: undefined;
	return source === undefined
		? {captureId, kind: kind as NativeScreenCaptureLifecycleEventKind, message}
		: {captureId, kind: kind as NativeScreenCaptureLifecycleEventKind, message, source};
}

interface VoiceEngineV2BridgeVideoFrameWire {
	meta: VoiceEngineV2BridgeVideoFrame['meta'];
	data: Uint8Array<ArrayBuffer>;
}

function videoFrameWireDataToArrayBuffer(data: Uint8Array<ArrayBuffer>): ArrayBuffer {
	if (data.byteOffset === 0 && data.byteLength === data.buffer.byteLength) {
		return data.buffer;
	}
	return data.buffer.slice(data.byteOffset, data.byteOffset + data.byteLength);
}

function clampZoomLevel(level: number): number {
	if (!Number.isFinite(level)) return 1;
	const pct = Math.round(level * 100);
	const clampedPct = Math.max(Math.round(ZOOM_LEVEL_MIN * 100), Math.min(Math.round(ZOOM_LEVEL_MAX * 100), pct));
	return clampedPct / 100;
}

function readLocalZoomLevel(): number | null {
	let raw: string | null;
	try {
		raw = window.localStorage.getItem(ACCESSIBILITY_ZOOM_STORAGE_KEY);
	} catch {
		return null;
	}
	if (raw === null) {
		return null;
	}
	let parsed: unknown;
	try {
		parsed = JSON.parse(raw);
	} catch {
		return null;
	}
	return typeof parsed === 'number' ? clampZoomLevel(parsed) : null;
}

function readLegacyAccessibilityStartupSettings(): AccessibilityStartupSettings | null {
	let raw: string | null;
	try {
		raw = window.localStorage.getItem(ACCESSIBILITY_STORE_STORAGE_KEY);
	} catch {
		return null;
	}
	if (raw === null) {
		return null;
	}
	let parsed: unknown;
	try {
		parsed = JSON.parse(raw);
	} catch {
		return null;
	}
	if (!isRecord(parsed)) {
		return null;
	}
	const metadata = parsed.__mps__;
	if (isRecord(metadata) && metadata.version !== 1) {
		return null;
	}
	return {
		zoomLevel: typeof parsed.zoomLevel === 'number' ? clampZoomLevel(parsed.zoomLevel) : 1.0,
		syncReducedMotionWithSystem:
			typeof parsed.syncReducedMotionWithSystem === 'boolean' ? parsed.syncReducedMotionWithSystem : true,
		reducedMotionOverride: typeof parsed.reducedMotionOverride === 'boolean' ? parsed.reducedMotionOverride : null,
	};
}

function readAccessibilityStartupSettings(): AccessibilityStartupSettings | null {
	const localZoomLevel = readLocalZoomLevel();
	const legacySettings = readLegacyAccessibilityStartupSettings();
	if (localZoomLevel === null && legacySettings === null) {
		return null;
	}
	return {
		zoomLevel: localZoomLevel ?? legacySettings?.zoomLevel ?? 1.0,
		syncReducedMotionWithSystem: legacySettings?.syncReducedMotionWithSystem ?? true,
		reducedMotionOverride: legacySettings?.reducedMotionOverride ?? null,
	};
}

function persistLocalZoomLevel(level: number): void {
	try {
		window.localStorage.setItem(ACCESSIBILITY_ZOOM_STORAGE_KEY, JSON.stringify(clampZoomLevel(level)));
	} catch {}
}

function getSystemReducedMotionPreference(): boolean {
	if (typeof window.matchMedia !== 'function') {
		return false;
	}
	return window.matchMedia('(prefers-reduced-motion: reduce)').matches;
}

function resolveReducedMotion(settings: AccessibilityStartupSettings): boolean {
	return settings.syncReducedMotionWithSystem
		? getSystemReducedMotionPreference()
		: (settings.reducedMotionOverride ?? false);
}

function withDocumentElement(callback: (root: HTMLElement) => void): void {
	const apply = (): boolean => {
		const root = document.documentElement as HTMLElement | null;
		if (root === null) {
			return false;
		}
		callback(root);
		return true;
	};
	if (apply()) {
		return;
	}
	let observer: MutationObserver | null = null;
	let applied = false;
	const applyWhenReady = (): void => {
		if (applied) {
			return;
		}
		const root = document.documentElement as HTMLElement | null;
		if (root === null) {
			return;
		}
		applied = true;
		observer?.disconnect();
		document.removeEventListener('DOMContentLoaded', applyWhenReady);
		callback(root);
	};
	observer = new MutationObserver(applyWhenReady);
	observer.observe(document, {childList: true});
	document.addEventListener('DOMContentLoaded', applyWhenReady, {once: true});
	applyWhenReady();
}

function applyReducedMotionClass(reducedMotion: boolean): void {
	withDocumentElement((root) => {
		root.classList.toggle('reduced-motion', reducedMotion);
	});
}

function applyCustomZoomProperty(level: number): void {
	const zoomPercent = Math.round(clampZoomLevel(level) * 100);
	withDocumentElement((root) => {
		root.style.setProperty('--custom-zoom', String(zoomPercent));
	});
}

function getStartupPlatformClass(): string {
	switch (process.platform) {
		case 'darwin':
			return 'platform-macos';
		case 'win32':
			return 'platform-windows';
		case 'linux':
			return 'platform-linux';
		default:
			return 'platform-unknown';
	}
}

function installStartupNativeTitlebar(activeUseNativeTitleBar: boolean): void {
	if (activeUseNativeTitleBar || window.location.pathname === THEME_STUDIO_STANDALONE_PATHNAME) return;
	const install = (): void => {
		if (!document.body || document.getElementById(STARTUP_NATIVE_TITLEBAR_ID)) return;
		const titlebar = document.createElement('div');
		titlebar.id = STARTUP_NATIVE_TITLEBAR_ID;
		titlebar.setAttribute('aria-hidden', 'true');
		titlebar.setAttribute('data-native-titlebar', '');
		document.body.prepend(titlebar);
	};
	if (document.body) {
		install();
		return;
	}
	document.addEventListener('DOMContentLoaded', install, {once: true});
}

function applyStartupDesktopWindowClasses(): void {
	const activeUseNativeTitleBar = process.argv.includes(ACTIVE_USE_NATIVE_TITLEBAR_RENDERER_ARG);
	withDocumentElement((root) => {
		root.classList.remove(
			'platform-web',
			'platform-native',
			'platform-macos',
			'platform-windows',
			'platform-linux',
			'platform-unknown',
		);
		root.classList.add('platform-native', getStartupPlatformClass());
		root.classList.toggle('allow-transparency', process.argv.includes(ACTIVE_ALLOW_TRANSPARENCY_RENDERER_ARG));
		root.classList.toggle('native-system-titlebar', activeUseNativeTitleBar);
		root.style.setProperty(
			'--native-titlebar-height',
			activeUseNativeTitleBar ? NATIVE_TITLEBAR_HEIGHT : CUSTOM_TITLEBAR_HEIGHT,
		);
	});
	installStartupNativeTitlebar(activeUseNativeTitleBar);
}

function applyStartupAccessibilitySettings(): void {
	const settings = readAccessibilityStartupSettings();
	if (settings === null) {
		return;
	}
	persistLocalZoomLevel(settings.zoomLevel);
	webFrame.setZoomFactor(1);
	applyCustomZoomProperty(settings.zoomLevel);
	applyReducedMotionClass(resolveReducedMotion(settings));
}

applyStartupDesktopWindowClasses();

applyStartupAccessibilitySettings();

const api: ElectronAPI = {
	platform: process.platform,
	buildChannel: BUILD_CHANNEL,
	getDesktopInfo: (): Promise<DesktopInfo> => ipcRenderer.invoke('get-desktop-info'),
	getGpuInfo: (): Promise<GpuInfo> => ipcRenderer.invoke('get-gpu-info'),
	getAppMetrics: (): Promise<AppMetricsSnapshot> => ipcRenderer.invoke('get-app-metrics'),
	getOpenH264Status: (): Promise<OpenH264Status> => ipcRenderer.invoke('get-openh264-status'),
	setOpenH264Enabled: (enabled: boolean): Promise<OpenH264Status> =>
		ipcRenderer.invoke('set-openh264-enabled', enabled),
	getSystemIdleTimeMs: (): Promise<number> => ipcRenderer.invoke('system-idle-time-ms'),
	getDesktopWindowBehaviorSettings: (): Promise<DesktopWindowBehaviorSettings> =>
		ipcRenderer.invoke('desktop-window-behavior-get'),
	setDesktopWindowBehaviorSettings: (
		settings: Partial<DesktopWindowBehaviorSettings>,
	): Promise<DesktopWindowBehaviorSettings> => ipcRenderer.invoke('desktop-window-behavior-set', settings),
	getDesktopWindowBehaviorPendingRestart: (): Promise<boolean> =>
		ipcRenderer.invoke('desktop-window-behavior-pending-restart'),
	desktopAppRelaunch: (): Promise<void> => ipcRenderer.invoke('desktop-app-relaunch'),
	pickThemeLocalFiles: () => ipcRenderer.invoke('theme-local-files-pick'),
	readThemeLocalFiles: (paths: Array<string>) => ipcRenderer.invoke('theme-local-files-read', paths),
	clearThemeLocalFiles: () => ipcRenderer.invoke('theme-local-files-clear'),
	importThemeDirectory: () => ipcRenderer.invoke('theme-directory-import'),
	cacheVoiceBackgroundMedia: (options) => ipcRenderer.invoke('voice-background-media-cache:write', options),
	resolveVoiceBackgroundMedia: (id) => ipcRenderer.invoke('voice-background-media-cache:resolve', id),
	readVoiceBackgroundMedia: (id) => ipcRenderer.invoke('voice-background-media-cache:read', id),
	deleteVoiceBackgroundMedia: (id) => ipcRenderer.invoke('voice-background-media-cache:delete', id),
	getDesktopTroubleshootingSettings: (): Promise<DesktopTroubleshootingSettings> =>
		ipcRenderer.invoke('desktop-troubleshooting-get'),
	setDesktopDisableHardwareAcceleration: (
		options: SetDesktopTroubleshootingDisableHardwareAccelerationOptions,
	): Promise<DesktopTroubleshootingSettings> =>
		ipcRenderer.invoke('desktop-troubleshooting-set-disable-hardware-acceleration', options),
	desktopTroubleshootingReload: (): Promise<void> => ipcRenderer.invoke('desktop-troubleshooting-reload'),
	desktopTroubleshootingResetAppData: (options?: {confirm?: boolean}): Promise<void> =>
		ipcRenderer.invoke('desktop-troubleshooting-reset-app-data', options),
	popupHelpMenu: (): Promise<void> => ipcRenderer.invoke('desktop-troubleshooting-popup-help-menu'),
	onUpdaterEvent: (callback: (event: UpdaterEvent) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, data: UpdaterEvent) => callback(data);
		ipcRenderer.on('updater-event', handler);
		return () => ipcRenderer.removeListener('updater-event', handler);
	},
	updaterCheck: (context: UpdaterContext): Promise<void> => ipcRenderer.invoke('updater-check', context),
	updaterDownload: (context: UpdaterContext): Promise<void> => ipcRenderer.invoke('updater-download', context),
	updaterInstall: () => ipcRenderer.invoke('updater-install'),
	windowMinimize: (): void => {
		ipcRenderer.send('window-minimize');
	},
	windowMaximize: (): void => {
		ipcRenderer.send('window-maximize');
	},
	windowClose: (): void => {
		ipcRenderer.send('window-close');
	},
	windowIsMaximized: (): Promise<boolean> => ipcRenderer.invoke('window-is-maximized'),
	focusThemeStudioPopout: (): Promise<boolean> => ipcRenderer.invoke('theme-studio-popout-focus'),
	closeThemeStudioPopout: (): Promise<boolean> => ipcRenderer.invoke('theme-studio-popout-close'),
	popoutSetAlwaysOnTop: (key: string, flag: boolean): Promise<boolean> =>
		ipcRenderer.invoke('popout:set-always-on-top', key, flag),
	popoutFocus: (key: string): Promise<boolean> => ipcRenderer.invoke('popout:focus', key),
	openVoiceDebugEventSinkPopout: (entries: Array<DesktopVoiceDebugEventSinkEntry>): Promise<void> =>
		ipcRenderer.invoke('voice-debug-event-sink:open', entries),
	appendVoiceDebugEventSinkEntries: (entries: Array<DesktopVoiceDebugEventSinkEntry>): void => {
		ipcRenderer.send('voice-debug-event-sink:append', entries);
	},
	setVoiceDebugEventSinkStatsHtml: (html: string): void => {
		ipcRenderer.send('voice-debug-event-sink:set-stats-html', html);
	},
	onWindowMaximizeChange: (callback: (maximized: boolean) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, maximized: boolean): void => {
			callback(maximized);
		};
		ipcRenderer.on('window-maximize-change', handler);
		return () => {
			ipcRenderer.removeListener('window-maximize-change', handler);
		};
	},
	openExternal: (url: string): Promise<void> => ipcRenderer.invoke('open-external', url),
	clipboardWriteText: (text: string): Promise<void> => ipcRenderer.invoke('clipboard-write-text', text),
	clipboardReadText: (): Promise<string> => ipcRenderer.invoke('clipboard-read-text'),
	clipboardWriteFile: (options: ClipboardWriteFileOptions): Promise<ClipboardWriteFileResult> =>
		ipcRenderer.invoke('clipboard-write-file', options),
	pasteFromClipboard: (): Promise<void> => ipcRenderer.invoke('clipboard-paste'),
	onDeepLink: (callback: (url: string) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, url: string): void => {
			callback(url);
		};
		ipcRenderer.on('deep-link', handler);
		return () => {
			ipcRenderer.removeListener('deep-link', handler);
		};
	},
	getInitialDeepLink: (): Promise<string | null> => ipcRenderer.invoke('get-initial-deep-link'),
	onRpcNavigate: (callback: (path: string) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, path: string): void => {
			callback(path);
		};
		ipcRenderer.on('rpc-navigate', handler);
		return () => {
			ipcRenderer.removeListener('rpc-navigate', handler);
		};
	},
	autostartEnable: (): Promise<void> => ipcRenderer.invoke('autostart-enable'),
	autostartDisable: (): Promise<void> => ipcRenderer.invoke('autostart-disable'),
	autostartIsEnabled: (): Promise<boolean> => ipcRenderer.invoke('autostart-is-enabled'),
	autostartIsInitialized: (): Promise<boolean> => ipcRenderer.invoke('autostart-is-initialized'),
	autostartMarkInitialized: (): Promise<void> => ipcRenderer.invoke('autostart-mark-initialized'),
	checkMediaAccess: (type: MediaAccessType): Promise<MediaAccessStatus> =>
		ipcRenderer.invoke('check-media-access', type),
	requestMediaAccess: (type: MediaAccessType): Promise<boolean> => ipcRenderer.invoke('request-media-access', type),
	openMediaAccessSettings: (type: MediaAccessType): Promise<void> =>
		ipcRenderer.invoke('open-media-access-settings', type),
	openInputMonitoringSettings: (): Promise<void> => ipcRenderer.invoke('open-input-monitoring-settings'),
	getInputMonitoringPermissionStatus: (): Promise<InputMonitoringPermissionStatus> =>
		ipcRenderer.invoke('mac-tcc:status', 'input-monitoring'),
	requestInputMonitoringPermission: (): Promise<InputMonitoringPermissionStatus> =>
		ipcRenderer.invoke('mac-tcc:request', 'input-monitoring'),
	getScreenRecordingPermissionStatus: (): Promise<InputMonitoringPermissionStatus> =>
		ipcRenderer.invoke('mac-tcc:status', 'screen-recording'),
	requestScreenRecordingPermission: (): Promise<InputMonitoringPermissionStatus> =>
		ipcRenderer.invoke('mac-tcc:request', 'screen-recording'),
	downloadFile: (url: string, defaultPath: string): Promise<DownloadFileResult> =>
		ipcRenderer.invoke('download-file', {url, defaultPath}),
	passkeyIsSupported: (): Promise<boolean> => ipcRenderer.invoke('passkey-is-supported'),
	passkeyAuthenticate: (
		options: PublicKeyCredentialRequestOptionsJSON,
		requestContext?: {pin?: string},
	): Promise<AuthenticationResponseJSON> => ipcRenderer.invoke('passkey-authenticate', options, requestContext),
	passkeyRegister: (
		options: PublicKeyCredentialCreationOptionsJSON,
		requestContext?: {pin?: string},
	): Promise<RegistrationResponseJSON> => ipcRenderer.invoke('passkey-register', options, requestContext),
	switchInstanceUrl: (options: SwitchInstanceUrlOptions): Promise<void> =>
		ipcRenderer.invoke('switch-instance-url', options),
	consumeDesktopHandoffCode: (): Promise<string | null> => ipcRenderer.invoke('consume-desktop-handoff-code'),
	toggleDevTools: (): void => {
		ipcRenderer.send('toggle-devtools');
	},
	getDesktopSources: (
		types: Array<'screen' | 'window'>,
		requestId?: string,
		options?: GetDesktopSourcesOptions,
	): Promise<Array<DesktopSource>> => ipcRenderer.invoke('get-desktop-sources', types, requestId, options),
	onDisplayMediaRequested: (callback: (requestId: string, info: DisplayMediaRequestInfo) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, requestId: string, info: DisplayMediaRequestInfo): void => {
			callback(requestId, info);
		};
		ipcRenderer.on('display-media-requested', handler);
		return () => {
			ipcRenderer.removeListener('display-media-requested', handler);
		};
	},
	onDisplayMediaPortalEmpty: (callback: (requestId: string) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, requestId: string): void => {
			callback(requestId);
		};
		ipcRenderer.on('display-media-portal-empty', handler);
		return () => {
			ipcRenderer.removeListener('display-media-portal-empty', handler);
		};
	},
	selectDisplayMediaSource: (requestId: string, sourceId: string | null, withAudio: boolean): void => {
		ipcRenderer.send('select-display-media-source', requestId, sourceId, withAudio);
	},
	setDisplayMediaPortalPreference: (preference: DisplayMediaPortalSurfacePreference): Promise<void> =>
		ipcRenderer.invoke('set-display-media-portal-preference', preference),
	showNotification: (options: NotificationOptions): Promise<NotificationResult> =>
		ipcRenderer.invoke('show-notification', options),
	shouldPlayNotificationSound: (): Promise<boolean> => ipcRenderer.invoke('notification-sound-allowed'),
	getStreamerModeCaptureAppStatus: (): Promise<StreamerModeCaptureAppStatus> =>
		ipcRenderer.invoke('streamer-mode:get-capture-app-status'),
	closeNotification: (id: string): void => {
		ipcRenderer.send('close-notification', id);
	},
	closeNotifications: (ids: Array<string>): void => {
		ipcRenderer.send('close-notifications', ids);
	},
	onNotificationClick: (callback: (id: string, url?: string) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, id: string, url?: string): void => {
			callback(id, url);
		};
		ipcRenderer.on('notification-click', handler);
		return () => {
			ipcRenderer.removeListener('notification-click', handler);
		};
	},
	setBadgeCount: (count: number): void => {
		ipcRenderer.send('set-badge-count', count);
	},
	getBadgeCount: (): Promise<number> => ipcRenderer.invoke('get-badge-count'),
	setNativeLocale: (locale: string, strings: Record<string, string>): void => {
		ipcRenderer.send('native-locale-set', {locale, strings});
	},
	flashFrame: (persistent?: boolean): void => {
		ipcRenderer.send('window-flash', persistent === true);
	},
	stopFlashFrame: (): void => {
		ipcRenderer.send('window-stop-flash');
	},
	setTaskbarProgress: (fraction: number, mode?: 'normal' | 'indeterminate' | 'error' | 'paused' | 'none'): void => {
		ipcRenderer.send('taskbar-progress-set', {fraction, mode: mode ?? 'normal'});
	},
	onJumpListNewDm: (callback: () => void): (() => void) => {
		const handler = (): void => callback();
		ipcRenderer.on('jump-list-new-dm', handler);
		return () => ipcRenderer.removeListener('jump-list-new-dm', handler);
	},
	bounceDock: (type?: 'critical' | 'informational'): number => {
		return ipcRenderer.sendSync('bounce-dock', type ?? 'informational');
	},
	cancelBounceDock: (id: number): void => {
		ipcRenderer.send('cancel-bounce-dock', id);
	},
	setZoomFactor: (factor: number): void => {
		ipcRenderer.send('set-zoom-factor', factor);
	},
	getZoomFactor: (): Promise<number> => ipcRenderer.invoke('get-zoom-factor'),
	getAccessibilitySupportEnabled: (): Promise<boolean> => ipcRenderer.invoke('get-accessibility-support-enabled'),
	onAccessibilitySupportChanged: (callback: (enabled: boolean) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, enabled: boolean): void => callback(enabled);
		ipcRenderer.on('accessibility-support-changed', handler);
		return () => ipcRenderer.removeListener('accessibility-support-changed', handler);
	},
	getLinuxAppearance: (): Promise<LinuxAppearanceSnapshot> => ipcRenderer.invoke('linux-appearance-get'),
	onLinuxAppearanceChanged: (callback: (snapshot: LinuxAppearanceSnapshot) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, snapshot: LinuxAppearanceSnapshot): void => callback(snapshot);
		ipcRenderer.on('linux-appearance-changed', handler);
		return () => ipcRenderer.removeListener('linux-appearance-changed', handler);
	},
	onZoomIn: (callback: () => void): (() => void) => {
		const handler = (): void => callback();
		ipcRenderer.on('zoom-in', handler);
		return () => ipcRenderer.removeListener('zoom-in', handler);
	},
	onZoomOut: (callback: () => void): (() => void) => {
		const handler = (): void => callback();
		ipcRenderer.on('zoom-out', handler);
		return () => ipcRenderer.removeListener('zoom-out', handler);
	},
	onZoomReset: (callback: () => void): (() => void) => {
		const handler = (): void => callback();
		ipcRenderer.on('zoom-reset', handler);
		return () => ipcRenderer.removeListener('zoom-reset', handler);
	},
	onOpenSettings: (callback: () => void): (() => void) => {
		const handler = (): void => callback();
		ipcRenderer.on('open-settings', handler);
		return () => ipcRenderer.removeListener('open-settings', handler);
	},
	setTrayRuntimeState: (state: Partial<TrayRuntimeStatePayload>): void => {
		ipcRenderer.send('tray-runtime-state-update', state);
	},
	acquireStreamingPriority: (): void => {
		ipcRenderer.send('streaming-priority-acquire');
	},
	releaseStreamingPriority: (): void => {
		ipcRenderer.send('streaming-priority-release');
	},
	resetStreamingPriority: (): void => {
		ipcRenderer.send('streaming-priority-reset');
	},
	getStreamingPriorityDiagnostics: (): Promise<StreamingPriorityDiagnostics> =>
		ipcRenderer.invoke('streaming-priority-get-diagnostics'),
	onTrayAction: (callback: (action: TrayActionPayload) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, data: TrayActionPayload): void => callback(data);
		ipcRenderer.on('tray-action', handler);
		return () => ipcRenderer.removeListener('tray-action', handler);
	},
	globalKeyHookStart: (): Promise<boolean> => ipcRenderer.invoke('global-key-hook-start'),
	globalKeyHookStop: (): Promise<void> => ipcRenderer.invoke('global-key-hook-stop'),
	globalKeyHookIsRunning: (): Promise<boolean> => ipcRenderer.invoke('global-key-hook-is-running'),
	checkInputMonitoringAccess: (): Promise<boolean> => ipcRenderer.invoke('check-input-monitoring-access'),
	globalKeyHookRegister: (options: GlobalKeyHookRegisterOptions): Promise<void> =>
		ipcRenderer.invoke('global-key-hook-register', options),
	globalKeyHookUnregister: (id: string): Promise<void> => ipcRenderer.invoke('global-key-hook-unregister', id),
	globalKeyHookUnregisterAll: (): Promise<void> => ipcRenderer.invoke('global-key-hook-unregister-all'),
	linuxEvdevStatus: () => ipcRenderer.invoke('linux-evdev-status'),
	linuxEvdevGrantAccess: () => ipcRenderer.invoke('linux-evdev-grant-access'),
	onGlobalKeyEvent: (callback: (event: GlobalKeyEvent) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, data: GlobalKeyEvent): void => {
			callback(data);
		};
		ipcRenderer.on('global-key-event', handler);
		return () => {
			ipcRenderer.removeListener('global-key-event', handler);
		};
	},
	onGlobalMouseEvent: (callback: (event: GlobalMouseEvent) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, data: GlobalMouseEvent): void => {
			callback(data);
		};
		ipcRenderer.on('global-mouse-event', handler);
		return () => {
			ipcRenderer.removeListener('global-mouse-event', handler);
		};
	},
	onGlobalKeybindTriggered: (callback: (event: GlobalKeybindTriggeredEvent) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, data: GlobalKeybindTriggeredEvent): void => {
			callback(data);
		};
		ipcRenderer.on('global-keybind-triggered', handler);
		return () => {
			ipcRenderer.removeListener('global-keybind-triggered', handler);
		};
	},
	spellcheckGetState: (): Promise<SpellcheckState> => ipcRenderer.invoke('spellcheck-get-state'),
	spellcheckSetState: (state: Partial<SpellcheckState>): Promise<SpellcheckState> =>
		ipcRenderer.invoke('spellcheck-set-state', state),
	spellcheckGetAvailableLanguages: (): Promise<Array<string>> =>
		ipcRenderer.invoke('spellcheck-get-available-languages'),
	spellcheckGetBundledDictionaries: (): Promise<Array<SpellcheckBundledDictionary>> =>
		ipcRenderer.invoke('spellcheck:get-bundled-dictionaries'),
	spellcheckSuggest: (word: string): Promise<Array<string>> => ipcRenderer.invoke('spellcheck:suggest', word),
	onSpellcheckEngineResolved: (callback: (info: SpellcheckResolvedEngineInfo) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, data: SpellcheckResolvedEngineInfo): void => callback(data);
		ipcRenderer.on('spellcheck-engine-resolved', handler);
		return () => {
			ipcRenderer.removeListener('spellcheck-engine-resolved', handler);
		};
	},
	onSpellcheckStateChanged: (callback: (state: SpellcheckState) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, data: SpellcheckState): void => callback(data);
		ipcRenderer.on('spellcheck-state-changed', handler);
		return () => {
			ipcRenderer.removeListener('spellcheck-state-changed', handler);
		};
	},
	onTextareaContextMenu: (callback: (params: TextareaContextMenuParams) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, data: TextareaContextMenuParams): void => callback(data);
		ipcRenderer.on('textarea-context-menu', handler);
		return () => {
			ipcRenderer.removeListener('textarea-context-menu', handler);
		};
	},
	spellcheckReplaceMisspelling: (replacement: string): Promise<void> =>
		ipcRenderer.invoke('spellcheck-replace-misspelling', replacement),
	spellcheckAddWordToDictionary: (word: string): Promise<void> =>
		ipcRenderer.invoke('spellcheck-add-word-to-dictionary', word),
	virtmic: {
		getAvailability: (): Promise<VirtmicAvailability> => ipcRenderer.invoke('virtmic:get-availability'),
		listTargets: (options?: {
			granular?: boolean;
		}): Promise<{
			ok: boolean;
			targets?: Array<VirtmicNode>;
			availability: VirtmicAvailability;
		}> => ipcRenderer.invoke('virtmic:list', options),
		getRoutingGraph: (): Promise<VirtmicRoutingGraphResult> => ipcRenderer.invoke('virtmic:get-routing-graph'),
		startInclude: (include: Array<VirtmicNode>, options?: VirtmicLinkOptions): Promise<boolean> =>
			ipcRenderer.invoke('virtmic:start-include', include, options),
		startSystem: (exclude: Array<VirtmicNode>, options?: VirtmicSystemLinkOptions): Promise<boolean> =>
			ipcRenderer.invoke('virtmic:start-system', exclude, options),
		resolveWindowPid: (sourceId: string): Promise<number | null> =>
			ipcRenderer.invoke('virtmic:resolve-window-pid', sourceId),
		stop: (): Promise<void> => ipcRenderer.invoke('virtmic:stop'),
	},
	nativeAudio: {
		getAvailability: (): Promise<NativeAudioAvailability> => ipcRenderer.invoke('native-audio:get-availability'),
		listAudibleApplications: (): Promise<Array<NativeAudioApplication>> =>
			ipcRenderer.invoke('native-audio:list-applications'),
		resolveAudioRootPidForSource: (sourceId: string): Promise<number | null> =>
			ipcRenderer.invoke('native-audio:resolve-root-pid', sourceId),
		start: (options: NativeAudioStartOptions): Promise<NativeAudioStartResult> =>
			ipcRenderer.invoke('native-audio:start', options),
		stop: (captureId: string): Promise<void> => ipcRenderer.invoke('native-audio:stop', captureId),
		getRoutingGraph: (captureId?: string): Promise<NativeAudioRoutingGraphResult> =>
			ipcRenderer.invoke('native-audio:get-routing-graph', captureId),
		onFrame: (callback: (message: NativeAudioFrameMessage) => void): (() => void) => {
			const handler = (_event: Electron.IpcRendererEvent, message: NativeAudioFrameMessage): void => {
				callback(message);
			};
			ipcRenderer.on('native-audio:frame', handler);
			return () => ipcRenderer.removeListener('native-audio:frame', handler);
		},
		onEnd: (callback: (message: NativeAudioEndMessage) => void): (() => void) => {
			const handler = (_event: Electron.IpcRendererEvent, message: NativeAudioEndMessage): void => {
				callback(message);
			};
			ipcRenderer.on('native-audio:end', handler);
			return () => ipcRenderer.removeListener('native-audio:end', handler);
		},
	},
	nativeScreenCapture: {
		getAvailability: (): Promise<NativeScreenCaptureAvailability> =>
			ipcRenderer.invoke('native-screen-capture:get-availability'),
		listSources: (): Promise<Array<NativeScreenCaptureSource>> =>
			ipcRenderer.invoke('native-screen-capture:list-sources'),
		start: (options: NativeScreenCaptureStartOptions): Promise<NativeScreenCaptureStartResult> =>
			ipcRenderer.invoke('native-screen-capture:start', options),
		getDiagnostics: (captureId: string): Promise<NativeScreenCaptureDiagnostics | null> =>
			ipcRenderer.invoke('native-screen-capture:get-diagnostics', captureId),
		stop: (captureId: string): Promise<void> => ipcRenderer.invoke('native-screen-capture:stop', captureId),
		onEnd: (callback: (message: NativeScreenCaptureEndMessage) => void): (() => void) => {
			const handler = (_event: Electron.IpcRendererEvent, message: NativeScreenCaptureEndMessage): void => {
				callback(message);
			};
			ipcRenderer.on('native-screen-capture:end', handler);
			return () => ipcRenderer.removeListener('native-screen-capture:end', handler);
		},
		onLifecycleEvent: (callback: (message: NativeScreenCaptureLifecycleMessage) => void): (() => void) => {
			const handler = (_event: Electron.IpcRendererEvent, message: unknown): void => {
				const validated = validateNativeScreenCaptureLifecycleMessage(message);
				if (!validated) return;
				callback(validated);
			};
			ipcRenderer.on('native-screen-capture:lifecycle', handler);
			return () => ipcRenderer.removeListener('native-screen-capture:lifecycle', handler);
		},
	},
	voiceEngine: {
		bridgeVersion: VOICE_ENGINE_V2_BRIDGE_VERSION,
		isSupported: () => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.isSupported),
		getCapabilities: () => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.getCapabilities),
		prewarm: () => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.prewarm),
		getHardwareEncoderCapabilities: () =>
			ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.getHardwareEncoderCapabilities),
		connect: (options) => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.connect, options),
		disconnect: () => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.disconnect),
		isConnected: () => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.isConnected),
		publishMicrophone: (options) => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.publishMicrophone, options),
		pushPcm: (frame) => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.pushPcm, frame),
		publishScreen: (options) => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.publishScreen, options),
		updateScreenShareEncoding: (options) =>
			ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.updateScreenShareEncoding, options),
		unpublishScreen: () => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.unpublishScreen),
		publishScreenAudio: (options) => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.publishScreenAudio, options),
		pushScreenAudioPcm: (frame) => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.pushScreenAudioPcm, frame),
		pushScreenAudioFloat: (frame) => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.pushScreenAudioFloat, frame),
		unpublishScreenAudio: () => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.unpublishScreenAudio),
		setMicEnabled: (enabled) => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.setMicEnabled, enabled),
		setSpeakingDetection: (options) => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.setSpeakingDetection, options),
		listAudioInputDevices: () => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.listAudioInputDevices),
		listAudioOutputDevices: () => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.listAudioOutputDevices),
		setAudioOutputDevice: (deviceId) => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.setAudioOutputDevice, deviceId),
		setParticipantVolume: (participantSid, volume) =>
			ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.setParticipantVolume, {participantSid, volume}),
		setRemoteTrackSubscription: (options) =>
			ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.setRemoteTrackSubscription, options),
		publishData: (options) => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.publishData, options),
		listCameraDevices: () => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.listCameraDevices),
		publishCamera: (options) => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.publishCamera, options),
		publishNativeCameraSink: (options) =>
			ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.publishNativeCameraSink, options),
		publishProcessedCamera: (options) =>
			ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.publishProcessedCamera, options),
		pushProcessedCameraFrame: (frame) =>
			ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.pushProcessedCameraFrame, frame),
		pushCameraBackgroundFrame: (frame) =>
			ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.pushCameraBackgroundFrame, frame),
		clearCameraBackgroundFrame: () => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.clearCameraBackgroundFrame),
		updateCameraCapture: (options) => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.updateCameraCapture, options),
		publishDeviceScreenShare: (options) =>
			ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.publishDeviceScreenShare, options),
		unpublishCamera: () => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.unpublishCamera),
		isPublishingCamera: () => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.isPublishingCamera),
		startCameraPreview: (options) => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.startCameraPreview, options),
		stopCameraPreview: () => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.stopCameraPreview),
		getConnectionStats: () => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.getConnectionStats),
		getVoiceEngineReadiness: () => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.getVoiceEngineReadiness),
		getAudioDeviceModuleState: () => ipcRenderer.invoke(VOICE_ENGINE_V2_IPC_CHANNELS.getAudioDeviceModuleState),
		onEvent: (callback) => {
			const handler = (_event: Electron.IpcRendererEvent, payload: VoiceEngineV2BridgeEvent): void => {
				callback(payload);
			};
			ipcRenderer.on(VOICE_ENGINE_V2_EVENT_CHANNELS.event, handler);
			return () => ipcRenderer.removeListener(VOICE_ENGINE_V2_EVENT_CHANNELS.event, handler);
		},
		onVideoFrame: (callback) => {
			const handler = (_event: Electron.IpcRendererEvent, payload: VoiceEngineV2BridgeVideoFrameWire): void => {
				callback({meta: payload.meta, data: videoFrameWireDataToArrayBuffer(payload.data)});
			};
			ipcRenderer.on(VOICE_ENGINE_V2_EVENT_CHANNELS.videoFrame, handler);
			return () => ipcRenderer.removeListener(VOICE_ENGINE_V2_EVENT_CHANNELS.videoFrame, handler);
		},
	} satisfies VoiceEngineV2BridgeApi,
};

window.addEventListener(
	'contextmenu',
	(event) => {
		const target = event.target as HTMLElement | null;
		const isTextarea = Boolean(target?.closest?.('textarea'));
		ipcRenderer.send('spellcheck-context-target', {isTextarea});
	},
	true,
);

let spellcheckAutodetectTimer: NodeJS.Timeout | null = null;
let spellcheckAutodetectContextSequence = 0;
const SPELLCHECK_AUTODETECT_DEBOUNCE_MS = 750;
const SPELLCHECK_AUTODETECT_PREFIX_SKIP_CHARS = 20;
const spellcheckAutodetectContexts = new WeakMap<
	HTMLTextAreaElement,
	{
		contextId: string;
		lastSentText: string;
		pendingText: string;
	}
>();

const isTinySpellcheckPrefixChange = (previous: string, next: string): boolean => {
	if (previous.length === 0 || next.length === 0) return false;
	if (Math.abs(next.length - previous.length) >= SPELLCHECK_AUTODETECT_PREFIX_SKIP_CHARS) return false;
	return next.startsWith(previous) || previous.startsWith(next);
};

const getSpellcheckAutodetectContext = (target: HTMLTextAreaElement) => {
	let context = spellcheckAutodetectContexts.get(target);
	if (!context) {
		spellcheckAutodetectContextSequence++;
		context = {
			contextId: `textarea-${spellcheckAutodetectContextSequence}`,
			lastSentText: '',
			pendingText: '',
		};
		spellcheckAutodetectContexts.set(target, context);
	}
	return context;
};

const sendSpellcheckAutodetectText = (target: HTMLTextAreaElement): void => {
	const context = getSpellcheckAutodetectContext(target);
	context.pendingText = target.value;
	if (spellcheckAutodetectTimer) {
		clearTimeout(spellcheckAutodetectTimer);
	}
	spellcheckAutodetectTimer = setTimeout(() => {
		spellcheckAutodetectTimer = null;
		if (context.pendingText === context.lastSentText) return;
		if (isTinySpellcheckPrefixChange(context.lastSentText, context.pendingText)) return;
		context.lastSentText = context.pendingText;
		ipcRenderer.send('spellcheck:update-autodetect-text', {
			contextId: context.contextId,
			text: context.pendingText,
		});
	}, SPELLCHECK_AUTODETECT_DEBOUNCE_MS);
};

window.addEventListener(
	'input',
	(event) => {
		const target = event.target as HTMLTextAreaElement | null;
		if (!(target instanceof HTMLTextAreaElement)) return;
		sendSpellcheckAutodetectText(target);
	},
	true,
);

let providerInstalled = false;

const installSpellcheckProvider = () => {
	if (providerInstalled) return;
	providerInstalled = true;
	webFrame.setSpellCheckProvider('en-US', {
		spellCheck(words, callback) {
			const list = Array.isArray(words) ? words : [words as unknown as string];
			ipcRenderer
				.invoke('spellcheck:check-words', list)
				.then((misspelled: Array<string>) => callback(Array.isArray(misspelled) ? misspelled : []))
				.catch(() => callback([]));
		},
	});
};

ipcRenderer.on('spellcheck-engine-resolved', (_event, info: SpellcheckResolvedEngineInfo) => {
	if (info?.mode === 'hunspell') {
		installSpellcheckProvider();
	}
});

contextBridge.exposeInMainWorld('electron', api);
