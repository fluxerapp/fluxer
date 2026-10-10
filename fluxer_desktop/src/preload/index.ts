// SPDX-License-Identifier: AGPL-3.0-or-later

import {BUILD_CHANNEL} from '@electron/common/BuildChannel';
import {DESKTOP_PREBOOT_THEME_CHANNEL, LOCAL_DEVELOPMENT_INSTANCE_URL} from '@electron/common/Constants';
import {IS_OFFLINE_BUILD} from '@electron/common/OfflineBuild';
import {readExactPlainRecord} from '@electron/common/PlainRecord';
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
	GlobalShortcutCaptureEvent,
	GlobalShortcutEvent,
	GlobalShortcutsStatus,
	GlobalShortcutsSyncPayload,
	GpuInfo,
	InputMonitoringPermissionStatus,
	LinuxAppearanceSnapshot,
	MediaAccessStatus,
	MediaAccessType,
	NativeAudioApplication,
	NativeAudioAvailability,
	NativeAudioEndMessage,
	NativeAudioFrameMessage,
	NativeAudioStartOptions,
	NativeAudioStartResult,
	NativeScreenCaptureLifecycleEventKind,
	NativeScreenCaptureLifecycleMessage,
	NativeScreenCaptureLifecycleSource,
	NotificationOptions,
	NotificationResult,
	SetDesktopTroubleshootingDisableHardwareAccelerationOptions,
	SpellcheckBundledDictionary,
	SpellcheckResolvedEngineInfo,
	SpellcheckState,
	StreamerModeCaptureAppStatus,
	StreamingPriorityDiagnostics,
	TextareaContextMenuParams,
	ThemeLinkedFileChange,
	TrayActionPayload,
	TrayRuntimeStatePayload,
	UpdaterContext,
	UpdaterEvent,
	VirtmicAvailability,
	VirtmicNode,
} from '@electron/common/Types';
import {createBrowserHandoffPreloadAPI} from '@electron/preload/BrowserHandoffPreload';
import {createDesktopStoragePreloadAPI} from '@electron/preload/DesktopStoragePreload';
import {createFirstContentPaintSignal} from '@electron/preload/FirstContentPaint';
import {applyLegacyRawLocalStorage} from '@electron/preload/LegacyRawStorageSeed';
import {createLocalAppPreloadAPI} from '@electron/preload/LocalAppPreload';
import {createNativeGatewayPreloadAPI} from '@electron/preload/NativeGatewayPreload';
import type {DesktopCapabilityManifest} from '@fluxer/desktop_ipc/src/CapabilityManifest';
import {DESKTOP_CAPABILITY_MANIFEST_CHANNEL} from '@fluxer/desktop_ipc/src/CapabilityManifest';
import {DESKTOP_LAST_ROUTE_CHANNEL} from '@fluxer/desktop_ipc/src/LastRouteContract';
import {
	buildNativeTitlebarStartupHtml,
	NATIVE_TITLEBAR_CLASS,
	NATIVE_TITLEBAR_CONTROL_ATTR,
	type NativeTitlebarControlName,
	type NativeTitlebarLayout,
} from '@fluxer/desktop_ipc/src/NativeTitlebarShell';
import {ZOOM_PREBOOT_MIRROR_STORAGE_KEY} from '@fluxer/desktop_ipc/src/StorageContract';
import {
	VOICE_ENGINE_V2_HARDWARE_ENCODER_IPC_CHANNEL,
	type VoiceEngineV2BridgeHardwareEncoderApi,
} from '@fluxer/voice_engine_v2/bridge';
import type {
	AuthenticationResponseJSON,
	PublicKeyCredentialCreationOptionsJSON,
	PublicKeyCredentialRequestOptionsJSON,
	RegistrationResponseJSON,
} from '@simplewebauthn/browser';
import {contextBridge, ipcRenderer, webFrame} from 'electron';

const ACCESSIBILITY_STORE_STORAGE_KEY = 'AccessibilityStore';
const ACTIVE_ALLOW_TRANSPARENCY_RENDERER_ARG = '--fluxer-active-allow-transparency=1';
const ACTIVE_USE_NATIVE_TITLEBAR_RENDERER_ARG = '--fluxer-active-use-native-titlebar=1';
const CUSTOM_TITLEBAR_HEIGHT = '32px';
const NATIVE_TITLEBAR_HEIGHT = '0px';
const STARTUP_NATIVE_TITLEBAR_ID = 'fluxer-startup-native-titlebar';
const THEME_STUDIO_STANDALONE_PATHNAME = '/theme-studio';
const DESKTOP_CAPABILITY_MANIFEST_KEYS = Object.freeze(['appStore', 'gatewaySocket'] as const);

class DesktopCapabilityManifestContractError extends TypeError {
	public constructor(reason: string, options?: ErrorOptions) {
		super(`Desktop capability manifest contract violation: ${reason}`, options);
		this.name = 'DesktopCapabilityManifestContractError';
	}
}

interface AccessibilityStartupSettings {
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
		syncReducedMotionWithSystem:
			typeof parsed.syncReducedMotionWithSystem === 'boolean' ? parsed.syncReducedMotionWithSystem : true,
		reducedMotionOverride: typeof parsed.reducedMotionOverride === 'boolean' ? parsed.reducedMotionOverride : null,
	};
}

function hasPrebootZoomMirror(): boolean {
	let raw: string | null;
	try {
		raw = window.localStorage.getItem(ZOOM_PREBOOT_MIRROR_STORAGE_KEY);
	} catch {
		return false;
	}
	return raw !== null && Number.isFinite(Number.parseInt(raw, 10));
}

function readAccessibilityStartupSettings(): AccessibilityStartupSettings | null {
	const hasZoomMirror = hasPrebootZoomMirror();
	const legacySettings = readLegacyAccessibilityStartupSettings();
	if (!hasZoomMirror && legacySettings === null) {
		return null;
	}
	return {
		syncReducedMotionWithSystem: legacySettings?.syncReducedMotionWithSystem ?? true,
		reducedMotionOverride: legacySettings?.reducedMotionOverride ?? null,
	};
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

function getStartupPlatform(): string {
	return getStartupPlatformClass().replace('platform-', '');
}

function startupNativeTitlebarLayout(): NativeTitlebarLayout {
	return process.platform === 'darwin' ? 'macos' : 'controls';
}

function wireStartupNativeTitlebarControls(titlebar: HTMLElement): void {
	titlebar.querySelectorAll<HTMLButtonElement>(`[${NATIVE_TITLEBAR_CONTROL_ATTR}]`).forEach((button) => {
		const control = button.getAttribute(NATIVE_TITLEBAR_CONTROL_ATTR) as NativeTitlebarControlName | null;
		if (control == null) return;
		button.addEventListener('click', () => {
			ipcRenderer.send(`window-${control}`);
		});
	});
}

function installStartupNativeTitlebar(activeUseNativeTitleBar: boolean): void {
	if (
		activeUseNativeTitleBar ||
		window.location.protocol === 'data:' ||
		window.location.pathname === THEME_STUDIO_STANDALONE_PATHNAME
	) {
		return;
	}
	const layout = startupNativeTitlebarLayout();
	const install = (): void => {
		if (!document.body || document.getElementById(STARTUP_NATIVE_TITLEBAR_ID)) return;
		const titlebar = document.createElement('div');
		titlebar.id = STARTUP_NATIVE_TITLEBAR_ID;
		titlebar.className = NATIVE_TITLEBAR_CLASS.root;
		titlebar.setAttribute('role', 'group');
		titlebar.setAttribute('aria-hidden', 'true');
		titlebar.setAttribute('data-native-titlebar', '');
		titlebar.setAttribute('data-platform', getStartupPlatform());
		titlebar.innerHTML = buildNativeTitlebarStartupHtml(layout);
		if (layout === 'controls') {
			wireStartupNativeTitlebarControls(titlebar);
		}
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
	webFrame.setZoomFactor(1);
	applyReducedMotionClass(resolveReducedMotion(settings));
}

function readDesktopCapabilityManifest(): DesktopCapabilityManifest {
	let value: unknown;
	try {
		value = ipcRenderer.sendSync(DESKTOP_CAPABILITY_MANIFEST_CHANNEL);
	} catch (error) {
		throw new DesktopCapabilityManifestContractError('the main process request failed', {cause: error});
	}
	const manifest = readExactPlainRecord({value, expectedKeys: DESKTOP_CAPABILITY_MANIFEST_KEYS});
	if (manifest === null) {
		throw new DesktopCapabilityManifestContractError(
			'the main process response must be a plain object with exactly appStore and gatewaySocket',
		);
	}
	if (typeof manifest.appStore !== 'boolean') {
		throw new DesktopCapabilityManifestContractError('appStore must be a boolean');
	}
	if (typeof manifest.gatewaySocket !== 'boolean') {
		throw new DesktopCapabilityManifestContractError('gatewaySocket must be a boolean');
	}
	return {appStore: manifest.appStore, gatewaySocket: manifest.gatewaySocket};
}

function reportPrebootTheme(renderer: typeof ipcRenderer): void {
	let theme: string | null = null;
	try {
		theme = window.localStorage.getItem('theme');
	} catch {
		return;
	}
	if (typeof theme === 'string' && theme.length > 0) {
		renderer.send(DESKTOP_PREBOOT_THEME_CHANNEL, theme);
	}
}

applyStartupDesktopWindowClasses();

applyLegacyRawLocalStorage(ipcRenderer);

reportPrebootTheme(ipcRenderer);
window.addEventListener('pagehide', () => reportPrebootTheme(ipcRenderer));
document.addEventListener('visibilitychange', () => {
	if (document.visibilityState === 'hidden') {
		reportPrebootTheme(ipcRenderer);
	}
});

applyStartupAccessibilitySettings();

const api: ElectronAPI = {
	platform: process.platform,
	buildChannel: BUILD_CHANNEL,
	localDevelopmentInstanceUrl: LOCAL_DEVELOPMENT_INSTANCE_URL,
	capabilities: readDesktopCapabilityManifest(),
	offlineBuild: IS_OFFLINE_BUILD,
	...createDesktopStoragePreloadAPI(ipcRenderer),
	...createLocalAppPreloadAPI(ipcRenderer),
	...createBrowserHandoffPreloadAPI(ipcRenderer),
	...createNativeGatewayPreloadAPI(ipcRenderer),
	reportLastRoute: (routePath: string): void => {
		ipcRenderer.send(DESKTOP_LAST_ROUTE_CHANNEL, routePath);
	},
	notifyFirstContentPainted: createFirstContentPaintSignal(ipcRenderer, (callback) =>
		window.requestAnimationFrame(callback),
	),
	getDesktopInfo: (): Promise<DesktopInfo> => ipcRenderer.invoke('get-desktop-info'),
	getGpuInfo: (): Promise<GpuInfo> => ipcRenderer.invoke('get-gpu-info'),
	getAppMetrics: (): Promise<AppMetricsSnapshot> => ipcRenderer.invoke('get-app-metrics'),
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
	pickThemeLinkedFiles: (options?: {multiple?: boolean}) => ipcRenderer.invoke('theme-linked-files-pick', options),
	watchThemeLinkedFiles: (paths: Array<string>) => ipcRenderer.invoke('theme-linked-files-watch', paths),
	onThemeLinkedFileChange: (callback: (change: ThemeLinkedFileChange) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, change: ThemeLinkedFileChange) => callback(change);
		ipcRenderer.on('theme-linked-file-changed', handler);
		return () => ipcRenderer.removeListener('theme-linked-file-changed', handler);
	},
	cacheVoiceBackgroundMedia: (options) => ipcRenderer.invoke('voice-background-media-cache:write', options),
	readVoiceBackgroundMedia: (id) => ipcRenderer.invoke('voice-background-media-cache:read', id),
	deleteVoiceBackgroundMedia: (id) => ipcRenderer.invoke('voice-background-media-cache:delete', id),
	getDesktopTroubleshootingSettings: (): Promise<DesktopTroubleshootingSettings> =>
		ipcRenderer.invoke('desktop-troubleshooting-get'),
	setDesktopDisableHardwareAcceleration: (
		options: SetDesktopTroubleshootingDisableHardwareAccelerationOptions,
	): Promise<DesktopTroubleshootingSettings> =>
		ipcRenderer.invoke('desktop-troubleshooting-set-disable-hardware-acceleration', options),
	onUpdaterEvent: (callback: (event: UpdaterEvent) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, data: UpdaterEvent) => callback(data);
		ipcRenderer.on('updater-event', handler);
		return () => ipcRenderer.removeListener('updater-event', handler);
	},
	updaterCheck: (context: UpdaterContext): Promise<void> => ipcRenderer.invoke('updater-check', context),
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
	onWindowLiveResizeChange: (callback: (resizing: boolean) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, resizing: boolean): void => {
			callback(resizing);
		};
		ipcRenderer.on('window-live-resize-change', handler);
		return () => {
			ipcRenderer.removeListener('window-live-resize-change', handler);
		};
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
	downloadFile: (url: string, defaultPath: string, sha256?: string | null): Promise<DownloadFileResult> =>
		ipcRenderer.invoke('download-file', {url, defaultPath, sha256}),
	passkeyIsSupported: (): Promise<boolean> => ipcRenderer.invoke('passkey-is-supported'),
	passkeyAuthenticate: (
		options: PublicKeyCredentialRequestOptionsJSON,
		requestContext?: {pin?: string; instanceKey?: string},
	): Promise<AuthenticationResponseJSON> => ipcRenderer.invoke('passkey-authenticate', options, requestContext),
	passkeyRegister: (
		options: PublicKeyCredentialCreationOptionsJSON,
		requestContext?: {pin?: string; instanceKey?: string},
	): Promise<RegistrationResponseJSON> => ipcRenderer.invoke('passkey-register', options, requestContext),
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
	setZoomFactor: (factor: number): void => {
		ipcRenderer.send('set-zoom-factor', factor);
	},
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
	getStreamingPriorityDiagnostics: (): Promise<StreamingPriorityDiagnostics> =>
		ipcRenderer.invoke('streaming-priority-get-diagnostics'),
	onTrayAction: (callback: (action: TrayActionPayload) => void): (() => void) => {
		const handler = (_event: Electron.IpcRendererEvent, data: TrayActionPayload): void => callback(data);
		ipcRenderer.on('tray-action', handler);
		return () => ipcRenderer.removeListener('tray-action', handler);
	},
	globalKeyHookStart: (): Promise<boolean> => ipcRenderer.invoke('global-key-hook-start'),
	globalKeyHookStop: (): Promise<void> => ipcRenderer.invoke('global-key-hook-stop'),
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
	globalShortcuts: {
		sync: (payload: GlobalShortcutsSyncPayload): Promise<void> => ipcRenderer.invoke('global-shortcuts:sync', payload),
		setPaused: (paused: boolean): Promise<void> => ipcRenderer.invoke('global-shortcuts:set-paused', paused),
		getStatus: (): Promise<GlobalShortcutsStatus> => ipcRenderer.invoke('global-shortcuts:get-status'),
		onStatus: (callback: (status: GlobalShortcutsStatus) => void): (() => void) => {
			const handler = (_event: Electron.IpcRendererEvent, data: GlobalShortcutsStatus): void => {
				callback(data);
			};
			ipcRenderer.on('global-shortcuts:status', handler);
			return () => {
				ipcRenderer.removeListener('global-shortcuts:status', handler);
			};
		},
		onEvent: (callback: (event: GlobalShortcutEvent) => void): (() => void) => {
			const handler = (_event: Electron.IpcRendererEvent, data: GlobalShortcutEvent): void => {
				callback(data);
			};
			ipcRenderer.on('global-shortcut-event', handler);
			return () => {
				ipcRenderer.removeListener('global-shortcut-event', handler);
			};
		},
		setUp: (): Promise<GlobalShortcutsStatus> => ipcRenderer.invoke('global-shortcuts:set-up'),
		recheck: (): Promise<GlobalShortcutsStatus> => ipcRenderer.invoke('global-shortcuts:recheck'),
		configure: (): Promise<void> => ipcRenderer.invoke('global-shortcuts:configure'),
		setDirectInputEnabled: (enabled: boolean): Promise<GlobalShortcutsStatus> =>
			ipcRenderer.invoke('global-shortcuts:set-direct-input-enabled', enabled),
		startCapture: (): Promise<number | null> => ipcRenderer.invoke('global-shortcuts:start-capture'),
		stopCapture: (captureId: number): Promise<void> => ipcRenderer.invoke('global-shortcuts:stop-capture', captureId),
		onCapture: (callback: (event: GlobalShortcutCaptureEvent) => void): (() => void) => {
			const handler = (_event: Electron.IpcRendererEvent, data: GlobalShortcutCaptureEvent): void => {
				callback(data);
			};
			ipcRenderer.on('global-shortcuts:capture', handler);
			return () => {
				ipcRenderer.removeListener('global-shortcuts:capture', handler);
			};
		},
	},
	spellcheckSetState: (state: Partial<SpellcheckState>): Promise<SpellcheckState> =>
		ipcRenderer.invoke('spellcheck-set-state', state),
	spellcheckGetAvailableLanguages: (): Promise<Array<string>> =>
		ipcRenderer.invoke('spellcheck-get-available-languages'),
	spellcheckGetBundledDictionaries: (): Promise<Array<SpellcheckBundledDictionary>> =>
		ipcRenderer.invoke('spellcheck:get-bundled-dictionaries'),
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
		setRule: (captureId: string, linuxRule: NonNullable<NativeAudioStartOptions['linuxRule']>): Promise<boolean> =>
			ipcRenderer.invoke('native-audio:set-rule', captureId, linuxRule),
		stop: (captureId: string): Promise<void> => ipcRenderer.invoke('native-audio:stop', captureId),
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
		getHardwareEncoderCapabilities: () => ipcRenderer.invoke(VOICE_ENGINE_V2_HARDWARE_ENCODER_IPC_CHANNEL),
	} satisfies VoiceEngineV2BridgeHardwareEncoderApi,
};

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
