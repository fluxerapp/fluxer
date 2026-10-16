// SPDX-License-Identifier: AGPL-3.0-or-later

export interface LinuxAppearanceSnapshot {
	colorScheme: 'no-preference' | 'prefer-dark' | 'prefer-light';
	contrast: 'no-preference' | 'high';
	accent: {r: number; g: number; b: number} | null;
}

export interface ChromiumRuntimeInfo {
	enableFeatures: Array<string>;
	disableFeatures: Array<string>;
	switches: Array<string>;
}

export interface GpuDeviceInfo {
	active: boolean;
	vendorId: number;
	deviceId: number;
	vendorName?: string;
	deviceString?: string;
	driverVendor?: string;
	driverVersion?: string;
	dedicatedVideoMemory?: number;
	sharedSystemMemory?: number;
	subsystemVendorId?: number;
	subsystemDeviceId?: number;
	registryId?: string;
	adapterLuid?: string;
	pciPath?: string;
	integrated?: boolean;
	removable?: boolean;
	headless?: boolean;
	source?: 'metal' | 'dxgi' | 'linux-sysfs' | 'electron';
}

export interface GpuInfo {
	devices: ReadonlyArray<GpuDeviceInfo>;
	glRenderer?: string;
	glVendor?: string;
	machineModelName?: string;
	machineModelVersion?: string;
	nativeSource?: 'metal' | 'dxgi' | 'linux-sysfs';
}

export type StreamingPriorityDiagnostics = Record<string, unknown>;

export interface CpuInfo {
	model: string;
	speed: number;
	cores: number;
	physicalCores: number;
}

export interface ProcessMetrics {
	cpu: {
		percentCPUUsage: number;
	};
	memory: {
		workingSetSize: number;
		peakWorkingSetSize: number;
		privateBytes?: number;
	};
	pid: number;
	type: string;
	name?: string;
}

export interface AppMetricsSnapshot {
	cpuInfo: CpuInfo;
	processes: Array<ProcessMetrics>;
	totalMemoryMB: number;
	freeMemoryMB: number;
}

export interface ThemeLocalFileReference {
	id: string;
	name: string;
	path: string;
	mimeType: string;
	size: number;
}

export interface ThemeLocalFileReadResult {
	path: string;
	dataUrl?: string;
	error?: string;
}

export interface ThemeDirectoryCssFile {
	fileName: string;
	path: string;
	css: string;
}

export type ThemeLinkedFileError = 'not_allowed' | 'missing' | 'not_file' | 'too_large' | 'too_many' | 'read_failed';

export interface ThemeLinkedFileChange {
	path: string;
	css?: string;
	error?: ThemeLinkedFileError;
}

export type VoiceBackgroundMediaKind = 'static' | 'animated' | 'video';

export interface VoiceBackgroundMediaCacheRequest {
	id: string;
	mimeType: string;
	fileName?: string;
	data: ArrayBuffer;
}

export interface VoiceBackgroundMediaCacheResult {
	path: string;
	mediaKind: VoiceBackgroundMediaKind;
}

export interface VoiceBackgroundMediaReadResult {
	path: string;
	mediaKind: VoiceBackgroundMediaKind;
	dataUrl: string;
}

export interface DesktopTroubleshootingSettings {
	disableHardwareAcceleration: boolean;
	disableModuleUpdates: boolean;
}

export interface SetDesktopTroubleshootingDisableHardwareAccelerationOptions {
	disable: boolean;
	restart?: boolean;
}

export interface SetDesktopTroubleshootingDisableModuleUpdatesOptions {
	disable: boolean;
}

export interface DesktopVoiceDebugEventSinkEntry {
	sequence: number;
	line: string;
}

export type ClipboardWriteFileMediaType = 'image' | 'gif' | 'video' | 'audio';

export interface ClipboardWriteFileOptions {
	url: string;
	suggestedName?: string;
	mediaType: ClipboardWriteFileMediaType;
}

export interface ClipboardWriteFileResult {
	success: boolean;
	path?: string;
	error?: string;
}

export type UpdaterDownloadFormat = 'setup' | 'dmg' | 'zip' | 'appimage' | 'deb' | 'rpm' | 'tar_gz';

export interface UpdaterDownloadOption {
	format: UpdaterDownloadFormat;
	label: string;
	url: string;
	suggestedName?: string;
	sha256?: string | null;
}

export type MediaAccessType = 'microphone' | 'camera' | 'screen' | 'audio-capture';

export type MediaAccessStatus = 'granted' | 'denied' | 'not-determined' | 'restricted' | 'unknown';

export type InputMonitoringPermissionStatus = 'granted' | 'denied' | 'not-determined' | 'unsupported';

export type VirtmicBackend = 'pipewire';

export type VirtmicNode = Record<string, string>;

export interface VirtmicLinkOptions {
	ignoreDevices?: boolean;
	ignoreInputMedia?: boolean;
	ignoreVirtual?: boolean;
	workaround?: boolean;
}

export type NativeAudioBackend = 'macos-sck' | 'macos-coreaudio' | 'windows-wasapi-loopback' | 'linux-pipewire';

export interface NativeAudioAvailability {
	available: boolean;
	backend?: NativeAudioBackend;
	capabilities?: {
		process: boolean;
		system: boolean;
		systemExcludesSelf?: boolean;
		processInclude?: boolean;
		processExclude?: boolean;
		sessionMixer?: boolean;
		endpointSelect?: boolean;
		systemLoopbackMode?: 'process-exclude' | 'session-mixer' | 'unavailable';
	};
	reason?:
		| 'unsupported-platform'
		| 'addon-not-installed'
		| 'load-failed'
		| 'os-version-too-old'
		| 'permission-denied'
		| 'no-pipewire'
		| 'disabled-by-launch';
	detail?: string;
}

export interface NativeAudioApplication {
	pid: number;
	identifier: string;
	name: string;
	audible?: boolean;
}

export interface NativeAudioStartOptions {
	targetPid?: number;
	includeProcessTree?: boolean;
	macBackend?: 'sck' | 'coreaudio' | 'auto';
	macCaptureScope?: 'process' | 'system';
	winCaptureScope?: 'process' | 'system' | 'session-mixer';
	winEndpoints?: Array<string>;
	linuxRule?: VirtmicLinkOptions & {
		include?: Array<VirtmicNode>;
		exclude?: Array<VirtmicNode>;
		onlySpeakers?: boolean;
		onlyDefaultSpeakers?: boolean;
	};
}

export interface NativeAudioPlaybackDeviceSession {
	pid: number;
	rootPid: number;
	exe: string;
	state: 'active' | 'inactive';
	excluded: boolean;
}

export interface NativeAudioPlaybackDevice {
	id: string;
	name: string;
	isDefault: boolean;
	sessions: Array<NativeAudioPlaybackDeviceSession>;
}

export interface NativeAudioStartResult {
	captureId: string;
	sampleRate: number;
	channels: number;
}

export interface NativeAudioFrameMessage {
	captureId: string;
	sampleRate: number;
	channels: number;
	timestampUs: number;
	samples: ArrayBuffer;
}

export type NativeAudioEndReason = 'stopped' | 'target-exited' | 'addon-error';

export interface NativeAudioEndMessage {
	captureId: string;
	reason: NativeAudioEndReason;
	detail?: string;
}

export type NativeScreenCaptureLifecycleEventKind = 'error' | 'closed' | 'closed-clean' | 'stalled' | 'diagnostic';

export type NativeScreenCaptureLifecycleSource = 'delegate' | 'programmatic';

export interface NativeScreenCaptureLifecycleMessage {
	captureId: string;
	kind: NativeScreenCaptureLifecycleEventKind;
	message: string;
	source?: NativeScreenCaptureLifecycleSource;
}

export type DisplayMediaPortalSurfacePreference = 'window' | 'monitor';

export interface StreamerModeCaptureProcess {
	name: string;
	pid?: number;
}

export interface StreamerModeCaptureAppStatus {
	detected: boolean;
	processes: Array<StreamerModeCaptureProcess>;
}

export type TrayPresenceStatus = 'online' | 'idle' | 'dnd' | 'invisible';

export interface TrayRuntimeStatePayload {
	voiceConnected: boolean;
	voiceChannelLabel: string | null;
	selfMute: boolean;
	selfDeaf: boolean;
	presenceStatus: TrayPresenceStatus | null;
	buildInfo: string | null;
}

export type GlobalShortcutsBackend = 'portal' | 'x11' | 'evdev' | 'windows' | 'macos' | 'none';

export type GlobalShortcutsPortalState =
	| 'unknown'
	| 'probing'
	| 'unsupported'
	| 'not-set-up'
	| 'binding'
	| 'bound'
	| 'declined'
	| 'error';

export interface GlobalShortcutsPortalShortcut {
	action: string;
	triggerDescription: string | null;
}

export interface GlobalShortcutCombo {
	code?: string;
	key: string;
	ctrl: boolean;
	alt: boolean;
	shift: boolean;
	meta: boolean;
	mouseButton?: number;
	modifierOnly?: boolean;
	bothSides?: boolean;
}

export interface GlobalShortcutBinding {
	sourceId: string;
	action: string;
	combo: GlobalShortcutCombo;
}

export interface GlobalShortcutActionDefinition {
	action: string;
	description: string;
	preferredCombo: GlobalShortcutCombo | null;
}

export interface GlobalShortcutsSyncPayload {
	bindings: Array<GlobalShortcutBinding>;
	actions: Array<GlobalShortcutActionDefinition>;
}

export interface GlobalShortcutEvent {
	action: string;
	sourceId: string;
	phase: 'press' | 'release';
}

export interface GlobalShortcutCaptureEvent {
	type: 'keydown' | 'keyup' | 'mousedown' | 'mouseup';
	code: string | null;
	key: string | null;
	button: number | null;
	ctrl: boolean;
	alt: boolean;
	shift: boolean;
	meta: boolean;
}

export interface GlobalKeyHookRegisterOptions {
	id: string;
	description?: string;
	keycode?: number;
	keyName?: string;
	physicalKeyName?: string;
	mouseButton?: number;
	ctrl?: boolean;
	alt?: boolean;
	shift?: boolean;
	meta?: boolean;
}

export interface GlobalKeyEvent {
	type: 'keydown' | 'keyup';
	keycode: number;
	keyName: string;
	backend?: 'evdev' | 'native' | null;
	altKey: boolean;
	ctrlKey: boolean;
	shiftKey: boolean;
	metaKey: boolean;
}

export interface GlobalMouseEvent {
	type: 'mousedown' | 'mouseup';
	button: number;
	altKey: boolean;
	ctrlKey: boolean;
	shiftKey: boolean;
	metaKey: boolean;
}

export interface GlobalKeybindTriggeredEvent {
	id: string;
	type: 'keydown' | 'keyup';
}

export type SpellcheckEngine = 'auto' | 'hunspell' | 'system';

export interface SpellcheckBundledDictionary {
	tag: string;
	package: string;
	displayName: string;
	nativeName: string;
}

export interface SpellcheckResolvedEngineInfo {
	mode: 'hunspell' | 'system' | 'off';
	hunspellLangs: Array<string>;
	systemLangs: Array<string>;
}
