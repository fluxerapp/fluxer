// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {app} from 'electron';
import log from 'electron-log';

const RUNTIME_CACHE_STATE_FILE = 'chromium-runtime-cache-state-v1.json';
const RUNTIME_CACHE_DIRECTORIES = [
	'Code Cache',
	'GPUCache',
	'DawnCache',
	'DawnGraphiteCache',
	'DawnWebGPUCache',
	'GrShaderCache',
	'GraphiteDawnCache',
	'ShaderCache',
	'Default/Code Cache',
	'Default/GPUCache',
	'Default/DawnCache',
	'Default/DawnGraphiteCache',
	'Default/DawnWebGPUCache',
	'Default/GrShaderCache',
	'Default/GraphiteDawnCache',
	'Default/ShaderCache',
	'Service Worker/ScriptCache',
	'Default/Service Worker/ScriptCache',
];
const MACOS_PRE_SEQUOIA_SCREEN_CAPTURE_DISABLED_FEATURES = [
	'ScreenCaptureKitMac',
	'ScreenCaptureKitMacWindow',
	'ScreenCaptureKitMacScreen',
	'ScreenCaptureKitPickerScreen',
	'ScreenCaptureKitStreamPickerSonoma',
	'WarmScreenCaptureSonoma',
	'UseSCContentSharingPicker',
];
const WINDOWS_NVIDIA_HEVC_DECODE_WORKAROUND_DEVICE_IDS = new Set([
	4928, 4929, 4932, 4934, 4935, 4936, 4937, 4939, 4941, 4942, 4943, 4986, 4987, 4992, 4993, 4994, 5008, 5009, 5010,
	5011, 5016, 5017, 5018, 5019, 5020, 5021, 5040, 5041, 5042, 5043, 5044, 5046, 5049, 5050, 5051, 5052, 5056, 5058,
	5079, 5080, 5081, 5082, 5104, 5105, 5106, 5107, 5112, 5113, 5114, 5115, 5121, 5126, 5127, 5159, 5655, 5656, 5657,
	5658, 5735, 5965, 5966, 6044, 6082, 6088, 6128, 6129, 6141,
]);

const PCI_VENDOR_NVIDIA = 0x10de;

type ElectronGpuInfo = {
	gpuDevice?: ReadonlyArray<{
		active?: boolean;
		vendorId?: number;
		deviceId?: number;
	}>;
	devices?: ReadonlyArray<{
		active?: boolean;
		vendorId?: number;
		deviceId?: number;
	}>;
};

const CONFIGURED_CHROMIUM_SWITCH_ALLOWLIST = new Set([
	'disable_accelerated_h264_decode',
	'disable_accelerated_h264_encode',
	'disable_accelerated_hevc_decode',
	'disable_d3d11',
	'disable_d3d11_video_decoder',
	'disable_decode_swap_chain',
	'disable_dxgi_zero_copy_video',
	'disable_dynamic_video_encode_framerate_update',
	'disable_media_foundation_clear_playback',
	'disable_media_foundation_frame_size_change',
	'disable_metal',
	'disable_nv12_dxgi_video',
	'force_high_performance_gpu',
	'force_low_power_gpu',
]);

const CHROMIUM_FEATURE_SWITCHES = new Set([
	'disable-blink-features',
	'disable-features',
	'enable-blink-features',
	'enable-features',
]);

const LINUX_FLAGS_CONFIG_FILE_NAMES: Record<'stable' | 'canary', Array<string>> = {
	stable: ['fluxer_desktop-flags.conf', 'fluxer-flags.conf'],
	canary: ['fluxer_desktop_canary-flags.conf', 'fluxer-canary-flags.conf'],
};

export const MIDDLE_CLICK_AUTOSCROLL_BLINK_FEATURE = 'MiddleClickAutoscroll';

export const BASE_DISABLED_CHROMIUM_FEATURES = [
	'WinRetrieveSuggestionsOnlyOnDemand',
	'HardwareMediaKeyHandling',
	'MediaSessionService',
	'UseEcoQoSForBackgroundProcess',
	'IntensiveWakeUpThrottling',
	'AllowAggressiveThrottlingWithWebSocket',
];

interface RuntimeCacheState {
	key?: string;
}

function safeFileFingerprint(filePath: string): {mtimeMs: number | null; size: number | null} {
	try {
		const stat = fs.statSync(filePath);
		return {mtimeMs: Math.trunc(stat.mtimeMs), size: stat.size};
	} catch {
		return {mtimeMs: null, size: null};
	}
}

function getRuntimeCacheKey(): string {
	const executable = safeFileFingerprint(process.execPath);
	const appBundle = safeFileFingerprint(path.join(process.resourcesPath, 'app.asar'));
	return JSON.stringify({
		appVersion: app.getVersion(),
		electronVersion: process.versions.electron ?? null,
		chromeVersion: process.versions.chrome ?? null,
		nodeVersion: process.versions.node ?? null,
		v8Version: process.versions.v8 ?? null,
		executable,
		appBundle,
	});
}

function readRuntimeCacheState(filePath: string): RuntimeCacheState {
	try {
		return JSON.parse(fs.readFileSync(filePath, 'utf8')) as RuntimeCacheState;
	} catch {
		return {};
	}
}

function writeRuntimeCacheState(filePath: string, state: RuntimeCacheState): void {
	fs.writeFileSync(filePath, JSON.stringify(state, null, 2), 'utf8');
}

function resolveUserDataChild(userDataPath: string, relativePath: string): string | null {
	const userDataRoot = path.resolve(userDataPath);
	const target = path.resolve(userDataRoot, relativePath);
	if (target !== userDataRoot && target.startsWith(`${userDataRoot}${path.sep}`)) {
		return target;
	}
	return null;
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return value !== null && typeof value === 'object';
}

function splitChromiumFeatureList(value: string): Array<string> {
	if (!value) return [];
	return value
		.split(',')
		.map((part) => part.trim())
		.filter(Boolean);
}

function appendChromiumFeatureSwitch(switchName: string, features: Iterable<string>): void {
	const featureSet = new Set(splitChromiumFeatureList(app.commandLine.getSwitchValue(switchName)));
	for (const feature of features) {
		const trimmed = feature.trim();
		if (trimmed.length > 0) {
			featureSet.add(trimmed);
		}
	}
	if (featureSet.size > 0) {
		app.commandLine.appendSwitch(switchName, Array.from(featureSet).join(','));
	}
}

function parseChromiumSwitchArgument(argument: string): {name: string; value?: string} | null {
	const trimmed = argument.trim();
	if (!trimmed.startsWith('--') || trimmed === '--') {
		return null;
	}
	const switchText = trimmed.slice(2);
	const equalsIndex = switchText.indexOf('=');
	if (equalsIndex >= 0) {
		const name = switchText.slice(0, equalsIndex).trim();
		const value = switchText.slice(equalsIndex + 1).trim();
		if (!isChromiumSwitchName(name)) return null;
		return {name, value: stripMatchingQuotes(value)};
	}
	const whitespaceIndex = switchText.search(/\s/);
	if (whitespaceIndex >= 0) {
		const name = switchText.slice(0, whitespaceIndex).trim();
		const value = switchText.slice(whitespaceIndex + 1).trim();
		if (!isChromiumSwitchName(name)) return null;
		return value.length > 0 ? {name, value: stripMatchingQuotes(value)} : {name};
	}
	if (!isChromiumSwitchName(switchText)) return null;
	return {name: switchText};
}

function isChromiumSwitchName(name: string): boolean {
	return /^[A-Za-z0-9][A-Za-z0-9_.-]*$/.test(name);
}

function stripMatchingQuotes(value: string): string {
	if (value.length < 2) return value;
	const first = value[0];
	const last = value[value.length - 1];
	if ((first === '"' && last === '"') || (first === "'" && last === "'")) {
		return value.slice(1, -1);
	}
	return value;
}

function parseChromiumFlagsConfig(contents: string): Array<string> {
	return contents
		.split(/\r?\n/)
		.map((line) => line.trim())
		.filter((line) => line.length > 0 && !line.startsWith('#'));
}

function getLinuxConfigHome(): string | null {
	const xdgConfigHome = process.env.XDG_CONFIG_HOME?.trim();
	if (xdgConfigHome) {
		return xdgConfigHome;
	}
	const home = os.homedir();
	return home ? path.join(home, '.config') : null;
}

function appendChromiumCommandLineArgument(argument: string): boolean {
	const parsed = parseChromiumSwitchArgument(argument);
	if (!parsed) {
		log.warn('[ChromiumRuntime] Ignoring invalid Chromium flags config entry', {argument});
		return false;
	}
	if (parsed.value !== undefined && CHROMIUM_FEATURE_SWITCHES.has(parsed.name)) {
		appendChromiumFeatureSwitch(parsed.name, splitChromiumFeatureList(parsed.value));
		return true;
	}
	if (parsed.value !== undefined) {
		app.commandLine.appendSwitch(parsed.name, parsed.value);
		return true;
	}
	app.commandLine.appendSwitch(parsed.name);
	return true;
}

export function appendEnabledBlinkFeature(feature: string): void {
	appendChromiumFeatureSwitch('enable-blink-features', [feature]);
}

export function hasEnabledBlinkFeature(feature: string): boolean {
	return splitChromiumFeatureList(app.commandLine.getSwitchValue('enable-blink-features')).includes(feature);
}

export function appendConfiguredChromiumSwitches(rawSwitches: unknown): void {
	const switches = Array.isArray(rawSwitches) ? rawSwitches : isRecord(rawSwitches) ? Object.keys(rawSwitches) : [];
	for (const chromiumSwitch of switches) {
		if (typeof chromiumSwitch !== 'string') continue;
		if (!CONFIGURED_CHROMIUM_SWITCH_ALLOWLIST.has(chromiumSwitch)) continue;
		app.commandLine.appendSwitch(chromiumSwitch);
	}
}

export function appendEnabledChromiumFeatures(features: Iterable<string>): void {
	appendChromiumFeatureSwitch('enable-features', features);
}

export function appendDisabledChromiumFeatures(features: Iterable<string>): void {
	appendChromiumFeatureSwitch('disable-features', features);
}

export function appendLinuxChromiumFlagsConfig(channel: 'stable' | 'canary'): void {
	if (process.platform !== 'linux') return;
	const configHome = getLinuxConfigHome();
	if (!configHome) {
		log.warn('[ChromiumRuntime] Cannot resolve Linux Chromium flags config directory');
		return;
	}
	for (const fileName of LINUX_FLAGS_CONFIG_FILE_NAMES[channel]) {
		const filePath = path.join(configHome, fileName);
		if (!fs.existsSync(filePath)) continue;
		try {
			const flags = parseChromiumFlagsConfig(fs.readFileSync(filePath, 'utf8'));
			let applied = 0;
			let ignored = 0;
			for (const flag of flags) {
				if (appendChromiumCommandLineArgument(flag)) {
					applied += 1;
				} else {
					ignored += 1;
				}
			}
			log.info('[ChromiumRuntime] Loaded Linux Chromium flags config', {path: filePath, applied, ignored});
		} catch (error) {
			log.warn('[ChromiumRuntime] Failed to load Linux Chromium flags config', {path: filePath, error});
		}
	}
}

export function addMacosPreSequoiaScreenCaptureDisabledFeatures(features: Set<string>): void {
	if (process.platform !== 'darwin') return;
	const majorRelease = Number.parseInt(os.release().split('.')[0] ?? '', 10);
	if (!Number.isFinite(majorRelease) || majorRelease >= 24) return;
	for (const feature of MACOS_PRE_SEQUOIA_SCREEN_CAPTURE_DISABLED_FEATURES) {
		features.add(feature);
	}
}

export function addLinuxHardwareVideoEncodeFeatures(features: Set<string>): void {
	if (process.platform !== 'linux') return;
	features.add('AcceleratedVideoEncoder');
}

export interface ChromiumCommandLine {
	appendSwitch(name: string, value?: string): void;
	getSwitchValue(name: string): string;
	hasSwitch(name: string): boolean;
}

export function addLinuxScreenCapturePipeWireFeature(features: Set<string>): void {
	if (process.platform !== 'linux') return;
	features.add('WebRTCPipeWireCapturer');
}

export function hasConfiguredOzonePlatformSwitch(commandLine: ChromiumCommandLine = app.commandLine): boolean {
	return commandLine.hasSwitch('ozone-platform') || commandLine.getSwitchValue('ozone-platform').length > 0;
}

export function appendLinuxOzonePlatformHint(commandLine: ChromiumCommandLine = app.commandLine): void {
	if (process.platform !== 'linux') return;
	if (hasConfiguredOzonePlatformSwitch(commandLine)) return;
	if (commandLine.hasSwitch('ozone-platform-hint')) return;
	commandLine.appendSwitch('ozone-platform-hint', 'auto');
}

export function addWindowsHardwareVideoEncodeFeatures(features: Set<string>): void {
	if (process.platform !== 'win32') return;
	features.add('WebRtcAV1HWEncode');
}

export async function appendWindowsGpuDriverWorkaroundSwitches(): Promise<void> {
	if (process.platform !== 'win32') return;
	try {
		const info = (await app.getGPUInfo('basic')) as ElectronGpuInfo;
		const devices = info.gpuDevice ?? info.devices ?? [];
		for (const gpu of devices) {
			if (
				gpu.active === true &&
				gpu.vendorId === PCI_VENDOR_NVIDIA &&
				typeof gpu.deviceId === 'number' &&
				WINDOWS_NVIDIA_HEVC_DECODE_WORKAROUND_DEVICE_IDS.has(gpu.deviceId)
			) {
				app.commandLine.appendSwitch('disable_accelerated_hevc_decode', '1');
				log.info('Windows NVIDIA HEVC decode workaround enabled', {deviceId: gpu.deviceId});
				return;
			}
		}
	} catch (error) {
		log.warn('Failed to evaluate Windows GPU driver workarounds', {error});
	}
}

export function purgeChromiumRuntimeCachesIfNeeded(userDataPath: string): void {
	try {
		fs.mkdirSync(userDataPath, {recursive: true});
	} catch (error) {
		log.warn('[ChromiumRuntime] Failed to create user data directory for runtime cache guard', {error});
		return;
	}
	const stateFilePath = path.join(userDataPath, RUNTIME_CACHE_STATE_FILE);
	const cacheKey = getRuntimeCacheKey();
	const previousState = readRuntimeCacheState(stateFilePath);
	if (previousState.key === cacheKey) {
		return;
	}
	const removed: Array<string> = [];
	const errors: Array<{path: string; error: unknown}> = [];
	for (const relativePath of RUNTIME_CACHE_DIRECTORIES) {
		const target = resolveUserDataChild(userDataPath, relativePath);
		if (!target) {
			continue;
		}
		try {
			if (fs.existsSync(target)) {
				fs.rmSync(target, {recursive: true, force: true});
				removed.push(relativePath);
			}
		} catch (error) {
			errors.push({path: relativePath, error});
		}
	}
	if (errors.length > 0) {
		log.warn('[ChromiumRuntime] Failed to purge one or more runtime caches after an app/runtime change', {
			removed,
			errors,
		});
		return;
	}
	try {
		writeRuntimeCacheState(stateFilePath, {key: cacheKey});
		log.info('[ChromiumRuntime] Runtime cache guard completed', {removed});
	} catch (error) {
		log.warn('[ChromiumRuntime] Failed to write runtime cache state', {error});
	}
}
