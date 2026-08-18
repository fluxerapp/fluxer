// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {BUILD_CHANNEL} from '@electron/common/BuildChannel';
import {CANARY_APP_URL, STABLE_APP_URL} from '@electron/common/Constants';
import type {DesktopTroubleshootingSettings, DesktopWindowBehaviorSettings} from '@electron/common/Types';
import log from 'electron-log';

export type {DesktopTroubleshootingSettings, DesktopWindowBehaviorSettings} from '@electron/common/Types';

const CONFIG_FILE_NAME = 'settings.json';
const MINIMIZE_TO_TRAY_STORAGE_KEY_V2 = 'minimizeToTrayV2';
const CLOSE_TO_TRAY_STORAGE_KEY_V2 = 'closeToTrayV2';

interface DesktopConfig extends Record<string, unknown> {
	chromiumSwitches?: ChromiumSwitchesSetting;
	window_behavior?: PersistedDesktopWindowBehaviorSettings;
	troubleshooting?: PersistedDesktopTroubleshootingSettings;
	theme_allowed_local_files?: Array<string>;
}

export type ChromiumSwitchesSetting = ReadonlyArray<string> | Record<string, unknown>;

interface PersistedDesktopWindowBehaviorSettings {
	showTrayIcon?: boolean;
	useNativeTitleBar?: boolean;
	minimizeToTrayV2?: boolean;
	closeToTrayV2?: boolean;
	rememberWindowState?: boolean;
	allowTransparency?: boolean;
	smoothScrolling?: boolean;
	middleClickAutoscroll?: boolean;
}

interface PersistedDesktopTroubleshootingSettings {
	disableHardwareAcceleration?: boolean;
}

let config: DesktopConfig = {};
let configPath: string | null = null;
let runtimeAppUrlOverride: string | null = null;

function getDefaultDesktopTroubleshootingSettings(): DesktopTroubleshootingSettings {
	return {
		disableHardwareAcceleration: false,
	};
}

function getDefaultDesktopWindowBehaviorSettings(): DesktopWindowBehaviorSettings {
	return {
		showTrayIcon: true,
		minimizeToTray: false,
		closeToTray: true,
		useNativeTitleBar: false,
		activeUseNativeTitleBar: false,
		rememberWindowState: true,
		allowTransparency: false,
		activeAllowTransparency: false,
		smoothScrolling: true,
		activeSmoothScrolling: true,
		middleClickAutoscroll: false,
		activeMiddleClickAutoscroll: false,
	};
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return value !== null && typeof value === 'object';
}

function sanitizePersistedDesktopWindowBehaviorSettings(
	value: unknown,
): PersistedDesktopWindowBehaviorSettings | undefined {
	if (!isRecord(value)) {
		return undefined;
	}
	const settings: PersistedDesktopWindowBehaviorSettings = {};
	if (typeof value.showTrayIcon === 'boolean') {
		settings.showTrayIcon = value.showTrayIcon;
	}
	if (typeof value.useNativeTitleBar === 'boolean') {
		settings.useNativeTitleBar = value.useNativeTitleBar;
	}
	if (typeof value.rememberWindowState === 'boolean') {
		settings.rememberWindowState = value.rememberWindowState;
	}
	if (typeof value.allowTransparency === 'boolean') {
		settings.allowTransparency = value.allowTransparency;
	}
	if (typeof value.smoothScrolling === 'boolean') {
		settings.smoothScrolling = value.smoothScrolling;
	}
	if (typeof value.middleClickAutoscroll === 'boolean') {
		settings.middleClickAutoscroll = value.middleClickAutoscroll;
	}
	const minimizeToTrayV2 = value[MINIMIZE_TO_TRAY_STORAGE_KEY_V2];
	if (typeof minimizeToTrayV2 === 'boolean') {
		settings.minimizeToTrayV2 = minimizeToTrayV2;
	}
	const closeToTrayV2 = value[CLOSE_TO_TRAY_STORAGE_KEY_V2];
	if (typeof closeToTrayV2 === 'boolean') {
		settings.closeToTrayV2 = closeToTrayV2;
	}
	return Object.keys(settings).length > 0 ? settings : undefined;
}

function sanitizePersistedDesktopTroubleshootingSettings(
	value: unknown,
): PersistedDesktopTroubleshootingSettings | undefined {
	if (!isRecord(value)) {
		return undefined;
	}
	const settings: PersistedDesktopTroubleshootingSettings = {};
	if (typeof value.disableHardwareAcceleration === 'boolean') {
		settings.disableHardwareAcceleration = value.disableHardwareAcceleration;
	}
	return Object.keys(settings).length > 0 ? settings : undefined;
}

function sanitizeChromiumSwitchesSetting(value: unknown): ChromiumSwitchesSetting | undefined {
	if (Array.isArray(value)) {
		const switches = value.filter((item): item is string => typeof item === 'string' && item.trim().length > 0);
		return switches.length > 0 ? switches : undefined;
	}
	if (isRecord(value)) {
		const switches: Record<string, unknown> = {};
		for (const [key, switchValue] of Object.entries(value)) {
			if (key.trim().length === 0) continue;
			switches[key] = switchValue;
		}
		return Object.keys(switches).length > 0 ? switches : undefined;
	}
	return undefined;
}

function sanitizeDesktopConfig(value: unknown): DesktopConfig {
	if (!isRecord(value)) {
		return {};
	}
	const nextConfig: DesktopConfig = {...value};
	delete nextConfig.app_url;
	const chromiumSwitches = sanitizeChromiumSwitchesSetting(value.chromiumSwitches);
	if (chromiumSwitches) {
		nextConfig.chromiumSwitches = chromiumSwitches;
	} else {
		delete nextConfig.chromiumSwitches;
	}
	const windowBehavior = sanitizePersistedDesktopWindowBehaviorSettings(value.window_behavior);
	if (windowBehavior) {
		nextConfig.window_behavior = windowBehavior;
	} else {
		delete nextConfig.window_behavior;
	}
	const troubleshooting = sanitizePersistedDesktopTroubleshootingSettings(value.troubleshooting);
	if (troubleshooting) {
		nextConfig.troubleshooting = troubleshooting;
	} else {
		delete nextConfig.troubleshooting;
	}
	if (Array.isArray(value.theme_allowed_local_files)) {
		nextConfig.theme_allowed_local_files = value.theme_allowed_local_files.filter(
			(item): item is string => typeof item === 'string' && item.trim().length > 0,
		);
	} else {
		delete nextConfig.theme_allowed_local_files;
	}
	return nextConfig;
}

function normalizeDesktopWindowBehaviorSettings(
	settings?: PersistedDesktopWindowBehaviorSettings | Partial<DesktopWindowBehaviorSettings>,
): DesktopWindowBehaviorSettings {
	const defaults = getDefaultDesktopWindowBehaviorSettings();
	const normalizedSettings = settings as
		| (PersistedDesktopWindowBehaviorSettings & Partial<DesktopWindowBehaviorSettings>)
		| undefined;
	const normalized = {
		showTrayIcon:
			typeof normalizedSettings?.showTrayIcon === 'boolean' ? normalizedSettings.showTrayIcon : defaults.showTrayIcon,
		minimizeToTray:
			typeof normalizedSettings?.minimizeToTray === 'boolean'
				? normalizedSettings.minimizeToTray
				: typeof normalizedSettings?.minimizeToTrayV2 === 'boolean'
					? normalizedSettings.minimizeToTrayV2
					: defaults.minimizeToTray,
		closeToTray:
			typeof normalizedSettings?.closeToTray === 'boolean'
				? normalizedSettings.closeToTray
				: typeof normalizedSettings?.closeToTrayV2 === 'boolean'
					? normalizedSettings.closeToTrayV2
					: defaults.closeToTray,
		useNativeTitleBar:
			typeof normalizedSettings?.useNativeTitleBar === 'boolean'
				? normalizedSettings.useNativeTitleBar
				: defaults.useNativeTitleBar,
		activeUseNativeTitleBar:
			typeof normalizedSettings?.activeUseNativeTitleBar === 'boolean'
				? normalizedSettings.activeUseNativeTitleBar
				: typeof normalizedSettings?.useNativeTitleBar === 'boolean'
					? normalizedSettings.useNativeTitleBar
					: defaults.useNativeTitleBar,
		rememberWindowState:
			typeof normalizedSettings?.rememberWindowState === 'boolean'
				? normalizedSettings.rememberWindowState
				: defaults.rememberWindowState,
		allowTransparency:
			typeof normalizedSettings?.allowTransparency === 'boolean'
				? normalizedSettings.allowTransparency
				: defaults.allowTransparency,
		activeAllowTransparency:
			typeof normalizedSettings?.activeAllowTransparency === 'boolean'
				? normalizedSettings.activeAllowTransparency
				: typeof normalizedSettings?.allowTransparency === 'boolean'
					? normalizedSettings.allowTransparency
					: defaults.allowTransparency,
		smoothScrolling:
			typeof normalizedSettings?.smoothScrolling === 'boolean'
				? normalizedSettings.smoothScrolling
				: defaults.smoothScrolling,
		activeSmoothScrolling:
			typeof normalizedSettings?.activeSmoothScrolling === 'boolean'
				? normalizedSettings.activeSmoothScrolling
				: typeof normalizedSettings?.smoothScrolling === 'boolean'
					? normalizedSettings.smoothScrolling
					: defaults.smoothScrolling,
		middleClickAutoscroll:
			typeof normalizedSettings?.middleClickAutoscroll === 'boolean'
				? normalizedSettings.middleClickAutoscroll
				: defaults.middleClickAutoscroll,
		activeMiddleClickAutoscroll:
			typeof normalizedSettings?.activeMiddleClickAutoscroll === 'boolean'
				? normalizedSettings.activeMiddleClickAutoscroll
				: typeof normalizedSettings?.middleClickAutoscroll === 'boolean'
					? normalizedSettings.middleClickAutoscroll
					: defaults.middleClickAutoscroll,
	};
	if (!normalized.showTrayIcon) {
		normalized.minimizeToTray = false;
		normalized.closeToTray = false;
	}
	return normalized;
}

function serializeDesktopWindowBehaviorSettings(
	settings: DesktopWindowBehaviorSettings,
): PersistedDesktopWindowBehaviorSettings {
	return {
		showTrayIcon: settings.showTrayIcon,
		useNativeTitleBar: settings.useNativeTitleBar,
		rememberWindowState: settings.rememberWindowState,
		allowTransparency: settings.allowTransparency,
		smoothScrolling: settings.smoothScrolling,
		middleClickAutoscroll: settings.middleClickAutoscroll,
		[MINIMIZE_TO_TRAY_STORAGE_KEY_V2]: settings.minimizeToTray,
		[CLOSE_TO_TRAY_STORAGE_KEY_V2]: settings.closeToTray,
	};
}

function normalizeDesktopTroubleshootingSettings(
	settings?: PersistedDesktopTroubleshootingSettings | Partial<DesktopTroubleshootingSettings>,
): DesktopTroubleshootingSettings {
	const defaults = getDefaultDesktopTroubleshootingSettings();
	return {
		disableHardwareAcceleration:
			typeof settings?.disableHardwareAcceleration === 'boolean'
				? settings.disableHardwareAcceleration
				: defaults.disableHardwareAcceleration,
	};
}

function serializeDesktopTroubleshootingSettings(
	settings: DesktopTroubleshootingSettings,
): PersistedDesktopTroubleshootingSettings {
	return {
		disableHardwareAcceleration: settings.disableHardwareAcceleration,
	};
}

function saveDesktopConfig(): void {
	if (!configPath) {
		log.warn('Desktop config path not initialised; cannot save settings');
		return;
	}
	const tempPath = `${configPath}.${process.pid}.tmp`;
	try {
		fs.writeFileSync(tempPath, JSON.stringify(config, null, 2), 'utf-8');
		fs.renameSync(tempPath, configPath);
		log.debug('Saved desktop config to', configPath);
	} catch (error) {
		log.error('Failed to save desktop config:', error);
		try {
			fs.rmSync(tempPath, {force: true});
		} catch {}
	}
}

export function loadDesktopConfig(userDataPath: string): void {
	configPath = path.join(userDataPath, CONFIG_FILE_NAME);
	try {
		if (fs.existsSync(configPath)) {
			const data = fs.readFileSync(configPath, 'utf-8');
			config = sanitizeDesktopConfig(JSON.parse(data));
			log.info('Loaded desktop config from', configPath);
		}
	} catch (error) {
		log.error('Failed to load desktop config:', error);
	}
}

export function getAppUrl(): string {
	if (runtimeAppUrlOverride) {
		return runtimeAppUrlOverride;
	}
	return BUILD_CHANNEL === 'canary' ? CANARY_APP_URL : STABLE_APP_URL;
}

export function getCustomAppUrl(): string | null {
	return runtimeAppUrlOverride;
}

export function setRuntimeAppUrlOverride(appUrl: string | null): void {
	runtimeAppUrlOverride = appUrl;
}

export function getConfiguredChromiumSwitches(): ChromiumSwitchesSetting | undefined {
	return sanitizeChromiumSwitchesSetting(config.chromiumSwitches);
}

export function getDesktopWindowBehaviorSettings(): DesktopWindowBehaviorSettings {
	return normalizeDesktopWindowBehaviorSettings(config.window_behavior);
}

export function setDesktopWindowBehaviorSettings(
	settings: Partial<DesktopWindowBehaviorSettings>,
): DesktopWindowBehaviorSettings {
	config.window_behavior = serializeDesktopWindowBehaviorSettings(
		normalizeDesktopWindowBehaviorSettings({
			...getDesktopWindowBehaviorSettings(),
			...settings,
		}),
	);
	saveDesktopConfig();
	return getDesktopWindowBehaviorSettings();
}

export function getDesktopTroubleshootingSettings(): DesktopTroubleshootingSettings {
	return normalizeDesktopTroubleshootingSettings(config.troubleshooting);
}

export function setDesktopTroubleshootingSettings(
	settings: Partial<DesktopTroubleshootingSettings>,
): DesktopTroubleshootingSettings {
	config.troubleshooting = serializeDesktopTroubleshootingSettings(
		normalizeDesktopTroubleshootingSettings({
			...getDesktopTroubleshootingSettings(),
			...settings,
		}),
	);
	saveDesktopConfig();
	return getDesktopTroubleshootingSettings();
}

export function getAllowedThemeLocalFiles(): Array<string> {
	return Array.isArray(config.theme_allowed_local_files) ? [...config.theme_allowed_local_files] : [];
}

export function addAllowedThemeLocalFiles(paths: ReadonlyArray<string>): Array<string> {
	const next = new Set(getAllowedThemeLocalFiles());
	for (const filePath of paths) {
		if (filePath.trim().length > 0) {
			next.add(filePath);
		}
	}
	config.theme_allowed_local_files = [...next].sort();
	saveDesktopConfig();
	return getAllowedThemeLocalFiles();
}

export function clearAllowedThemeLocalFiles(): void {
	delete config.theme_allowed_local_files;
	saveDesktopConfig();
}
