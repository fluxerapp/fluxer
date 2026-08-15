// SPDX-License-Identifier: AGPL-3.0-or-later

import path from 'node:path';
import {
	getAppUrl,
	getCustomAppUrl,
	getDesktopTroubleshootingSettings,
	getDesktopWindowBehaviorSettings,
} from '@electron/common/DesktopConfig';
import type {DesktopInfo, DesktopTroubleshootingSettings, DesktopWindowBehaviorSettings} from '@electron/common/Types';
import {isPortableMode} from '@electron/common/UserDataPath';
import {hasEnabledBlinkFeature, MIDDLE_CLICK_AUTOSCROLL_BLINK_FEATURE} from '@electron/main/ChromiumRuntime';
import {getDesktopInfo} from '@electron/main/PlatformInfo';
import {app} from 'electron';
import log from 'electron-log';

const DEBUG_INFO_ARGS = new Set(['--fluxer-debug-info', '--fluxer-client-info', '--client-info']);
const DISABLE_HARDWARE_ACCELERATION_ARGS = new Set(['--fluxer-disable-gpu', '--fluxer-disable-hardware-acceleration']);
const OPEN_DEVTOOLS_ARGS = new Set(['--fluxer-devtools', '--fluxer-open-devtools']);
const RESET_WINDOW_STATE_ARGS = new Set(['--fluxer-reset-window-state']);
const SAFE_MODE_ARGS = new Set(['--fluxer-safe-mode']);
const RENDERER_CONSOLE_LOG_ARGS = new Set(['--fluxer-log-renderer-console']);
const NET_LOG_ARGS = new Set(['--fluxer-net-log']);
const APP_URL_ARGS = new Set(['--fluxer-app-url']);

interface DesktopDebugInfo {
	clientInfo: string;
	desktopInfo: DesktopInfo;
	appUrl: string;
	customAppUrl: string | null;
	userDataPath: string;
	logsPath: string | null;
	logFilePath: string | null;
	configPath: string;
	windowBehavior: DesktopWindowBehaviorSettings;
	troubleshooting: DesktopTroubleshootingSettings;
	packaged: boolean;
	portable: boolean;
	pid: number;
	execPath: string;
	appImagePath: string | null;
	appDirPath: string | null;
	cwd: string;
	launchArgs: Array<string>;
}

interface DesktopDebugInfoOptions {
	nativeProbes?: boolean;
}

interface DesktopClientInfoOptions {
	locale?: string | null;
}

function getDesktopWindowBehaviorDebugSettings(): DesktopWindowBehaviorSettings {
	return {
		...getDesktopWindowBehaviorSettings(),
		activeSmoothScrolling: !app.commandLine.hasSwitch('disable-smooth-scrolling'),
		activeMiddleClickAutoscroll:
			process.platform === 'linux' && hasEnabledBlinkFeature(MIDDLE_CLICK_AUTOSCROLL_BLINK_FEATURE),
	};
}

export function hasDesktopDebugInfoArg(argv: ReadonlyArray<string>): boolean {
	return hasFlag(argv, DEBUG_INFO_ARGS);
}

export function shouldDisableHardwareAccelerationForLaunch(argv: ReadonlyArray<string>): boolean {
	return hasFlag(argv, DISABLE_HARDWARE_ACCELERATION_ARGS) || isSafeModeLaunch(argv);
}

export function shouldOpenDevToolsOnLaunch(argv: ReadonlyArray<string>): boolean {
	return hasFlag(argv, OPEN_DEVTOOLS_ARGS);
}

export function shouldResetWindowStateOnLaunch(argv: ReadonlyArray<string>): boolean {
	return hasFlag(argv, RESET_WINDOW_STATE_ARGS);
}

export function shouldIgnoreWindowStateForLaunch(argv: ReadonlyArray<string>): boolean {
	return isSafeModeLaunch(argv);
}

export function shouldForwardRendererConsoleToMainLog(argv: ReadonlyArray<string>): boolean {
	return hasFlag(argv, RENDERER_CONSOLE_LOG_ARGS);
}

function hasFlag(argv: ReadonlyArray<string>, flags: ReadonlySet<string>): boolean {
	return argv.some((arg) => flags.has(arg) || [...flags].some((flag) => arg.startsWith(`${flag}=`)));
}

function isSafeModeLaunch(argv: ReadonlyArray<string>): boolean {
	return hasFlag(argv, SAFE_MODE_ARGS);
}

function getArgValue(argv: ReadonlyArray<string>, names: ReadonlySet<string>): string | null {
	for (let index = 0; index < argv.length; index += 1) {
		const arg = argv[index];
		for (const name of names) {
			if (arg === name) {
				const next = argv[index + 1];
				return next && !next.startsWith('--') ? next : '';
			}
			if (arg.startsWith(`${name}=`)) {
				return arg.slice(name.length + 1);
			}
		}
	}
	return null;
}

export function getLaunchAppUrlOverride(argv: ReadonlyArray<string>): string | null {
	const value = getArgValue(argv, APP_URL_ARGS);
	if (value === null) {
		return null;
	}
	const trimmed = value.trim();
	if (!trimmed) {
		throw new Error('--fluxer-app-url requires a URL');
	}
	const candidate = /^[a-zA-Z][a-zA-Z0-9+\-.]*:\/\//.test(trimmed) ? trimmed : `https://${trimmed}`;
	const url = new URL(candidate);
	if (url.protocol !== 'https:' && url.protocol !== 'http:') {
		throw new Error('--fluxer-app-url must use http or https');
	}
	return url.toString();
}

export function getLaunchNetLogPath(userDataPath: string, argv: ReadonlyArray<string>): string | null {
	const value = getArgValue(argv, NET_LOG_ARGS);
	if (value === null) {
		return null;
	}
	const trimmed = value.trim();
	if (trimmed.length > 0) {
		return path.resolve(trimmed);
	}
	return path.join(userDataPath, `netlog-${new Date().toISOString().replace(/[:.]/g, '-')}.json`);
}

function safeGetLogsPath(): string | null {
	try {
		return app.getPath('logs');
	} catch {
		return null;
	}
}

function getLogFilePath(): string | null {
	try {
		const file = log.transports.file.getFile();
		return file?.path ?? null;
	} catch {
		return null;
	}
}

function sanitizeLaunchArg(arg: string): string {
	if (/^fluxer:\/\//i.test(arg)) {
		return 'fluxer://<redacted>';
	}
	if (arg.startsWith('--fluxer-app-url=')) {
		return `--fluxer-app-url=${sanitizeUrlForDiagnostics(arg.slice('--fluxer-app-url='.length))}`;
	}
	if (/^(--[^=]*(?:token|secret|password|key|code)[^=]*)=/i.test(arg)) {
		return `${arg.slice(0, arg.indexOf('='))}=<redacted>`;
	}
	return arg;
}

function sanitizeLaunchArgs(argv: ReadonlyArray<string>): Array<string> {
	const sanitized: Array<string> = [];
	for (let index = 0; index < argv.length; index += 1) {
		const arg = argv[index];
		if (arg === '--fluxer-app-url') {
			sanitized.push(arg);
			const next = argv[index + 1];
			if (next && !next.startsWith('--')) {
				sanitized.push(sanitizeUrlForDiagnostics(next));
				index += 1;
			}
			continue;
		}
		sanitized.push(sanitizeLaunchArg(arg));
	}
	return sanitized;
}

function sanitizeUrlForDiagnostics(rawUrl: string): string {
	try {
		const candidate = /^[a-zA-Z][a-zA-Z0-9+\-.]*:\/\//.test(rawUrl) ? rawUrl : `https://${rawUrl}`;
		const url = new URL(candidate);
		url.username = '';
		url.password = '';
		url.search = '';
		url.hash = '';
		return url.toString();
	} catch {
		return '<invalid-url>';
	}
}

function normalizeArchitectureValue(value: string | null | undefined): string | undefined {
	if (!value) return undefined;
	const trimmed = value.trim();
	if (/\barm64\b|\baarch64\b|\barmv8\b|\barm64e\b/i.test(trimmed)) return 'arm64';
	if (/\barm\b|\barmv7\b|\barmv6\b/i.test(trimmed)) return 'arm';
	if (/\bx86_64\b|\bx64\b|\bamd64\b|\bwin64\b|\bwow64\b/i.test(trimmed)) return 'x64';
	if (/\bx86\b|\bi[3-6]86\b/i.test(trimmed)) return 'x86';
	return trimmed || undefined;
}

function formatSystemArchitecture(desktopInfo: DesktopInfo): string {
	return (
		normalizeArchitectureValue(desktopInfo.hardwareArch || desktopInfo.arch) ??
		desktopInfo.hardwareArch ??
		desktopInfo.arch
	);
}

function getWindowsVersionName(osVersion: string): string {
	const parts = osVersion.split('.');
	const majorVersion = parseInt(parts[0], 10);
	const buildNumber = parseInt(parts[2], 10);
	if (majorVersion === 10) {
		if (buildNumber >= 22000) {
			return 'Windows 11';
		}
		return 'Windows 10';
	}
	return 'Windows';
}

function formatOsName(desktopInfo: DesktopInfo): string {
	switch (desktopInfo.os) {
		case 'darwin':
			return 'macOS';
		case 'win32':
			return desktopInfo.osVersion ? getWindowsVersionName(desktopInfo.osVersion) : 'Windows';
		case 'linux':
			return 'Linux';
		default:
			return desktopInfo.os;
	}
}

function formatReleaseChannelLabel(value: string): string {
	const normalized = value.trim().toLowerCase();
	return normalized.charAt(0).toUpperCase() + normalized.slice(1);
}

function getBuildString(desktopInfo: DesktopInfo): string {
	const buildVersion = process.env.PUBLIC_BUILD_VERSION || process.env.BUILD_VERSION || desktopInfo.version || 'dev';
	const releaseChannel = formatReleaseChannelLabel(
		process.env.PUBLIC_RELEASE_CHANNEL || process.env.RELEASE_CHANNEL || desktopInfo.channel,
	);
	return `${releaseChannel} Desktop ${buildVersion}`;
}

function safeGetLocale(): string {
	try {
		if (!app.isReady()) return 'en-US';
		return app.getLocale() || 'en-US';
	} catch {
		return 'en-US';
	}
}

function formatDesktopClientInfo(desktopInfo: DesktopInfo, options: DesktopClientInfoOptions = {}): string {
	const osVersion = desktopInfo.systemVersion || desktopInfo.osVersion;
	const osDescription = `${[formatOsName(desktopInfo), osVersion].filter(Boolean).join(' ')} (${formatSystemArchitecture(
		desktopInfo,
	)})`;
	const parts = [
		getBuildString(desktopInfo),
		osDescription,
		`Electron ${desktopInfo.electronVersion}`,
		`Chrome ${desktopInfo.chromeVersion}`,
		`Node ${desktopInfo.nodeVersion}`,
		options.locale ? `Locale ${options.locale}` : '',
	];
	return parts.filter(Boolean).join(', ');
}

export async function getDesktopDebugInfo(
	userDataPath: string,
	options: DesktopDebugInfoOptions = {},
): Promise<DesktopDebugInfo> {
	const desktopInfo = await getDesktopInfo({nativeProbes: options.nativeProbes ?? true});
	return {
		clientInfo: formatDesktopClientInfo(desktopInfo, {locale: safeGetLocale()}),
		desktopInfo,
		appUrl: getAppUrl(),
		customAppUrl: getCustomAppUrl(),
		userDataPath,
		logsPath: safeGetLogsPath(),
		logFilePath: getLogFilePath(),
		configPath: path.join(userDataPath, 'settings.json'),
		windowBehavior: getDesktopWindowBehaviorDebugSettings(),
		troubleshooting: getDesktopTroubleshootingSettings(),
		packaged: app.isPackaged,
		portable: isPortableMode(),
		pid: process.pid,
		execPath: process.execPath,
		appImagePath: process.env.APPIMAGE || null,
		appDirPath: process.env.APPDIR || null,
		cwd: process.cwd(),
		launchArgs: sanitizeLaunchArgs(process.argv.slice(1)),
	};
}

function formatWindowBehavior(settings: DesktopWindowBehaviorSettings): string {
	return [
		`showTrayIcon=${settings.showTrayIcon}`,
		`minimizeToTray=${settings.minimizeToTray}`,
		`closeToTray=${settings.closeToTray}`,
		`useNativeTitleBar=${settings.useNativeTitleBar}`,
		`rememberWindowState=${settings.rememberWindowState}`,
		`allowTransparency=${settings.allowTransparency}`,
		`activeAllowTransparency=${settings.activeAllowTransparency}`,
		`smoothScrolling=${settings.smoothScrolling}`,
		`activeSmoothScrolling=${settings.activeSmoothScrolling}`,
		`middleClickAutoscroll=${settings.middleClickAutoscroll}`,
		`activeMiddleClickAutoscroll=${settings.activeMiddleClickAutoscroll}`,
		`firstClickPassThroughWhenUnfocused=${settings.firstClickPassThroughWhenUnfocused}`,
	].join(', ');
}

export function formatDesktopDebugInfo(info: DesktopDebugInfo): string {
	return [
		'Fluxer desktop debug info',
		info.clientInfo,
		`App URL: ${info.appUrl}`,
		`Custom app URL: ${info.customAppUrl ?? '(none)'}`,
		`User data: ${info.userDataPath}`,
		`Config: ${info.configPath}`,
		`Logs: ${info.logFilePath ?? info.logsPath ?? '(unavailable)'}`,
		`Troubleshooting: disableHardwareAcceleration=${info.troubleshooting.disableHardwareAcceleration}`,
		`Window behavior: ${formatWindowBehavior(info.windowBehavior)}`,
		`Runtime: packaged=${info.packaged}, portable=${info.portable}, pid=${info.pid}, cwd=${info.cwd}`,
		`Sandbox: flatpak=${info.desktopInfo.flatpak}, flatpakAppId=${info.desktopInfo.flatpakAppId ?? '(none)'}`,
		`Executable: ${info.execPath}`,
		...(info.appImagePath ? [`AppImage: ${info.appImagePath}`] : []),
		`Launch args: ${info.launchArgs.length > 0 ? info.launchArgs.join(' ') : '(none)'}`,
	].join('\n');
}

export function logDesktopDebugInfo(info: DesktopDebugInfo): void {
	log.info('[DebugInfo] Client info:', info.clientInfo);
	log.info('[DebugInfo] Runtime:', {
		appUrl: info.appUrl,
		customAppUrl: info.customAppUrl,
		userDataPath: info.userDataPath,
		configPath: info.configPath,
		logsPath: info.logsPath,
		logFilePath: info.logFilePath,
		troubleshooting: info.troubleshooting,
		windowBehavior: info.windowBehavior,
		packaged: info.packaged,
		portable: info.portable,
		pid: info.pid,
		execPath: info.execPath,
		flatpak: info.desktopInfo.flatpak,
		flatpakAppId: info.desktopInfo.flatpakAppId,
		appImagePath: info.appImagePath,
		appDirPath: info.appDirPath,
		cwd: info.cwd,
		launchArgs: info.launchArgs,
	});
}
