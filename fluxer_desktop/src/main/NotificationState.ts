// SPDX-License-Identifier: AGPL-3.0-or-later

import {execFile} from 'node:child_process';
import {promises as fs} from 'node:fs';
import {createRequire} from 'node:module';
import os from 'node:os';
import path from 'node:path';
import {promisify} from 'node:util';
import {MACOS_BUNDLE_ID} from '@electron/common/DesktopIdentity';
import {createChildLogger} from '@electron/common/Logger';

const logger = createChildLogger('NotificationState');
const requireModule = createRequire(import.meta.url);
const execFileAsync = promisify(execFile);

const POLICY_CACHE_TTL_MS = 1_000;
const COMMAND_TIMEOUT_MS = 1_000;
const MACOS_FOCUS_DB_DIR = path.join(os.homedir(), 'Library', 'DoNotDisturb', 'DB');
const WINDOWS_SUPPRESSING_STATES = new Set(['not-present', 'presentation-mode', 'quiet-time']);

export interface NotificationSoundPolicy {
	shouldPlaySound: boolean;
	reason: string | null;
}

interface WinShellModule {
	getUserNotificationState?: (() => string) | null;
	loadError?: Error | null;
}

let cachedPolicy: {expiresAt: number; promise: Promise<NotificationSoundPolicy>} | null = null;
let cachedWinShellModule: WinShellModule | null | undefined;

function isRecord(value: unknown): value is Record<string, unknown> {
	return value !== null && typeof value === 'object' && !Array.isArray(value);
}

function firstDataRecord(value: unknown): Record<string, unknown> | null {
	if (!isRecord(value)) return null;
	const data = value.data;
	if (!Array.isArray(data) || data.length === 0) return null;
	return isRecord(data[0]) ? data[0] : null;
}

async function readJsonFile(filePath: string): Promise<unknown | null> {
	try {
		const content = await fs.readFile(filePath, 'utf8');
		return JSON.parse(content) as unknown;
	} catch {
		return null;
	}
}

function readFocusAssertionRecords(assertions: unknown): Array<Record<string, unknown>> {
	const dataRecord = firstDataRecord(assertions);
	if (!dataRecord) return [];
	const records = dataRecord.storeAssertionRecords;
	return Array.isArray(records) ? records.filter(isRecord) : [];
}

function readFocusModeIdentifier(assertion: Record<string, unknown>): string | null {
	const details = assertion.assertionDetails;
	if (!isRecord(details)) return null;
	const modeIdentifier = details.assertionDetailsModeIdentifier;
	return typeof modeIdentifier === 'string' && modeIdentifier.length > 0 ? modeIdentifier : null;
}

function getSecureModeConfiguration(
	modeConfigurationsSecure: unknown,
	modeIdentifier: string,
): Record<string, unknown> | null {
	const dataRecord = firstDataRecord(modeConfigurationsSecure);
	if (!dataRecord) return null;
	const secureModeConfigurations = dataRecord.secureModeConfigurations;
	if (!isRecord(secureModeConfigurations)) return null;
	const modeConfiguration = secureModeConfigurations[modeIdentifier];
	if (!isRecord(modeConfiguration)) return null;
	return modeConfiguration;
}

function getModeConfigurations(modeConfigurations: unknown): Record<string, unknown> | null {
	const dataRecord = firstDataRecord(modeConfigurations);
	if (!dataRecord) return null;
	const configurations = dataRecord.modeConfigurations;
	return isRecord(configurations) ? configurations : null;
}

function readNumber(value: unknown): number | null {
	return typeof value === 'number' && Number.isFinite(value) ? value : null;
}

function isCurrentTimeInMacosFocusTrigger(trigger: Record<string, unknown>, now: Date): boolean {
	if (readNumber(trigger.enabledSetting) !== 2) return false;
	const startHour = readNumber(trigger.timePeriodStartTimeHour);
	const endHour = readNumber(trigger.timePeriodEndTimeHour);
	if (startHour === null || endHour === null) return false;
	const startMinute = readNumber(trigger.timePeriodStartTimeMinute) ?? 0;
	const endMinute = readNumber(trigger.timePeriodEndTimeMinute) ?? 0;
	const start = startHour * 60 + startMinute;
	const end = endHour * 60 + endMinute;
	const current = now.getHours() * 60 + now.getMinutes();
	if (start < end) return current >= start && current < end;
	if (end < start) return current >= start || current < end;
	return false;
}

function hasActiveMacosFocusSchedule(modeConfiguration: Record<string, unknown>, now: Date): boolean {
	const triggersContainer = modeConfiguration.triggers;
	if (!isRecord(triggersContainer)) return false;
	const triggers = triggersContainer.triggers;
	if (!Array.isArray(triggers)) return false;
	return triggers.filter(isRecord).some((trigger) => isCurrentTimeInMacosFocusTrigger(trigger, now));
}

async function getScheduledMacosFocusModeIdentifiers(): Promise<Array<string>> {
	const modeConfigurationsJson = await readJsonFile(path.join(MACOS_FOCUS_DB_DIR, 'ModeConfigurations.json'));
	const modeConfigurations = getModeConfigurations(modeConfigurationsJson);
	if (!modeConfigurations) return [];
	const now = new Date();
	const activeModes = new Set<string>();
	for (const [modeIdentifier, modeConfiguration] of Object.entries(modeConfigurations)) {
		if (isRecord(modeConfiguration) && hasActiveMacosFocusSchedule(modeConfiguration, now)) {
			activeModes.add(modeIdentifier);
		}
	}
	return [...activeModes];
}

function isAllowedMacosApplicationKey(key: string): boolean {
	return key === MACOS_BUNDLE_ID || key.startsWith(`${MACOS_BUNDLE_ID}.`) || MACOS_BUNDLE_ID.startsWith(`${key}.`);
}

async function isMacosBundleAllowedInFocus(modeIdentifier: string | null): Promise<boolean> {
	if (!modeIdentifier || modeIdentifier === 'NONE') return false;
	const secureConfigurations = await readJsonFile(path.join(MACOS_FOCUS_DB_DIR, 'ModeConfigurationsSecure.json'));
	const modeConfiguration = getSecureModeConfiguration(secureConfigurations, modeIdentifier);
	if (!modeConfiguration) return false;
	const secureConfiguration = modeConfiguration.secureConfiguration;
	if (!isRecord(secureConfiguration)) return false;
	const allowedApplications = secureConfiguration.allowedApplications;
	if (!isRecord(allowedApplications)) return false;
	return Object.keys(allowedApplications).some(isAllowedMacosApplicationKey);
}

async function getMacosNotificationSoundPolicy(): Promise<NotificationSoundPolicy> {
	const assertions = await readJsonFile(path.join(MACOS_FOCUS_DB_DIR, 'Assertions.json'));
	const activeAssertions = readFocusAssertionRecords(assertions);
	const activeFocusModes = new Set<string>();
	for (const modeIdentifier of activeAssertions.map(readFocusModeIdentifier)) {
		if (modeIdentifier) activeFocusModes.add(modeIdentifier);
	}
	for (const modeIdentifier of await getScheduledMacosFocusModeIdentifiers()) {
		activeFocusModes.add(modeIdentifier);
	}
	if (activeAssertions.length === 0 && activeFocusModes.size === 0) {
		return {shouldPlaySound: true, reason: null};
	}
	if (activeFocusModes.size === 0) {
		return {shouldPlaySound: false, reason: 'macos-focus'};
	}
	for (const focusModeIdentifier of activeFocusModes) {
		if (!(await isMacosBundleAllowedInFocus(focusModeIdentifier))) {
			return {shouldPlaySound: false, reason: 'macos-focus'};
		}
	}
	return {shouldPlaySound: true, reason: null};
}

function loadWinShellModule(): WinShellModule | null {
	if (cachedWinShellModule !== undefined) return cachedWinShellModule;
	if (process.platform !== 'win32') {
		cachedWinShellModule = null;
		return cachedWinShellModule;
	}
	try {
		const mod = requireModule('@fluxer/win-shell') as WinShellModule;
		if (mod.loadError) {
			logger.warn('@fluxer/win-shell load error; Windows notification state unavailable', {error: mod.loadError});
			cachedWinShellModule = null;
			return cachedWinShellModule;
		}
		cachedWinShellModule = mod;
		return cachedWinShellModule;
	} catch (error) {
		logger.warn('Failed to load @fluxer/win-shell; Windows notification state unavailable', {error});
		cachedWinShellModule = null;
		return cachedWinShellModule;
	}
}

function getWindowsNotificationSoundPolicy(): NotificationSoundPolicy {
	const mod = loadWinShellModule();
	const state = mod?.getUserNotificationState?.();
	if (!state || state === 'accepts-notifications' || state === 'app' || state === 'unknown') {
		return {shouldPlaySound: true, reason: null};
	}
	if (WINDOWS_SUPPRESSING_STATES.has(state)) {
		return {shouldPlaySound: false, reason: `windows-${state}`};
	}
	return {shouldPlaySound: true, reason: null};
}

async function runCommand(command: string, args: Array<string>): Promise<string | null> {
	try {
		const result = await execFileAsync(command, args, {
			encoding: 'utf8',
			timeout: COMMAND_TIMEOUT_MS,
			windowsHide: true,
		});
		return result.stdout.trim();
	} catch {
		return null;
	}
}

function parseBooleanOutput(output: string | null): boolean | null {
	if (!output) return null;
	if (/\btrue\b/i.test(output)) return true;
	if (/\bfalse\b/i.test(output)) return false;
	return null;
}

async function readFreedesktopNotificationsInhibited(): Promise<boolean | null> {
	const gdbusOutput = await runCommand('gdbus', [
		'call',
		'--session',
		'--dest',
		'org.freedesktop.Notifications',
		'--object-path',
		'/org/freedesktop/Notifications',
		'--method',
		'org.freedesktop.DBus.Properties.Get',
		'org.freedesktop.Notifications',
		'Inhibited',
	]);
	const gdbusResult = parseBooleanOutput(gdbusOutput);
	if (gdbusResult !== null) return gdbusResult;

	for (const command of ['qdbus6', 'qdbus-qt6', 'qdbus']) {
		const output = await runCommand(command, [
			'org.freedesktop.Notifications',
			'/org/freedesktop/Notifications',
			'org.freedesktop.DBus.Properties.Get',
			'org.freedesktop.Notifications',
			'Inhibited',
		]);
		const result = parseBooleanOutput(output);
		if (result !== null) return result;
	}
	return null;
}

async function readGnomeShowBanners(): Promise<boolean | null> {
	const output = await runCommand('gsettings', ['get', 'org.gnome.desktop.notifications', 'show-banners']);
	return parseBooleanOutput(output);
}

async function getLinuxNotificationSoundPolicy(): Promise<NotificationSoundPolicy> {
	const [freedesktopInhibited, gnomeShowBanners] = await Promise.all([
		readFreedesktopNotificationsInhibited(),
		readGnomeShowBanners(),
	]);
	if (freedesktopInhibited === true) {
		return {shouldPlaySound: false, reason: 'linux-notifications-inhibited'};
	}
	if (gnomeShowBanners === false) {
		return {shouldPlaySound: false, reason: 'linux-gnome-do-not-disturb'};
	}
	return {shouldPlaySound: true, reason: null};
}

async function computeNotificationSoundPolicy(): Promise<NotificationSoundPolicy> {
	if (process.platform === 'darwin') {
		return getMacosNotificationSoundPolicy();
	}
	if (process.platform === 'win32') {
		return getWindowsNotificationSoundPolicy();
	}
	if (process.platform === 'linux') {
		return getLinuxNotificationSoundPolicy();
	}
	return {shouldPlaySound: true, reason: null};
}

export async function getNotificationSoundPolicy(): Promise<NotificationSoundPolicy> {
	const now = Date.now();
	if (cachedPolicy && cachedPolicy.expiresAt > now) {
		return cachedPolicy.promise;
	}
	const promise = computeNotificationSoundPolicy().catch((error) => {
		logger.warn('Failed to query OS notification state; allowing notification sound', {error});
		return {shouldPlaySound: true, reason: null};
	});
	cachedPolicy = {expiresAt: now + POLICY_CACHE_TTL_MS, promise};
	return promise;
}

export async function shouldPlayNotificationSound(): Promise<boolean> {
	return (await getNotificationSoundPolicy()).shouldPlaySound;
}
