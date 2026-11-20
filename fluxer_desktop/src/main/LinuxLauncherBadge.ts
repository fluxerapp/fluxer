// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import {createRequire} from 'node:module';
import path from 'node:path';
import {
	LEGACY_LINUX_DESKTOP_ENTRY_ID,
	LINUX_DESKTOP_ENTRY_ID,
	OTHER_CHANNEL_LINUX_DESKTOP_FILES,
} from '@electron/common/DesktopIdentity';
import {createChildLogger} from '@electron/common/Logger';
import {isWindowDesktopEntry} from '@electron/main/DesktopEntryText';
import {getLinuxPortalsMode} from '@electron/main/LaunchOptions';
import {getLinuxApplicationsDirs} from '@electron/main/LinuxDesktopEntry';
import {getStableLinuxLaunchPath} from '@electron/main/LinuxLaunchPath';
import {loadOptionalNativeAddon} from '@electron/main/NativeAddon';
import {app} from 'electron';

const logger = createChildLogger('LinuxLauncherBadge');
const requireModule = createRequire(import.meta.url);
const SETTLE_REFRESH_MS = 30_000;
const PERIODIC_REFRESH_MS = 10 * 60_000;

interface LauncherEntryBinding {
	update(uri: string, count: number, countVisible: boolean): void;
}

interface LinuxPortalsBinding {
	LauncherEntry: (new (onShellStarted: () => void) => LauncherEntryBinding) | null;
	loadError?: Error | null;
}

const loadLinuxPortals = loadOptionalNativeAddon<LinuxPortalsBinding>({
	platform: 'linux',
	load: () => requireModule('@fluxer/linux-portals'),
	logger,
	level: 'warn',
	unavailableMessage: 'Failed to load @fluxer/linux-portals, the launcher badge only targets the primary desktop id',
	disabled: () => getLinuxPortalsMode(process.argv) === 'off',
});

let launcherEntry: LauncherEntryBinding | null | undefined;
let badgeCount = 0;
let windowDesktopIds: ReadonlyArray<string> = [];
let started = false;

function findWindowDesktopIds(): Array<string> {
	const identity = {
		wmClasses: [LINUX_DESKTOP_ENTRY_ID, LEGACY_LINUX_DESKTOP_ENTRY_ID],
		launchPath: getStableLinuxLaunchPath(),
	};
	const primaryDesktopId = process.env.CHROME_DESKTOP;
	const seen = new Set<string>();
	const ids: Array<string> = [];
	for (const dir of getLinuxApplicationsDirs()) {
		let entries: Array<string>;
		try {
			entries = fs.readdirSync(dir);
		} catch {
			continue;
		}
		for (const entry of entries) {
			if (!entry.endsWith('.desktop') || seen.has(entry)) continue;
			seen.add(entry);
			if (entry === primaryDesktopId || OTHER_CHANNEL_LINUX_DESKTOP_FILES.has(entry)) continue;
			let contents: string;
			try {
				contents = fs.readFileSync(path.join(dir, entry), 'utf8');
			} catch {
				continue;
			}
			if (isWindowDesktopEntry(contents, identity)) ids.push(entry);
		}
	}
	return ids;
}

function getLauncherEntry(): LauncherEntryBinding | null {
	if (launcherEntry !== undefined) return launcherEntry;
	const LauncherEntry = loadLinuxPortals()?.LauncherEntry ?? null;
	try {
		launcherEntry = LauncherEntry ? new LauncherEntry(emitBadge) : null;
	} catch (error) {
		logger.warn('Failed to open the launcher entry emitter', {error});
		launcherEntry = null;
	}
	return launcherEntry;
}

function sendToDesktopIds(desktopIds: ReadonlyArray<string>, count: number): void {
	if (desktopIds.length === 0) return;
	const entry = getLauncherEntry();
	if (!entry) return;
	for (const desktopId of desktopIds) {
		try {
			entry.update(`application://${desktopId}`, count, count !== 0);
		} catch (error) {
			logger.warn('Failed to send the launcher badge', {desktopId, error});
		}
	}
}

function emitBadge(): void {
	app.setBadgeCount(badgeCount);
	sendToDesktopIds(windowDesktopIds, badgeCount);
}

function refreshWindowDesktopIds(): void {
	const next = findWindowDesktopIds();
	const added = next.filter((id) => !windowDesktopIds.includes(id));
	if (added.length > 0) {
		logger.warn('Other desktop entries claim the Fluxer window, sending the launcher badge to them as well', {
			desktopIds: added,
		});
	}
	sendToDesktopIds(
		windowDesktopIds.filter((id) => !next.includes(id)),
		0,
	);
	windowDesktopIds = next;
	emitBadge();
}

export function setLinuxLauncherBadge(count: number): void {
	badgeCount = count;
	emitBadge();
}

export function reemitLinuxLauncherBadge(): void {
	emitBadge();
}

export function startLinuxLauncherBadge(): void {
	if (process.platform !== 'linux' || started) return;
	started = true;
	refreshWindowDesktopIds();
	getLauncherEntry();
	setTimeout(refreshWindowDesktopIds, SETTLE_REFRESH_MS).unref();
	setInterval(refreshWindowDesktopIds, PERIODIC_REFRESH_MS).unref();
	app.on('will-quit', () => sendToDesktopIds(windowDesktopIds, 0));
}
