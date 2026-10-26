import {isRecord} from '@fluxer/record_utils/src/RecordGuards';
// SPDX-License-Identifier: AGPL-3.0-or-later

import {errorMessage as getErrorMessage} from '@electron/common/ErrorMessage';
import {checkDesktopUpdateNow} from '@electron/main/DesktopUpdateGate';
import {getFlatpakAppId} from '@electron/main/LinuxSandbox';
import {compareShellVersions} from '@electron/main/ModuleVersion';
import {MANUAL_DESKTOP_FORMATS, type ManualDesktopFormat} from '@electron/main/ShellDownloadFormats';
import {resolveShellUpdatePlan, ShellUpdateCapability} from '@electron/main/ShellUpdateCapability';
import {
	DOWNLOAD_PAGE_URL,
	getManualDownloadUrl,
	type ManualLatestFile,
	type ManualLatestInfo,
	UPDATE_BASE_URL,
} from '@electron/main/UpdaterDownloads';
import {app, type BrowserWindow, ipcMain, net} from 'electron';
import log from 'electron-log';

type UpdaterContext = 'user' | 'background' | 'focus';
type UpdaterEvent =
	| {
			type: 'checking';
			context: UpdaterContext;
	  }
	| {
			type: 'available';
			context: UpdaterContext;
			version?: string | null;
			downloadUrl?: string;
	  }
	| {
			type: 'not-available';
			context: UpdaterContext;
	  }
	| {
			type: 'error';
			context: UpdaterContext;
			message: string;
	  }
	| {
			type: 'unsupported';
			context: UpdaterContext;
			reason: 'platform' | 'unpackaged';
			downloadUrl?: string;
	  };

function send(win: BrowserWindow | null, event: UpdaterEvent) {
	win?.webContents.send('updater-event', event);
}

let manualLatestCache: {at: number; info: ManualLatestInfo} | null = null;

const MANUAL_CACHE_TTL_MS = 5 * 60 * 1000;
const FLATHUB_APPSTREAM_URL = 'https://flathub.org/api/v2/appstream';

function parseManualLatestFiles(value: unknown): Partial<Record<ManualDesktopFormat, ManualLatestFile>> {
	if (!isRecord(value)) {
		return {};
	}
	const files: Partial<Record<ManualDesktopFormat, ManualLatestFile>> = {};
	for (const format of MANUAL_DESKTOP_FORMATS) {
		const entry = value[format];
		if (!isRecord(entry) || typeof entry.url !== 'string' || entry.url.trim().length === 0) {
			continue;
		}
		files[format] = {
			url: entry.url,
			sha256: typeof entry.sha256 === 'string' ? entry.sha256 : null,
		};
	}
	return files;
}

async function fetchFluxerLatest(): Promise<ManualLatestInfo> {
	const response = await net.fetch(`${UPDATE_BASE_URL}/latest`, {
		cache: 'no-store',
		headers: {
			Accept: 'application/json',
			'Cache-Control': 'no-cache',
			Pragma: 'no-cache',
		},
	});
	if (!response.ok) {
		throw new Error(`Latest version request failed: ${response.status}`);
	}
	const payload = (await response.json()) as {version?: unknown; pub_date?: unknown; files?: unknown};
	if (typeof payload.version !== 'string' || payload.version.length === 0) {
		throw new Error('Latest version response missing version string');
	}
	return {
		version: payload.version,
		pubDate: typeof payload.pub_date === 'string' ? payload.pub_date : null,
		files: parseManualLatestFiles(payload.files),
	};
}

function newestFlathubReleaseVersion(payload: unknown): string | null {
	if (!isRecord(payload) || !Array.isArray(payload.releases)) {
		return null;
	}
	let newest: string | null = null;
	for (const release of payload.releases) {
		if (!isRecord(release) || typeof release.version !== 'string') {
			continue;
		}
		if (newest == null || compareShellVersions(release.version, newest) > 0) {
			newest = release.version;
		}
	}
	return newest;
}

async function fetchFlathubLatest(): Promise<ManualLatestInfo> {
	const appId = getFlatpakAppId();
	if (appId == null) {
		throw new Error('The Flatpak app id is unknown, so Flathub cannot be asked for a release');
	}
	const response = await net.fetch(`${FLATHUB_APPSTREAM_URL}/${encodeURIComponent(appId)}?locale=en`, {
		cache: 'no-store',
		headers: {Accept: 'application/json'},
	});
	if (!response.ok) {
		throw new Error(`Flathub release request failed: ${response.status}`);
	}
	const version = newestFlathubReleaseVersion(await response.json());
	if (version == null) {
		throw new Error('Flathub lists no release version');
	}
	return {version, pubDate: null, files: {}};
}

async function fetchManualLatest(options: {forceRefresh?: boolean} = {}): Promise<ManualLatestInfo> {
	const now = Date.now();
	if (!options.forceRefresh && manualLatestCache && now - manualLatestCache.at < MANUAL_CACHE_TTL_MS) {
		return manualLatestCache.info;
	}
	const plan = resolveShellUpdatePlan();
	const fromFlathub = plan.capability === ShellUpdateCapability.MANAGED_PACKAGE && plan.releaseFeed === 'flathub';
	const info = fromFlathub ? await fetchFlathubLatest() : await fetchFluxerLatest();
	manualLatestCache = {at: now, info};
	return info;
}

export async function findNewerManualShell(): Promise<ManualLatestInfo | null> {
	const plan = resolveShellUpdatePlan();
	const managed = plan.capability === ShellUpdateCapability.MANAGED_PACKAGE;
	const manual = plan.capability === ShellUpdateCapability.MANUAL_DOWNLOAD && plan.reason === 'platform';
	if (!managed && !manual) {
		return null;
	}
	const latest = await fetchManualLatest({forceRefresh: true});
	return compareShellVersions(latest.version, app.getVersion()) > 0 ? latest : null;
}

function sendManualUpdateAvailable(
	getMainWindow: () => BrowserWindow | null,
	context: UpdaterContext,
	latest: ManualLatestInfo,
): void {
	send(getMainWindow(), {
		type: 'available',
		context,
		version: latest.version,
		downloadUrl: getManualDownloadUrl(latest),
	});
}

async function checkManualUpdate(context: UpdaterContext, getMainWindow: () => BrowserWindow | null): Promise<void> {
	send(getMainWindow(), {type: 'checking', context});
	try {
		const latest = await fetchManualLatest({forceRefresh: context === 'user'});
		const current = app.getVersion();
		if (compareShellVersions(latest.version, current) > 0) {
			sendManualUpdateAvailable(getMainWindow, context, latest);
		} else {
			send(getMainWindow(), {type: 'not-available', context});
		}
	} catch (error) {
		log.warn('Manual update check failed', error);
		send(getMainWindow(), {type: 'error', context, message: getErrorMessage(error)});
	}
}

function registerSelfUpdater(getMainWindow: () => BrowserWindow | null): void {
	ipcMain.handle('updater-check', async (_e, context: UpdaterContext) => {
		if (context !== 'user') {
			return;
		}
		try {
			await checkDesktopUpdateNow({manual: true});
			send(getMainWindow(), {type: 'not-available', context});
		} catch (error) {
			log.warn('Desktop update check failed', error);
			send(getMainWindow(), {type: 'error', context, message: getErrorMessage(error)});
		}
	});
}

function registerManualUpdater(getMainWindow: () => BrowserWindow | null, unpackaged: boolean): void {
	ipcMain.handle('updater-check', async (_e, context: UpdaterContext) => {
		if (context === 'user') {
			try {
				await checkDesktopUpdateNow({manual: true});
			} catch (error) {
				log.warn('Desktop module update check failed', error);
			}
		}
		if (unpackaged) {
			send(getMainWindow(), {type: 'unsupported', context, reason: 'unpackaged', downloadUrl: DOWNLOAD_PAGE_URL});
			return;
		}
		await checkManualUpdate(context, getMainWindow);
	});
}

export function registerUpdater(getMainWindow: () => BrowserWindow | null) {
	const plan = resolveShellUpdatePlan();
	if (plan.capability === ShellUpdateCapability.SELF_UPDATE) {
		registerSelfUpdater(getMainWindow);
		return;
	}
	registerManualUpdater(
		getMainWindow,
		plan.capability === ShellUpdateCapability.MANUAL_DOWNLOAD && plan.reason === 'unpackaged',
	);
}
