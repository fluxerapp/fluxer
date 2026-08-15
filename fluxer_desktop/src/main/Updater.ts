// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import {BUILD_CHANNEL} from '@electron/common/BuildChannel';
import {isPortableMode} from '@electron/common/UserDataPath';
import {destroyDesktopTray} from '@electron/main/DesktopTray';
import {isFlatpakRuntime} from '@electron/main/LinuxSandbox';
import {setQuitting} from '@electron/main/Window';
import {app, autoUpdater, type BrowserWindow, ipcMain} from 'electron';
import log from 'electron-log';
import type {UpdateInfo} from 'velopack';

type UpdaterContext = 'user' | 'background' | 'focus';
type UpdaterDownloadOption = {
	format: ManualDesktopFormat;
	label: string;
	url: string;
	suggestedName?: string;
	sha256?: string | null;
};
type UpdaterEvent =
	| {
			type: 'checking';
			context: UpdaterContext;
	  }
	| {
			type: 'available';
			context: UpdaterContext;
			version?: string | null;
			downloadSize?: number | null;
			downloadStarted: boolean;
			downloadUrl?: string;
			downloadOptions?: Array<UpdaterDownloadOption>;
	  }
	| {
			type: 'not-available';
			context: UpdaterContext;
	  }
	| {
			type: 'downloaded';
			context: UpdaterContext;
			version?: string | null;
	  }
	| {
			type: 'progress';
			context: UpdaterContext;
			percent: number;
			transferred: number;
			total: number;
			bytesPerSecond: number;
	  }
	| {
			type: 'error';
			context: UpdaterContext;
			message: string;
			phase?: 'check' | 'download' | 'install';
	  }
	| {
			type: 'unsupported';
			context: UpdaterContext;
			reason: 'platform' | 'unpackaged' | 'managed-package';
			downloadUrl?: string;
	  };

const requireModule = createRequire(import.meta.url);
type DesktopDownloadArch = 'x64' | 'arm64';

function getDesktopDownloadArch(arch: NodeJS.Architecture): DesktopDownloadArch {
	return arch === 'arm64' ? 'arm64' : 'x64';
}

const DESKTOP_DOWNLOAD_ARCH = getDesktopDownloadArch(process.arch);
const UPDATE_API_ENDPOINT = BUILD_CHANNEL === 'canary' ? 'https://api.canary.fluxer.app' : 'https://api.fluxer.app';
const UPDATE_BASE_URL = `${UPDATE_API_ENDPOINT}/dl/desktop/${BUILD_CHANNEL}/${process.platform}/${DESKTOP_DOWNLOAD_ARCH}`;
const DOWNLOAD_PAGE_URL =
	BUILD_CHANNEL === 'canary' ? 'https://canary.fluxer.app/download' : 'https://fluxer.app/download';

let lastContext: UpdaterContext = 'background';
let pendingVelopackUpdate: UpdateInfo | null = null;
let velopackCheckPromise: Promise<void> | null = null;
let velopackDownloadPromise: Promise<void> | null = null;
let velopackInstallStarted = false;

const UPDATE_DOWNLOAD_MAX_ATTEMPTS = 5;
const UPDATE_DOWNLOAD_RETRY_BASE_DELAY_MS = 3000;
const UPDATE_DOWNLOAD_RETRY_MAX_DELAY_MS = 60000;
const ELECTRON_DOWNLOAD_MAX_RETRIES = 4;

const MANUAL_DESKTOP_FORMATS = ['setup', 'dmg', 'zip', 'appimage', 'deb', 'rpm', 'tar_gz'] as const;

type ManualDesktopFormat = (typeof MANUAL_DESKTOP_FORMATS)[number];
type ManualLatestFile = {url: string; sha256: string | null};
type LinuxManualDesktopFormat = Extract<ManualDesktopFormat, 'appimage' | 'deb' | 'rpm' | 'tar_gz'>;

function send(win: BrowserWindow | null, event: UpdaterEvent) {
	win?.webContents.send('updater-event', event);
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return value !== null && typeof value === 'object' && !Array.isArray(value);
}

function getErrorMessage(error: unknown): string {
	if (error instanceof Error) {
		return error.message;
	}
	return String(error);
}

function sleep(ms: number): Promise<void> {
	return new Promise((resolve) => {
		setTimeout(resolve, ms);
	});
}

function backoffDelay(attempt: number): number {
	const exponential = UPDATE_DOWNLOAD_RETRY_BASE_DELAY_MS * 2 ** Math.max(0, attempt - 1);
	const capped = Math.min(exponential, UPDATE_DOWNLOAD_RETRY_MAX_DELAY_MS);
	return Math.round(capped * (0.5 + Math.random() * 0.5));
}

function getVelopackUpdateVersion(update: UpdateInfo): string | null {
	return update.TargetFullRelease?.Version ?? null;
}

function getVelopackUpdateSize(update: UpdateInfo): number | null {
	const raw = update.TargetFullRelease?.Size;
	if (raw == null) return null;
	if (typeof raw === 'bigint') {
		return Number(raw);
	}
	return Number(raw);
}

function createVelopackUpdateManager() {
	const {UpdateManager} = requireModule('velopack') as typeof import('velopack');
	return new UpdateManager(UPDATE_BASE_URL);
}

async function checkVelopackForUpdates(
	context: UpdaterContext,
	getMainWindow: () => BrowserWindow | null,
): Promise<void> {
	if (velopackCheckPromise) {
		return velopackCheckPromise;
	}
	velopackCheckPromise = (async () => {
		try {
			send(getMainWindow(), {type: 'checking', context});
			const updateManager = createVelopackUpdateManager();
			const pendingUpdate = updateManager.getUpdatePendingRestart();
			const update = await updateManager.checkForUpdatesAsync();
			if (!update) {
				if (pendingUpdate) {
					pendingVelopackUpdate = pendingUpdate;
					send(getMainWindow(), {
						type: 'downloaded',
						context,
						version: getVelopackUpdateVersion(pendingUpdate),
					});
					return;
				}
				pendingVelopackUpdate = null;
				send(getMainWindow(), {type: 'not-available', context});
				return;
			}
			if (pendingUpdate) {
				const pendingVersion = getVelopackUpdateVersion(pendingUpdate);
				const updateVersion = getVelopackUpdateVersion(update);
				if (pendingVersion && updateVersion && compareVersions(updateVersion, pendingVersion) <= 0) {
					pendingVelopackUpdate = pendingUpdate;
					send(getMainWindow(), {
						type: 'downloaded',
						context,
						version: pendingVersion,
					});
					return;
				}
				log.info('Newer Velopack update found while another update is pending restart.', {
					pendingVersion,
					updateVersion,
				});
			}
			pendingVelopackUpdate = update;
			send(getMainWindow(), {
				type: 'available',
				context,
				version: getVelopackUpdateVersion(update),
				downloadSize: getVelopackUpdateSize(update),
				downloadStarted: false,
			});
		} catch (error) {
			send(getMainWindow(), {type: 'error', context, phase: 'check', message: getErrorMessage(error)});
		}
	})().finally(() => {
		velopackCheckPromise = null;
	});
	return velopackCheckPromise;
}

async function downloadVelopackUpdate(
	context: UpdaterContext,
	getMainWindow: () => BrowserWindow | null,
): Promise<void> {
	if (velopackDownloadPromise) {
		return velopackDownloadPromise;
	}
	const update = pendingVelopackUpdate;
	if (!update) {
		send(getMainWindow(), {
			type: 'error',
			context,
			phase: 'download',
			message: 'No update available to download. Please check for updates first.',
		});
		return;
	}
	velopackDownloadPromise = (async () => {
		const total = getVelopackUpdateSize(update) ?? 0;
		let lastError: unknown;
		for (let attempt = 1; attempt <= UPDATE_DOWNLOAD_MAX_ATTEMPTS; attempt += 1) {
			let lastSampleAt = Date.now();
			let lastSampleTransferred = 0;
			let smoothedBytesPerSecond = 0;
			try {
				const updateManager = createVelopackUpdateManager();
				await updateManager.downloadUpdateAsync(update, (percent) => {
					const transferred = total > 0 ? Math.round((total * percent) / 100) : 0;
					const now = Date.now();
					const dtMs = now - lastSampleAt;
					if (dtMs >= 500 || percent >= 100) {
						if (dtMs > 0 && transferred >= lastSampleTransferred) {
							const instant = ((transferred - lastSampleTransferred) * 1000) / dtMs;
							smoothedBytesPerSecond =
								smoothedBytesPerSecond === 0 ? instant : smoothedBytesPerSecond * 0.7 + instant * 0.3;
						}
						lastSampleAt = now;
						lastSampleTransferred = transferred;
						send(getMainWindow(), {
							type: 'progress',
							context,
							percent,
							transferred,
							total,
							bytesPerSecond: Math.round(smoothedBytesPerSecond),
						});
					}
				});
				send(getMainWindow(), {
					type: 'downloaded',
					context,
					version: getVelopackUpdateVersion(update),
				});
				return;
			} catch (error) {
				lastError = error;
				if (attempt >= UPDATE_DOWNLOAD_MAX_ATTEMPTS) {
					break;
				}
				const delay = backoffDelay(attempt);
				const reason = getErrorMessage(error);
				const waitSeconds = Math.round(delay / 1000);
				log.warn(
					`Velopack update download attempt ${attempt}/${UPDATE_DOWNLOAD_MAX_ATTEMPTS} failed (${reason}); retrying in ${waitSeconds}s`,
				);
				await sleep(delay);
			}
		}
		log.error('Velopack update download failed after retries', lastError);
		send(getMainWindow(), {type: 'error', context, phase: 'download', message: getErrorMessage(lastError)});
	})().finally(() => {
		velopackDownloadPromise = null;
	});
	return velopackDownloadPromise;
}

function installVelopackUpdate(): void {
	if (velopackInstallStarted) {
		log.warn('Velopack install already in progress; ignoring duplicate request.');
		return;
	}
	const updateManager = createVelopackUpdateManager();
	const update = pendingVelopackUpdate ?? updateManager.getUpdatePendingRestart();
	if (!update) {
		throw new Error('No Velopack update is ready to install.');
	}
	velopackInstallStarted = true;
	setQuitting(true);
	destroyDesktopTray();
	updateManager.waitExitThenApplyUpdate(update);
	setImmediate(() => app.exit(0));
}

function registerVelopackUpdater(getMainWindow: () => BrowserWindow | null): void {
	ipcMain.handle('updater-check', async (_e, context: UpdaterContext) => {
		lastContext = context;
		await checkVelopackForUpdates(context, getMainWindow);
	});
	ipcMain.handle('updater-download', async (_e, context: UpdaterContext) => {
		lastContext = context;
		await downloadVelopackUpdate(context, getMainWindow);
	});
	ipcMain.handle('updater-install', async () => {
		installVelopackUpdate();
	});
}

function registerElectronUpdater(getMainWindow: () => BrowserWindow | null): void {
	let electronUpdateDownloading = false;
	let electronDownloadRetries = 0;
	const {UpdateSourceType, updateElectronApp} = requireModule(
		'update-electron-app',
	) as typeof import('update-electron-app');
	updateElectronApp({
		updateSource: {
			type: UpdateSourceType.StaticStorage,
			baseUrl: UPDATE_BASE_URL,
		},
		updateInterval: '12 hours',
		logger: log,
		notifyUser: false,
	});
	autoUpdater.on('checking-for-update', () => {
		send(getMainWindow(), {type: 'checking', context: lastContext});
	});
	autoUpdater.on('update-available', () => {
		electronUpdateDownloading = true;
		send(getMainWindow(), {
			type: 'available',
			context: lastContext,
			version: null,
			downloadSize: null,
			downloadStarted: true,
		});
	});
	autoUpdater.on('update-not-available', () => {
		send(getMainWindow(), {type: 'not-available', context: lastContext});
	});
	autoUpdater.on('update-downloaded', (_event, _releaseNotes, releaseName) => {
		send(getMainWindow(), {type: 'downloaded', context: lastContext, version: releaseName ?? null});
	});
	autoUpdater.on('error', (err: Error) => {
		const message = err?.message ?? String(err);
		const phase: 'check' | 'download' = electronUpdateDownloading ? 'download' : 'check';
		if (electronUpdateDownloading && electronDownloadRetries < ELECTRON_DOWNLOAD_MAX_RETRIES) {
			electronDownloadRetries += 1;
			electronUpdateDownloading = false;
			const delay = backoffDelay(electronDownloadRetries);
			const waitSeconds = Math.round(delay / 1000);
			log.warn(
				`Update download failed (attempt ${electronDownloadRetries}/${ELECTRON_DOWNLOAD_MAX_RETRIES}); retrying in ${waitSeconds}s: ${message}`,
			);
			setTimeout(() => {
				try {
					autoUpdater.checkForUpdates();
				} catch (retryError) {
					log.warn('Update retry check failed', retryError);
				}
			}, delay);
			return;
		}
		electronUpdateDownloading = false;
		send(getMainWindow(), {type: 'error', context: lastContext, phase, message});
	});
	ipcMain.handle('updater-check', async (_e, context: UpdaterContext) => {
		lastContext = context;
		try {
			autoUpdater.checkForUpdates();
		} catch (error) {
			send(getMainWindow(), {type: 'error', context, phase: 'check', message: getErrorMessage(error)});
		}
	});
	ipcMain.handle('updater-download', async () => {});
	ipcMain.handle('updater-install', async () => {
		setQuitting(true);
		autoUpdater.quitAndInstall();
	});
}

type ManualLatestInfo = {
	version: string;
	pubDate: string | null;
	files: Partial<Record<ManualDesktopFormat, ManualLatestFile>>;
};

let manualLatestCache: {at: number; info: ManualLatestInfo} | null = null;

const MANUAL_CACHE_TTL_MS = 5 * 60 * 1000;

function parseSemverTuple(input: string): [number, number, number, string] {
	const trimmed = input.trim().replace(/^v/, '');
	const [core, ...preParts] = trimmed.split('-');
	const pre = preParts.join('-');
	const segments = core.split('.').map((part) => Number.parseInt(part, 10));
	const [major = 0, minor = 0, patch = 0] = segments;
	return [
		Number.isFinite(major) ? major : 0,
		Number.isFinite(minor) ? minor : 0,
		Number.isFinite(patch) ? patch : 0,
		pre,
	];
}

function compareVersions(a: string, b: string): number {
	const [aMaj, aMin, aPat, aPre] = parseSemverTuple(a);
	const [bMaj, bMin, bPat, bPre] = parseSemverTuple(b);
	if (aMaj !== bMaj) return aMaj < bMaj ? -1 : 1;
	if (aMin !== bMin) return aMin < bMin ? -1 : 1;
	if (aPat !== bPat) return aPat < bPat ? -1 : 1;
	if (aPre === bPre) return 0;
	if (!aPre) return 1;
	if (!bPre) return -1;
	return aPre < bPre ? -1 : 1;
}

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

function getManualDownloadFormatPreference(): Array<ManualDesktopFormat> {
	if (process.platform === 'linux') {
		return ['appimage', 'deb', 'rpm', 'tar_gz'];
	}
	if (process.platform === 'darwin') {
		return ['dmg', 'zip'];
	}
	if (process.platform === 'win32') {
		return ['setup'];
	}
	return [];
}

const LINUX_MANUAL_FORMAT_LABELS: Record<LinuxManualDesktopFormat, string> = {
	appimage: 'AppImage',
	deb: 'DEB package',
	rpm: 'RPM package',
	tar_gz: 'tar.gz archive',
};

const LINUX_MANUAL_FORMAT_EXTENSIONS: Record<LinuxManualDesktopFormat, string> = {
	appimage: '.AppImage',
	deb: '.deb',
	rpm: '.rpm',
	tar_gz: '.tar.gz',
};

const LINUX_MANUAL_ARCH_TOKENS: Record<LinuxManualDesktopFormat, Record<DesktopDownloadArch, string>> = {
	appimage: {x64: 'x86_64', arm64: 'arm64'},
	deb: {x64: 'amd64', arm64: 'arm64'},
	rpm: {x64: 'x86_64', arm64: 'aarch64'},
	tar_gz: {x64: 'x64', arm64: 'arm64'},
};

function isLinuxManualDesktopFormat(format: ManualDesktopFormat): format is LinuxManualDesktopFormat {
	return format === 'appimage' || format === 'deb' || format === 'rpm' || format === 'tar_gz';
}

function buildManualLatestDownloadUrl(format: ManualDesktopFormat): string {
	return `${UPDATE_BASE_URL}/latest/${format}`;
}

function getModernProductName(): string {
	return BUILD_CHANNEL === 'canary' ? 'Fluxer Canary' : 'Fluxer';
}

function getManualUpdateSuggestedName(format: LinuxManualDesktopFormat, version: string): string {
	const archToken = LINUX_MANUAL_ARCH_TOKENS[format][DESKTOP_DOWNLOAD_ARCH];
	const extension = LINUX_MANUAL_FORMAT_EXTENSIONS[format];
	return `${getModernProductName()}-${version}-linux-${archToken}${extension}`;
}

function getManualDownloadOptions(info: ManualLatestInfo): Array<UpdaterDownloadOption> {
	if (process.platform !== 'linux') {
		return [];
	}
	return getManualDownloadFormatPreference()
		.filter(isLinuxManualDesktopFormat)
		.map((format) => {
			const file = info.files[format];
			return {
				format,
				label: LINUX_MANUAL_FORMAT_LABELS[format],
				url: buildManualLatestDownloadUrl(format),
				suggestedName: getManualUpdateSuggestedName(format, info.version),
				sha256: file?.sha256 ?? null,
			};
		});
}

function getManualDownloadUrl(info: ManualLatestInfo): string {
	const [preferredOption] = getManualDownloadOptions(info);
	if (preferredOption) {
		return preferredOption.url;
	}
	for (const format of getManualDownloadFormatPreference()) {
		const url = info.files[format]?.url;
		if (url) {
			return url;
		}
	}
	return DOWNLOAD_PAGE_URL;
}

async function fetchManualLatest(options: {forceRefresh?: boolean} = {}): Promise<ManualLatestInfo> {
	const now = Date.now();
	if (!options.forceRefresh && manualLatestCache && now - manualLatestCache.at < MANUAL_CACHE_TTL_MS) {
		return manualLatestCache.info;
	}
	const response = await fetch(`${UPDATE_BASE_URL}/latest`, {
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
	const info: ManualLatestInfo = {
		version: payload.version,
		pubDate: typeof payload.pub_date === 'string' ? payload.pub_date : null,
		files: parseManualLatestFiles(payload.files),
	};
	manualLatestCache = {at: now, info};
	return info;
}

async function checkManualUpdate(context: UpdaterContext, getMainWindow: () => BrowserWindow | null): Promise<void> {
	send(getMainWindow(), {type: 'checking', context});
	try {
		const latest = await fetchManualLatest({forceRefresh: context === 'user'});
		const current = app.getVersion();
		if (compareVersions(latest.version, current) > 0) {
			const downloadOptions = getManualDownloadOptions(latest);
			send(getMainWindow(), {
				type: 'available',
				context,
				version: latest.version,
				downloadSize: null,
				downloadStarted: false,
				downloadUrl: getManualDownloadUrl(latest),
				...(downloadOptions.length > 0 ? {downloadOptions} : {}),
			});
		} else {
			send(getMainWindow(), {type: 'not-available', context});
		}
	} catch (error) {
		log.warn('Manual update check failed', error);
		send(getMainWindow(), {type: 'error', context, phase: 'check', message: getErrorMessage(error)});
	}
}

function registerManualUpdater(
	getMainWindow: () => BrowserWindow | null,
	reason: 'platform' | 'unpackaged' | 'managed-package',
): void {
	ipcMain.handle('updater-check', async (_e, context: UpdaterContext) => {
		if (reason !== 'platform') {
			send(getMainWindow(), {
				type: 'unsupported',
				context,
				reason,
				...(reason === 'unpackaged' ? {downloadUrl: DOWNLOAD_PAGE_URL} : {}),
			});
			return;
		}
		await checkManualUpdate(context, getMainWindow);
	});
	ipcMain.handle('updater-download', async (_e, context: UpdaterContext) => {
		send(getMainWindow(), {
			type: 'unsupported',
			context,
			reason,
			...(reason === 'managed-package' ? {} : {downloadUrl: DOWNLOAD_PAGE_URL}),
		});
	});
	ipcMain.handle('updater-install', async () => {
		throw new Error('In-app updates are not supported on this platform.');
	});
}

export function registerUpdater(getMainWindow: () => BrowserWindow | null) {
	if (!app.isPackaged) {
		registerManualUpdater(getMainWindow, 'unpackaged');
		return;
	}
	if (isPortableMode()) {
		registerManualUpdater(getMainWindow, 'platform');
		return;
	}
	if (isFlatpakRuntime()) {
		registerManualUpdater(getMainWindow, 'managed-package');
		return;
	}
	if (process.platform === 'win32') {
		registerVelopackUpdater(getMainWindow);
		return;
	}
	if (process.platform === 'darwin') {
		registerElectronUpdater(getMainWindow);
		return;
	}
	registerManualUpdater(getMainWindow, 'platform');
}
