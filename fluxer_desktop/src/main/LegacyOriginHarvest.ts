// SPDX-License-Identifier: AGPL-3.0-or-later

import {BUILD_CHANNEL} from '@electron/common/BuildChannel';
import {getLegacyAppOrigin} from '@electron/common/DesktopConfig';
import {createChildLogger} from '@electron/common/Logger';
import {type DesktopAppStorage, getDesktopAppStorage} from '@electron/main/DesktopAppStorage';
import {getDesktopDistributionPath} from '@electron/main/DesktopDistributionPath';
import {seedDesktopLastRouteFromLegacyStorage} from '@electron/main/DesktopLastRoute';
import {rekeyHarvestToOfficialInstance} from '@electron/main/LegacyHarvestOfficialInstance';
import {
	parseStagedLegacyHarvest,
	type StagedLegacyHarvest,
	writeStagedHarvest,
} from '@electron/main/LegacyOriginHarvestStore';
import {
	DESKTOP_LEGACY_HARVEST_CHANNELS,
	DESKTOP_LEGACY_HARVEST_FAILURE_MARKER_KEY,
	DESKTOP_LEGACY_HARVEST_MARKER_KEY,
	DESKTOP_LEGACY_HARVEST_MAX_FAILURES,
	DESKTOP_LEGACY_HARVEST_SENTINEL_PATH,
	DesktopLegacyHarvestStatus,
} from '@fluxer/desktop_ipc/src/LegacyHarvestContract';
import {app, BrowserWindow, ipcMain, net, session} from 'electron';

const LEGACY_HARVEST_TIMEOUT_MS = 60_000;
const LEGACY_HARVEST_MARKER_VERSION = 1;
const SENTINEL_DOCUMENT = '<!doctype html><meta charset="utf-8"><title>.</title>';
const SENTINEL_HEADERS: Readonly<Record<string, string>> = {
	'content-type': 'text/html; charset=utf-8',
	'cache-control': 'no-store',
};

const logger = createChildLogger('LegacyHarvest');

class LegacyOriginHarvestUnavailableError extends Error {
	public constructor(reason: string) {
		super(`Legacy origin harvest is unavailable: ${reason}`);
		this.name = 'LegacyOriginHarvestUnavailableError';
	}
}

class LegacyOriginHarvestAbortedError extends Error {
	public constructor() {
		super('Legacy origin harvest was aborted');
		this.name = 'LegacyOriginHarvestAbortedError';
	}
}

class LegacyOriginHarvestTimeoutError extends Error {
	public constructor(timeoutMs: number) {
		super(`Legacy origin harvest timed out after ${timeoutMs}ms`);
		this.name = 'LegacyOriginHarvestTimeoutError';
	}
}

class LegacyOriginHarvestFailedError extends Error {
	public constructor(reason: string) {
		super(`Legacy origin harvest failed in the renderer: ${reason}`);
		this.name = 'LegacyOriginHarvestFailedError';
	}
}

interface LegacyHarvestResult {
	readonly harvest: StagedLegacyHarvest;
	readonly blobs: Map<string, Uint8Array>;
}

interface LegacyHarvestMarker {
	readonly status: DesktopLegacyHarvestStatus;
	readonly capturedAt: number;
	readonly origin: string;
}

function isPlainObject(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function describeError(error: unknown): string {
	return error instanceof Error ? error.message : String(error);
}

function parseHarvestMarker(value: string | null): LegacyHarvestMarker | null {
	if (value === null) return null;
	let parsed: unknown;
	try {
		parsed = JSON.parse(value);
	} catch {
		return null;
	}
	if (!isPlainObject(parsed)) return null;
	const {capturedAt, origin, status} = parsed;
	if (typeof status !== 'string' || typeof origin !== 'string') return null;
	if (typeof capturedAt !== 'number' || !Number.isFinite(capturedAt)) return null;
	const known = Object.values(DesktopLegacyHarvestStatus).find((candidate) => candidate === status);
	if (known === undefined) return null;
	return {status: known, capturedAt, origin};
}

function parseFailureCount(value: string | null): number {
	if (value === null) return 0;
	const parsed = Number.parseInt(value, 10);
	return Number.isFinite(parsed) && parsed > 0 ? parsed : 0;
}

function writeHarvestMarker(
	storage: DesktopAppStorage,
	status: DesktopLegacyHarvestStatus,
	origin: string,
): Promise<void> {
	return storage.setMarker(
		DESKTOP_LEGACY_HARVEST_MARKER_KEY,
		JSON.stringify({version: LEGACY_HARVEST_MARKER_VERSION, status, capturedAt: Date.now(), origin}),
	);
}

function parseHarvestBlobs(value: unknown): Map<string, Uint8Array> {
	const blobs = new Map<string, Uint8Array>();
	if (!Array.isArray(value)) {
		throw new LegacyOriginHarvestFailedError('the submitted blob list is not an array');
	}
	for (const entry of value) {
		if (!isPlainObject(entry)) {
			throw new LegacyOriginHarvestFailedError('a submitted blob entry is not an object');
		}
		const {blobId, bytes} = entry;
		if (typeof blobId !== 'string' || !(bytes instanceof Uint8Array)) {
			throw new LegacyOriginHarvestFailedError('a submitted blob entry is malformed');
		}
		blobs.set(blobId, bytes);
	}
	return blobs;
}

function parseHarvestSubmission(origin: string, submission: unknown): LegacyHarvestResult {
	if (!isPlainObject(submission)) {
		throw new LegacyOriginHarvestFailedError('the submission is not an object');
	}
	if (submission.ok !== true) {
		const reason = typeof submission.error === 'string' ? submission.error : 'unknown reason';
		throw new LegacyOriginHarvestFailedError(reason);
	}
	const payload = submission.payload;
	if (!isPlainObject(payload)) {
		throw new LegacyOriginHarvestFailedError('the submitted payload is not an object');
	}
	const harvest = parseStagedLegacyHarvest({
		version: LEGACY_HARVEST_MARKER_VERSION,
		origin,
		capturedAt: Date.now(),
		localStorage: payload.localStorage,
		stores: payload.stores,
		mediaDevices: payload.mediaDevices,
		serviceWorkersUnregistered: payload.serviceWorkersUnregistered,
		cachesDeleted: payload.cachesDeleted,
		truncated: payload.truncated,
	});
	if (harvest === null) {
		throw new LegacyOriginHarvestFailedError('the submitted payload did not match the harvest shape');
	}
	return {harvest, blobs: parseHarvestBlobs(payload.blobs)};
}

function createHarvestWindow(): BrowserWindow {
	return new BrowserWindow({
		show: false,
		width: 640,
		height: 480,
		webPreferences: {
			preload: getDesktopDistributionPath('preload', 'legacy-harvest.cjs'),
			contextIsolation: true,
			nodeIntegration: false,
			sandbox: true,
			webSecurity: true,
			spellcheck: false,
			backgroundThrottling: false,
		},
	});
}

function awaitHarvestSubmission(
	harvestWindow: BrowserWindow,
	origin: string,
	sentinelUrl: string,
	signal: AbortSignal,
): Promise<LegacyHarvestResult> {
	return new Promise<LegacyHarvestResult>((resolve, reject) => {
		let settled = false;
		const finish = (action: () => void): void => {
			if (settled) return;
			settled = true;
			clearTimeout(timer);
			signal.removeEventListener('abort', onAbort);
			action();
		};
		const onAbort = (): void => finish(() => reject(new LegacyOriginHarvestAbortedError()));
		const timer = setTimeout(
			() => finish(() => reject(new LegacyOriginHarvestTimeoutError(LEGACY_HARVEST_TIMEOUT_MS))),
			LEGACY_HARVEST_TIMEOUT_MS,
		);
		signal.addEventListener('abort', onAbort, {once: true});
		if (signal.aborted) {
			finish(() => reject(new LegacyOriginHarvestAbortedError()));
			return;
		}
		ipcMain.handle(DESKTOP_LEGACY_HARVEST_CHANNELS.submit, (event, submission: unknown): void => {
			if (event.sender !== harvestWindow.webContents) {
				throw new LegacyOriginHarvestUnavailableError('a foreign renderer attempted to submit a harvest');
			}
			try {
				const result = parseHarvestSubmission(origin, submission);
				finish(() => resolve(result));
			} catch (error) {
				finish(() => reject(error));
			}
		});
		harvestWindow.webContents.on('render-process-gone', (_event, details) => {
			finish(() => reject(new LegacyOriginHarvestFailedError(`the harvest renderer exited: ${details.reason}`)));
		});
		harvestWindow.webContents.on('did-fail-load', (_event, _errorCode, errorDescription, _url, isMainFrame) => {
			if (!isMainFrame) return;
			finish(() =>
				reject(new LegacyOriginHarvestFailedError(`the sentinel document failed to load: ${errorDescription}`)),
			);
		});
		harvestWindow.loadURL(sentinelUrl).catch((error: unknown) => {
			finish(() => reject(new LegacyOriginHarvestFailedError(describeError(error))));
		});
	});
}

async function runLegacyOriginHarvest(origin: string, signal: AbortSignal): Promise<LegacyHarvestResult> {
	const {protocol} = session.defaultSession;
	if (protocol.isProtocolHandled('https')) {
		throw new LegacyOriginHarvestUnavailableError('another handler already owns the https scheme');
	}
	const sentinelUrl = `${origin}${DESKTOP_LEGACY_HARVEST_SENTINEL_PATH}`;
	protocol.handle('https', (request) => {
		if (request.url === sentinelUrl) {
			return new Response(SENTINEL_DOCUMENT, {status: 200, headers: SENTINEL_HEADERS});
		}
		return net.fetch(request, {bypassCustomProtocolHandlers: true});
	});
	const harvestWindow = createHarvestWindow();
	try {
		return await awaitHarvestSubmission(harvestWindow, origin, sentinelUrl, signal);
	} finally {
		ipcMain.removeHandler(DESKTOP_LEGACY_HARVEST_CHANNELS.submit);
		if (!harvestWindow.isDestroyed()) harvestWindow.destroy();
		protocol.unhandle('https');
	}
}

function isEmptyHarvest(harvest: StagedLegacyHarvest): boolean {
	if (Object.keys(harvest.localStorage).length > 0) return false;
	return harvest.stores.every((store) => store.records.length === 0);
}

async function recordHarvestFailure(
	storage: DesktopAppStorage,
	origin: string,
	failures: number,
	error: unknown,
): Promise<void> {
	if (error instanceof LegacyOriginHarvestAbortedError || error instanceof LegacyOriginHarvestUnavailableError) {
		logger.warn('Deferring the legacy origin harvest to the next launch', {origin, reason: describeError(error)});
		return;
	}
	const attempts = failures + 1;
	logger.error('Legacy origin harvest attempt failed', {origin, attempts, error});
	try {
		await storage.setMarker(DESKTOP_LEGACY_HARVEST_FAILURE_MARKER_KEY, String(attempts));
		if (attempts >= DESKTOP_LEGACY_HARVEST_MAX_FAILURES) {
			await writeHarvestMarker(storage, DesktopLegacyHarvestStatus.ABANDONED, origin);
			logger.error('Abandoning the legacy origin harvest after repeated failures', {origin, attempts});
		}
	} catch (markerError) {
		logger.error('Failed to record the legacy origin harvest failure', markerError);
	}
}

export async function initializeLegacyOriginHarvest(signal: AbortSignal): Promise<void> {
	if (BUILD_CHANNEL === 'development') {
		logger.info('Skipping the legacy origin harvest because the development channel never shipped a remote origin');
		return;
	}
	const storage = getDesktopAppStorage();
	if (storage === null) {
		logger.warn('Skipping the legacy origin harvest because the desktop app store is unavailable');
		return;
	}
	const origin = getLegacyAppOrigin();
	let failures = 0;
	try {
		const marker = parseHarvestMarker(await storage.getMarker(DESKTOP_LEGACY_HARVEST_MARKER_KEY));
		if (marker !== null && marker.status !== DesktopLegacyHarvestStatus.PENDING) return;
		failures = parseFailureCount(await storage.getMarker(DESKTOP_LEGACY_HARVEST_FAILURE_MARKER_KEY));
		if (failures >= DESKTOP_LEGACY_HARVEST_MAX_FAILURES) {
			await writeHarvestMarker(storage, DesktopLegacyHarvestStatus.ABANDONED, origin);
			return;
		}
		if (signal.aborted) return;
		const startedAt = Date.now();
		const harvested = await runLegacyOriginHarvest(origin, signal);
		const harvest = rekeyHarvestToOfficialInstance(harvested.harvest);
		const blobs = harvested.blobs;
		await writeStagedHarvest(harvest, blobs);
		seedDesktopLastRouteFromLegacyStorage(app.getPath('userData'), harvest.localStorage);
		const status = isEmptyHarvest(harvest) ? DesktopLegacyHarvestStatus.EMPTY : DesktopLegacyHarvestStatus.HARVESTED;
		await writeHarvestMarker(storage, status, origin);
		await storage.setMarker(DESKTOP_LEGACY_HARVEST_FAILURE_MARKER_KEY, '0');
		logger.info('Legacy origin harvest completed', {
			origin,
			status,
			elapsedMs: Date.now() - startedAt,
			localStorageKeys: Object.keys(harvest.localStorage).length,
			stores: harvest.stores.length,
			blobs: blobs.size,
			mediaDevices: harvest.mediaDevices.length,
			serviceWorkersUnregistered: harvest.serviceWorkersUnregistered,
			cachesDeleted: harvest.cachesDeleted,
			truncated: harvest.truncated,
		});
	} catch (error) {
		await recordHarvestFailure(storage, origin, failures, error);
	}
}
