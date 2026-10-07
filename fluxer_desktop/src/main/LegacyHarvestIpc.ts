// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChildLogger} from '@electron/common/Logger';
import {getDesktopAppStorage} from '@electron/main/DesktopAppStorage';
import {
	discardStagedHarvest,
	isStagedHarvestReplanted,
	markStagedHarvestReplanted,
	readStagedHarvest,
	readStagedHarvestLocalStorageSync,
	type StagedLegacyHarvest,
} from '@electron/main/LegacyOriginHarvestStore';
import {requirePrivilegedRendererDocumentSender} from '@electron/main/PrivilegedRendererDocuments';
import type {
	DesktopLegacyHarvestRecord,
	DesktopLegacyHarvestStore,
} from '@fluxer/desktop_ipc/src/LegacyHarvestContract';
import {
	DESKTOP_LEGACY_HARVEST_BLOB_REF_KEY,
	DESKTOP_LEGACY_HARVEST_CHANNELS,
	DESKTOP_LEGACY_REPLANT_MARKER_KEY,
	desktopLegacyReplantMarker,
} from '@fluxer/desktop_ipc/src/LegacyHarvestContract';
import {
	DESKTOP_LEGACY_IMPORT_MARKER_KEY,
	DesktopLegacyImportPhase,
	readDesktopLegacyImportPhase,
} from '@fluxer/desktop_ipc/src/StorageContract';
import {type IpcMainEvent, ipcMain} from 'electron';

const logger = createChildLogger('LegacyHarvest');

type BlobReader = (blobId: string) => Promise<Uint8Array | null>;

let handlersRegistered = false;

function isPlainObject(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function readBlobId(value: unknown): string | null {
	if (!isPlainObject(value)) return null;
	const reference = value[DESKTOP_LEGACY_HARVEST_BLOB_REF_KEY];
	if (!isPlainObject(reference)) return null;
	const blobId = reference.blobId;
	return typeof blobId === 'string' ? blobId : null;
}

async function readBlobBytes(readBlob: BlobReader, blobId: string): Promise<Uint8Array | null> {
	try {
		return await readBlob(blobId);
	} catch (error) {
		logger.warn('Dropping a legacy harvest field whose staged blob could not be read', error);
		return null;
	}
}

async function inlineBlobs(value: unknown, readBlob: BlobReader): Promise<unknown> {
	if (value === null || typeof value !== 'object') return value;
	const blobId = readBlobId(value);
	if (blobId !== null) {
		const bytes = await readBlobBytes(readBlob, blobId);
		return bytes === null ? null : {...value, bytes};
	}
	if (Array.isArray(value)) {
		const items: Array<unknown> = [];
		for (const item of value) {
			items.push(await inlineBlobs(item, readBlob));
		}
		return items;
	}
	const inlined: Record<string, unknown> = {};
	for (const [key, item] of Object.entries(value)) {
		inlined[key] = await inlineBlobs(item, readBlob);
	}
	return inlined;
}

async function readHarvestForRenderer(): Promise<StagedLegacyHarvest | null> {
	if (await isStagedHarvestReplanted()) return null;
	const staged = await readStagedHarvest();
	if (staged === null) return null;
	const stores: Array<DesktopLegacyHarvestStore> = [];
	for (const store of staged.harvest.stores) {
		const records: Array<DesktopLegacyHarvestRecord> = [];
		for (const record of store.records) {
			records.push({key: record.key, value: await inlineBlobs(record.value, staged.readBlob)});
		}
		stores.push({...store, records});
	}
	return {...staged.harvest, stores};
}

async function markHarvestReplanted(): Promise<void> {
	await markStagedHarvestReplanted();
	const storage = getDesktopAppStorage();
	if (storage === null) return;
	await storage.setMarker(DESKTOP_LEGACY_REPLANT_MARKER_KEY, desktopLegacyReplantMarker(Date.now()));
}

async function discardHarvestWhenComplete(): Promise<void> {
	const storage = getDesktopAppStorage();
	if (storage === null) return;
	if ((await storage.getMarker(DESKTOP_LEGACY_REPLANT_MARKER_KEY)) === null) return;
	const phase = readDesktopLegacyImportPhase(await storage.getMarker(DESKTOP_LEGACY_IMPORT_MARKER_KEY));
	if (phase !== DesktopLegacyImportPhase.DONE) return;
	if (await discardStagedHarvest()) {
		logger.info('Discarded the staged legacy harvest');
	}
}

function readPrebootLocalStorage(): Readonly<Record<string, string>> | null {
	return readStagedHarvestLocalStorageSync();
}

function isPrivilegedHarvestSender(event: IpcMainEvent, channel: string): boolean {
	try {
		requirePrivilegedRendererDocumentSender(event, channel);
		return true;
	} catch {
		return false;
	}
}

export function registerLegacyHarvestHandlers(): void {
	if (handlersRegistered) return;
	handlersRegistered = true;
	ipcMain.handle(DESKTOP_LEGACY_HARVEST_CHANNELS.read, (event) => {
		requirePrivilegedRendererDocumentSender(event, DESKTOP_LEGACY_HARVEST_CHANNELS.read);
		return readHarvestForRenderer();
	});
	ipcMain.handle(DESKTOP_LEGACY_HARVEST_CHANNELS.markReplanted, (event) => {
		requirePrivilegedRendererDocumentSender(event, DESKTOP_LEGACY_HARVEST_CHANNELS.markReplanted);
		return markHarvestReplanted();
	});
	ipcMain.handle(DESKTOP_LEGACY_HARVEST_CHANNELS.discard, (event) => {
		requirePrivilegedRendererDocumentSender(event, DESKTOP_LEGACY_HARVEST_CHANNELS.discard);
		return discardHarvestWhenComplete();
	});
	ipcMain.on(DESKTOP_LEGACY_HARVEST_CHANNELS.readRawSync, (event) => {
		event.returnValue = isPrivilegedHarvestSender(event, DESKTOP_LEGACY_HARVEST_CHANNELS.readRawSync)
			? readPrebootLocalStorage()
			: null;
	});
}

export function cleanupLegacyHarvestHandlers(): void {
	if (!handlersRegistered) return;
	handlersRegistered = false;
	ipcMain.removeHandler(DESKTOP_LEGACY_HARVEST_CHANNELS.read);
	ipcMain.removeHandler(DESKTOP_LEGACY_HARVEST_CHANNELS.markReplanted);
	ipcMain.removeHandler(DESKTOP_LEGACY_HARVEST_CHANNELS.discard);
	ipcMain.removeAllListeners(DESKTOP_LEGACY_HARVEST_CHANNELS.readRawSync);
}
