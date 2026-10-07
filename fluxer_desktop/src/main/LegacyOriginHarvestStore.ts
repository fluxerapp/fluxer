// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import fs from 'node:fs';
import fsPromises from 'node:fs/promises';
import path from 'node:path';
import type {
	DesktopLegacyHarvest,
	DesktopLegacyHarvestMediaDevice,
	DesktopLegacyHarvestRecord,
	DesktopLegacyHarvestStore,
} from '@fluxer/desktop_ipc/src/LegacyHarvestContract';
import {app} from 'electron';

const HARVEST_DIRECTORY_NAME = 'legacy-harvest';
const MANIFEST_FILE_NAME = 'manifest.json';
const BLOB_DIRECTORY_NAME = 'blobs';
const BLOB_FILE_EXTENSION = '.bin';
const REPLANT_SENTINEL_FILE_NAME = 'replanted';
const DIRECTORY_MODE = 0o700;
const FILE_MODE = 0o600;
const BLOB_ID_PATTERN = /^[0-9a-f]{32}$/u;

class LegacyHarvestBlobIdError extends TypeError {
	public constructor(blobId: string) {
		super(`Legacy harvest blob id is not a 32 character hex string: ${blobId}`);
		this.name = 'LegacyHarvestBlobIdError';
	}
}

export interface StagedLegacyHarvest extends DesktopLegacyHarvest {
	readonly mediaDevices: ReadonlyArray<DesktopLegacyHarvestMediaDevice>;
}

interface StagedLegacyHarvestHandle {
	readonly harvest: StagedLegacyHarvest;
	readonly readBlob: (blobId: string) => Promise<Uint8Array | null>;
}

let cachedRawLocalStorage: Readonly<Record<string, string>> | null = null;
let rawLocalStorageResolved = false;

function isPlainObject(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function parseStringRecord(value: unknown): Record<string, string> | null {
	if (!isPlainObject(value)) return null;
	const result: Record<string, string> = {};
	for (const [key, item] of Object.entries(value)) {
		if (typeof item !== 'string') return null;
		result[key] = item;
	}
	return result;
}

function parseStringArray(value: unknown): Array<string> | null {
	if (!Array.isArray(value)) return null;
	const result: Array<string> = [];
	for (const item of value) {
		if (typeof item !== 'string') return null;
		result.push(item);
	}
	return result;
}

function parseCount(value: unknown): number | null {
	if (typeof value !== 'number' || !Number.isFinite(value) || value < 0) return null;
	return Math.trunc(value);
}

function parseRecords(value: unknown): Array<DesktopLegacyHarvestRecord> | null {
	if (!Array.isArray(value)) return null;
	const result: Array<DesktopLegacyHarvestRecord> = [];
	for (const item of value) {
		if (!isPlainObject(item)) return null;
		if (!('key' in item) || !('value' in item)) return null;
		result.push({key: item.key, value: item.value});
	}
	return result;
}

function parseStores(value: unknown): Array<DesktopLegacyHarvestStore> | null {
	if (!Array.isArray(value)) return null;
	const result: Array<DesktopLegacyHarvestStore> = [];
	for (const item of value) {
		if (!isPlainObject(item)) return null;
		const {database, store, version} = item;
		if (typeof database !== 'string' || database.length === 0) return null;
		if (typeof store !== 'string' || store.length === 0) return null;
		const storeVersion = parseCount(version);
		if (storeVersion === null) return null;
		const records = parseRecords(item.records);
		if (records === null) return null;
		result.push({database, version: storeVersion, store, records});
	}
	return result;
}

function parseMediaDevices(value: unknown): Array<DesktopLegacyHarvestMediaDevice> | null {
	if (value === undefined) {
		return [];
	}
	if (!Array.isArray(value)) {
		return null;
	}
	const result: Array<DesktopLegacyHarvestMediaDevice> = [];
	for (const item of value) {
		if (!isPlainObject(item)) {
			return null;
		}
		const {deviceId, groupId, kind, label} = item;
		if (typeof deviceId !== 'string' || typeof kind !== 'string' || typeof label !== 'string') {
			return null;
		}
		if (groupId !== undefined && typeof groupId !== 'string') {
			return null;
		}
		result.push(groupId === undefined ? {deviceId, kind, label} : {deviceId, kind, label, groupId});
	}
	return result;
}

export function parseStagedLegacyHarvest(value: unknown): StagedLegacyHarvest | null {
	if (!isPlainObject(value)) return null;
	if (value.version !== 1) return null;
	const {capturedAt, origin} = value;
	if (typeof origin !== 'string' || origin.length === 0) return null;
	if (typeof capturedAt !== 'number' || !Number.isFinite(capturedAt)) return null;
	const localStorage = parseStringRecord(value.localStorage);
	if (localStorage === null) return null;
	const stores = parseStores(value.stores);
	if (stores === null) return null;
	const mediaDevices = parseMediaDevices(value.mediaDevices);
	if (mediaDevices === null) return null;
	const serviceWorkersUnregistered = parseCount(value.serviceWorkersUnregistered);
	if (serviceWorkersUnregistered === null) return null;
	const cachesDeleted = parseCount(value.cachesDeleted);
	if (cachesDeleted === null) return null;
	const truncated = parseStringArray(value.truncated);
	if (truncated === null) return null;
	return {
		version: 1,
		origin,
		capturedAt,
		localStorage,
		stores,
		mediaDevices,
		serviceWorkersUnregistered,
		cachesDeleted,
		truncated,
	};
}

function stagedHarvestDirectory(): string {
	return path.join(app.getPath('userData'), HARVEST_DIRECTORY_NAME);
}

function manifestPath(): string {
	return path.join(stagedHarvestDirectory(), MANIFEST_FILE_NAME);
}

function replantSentinelPath(): string {
	return path.join(stagedHarvestDirectory(), REPLANT_SENTINEL_FILE_NAME);
}

function blobPath(blobId: string): string {
	if (!BLOB_ID_PATTERN.test(blobId)) {
		throw new LegacyHarvestBlobIdError(blobId);
	}
	return path.join(stagedHarvestDirectory(), BLOB_DIRECTORY_NAME, `${blobId}${BLOB_FILE_EXTENSION}`);
}

async function pathExists(target: string): Promise<boolean> {
	try {
		await fsPromises.access(target);
		return true;
	} catch {
		return false;
	}
}

async function writeFileAtomically(filePath: string, contents: Uint8Array): Promise<void> {
	const temporaryPath = `${filePath}.${crypto.randomBytes(8).toString('hex')}.tmp`;
	await fsPromises.writeFile(temporaryPath, contents, {flag: 'wx', mode: FILE_MODE});
	try {
		await fsPromises.rename(temporaryPath, filePath);
	} catch (error) {
		await fsPromises.rm(temporaryPath, {force: true});
		throw error;
	}
}

export async function writeStagedHarvest(
	harvest: StagedLegacyHarvest,
	blobs: ReadonlyMap<string, Uint8Array>,
): Promise<void> {
	const directory = stagedHarvestDirectory();
	await fsPromises.rm(directory, {recursive: true, force: true});
	await fsPromises.mkdir(path.join(directory, BLOB_DIRECTORY_NAME), {recursive: true, mode: DIRECTORY_MODE});
	for (const [blobId, bytes] of blobs) {
		await writeFileAtomically(blobPath(blobId), bytes);
	}
	await writeFileAtomically(manifestPath(), Buffer.from(JSON.stringify(harvest), 'utf8'));
	cachedRawLocalStorage = null;
	rawLocalStorageResolved = false;
}

async function readStagedBlob(blobId: string): Promise<Uint8Array | null> {
	const contents = await fsPromises.readFile(blobPath(blobId)).catch(() => null);
	if (contents === null) return null;
	return new Uint8Array(contents.buffer, contents.byteOffset, contents.byteLength);
}

export async function readStagedHarvest(): Promise<StagedLegacyHarvestHandle | null> {
	const raw = await fsPromises.readFile(manifestPath(), 'utf8').catch(() => null);
	if (raw === null) return null;
	let parsed: unknown;
	try {
		parsed = JSON.parse(raw);
	} catch {
		return null;
	}
	const harvest = parseStagedLegacyHarvest(parsed);
	if (harvest === null) return null;
	return {harvest, readBlob: readStagedBlob};
}

export function readStagedHarvestLocalStorageSync(): Readonly<Record<string, string>> | null {
	if (rawLocalStorageResolved) return cachedRawLocalStorage;
	rawLocalStorageResolved = true;
	cachedRawLocalStorage = null;
	try {
		if (fs.existsSync(replantSentinelPath())) return null;
		const harvest = parseStagedLegacyHarvest(JSON.parse(fs.readFileSync(manifestPath(), 'utf8')));
		if (harvest === null) return null;
		cachedRawLocalStorage = harvest.localStorage;
	} catch {
		cachedRawLocalStorage = null;
	}
	return cachedRawLocalStorage;
}

export async function markStagedHarvestReplanted(): Promise<void> {
	const directory = stagedHarvestDirectory();
	await fsPromises.mkdir(directory, {recursive: true, mode: DIRECTORY_MODE});
	await fsPromises.writeFile(replantSentinelPath(), '', {mode: FILE_MODE});
	cachedRawLocalStorage = null;
	rawLocalStorageResolved = true;
}

export async function isStagedHarvestReplanted(): Promise<boolean> {
	return await pathExists(replantSentinelPath());
}

export async function discardStagedHarvest(): Promise<boolean> {
	const directory = stagedHarvestDirectory();
	cachedRawLocalStorage = null;
	rawLocalStorageResolved = true;
	if (!(await pathExists(directory))) return false;
	await fsPromises.rm(directory, {recursive: true, force: true});
	return true;
}
