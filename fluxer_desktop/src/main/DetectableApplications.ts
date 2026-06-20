// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {app} from 'electron';
import log from 'electron-log';
import {DISCORD_CDN_HOST, EXECUTABLE_EXACT_MATCH_PREFIX} from '@electron/main/rpc/RpcConstants';
import type {DetectableApp, DetectableExecutable} from '@electron/main/rpc/RpcTypes';

export interface ResolvedApplication {
	id: string;
	name: string;
	iconUrl: string | null;
}

interface FluxerDetectableRecord {
	name: string;
	url?: string;
	aliases?: string[];
	executables?: DetectableExecutable[];
	client_id?: string;
}

let detectableDb: DetectableApp[] = [];
const clientIdIndex = new Map<string, DetectableApp>();
const executableIndex = new Map<string, DetectableApp[]>();
let loaded = false;
let syncPromise: Promise<void> | null = null;

function getBundledRpcDir(): string {
	if (app.isPackaged) {
		return path.join(process.resourcesPath, 'rpc');
	}
	const candidates = [
		path.join(app.getAppPath(), 'assets', 'rpc'),
		path.join(app.getAppPath(), 'dist', 'rpc'),
	];
	for (const candidate of candidates) {
		if (fs.existsSync(path.join(candidate, 'detectables.json'))) {
			return candidate;
		}
	}
	return path.join(app.getAppPath(), 'assets', 'rpc');
}

function getDetectablesCacheDir(): string {
	return path.join(app.getPath('userData'), 'rpc');
}

function getLockPath(): string {
	return path.join(getBundledRpcDir(), 'detectable-lock.json');
}

function getDetectablesPath(): string {
	return path.join(getDetectablesCacheDir(), 'detectables.json');
}

function slugifyDetectableId(name: string): string {
	return name
		.toLowerCase()
		.replace(/[^a-z0-9]+/g, '-')
		.replace(/^-+|-+$/g, '');
}

function normalizeDetectableRecord(record: FluxerDetectableRecord): DetectableApp {
	const id = record.client_id ?? slugifyDetectableId(record.name);
	return {
		id,
		name: record.name,
		url: record.url,
		aliases: record.aliases,
		executables: record.executables,
		client_id: record.client_id,
	};
}

function buildIndexes(): void {
	clientIdIndex.clear();
	executableIndex.clear();
	windowsCmdlinePatternsByBasename = null;
	for (const entry of detectableDb) {
		clientIdIndex.set(entry.id, entry);
		if (entry.client_id) {
			clientIdIndex.set(entry.client_id, entry);
		}
		if (!entry.executables) continue;
		for (const exe of entry.executables) {
			const exeName = exe.name.toLowerCase();
			const key = exeName.startsWith(EXECUTABLE_EXACT_MATCH_PREFIX) ? exeName.slice(1) : exeName;
			const list = executableIndex.get(key) ?? [];
			list.push(entry);
			executableIndex.set(key, list);
		}
	}
}

function buildIconUrl(entry: DetectableApp): string | null {
	if (entry.url) return entry.url;
	if (entry.icon_hash) {
		return `https://${DISCORD_CDN_HOST}/app-assets/${entry.id}/${entry.icon_hash}.png`;
	}
	return null;
}

async function syncDetectableApplicationsInner(): Promise<void> {
	const lockPath = getLockPath();
	const lock = JSON.parse(fs.readFileSync(lockPath, 'utf8')) as {
		repo?: string;
		ref?: string;
		files?: Array<string>;
	};
	const repo = lock.repo ?? 'fluxerapp/detectables';
	const ref = lock.ref ?? 'main';
	const files = lock.files ?? ['detectables.json', 'detectables.schema.json'];
	const baseUrl = `https://raw.githubusercontent.com/${repo}/${ref}`;
	const cacheDir = getDetectablesCacheDir();

	fs.mkdirSync(cacheDir, {recursive: true});
	for (const file of files) {
		const url = `${baseUrl}/${file}`;
		const res = await fetch(url, {cache: 'no-store'});
		if (!res.ok) {
			throw new Error(`Failed ${url}: ${res.status}`);
		}
		const targetPath = path.join(cacheDir, file);
		fs.writeFileSync(targetPath, Buffer.from(await res.arrayBuffer()));
	}
	log.info('[RPC] Synced detectables', {repo, ref, files});
}

export function syncDetectableApplications(): Promise<void> {
	if (!syncPromise) {
		syncPromise = syncDetectableApplicationsInner().catch((error) => {
			syncPromise = null;
			throw error;
		});
	}
	return syncPromise;
}

export function loadDetectableApplications(): void {
	if (loaded) return;
	const detectablePath = getDetectablesPath();
	if (!fs.existsSync(detectablePath)) {
		log.warn('[RPC] Detectables cache missing, using empty database', {path: detectablePath});
		detectableDb = [];
		buildIndexes();
		loaded = true;
		return;
	}
	const raw = JSON.parse(fs.readFileSync(detectablePath, 'utf8')) as FluxerDetectableRecord[];
	detectableDb = raw.map(normalizeDetectableRecord);
	buildIndexes();
	loaded = true;
	log.info(`[RPC] Loaded ${detectableDb.length} detectable applications`);
}

export function resetDetectableApplicationsForTests(): void {
	detectableDb = [];
	clientIdIndex.clear();
	executableIndex.clear();
	windowsCmdlinePatternsByBasename = null;
	loaded = false;
	syncPromise = null;
}

export function resolveByClientId(clientId: string): ResolvedApplication | null {
	loadDetectableApplications();
	const entry = clientIdIndex.get(clientId);
	if (!entry) return null;
	return {
		id: entry.client_id ?? entry.id,
		name: entry.name,
		iconUrl: buildIconUrl(entry),
	};
}

export function resolveByExecutable(exeName: string): DetectableApp | null {
	loadDetectableApplications();
	const key = exeName.toLowerCase();
	const candidates = executableIndex.get(key);
	return candidates?.[0] ?? null;
}

export function getDetectableDb(): DetectableApp[] {
	loadDetectableApplications();
	return detectableDb;
}

export function getExecutableIndex(): Map<string, DetectableApp[]> {
	loadDetectableApplications();
	return executableIndex;
}

export function matchLinuxExecutable(
	executable: DetectableExecutable,
	pathVariations: string[],
	platform: string = process.platform,
): boolean {
	if (executable.os && executable.os !== platform) return false;
	const firstCompare = pathVariations[0];
	if (!firstCompare) return false;
	const firstChar = executable.name[0];
	if (firstChar === EXECUTABLE_EXACT_MATCH_PREFIX) {
		return executable.name.slice(1) === firstCompare;
	}
	return pathVariations.some((variation) => variation === executable.name.toLowerCase());
}

interface WindowsCmdlinePattern {
	pattern: string;
	argsPattern?: string;
	app: DetectableApp;
}

let windowsCmdlinePatternsByBasename: Map<string, WindowsCmdlinePattern[]> | null = null;
const WIN32_EXE_IN_CMDLINE = /[^/\\]+\.exe/gi;

function addCmdlinePattern(map: Map<string, WindowsCmdlinePattern[]>, key: string, entry: WindowsCmdlinePattern): void {
	const list = map.get(key) ?? [];
	list.push(entry);
	map.set(key, list);
}

function buildWindowsCmdlinePatternsByBasename(): Map<string, WindowsCmdlinePattern[]> {
	const byBasename = new Map<string, WindowsCmdlinePattern[]>();
	for (const entry of detectableDb) {
		if (!entry.executables) continue;
		for (const exe of entry.executables) {
			if (exe.os !== 'win32' || exe.is_launcher) continue;
			const rawName = exe.name.toLowerCase();
			const pattern = rawName.startsWith(EXECUTABLE_EXACT_MATCH_PREFIX) ? rawName.slice(1) : rawName;
			const item: WindowsCmdlinePattern = {pattern, argsPattern: exe.arguments?.toLowerCase(), app: entry};
			addCmdlinePattern(byBasename, pattern.split('/').pop() ?? pattern, item);
			if (pattern.includes('/')) {
				addCmdlinePattern(byBasename, pattern, item);
			}
		}
	}
	for (const list of byBasename.values()) {
		list.sort((a, b) => b.pattern.length - a.pattern.length);
	}
	return byBasename;
}

export function matchAppByWindowsCmdline(args: string[]): DetectableApp | null {
	loadDetectableApplications();
	const cmdlineLower = args.join(' ').toLowerCase();
	if (!cmdlineLower.includes('.exe')) return null;
	if (!windowsCmdlinePatternsByBasename) {
		windowsCmdlinePatternsByBasename = buildWindowsCmdlinePatternsByBasename();
	}
	const exeNames = cmdlineLower.match(WIN32_EXE_IN_CMDLINE);
	if (!exeNames || exeNames.length === 0) return null;
	for (const exeName of exeNames) {
		const patterns = windowsCmdlinePatternsByBasename.get(exeName.toLowerCase());
		if (!patterns) continue;
		for (const {pattern, argsPattern, app: matchedApp} of patterns) {
			if (!cmdlineLower.includes(pattern)) continue;
			if (argsPattern && !cmdlineLower.includes(argsPattern)) continue;
			return matchedApp;
		}
	}
	return null;
}
