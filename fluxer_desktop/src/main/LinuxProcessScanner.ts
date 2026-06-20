// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs/promises';
import path from 'node:path';
import log from 'electron-log';
import {emitSyntheticActivity} from '@electron/main/ArRpcServer';
import {
	getDetectableDb,
	getExecutableIndex,
	matchAppByWindowsCmdline,
	matchLinuxExecutable,
	resolveByClientId,
} from '@electron/main/DetectableApplications';
import {
	ANTI_CHEAT_EXECUTABLES,
	CMDLINE_NULL_SEPARATOR,
	EXECUTABLE_ARCH_SUFFIXES,
	LOST_GAME_MISS_THRESHOLD,
	LINUX_PROC_DIR,
	PROCESS_SCAN_INTERVAL,
} from '@electron/main/rpc/RpcConstants';
import type {DetectableApp, RpcActivityPayload} from '@electron/main/rpc/RpcTypes';

interface GameState {
	name: string;
	pid: number;
	timestamp: number;
	missedScans: number;
}

let scanTimer: NodeJS.Timeout | null = null;
let isScanning = false;
const gameState = new Map<string, GameState>();
let lastEmittedPrimaryId: string | null = null;

export function getActiveScannedGameId(): string | null {
	return lastEmittedPrimaryId;
}

function isIgnoredPath(processPath: string): boolean {
	const lower = processPath.toLowerCase();
	return ANTI_CHEAT_EXECUTABLES.some((name) => lower.includes(name));
}

function generatePathVariations(normalizedPath: string): string[] {
	const toCompare: string[] = [];
	const splitPath = normalizedPath.split('/');
	for (let i = 1; i <= splitPath.length; i++) {
		toCompare.push(splitPath.slice(-i).join('/'));
	}
	const baseLength = toCompare.length;
	for (let i = 0; i < baseLength; i++) {
		const segment = toCompare[i];
		if (!segment) continue;
		for (const suffix of EXECUTABLE_ARCH_SUFFIXES) {
			if (segment.includes(suffix)) {
				toCompare.push(segment.replace(suffix, ''));
			}
		}
	}
	return toCompare;
}

async function readProcessEntries(): Promise<Array<[number, string, string[]]>> {
	const entries = await fs.readdir(LINUX_PROC_DIR, {withFileTypes: true});
	const processes: Array<[number, string, string[]]> = [];
	for (const entry of entries) {
		if (!entry.isDirectory() || !/^\d+$/.test(entry.name)) continue;
		const pid = Number.parseInt(entry.name, 10);
		try {
			const cmdline = await fs.readFile(path.join(LINUX_PROC_DIR, entry.name, 'cmdline'), 'utf8');
			if (!cmdline) continue;
			const args = cmdline.split(CMDLINE_NULL_SEPARATOR).filter(Boolean);
			const exePath = args[0] ?? cmdline.replaceAll(CMDLINE_NULL_SEPARATOR, ' ').trim();
			if (!exePath || isIgnoredPath(exePath)) continue;
			processes.push([pid, exePath, args]);
		} catch {
			continue;
		}
	}
	return processes;
}

function getCandidateApps(pathVariations: string[]): DetectableApp[] {
	const executableIndex = getExecutableIndex();
	const candidateSet = new Set<DetectableApp>();
	for (const pathVar of pathVariations) {
		const apps = executableIndex.get(pathVar);
		if (apps) {
			for (const app of apps) apps.forEach((a) => candidateSet.add(a));
		}
		const lastSlash = pathVar.lastIndexOf('/');
		const filename = lastSlash >= 0 ? pathVar.slice(lastSlash + 1) : pathVar;
		const dotIndex = filename.lastIndexOf('.');
		if (dotIndex > 0) {
			const withoutExt = filename.slice(0, dotIndex);
			const appsNoExt = executableIndex.get(withoutExt);
			if (appsNoExt) {
				for (const app of appsNoExt) candidateSet.add(app);
			}
		}
	}
	return [...candidateSet];
}

function pickPrimaryGameId(activeIds: Set<string>): string | null {
	let primaryId: string | null = null;
	let primaryTimestamp = 0;
	for (const id of activeIds) {
		const state = gameState.get(id);
		if (!state) continue;
		if (state.timestamp > primaryTimestamp) {
			primaryTimestamp = state.timestamp;
			primaryId = id;
		}
	}
	return primaryId;
}

function emitPrimaryGame(id: string): void {
	const state = gameState.get(id);
	if (!state) return;
	const payload: RpcActivityPayload = {
		application_id: id,
		name: state.name,
		type: 0,
		timestamps: {start: Math.floor(state.timestamp / 1000)},
		pid: state.pid,
	};
	emitSyntheticActivity(payload, state.pid);
	lastEmittedPrimaryId = id;
	log.info('[RPC] Process scan active game', state.name);
}

export function reemitActiveScannedGame(): void {
	if (!lastEmittedPrimaryId) return;
	emitPrimaryGame(lastEmittedPrimaryId);
}

function clearPrimaryGame(): void {
	if (!lastEmittedPrimaryId) return;
	emitSyntheticActivity(null);
	lastEmittedPrimaryId = null;
	log.info('[RPC] Process scan cleared active game');
}

function syncPrimaryGame(activeIds: Set<string>): void {
	const primaryId = pickPrimaryGameId(activeIds);
	if (primaryId === lastEmittedPrimaryId) {
		return;
	}
	if (primaryId) {
		emitPrimaryGame(primaryId);
		return;
	}
	clearPrimaryGame();
}

async function scan(): Promise<void> {
	if (process.platform !== 'linux' || isScanning) return;
	isScanning = true;
	try {
		getDetectableDb();
		const processes = await readProcessEntries();
		const activeIds = new Set<string>();
		for (const [pid, processPath, args] of processes) {
			let matchedApp: DetectableApp | null = null;
			if (!isIgnoredPath(processPath)) {
				const normalized = processPath.toLowerCase().replaceAll('\\', '/');
				const variations = generatePathVariations(normalized);
				const candidates = getCandidateApps(variations);
				for (const app of candidates) {
					if (!app.executables) continue;
					const matched = app.executables.some((exe) => matchLinuxExecutable(exe, variations));
					if (matched) {
						matchedApp = app;
						break;
					}
				}
			}
			if (!matchedApp && args.some((arg) => /\.exe/i.test(arg))) {
				matchedApp = matchAppByWindowsCmdline(args);
			}
			if (!matchedApp) continue;
			activeIds.add(matchedApp.id);
			const state = gameState.get(matchedApp.id);
			if (!state) {
				gameState.set(matchedApp.id, {name: matchedApp.name, pid, timestamp: Date.now(), missedScans: 0});
				log.info('[RPC] Process scan detected game', matchedApp.name);
			} else if (state.pid !== pid) {
				state.pid = pid;
				state.missedScans = 0;
			} else {
				state.missedScans = 0;
			}
		}
		for (const [id, state] of gameState) {
			if (activeIds.has(id)) continue;
			state.missedScans += 1;
			if (state.missedScans < LOST_GAME_MISS_THRESHOLD) continue;
			gameState.delete(id);
			log.info('[RPC] Process scan lost game', state.name);
		}
		syncPrimaryGame(activeIds);
	} finally {
		isScanning = false;
	}
}

export function startLinuxProcessScanner(): void {
	if (process.platform !== 'linux' || scanTimer) return;
	scanTimer = setInterval(() => void scan(), PROCESS_SCAN_INTERVAL);
	setTimeout(() => void scan(), 10_000);
	log.info('[RPC] LinuxProcessScanner started');
}

export function stopLinuxProcessScanner(): void {
	if (scanTimer) {
		clearInterval(scanTimer);
		scanTimer = null;
	}
	gameState.clear();
	lastEmittedPrimaryId = null;
}

export function __testGeneratePathVariations(normalizedPath: string): string[] {
	return generatePathVariations(normalizedPath);
}

export function __testGetCandidateApps(pathVariations: string[]): DetectableApp[] {
	return getCandidateApps(pathVariations);
}

export function __testReadProcessEntries(): Promise<Array<[number, string, string[]]>> {
	return readProcessEntries();
}

export function __testMatchAppByWindowsCmdline(args: string[]) {
	return matchAppByWindowsCmdline(args);
}
