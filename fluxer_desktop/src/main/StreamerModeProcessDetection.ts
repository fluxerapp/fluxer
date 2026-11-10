// SPDX-License-Identifier: AGPL-3.0-or-later

import {execFile} from 'node:child_process';
import path from 'node:path';
import {promisify} from 'node:util';
import type {StreamerModeCaptureAppStatus, StreamerModeCaptureProcess} from '@electron/common/Types';

const execFileAsync = promisify(execFile);
const PROCESS_QUERY_MAX_BUFFER_BYTES = 64 * 1024;
const MAX_DETECTED_CAPTURE_PROCESSES = 16;
const CAPTURE_APP_STATUS_REUSE_MS = 2_000;
const WINDOWS_CAPTURE_PROCESS_NAMES = [
	'obs.exe',
	'obs32.exe',
	'obs64.exe',
	'obs-studio.exe',
	'xsplit.exe',
	'xsplit.core.exe',
	'xsplit.broadcaster.exe',
];
const POSIX_CAPTURE_PROCESS_NAMES = ['obs', 'obs-studio', 'xsplit'];
const MATCHED_PROCESS_NAMES = new Set<string>([
	'obs',
	'obs.exe',
	'obs32.exe',
	'obs64.exe',
	'obs-studio',
	'obs-studio.exe',
	'xsplit',
	'xsplit.exe',
	'xsplit.core.exe',
	'xsplit.broadcaster.exe',
]);

function normalizeProcessName(value: string): string {
	const normalized = value.trim().replace(/^"|"$/g, '');
	return path.basename(normalized).toLowerCase();
}

function isCaptureAppProcess(name: string, args?: string): boolean {
	const normalizedName = normalizeProcessName(name);
	if (MATCHED_PROCESS_NAMES.has(normalizedName)) {
		return true;
	}
	const normalizedArgs = args?.toLowerCase() ?? '';
	return normalizedName === 'xsplit' || normalizedArgs.includes('xsplit broadcaster');
}

function getProcessKey(process: StreamerModeCaptureProcess): string {
	return process.pid == null ? `name:${process.name}` : `pid:${process.pid}`;
}

function pushProcess(
	matches: Array<StreamerModeCaptureProcess>,
	seenKeys: Set<string>,
	process: StreamerModeCaptureProcess,
): void {
	if (matches.length >= MAX_DETECTED_CAPTURE_PROCESSES) return;
	const key = getProcessKey(process);
	if (seenKeys.has(key)) return;
	seenKeys.add(key);
	matches.push(process);
}

function parseWindowsCsvLine(line: string): Array<string> {
	const cells: Array<string> = [];
	let current = '';
	let inQuotes = false;
	for (let i = 0; i < line.length; i++) {
		const char = line[i];
		if (char === '"') {
			if (inQuotes && line[i + 1] === '"') {
				current += '"';
				i++;
			} else {
				inQuotes = !inQuotes;
			}
			continue;
		}
		if (char === ',' && !inQuotes) {
			cells.push(current);
			current = '';
			continue;
		}
		current += char;
	}
	cells.push(current);
	return cells;
}

function parseWindowsTasklistCsv(stdout: string): Array<StreamerModeCaptureProcess> {
	const processes: Array<StreamerModeCaptureProcess> = [];
	for (const line of stdout.split(/\r?\n/)) {
		if (!line.trim()) continue;
		const [imageName, pidRaw] = parseWindowsCsvLine(line);
		if (!imageName || !isCaptureAppProcess(imageName)) continue;
		const pid = Number.parseInt(pidRaw ?? '', 10);
		processes.push({
			name: normalizeProcessName(imageName),
			...(Number.isFinite(pid) ? {pid} : {}),
		});
	}
	return processes;
}

async function getWindowsProcesses(): Promise<Array<StreamerModeCaptureProcess>> {
	const matches: Array<StreamerModeCaptureProcess> = [];
	const seenKeys = new Set<string>();
	for (const processName of WINDOWS_CAPTURE_PROCESS_NAMES) {
		if (matches.length >= MAX_DETECTED_CAPTURE_PROCESSES) break;
		const {stdout} = await execFileAsync('tasklist', ['/fi', `IMAGENAME eq ${processName}`, '/fo', 'csv', '/nh'], {
			windowsHide: true,
			maxBuffer: PROCESS_QUERY_MAX_BUFFER_BYTES,
		});
		for (const process of parseWindowsTasklistCsv(stdout)) {
			pushProcess(matches, seenKeys, process);
		}
	}
	return matches;
}

function parsePosixPidLines(stdout: string): Array<number> {
	const pids: Array<number> = [];
	for (const line of stdout.split(/\r?\n/)) {
		if (pids.length >= MAX_DETECTED_CAPTURE_PROCESSES) break;
		const pid = Number.parseInt(line.trim(), 10);
		if (Number.isFinite(pid) && pid > 0) {
			pids.push(pid);
		}
	}
	return pids;
}

function getExecErrorCode(error: unknown): string | number | undefined {
	if (typeof error !== 'object' || error == null) return undefined;
	return (error as {code?: string | number}).code;
}

function getExecErrorStdout(error: unknown): string {
	if (typeof error !== 'object' || error == null) return '';
	const stdout = (error as {stdout?: unknown}).stdout;
	if (typeof stdout === 'string') return stdout;
	if (stdout == null) return '';
	return String(stdout);
}

function isExecNoMatch(error: unknown): boolean {
	const code = getExecErrorCode(error);
	return code === 1 || code === '1';
}

function isExecMissing(error: unknown): boolean {
	return getExecErrorCode(error) === 'ENOENT';
}

function isExecMaxBuffer(error: unknown): boolean {
	if (getExecErrorCode(error) === 'ERR_CHILD_PROCESS_STDIO_MAXBUFFER') return true;
	if (!(error instanceof Error)) return false;
	return error.message.includes('maxBuffer');
}

async function getPosixProcessesWithPgrep(): Promise<Array<StreamerModeCaptureProcess> | null> {
	const matches: Array<StreamerModeCaptureProcess> = [];
	const seenKeys = new Set<string>();
	for (const processName of POSIX_CAPTURE_PROCESS_NAMES) {
		if (matches.length >= MAX_DETECTED_CAPTURE_PROCESSES) break;
		let stdout = '';
		try {
			const result = await execFileAsync('pgrep', ['-i', '-x', processName], {
				maxBuffer: PROCESS_QUERY_MAX_BUFFER_BYTES,
			});
			stdout = result.stdout;
		} catch (error) {
			if (isExecNoMatch(error)) continue;
			if (isExecMissing(error)) return null;
			if (!isExecMaxBuffer(error)) return null;
			stdout = getExecErrorStdout(error);
			if (!stdout.trim()) {
				pushProcess(matches, seenKeys, {name: normalizeProcessName(processName)});
				continue;
			}
		}
		for (const pid of parsePosixPidLines(stdout)) {
			pushProcess(matches, seenKeys, {name: normalizeProcessName(processName), pid});
		}
	}
	return matches;
}

async function getPosixProcessesWithPs(): Promise<Array<StreamerModeCaptureProcess>> {
	const {stdout} = await execFileAsync('ps', ['-axo', 'pid=,comm='], {maxBuffer: PROCESS_QUERY_MAX_BUFFER_BYTES});
	const matches: Array<StreamerModeCaptureProcess> = [];
	const seenKeys = new Set<string>();
	for (const line of stdout.split(/\r?\n/)) {
		const match = line.match(/^\s*(\d+)\s+(\S+)\s*(.*)$/);
		if (!match) continue;
		const [, pidRaw, command] = match;
		if (!command || !isCaptureAppProcess(command)) continue;
		const pid = Number.parseInt(pidRaw, 10);
		pushProcess(matches, seenKeys, {
			name: normalizeProcessName(command),
			...(Number.isFinite(pid) ? {pid} : {}),
		});
	}
	return matches;
}

async function getPosixProcesses(): Promise<Array<StreamerModeCaptureProcess>> {
	const pgrepProcesses = await getPosixProcessesWithPgrep();
	if (pgrepProcesses != null) return pgrepProcesses;
	return getPosixProcessesWithPs();
}

let recentStatus: {readonly startedAt: number; readonly status: Promise<StreamerModeCaptureAppStatus>} | null = null;

export function getStreamerModeCaptureAppStatus(): Promise<StreamerModeCaptureAppStatus> {
	const now = Date.now();
	if (recentStatus !== null && now - recentStatus.startedAt < CAPTURE_APP_STATUS_REUSE_MS) {
		return recentStatus.status;
	}
	const status = detectCaptureAppStatus();
	recentStatus = {startedAt: now, status};
	return status;
}

async function detectCaptureAppStatus(): Promise<StreamerModeCaptureAppStatus> {
	let processes: Array<StreamerModeCaptureProcess> = [];
	try {
		processes = process.platform === 'win32' ? await getWindowsProcesses() : await getPosixProcesses();
	} catch {
		processes = [];
	}
	return {
		detected: processes.length > 0,
		processes,
	};
}
