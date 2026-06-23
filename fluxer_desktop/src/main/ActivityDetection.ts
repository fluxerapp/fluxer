// SPDX-License-Identifier: AGPL-3.0-or-later

import {execFile} from 'node:child_process';
import path from 'node:path';
import {promisify} from 'node:util';
import type {ActivityDetectionStatus, DetectedActivity} from '@electron/common/Types';
import detectablesCatalog from './fixtures/detectables.json' with {type: 'json'};

const execFileAsync = promisify(execFile);
const PROCESS_QUERY_MAX_BUFFER_BYTES = 1024 * 1024;
const MAX_ACTIVITY_PROCESSES = 64;
const MAX_DETECTED_ACTIVITIES = 5;

interface DetectableExecutable {
	name: string;
	os: NodeJS.Platform;
	arguments?: string;
}

interface DetectableApplication {
	name: string;
	aliases?: Array<string>;
	icon: string;
	executables: Array<DetectableExecutable>;
	presence_assets?: Record<string, string>;
}

interface ProcessSnapshot {
	pid?: number;
	name: string;
	path?: string;
	commandLine?: string;
}

const DETECTABLES = detectablesCatalog as Array<DetectableApplication>;

function normalizeComparable(value: string): string {
	return value.trim().replace(/^"|"$/g, '').replace(/\\/g, '/').toLowerCase();
}

function normalizeExecutableName(value: string): string {
	return path.basename(normalizeComparable(value));
}

function activityId(name: string): string {
	return name
		.toLowerCase()
		.replace(/[^a-z0-9]+/g, '-')
		.replace(/^-+|-+$/g, '');
}

function splitCsvLine(line: string): Array<string> {
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

function parseWindowsWmicCsv(stdout: string): Array<ProcessSnapshot> {
	const processes: Array<ProcessSnapshot> = [];
	for (const line of stdout.split(/\r?\n/)) {
		if (!line.trim() || line.startsWith('Node,')) continue;
		const [, commandLine, executablePath, name, pidRaw] = splitCsvLine(line);
		if (!name) continue;
		const pid = Number.parseInt(pidRaw ?? '', 10);
		processes.push({
			name,
			...(Number.isFinite(pid) ? {pid} : {}),
			...(executablePath ? {path: executablePath} : {}),
			...(commandLine ? {commandLine} : {}),
		});
		if (processes.length >= MAX_ACTIVITY_PROCESSES) break;
	}
	return processes;
}

function parseWindowsTasklistCsv(stdout: string): Array<ProcessSnapshot> {
	const processes: Array<ProcessSnapshot> = [];
	for (const line of stdout.split(/\r?\n/)) {
		if (!line.trim()) continue;
		const [imageName, pidRaw] = splitCsvLine(line);
		if (!imageName) continue;
		const pid = Number.parseInt(pidRaw ?? '', 10);
		processes.push({
			name: imageName,
			...(Number.isFinite(pid) ? {pid} : {}),
		});
		if (processes.length >= MAX_ACTIVITY_PROCESSES) break;
	}
	return processes;
}

function parsePosixPs(stdout: string): Array<ProcessSnapshot> {
	const processes: Array<ProcessSnapshot> = [];
	for (const line of stdout.split(/\r?\n/)) {
		if (!line.trim()) continue;
		const match = line.match(/^\s*(\d+)\s+(\S+)\s*(.*)$/);
		if (!match) continue;
		const [, pidRaw, command, args = ''] = match;
		const pid = Number.parseInt(pidRaw, 10);
		processes.push({
			...(Number.isFinite(pid) ? {pid} : {}),
			name: normalizeExecutableName(command ?? ''),
			path: command,
			commandLine: args,
		});
		if (processes.length >= MAX_ACTIVITY_PROCESSES) break;
	}
	return processes;
}

async function listWindowsProcesses(): Promise<Array<ProcessSnapshot>> {
	try {
		const {stdout} = await execFileAsync(
			'wmic',
			['process', 'get', 'CommandLine,ExecutablePath,Name,ProcessId', '/format:csv'],
			{windowsHide: true, maxBuffer: PROCESS_QUERY_MAX_BUFFER_BYTES},
		);
		return parseWindowsWmicCsv(stdout);
	} catch {
		const {stdout} = await execFileAsync('tasklist', ['/fo', 'csv', '/nh'], {
			windowsHide: true,
			maxBuffer: PROCESS_QUERY_MAX_BUFFER_BYTES,
		});
		return parseWindowsTasklistCsv(stdout);
	}
}

async function listPosixProcesses(): Promise<Array<ProcessSnapshot>> {
	const {stdout} = await execFileAsync('ps', ['-axo', 'pid=,comm=,args='], {
		maxBuffer: PROCESS_QUERY_MAX_BUFFER_BYTES,
	});
	return parsePosixPs(stdout);
}

async function listProcesses(): Promise<Array<ProcessSnapshot>> {
	return process.platform === 'win32' ? listWindowsProcesses() : listPosixProcesses();
}

function executableMatches(rule: DetectableExecutable, processSnapshot: ProcessSnapshot): boolean {
	if (rule.os !== process.platform) return false;
	const ruleName = normalizeComparable(rule.name.startsWith('>') ? rule.name.slice(1) : rule.name);
	const processName = normalizeExecutableName(processSnapshot.name);
	const processPath = normalizeComparable(processSnapshot.path ?? processSnapshot.name);
	const ruleMatchesPath = ruleName.includes('/')
		? processPath.endsWith(ruleName)
		: processName === ruleName || processPath.endsWith(`/${ruleName}`);
	if (!ruleMatchesPath) return false;
	if (!rule.arguments) return true;
	return normalizeComparable(processSnapshot.commandLine ?? '').includes(normalizeComparable(rule.arguments));
}

function matchProcess(processSnapshot: ProcessSnapshot): DetectedActivity | null {
	for (const app of DETECTABLES) {
		if (!app.executables.some((rule) => executableMatches(rule, processSnapshot))) continue;
		return {
			id: activityId(app.name),
			type: 'application',
			name: app.name,
			icon: app.icon,
			...(processSnapshot.pid !== undefined ? {pid: processSnapshot.pid} : {}),
			executable: normalizeExecutableName(processSnapshot.name),
		};
	}
	return null;
}

export async function getActivityDetectionStatus(): Promise<ActivityDetectionStatus> {
	try {
		const processes = await listProcesses();
		const activities: Array<DetectedActivity> = [];
		const seen = new Set<string>();
		for (const processSnapshot of processes) {
			const activity = matchProcess(processSnapshot);
			if (!activity || seen.has(activity.id)) continue;
			seen.add(activity.id);
			activities.push(activity);
			if (activities.length >= MAX_DETECTED_ACTIVITIES) break;
		}
		return {detected: activities.length > 0, activities};
	} catch {
		return {detected: false, activities: []};
	}
}
