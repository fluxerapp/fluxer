// SPDX-License-Identifier: AGPL-3.0-or-later

import {execFile} from 'node:child_process';
import path from 'node:path';
import {promisify} from 'node:util';
import type {ActivityDetectionProcess, ActivityDetectionStatus, DetectedActivity} from '@electron/common/Types';
import detectablesCatalog from './DetectablesCatalog.json';

const execFileAsync = promisify(execFile);
const PROCESS_QUERY_MAX_BUFFER_BYTES = 2 * 1024 * 1024;
const MAX_DETECTED_ACTIVITIES = 8;
const MAX_ACTIVITY_PROCESSES = 4;

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

interface ActivityProcessSnapshot extends ActivityDetectionProcess {
	path?: string | null;
	arguments?: string | null;
}

function normalizeProcessName(value: string): string {
	const normalized = value.trim().replace(/^"|"$/g, '');
	return path.basename(normalized).toLowerCase();
}

function normalizeProcessPath(value: string | null | undefined): string {
	return (value ?? '').trim().replace(/\\/g, '/').replace(/^"|"$/g, '').toLowerCase();
}

function normalizeArguments(value: string | null | undefined): string {
	return (value ?? '').toLowerCase();
}

function executableMatchesProcess(rule: DetectableExecutable, processSnapshot: ActivityProcessSnapshot): boolean {
	if (rule.os !== process.platform) return false;
	const rawRuleName = rule.name.trim().toLowerCase();
	const requiresArguments = rawRuleName.startsWith('>');
	const ruleName = requiresArguments ? rawRuleName.slice(1) : rawRuleName;
	if (!ruleName) return false;
	if (requiresArguments && !rule.arguments) return false;
	if (rule.arguments && !normalizeArguments(processSnapshot.arguments).includes(rule.arguments.toLowerCase())) {
		return false;
	}
	const processName = normalizeProcessName(processSnapshot.name);
	const processPath = normalizeProcessPath(processSnapshot.path ?? processSnapshot.name);
	if (ruleName.includes('/')) {
		return processPath.endsWith(ruleName);
	}
	return processName === ruleName;
}

function getProcessKey(processSnapshot: ActivityProcessSnapshot): string {
	return processSnapshot.pid == null ? `name:${processSnapshot.name}` : `pid:${processSnapshot.pid}`;
}

function detectActivities(
	catalog: ReadonlyArray<DetectableApplication>,
	processes: ReadonlyArray<ActivityProcessSnapshot>,
): Array<DetectedActivity> {
	const activities: Array<DetectedActivity> = [];
	for (const application of catalog) {
		if (activities.length >= MAX_DETECTED_ACTIVITIES) break;
		const matchedProcesses: Array<ActivityDetectionProcess> = [];
		const seenProcessKeys = new Set<string>();
		for (const processSnapshot of processes) {
			if (matchedProcesses.length >= MAX_ACTIVITY_PROCESSES) break;
			if (!application.executables.some((rule) => executableMatchesProcess(rule, processSnapshot))) continue;
			const key = getProcessKey(processSnapshot);
			if (seenProcessKeys.has(key)) continue;
			seenProcessKeys.add(key);
			matchedProcesses.push({
				name: normalizeProcessName(processSnapshot.name),
				...(processSnapshot.pid != null ? {pid: processSnapshot.pid} : {}),
			});
		}
		if (matchedProcesses.length === 0) continue;
		activities.push({
			name: application.name,
			...(application.aliases ? {aliases: application.aliases} : {}),
			icon: application.icon,
			...(application.presence_assets ? {presenceAssets: application.presence_assets} : {}),
			processes: matchedProcesses,
		});
	}
	return activities;
}

function parsePosixProcesses(stdout: string): Array<ActivityProcessSnapshot> {
	const processes: Array<ActivityProcessSnapshot> = [];
	for (const line of stdout.split(/\r?\n/)) {
		const match = line.match(/^\s*(\d+)\s+(.+?)\s{2,}(.*)$/);
		if (!match) continue;
		const [, pidRaw, command, args = ''] = match;
		const pid = Number.parseInt(pidRaw ?? '', 10);
		if (!command) continue;
		processes.push({
			name: command,
			...(Number.isFinite(pid) ? {pid} : {}),
			path: command,
			arguments: args,
		});
	}
	return processes;
}

function parseWindowsProcessJson(stdout: string): Array<ActivityProcessSnapshot> {
	const trimmed = stdout.trim();
	if (!trimmed) return [];
	let parsed: unknown;
	try {
		parsed = JSON.parse(trimmed);
	} catch {
		return [];
	}
	const rows = Array.isArray(parsed) ? parsed : [parsed];
	const processes: Array<ActivityProcessSnapshot> = [];
	for (const row of rows) {
		if (!row || typeof row !== 'object') continue;
		const record = row as Record<string, unknown>;
		const name = typeof record.Name === 'string' ? record.Name : null;
		if (!name) continue;
		const pid = typeof record.ProcessId === 'number' ? record.ProcessId : Number.parseInt(String(record.ProcessId), 10);
		const executablePath = typeof record.ExecutablePath === 'string' ? record.ExecutablePath : null;
		const commandLine = typeof record.CommandLine === 'string' ? record.CommandLine : null;
		processes.push({
			name,
			...(Number.isFinite(pid) ? {pid} : {}),
			path: executablePath,
			arguments: commandLine,
		});
	}
	return processes;
}

async function getRunningProcesses(): Promise<Array<ActivityProcessSnapshot>> {
	if (process.platform === 'win32') {
		const {stdout} = await execFileAsync(
			'powershell.exe',
			[
				'-NoProfile',
				'-Command',
				'Get-CimInstance Win32_Process | Select-Object ProcessId,Name,ExecutablePath,CommandLine | ConvertTo-Json -Compress',
			],
			{windowsHide: true, maxBuffer: PROCESS_QUERY_MAX_BUFFER_BYTES},
		);
		return parseWindowsProcessJson(stdout);
	}
	const {stdout} = await execFileAsync('ps', ['-axo', 'pid=,comm=,args='], {
		maxBuffer: PROCESS_QUERY_MAX_BUFFER_BYTES,
	});
	return parsePosixProcesses(stdout);
}

export async function getDetectedActivities(): Promise<ActivityDetectionStatus> {
	let activities: Array<DetectedActivity> = [];
	try {
		activities = detectActivities(detectablesCatalog as Array<DetectableApplication>, await getRunningProcesses());
	} catch {
		activities = [];
	}
	return {
		detected: activities.length > 0,
		activities,
	};
}

export const __activityDetectionTest = {
	detectActivities,
	executableMatchesProcess,
	parsePosixProcesses,
	parseWindowsProcessJson,
};
