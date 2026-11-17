// SPDX-License-Identifier: AGPL-3.0-or-later

import path from 'node:path';

const MAIN_GROUP = '[Desktop Entry]';
const KEY_LINE = /^([A-Za-z0-9-]+)\s*=\s*(.*)$/;
const ENV_ASSIGNMENT = /^[A-Za-z_][A-Za-z0-9_]*=/;

function isGroupHeader(trimmed: string): boolean {
	return trimmed.startsWith('[') && trimmed.endsWith(']');
}

function matchKey(trimmed: string, key: string): string | null {
	const match = KEY_LINE.exec(trimmed);
	return match !== null && match[1] === key ? match[2].trim() : null;
}

export function readDesktopEntryValue(contents: string, key: string): string | null {
	let inMainGroup = true;
	for (const line of contents.split('\n')) {
		const trimmed = line.trim();
		if (isGroupHeader(trimmed)) {
			inMainGroup = trimmed === MAIN_GROUP;
			continue;
		}
		if (!inMainGroup) continue;
		const value = matchKey(trimmed, key);
		if (value !== null) return value;
	}
	return null;
}

function hasDesktopEntryKeyPrefix(contents: string, prefix: string): boolean {
	let inMainGroup = true;
	for (const line of contents.split('\n')) {
		const trimmed = line.trim();
		if (isGroupHeader(trimmed)) {
			inMainGroup = trimmed === MAIN_GROUP;
			continue;
		}
		if (inMainGroup && trimmed.startsWith(prefix)) return true;
	}
	return false;
}

export function setDesktopEntryValue(contents: string, key: string, value: string): string {
	const lines = contents.split('\n');
	const groupStart = lines.findIndex((line) => line.trim() === MAIN_GROUP);
	if (groupStart === -1) return contents;
	let groupEnd = lines.length;
	let replaced = false;
	const kept: Array<string> = lines.slice(0, groupStart + 1);
	for (let index = groupStart + 1; index < lines.length; index++) {
		const trimmed = lines[index].trim();
		if (isGroupHeader(trimmed)) {
			groupEnd = index;
			break;
		}
		if (matchKey(trimmed, key) !== null) {
			if (!replaced) kept.push(`${key}=${value}`);
			replaced = true;
			continue;
		}
		kept.push(lines[index]);
	}
	if (!replaced) {
		let insertAt = kept.length;
		while (insertAt > groupStart + 1 && kept[insertAt - 1].trim() === '') insertAt--;
		kept.splice(insertAt, 0, `${key}=${value}`);
	}
	return [...kept, ...lines.slice(groupEnd)].join('\n');
}

function unescapeDesktopValue(value: string): string {
	return value.replace(/\\([sntr\\])/g, (_, code: string) => {
		switch (code) {
			case 's':
				return ' ';
			case 'n':
				return '\n';
			case 't':
				return '\t';
			case 'r':
				return '\r';
			default:
				return '\\';
		}
	});
}

function splitExecLine(exec: string): Array<string> {
	const tokens: Array<string> = [];
	let current = '';
	let started = false;
	let quote: '"' | "'" | null = null;
	for (let index = 0; index < exec.length; index++) {
		const char = exec[index];
		if (quote === '"') {
			if (char === '\\' && index + 1 < exec.length) {
				current += exec[++index];
			} else if (char === '"') {
				quote = null;
			} else {
				current += char;
			}
			continue;
		}
		if (quote === "'") {
			if (char === "'") quote = null;
			else current += char;
			continue;
		}
		if (char === '"' || char === "'") {
			quote = char;
			started = true;
		} else if (char === ' ' || char === '\t') {
			if (started) tokens.push(current);
			current = '';
			started = false;
		} else {
			current += char;
			started = true;
		}
	}
	if (started) tokens.push(current);
	return tokens;
}

function getDesktopExecProgram(exec: string): string | null {
	const tokens = splitExecLine(unescapeDesktopValue(exec));
	let index = 0;
	if (tokens[index] === 'env' || tokens[index] === '/usr/bin/env') {
		index++;
		while (index < tokens.length && ENV_ASSIGNMENT.test(tokens[index])) index++;
	}
	return tokens[index] ?? null;
}

export function desktopExecRunsPath(exec: string | null, launchPath: string): boolean {
	if (exec === null || launchPath.length === 0) return false;
	const program = getDesktopExecProgram(exec);
	if (program === null) return false;
	if (program === launchPath) return true;
	if (path.isAbsolute(program) && path.isAbsolute(launchPath)) return false;
	return path.basename(program) === path.basename(launchPath);
}

interface WindowIdentity {
	wmClasses: ReadonlyArray<string>;
	launchPath: string;
}

function isStockElectron(launchPath: string): boolean {
	return /^electron[0-9]*$/.test(path.basename(launchPath));
}

export function isWindowDesktopEntry(contents: string, identity: WindowIdentity): boolean {
	if (readDesktopEntryValue(contents, 'Hidden') === 'true') return false;
	const wmClass = readDesktopEntryValue(contents, 'StartupWMClass')?.toLowerCase();
	if (wmClass !== undefined && identity.wmClasses.some((candidate) => candidate.toLowerCase() === wmClass)) return true;
	if (isStockElectron(identity.launchPath)) return false;
	return desktopExecRunsPath(readDesktopEntryValue(contents, 'Exec'), identity.launchPath);
}

export function isAppImageIntegratorEntry(contents: string, appImagePath: string): boolean {
	return (
		path.isAbsolute(appImagePath) &&
		hasDesktopEntryKeyPrefix(contents, 'X-AppImage-') &&
		getDesktopExecProgram(readDesktopEntryValue(contents, 'Exec') ?? '') === appImagePath
	);
}
