// SPDX-License-Identifier: AGPL-3.0-or-later

export function formatReleaseChannelLabel(value: string): string {
	const normalized = value.trim().toLowerCase();
	return normalized.charAt(0).toUpperCase() + normalized.slice(1);
}

export const ARCHITECTURE_PATTERNS: ReadonlyArray<{
	pattern: RegExp;
	label: string;
}> = [
	{pattern: /\barm64\b|\baarch64\b|\barmv8\b|\barm64e\b/i, label: 'arm64'},
	{pattern: /\barm\b|\barmv7\b|\barmv6\b/i, label: 'arm'},
	{pattern: /MacIntel/i, label: 'x64'},
	{pattern: /\bx86_64\b|\bx64\b|\bamd64\b|\bwin64\b|\bwow64\b/i, label: 'x64'},
	{pattern: /\bx86\b|\bi[3-6]86\b/i, label: 'x86'},
];

export function normalizeArchitectureValue(value: string | null | undefined): string | undefined {
	if (!value) {
		return undefined;
	}
	const trimmed = value.trim();
	for (const entry of ARCHITECTURE_PATTERNS) {
		if (entry.pattern.test(trimmed)) {
			return entry.label;
		}
	}
	return trimmed || undefined;
}

function getWindowsVersionName(osVersion: string): string {
	const parts = osVersion.split('.');
	const majorVersion = parseInt(parts[0], 10);
	const buildNumber = parseInt(parts[2], 10);
	if (majorVersion === 10) {
		if (buildNumber >= 22000) {
			return 'Windows 11';
		}
		return 'Windows 10';
	}
	return 'Windows';
}

export function formatDesktopOsName(desktopInfo: {os: string; osVersion?: string}): string {
	switch (desktopInfo.os) {
		case 'darwin':
			return 'macOS';
		case 'win32':
			return desktopInfo.osVersion ? getWindowsVersionName(desktopInfo.osVersion) : 'Windows';
		case 'linux':
			return 'Linux';
		default:
			return desktopInfo.os;
	}
}
