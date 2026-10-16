// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import {LINUX_DESKTOP_ENTRY_ID} from '@electron/common/DesktopIdentity';

const FLATPAK_INFO_PATH = '/.flatpak-info';

export function isFlatpakRuntime(): boolean {
	if (process.platform !== 'linux') return false;
	try {
		return fs.existsSync(FLATPAK_INFO_PATH);
	} catch {
		return false;
	}
}

export function parseFlatpakInfoAppId(contents: string): string | null {
	let inApplication = false;
	for (const rawLine of contents.split('\n')) {
		const line = rawLine.trim();
		if (line.startsWith('[')) {
			inApplication = line === '[Application]';
			continue;
		}
		if (!inApplication || !line.startsWith('name=')) continue;
		const name = line.slice('name='.length).trim();
		return name.length > 0 ? name : null;
	}
	return null;
}

export function getFlatpakAppId(): string | null {
	if (!isFlatpakRuntime()) return null;
	const envId = process.env.FLATPAK_ID?.trim();
	if (envId && envId.length > 0) return envId;
	try {
		return parseFlatpakInfoAppId(fs.readFileSync(FLATPAK_INFO_PATH, 'utf8'));
	} catch {
		return null;
	}
}

export function getLinuxDesktopId(): string {
	return getFlatpakAppId() ?? LINUX_DESKTOP_ENTRY_ID;
}
