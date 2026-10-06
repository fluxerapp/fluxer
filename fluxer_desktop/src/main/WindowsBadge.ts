// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {getNativeLocale, t} from '@electron/main/MainI18n';
import type {BrowserWindow, NativeImage} from 'electron';
import {nativeImage} from 'electron';

const badgeIcons: Array<NativeImage | null> = [];

let hasInit = false;
let lastIndex: number | null = null;
let lastDescription: string | null = null;
let lastCount: number | null = null;

function isSupported(): boolean {
	return process.platform === 'win32';
}

function ensureInitialized(): void {
	if (hasInit || !isSupported()) {
		return;
	}
	hasInit = true;
	const badgeDir = path.join(process.resourcesPath, 'badges');
	for (let i = 1; i <= 11; i++) {
		const iconPath = path.join(badgeDir, `badge-${i}.ico`);
		if (!fs.existsSync(iconPath)) {
			badgeIcons.push(null);
			continue;
		}
		const icon = nativeImage.createFromPath(iconPath);
		badgeIcons.push(icon.isEmpty() ? null : icon);
	}
}

function formatBadgeCount(count: number): string {
	try {
		return count.toLocaleString(getNativeLocale());
	} catch {
		return String(count);
	}
}

function getOverlayIconData(count: number): {
	index: number | null;
	description: string;
} {
	if (count === -1) {
		return {
			index: 10,
			description: t('desktop.badge.unreadMessages'),
		};
	}
	if (count === 0) {
		return {
			index: null,
			description: 'No Notifications',
		};
	}
	const index = Math.max(1, Math.min(count, 10)) - 1;
	return {
		index,
		description: t('desktop.badge.unreadMessagesCount', {count: formatBadgeCount(count)}),
	};
}

function applyOverlay(win: BrowserWindow | null, count: number, force: boolean): void {
	if (!isSupported()) {
		return;
	}
	if (!win || win.isDestroyed()) {
		return;
	}
	const {index, description} = getOverlayIconData(count);
	if (force || lastIndex !== index || lastDescription !== description) {
		if (index == null) {
			win.setOverlayIcon(null, description);
		} else {
			const icon = badgeIcons[index];
			win.setOverlayIcon(icon ?? null, description);
		}
		lastIndex = index;
		lastDescription = description;
	}
	lastCount = count;
}

export function setWindowsBadgeOverlay(win: BrowserWindow | null, count: number): void {
	if (!isSupported()) {
		return;
	}
	ensureInitialized();
	applyOverlay(win, count, false);
}

export function refreshWindowsBadgeOverlay(win: BrowserWindow | null): void {
	if (!isSupported() || lastCount == null) {
		return;
	}
	ensureInitialized();
	applyOverlay(win, lastCount, true);
}
