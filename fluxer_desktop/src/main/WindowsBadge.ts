// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {describeUnreadBadge, UNREAD_BADGE_DOT} from '@electron/main/UnreadBadge';
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

function getOverlayIconData(count: number): {
	index: number | null;
	description: string;
} {
	const description = describeUnreadBadge(count) ?? 'No Notifications';
	if (count === UNREAD_BADGE_DOT) {
		return {index: 10, description};
	}
	if (count <= 0) {
		return {index: null, description};
	}
	return {index: Math.min(count, 10) - 1, description};
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
