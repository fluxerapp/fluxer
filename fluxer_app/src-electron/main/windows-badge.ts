/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import fs from 'node:fs';
import path from 'node:path';
import type {BrowserWindow, NativeImage} from 'electron';
import {nativeImage} from 'electron';

const badgeIcons: Array<NativeImage | null> = [];
let hasInit = false;
let lastIndex: number | null = null;
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

function getOverlayIconData(count: number): {index: number | null; description: string} {
	if (count === -1) {
		return {
			index: 10,
			description: 'Unread messages',
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
		description: `${index} notifications`,
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
	if (force || lastIndex !== index) {
		if (index == null) {
			win.setOverlayIcon(null, description);
		} else {
			const icon = badgeIcons[index];
			win.setOverlayIcon(icon ?? null, description);
		}
		lastIndex = index;
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
