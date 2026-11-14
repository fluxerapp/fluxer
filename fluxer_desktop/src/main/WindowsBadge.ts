// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {getDesktopWindowBehaviorSettings} from '@electron/common/DesktopConfig';
import {describeUnreadBadge, UNREAD_BADGE_DOT} from '@electron/main/UnreadBadge';
import type {BrowserWindow, NativeImage} from 'electron';
import {nativeImage, screen} from 'electron';

const BADGE_COUNT = 11;
const OVERLAY_BASE_SIZE = 16;
const ICON_DIRECTORY_ENTRY_BYTES = 16;
const ICON_HEADER_BYTES = 6;
const PNG_SIGNATURE = Buffer.from([0x89, 0x50, 0x4e, 0x47]);

type BadgeFrames = ReadonlyMap<number, Buffer>;

const badgeFrames: Array<BadgeFrames | null> = [];
const darkBadgeFrames: Array<BadgeFrames | null> = [];
const badgeImages = new Map<Buffer, NativeImage>();

let hasInit = false;
let lastIcon: NativeImage | null = null;
let lastDescription: string | null = null;
let lastCount: number | null = null;

function isSupported(): boolean {
	return process.platform === 'win32';
}

function readIconPngFrames(data: Buffer): BadgeFrames {
	const frames = new Map<number, Buffer>();
	if (data.length < ICON_HEADER_BYTES || data.readUInt16LE(2) !== 1) return frames;
	const count = data.readUInt16LE(4);
	for (let index = 0; index < count; index++) {
		const entry = ICON_HEADER_BYTES + index * ICON_DIRECTORY_ENTRY_BYTES;
		if (entry + ICON_DIRECTORY_ENTRY_BYTES > data.length) break;
		const size = data[entry] || 256;
		const length = data.readUInt32LE(entry + 8);
		const offset = data.readUInt32LE(entry + 12);
		const frame = data.subarray(offset, offset + length);
		if (frame.length === length && frame.subarray(0, PNG_SIGNATURE.length).equals(PNG_SIGNATURE)) {
			frames.set(size, frame);
		}
	}
	return frames;
}

function pickBadgeFrameSize(sizes: Iterable<number>, scaleFactor: number): number | null {
	const target = Math.round(OVERLAY_BASE_SIZE * scaleFactor);
	const sorted = [...sizes].sort((left, right) => left - right);
	return sorted.find((size) => size >= target) ?? sorted.at(-1) ?? null;
}

function loadBadgeFrames(badgeDir: string, name: string): BadgeFrames | null {
	try {
		const frames = readIconPngFrames(fs.readFileSync(path.join(badgeDir, name)));
		return frames.size > 0 ? frames : null;
	} catch {
		return null;
	}
}

function ensureInitialized(): void {
	if (hasInit || !isSupported()) {
		return;
	}
	hasInit = true;
	const badgeDir = path.join(process.resourcesPath, 'badges');
	for (let i = 1; i <= BADGE_COUNT; i++) {
		badgeFrames.push(loadBadgeFrames(badgeDir, `badge-${i}.ico`));
		darkBadgeFrames.push(loadBadgeFrames(badgeDir, `badge-dark-${i}.ico`));
	}
}

function windowScaleFactor(win: BrowserWindow): number {
	try {
		return screen.getDisplayMatching(win.getBounds()).scaleFactor;
	} catch {
		return 1;
	}
}

function getBadgeIcon(index: number, scaleFactor: number): NativeImage | null {
	const frames = (getDesktopWindowBehaviorSettings().darkTaskbarBadge ? darkBadgeFrames : badgeFrames)[index];
	if (!frames) return null;
	const size = pickBadgeFrameSize(frames.keys(), scaleFactor);
	const png = size == null ? undefined : frames.get(size);
	if (!png) return null;
	let image = badgeImages.get(png);
	if (!image) {
		image = nativeImage.createFromBuffer(png, {scaleFactor: 1});
		badgeImages.set(png, image);
	}
	return image.isEmpty() ? null : image;
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
	const icon = index == null ? null : getBadgeIcon(index, windowScaleFactor(win));
	if (force || lastIcon !== icon || lastDescription !== description) {
		win.setOverlayIcon(icon, description);
		lastIcon = icon;
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
