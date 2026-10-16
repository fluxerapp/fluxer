// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import {installElectronStub, installTestModuleStub} from './LocalAppTestSupport.test.mjs';

const BUILD_RESOURCES = path.join(path.dirname(fileURLToPath(import.meta.url)), '..', '..', 'build_resources');
const CHANNEL_ICON_DIRECTORIES = ['icons-stable', 'icons-canary', 'icons-development'];
const BADGE_NAMES = Array.from({length: 11}, (_, offset) => [
	`badge-${offset + 1}.ico`,
	`badge-dark-${offset + 1}.ico`,
]).flat();

const resourcesPath = fs.mkdtempSync(path.join(os.tmpdir(), 'fluxer-desktop-windows-badge-test-'));
fs.mkdirSync(path.join(resourcesPath, 'badges'));
for (const name of BADGE_NAMES) {
	fs.copyFileSync(path.join(BUILD_RESOURCES, 'icons-stable', 'badges', name), path.join(resourcesPath, 'badges', name));
}
Object.defineProperty(process, 'resourcesPath', {value: resourcesPath, configurable: true});
Object.defineProperty(process, 'platform', {value: 'win32', configurable: true});

const settings = {darkTaskbarBadge: false};
globalThis.__fluxerDesktopWindowsBadgeTestSettings__ = settings;

installTestModuleStub(
	'@electron/common/DesktopConfig',
	'export const getDesktopWindowBehaviorSettings = () => ({...globalThis.__fluxerDesktopWindowsBadgeTestSettings__});',
);

const display = {scaleFactor: 1};

installElectronStub({
	nativeImage: {
		createFromBuffer: (png) => ({png, isEmpty: () => false}),
	},
	screen: {
		getDisplayMatching: () => display,
	},
});

const {pickBadgeFrameSize, readIconPngFrames, refreshWindowsBadgeOverlay, setWindowsBadgeOverlay} = await import(
	'./WindowsBadge.ts'
);

const framesByName = new Map(
	BADGE_NAMES.map((name) => [name, readIconPngFrames(fs.readFileSync(path.join(resourcesPath, 'badges', name)))]),
);

function frameOf(png) {
	for (const [name, frames] of framesByName) {
		for (const [size, frame] of frames) {
			if (frame === png || frame.equals(png)) return `${name}@${size}`;
		}
	}
	return null;
}

function createWindow() {
	return {
		overlays: [],
		isDestroyed: () => false,
		getBounds: () => ({x: 0, y: 0, width: 800, height: 600}),
		setOverlayIcon(icon, description) {
			this.overlays.push({name: icon ? frameOf(icon.png) : null, description});
		},
	};
}

function readIconFrames(file) {
	const data = fs.readFileSync(file);
	assert.equal(data.readUInt16LE(2), 1);
	return Array.from({length: data.readUInt16LE(4)}, (_, index) => {
		const entry = 6 + index * 16;
		const offset = data.readUInt32LE(entry + 12);
		return {
			size: data[entry],
			bitsPerPixel: data.readUInt16LE(entry + 6),
			png: data.subarray(offset, offset + 4).toString('latin1') === '\x89PNG',
		};
	});
}

describe('windows taskbar badge overlay', () => {
	test('redraws in the chosen color as soon as the setting changes', () => {
		const win = createWindow();
		refreshWindowsBadgeOverlay(win);
		assert.equal(win.overlays.length, 0, 'nothing to refresh before the first count');

		setWindowsBadgeOverlay(win, 3);
		assert.equal(win.overlays.at(-1).name, 'badge-3.ico@16');
		assert.match(win.overlays.at(-1).description, /3/);
		setWindowsBadgeOverlay(win, 3);
		assert.equal(win.overlays.length, 1);

		settings.darkTaskbarBadge = true;
		refreshWindowsBadgeOverlay(win);
		assert.equal(win.overlays.at(-1).name, 'badge-dark-3.ico@16');
		setWindowsBadgeOverlay(win, 3);
		assert.equal(win.overlays.length, 2, 'the same dark icon is not set twice');

		settings.darkTaskbarBadge = false;
		setWindowsBadgeOverlay(win, 3);
		assert.equal(win.overlays.length, 3, 'a color switch alone redraws the same count');
		assert.equal(win.overlays.at(-1).name, 'badge-3.ico@16');

		settings.darkTaskbarBadge = true;
		setWindowsBadgeOverlay(win, 25);
		assert.equal(win.overlays.at(-1).name, 'badge-dark-10.ico@16');
		setWindowsBadgeOverlay(win, -1);
		assert.equal(win.overlays.at(-1).name, 'badge-dark-11.ico@16');

		display.scaleFactor = 1.5;
		refreshWindowsBadgeOverlay(win);
		assert.equal(win.overlays.at(-1).name, 'badge-dark-11.ico@24', 'the frame follows the display scale');
		display.scaleFactor = 1;

		setWindowsBadgeOverlay(win, 0);
		assert.equal(win.overlays.at(-1).name, null);
		assert.equal(win.overlays.at(-1).description, 'No Notifications');
	});
});

describe('badge frame selection', () => {
	test('uses the exact frame for each display scale and never upscales when one exists', () => {
		const sizes = [16, 20, 24, 32, 40, 48];
		assert.equal(pickBadgeFrameSize(sizes, 1), 16);
		assert.equal(pickBadgeFrameSize(sizes, 1.25), 20);
		assert.equal(pickBadgeFrameSize(sizes, 1.5), 24);
		assert.equal(pickBadgeFrameSize(sizes, 1.75), 32);
		assert.equal(pickBadgeFrameSize(sizes, 2), 32);
		assert.equal(pickBadgeFrameSize(sizes, 3), 48);
		assert.equal(pickBadgeFrameSize(sizes, 4), 48);
		assert.equal(pickBadgeFrameSize([], 1), null);
	});

	test('ignores frames that are not PNG and malformed icon data', () => {
		assert.equal(readIconPngFrames(Buffer.alloc(0)).size, 0);
		assert.equal(readIconPngFrames(Buffer.from([0, 0, 2, 0, 1, 0])).size, 0);
		const truncated = fs.readFileSync(path.join(resourcesPath, 'badges', 'badge-3.ico')).subarray(0, 200);
		assert.ok(readIconPngFrames(truncated).size < 6);
	});
});

describe('bundled taskbar badge icons', () => {
	test('every channel ships the same red and dark PNG badges for every display scale', () => {
		const [reference, ...others] = CHANNEL_ICON_DIRECTORIES.map((directory) =>
			path.join(BUILD_RESOURCES, directory, 'badges'),
		);
		for (const name of BADGE_NAMES) {
			const frames = readIconFrames(path.join(reference, name));
			assert.deepEqual(
				frames.map(({size, bitsPerPixel, png}) => [size, bitsPerPixel, png]),
				[16, 20, 24, 32, 40, 48].map((size) => [size, 32, true]),
				name,
			);
			for (const directory of others) {
				assert.ok(
					fs.readFileSync(path.join(directory, name)).equals(fs.readFileSync(path.join(reference, name))),
					name,
				);
			}
		}
	});
});
