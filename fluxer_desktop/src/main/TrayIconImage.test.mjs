// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';
import {BLUE_BGRA, discBitmap, FakeNativeImage, fakeNativeImageModule, pixel} from './TrayIconTestSupport.test.mjs';

installElectronStub({nativeImage: fakeNativeImageModule});

const {padTrayIconBitmap, paintMonochromeGlyph, paintTrayIndicatorDot, renderTrayIcon, resolveTrayIconAppearance} =
	await import('./TrayIconImage.ts');

describe('tray icon appearance', () => {
	test('macOS always uses the template image', () => {
		for (const coloredTrayIcon of [false, true]) {
			for (const darkSystemUI of [false, true]) {
				assert.equal(resolveTrayIconAppearance({platform: 'darwin', coloredTrayIcon, darkSystemUI}), 'template');
			}
		}
	});

	test('Windows follows the taskbar theme unless the colored icon is chosen', () => {
		assert.equal(resolveTrayIconAppearance({platform: 'win32', coloredTrayIcon: false, darkSystemUI: true}), 'white');
		assert.equal(resolveTrayIconAppearance({platform: 'win32', coloredTrayIcon: false, darkSystemUI: false}), 'black');
		assert.equal(resolveTrayIconAppearance({platform: 'win32', coloredTrayIcon: true, darkSystemUI: false}), 'colored');
	});

	test('Linux uses a white glyph whatever the system theme says', () => {
		assert.equal(resolveTrayIconAppearance({platform: 'linux', coloredTrayIcon: false, darkSystemUI: false}), 'white');
		assert.equal(resolveTrayIconAppearance({platform: 'linux', coloredTrayIcon: true, darkSystemUI: false}), 'colored');
	});
});

describe('monochrome glyph', () => {
	test('keeps the body, cuts out the light mark and leaves the outside transparent', () => {
		const bitmap = discBitmap(32);
		paintMonochromeGlyph(bitmap, [0xff, 0xff, 0xff]);
		assert.deepEqual(pixel(bitmap, 32, 16, 6), [0xff, 0xff, 0xff, 0xff]);
		assert.deepEqual(pixel(bitmap, 32, 16, 16), [0, 0, 0, 0]);
		assert.deepEqual(pixel(bitmap, 32, 0, 0), [0, 0, 0, 0]);
	});

	test('tints the glyph with premultiplied color', () => {
		const bitmap = discBitmap(32);
		paintMonochromeGlyph(bitmap, [0x1c, 0x1c, 0x1c]);
		assert.deepEqual(pixel(bitmap, 32, 16, 6), [0x1c, 0x1c, 0x1c, 0xff]);
	});

	test('an icon without a light mark keeps its whole silhouette', () => {
		const bitmap = Buffer.alloc(4 * 4 * 4);
		for (let offset = 0; offset < bitmap.length; offset += 4) bitmap.set(BLUE_BGRA, offset);
		bitmap.set([0, 0, 0, 0], 0);
		paintMonochromeGlyph(bitmap, [0xff, 0xff, 0xff]);
		assert.deepEqual(pixel(bitmap, 4, 0, 0), [0, 0, 0, 0]);
		assert.deepEqual(pixel(bitmap, 4, 2, 2), [0xff, 0xff, 0xff, 0xff]);
	});
});

describe('unread dot', () => {
	test('draws an opaque dot in the top-right corner with a transparent gap around it', () => {
		const size = 64;
		const bitmap = Buffer.alloc(size * size * 4, 0xff);
		paintTrayIndicatorDot(bitmap, size, size, [0x00, 0x00, 0xdd]);
		const radius = size * 0.22;
		assert.deepEqual(pixel(bitmap, size, Math.floor(size - radius), Math.floor(radius)), [0x00, 0x00, 0xdd, 0xff]);
		assert.deepEqual(pixel(bitmap, size, Math.floor(size - radius), Math.floor(radius * 2 + 2)), [0, 0, 0, 0]);
		assert.deepEqual(pixel(bitmap, size, 4, size - 4), [0xff, 0xff, 0xff, 0xff]);
	});
});

describe('padTrayIconBitmap', () => {
	test('centers the bitmap in a transparent 22:16 canvas', () => {
		const padded = padTrayIconBitmap(Buffer.alloc(16 * 16 * 4, 0xff), 16, 16);
		assert.equal(padded.width, 22);
		assert.equal(padded.height, 22);
		assert.deepEqual(pixel(padded.buffer, 22, 2, 2), [0, 0, 0, 0]);
		assert.deepEqual(pixel(padded.buffer, 22, 3, 3), [0xff, 0xff, 0xff, 0xff]);
		assert.deepEqual(pixel(padded.buffer, 22, 18, 18), [0xff, 0xff, 0xff, 0xff]);
		assert.deepEqual(pixel(padded.buffer, 22, 19, 19), [0, 0, 0, 0]);
	});
});

describe('renderTrayIcon', () => {
	test('returns the base image untouched for the colored icon without a dot', () => {
		const base = FakeNativeImage.fromBitmap(16);
		assert.equal(renderTrayIcon(base, 'colored', false, false), base);
		const template = FakeNativeImage.fromBitmap(16);
		template.setTemplateImage(true);
		assert.equal(renderTrayIcon(template, 'template', false, false), template);
	});

	test('paints every scale factor and keeps the template flag', () => {
		const base = FakeNativeImage.fromBitmap(16);
		base.addRepresentation({scaleFactor: 2, width: 32, height: 32, buffer: discBitmap(32)});
		base.setTemplateImage(true);
		const result = renderTrayIcon(base, 'template', true, false);
		assert.notEqual(result, base);
		assert.deepEqual(result.getScaleFactors(), [1, 2]);
		assert.equal(result.isTemplateImage(), true);
		assert.deepEqual(pixel(result.toBitmap({scaleFactor: 2}), 32, 25, 7), [0, 0, 0, 0xff]);
		assert.deepEqual(pixel(base.toBitmap({scaleFactor: 2}), 32, 25, 7), BLUE_BGRA, 'the base image is not mutated');
	});

	test('combines the monochrome glyph with a red dot', () => {
		const result = renderTrayIcon(FakeNativeImage.fromBitmap(32), 'white', true, false);
		assert.equal(result.isTemplateImage(), false);
		const bitmap = result.toBitmap();
		assert.deepEqual(pixel(bitmap, 32, 25, 7), [0x00, 0x00, 0xdd, 0xff]);
		assert.deepEqual(pixel(bitmap, 32, 6, 16), [0xff, 0xff, 0xff, 0xff]);
	});

	test('pads the glyph to the 16px-in-22px tray grid at every scale', () => {
		const base = FakeNativeImage.fromBitmap(16);
		base.addRepresentation({scaleFactor: 2, width: 32, height: 32, buffer: discBitmap(32)});
		const result = renderTrayIcon(base, 'colored', false, true);
		assert.notEqual(result, base);
		assert.deepEqual(result.getSize(1), {width: 22, height: 22});
		assert.deepEqual(result.getSize(2), {width: 22, height: 22});
		const bitmap = result.toBitmap();
		assert.deepEqual(pixel(bitmap, 22, 11, 1), [0, 0, 0, 0]);
		assert.deepEqual(pixel(bitmap, 22, 11, 4), BLUE_BGRA);
		assert.deepEqual(pixel(result.toBitmap({scaleFactor: 2}), 44, 22, 7), BLUE_BGRA);
		assert.deepEqual(pixel(result.toBitmap({scaleFactor: 2}), 44, 22, 5), [0, 0, 0, 0]);
	});

	test('keeps the dot on the glyph corner when padded', () => {
		const bitmap = renderTrayIcon(FakeNativeImage.fromBitmap(32), 'white', true, true).toBitmap();
		assert.deepEqual(pixel(bitmap, 44, 31, 13), [0x00, 0x00, 0xdd, 0xff]);
		assert.deepEqual(pixel(bitmap, 44, 42, 1), [0, 0, 0, 0]);
	});

	test('skips representations whose bitmap does not match their size', () => {
		const base = FakeNativeImage.fromBitmap(16);
		base.toBitmap = () => Buffer.alloc(3);
		assert.equal(renderTrayIcon(base, 'white', true, false), base);
	});
});
