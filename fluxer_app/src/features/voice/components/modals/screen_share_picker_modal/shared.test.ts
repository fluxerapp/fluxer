// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	findNativeCaptureSourceForDesktopSource,
	getDesktopSourceThumbnailStateKey,
	hasDesktopSourcesMissingThumbnails,
	isDisplaySource,
	mergeDesktopSources,
} from '@app/features/voice/components/modals/screen_share_picker_modal/shared';
import type {DesktopSource, NativeScreenCaptureSource} from '@app/types/electron.d';
import {describe, expect, test} from 'vitest';

const USABLE_IMAGE_DATA_URL = 'data:image/png;base64,aW1hZ2U=';

function desktopSource(overrides: Partial<DesktopSource>): DesktopSource {
	return {
		id: 'window:100:0',
		name: 'App',
		...overrides,
	};
}

function nativeSource(overrides: Partial<NativeScreenCaptureSource>): NativeScreenCaptureSource {
	return {
		kind: 'window',
		id: '100',
		name: 'App',
		width: 1280,
		height: 720,
		...overrides,
	};
}

describe('findNativeCaptureSourceForDesktopSource', () => {
	test('matches the selected desktopCapturer window by native window id', () => {
		const selected = desktopSource({id: 'window:5050:0', name: 'Browser'});
		const nativeSources = [
			nativeSource({id: '4040', name: 'Control Centre'}),
			nativeSource({id: '5050', name: 'Browser'}),
		];
		expect(findNativeCaptureSourceForDesktopSource(selected, nativeSources)?.name).toBe('Browser');
	});
	test('matches displays by display_id instead of the sequential desktop source token', () => {
		const selected = desktopSource({
			id: 'screen:0:0',
			name: 'Entire Screen',
			display_id: '69733248',
		});
		const nativeSources = [
			nativeSource({kind: 'screen', id: '123', name: 'Display 1'}),
			nativeSource({kind: 'screen', id: '69733248', name: 'Display 2'}),
		];
		expect(findNativeCaptureSourceForDesktopSource(selected, nativeSources)?.name).toBe('Display 2');
	});
	test('does not match a desktop display card to a native game capture source', () => {
		const selected = desktopSource({
			id: 'screen:0:0',
			name: 'Primary monitor',
			display_id: '69733248',
		});
		const nativeSources = [
			nativeSource({kind: 'game', id: 'screen:0:0', name: 'Fullscreen game on Primary monitor'}),
			nativeSource({kind: 'window', id: 'window:5050:0', name: 'Browser'}),
		];
		expect(findNativeCaptureSourceForDesktopSource(selected, nativeSources)).toBeUndefined();
	});
	test('normalizes numeric native ids without matching unrelated native sources', () => {
		const selected = desktopSource({id: 'window:0x10:0', name: 'Terminal'});
		const nativeSources = [nativeSource({id: '15', name: 'System UI'}), nativeSource({id: '16', name: 'Terminal'})];
		expect(findNativeCaptureSourceForDesktopSource(selected, nativeSources)?.name).toBe('Terminal');
	});
	test('falls back to a unique native window name when desktopCapturer uses a different token', () => {
		const selected = desktopSource({id: 'window:123:0', name: 'Project Notes'});
		const nativeSources = [
			nativeSource({id: '9001', name: 'Browser - Project Notes', appName: 'Browser'}),
			nativeSource({id: '9002', name: 'Music', appName: 'Music'}),
		];
		expect(findNativeCaptureSourceForDesktopSource(selected, nativeSources)?.id).toBe('9001');
	});
	test('does not use a name fallback when multiple native windows match', () => {
		const selected = desktopSource({id: 'window:123:0', name: 'Project Notes'});
		const nativeSources = [
			nativeSource({id: '9001', name: 'Browser - Project Notes', appName: 'Browser'}),
			nativeSource({id: '9002', name: 'Editor - Project Notes', appName: 'Editor'}),
		];
		expect(findNativeCaptureSourceForDesktopSource(selected, nativeSources)).toBeUndefined();
	});
	test('falls back to a native display when display ids differ but dimensions line up', () => {
		const selected = desktopSource({
			id: 'screen:0:0',
			name: 'Built-in Retina Display',
			display_id: '42',
			nativeWidth: 3024,
			nativeHeight: 1964,
		});
		const nativeSources = [
			nativeSource({kind: 'screen', id: '9001', name: 'Built-in Retina Display', width: 3024, height: 1964}),
			nativeSource({kind: 'screen', id: '9002', name: 'Studio Display', width: 5120, height: 2880}),
		];
		expect(findNativeCaptureSourceForDesktopSource(selected, nativeSources)?.id).toBe('9001');
	});
	test('falls back to the native display ordinal when display ids differ', () => {
		const selected = desktopSource({
			id: 'screen:1:0',
			name: 'Screen 2',
			display_id: '42',
			nativeWidth: 1920,
			nativeHeight: 1080,
		});
		const nativeSources = [
			nativeSource({kind: 'screen', id: '9001', name: 'Display 1', width: 1920, height: 1080}),
			nativeSource({kind: 'screen', id: '9002', name: 'Display 2', width: 1920, height: 1080}),
		];
		expect(findNativeCaptureSourceForDesktopSource(selected, nativeSources)?.id).toBe('9002');
	});
	test('does not fall back to a native game capture display ordinal when display ids differ', () => {
		const selected = desktopSource({
			id: 'screen:1:0',
			name: 'Screen 2',
			display_id: '42',
			nativeWidth: 1920,
			nativeHeight: 1080,
		});
		const nativeSources = [
			nativeSource({kind: 'game', id: 'screen:0:0', name: 'Game Capture Display 1', width: 1920, height: 1080}),
			nativeSource({kind: 'game', id: 'screen:1:0', name: 'Game Capture Display 2', width: 1920, height: 1080}),
		];
		expect(findNativeCaptureSourceForDesktopSource(selected, nativeSources)).toBeUndefined();
	});
	test('falls back to a unique native display by dimensions when ids and names differ', () => {
		const selected = desktopSource({
			id: 'screen:0:0',
			name: 'Entire Screen',
			display_id: '42',
			nativeWidth: 2560,
			nativeHeight: 1440,
		});
		const nativeSources = [
			nativeSource({kind: 'screen', id: '9001', name: 'Display 1', width: 1920, height: 1080}),
			nativeSource({kind: 'screen', id: '9002', name: 'Display 2', width: 2560, height: 1440}),
		];
		expect(findNativeCaptureSourceForDesktopSource(selected, nativeSources)?.id).toBe('9002');
	});
	test('does not use a name fallback when native dimensions disagree', () => {
		const selected = desktopSource({
			id: 'window:123:0',
			name: 'Project Notes',
			nativeWidth: 1280,
			nativeHeight: 720,
		});
		const nativeSources = [
			nativeSource({id: '9001', name: 'Browser - Project Notes', appName: 'Browser', width: 1440, height: 900}),
		];
		expect(findNativeCaptureSourceForDesktopSource(selected, nativeSources)).toBeUndefined();
	});
	test('returns undefined when the native source list has no exact counterpart', () => {
		const selected = desktopSource({id: 'window:777:0', name: 'Editor'});
		const nativeSources = [nativeSource({id: '1', name: 'Menubar'}), nativeSource({id: '2', name: 'Control Centre'})];
		expect(findNativeCaptureSourceForDesktopSource(selected, nativeSources)).toBeUndefined();
	});
});

describe('desktop source thumbnails', () => {
	test('detects display cards missing preview thumbnails', () => {
		const sources = [
			desktopSource({id: 'window:100:0', thumbnailDataUrl: undefined}),
			desktopSource({id: 'screen:0:0', thumbnailDataUrl: undefined}),
			desktopSource({id: 'screen:1:0', thumbnailDataUrl: USABLE_IMAGE_DATA_URL}),
		];
		expect(hasDesktopSourcesMissingThumbnails(sources, isDisplaySource)).toBe(true);
		expect(getDesktopSourceThumbnailStateKey(sources, isDisplaySource)).toBe('screen:0:0:missing|screen:1:0:thumbnail');
	});

	test('does not flag display cards after each display has a usable thumbnail', () => {
		const sources = [
			desktopSource({id: 'screen:0:0', thumbnailDataUrl: USABLE_IMAGE_DATA_URL}),
			desktopSource({id: 'screen:1:0', thumbnailDataUrl: USABLE_IMAGE_DATA_URL}),
		];
		expect(hasDesktopSourcesMissingThumbnails(sources, isDisplaySource)).toBe(false);
		expect(getDesktopSourceThumbnailStateKey(sources, isDisplaySource)).toBe(
			'screen:0:0:thumbnail|screen:1:0:thumbnail',
		);
	});

	test('preserves thumbnails when merging list-only source refreshes', () => {
		const previous = [
			desktopSource({
				id: 'screen:0:0',
				name: 'Entire Screen',
				thumbnailDataUrl: USABLE_IMAGE_DATA_URL,
			}),
		];
		const next = [
			desktopSource({
				id: 'screen:0:0',
				name: 'Entire Screen',
				thumbnailDataUrl: undefined,
			}),
		];
		expect(mergeDesktopSources(previous, next)[0]?.thumbnailDataUrl).toBe(USABLE_IMAGE_DATA_URL);
	});
});
