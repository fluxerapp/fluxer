// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {act, createElement} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {beforeEach, describe, expect, test, vi} from 'vitest';

const preload = vi.hoisted(() => ({
	start: vi.fn(),
	stop: vi.fn(),
}));

vi.mock('@app/features/voice/utils/MediaDeviceStartupPreload', () => ({
	startMediaDeviceStartupPreload: () => {
		preload.start();
		return preload.stop;
	},
}));

const {MediaDeviceStartupPreloadManager} = await import(
	'@app/features/voice/components/MediaDeviceStartupPreloadManager'
);

(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;

let root: Root;

beforeEach(() => {
	vi.clearAllMocks();
	document.body.replaceChildren();
	const host = document.createElement('div');
	document.body.append(host);
	root = createRoot(host);
});

describe('MediaDeviceStartupPreloadManager', () => {
	test('starts the preload once loaded and stops it on unmount', async () => {
		act(() => {
			root.render(createElement(MediaDeviceStartupPreloadManager));
		});
		await vi.waitFor(() => expect(preload.start).toHaveBeenCalledTimes(1));
		expect(preload.stop).not.toHaveBeenCalled();
		act(() => {
			root.unmount();
		});
		expect(preload.stop).toHaveBeenCalledTimes(1);
	});

	test('never starts the preload when unmounted before it finishes loading', async () => {
		act(() => {
			root.render(createElement(MediaDeviceStartupPreloadManager));
		});
		act(() => {
			root.unmount();
		});
		await import('@app/features/voice/utils/MediaDeviceStartupPreload');
		await new Promise((resolve) => setTimeout(resolve, 0));
		expect(preload.start).not.toHaveBeenCalled();
		expect(preload.stop).not.toHaveBeenCalled();
	});
});
