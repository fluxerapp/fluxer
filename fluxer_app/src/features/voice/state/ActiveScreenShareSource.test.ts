// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, describe, expect, it} from 'vitest';
import ActiveScreenShareSource from './ActiveScreenShareSource';

describe('ActiveScreenShareSource', () => {
	afterEach(() => {
		ActiveScreenShareSource.clear();
	});

	it('tracks and clears whether the selected source is a Fluxer-owned window', () => {
		ActiveScreenShareSource.setPublishedSource('app', 'window:42:0', {isOwnWindow: true});
		expect(ActiveScreenShareSource.getSourceId()).toBe('window:42:0');
		expect(ActiveScreenShareSource.isOwnWindow()).toBe(true);
		expect(ActiveScreenShareSource.getShareContext()).toBe('app');

		ActiveScreenShareSource.setPublishedSource('display', 'screen:1:0');
		expect(ActiveScreenShareSource.getSourceId()).toBe('screen:1:0');
		expect(ActiveScreenShareSource.isOwnWindow()).toBe(false);
		expect(ActiveScreenShareSource.getShareContext()).toBe('display');

		ActiveScreenShareSource.clear();
		expect(ActiveScreenShareSource.getSourceId()).toBeNull();
		expect(ActiveScreenShareSource.isOwnWindow()).toBe(false);
		expect(ActiveScreenShareSource.getShareContext()).toBeNull();
	});

	it('keeps a scope picked before the share starts out of the running share until it is committed', () => {
		expect(ActiveScreenShareSource.getPendingWindowAudioScope()).toBe('window');

		ActiveScreenShareSource.setPendingWindowAudioScope('system');
		expect(ActiveScreenShareSource.getPendingWindowAudioScope()).toBe('system');
		expect(ActiveScreenShareSource.getWindowAudioScope()).toBe('window');

		ActiveScreenShareSource.clearPendingWindowAudioScope();
		expect(ActiveScreenShareSource.getPendingWindowAudioScope()).toBe('window');
	});

	it('offers the running share own scope to the next picker until the user picks another one', () => {
		ActiveScreenShareSource.setPublishedSource('app', 'window:42:0');
		ActiveScreenShareSource.setWindowAudioScope('system');
		expect(ActiveScreenShareSource.getPendingWindowAudioScope()).toBe('system');

		ActiveScreenShareSource.setPendingWindowAudioScope('window');
		expect(ActiveScreenShareSource.getPendingWindowAudioScope()).toBe('window');
		expect(ActiveScreenShareSource.getWindowAudioScope()).toBe('system');

		ActiveScreenShareSource.clearPendingWindowAudioScope();
		expect(ActiveScreenShareSource.getPendingWindowAudioScope()).toBe('system');
	});

	it('keeps the window audio scope for the running share and drops it when the share ends', () => {
		expect(ActiveScreenShareSource.getWindowAudioScope()).toBe('window');

		ActiveScreenShareSource.setWindowAudioScope('system');
		expect(ActiveScreenShareSource.getWindowAudioScope()).toBe('system');

		ActiveScreenShareSource.setPublishedSource('app', 'window:42:0');
		expect(ActiveScreenShareSource.getWindowAudioScope()).toBe('system');

		ActiveScreenShareSource.setPendingWindowAudioScope('system');
		ActiveScreenShareSource.clear();
		expect(ActiveScreenShareSource.getWindowAudioScope()).toBe('window');
		expect(ActiveScreenShareSource.getPendingWindowAudioScope()).toBe('window');
	});
});
