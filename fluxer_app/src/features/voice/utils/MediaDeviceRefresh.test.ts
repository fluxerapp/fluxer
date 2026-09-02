// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, test, vi} from 'vitest';

const ensureDevices = vi.fn(async () => ({}));
const invalidate = vi.fn();

vi.mock('@app/features/voice/engine/VoiceDevicePermissionState', () => ({
	default: {ensureDevices},
}));

vi.mock('@app/features/voice/devices/MediaDeviceCache', () => ({
	mediaDeviceCache: {invalidate},
}));

const {refreshMediaDeviceLists, MediaDeviceRefreshType} = await import('@app/features/voice/utils/MediaDeviceRefresh');

beforeEach(() => {
	vi.clearAllMocks();
});

describe('refreshMediaDeviceLists', () => {
	test('an audio refresh only requests audio permission, never video', async () => {
		await refreshMediaDeviceLists({type: MediaDeviceRefreshType.audio});
		expect(invalidate).toHaveBeenCalledWith(MediaDeviceRefreshType.audio);
		expect(ensureDevices).toHaveBeenCalledTimes(1);
		expect(ensureDevices).toHaveBeenCalledWith({requestPermissionTypes: ['audio'], forceRefresh: true});
	});

	test('a video refresh only requests video permission, never audio', async () => {
		await refreshMediaDeviceLists({type: MediaDeviceRefreshType.video});
		expect(invalidate).toHaveBeenCalledWith(MediaDeviceRefreshType.video);
		expect(ensureDevices).toHaveBeenCalledTimes(1);
		expect(ensureDevices).toHaveBeenCalledWith({requestPermissionTypes: ['video'], forceRefresh: true});
	});
});
