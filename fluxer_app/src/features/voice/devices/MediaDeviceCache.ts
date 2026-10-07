// SPDX-License-Identifier: AGPL-3.0-or-later

type MediaDeviceCacheKind = 'audio' | 'video';

interface CachedDeviceEntry {
	devices: Array<MediaDeviceInfo>;
	fetchedAt: number;
}

class MediaDeviceCache {
	private cache = new Map<MediaDeviceCacheKind, CachedDeviceEntry>();
	private pending = new Map<MediaDeviceCacheKind, Promise<CachedDeviceEntry>>();
	private revision = 0;

	invalidate(type: MediaDeviceCacheKind): void {
		this.revision += 1;
		this.cache.delete(type);
		this.pending.delete(type);
	}
}

export const mediaDeviceCache = new MediaDeviceCache();
