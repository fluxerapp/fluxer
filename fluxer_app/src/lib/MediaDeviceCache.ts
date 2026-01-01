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

interface CacheEntry {
	devices: Array<MediaDeviceInfo>;
	timestamp: number;
}

type PermissionType = 'audio' | 'video';

class MediaDeviceCache {
	private cache: Map<PermissionType, CacheEntry> = new Map();
	private readonly STALE_TIME = 5000;
	private revalidationPromises: Map<PermissionType, Promise<Array<MediaDeviceInfo>>> = new Map();

	public async getDevices(
		type: PermissionType,
		fetchFn: () => Promise<Array<MediaDeviceInfo>>,
	): Promise<{devices: Array<MediaDeviceInfo>; isStale: boolean}> {
		const cached = this.cache.get(type);
		const now = Date.now();

		if (cached && now - cached.timestamp < this.STALE_TIME) {
			return {devices: cached.devices, isStale: false};
		}

		if (cached) {
			if (!this.revalidationPromises.has(type)) {
				const revalidationPromise = this.revalidate(type, fetchFn);
				this.revalidationPromises.set(type, revalidationPromise);
				revalidationPromise.finally(() => {
					this.revalidationPromises.delete(type);
				});
			}
			return {devices: cached.devices, isStale: true};
		}

		try {
			const devices = await fetchFn();
			this.cache.set(type, {devices, timestamp: now});
			return {devices, isStale: false};
		} catch (_error) {
			return {devices: [], isStale: false};
		}
	}

	private async revalidate(
		type: PermissionType,
		fetchFn: () => Promise<Array<MediaDeviceInfo>>,
	): Promise<Array<MediaDeviceInfo>> {
		try {
			const devices = await fetchFn();
			this.cache.set(type, {devices, timestamp: Date.now()});
			return devices;
		} catch (_error) {
			return this.cache.get(type)?.devices ?? [];
		}
	}

	public invalidate(type: PermissionType): void {
		this.cache.delete(type);
		this.revalidationPromises.delete(type);
	}

	public clear(): void {
		this.cache.clear();
		this.revalidationPromises.clear();
	}

	public startDeviceChangeListener(): () => void {
		const mediaDevices = navigator.mediaDevices;
		if (!mediaDevices || typeof mediaDevices.addEventListener !== 'function') {
			const previousHandler = mediaDevices?.ondevicechange ?? null;
			const handleDeviceChange = (event: Event) => {
				this.clear();
				previousHandler?.call(mediaDevices ?? undefined, event);
			};

			if (mediaDevices) {
				mediaDevices.ondevicechange = handleDeviceChange;
			}

			return () => {
				if (mediaDevices) {
					mediaDevices.ondevicechange = previousHandler;
				}
			};
		}

		const handleDeviceChange = () => {
			this.clear();
		};

		mediaDevices.addEventListener('devicechange', handleDeviceChange);

		return () => {
			mediaDevices.removeEventListener('devicechange', handleDeviceChange);
		};
	}
}

export const mediaDeviceCache = new MediaDeviceCache();
