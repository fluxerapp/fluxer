// SPDX-License-Identifier: AGPL-3.0-or-later

import {videoElementHasRenderedFrame} from '@app/features/voice/components/VideoElementFrameState';
import {Store} from '@app/features/voice/engine/Store';
import ScreenShareDeliveryRollout from '@app/features/voice/state/ScreenShareDeliveryRollout';

export const LAST_FRAME_SNAPSHOTS_MAX = 8;
export const LAST_FRAME_SNAPSHOT_WIDTH_MAX = 640;
export const LAST_FRAME_SNAPSHOT_JPEG_QUALITY = 0.7;

const LAST_FRAME_SNAPSHOT_LEGACY_WIDTH_MAX = 1280;

function computeSnapshotDimensions(
	sourceWidth: number,
	sourceHeight: number,
	widthMax: number,
): {width: number; height: number} {
	if (sourceWidth <= widthMax) {
		return {width: sourceWidth, height: sourceHeight};
	}
	const scale = widthMax / sourceWidth;
	return {
		width: widthMax,
		height: Math.max(1, Math.round(sourceHeight * scale)),
	};
}

function drawSourceToOffscreenCanvas(
	source: CanvasImageSource,
	sourceWidth: number,
	sourceHeight: number,
	widthMax: number,
): OffscreenCanvas | null {
	if (typeof OffscreenCanvas === 'undefined') return null;
	if (sourceWidth <= 0 || sourceHeight <= 0) return null;
	try {
		const {width, height} = computeSnapshotDimensions(sourceWidth, sourceHeight, widthMax);
		const canvas = new OffscreenCanvas(width, height);
		const context = canvas.getContext('2d');
		if (!context) return null;
		context.drawImage(source, 0, 0, width, height);
		return canvas;
	} catch {
		return null;
	}
}

function drawSourceToDataUrl(
	source: CanvasImageSource,
	sourceWidth: number,
	sourceHeight: number,
	widthMax: number,
): string | null {
	if (typeof document === 'undefined') return null;
	if (sourceWidth <= 0 || sourceHeight <= 0) return null;
	try {
		const {width, height} = computeSnapshotDimensions(sourceWidth, sourceHeight, widthMax);
		const canvas = document.createElement('canvas');
		canvas.width = width;
		canvas.height = height;
		const context = canvas.getContext('2d');
		if (!context) return null;
		context.drawImage(source, 0, 0, width, height);
		return canvas.toDataURL('image/jpeg', LAST_FRAME_SNAPSHOT_JPEG_QUALITY);
	} catch {
		return null;
	}
}

function revokeSnapshotUrl(url: string): void {
	if (!url.startsWith('blob:')) return;
	try {
		URL.revokeObjectURL(url);
	} catch {}
}

class LastFrameSnapshotCache extends Store {
	private snapshots = new Map<string, string>();
	private captureIds = new Map<string, number>();
	private nextCaptureId = 1;

	get size(): number {
		return this.snapshots.size;
	}

	getSnapshotUrl(key: string): string | null {
		return this.snapshots.get(key) ?? null;
	}

	retainSnapshot(key: string, url: string): void {
		if (!key || !url) return;
		const revoked: Array<string> = [];
		this.update(() => {
			const previous = this.snapshots.get(key);
			if (previous !== undefined) {
				this.snapshots.delete(key);
				revoked.push(previous);
			}
			this.snapshots.set(key, url);
			while (this.snapshots.size > LAST_FRAME_SNAPSHOTS_MAX) {
				const oldestKey = this.snapshots.keys().next().value;
				if (oldestKey === undefined) break;
				const oldestUrl = this.snapshots.get(oldestKey);
				this.snapshots.delete(oldestKey);
				if (oldestUrl !== undefined) {
					revoked.push(oldestUrl);
				}
			}
		});
		for (const stale of revoked) {
			revokeSnapshotUrl(stale);
		}
	}

	captureFromVideoElement(key: string, video: HTMLVideoElement | null): void {
		if (!key) return;
		const deliveryEnabled = ScreenShareDeliveryRollout.enabled;
		if (deliveryEnabled && typeof document !== 'undefined' && document.visibilityState === 'hidden') return;
		if (!videoElementHasRenderedFrame(video)) return;
		const renderedVideo = video as HTMLVideoElement;
		const widthMax = deliveryEnabled ? LAST_FRAME_SNAPSHOT_WIDTH_MAX : LAST_FRAME_SNAPSHOT_LEGACY_WIDTH_MAX;
		if (deliveryEnabled) {
			const captureId = this.nextCaptureId;
			this.nextCaptureId += 1;
			this.captureIds.set(key, captureId);
			const canvas = drawSourceToOffscreenCanvas(
				renderedVideo,
				renderedVideo.videoWidth,
				renderedVideo.videoHeight,
				widthMax,
			);
			if (canvas) {
				void canvas
					.convertToBlob({type: 'image/jpeg', quality: LAST_FRAME_SNAPSHOT_JPEG_QUALITY})
					.then((blob) => {
						if (this.captureIds.get(key) !== captureId) return;
						this.captureIds.delete(key);
						this.retainSnapshot(key, URL.createObjectURL(blob));
					})
					.catch(() => {
						if (this.captureIds.get(key) !== captureId) return;
						this.captureIds.delete(key);
					});
				return;
			}
			this.captureIds.delete(key);
		}
		const dataUrl = drawSourceToDataUrl(renderedVideo, renderedVideo.videoWidth, renderedVideo.videoHeight, widthMax);
		if (!dataUrl) return;
		this.retainSnapshot(key, dataUrl);
	}

	release(key: string): void {
		this.captureIds.delete(key);
		const url = this.snapshots.get(key);
		if (url === undefined) return;
		this.update(() => {
			this.snapshots.delete(key);
		});
		revokeSnapshotUrl(url);
	}

	clear(): void {
		this.captureIds.clear();
		if (this.snapshots.size === 0) return;
		const revoked = [...this.snapshots.values()];
		this.update(() => {
			this.snapshots.clear();
		});
		for (const stale of revoked) {
			revokeSnapshotUrl(stale);
		}
	}
}

export default new LastFrameSnapshotCache();
export {LastFrameSnapshotCache};
