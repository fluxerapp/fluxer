// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	mediaStreamTrackProcessorSupported,
	videoFrameCaptureSupported,
} from '@app/features/voice/utils/camera-effects/MediaStreamTrackProcessorPolyfill';

export interface WebCameraSegmentationRuntimeCapability {
	readonly available: boolean;
	readonly reason: string;
}

class WebCameraSegmentationCapabilityOwner {
	private capability: WebCameraSegmentationRuntimeCapability | null = null;

	get(): WebCameraSegmentationRuntimeCapability {
		if (this.capability == null) {
			this.capability = detectWebCameraSegmentationCapability();
		}
		return this.capability;
	}
}

const webCameraSegmentationCapabilityOwner = new WebCameraSegmentationCapabilityOwner();

function transferableReadableStreamSupported(): boolean {
	if (typeof structuredClone !== 'function' || !('ReadableStream' in globalThis)) {
		return false;
	}
	const stream = new ReadableStream<never>({
		start(controller): void {
			controller.close();
		},
	});
	try {
		const transferred = structuredClone(stream, {transfer: [stream]});
		return transferred instanceof ReadableStream;
	} catch {
		return false;
	}
}

function offscreenCanvasFallbackSupported(): boolean {
	if (!('OffscreenCanvas' in globalThis)) {
		return false;
	}
	try {
		return new OffscreenCanvas(1, 1).getContext('2d') != null;
	} catch {
		return false;
	}
}

export function detectWebCameraSegmentationCapability(): WebCameraSegmentationRuntimeCapability {
	if (!('window' in globalThis)) {
		return {available: false, reason: 'Camera effects require a window and document'};
	}
	if (!('document' in globalThis)) {
		return {available: false, reason: 'Camera effects require a window and document'};
	}
	if (!('Worker' in globalThis)) {
		return {available: false, reason: 'Camera effects require module workers and VideoFrame'};
	}
	if (!('VideoFrame' in globalThis)) {
		return {available: false, reason: 'Camera effects require module workers and VideoFrame'};
	}
	if (!('HTMLCanvasElement' in globalThis)) {
		return {available: false, reason: 'Camera effects require transferable captured canvas output'};
	}
	if (typeof HTMLCanvasElement.prototype.captureStream !== 'function') {
		return {available: false, reason: 'Camera effects require transferable captured canvas output'};
	}
	if (typeof HTMLCanvasElement.prototype.transferControlToOffscreen !== 'function') {
		return {available: false, reason: 'Camera effects require transferable captured canvas output'};
	}
	if (!offscreenCanvasFallbackSupported()) {
		return {available: false, reason: 'Camera effects require an OffscreenCanvas 2D fallback'};
	}
	if (!mediaStreamTrackProcessorSupported() && !videoFrameCaptureSupported()) {
		return {available: false, reason: 'Camera effects require bounded VideoFrame capture'};
	}
	if (!transferableReadableStreamSupported()) {
		return {available: false, reason: 'Camera effects require transferable readable streams'};
	}
	return {available: true, reason: 'Worker camera effect pipeline is available'};
}

export function webCameraSegmentationRuntimeAvailable(): boolean {
	return webCameraSegmentationCapabilityOwner.get().available;
}
