// SPDX-License-Identifier: AGPL-3.0-or-later

import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {NativeScreenCaptureAvailability} from '@app/types/electron.d';

export const MAX_GENERATOR_PENDING_VIDEO_FRAMES = 2;
export const MIN_NATIVE_SCREEN_DIM = 16;
export const MAX_NATIVE_SCREEN_DIM = 8192;

export interface VideoFrameInit {
	format: 'NV12' | 'BGRA' | 'I420' | string;
	codedWidth: number;
	codedHeight: number;
	displayWidth?: number;
	displayHeight?: number;
	timestamp: number;
	layout?: Array<{offset: number; stride: number}>;
}

export interface CanvasVideoFrameInit {
	timestamp: number;
	displayWidth?: number;
	displayHeight?: number;
}

export interface RendererVideoFrame {
	close: () => void;
}

export interface VideoFrameCtor {
	new (data: BufferSource, init: VideoFrameInit): RendererVideoFrame;
	new (data: CanvasImageSource, init: CanvasVideoFrameInit): RendererVideoFrame;
}

export interface GeneratorVideoTrack extends MediaStreamTrack {
	writable: WritableStream<unknown>;
}

export interface GeneratorVideoCtor {
	new (options: {kind: 'video'}): GeneratorVideoTrack;
}

export interface NativeScreenBridgeHandle {
	track: MediaStreamTrack;
	cleanup: (stopRemote?: boolean) => Promise<void>;
}

const NATIVE_SCREEN_SHARE_TRACK_MARKER = Symbol.for('fluxer.nativeScreenShareTrack');

export function markNativeScreenShareTrack(track: MediaStreamTrack): void {
	try {
		Object.defineProperty(track, NATIVE_SCREEN_SHARE_TRACK_MARKER, {
			value: true,
			configurable: true,
		});
	} catch {
		(track as MediaStreamTrack & Record<PropertyKey, unknown>)[NATIVE_SCREEN_SHARE_TRACK_MARKER] = true;
	}
}

export function getVideoFrameCtor(): VideoFrameCtor | undefined {
	return (
		window as typeof window & {
			VideoFrame?: VideoFrameCtor;
		}
	).VideoFrame;
}

export function getGeneratorVideoCtor(): GeneratorVideoCtor | undefined {
	return (
		window as typeof window & {
			MediaStreamTrackGenerator?: GeneratorVideoCtor;
		}
	).MediaStreamTrackGenerator;
}

export function getNativeScreenCaptureApi() {
	return getElectronAPI()?.nativeScreenCapture ?? null;
}

export function normalizeNativeScreenCaptureDimension(value: number | undefined): number | undefined {
	if (value === undefined || value <= 0 || !Number.isFinite(value)) return undefined;
	let normalized = Math.max(MIN_NATIVE_SCREEN_DIM, Math.min(MAX_NATIVE_SCREEN_DIM, Math.floor(value)));
	if (normalized % 2 !== 0) {
		normalized = normalized < MAX_NATIVE_SCREEN_DIM ? normalized + 1 : normalized - 1;
	}
	return normalized;
}

export function normalizeNativeScreenCaptureResolution(
	resolution:
		| {
				width?: number;
				height?: number;
		  }
		| undefined,
):
	| {
			width?: number;
			height?: number;
	  }
	| undefined {
	const width = normalizeNativeScreenCaptureDimension(resolution?.width);
	const height = normalizeNativeScreenCaptureDimension(resolution?.height);
	if (width === undefined && height === undefined) return undefined;
	return {width, height};
}

export function unsupportedPlatformScreenAvailability(): NativeScreenCaptureAvailability {
	return {available: false, reason: 'unsupported-platform'};
}
