// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	cancelResponseBodyAndThrow,
	readBoundedResponseBlob,
	runWithResponseDeadline,
} from '@app/features/voice/utils/camera-effects/BoundedResponse';
import {createWebCameraEffectImageFrameSource} from '@app/features/voice/utils/camera-effects/WebCameraEffectCustomFrameSource';

export type {
	WebCameraEffectCustomFrame,
	WebCameraEffectCustomFrameImage,
	WebCameraEffectCustomFrameLease,
	WebCameraEffectCustomFrameSource,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectCustomFrameSource';
export {
	createWebCameraEffectVideoFrameSource,
	WebCameraEffectCustomFrameSourceKind,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectCustomFrameSource';

import type {WebCameraEffectCustomFrameSource} from '@app/features/voice/utils/camera-effects/WebCameraEffectCustomFrameSource';

const MAX_CUSTOM_MEDIA_BYTES = 10 * 1024 * 1024;
const MAX_CUSTOM_MEDIA_URL_LENGTH = 16 * 1024;
const MAX_CUSTOM_MEDIA_RESPONSE_CHUNKS = 4096;
export const WEB_CAMERA_EFFECT_CUSTOM_MEDIA_OPERATION_TIMEOUT_MS = 8_000;

const SUPPORTED_IMAGE_MEDIA_TYPES = new Set(['image/gif', 'image/jpeg', 'image/png', 'image/webp']);

export function requireWebCameraEffectCustomMediaURL(URL: string): void {
	if (URL.length === 0 || URL.length > MAX_CUSTOM_MEDIA_URL_LENGTH) {
		throw new Error('Custom camera background URL has an invalid length');
	}
}

function throwIfCustomMediaOperationAborted(signal: AbortSignal): void {
	if (signal.aborted) {
		throw signal.reason ?? new Error('Custom camera background operation was aborted');
	}
}

export async function readWebCameraEffectCustomMediaBlob(URL: string, signal: AbortSignal): Promise<Blob> {
	requireWebCameraEffectCustomMediaURL(URL);
	throwIfCustomMediaOperationAborted(signal);
	let response: Response;
	try {
		response = await fetch(URL, {
			credentials: 'omit',
			redirect: 'error',
			referrerPolicy: 'no-referrer',
			signal,
		});
	} catch {
		if (signal.aborted) throw signal.reason;
		throw new Error('Custom camera background request failed');
	}
	if (!response.ok) {
		await cancelResponseBodyAndThrow({
			response,
			error: new Error(`Custom camera background request failed with status ${response.status}`),
			description: 'Custom camera background response',
		});
	}
	const blob = await readBoundedResponseBlob({
		response,
		maximumBytes: MAX_CUSTOM_MEDIA_BYTES,
		maximumChunks: MAX_CUSTOM_MEDIA_RESPONSE_CHUNKS,
		description: 'Custom camera background response',
	});
	throwIfCustomMediaOperationAborted(signal);
	if (blob.size === 0) {
		throw new Error('Custom camera background response is empty');
	}
	return blob;
}

function normalizedMediaType(value: string): string {
	return value.split(';', 1)[0]?.trim().toLowerCase() ?? '';
}

function bytesEqualAt(bytes: Uint8Array, offset: number, expected: ReadonlyArray<number>): boolean {
	if (bytes.byteLength < offset + expected.length) return false;
	for (let index = 0; index < expected.length; index += 1) {
		if (bytes[offset + index] !== expected[index]) return false;
	}
	return true;
}

function sniffImageMediaType(bytes: Uint8Array): string | null {
	if (bytesEqualAt(bytes, 0, [0x47, 0x49, 0x46, 0x38, 0x37, 0x61])) return 'image/gif';
	if (bytesEqualAt(bytes, 0, [0x47, 0x49, 0x46, 0x38, 0x39, 0x61])) return 'image/gif';
	if (bytesEqualAt(bytes, 0, [0xff, 0xd8, 0xff])) return 'image/jpeg';
	if (bytesEqualAt(bytes, 0, [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a])) return 'image/png';
	if (bytesEqualAt(bytes, 0, [0x52, 0x49, 0x46, 0x46]) && bytesEqualAt(bytes, 8, [0x57, 0x45, 0x42, 0x50])) {
		return 'image/webp';
	}
	return null;
}

export async function resolveWebCameraEffectCustomImageMediaType(blob: Blob, signal: AbortSignal): Promise<string> {
	const header = new Uint8Array(await blob.slice(0, 12).arrayBuffer());
	throwIfCustomMediaOperationAborted(signal);
	const detectedMediaType = sniffImageMediaType(header);
	if (detectedMediaType == null) {
		throw new Error('Custom camera background is not a supported image format');
	}
	const declaredMediaType = normalizedMediaType(blob.type);
	if (SUPPORTED_IMAGE_MEDIA_TYPES.has(declaredMediaType) && declaredMediaType !== detectedMediaType) {
		throw new Error('Custom camera background media type does not match its encoded data');
	}
	return detectedMediaType;
}

export async function loadWebCameraEffectCustomFrameSource(URL: string): Promise<WebCameraEffectCustomFrameSource> {
	requireWebCameraEffectCustomMediaURL(URL);
	return runWithResponseDeadline({
		timeoutMilliseconds: WEB_CAMERA_EFFECT_CUSTOM_MEDIA_OPERATION_TIMEOUT_MS,
		description: 'Custom camera background initialization',
		signal: null,
		operation: async (signal) => {
			const blob = await readWebCameraEffectCustomMediaBlob(URL, signal);
			const mediaType = await resolveWebCameraEffectCustomImageMediaType(blob, signal);
			return createWebCameraEffectImageFrameSource(blob, mediaType, signal);
		},
	});
}
