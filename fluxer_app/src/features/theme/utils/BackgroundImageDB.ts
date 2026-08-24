// SPDX-License-Identifier: AGPL-3.0-or-later

import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {VoiceBackgroundMediaKind} from '@app/types/electron.d';

export interface NativeBackgroundMediaSource {
	path: string;
	mediaKind: VoiceBackgroundMediaKind;
}

interface BackgroundImageRead {
	cancelled: boolean;
	promise: Promise<string | null>;
}

const resolvedUrls = new Map<string, string>();
const pendingReads = new Map<string, BackgroundImageRead>();

async function readBackgroundImageObjectURL(id: string): Promise<string | null> {
	const readVoiceBackgroundMedia = getElectronAPI()?.readVoiceBackgroundMedia;
	if (!readVoiceBackgroundMedia) return null;
	const media = await readVoiceBackgroundMedia(id);
	if (!media?.dataUrl) return null;
	const response = await fetch(media.dataUrl);
	return URL.createObjectURL(await response.blob());
}

export async function saveBackgroundImage(id: string, blob: Blob): Promise<NativeBackgroundMediaSource> {
	const cacheVoiceBackgroundMedia = getElectronAPI()?.cacheVoiceBackgroundMedia;
	if (!cacheVoiceBackgroundMedia) {
		throw new Error('Native background media cache unavailable');
	}
	const fileName = blob instanceof File ? blob.name : undefined;
	releaseBackgroundImageURL(id);
	return cacheVoiceBackgroundMedia({
		id,
		mimeType: blob.type,
		...(fileName ? {fileName} : {}),
		data: await blob.arrayBuffer(),
	});
}

export async function deleteBackgroundImage(id: string): Promise<void> {
	const deleteVoiceBackgroundMedia = getElectronAPI()?.deleteVoiceBackgroundMedia;
	if (!deleteVoiceBackgroundMedia) {
		throw new Error('Native background media cache unavailable');
	}
	releaseBackgroundImageURL(id);
	await deleteVoiceBackgroundMedia(id);
}

export function getCachedBackgroundImageURL(id: string): string | null {
	return resolvedUrls.get(id) ?? null;
}

export function releaseBackgroundImageURL(id: string): void {
	const resolved = resolvedUrls.get(id);
	if (resolved != null) {
		URL.revokeObjectURL(resolved);
		resolvedUrls.delete(id);
	}
	const pending = pendingReads.get(id);
	if (pending != null) {
		pending.cancelled = true;
		pendingReads.delete(id);
	}
}

export async function getBackgroundImageURL(id: string): Promise<string | null> {
	const resolved = resolvedUrls.get(id);
	if (resolved != null) return resolved;
	const inFlight = pendingReads.get(id);
	if (inFlight != null) return inFlight.promise;
	const read: BackgroundImageRead = {cancelled: false, promise: Promise.resolve(null)};
	read.promise = readBackgroundImageObjectURL(id).then(
		(url) => {
			if (read.cancelled) {
				if (url != null) URL.revokeObjectURL(url);
				return null;
			}
			pendingReads.delete(id);
			if (url != null) resolvedUrls.set(id, url);
			return url;
		},
		(error: unknown) => {
			if (!read.cancelled) pendingReads.delete(id);
			throw error;
		},
	);
	pendingReads.set(id, read);
	return read.promise;
}
