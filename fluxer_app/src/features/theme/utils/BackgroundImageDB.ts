// SPDX-License-Identifier: AGPL-3.0-or-later

import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {VoiceBackgroundMediaKind} from '@app/types/electron.d';

export interface NativeBackgroundMediaSource {
	path: string;
	mediaKind: VoiceBackgroundMediaKind;
}

export interface BackgroundMediaObjectURL {
	url: string;
	mediaKind: VoiceBackgroundMediaKind;
}

interface BackgroundImageRead {
	cancelled: boolean;
	promise: Promise<BackgroundMediaObjectURL | null>;
}

const resolvedMedia = new Map<string, BackgroundMediaObjectURL>();
const pendingReads = new Map<string, BackgroundImageRead>();

async function readBackgroundMediaObjectURL(id: string): Promise<BackgroundMediaObjectURL | null> {
	const readVoiceBackgroundMedia = getElectronAPI()?.readVoiceBackgroundMedia;
	if (!readVoiceBackgroundMedia) return null;
	const media = await readVoiceBackgroundMedia(id);
	if (!media?.dataUrl) return null;
	const response = await fetch(media.dataUrl);
	return {
		url: URL.createObjectURL(await response.blob()),
		mediaKind: media.mediaKind,
	};
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
	return resolvedMedia.get(id)?.url ?? null;
}

export function releaseBackgroundImageURL(id: string): void {
	const resolved = resolvedMedia.get(id);
	if (resolved != null) {
		URL.revokeObjectURL(resolved.url);
		resolvedMedia.delete(id);
	}
	const pending = pendingReads.get(id);
	if (pending != null) {
		pending.cancelled = true;
		pendingReads.delete(id);
	}
}

export async function getBackgroundMediaObjectURL(id: string): Promise<BackgroundMediaObjectURL | null> {
	const resolved = resolvedMedia.get(id);
	if (resolved != null) return resolved;
	const inFlight = pendingReads.get(id);
	if (inFlight != null) return inFlight.promise;
	const read: BackgroundImageRead = {cancelled: false, promise: Promise.resolve(null)};
	read.promise = readBackgroundMediaObjectURL(id).then(
		(media) => {
			if (read.cancelled) {
				if (media != null) URL.revokeObjectURL(media.url);
				return null;
			}
			pendingReads.delete(id);
			if (media != null) resolvedMedia.set(id, media);
			return media;
		},
		(error: unknown) => {
			if (!read.cancelled) pendingReads.delete(id);
			throw error;
		},
	);
	pendingReads.set(id, read);
	return read.promise;
}

export async function getBackgroundImageURL(id: string): Promise<string | null> {
	return (await getBackgroundMediaObjectURL(id))?.url ?? null;
}
