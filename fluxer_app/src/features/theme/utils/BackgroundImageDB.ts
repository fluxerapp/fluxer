// SPDX-License-Identifier: AGPL-3.0-or-later

import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {VoiceBackgroundMediaKind} from '@app/types/electron.d';

export interface NativeBackgroundMediaSource {
	path: string;
	mediaKind: VoiceBackgroundMediaKind;
}

export async function saveBackgroundImage(id: string, blob: Blob): Promise<NativeBackgroundMediaSource> {
	const cacheVoiceBackgroundMedia = getElectronAPI()?.cacheVoiceBackgroundMedia;
	if (!cacheVoiceBackgroundMedia) {
		throw new Error('Native background media cache unavailable');
	}
	const fileName = blob instanceof File ? blob.name : undefined;
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
	await deleteVoiceBackgroundMedia(id);
}

export function dataUrlToObjectUrl(dataUrl: string): string | null {
	const match = dataUrl.match(/^data:([^;,]+)?(;base64)?,([\s\S]*)$/);
	if (!match) return null;
	const mimeType = match[1] || 'application/octet-stream';
	const isBase64 = match[2] === ';base64';
	const payload = match[3] ?? '';
	try {
		const bytes = isBase64
			? Uint8Array.from(atob(payload), (char) => char.charCodeAt(0))
			: new TextEncoder().encode(decodeURIComponent(payload));
		return URL.createObjectURL(new Blob([bytes], {type: mimeType}));
	} catch {
		return null;
	}
}

export async function getBackgroundImageURL(id: string): Promise<string | null> {
	const readVoiceBackgroundMedia = getElectronAPI()?.readVoiceBackgroundMedia;
	if (!readVoiceBackgroundMedia) return null;
	const media = await readVoiceBackgroundMedia(id);
	if (!media?.dataUrl) return null;
	return dataUrlToObjectUrl(media.dataUrl);
}
