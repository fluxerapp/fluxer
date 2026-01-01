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

/// <reference lib="webworker" />

import {cropAndRotateGif, ensureLibfluxcoreReady} from '~/lib/libfluxcore.worker';

export enum CropGifMessageType {
	CROP_GIF_START = 0,
	CROP_GIF_COMPLETE = 1,
	CROP_GIF_ERROR = 2,
}

export interface CropGifStartMessage {
	type: CropGifMessageType.CROP_GIF_START;
	gif: Uint8Array;
	x: number;
	y: number;
	width: number;
	height: number;
	imageRotation?: number;
	resizeWidth?: number | null;
	resizeHeight?: number | null;
}

export interface CropGifCompleteMessage {
	type: CropGifMessageType.CROP_GIF_COMPLETE;
	result: Uint8Array;
}

export interface CropGifErrorMessage {
	type: CropGifMessageType.CROP_GIF_ERROR;
	error: string;
}

type IncomingMessage = CropGifStartMessage;

declare const self: DedicatedWorkerGlobalScope;

self.addEventListener('message', async (event: MessageEvent<IncomingMessage>) => {
	const msg = event.data;
	if (msg?.type !== CropGifMessageType.CROP_GIF_START) return;

	const {gif, x, y, width, height, imageRotation = 0, resizeWidth = null, resizeHeight = null} = msg;

	try {
		await ensureLibfluxcoreReady();
		const result = cropAndRotateGif(gif, x, y, width, height, imageRotation, resizeWidth, resizeHeight);

		const transferables: Array<Transferable> = [];
		if (result?.buffer) {
			transferables.push(result.buffer);
		}

		const response: CropGifCompleteMessage = {
			type: CropGifMessageType.CROP_GIF_COMPLETE,
			result,
		};

		self.postMessage(response, transferables);
	} catch (err) {
		console.error('[GIF Worker] Error:', err);
		const response: CropGifErrorMessage = {
			type: CropGifMessageType.CROP_GIF_ERROR,
			error: err instanceof Error ? err.message : String(err),
		};
		self.postMessage(response);
	}
});
