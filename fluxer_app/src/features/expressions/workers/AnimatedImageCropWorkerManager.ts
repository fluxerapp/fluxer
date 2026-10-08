// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type AnimatedCropOutputMime,
	type AnimatedCropSourceFormat,
	type CropErrorCode,
	type CropParams,
	CropPipelineError,
	type CropProgressPhase,
	type CropStats,
	type CropWorkerRequest,
	type CropWorkerResponse,
} from '@app/features/expressions/workers/AnimatedImageCropMessages';

export {CropPipelineError};

const STALL_TIMEOUT_MS = 45_000;

let lastJobId = 0;

export interface AnimatedCropRequest {
	bytes: ArrayBuffer;
	format: AnimatedCropSourceFormat;
	crop: CropParams;
	sizeLimitBytes: number;
}

export interface AnimatedCropProgress {
	phase: CropProgressPhase;
	fraction: number;
}

export interface AnimatedCropOptions {
	signal: AbortSignal;
	onProgress?: (progress: AnimatedCropProgress) => void;
}

export interface AnimatedCropResult {
	bytes: Uint8Array;
	mime: AnimatedCropOutputMime;
	stats: CropStats;
}

function abortedError(): CropPipelineError {
	return new CropPipelineError('aborted', 'Animated crop was aborted');
}

export function cropAnimatedImage(
	request: AnimatedCropRequest,
	options: AnimatedCropOptions,
): Promise<AnimatedCropResult> {
	const {signal, onProgress} = options;
	if (signal.aborted) return Promise.reject(abortedError());
	const jobId = ++lastJobId;

	return new Promise<AnimatedCropResult>((resolve, reject) => {
		let worker: Worker;
		try {
			worker = new Worker(
				new URL(/* webpackChunkName: "animated-image-crop.worker" */ './AnimatedImageCropWorker.ts', import.meta.url),
				{type: 'module'},
			);
		} catch (error) {
			reject(new CropPipelineError('internal', error instanceof Error ? error.message : String(error)));
			return;
		}
		let settled = false;
		let stallTimer: ReturnType<typeof setTimeout> | null = null;

		const settle = (finish: () => void): void => {
			if (settled) return;
			settled = true;
			if (stallTimer !== null) clearTimeout(stallTimer);
			stallTimer = null;
			worker.removeEventListener('message', handleMessage);
			worker.removeEventListener('error', handleError);
			worker.removeEventListener('messageerror', handleMessageError);
			signal.removeEventListener('abort', handleAbort);
			worker.terminate();
			finish();
		};
		const fail = (code: CropErrorCode, message: string): void => {
			settle(() => reject(new CropPipelineError(code, message)));
		};
		const armStallTimer = (): void => {
			if (stallTimer !== null) clearTimeout(stallTimer);
			stallTimer = setTimeout(() => fail('timeout', 'Animated crop worker stopped responding'), STALL_TIMEOUT_MS);
		};

		function handleMessage(event: MessageEvent<CropWorkerResponse>): void {
			const message = event.data;
			if (message?.jobId !== jobId) return;
			armStallTimer();
			switch (message.type) {
				case 'progress':
					onProgress?.({phase: message.phase, fraction: message.total > 0 ? message.done / message.total : 0});
					return;
				case 'done':
					settle(() => resolve({bytes: new Uint8Array(message.bytes), mime: message.mime, stats: message.stats}));
					return;
				case 'error':
					fail(message.code, message.message);
					return;
			}
		}
		function handleError(event: ErrorEvent): void {
			fail('internal', event.message || 'Animated crop worker failed');
		}
		function handleMessageError(): void {
			fail('internal', 'Animated crop worker sent a message that could not be read');
		}
		function handleAbort(): void {
			settle(() => reject(abortedError()));
		}

		worker.addEventListener('message', handleMessage);
		worker.addEventListener('error', handleError);
		worker.addEventListener('messageerror', handleMessageError);
		signal.addEventListener('abort', handleAbort);
		armStallTimer();
		const message: CropWorkerRequest = {
			type: 'crop',
			jobId,
			bytes: request.bytes,
			format: request.format,
			crop: request.crop,
			sizeLimitBytes: request.sizeLimitBytes,
		};
		try {
			worker.postMessage(message, [request.bytes]);
		} catch (error) {
			fail('internal', error instanceof Error ? error.message : String(error));
		}
	});
}
