// SPDX-License-Identifier: AGPL-3.0-or-later

export type AnimatedCropSourceFormat = 'gif' | 'png' | 'webp' | 'avif';
export type AnimatedCropOutputMime = 'image/gif' | 'image/webp';
export type CropErrorCode =
	| 'aborted'
	| 'timeout'
	| 'budget_exceeded'
	| 'too_large'
	| 'decode_failed'
	| 'animated_avif_unsupported'
	| 'wasm_unavailable'
	| 'encode_failed'
	| 'internal';
export type CropProgressPhase = 'init' | 'crop' | 'compress';

export interface CropParams {
	x: number;
	y: number;
	width: number;
	height: number;
	rotation: 0 | 90 | 180 | 270;
	outputWidth: number;
	outputHeight: number;
}

export interface CropWorkerRequest {
	type: 'crop';
	jobId: number;
	bytes: ArrayBuffer;
	format: AnimatedCropSourceFormat;
	crop: CropParams;
	sizeLimitBytes: number;
}

export interface CropStats {
	variant: 'simd' | 'scalar' | 'none';
	passes: number;
	method: number;
	quality: number;
	lossless: boolean;
	framesDecoded: number;
	decodeMs: number;
	encodeMs: number;
}

export type CropWorkerResponse =
	| {type: 'progress'; jobId: number; phase: CropProgressPhase; done: number; total: number}
	| {type: 'done'; jobId: number; bytes: ArrayBuffer; mime: AnimatedCropOutputMime; stats: CropStats}
	| {type: 'error'; jobId: number; code: CropErrorCode; message: string};

export class CropPipelineError extends Error {
	readonly code: CropErrorCode;

	constructor(code: CropErrorCode, message: string) {
		super(message);
		this.code = code;
		this.name = 'CropPipelineError';
	}
}
