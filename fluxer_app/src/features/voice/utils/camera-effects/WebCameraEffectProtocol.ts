// SPDX-License-Identifier: AGPL-3.0-or-later

import {CameraBackgroundMode} from '@app/features/voice/utils/camera-effects/CameraCaptureContract';
import type {ErrorDiagnostic, ErrorDiagnosticType} from '@app/features/voice/utils/camera-effects/ErrorDiagnostic';

class InvalidCameraEffectFrameDimensionsError extends Error {
	constructor() {
		super('Camera effect frame dimensions must be positive safe integers');
		this.name = 'InvalidCameraEffectFrameDimensionsError';
	}
}

class CameraEffectFrameDimensionsExceededError extends Error {
	constructor() {
		super('Camera effect frame dimensions exceed the bounded working set');
		this.name = 'CameraEffectFrameDimensionsExceededError';
	}
}

export interface WebCameraEffectBlurConfig {
	readonly mode: typeof CameraBackgroundMode.BLUR;
	readonly blurStrength: number;
}

export interface WebCameraEffectCustomConfig {
	readonly mode: typeof CameraBackgroundMode.CUSTOM;
	readonly blurStrength: number;
	readonly customMediaURL: string;
	readonly customMediaKind: WebCameraEffectCustomMediaKind;
}

export type WebCameraEffectConfig = WebCameraEffectBlurConfig | WebCameraEffectCustomConfig;

export interface WebCameraPipelineConfig {
	readonly background: WebCameraEffectConfig | null;
}

export const WEB_CAMERA_EFFECT_SEGMENTATION_MIN_INTERVAL_MS = 50;

export const WEB_CAMERA_EFFECT_STOP_GRACE_MS = 250;

const MIN_BLUR_PIXELS = 4;
const MAX_BLUR_PIXELS = 20;
const WEB_CAMERA_EFFECT_BLUR_STRENGTH_MIN = 0;
const WEB_CAMERA_EFFECT_BLUR_STRENGTH_MAX = 100;
const MAX_FRAME_EDGE = 4096;
const MAX_FRAME_PIXELS = 3840 * 2160;

export function requireCameraEffectBlurStrength(strength: number): number {
	if (!Number.isFinite(strength)) {
		throw new Error('Camera effect blur strength must be finite');
	}
	if (strength < WEB_CAMERA_EFFECT_BLUR_STRENGTH_MIN) {
		throw new Error('Camera effect blur strength is below the supported range');
	}
	if (strength > WEB_CAMERA_EFFECT_BLUR_STRENGTH_MAX) {
		throw new Error('Camera effect blur strength exceeds the supported range');
	}
	return strength;
}

export function cameraEffectBlurPixels(strength: number): number {
	const validatedStrength = requireCameraEffectBlurStrength(strength);
	const range = MAX_BLUR_PIXELS - MIN_BLUR_PIXELS;
	return Math.round(MIN_BLUR_PIXELS + (validatedStrength / WEB_CAMERA_EFFECT_BLUR_STRENGTH_MAX) * range);
}

export function validateCameraEffectFrameDimensions(width: number, height: number): void {
	if (!Number.isSafeInteger(width)) {
		throw new InvalidCameraEffectFrameDimensionsError();
	}
	if (width <= 0) {
		throw new InvalidCameraEffectFrameDimensionsError();
	}
	if (!Number.isSafeInteger(height)) {
		throw new InvalidCameraEffectFrameDimensionsError();
	}
	if (height <= 0) {
		throw new InvalidCameraEffectFrameDimensionsError();
	}
	if (width > MAX_FRAME_EDGE) {
		throw new CameraEffectFrameDimensionsExceededError();
	}
	if (height > MAX_FRAME_EDGE) {
		throw new CameraEffectFrameDimensionsExceededError();
	}
	if (width * height > MAX_FRAME_PIXELS) {
		throw new CameraEffectFrameDimensionsExceededError();
	}
}

export const WebCameraEffectBackend = Object.freeze({
	WEB_GPU: 'webgpu',
	WASM_WORKER: 'wasm-worker',
	CANVAS_WORKER: 'canvas-worker',
} as const);

export type WebCameraEffectBackend = (typeof WebCameraEffectBackend)[keyof typeof WebCameraEffectBackend];

export const WebCameraEffectCommandKind = Object.freeze({
	START: 'start',
	UPDATE: 'update',
	STOP: 'stop',
} as const);

export type WebCameraEffectCommandKind = (typeof WebCameraEffectCommandKind)[keyof typeof WebCameraEffectCommandKind];

export const WebCameraEffectEventKind = Object.freeze({
	READY: 'ready',
	UPDATED: 'updated',
	UPDATE_FAILED: 'update_failed',
	FAILED: 'failed',
	STOPPED: 'stopped',
} as const);

export type WebCameraEffectEventKind = (typeof WebCameraEffectEventKind)[keyof typeof WebCameraEffectEventKind];

export const WebCameraEffectShutdownReason = Object.freeze({
	OWNER_STOP: 'owner-stop',
	INPUT_ENDED: 'input-ended',
	OPERATION_FAILED: 'operation-failed',
	CLEANUP_FAILED: 'cleanup-failed',
} as const);

export type WebCameraEffectShutdownReason =
	(typeof WebCameraEffectShutdownReason)[keyof typeof WebCameraEffectShutdownReason];

export function isWebCameraEffectShutdownReason(value: unknown): value is WebCameraEffectShutdownReason {
	if (value === WebCameraEffectShutdownReason.OWNER_STOP) {
		return true;
	}
	if (value === WebCameraEffectShutdownReason.INPUT_ENDED) {
		return true;
	}
	if (value === WebCameraEffectShutdownReason.OPERATION_FAILED) {
		return true;
	}
	return value === WebCameraEffectShutdownReason.CLEANUP_FAILED;
}

export const WebCameraEffectCustomMediaKind = Object.freeze({
	STATIC: 'static',
	ANIMATED: 'animated',
	VIDEO: 'video',
} as const);

export type WebCameraEffectCustomMediaKind =
	(typeof WebCameraEffectCustomMediaKind)[keyof typeof WebCameraEffectCustomMediaKind];

export function isWebCameraEffectCustomMediaKind(value: unknown): value is WebCameraEffectCustomMediaKind {
	if (value === WebCameraEffectCustomMediaKind.STATIC) {
		return true;
	}
	if (value === WebCameraEffectCustomMediaKind.ANIMATED) {
		return true;
	}
	return value === WebCameraEffectCustomMediaKind.VIDEO;
}

export const WEB_CAMERA_EFFECT_UPDATE_REQUEST_ID_MAX = 2_147_483_647;

function isWebCameraPipelineConfig(value: unknown): value is WebCameraPipelineConfig {
	if (value == null || typeof value !== 'object') {
		return false;
	}
	const background = Reflect.get(value, 'background');
	if (background === null) {
		return true;
	}
	if (background == null || typeof background !== 'object') {
		return false;
	}
	const mode = Reflect.get(background, 'mode');
	const blurStrength = Reflect.get(background, 'blurStrength');
	if (typeof blurStrength !== 'number') {
		return false;
	}
	try {
		requireCameraEffectBlurStrength(blurStrength);
	} catch {
		return false;
	}
	if (mode === CameraBackgroundMode.BLUR) {
		return true;
	}
	if (mode !== CameraBackgroundMode.CUSTOM) {
		return false;
	}
	const customMediaURL = Reflect.get(background, 'customMediaURL');
	if (typeof customMediaURL !== 'string' || customMediaURL.length === 0) {
		return false;
	}
	return isWebCameraEffectCustomMediaKind(Reflect.get(background, 'customMediaKind'));
}

function hasExpectedCustomBackgroundFrames(
	config: WebCameraPipelineConfig,
	frames: unknown,
	requireVideoFrames: boolean,
): frames is ReadableStream<VideoFrame> | null {
	const background = config.background;
	if (background == null || background.mode !== CameraBackgroundMode.CUSTOM) {
		return frames === null;
	}
	if (background.customMediaKind !== WebCameraEffectCustomMediaKind.VIDEO) {
		return frames === null;
	}
	if (!requireVideoFrames && frames === null) {
		return true;
	}
	return frames instanceof ReadableStream;
}

function isValidStartCommand(value: object): value is WebCameraEffectStartCommand {
	if (typeof Reflect.get(value, 'preferWebGPU') !== 'boolean') {
		return false;
	}
	const config = Reflect.get(value, 'config');
	if (!isWebCameraPipelineConfig(config)) {
		return false;
	}
	if (!(Reflect.get(value, 'readable') instanceof ReadableStream)) {
		return false;
	}
	if (!(Reflect.get(value, 'gpuCanvas') instanceof OffscreenCanvas)) {
		return false;
	}
	if (!(Reflect.get(value, 'fallbackCanvas') instanceof OffscreenCanvas)) {
		return false;
	}
	return hasExpectedCustomBackgroundFrames(config, Reflect.get(value, 'customBackgroundFrames'), true);
}

function isValidUpdateRequestId(value: unknown): value is number {
	if (!Number.isSafeInteger(value)) {
		return false;
	}
	if ((value as number) <= 0) {
		return false;
	}
	return (value as number) <= WEB_CAMERA_EFFECT_UPDATE_REQUEST_ID_MAX;
}

function isValidUpdateCommand(value: object): value is WebCameraEffectUpdateCommand {
	if (!isValidUpdateRequestId(Reflect.get(value, 'requestId'))) {
		return false;
	}
	const config = Reflect.get(value, 'config');
	if (!isWebCameraPipelineConfig(config)) {
		return false;
	}
	return hasExpectedCustomBackgroundFrames(config, Reflect.get(value, 'customBackgroundFrames'), false);
}

export interface WebCameraEffectStartCommand {
	readonly kind: typeof WebCameraEffectCommandKind.START;
	readonly readable: ReadableStream<VideoFrame>;
	readonly customBackgroundFrames: ReadableStream<VideoFrame> | null;
	readonly gpuCanvas: OffscreenCanvas;
	readonly fallbackCanvas: OffscreenCanvas;
	readonly config: WebCameraPipelineConfig;
	readonly preferWebGPU: boolean;
}

export interface WebCameraEffectUpdateCommand {
	readonly kind: typeof WebCameraEffectCommandKind.UPDATE;
	readonly requestId: number;
	readonly config: WebCameraPipelineConfig;
	readonly customBackgroundFrames: ReadableStream<VideoFrame> | null;
}

export interface WebCameraEffectStopCommand {
	readonly kind: typeof WebCameraEffectCommandKind.STOP;
}

export type WebCameraEffectWorkerCommand =
	| WebCameraEffectStartCommand
	| WebCameraEffectUpdateCommand
	| WebCameraEffectStopCommand;

export const WebCameraEffectCommandPolicy = Object.freeze({
	isValid(value: object): value is WebCameraEffectWorkerCommand {
		switch (Reflect.get(value, 'kind')) {
			case WebCameraEffectCommandKind.START:
				return isValidStartCommand(value);
			case WebCameraEffectCommandKind.UPDATE:
				return isValidUpdateCommand(value);
			case WebCameraEffectCommandKind.STOP:
				return true;
			default:
				return false;
		}
	},
});

export interface WebCameraEffectReadyEvent {
	readonly kind: typeof WebCameraEffectEventKind.READY;
	readonly backend: WebCameraEffectBackend;
	readonly fallbackErrorType: ErrorDiagnosticType | null;
}

export interface WebCameraEffectUpdatedEvent {
	readonly kind: typeof WebCameraEffectEventKind.UPDATED;
	readonly requestId: number;
}

export interface WebCameraEffectUpdateFailedEvent extends ErrorDiagnostic {
	readonly kind: typeof WebCameraEffectEventKind.UPDATE_FAILED;
	readonly requestId: number;
}

export interface WebCameraEffectFailedEvent extends ErrorDiagnostic {
	readonly kind: typeof WebCameraEffectEventKind.FAILED;
}

export interface WebCameraEffectStoppedEvent {
	readonly kind: typeof WebCameraEffectEventKind.STOPPED;
	readonly reason: WebCameraEffectShutdownReason;
	readonly diagnostic: ErrorDiagnostic | null;
}

export type WebCameraEffectWorkerEvent =
	| WebCameraEffectReadyEvent
	| WebCameraEffectUpdatedEvent
	| WebCameraEffectUpdateFailedEvent
	| WebCameraEffectFailedEvent
	| WebCameraEffectStoppedEvent;
