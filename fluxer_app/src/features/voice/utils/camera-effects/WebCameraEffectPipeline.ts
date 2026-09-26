// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	collectSettledFailures,
	throwCollectedFailures,
} from '@app/features/voice/utils/camera-effects/AggregateOperations';
import {
	CameraBackgroundMode,
	clampVideoFrameRate,
} from '@app/features/voice/utils/camera-effects/CameraCaptureContract';
import {
	type ErrorDiagnostic,
	type ErrorDiagnosticType,
	getErrorDiagnosticType,
	isErrorDiagnostic,
	isErrorDiagnosticType,
} from '@app/features/voice/utils/camera-effects/ErrorDiagnostic';
import {createTrackProcessor} from '@app/features/voice/utils/camera-effects/MediaStreamTrackProcessorPolyfill';
import {detectWebCameraSegmentationCapability} from '@app/features/voice/utils/camera-effects/WebCameraBackgroundSupport';
import {
	isWebCameraEffectShutdownReason,
	validateCameraEffectFrameDimensions,
	WEB_CAMERA_EFFECT_UPDATE_REQUEST_ID_MAX,
	WebCameraEffectBackend,
	WebCameraEffectCommandKind,
	WebCameraEffectCustomMediaKind,
	WebCameraEffectEventKind,
	WebCameraEffectShutdownReason,
	type WebCameraEffectStartCommand,
	type WebCameraEffectUpdateCommand,
	type WebCameraEffectUpdateFailedEvent,
	type WebCameraEffectWorkerEvent,
	type WebCameraPipelineConfig,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectProtocol';
import {
	createWebCameraEffectVideoFrameProducer,
	type WebCameraEffectVideoFrameProducer,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectVideoFrameSource';

const VIDEO_TRACK_KIND = 'video';

class InvalidCameraEffectReadyEventError extends Error {
	constructor() {
		super('Camera effect worker emitted an invalid ready event');
		this.name = 'InvalidCameraEffectReadyEventError';
	}
}

const logger = new Logger('WebCameraEffectPipeline');

const DEFAULT_OUTPUT_FRAME_RATE = 30;
const INITIALIZATION_TIMEOUT_MS = 30_000;
const SHUTDOWN_TIMEOUT_MS = 2_000;
const UPDATE_TIMEOUT_MS = 12_000;

const WebGPUProbeState = Object.freeze({
	UNKNOWN: 'unknown',
	PROBING: 'probing',
	ENABLED: 'enabled',
	DISABLED: 'disabled',
} as const);

type WebGPUProbeState = (typeof WebGPUProbeState)[keyof typeof WebGPUProbeState];

let webGPUProbeState: WebGPUProbeState = WebGPUProbeState.UNKNOWN;

interface CapturedSurface {
	readonly offscreen: OffscreenCanvas;
	readonly track: MediaStreamTrack;
}

interface InitializationResult {
	readonly backend: WebCameraEffectBackend;
	readonly fallbackErrorType: ErrorDiagnosticType | null;
}

interface PendingUpdate {
	readonly requestId: number;
	readonly config: WebCameraPipelineConfig;
	readonly candidateVideoProducer: WebCameraEffectVideoFrameProducer | null;
	readonly ownsCandidateVideoProducer: boolean;
	readonly resolve: () => void;
	readonly reject: (error: Error) => void;
	readonly timeout: number;
}

export interface WebCameraPipeline {
	readonly outputTrack: MediaStreamTrack;
	readonly backend: WebCameraEffectBackend;
	assertActive(): void;
	updateConfig(config: WebCameraPipelineConfig): Promise<void>;
	beginStop(): void;
	stop(): void;
}

export interface WebCameraEffectPipelineCreateRequest {
	readonly source: MediaStreamTrack;
	readonly config: WebCameraPipelineConfig;
	readonly onFailure: (pipeline: WebCameraPipeline, error: Error) => void;
}

function parseCameraEffectWorkerEvent(value: unknown): WebCameraEffectWorkerEvent {
	if (value == null || typeof value !== 'object') {
		throw new Error('Camera effect worker emitted a non-object event');
	}
	const record = value as Record<string, unknown>;
	if (record.kind === WebCameraEffectEventKind.READY) {
		if (
			record.backend !== WebCameraEffectBackend.WEB_GPU &&
			record.backend !== WebCameraEffectBackend.WASM_WORKER &&
			record.backend !== WebCameraEffectBackend.CANVAS_WORKER
		) {
			throw new InvalidCameraEffectReadyEventError();
		}
		if (!('fallbackErrorType' in record)) {
			throw new InvalidCameraEffectReadyEventError();
		}
		const fallbackErrorType = record.fallbackErrorType;
		let resolvedFallbackErrorType: ErrorDiagnosticType | null = null;
		if (!Object.is(fallbackErrorType, null)) {
			if (!isErrorDiagnosticType(fallbackErrorType)) {
				throw new InvalidCameraEffectReadyEventError();
			}
			resolvedFallbackErrorType = fallbackErrorType;
		}
		return {
			kind: WebCameraEffectEventKind.READY,
			backend: record.backend,
			fallbackErrorType: resolvedFallbackErrorType,
		};
	}
	if (record.kind === WebCameraEffectEventKind.UPDATED) {
		if (!isValidUpdateRequestId(record.requestId)) {
			throw new Error('Camera effect worker emitted an invalid update acknowledgement');
		}
		return {kind: WebCameraEffectEventKind.UPDATED, requestId: record.requestId};
	}
	if (record.kind === WebCameraEffectEventKind.UPDATE_FAILED) {
		if (!isValidUpdateRequestId(record.requestId) || !isErrorDiagnostic(record)) {
			throw new Error('Camera effect worker emitted an invalid update rejection');
		}
		return {
			kind: WebCameraEffectEventKind.UPDATE_FAILED,
			requestId: record.requestId,
			errorType: record.errorType,
			message: record.message,
			stack: record.stack,
		};
	}
	if (record.kind === WebCameraEffectEventKind.FAILED) {
		if (!isErrorDiagnostic(record)) {
			throw new Error('Camera effect worker emitted an invalid failure event');
		}
		return {
			kind: WebCameraEffectEventKind.FAILED,
			errorType: record.errorType,
			message: record.message,
			stack: record.stack,
		};
	}
	if (record.kind === WebCameraEffectEventKind.STOPPED) {
		const reason = record.reason;
		if (!isWebCameraEffectShutdownReason(reason)) {
			throw new Error('Camera effect worker emitted a stop event without a valid reason');
		}
		return {
			kind: WebCameraEffectEventKind.STOPPED,
			reason,
			diagnostic: parseOptionalErrorDiagnostic(record.diagnostic),
		};
	}
	throw new Error('Camera effect worker emitted an unknown event');
}

function parseOptionalErrorDiagnostic(value: unknown): ErrorDiagnostic | null {
	if (value == null) {
		return null;
	}
	if (!isErrorDiagnostic(value)) {
		throw new Error('Camera effect worker emitted an invalid stop diagnostic');
	}
	return {errorType: value.errorType, message: value.message, stack: value.stack};
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

function createWorkerReportedError(event: ErrorDiagnostic, context: string): Error {
	const error = new Error(`${context}: ${event.message}`);
	if (event.stack != null) {
		error.stack = `${error.stack ?? error.message}\nWorker stack:\n${event.stack}`;
	}
	return error;
}

function throwAfterTrackCleanup(error: unknown, tracks: ReadonlyArray<MediaStreamTrack>, message: string): never {
	const failures: Array<unknown> = [error];
	for (const track of tracks) {
		try {
			track.stop();
		} catch (cleanupError) {
			failures.push(cleanupError);
		}
	}
	throwCollectedFailures({failures, message: message});
	throw error;
}

function createCapturedSurface(frameRate: number, width: number, height: number): CapturedSurface {
	const canvas = document.createElement('canvas');
	canvas.width = width;
	canvas.height = height;
	const stream = canvas.captureStream(frameRate);
	const track = stream.getVideoTracks()[0];
	if (track == null) {
		throw new Error('Camera effect canvas capture produced no video track');
	}
	try {
		return {offscreen: canvas.transferControlToOffscreen(), track};
	} catch (error) {
		throwAfterTrackCleanup(error, [track], 'Camera effect surface creation failed during cleanup');
	}
}

function initialSurfaceDimension(value: number | null): number {
	if (value == null) {
		return 1;
	}
	if (!Number.isSafeInteger(value)) {
		return 1;
	}
	if (value <= 0) {
		return 1;
	}
	return value;
}

function hasVideoBackground(config: WebCameraPipelineConfig): boolean {
	const background = config.background;
	if (background == null || background.mode !== CameraBackgroundMode.CUSTOM) {
		return false;
	}
	return background.customMediaKind === WebCameraEffectCustomMediaKind.VIDEO;
}

function hasSameVideoBackground(current: WebCameraPipelineConfig, next: WebCameraPipelineConfig): boolean {
	if (!hasVideoBackground(current) || !hasVideoBackground(next)) {
		return false;
	}
	const currentBackground = current.background;
	const nextBackground = next.background;
	if (currentBackground?.mode !== CameraBackgroundMode.CUSTOM) {
		return false;
	}
	if (nextBackground?.mode !== CameraBackgroundMode.CUSTOM) {
		return false;
	}
	return currentBackground.customMediaURL === nextBackground.customMediaURL;
}

async function createVideoFrameProducer(
	config: WebCameraPipelineConfig,
): Promise<WebCameraEffectVideoFrameProducer | null> {
	const background = config.background;
	if (background == null || background.mode !== CameraBackgroundMode.CUSTOM) {
		return null;
	}
	if (background.customMediaKind !== WebCameraEffectCustomMediaKind.VIDEO) {
		return null;
	}
	return createWebCameraEffectVideoFrameProducer(background.customMediaURL);
}

function selectWebGPUPreference(config: WebCameraPipelineConfig): boolean {
	if (config.background == null) {
		return false;
	}
	if (webGPUProbeState === WebGPUProbeState.UNKNOWN) {
		webGPUProbeState = WebGPUProbeState.PROBING;
		return true;
	}
	return webGPUProbeState === WebGPUProbeState.ENABLED;
}

function recordWebGPUSelection(preferred: boolean, backend: WebCameraEffectBackend): void {
	if (!preferred) {
		return;
	}
	if (backend === WebCameraEffectBackend.WEB_GPU) {
		webGPUProbeState = WebGPUProbeState.ENABLED;
		return;
	}
	webGPUProbeState = WebGPUProbeState.DISABLED;
}

function releaseWebGPUProbe(preferred: boolean): void {
	if (preferred && webGPUProbeState === WebGPUProbeState.PROBING) {
		webGPUProbeState = WebGPUProbeState.UNKNOWN;
	}
}

export class WebCameraEffectPipeline implements WebCameraPipeline {
	private readonly worker: Worker;
	private readonly gpuTrack: MediaStreamTrack;
	private readonly fallbackTrack: MediaStreamTrack;
	private readonly onFailure: (pipeline: WebCameraPipeline, error: Error) => void;
	private readonly preferWebGPU: boolean;
	private readonly initializationPromise: Promise<InitializationResult>;
	private readonly resolveInitialization: (result: InitializationResult) => void;
	private readonly rejectInitialization: (error: Error) => void;
	private initializationTimeout: number | null;
	private shutdownTimeout: number | null = null;
	private selectedTrack: MediaStreamTrack | null = null;
	private selectedBackend: WebCameraEffectBackend | null = null;
	private operationalFailure: Error | null = null;
	private currentConfig: WebCameraPipelineConfig;
	private currentVideoProducer: WebCameraEffectVideoFrameProducer | null = null;
	private pendingUpdate: PendingUpdate | null = null;
	private nextUpdateRequestId = 1;
	private updatePreparationActive = false;
	private stopped = false;
	private stopIntent = false;
	private initialized = false;

	private constructor(
		worker: Worker,
		GPUTrack: MediaStreamTrack,
		fallbackTrack: MediaStreamTrack,
		onFailure: (pipeline: WebCameraPipeline, error: Error) => void,
		config: WebCameraPipelineConfig,
		preferWebGPU: boolean,
	) {
		this.worker = worker;
		this.gpuTrack = GPUTrack;
		this.fallbackTrack = fallbackTrack;
		this.onFailure = onFailure;
		this.currentConfig = config;
		this.preferWebGPU = preferWebGPU;
		let capturedResolveInitialization: ((result: InitializationResult) => void) | null = null;
		let capturedRejectInitialization: ((error: Error) => void) | null = null;
		this.initializationPromise = new Promise<InitializationResult>((resolve, reject) => {
			capturedResolveInitialization = resolve;
			capturedRejectInitialization = reject;
		});
		const resolveInitialization = capturedResolveInitialization;
		if (resolveInitialization == null) {
			throw new Error('camera_effect_initialization_resolve_not_captured');
		}
		const rejectInitialization = capturedRejectInitialization;
		if (rejectInitialization == null) {
			throw new Error('camera_effect_initialization_reject_not_captured');
		}
		this.resolveInitialization = resolveInitialization;
		this.rejectInitialization = rejectInitialization;
		this.initializationTimeout = window.setTimeout(() => {
			this.failInitialization(new Error('Camera effect worker initialization timed out'));
		}, INITIALIZATION_TIMEOUT_MS);
		worker.addEventListener('message', this.handleWorkerMessage);
		worker.addEventListener('error', this.handleWorkerError);
		worker.addEventListener('messageerror', this.handleWorkerMessageError);
	}

	get outputTrack(): MediaStreamTrack {
		if (this.selectedTrack == null) {
			throw new Error('Camera effect output track requested before initialization');
		}
		this.assertActive();
		return this.selectedTrack;
	}

	get backend(): WebCameraEffectBackend {
		if (this.selectedBackend == null) {
			throw new Error('Camera effect backend requested before initialization');
		}
		this.assertActive();
		return this.selectedBackend;
	}

	static async create({
		source,
		config,
		onFailure,
	}: WebCameraEffectPipelineCreateRequest): Promise<WebCameraEffectPipeline> {
		if (source.kind !== VIDEO_TRACK_KIND) {
			throw new Error('Camera effect pipeline requires a video track');
		}
		const capability = detectWebCameraSegmentationCapability();
		if (!capability.available) {
			throw new Error(capability.reason);
		}
		const settings = source.getSettings();
		let configuredFrameRate = settings.frameRate;
		if (configuredFrameRate == null) {
			configuredFrameRate = DEFAULT_OUTPUT_FRAME_RATE;
		}
		const frameRate = clampVideoFrameRate(configuredFrameRate);
		let configuredWidth: number | null = null;
		if (settings.width != null) {
			configuredWidth = settings.width;
		}
		let configuredHeight: number | null = null;
		if (settings.height != null) {
			configuredHeight = settings.height;
		}
		const width = initialSurfaceDimension(configuredWidth);
		const height = initialSurfaceDimension(configuredHeight);
		validateCameraEffectFrameDimensions(width, height);
		const GPUSurface = createCapturedSurface(frameRate, width, height);
		let fallbackSurface: CapturedSurface;
		try {
			fallbackSurface = createCapturedSurface(frameRate, width, height);
		} catch (error) {
			throwAfterTrackCleanup(
				error,
				[GPUSurface.track],
				'Camera effect fallback surface creation failed during cleanup',
			);
		}
		let worker: Worker;
		try {
			worker = new Worker(new URL('./WebCameraEffectWorker.ts', import.meta.url), {type: 'module'});
		} catch (error) {
			throwAfterTrackCleanup(
				error,
				[GPUSurface.track, fallbackSurface.track],
				'Camera effect worker creation failed during cleanup',
			);
		}
		const preferWebGPU = selectWebGPUPreference(config);
		const pipeline = new WebCameraEffectPipeline(
			worker,
			GPUSurface.track,
			fallbackSurface.track,
			onFailure,
			config,
			preferWebGPU,
		);
		let readable: ReadableStream<VideoFrame> | null = null;
		try {
			pipeline.currentVideoProducer = await createVideoFrameProducer(config);
			readable = createTrackProcessor<VideoFrame>(source).readable;
			const customBackgroundFrames = pipeline.currentVideoProducer?.readable ?? null;
			const command: WebCameraEffectStartCommand = {
				kind: WebCameraEffectCommandKind.START,
				readable,
				customBackgroundFrames,
				gpuCanvas: GPUSurface.offscreen,
				fallbackCanvas: fallbackSurface.offscreen,
				config,
				preferWebGPU,
			};
			const transfer: Array<Transferable> = [readable, GPUSurface.offscreen, fallbackSurface.offscreen];
			if (customBackgroundFrames != null) {
				transfer.push(customBackgroundFrames);
			}
			worker.postMessage(command, transfer);
		} catch (error) {
			const primaryError = new Error('Camera effect pipeline could not prepare or transfer its input streams', {
				cause: error,
			});
			let failures: ReadonlyArray<unknown> = [];
			if (readable != null) {
				failures = await collectSettledFailures([readable.cancel(primaryError)]);
			}
			let initializationError: Error = primaryError;
			if (failures.length > 0) {
				initializationError = new AggregateError(
					[primaryError, ...failures],
					'Camera effect pipeline initialization cleanup failed',
				);
			}
			pipeline.failInitialization(initializationError);
		}
		const initialization = await pipeline.initializationPromise;
		if (initialization.fallbackErrorType != null) {
			logger.debug('voice.camera_effect_backend.fallback_selected', {
				errorType: initialization.fallbackErrorType,
			});
		} else {
			logger.info('voice.camera_effect_worker.initialized', {backend: initialization.backend});
		}
		return pipeline;
	}

	assertActive(): void {
		if (this.operationalFailure != null) {
			throw this.operationalFailure;
		}
		if (this.stopped || !this.initialized) {
			throw new Error('Camera effect pipeline is inactive');
		}
	}

	async updateConfig(config: WebCameraPipelineConfig): Promise<void> {
		this.assertActive();
		if (this.updatePreparationActive || this.pendingUpdate != null) {
			throw new Error('Camera effect configuration update is already in progress');
		}
		this.updatePreparationActive = true;
		let candidateVideoProducer: WebCameraEffectVideoFrameProducer | null = null;
		let ownsCandidateVideoProducer = false;
		try {
			if (hasSameVideoBackground(this.currentConfig, config)) {
				candidateVideoProducer = this.currentVideoProducer;
			} else {
				candidateVideoProducer = await createVideoFrameProducer(config);
				ownsCandidateVideoProducer = candidateVideoProducer != null;
			}
			this.assertActive();
		} catch (error) {
			if (ownsCandidateVideoProducer) {
				candidateVideoProducer?.stop();
			}
			this.updatePreparationActive = false;
			throw error;
		}
		const requestId = this.takeUpdateRequestId();
		let resolveUpdate: (() => void) | null = null;
		let rejectUpdate: ((error: Error) => void) | null = null;
		const completion = new Promise<void>((resolve, reject) => {
			resolveUpdate = resolve;
			rejectUpdate = reject;
		});
		if (resolveUpdate == null || rejectUpdate == null) {
			if (ownsCandidateVideoProducer) {
				candidateVideoProducer?.stop();
			}
			this.updatePreparationActive = false;
			throw new Error('Camera effect update promise handlers were not captured');
		}
		const timeout = window.setTimeout(() => {
			const timeoutError = new Error('Camera effect configuration update timed out');
			this.rejectPendingUpdate(timeoutError);
			this.stopAfterWorkerFailure(timeoutError);
		}, UPDATE_TIMEOUT_MS);
		this.pendingUpdate = {
			requestId,
			config,
			candidateVideoProducer,
			ownsCandidateVideoProducer,
			resolve: resolveUpdate,
			reject: rejectUpdate,
			timeout,
		};
		this.updatePreparationActive = false;
		const customBackgroundFrames = ownsCandidateVideoProducer ? (candidateVideoProducer?.readable ?? null) : null;
		const command: WebCameraEffectUpdateCommand = {
			kind: WebCameraEffectCommandKind.UPDATE,
			requestId,
			config,
			customBackgroundFrames,
		};
		try {
			const transfer: Array<Transferable> = [];
			if (customBackgroundFrames != null) {
				transfer.push(customBackgroundFrames);
			}
			this.worker.postMessage(command, transfer);
		} catch (error) {
			const updateError = new Error('Camera effect worker could not receive its updated configuration', {
				cause: error,
			});
			this.rejectPendingUpdate(updateError);
			this.stopAfterWorkerFailure(updateError);
		}
		return completion;
	}

	private takeUpdateRequestId(): number {
		const requestId = this.nextUpdateRequestId;
		this.nextUpdateRequestId += 1;
		if (this.nextUpdateRequestId > WEB_CAMERA_EFFECT_UPDATE_REQUEST_ID_MAX) {
			this.nextUpdateRequestId = 1;
		}
		return requestId;
	}

	private completeUpdate(requestId: number): void {
		const pending = this.pendingUpdate;
		if (pending == null || pending.requestId !== requestId) {
			this.stopAfterWorkerFailure(new Error('Camera effect worker acknowledged an unknown update'));
			return;
		}
		window.clearTimeout(pending.timeout);
		this.pendingUpdate = null;
		const previousVideoProducer = this.currentVideoProducer;
		this.currentConfig = pending.config;
		this.currentVideoProducer = pending.candidateVideoProducer;
		pending.resolve();
		if (previousVideoProducer !== pending.candidateVideoProducer) {
			try {
				previousVideoProducer?.stop();
			} catch (error) {
				const cleanupError = new Error('Camera effect update committed but previous video source cleanup failed', {
					cause: error,
				});
				this.stopAfterWorkerFailure(cleanupError);
				return;
			}
		}
	}

	private rejectUpdate(event: WebCameraEffectUpdateFailedEvent): void {
		const pending = this.pendingUpdate;
		if (pending == null || pending.requestId !== event.requestId) {
			this.stopAfterWorkerFailure(new Error('Camera effect worker rejected an unknown update'));
			return;
		}
		this.rejectPendingUpdate(createWorkerReportedError(event, 'Camera effect configuration update failed'));
	}

	private rejectPendingUpdate(error: Error): void {
		const pending = this.pendingUpdate;
		if (pending == null) {
			return;
		}
		window.clearTimeout(pending.timeout);
		this.pendingUpdate = null;
		let rejection = error;
		if (pending.ownsCandidateVideoProducer) {
			try {
				pending.candidateVideoProducer?.stop();
			} catch (cleanupError) {
				rejection = new AggregateError([error, cleanupError], 'Camera effect update rejection cleanup failed');
			}
		}
		pending.reject(rejection);
	}

	beginStop(): void {
		if (this.stopIntent) {
			return;
		}
		this.stopIntent = true;
		const failures: Array<unknown> = [];
		const operations = [
			() => {
				if (this.selectedTrack != null) {
					this.selectedTrack.removeEventListener('ended', this.handleOutputTrackEnded);
				}
			},
			() => this.worker.postMessage({kind: WebCameraEffectCommandKind.STOP}),
		];
		for (const operation of operations) {
			try {
				operation();
			} catch (error) {
				failures.push(error);
			}
		}
		throwCollectedFailures({failures, message: 'Camera effect pipeline stop intent failed'});
	}

	stop(): void {
		if (this.stopped) {
			return;
		}
		if (!this.initialized) {
			this.failInitialization(new Error('Camera effect pipeline stopped during initialization'));
			return;
		}
		this.stopped = true;
		const failures: Array<unknown> = [];
		const operations = [
			() => this.beginStop(),
			() => this.rejectPendingUpdate(new Error('Camera effect pipeline stopped during configuration update')),
			() => {
				this.currentVideoProducer?.stop();
				this.currentVideoProducer = null;
			},
			() => this.gpuTrack.stop(),
			() => this.fallbackTrack.stop(),
			() => {
				this.shutdownTimeout = window.setTimeout(() => {
					try {
						this.terminateWorker();
					} catch (error) {
						logger.error('voice.camera_effect_worker_termination.failed', {
							errorType: getErrorDiagnosticType(error),
						});
					}
				}, SHUTDOWN_TIMEOUT_MS);
			},
		];
		for (const operation of operations) {
			try {
				operation();
			} catch (error) {
				failures.push(error);
			}
		}
		throwCollectedFailures({failures, message: 'Camera effect pipeline shutdown failed'});
	}

	private readonly handleWorkerMessage = (event: MessageEvent<unknown>): void => {
		let message: WebCameraEffectWorkerEvent;
		try {
			message = parseCameraEffectWorkerEvent(event.data);
		} catch (error) {
			const eventError = new Error('Camera effect worker emitted an invalid event', {cause: error});
			if (!this.initialized) {
				this.failInitialization(eventError);
				return;
			}
			logger.error('voice.camera_effect_worker_event_invalid.rejected', {errorType: getErrorDiagnosticType(error)});
			this.stopAfterWorkerFailure(eventError);
			return;
		}
		if (message.kind === WebCameraEffectEventKind.READY) {
			this.completeInitialization(message);
			return;
		}
		if (message.kind === WebCameraEffectEventKind.UPDATED) {
			this.completeUpdate(message.requestId);
			return;
		}
		if (message.kind === WebCameraEffectEventKind.UPDATE_FAILED) {
			this.rejectUpdate(message);
			return;
		}
		if (message.kind === WebCameraEffectEventKind.FAILED) {
			const error = createWorkerReportedError(message, 'Camera effect worker failed');
			if (!this.initialized) {
				this.failInitialization(error);
				return;
			}
			if (this.stopped) {
				return;
			}
			this.rejectPendingUpdate(error);
			if (this.stopIntent) {
				logger.debug('voice.camera_effect_worker_failed_after_stop_intent.ignored', {
					errorType: message.errorType,
				});
				this.stop();
				return;
			}
			logger.error('voice.camera_effect_worker.failed', {
				errorType: message.errorType,
				message: message.message,
				stack: message.stack,
			});
			this.stopAfterWorkerFailure(error);
			return;
		}
		if (!this.initialized) {
			this.failInitialization(new Error('Camera effect worker stopped during initialization'));
			return;
		}
		if (message.reason === WebCameraEffectShutdownReason.CLEANUP_FAILED) {
			logger.warn('voice.camera_effect_worker_cleanup.incomplete', {
				errorType: message.diagnostic?.errorType ?? null,
				message: message.diagnostic?.message ?? null,
				stack: message.diagnostic?.stack ?? null,
			});
		} else if (message.reason !== WebCameraEffectShutdownReason.OWNER_STOP && !this.stopIntent && !this.stopped) {
			const error = new Error('Camera effect worker stopped unexpectedly');
			logger.error('voice.camera_effect_worker_stopped_unexpectedly.failed', {reason: message.reason});
			this.stopAfterWorkerFailure(error);
			return;
		}
		try {
			this.terminateWorker();
		} catch (error) {
			logger.error('voice.camera_effect_worker_termination.failed', {
				errorType: getErrorDiagnosticType(error),
			});
		}
	};

	private readonly handleWorkerError = (event: ErrorEvent): void => {
		const location = event.filename ? ` (${event.filename}:${event.lineno}:${event.colno})` : '';
		const detail = event.message ? `: ${event.message}${location}` : location;
		const error = new Error(`Camera effect worker raised an error${detail}`, {
			cause: event.error ?? undefined,
		});
		if (!this.initialized) {
			this.failInitialization(error);
			return;
		}
		if (!this.stopped) {
			logger.error('voice.camera_effect_worker.failed', {
				errorType: getErrorDiagnosticType(event.error),
				message: event.message,
				filename: event.filename,
				lineno: event.lineno,
				colno: event.colno,
			});
			this.stopAfterWorkerFailure(error);
		}
	};

	private readonly handleWorkerMessageError = (): void => {
		const error = new Error('Camera effect worker message could not be deserialized');
		if (!this.initialized) {
			this.failInitialization(error);
			return;
		}
		if (!this.stopped) {
			logger.error('voice.camera_effect_worker_message.failed');
			this.stopAfterWorkerFailure(error);
		}
	};

	private readonly handleOutputTrackEnded = (): void => {
		if (!this.initialized || this.stopped) {
			return;
		}
		this.stopAfterWorkerFailure(new Error('Camera effect output track ended unexpectedly'));
	};

	private completeInitialization(result: InitializationResult): void {
		if (this.initialized) {
			logger.error('voice.camera_effect_worker_sent_more_than_one_ready_event.failed');
			this.stopAfterWorkerFailure(new Error('Camera effect worker sent more than one ready event'));
			return;
		}
		if (this.stopped) {
			return;
		}
		recordWebGPUSelection(this.preferWebGPU, result.backend);
		let selectedTrack = this.fallbackTrack;
		let unusedTrack = this.gpuTrack;
		if (result.backend === WebCameraEffectBackend.WEB_GPU) {
			selectedTrack = this.gpuTrack;
			unusedTrack = this.fallbackTrack;
		}
		try {
			unusedTrack.stop();
			selectedTrack.addEventListener('ended', this.handleOutputTrackEnded, {once: true});
		} catch (error) {
			this.failInitialization(
				new Error('Camera effect pipeline could not release its unused output track', {cause: error}),
			);
			return;
		}
		this.initialized = true;
		this.selectedBackend = result.backend;
		this.selectedTrack = selectedTrack;
		this.clearInitializationTimeout();
		this.resolveInitialization(result);
	}

	private stopAfterWorkerFailure(error: Error): void {
		if (this.operationalFailure != null || this.stopped) {
			return;
		}
		this.operationalFailure = error;
		const failures: Array<unknown> = [];
		try {
			this.onFailure(this, error);
		} catch (callbackError) {
			failures.push(callbackError);
		}
		try {
			this.stop();
		} catch (cleanupError) {
			failures.push(cleanupError);
		}
		if (failures.length > 0) {
			const failure = new AggregateError(
				[error, ...failures],
				'Camera effect worker failure notification or cleanup failed',
			);
			logger.error('voice.camera_effect_worker_failure_cleanup.failed', {
				errorType: getErrorDiagnosticType(failure),
			});
		}
	}

	private failInitialization(error: Error): void {
		if (this.initialized || this.stopped) {
			return;
		}
		this.stopped = true;
		this.clearInitializationTimeout();
		releaseWebGPUProbe(this.preferWebGPU);
		const failures: Array<unknown> = [error];
		const cleanup = [
			() => {
				this.currentVideoProducer?.stop();
				this.currentVideoProducer = null;
			},
			() => this.gpuTrack.stop(),
			() => this.fallbackTrack.stop(),
			() => this.terminateWorker(),
		];
		for (const operation of cleanup) {
			try {
				operation();
			} catch (cleanupError) {
				failures.push(cleanupError);
			}
		}
		if (failures.length === 1) {
			this.rejectInitialization(error);
			return;
		}
		this.rejectInitialization(
			new AggregateError(failures, 'Camera effect pipeline initialization failed during cleanup'),
		);
	}

	private clearInitializationTimeout(): void {
		if (this.initializationTimeout != null) {
			window.clearTimeout(this.initializationTimeout);
			this.initializationTimeout = null;
		}
	}

	private terminateWorker(): void {
		if (this.shutdownTimeout != null) {
			window.clearTimeout(this.shutdownTimeout);
			this.shutdownTimeout = null;
		}
		const failures: Array<unknown> = [];
		const cleanup = [
			() => this.worker.removeEventListener('message', this.handleWorkerMessage),
			() => this.worker.removeEventListener('error', this.handleWorkerError),
			() => this.worker.removeEventListener('messageerror', this.handleWorkerMessageError),
			() => this.worker.terminate(),
		];
		for (const operation of cleanup) {
			try {
				operation();
			} catch (error) {
				failures.push(error);
			}
		}
		throwCollectedFailures({failures, message: 'Camera effect worker termination failed'});
	}
}
