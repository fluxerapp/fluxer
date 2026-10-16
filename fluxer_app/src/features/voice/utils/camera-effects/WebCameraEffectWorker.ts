// SPDX-License-Identifier: AGPL-3.0-or-later

import {CameraBackgroundMode} from '@app/features/voice/utils/camera-effects/CameraCaptureContract';
import {
	type ErrorDiagnosticType,
	getErrorDiagnostic,
	getErrorDiagnosticType,
} from '@app/features/voice/utils/camera-effects/ErrorDiagnostic';
import {WebCameraEffectCanvasRenderer} from '@app/features/voice/utils/camera-effects/WebCameraEffectCanvasRenderer';
import {
	createWebCameraEffectVideoFrameSource,
	loadWebCameraEffectCustomFrameSource,
	type WebCameraEffectCustomFrameSource,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectCustomImage';
import {
	WEB_CAMERA_EFFECT_STOP_GRACE_MS,
	WebCameraEffectCommandKind,
	WebCameraEffectCommandPolicy,
	WebCameraEffectCustomMediaKind,
	WebCameraEffectEventKind,
	WebCameraEffectShutdownReason,
	type WebCameraEffectStartCommand,
	type WebCameraEffectUpdateCommand,
	type WebCameraEffectWorkerEvent,
	type WebCameraPipelineConfig,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectProtocol';
import type {WebCameraEffectRenderer} from '@app/features/voice/utils/camera-effects/WebCameraEffectRenderer';
import {WebCameraEffectWebGPURenderer} from '@app/features/voice/utils/camera-effects/WebCameraEffectWebGPURenderer';

type CameraEffectWorkerScope = DedicatedWorkerGlobalScope & {close(): void};

function requireCameraEffectWorkerScope(value: unknown): asserts value is CameraEffectWorkerScope {
	if (typeof value !== 'object' || value == null) {
		throw new Error('Camera effect worker global scope is unavailable');
	}
	if (!('postMessage' in value) || typeof value.postMessage !== 'function') {
		throw new Error('Camera effect worker global scope cannot post messages');
	}
	if (!('addEventListener' in value) || typeof value.addEventListener !== 'function') {
		throw new Error('Camera effect worker global scope cannot receive messages');
	}
	if (!('close' in value) || typeof value.close !== 'function') {
		throw new Error('Camera effect worker global scope cannot be closed');
	}
}

requireCameraEffectWorkerScope(self);
const workerScope = self;
__webpack_base_uri__ = workerScope.location.href;
const OPERATION_TIMEOUT_MS = 10_000;
const OPERATION_QUEUE_MAX = 8;
const DISPOSAL_TIMEOUT_MS = 1_500;

const WebCameraEffectLifecycle = Object.freeze({
	RUNNING: 'running',
	DRAINING: 'draining',
	STOPPING: 'stopping',
	CLOSED: 'closed',
} as const);

type WebCameraEffectLifecycle = (typeof WebCameraEffectLifecycle)[keyof typeof WebCameraEffectLifecycle];

function postEvent(event: WebCameraEffectWorkerEvent): void {
	workerScope.postMessage(event);
}

function postFailure(error: unknown): void {
	postEvent({kind: WebCameraEffectEventKind.FAILED, ...getErrorDiagnostic(error)});
}

function rejectCommand(error: unknown): void {
	try {
		postFailure(error);
	} finally {
		workerScope.close();
	}
}

class CameraEffectInputStreamEndedError extends Error {
	constructor() {
		super('Camera effect input stream ended');
		this.name = 'CameraEffectInputStreamEndedError';
	}
}

class CameraEffectDisposalTimeoutError extends Error {
	constructor() {
		super('Camera effect worker cleanup exceeded its deadline');
		this.name = 'CameraEffectDisposalTimeoutError';
	}
}

interface RendererSelection {
	readonly renderer: WebCameraEffectRenderer;
	readonly fallbackErrorType: ErrorDiagnosticType | null;
}

function hasSameCustomBackground(current: WebCameraPipelineConfig, next: WebCameraPipelineConfig): boolean {
	const currentBackground = current.background;
	const nextBackground = next.background;
	if (currentBackground == null || nextBackground == null) {
		return false;
	}
	if (currentBackground.mode !== CameraBackgroundMode.CUSTOM) {
		return false;
	}
	if (nextBackground.mode !== CameraBackgroundMode.CUSTOM) {
		return false;
	}
	if (currentBackground.customMediaURL !== nextBackground.customMediaURL) {
		return false;
	}
	return currentBackground.customMediaKind === nextBackground.customMediaKind;
}

async function createCustomFrameSource(
	config: WebCameraPipelineConfig,
	videoFrames: ReadableStream<VideoFrame> | null,
): Promise<WebCameraEffectCustomFrameSource | null> {
	const background = config.background;
	if (background == null || background.mode !== CameraBackgroundMode.CUSTOM) {
		return null;
	}
	let source: WebCameraEffectCustomFrameSource;
	if (background.customMediaKind === WebCameraEffectCustomMediaKind.VIDEO) {
		if (videoFrames == null) {
			throw new Error('Video camera background update requires a transferred frame stream');
		}
		source = await createWebCameraEffectVideoFrameSource(videoFrames);
	} else {
		source = await loadWebCameraEffectCustomFrameSource(background.customMediaURL);
	}
	const decodedKindMatches = source.kind === background.customMediaKind;
	const singleFrameAnimation =
		background.customMediaKind === WebCameraEffectCustomMediaKind.ANIMATED &&
		source.kind === WebCameraEffectCustomMediaKind.STATIC;
	if (decodedKindMatches || singleFrameAnimation) {
		return source;
	}
	let cleanupError: unknown;
	try {
		await source.dispose();
	} catch (error) {
		cleanupError = error;
	}
	const mismatchError = new Error('Custom camera background media kind does not match its decoded content');
	if (cleanupError !== undefined) {
		throw new AggregateError([mismatchError, cleanupError], 'Custom camera background rejection cleanup failed');
	}
	throw mismatchError;
}

async function selectRenderer(
	command: WebCameraEffectStartCommand,
	customFrameSource: WebCameraEffectCustomFrameSource | null,
): Promise<RendererSelection> {
	if (command.config.background == null) {
		return {
			renderer: await WebCameraEffectCanvasRenderer.create(command.fallbackCanvas, command.config, customFrameSource),
			fallbackErrorType: null,
		};
	}
	if (!command.preferWebGPU) {
		return {
			renderer: await WebCameraEffectCanvasRenderer.create(command.fallbackCanvas, command.config, customFrameSource),
			fallbackErrorType: null,
		};
	}
	try {
		return {
			renderer: await WebCameraEffectWebGPURenderer.create(command.gpuCanvas, command.config, customFrameSource),
			fallbackErrorType: null,
		};
	} catch (webGPUError) {
		try {
			return {
				renderer: await WebCameraEffectCanvasRenderer.create(command.fallbackCanvas, command.config, customFrameSource),
				fallbackErrorType: getErrorDiagnosticType(webGPUError),
			};
		} catch (WASMError) {
			throw new AggregateError([webGPUError, WASMError], 'Every camera effect renderer failed');
		}
	}
}

class WebCameraEffectWorkerController {
	private readonly reader: ReadableStreamDefaultReader<VideoFrame>;
	private operationTail: Promise<void> = Promise.resolve();
	private updatePreparationPromise: Promise<void> = Promise.resolve();
	private disposePromise: Promise<void> | null = null;
	private pendingOperations = 0;
	private updateInProgress = false;
	private lifecycle: WebCameraEffectLifecycle = WebCameraEffectLifecycle.RUNNING;
	private terminalReason: WebCameraEffectShutdownReason | null = null;
	private terminalDiagnosticError: unknown = null;
	private resolveOwnerStopIntent: (() => void) | null = null;
	private readonly ownerStopIntent: Promise<void>;

	constructor(
		private readonly renderer: WebCameraEffectRenderer,
		readable: ReadableStream<VideoFrame>,
		private config: WebCameraPipelineConfig,
		private customFrameSource: WebCameraEffectCustomFrameSource | null,
	) {
		this.reader = readable.getReader();
		let capturedResolveOwnerStopIntent: (() => void) | null = null;
		this.ownerStopIntent = new Promise<void>((resolve) => {
			capturedResolveOwnerStopIntent = resolve;
		});
		const resolveOwnerStopIntent = capturedResolveOwnerStopIntent;
		if (resolveOwnerStopIntent == null) {
			throw new Error('Camera effect worker stop intent resolver was not captured');
		}
		this.resolveOwnerStopIntent = resolveOwnerStopIntent;
	}

	private isShuttingDown(): boolean {
		return this.lifecycle !== WebCameraEffectLifecycle.RUNNING;
	}

	run(): void {
		this.pump().catch((error) => {
			this.fail(error);
		});
	}

	update(command: WebCameraEffectUpdateCommand): void {
		if (this.isShuttingDown()) {
			return;
		}
		if (this.updateInProgress) {
			postEvent({
				kind: WebCameraEffectEventKind.UPDATE_FAILED,
				requestId: command.requestId,
				...getErrorDiagnostic(new Error('Camera effect worker already has an update in progress')),
			});
			return;
		}
		this.updateInProgress = true;
		const preparation = this.prepareUpdate(command).finally(() => {
			this.updateInProgress = false;
		});
		this.updatePreparationPromise = preparation;
		preparation.catch((error) => {
			this.fail(error);
		});
	}

	private async prepareUpdate(command: WebCameraEffectUpdateCommand): Promise<void> {
		let nextCustomFrameSource = this.customFrameSource;
		let ownsCandidate = false;
		try {
			if (!hasSameCustomBackground(this.config, command.config)) {
				nextCustomFrameSource = await createCustomFrameSource(command.config, command.customBackgroundFrames);
				ownsCandidate = nextCustomFrameSource != null;
			}
		} catch (error) {
			const diagnosticError = await this.disposeRejectedCandidate(error, nextCustomFrameSource, ownsCandidate);
			this.postUpdateFailure(command.requestId, diagnosticError);
			return;
		}
		if (this.isShuttingDown()) {
			await this.disposeStoppedCandidate(nextCustomFrameSource, ownsCandidate);
			return;
		}
		let configurationError: unknown;
		let configurationFailed = false;
		let skipped = false;
		try {
			await this.enqueue(async () => {
				if (this.isShuttingDown()) {
					skipped = true;
					return;
				}
				try {
					await this.renderer.configure(command.config, nextCustomFrameSource);
				} catch (error) {
					configurationError = error;
					configurationFailed = true;
				}
			}, false);
		} catch (error) {
			const diagnosticError = await this.disposeRejectedCandidate(error, nextCustomFrameSource, ownsCandidate);
			this.postUpdateFailure(command.requestId, diagnosticError);
			return;
		}
		if (skipped) {
			await this.disposeStoppedCandidate(nextCustomFrameSource, ownsCandidate);
			return;
		}
		if (configurationFailed) {
			const diagnosticError = await this.disposeRejectedCandidate(
				configurationError,
				nextCustomFrameSource,
				ownsCandidate,
			);
			this.postUpdateFailure(command.requestId, diagnosticError);
			return;
		}
		const previousCustomFrameSource = this.customFrameSource;
		this.config = command.config;
		this.customFrameSource = nextCustomFrameSource;
		if (!this.isShuttingDown()) {
			postEvent({kind: WebCameraEffectEventKind.UPDATED, requestId: command.requestId});
		}
		if (previousCustomFrameSource !== nextCustomFrameSource) {
			try {
				await previousCustomFrameSource?.dispose();
			} catch (error) {
				throw new Error('Camera effect update committed but previous custom source cleanup failed', {cause: error});
			}
		}
	}

	private postUpdateFailure(requestId: number, error: unknown): void {
		if (this.isShuttingDown()) {
			return;
		}
		postEvent({
			kind: WebCameraEffectEventKind.UPDATE_FAILED,
			requestId,
			...getErrorDiagnostic(error),
		});
	}

	private async disposeStoppedCandidate(
		candidate: WebCameraEffectCustomFrameSource | null,
		ownsCandidate: boolean,
	): Promise<void> {
		if (!ownsCandidate || candidate == null) {
			return;
		}
		await candidate.dispose();
	}

	private async disposeRejectedCandidate(
		primaryError: unknown,
		candidate: WebCameraEffectCustomFrameSource | null,
		ownsCandidate: boolean,
	): Promise<unknown> {
		if (!ownsCandidate || candidate == null) {
			return primaryError;
		}
		try {
			await candidate.dispose();
			return primaryError;
		} catch (cleanupError) {
			return new AggregateError([primaryError, cleanupError], 'Camera effect update and candidate cleanup both failed');
		}
	}

	stop(): void {
		if (this.lifecycle === WebCameraEffectLifecycle.STOPPING || this.lifecycle === WebCameraEffectLifecycle.CLOSED) {
			this.resolveOwnerStopIntent?.();
			return;
		}
		if (this.terminalReason == null) {
			this.terminalReason = WebCameraEffectShutdownReason.OWNER_STOP;
		}
		this.lifecycle = WebCameraEffectLifecycle.STOPPING;
		this.resolveOwnerStopIntent?.();
		this.dispose().catch(() => {
			workerScope.close();
		});
	}

	private async pump(): Promise<void> {
		while (this.lifecycle === WebCameraEffectLifecycle.RUNNING) {
			const {value, done} = await this.reader.read();
			if (done || value == null) {
				await this.settleStreamEnd();
				break;
			}
			try {
				await this.enqueue(() => this.renderer.render(value, performance.now()));
			} finally {
				value.close();
			}
		}
		await this.dispose();
	}

	private async settleStreamEnd(): Promise<void> {
		if (this.isShuttingDown()) {
			return;
		}
		this.lifecycle = WebCameraEffectLifecycle.DRAINING;
		if (await this.awaitOwnerStopIntent()) {
			return;
		}
		throw new CameraEffectInputStreamEndedError();
	}

	private awaitOwnerStopIntent(): Promise<boolean> {
		return new Promise<boolean>((resolve) => {
			const timeout = setTimeout(() => {
				resolve(false);
			}, WEB_CAMERA_EFFECT_STOP_GRACE_MS);
			void this.ownerStopIntent.then(() => {
				clearTimeout(timeout);
				resolve(true);
			});
		});
	}

	private enqueue(operation: () => Promise<void>, enforceDeadline = true): Promise<void> {
		if (this.pendingOperations >= OPERATION_QUEUE_MAX) {
			return Promise.reject(new Error('Camera effect worker operation queue is full'));
		}
		this.pendingOperations += 1;
		const result = this.operationTail.then(() => this.executeOperation(operation, enforceDeadline));
		const trackedResult = result.finally(() => {
			this.pendingOperations -= 1;
		});
		this.operationTail = trackedResult.catch((error) => {
			this.fail(error);
		});
		return trackedResult;
	}

	private async executeOperation(operation: () => Promise<void>, enforceDeadline: boolean): Promise<void> {
		if (!enforceDeadline) {
			await operation();
			return;
		}
		const timeout = setTimeout(() => {
			this.fail(new Error('Camera effect worker operation exceeded its deadline'));
		}, OPERATION_TIMEOUT_MS);
		try {
			await operation();
		} finally {
			clearTimeout(timeout);
		}
	}

	private fail(error: unknown): void {
		if (this.terminalReason != null) {
			return;
		}
		this.terminalReason =
			error instanceof CameraEffectInputStreamEndedError
				? WebCameraEffectShutdownReason.INPUT_ENDED
				: WebCameraEffectShutdownReason.OPERATION_FAILED;
		try {
			postFailure(error);
		} finally {
			this.stop();
		}
	}

	private dispose(): Promise<void> {
		if (this.disposePromise == null) {
			const updateSettlement = this.updatePreparationPromise.then(
				() => [] as Array<unknown>,
				(error: unknown) => [error],
			);
			const resourceDisposal = updateSettlement.then(async (preparationFailures) => {
				await this.operationTail;
				const outcomes = await Promise.allSettled([
					this.renderer.dispose(),
					Promise.resolve().then(() => this.customFrameSource?.dispose()),
				]);
				const failures = [
					...preparationFailures,
					...outcomes.flatMap((outcome) => (outcome.status === 'rejected' ? [outcome.reason] : [])),
				];
				if (failures.length > 0) {
					throw this.resolveDisposalError(failures);
				}
			});
			this.disposePromise = Promise.allSettled([this.reader.cancel(), this.withDisposalDeadline(resourceDisposal)])
				.then((outcomes) => this.reportDisposalFailures(outcomes))
				.finally(() => {
					this.lifecycle = WebCameraEffectLifecycle.CLOSED;
					try {
						postEvent({
							kind: WebCameraEffectEventKind.STOPPED,
							reason: this.terminalReason ?? WebCameraEffectShutdownReason.OWNER_STOP,
							diagnostic:
								this.terminalDiagnosticError == null ? null : getErrorDiagnostic(this.terminalDiagnosticError),
						});
					} finally {
						workerScope.close();
					}
				});
		}
		return this.disposePromise;
	}

	private withDisposalDeadline(disposal: Promise<void>): Promise<void> {
		return new Promise<void>((resolve, reject) => {
			const timeout = setTimeout(() => {
				reject(new CameraEffectDisposalTimeoutError());
			}, DISPOSAL_TIMEOUT_MS);
			disposal.then(
				() => {
					clearTimeout(timeout);
					resolve();
				},
				(error: unknown) => {
					clearTimeout(timeout);
					reject(error);
				},
			);
		});
	}

	private reportDisposalFailures(outcomes: ReadonlyArray<PromiseSettledResult<void>>): void {
		const failures = outcomes
			.filter((outcome): outcome is PromiseRejectedResult => outcome.status === 'rejected')
			.map((outcome) => outcome.reason);
		if (failures.length === 0) {
			return;
		}
		const error = this.resolveDisposalError(failures);
		if (this.terminalReason === WebCameraEffectShutdownReason.OWNER_STOP) {
			this.terminalReason = WebCameraEffectShutdownReason.CLEANUP_FAILED;
			this.terminalDiagnosticError = error;
			return;
		}
		postFailure(error);
	}

	private resolveDisposalError(failures: ReadonlyArray<unknown>): unknown {
		if (failures.length === 1) {
			return failures[0];
		}
		return new AggregateError(failures, 'Camera effect worker cleanup failed');
	}
}

class WebCameraEffectWorkerEntry {
	private controller: WebCameraEffectWorkerController | null = null;
	private starting = false;

	install(): void {
		workerScope.addEventListener('message', (event: MessageEvent<unknown>) => {
			this.handle(event.data);
		});
	}

	handle(data: unknown): void {
		if (data == null || typeof data !== 'object') {
			rejectCommand(new Error('Camera effect worker received a non-object command'));
			return;
		}
		if (!WebCameraEffectCommandPolicy.isValid(data)) {
			rejectCommand(new Error('Camera effect worker received an unknown command'));
			return;
		}
		if (data.kind === WebCameraEffectCommandKind.START) {
			this.start(data).catch((error) => {
				rejectCommand(error);
			});
			return;
		}
		if (data.kind === WebCameraEffectCommandKind.UPDATE) {
			if (this.controller == null) {
				rejectCommand(new Error('Camera effect worker received update before start'));
				return;
			}
			this.controller.update(data);
			return;
		}
		if (data.kind === WebCameraEffectCommandKind.STOP) {
			if (this.controller == null) {
				rejectCommand(new Error('Camera effect worker received stop before start'));
				return;
			}
			this.controller.stop();
		}
	}

	private async start(command: WebCameraEffectStartCommand): Promise<void> {
		if (this.starting || this.controller != null) {
			throw new Error('Camera effect worker received more than one start command');
		}
		this.starting = true;
		const customFrameSource = await createCustomFrameSource(command.config, command.customBackgroundFrames);
		let selection: RendererSelection;
		try {
			selection = await selectRenderer(command, customFrameSource);
		} catch (error) {
			let cleanupError: unknown;
			try {
				await customFrameSource?.dispose();
			} catch (caughtCleanupError) {
				cleanupError = caughtCleanupError;
			}
			if (cleanupError !== undefined) {
				throw new AggregateError([error, cleanupError], 'Camera effect renderer selection cleanup failed');
			}
			throw error;
		}
		const nextController = new WebCameraEffectWorkerController(
			selection.renderer,
			command.readable,
			command.config,
			customFrameSource,
		);
		this.controller = nextController;
		postEvent({
			kind: WebCameraEffectEventKind.READY,
			backend: selection.renderer.backend,
			fallbackErrorType: selection.fallbackErrorType,
		});
		nextController.run();
	}
}

new WebCameraEffectWorkerEntry().install();
