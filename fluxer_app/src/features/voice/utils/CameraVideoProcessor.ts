// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	WebCameraEffectPipeline,
	type WebCameraPipeline,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectPipeline';
import type {WebCameraEffectConfig} from '@app/features/voice/utils/camera-effects/WebCameraEffectProtocol';
import type {Track, TrackProcessor, VideoProcessorOptions} from 'livekit-client';

const logger = new Logger('CameraVideoProcessor');

type VideoTrackProcessor = TrackProcessor<Track.Kind.Video, VideoProcessorOptions>;

export interface CameraVideoProcessorOptions {
	background?: WebCameraEffectConfig | null;
}

export interface CameraVideoProcessorHandle extends VideoTrackProcessor {
	canUpdate(options: CameraVideoProcessorOptions): boolean;
	update(options: CameraVideoProcessorOptions): Promise<void>;
	setOperationalFailureHandler(handler: (error: Error) => void): void;
}

class CameraVideoProcessor implements CameraVideoProcessorHandle {
	name = 'camera-video-processor';
	processedTrack?: MediaStreamTrack;
	private backgroundPipeline: WebCameraPipeline | null = null;
	private currentOptions: CameraVideoProcessorOptions;
	private operationTail: Promise<void> = Promise.resolve();
	private destroyPromise: Promise<void> | null = null;
	private operationalFailureHandler: ((error: Error) => void) | null = null;
	private initialized = false;
	private destroyed = false;

	constructor(options: CameraVideoProcessorOptions) {
		this.currentOptions = options;
	}

	async init(opts: VideoProcessorOptions): Promise<void> {
		await this.runExclusive(async () => {
			if (this.initialized || this.destroyed) {
				throw new Error('Camera video processor cannot be initialized in its current lifecycle state');
			}
			await this.setup(opts);
			this.initialized = true;
		});
	}

	async restart(opts: VideoProcessorOptions): Promise<void> {
		await this.runExclusive(async () => {
			if (this.destroyed) {
				throw new Error('Cannot restart a destroyed camera video processor');
			}
			await this.teardown();
			this.initialized = false;
			await this.setup(opts);
			this.initialized = true;
		});
	}

	destroy(): Promise<void> {
		if (this.destroyPromise == null) {
			this.destroyPromise = this.runExclusive(async () => {
				if (this.destroyed) {
					return;
				}
				await this.teardown();
				this.initialized = false;
				this.destroyed = true;
			});
		}
		return this.destroyPromise;
	}

	canUpdate(options: CameraVideoProcessorOptions): boolean {
		const currentHasBackground = this.currentOptions.background != null;
		const nextHasBackground = options.background != null;
		return currentHasBackground === nextHasBackground;
	}

	setOperationalFailureHandler(handler: (error: Error) => void): void {
		if (this.operationalFailureHandler != null) {
			throw new Error('Camera video processor operational failure handler was already assigned');
		}
		this.operationalFailureHandler = handler;
	}

	async update(options: CameraVideoProcessorOptions): Promise<void> {
		await this.runExclusive(async () => {
			if (!this.initialized || this.destroyed) {
				throw new Error('Cannot update an inactive camera video processor');
			}
			if (!this.canUpdate(options)) {
				throw new Error('Camera video processor update would change its output topology');
			}
			const background = options.background ?? null;
			if (background != null) {
				const backgroundPipeline = this.backgroundPipeline;
				if (backgroundPipeline == null) {
					throw new Error('Camera video processor has no active background pipeline to update');
				}
				await backgroundPipeline.updateConfig({background});
			}
			this.currentOptions = options;
		});
	}

	private runExclusive<T>(operation: () => Promise<T>): Promise<T> {
		const result = this.operationTail.then(operation);
		this.operationTail = result.then(
			() => undefined,
			() => undefined,
		);
		return result;
	}

	private async teardown(): Promise<void> {
		const backgroundPipeline = this.backgroundPipeline;
		if (backgroundPipeline) {
			try {
				backgroundPipeline.beginStop();
			} catch (error) {
				logger.warn('Failed to signal camera background pipeline stop intent', {error});
			}
		}
		this.backgroundPipeline = null;
		this.processedTrack = undefined;
		if (backgroundPipeline) {
			try {
				backgroundPipeline.stop();
			} catch (error) {
				logger.warn('Failed to stop camera background pipeline', {error});
			}
		}
	}

	private async setup(opts: VideoProcessorOptions): Promise<void> {
		const background = this.currentOptions.background ?? null;
		try {
			if (background) {
				const backgroundPipeline = await WebCameraEffectPipeline.create({
					source: opts.track,
					config: {background},
					onFailure: (_pipeline, error) => {
						this.handleBackgroundPipelineFailure(error);
					},
				});
				this.backgroundPipeline = backgroundPipeline;
				this.processedTrack = backgroundPipeline.outputTrack;
				return;
			}
			this.processedTrack = undefined;
		} catch (error) {
			await this.teardown();
			throw error;
		}
	}

	private handleBackgroundPipelineFailure(error: unknown): void {
		const normalizedError =
			error instanceof Error ? error : new Error('Camera background pipeline failed', {cause: error});
		const handler = this.operationalFailureHandler;
		if (handler == null) {
			logger.error('Camera background pipeline failed without a recovery owner', {error: normalizedError});
			return;
		}
		handler(normalizedError);
	}
}

export function createCameraVideoProcessor(options: CameraVideoProcessorOptions): CameraVideoProcessorHandle {
	return new CameraVideoProcessor(options);
}

export function isCameraVideoProcessor(processor: unknown): processor is CameraVideoProcessorHandle {
	return processor instanceof CameraVideoProcessor;
}
