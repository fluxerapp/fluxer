// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {getBackgroundMediaObjectURL} from '@app/features/theme/utils/BackgroundImageDB';
import VoiceSettings, {
	type BackgroundImage,
	BLUR_BACKGROUND_ID,
	NONE_BACKGROUND_ID,
} from '@app/features/voice/state/VoiceSettings';
import {
	type CameraVideoProcessorHandle,
	type CameraVideoProcessorOptions,
	createCameraVideoProcessor,
	isCameraVideoProcessor,
} from '@app/features/voice/utils/CameraVideoProcessor';
import {CameraBackgroundMode} from '@app/features/voice/utils/camera-effects/CameraCaptureContract';
import type {WebCameraEffectConfig} from '@app/features/voice/utils/camera-effects/WebCameraEffectProtocol';
import {areVoiceBackgroundsAvailable} from '@app/features/voice/utils/VoiceBackgroundAvailability';
import type {LocalVideoTrack} from 'livekit-client';

const logger = new Logger('VideoBackgroundProcessor');

export interface BackgroundProcessorOptions {
	backgroundImageId?: string;
	backgroundImages?: Array<BackgroundImage>;
	mirrorCamera?: boolean;
}

export interface AppliedBackgroundProcessor {
	destroy: () => Promise<void>;
}

export async function clearCameraVideoProcessor(track: LocalVideoTrack): Promise<void> {
	if (!track.getProcessor()) {
		return;
	}
	await track.stopProcessor(false);
	if (track.getProcessor()) {
		throw new Error('Camera video processor remained attached after it was cleared');
	}
	logger.info('Cleared background processor');
}

async function recoverRawCameraAfterProcessorFailure(
	track: LocalVideoTrack,
	processor: CameraVideoProcessorHandle,
	error: Error,
): Promise<void> {
	try {
		const recovered = await track.stopProcessorIfCurrent(processor, false);
		if (recovered) {
			logger.warn('Camera video processor failed; restored the raw camera track', {error});
		}
	} catch (recoveryError) {
		if (track.mediaStreamTrack.readyState === 'ended') {
			logger.warn('Camera video processor failed and the camera track was already gone during recovery', {
				error,
				recoveryError,
			});
			return;
		}
		logger.error('Camera video processor failed and raw camera recovery was incomplete', {
			error,
			recoveryError,
		});
	}
}

async function applyCameraVideoProcessor(
	track: LocalVideoTrack,
	options: CameraVideoProcessorOptions,
	logLabel: string,
): Promise<AppliedBackgroundProcessor> {
	const updatedProcessor = await track.runWithTrackChangeLock(async () => {
		const activeProcessor = track.getProcessor();
		if (!isCameraVideoProcessor(activeProcessor) || !activeProcessor.canUpdate(options)) {
			return null;
		}
		await activeProcessor.update(options);
		if (track.getProcessor() !== activeProcessor) {
			throw new Error('Updated camera video processor is no longer attached to the active track');
		}
		if (!activeProcessor.processedTrack || activeProcessor.processedTrack.readyState !== 'live') {
			throw new Error('Updated camera video processor has no live output track');
		}
		return activeProcessor;
	});
	if (updatedProcessor != null) {
		logger.info(logLabel);
		return updatedProcessor;
	}
	const processor = createCameraVideoProcessor(options);
	processor.setOperationalFailureHandler((error) => {
		void recoverRawCameraAfterProcessorFailure(track, processor, error);
	});
	try {
		await track.setProcessor(processor);
		if (track.getProcessor() !== processor) {
			throw new Error('Camera video processor was not retained by the active track');
		}
		if (!processor.processedTrack || processor.processedTrack.readyState !== 'live') {
			throw new Error('Camera video processor produced no live output track');
		}
		logger.info(logLabel);
		return processor;
	} catch (error) {
		try {
			if (track.getProcessor() === processor) {
				await track.stopProcessor(false);
			}
		} catch (cleanupError) {
			throw new AggregateError([error, cleanupError], 'Camera video processor apply and cleanup both failed');
		}
		throw error;
	}
}

async function resolveBackgroundEffectConfig(
	options?: BackgroundProcessorOptions,
): Promise<WebCameraEffectConfig | null> {
	const backgroundImageId = options?.backgroundImageId ?? VoiceSettings.getBackgroundImageId();
	if (backgroundImageId === NONE_BACKGROUND_ID) {
		return null;
	}
	if (!areVoiceBackgroundsAvailable()) {
		throw new Error('Camera background effects are unavailable');
	}
	const blurStrength = VoiceSettings.getBackgroundBlurStrength();
	if (backgroundImageId === BLUR_BACKGROUND_ID) {
		return {mode: CameraBackgroundMode.BLUR, blurStrength};
	}
	const backgroundImages = options?.backgroundImages ?? VoiceSettings.getBackgroundImages();
	const hasImage = backgroundImages.some((image) => image.id === backgroundImageId);
	if (!hasImage) {
		throw new Error(`Custom camera background is not present in the saved media list: ${backgroundImageId}`);
	}
	const customMedia = await getBackgroundMediaObjectURL(backgroundImageId);
	if (!customMedia) {
		throw new Error(`Custom camera background media could not be resolved: ${backgroundImageId}`);
	}
	return {
		mode: CameraBackgroundMode.CUSTOM,
		blurStrength,
		customMediaURL: customMedia.url,
		customMediaKind: customMedia.mediaKind,
	};
}

export async function applyCameraMirrorProcessor(
	track: LocalVideoTrack,
	mirrorCamera = VoiceSettings.getMirrorCamera(),
) {
	try {
		if (!mirrorCamera) {
			await clearCameraVideoProcessor(track);
			logger.debug('No camera mirror processor applied');
			return null;
		}
		return applyCameraVideoProcessor(track, {mirror: true}, 'Applied camera mirror');
	} catch (error) {
		logger.warn('Failed to apply camera mirror processor', error);
		throw error;
	}
}

export async function applyBackgroundProcessor(
	track: LocalVideoTrack,
	options?: BackgroundProcessorOptions,
): Promise<AppliedBackgroundProcessor | null> {
	try {
		const mirrorCamera = options?.mirrorCamera ?? VoiceSettings.getMirrorCamera();
		const background = await resolveBackgroundEffectConfig(options);
		if (!mirrorCamera && background == null) {
			await clearCameraVideoProcessor(track);
			logger.debug('No camera video processor applied');
			return null;
		}
		const logLabel = background == null ? 'Applied camera mirror' : `Applied camera background (${background.mode})`;
		return applyCameraVideoProcessor(track, {mirror: mirrorCamera, background}, logLabel);
	} catch (error) {
		logger.warn('Failed to apply camera video processor', error);
		throw error;
	}
}
