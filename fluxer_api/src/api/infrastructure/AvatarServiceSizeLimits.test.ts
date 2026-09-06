// SPDX-License-Identifier: AGPL-3.0-or-later

import {EMOJI_MAX_SIZE, STICKER_MAX_SIZE} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {LimitConfigSnapshot, LimitRule} from '@fluxer/limits/src/LimitTypes';
import {describe, expect, it} from 'vitest';
import {AvatarService} from './AvatarService';
import type {IMediaService} from './IMediaService';
import type {IStorageService} from './IStorageService';

function createSnapshot(rules: Array<LimitRule> = []): LimitConfigSnapshot {
	return {traitDefinitions: [], rules};
}

function createService(snapshot: LimitConfigSnapshot): AvatarService {
	const mediaService = {
		getMetadata: () => {
			throw new Error('getMetadata should not be reached for an oversized image');
		},
	} as unknown as IMediaService;
	const storageService = {} as unknown as IStorageService;
	return new AvatarService(storageService, mediaService, {getConfigSnapshot: () => snapshot});
}

function encodedImage(byteLength: number): string {
	return Buffer.alloc(byteLength, 1).toString('base64');
}

async function captureValidationError(promise: Promise<unknown>): Promise<InputValidationError> {
	try {
		await promise;
	} catch (error) {
		expect(error).toBeInstanceOf(InputValidationError);
		return error as InputValidationError;
	}
	throw new Error('expected the upload to be rejected');
}

describe('AvatarService emoji and sticker size ceilings', () => {
	it('rejects an emoji one byte over the built-in ceiling', async () => {
		const service = createService(createSnapshot());
		const error = await captureValidationError(
			service.processEmoji({
				errorPath: 'image',
				base64Image: encodedImage(EMOJI_MAX_SIZE + 1),
				guildFeatures: [],
			}),
		);
		expect(error.getLocalizedErrors()).toEqual([
			{path: 'image', code: ValidationErrorCodes.IMAGE_SIZE_EXCEEDS_LIMIT, variables: {maxSize: EMOJI_MAX_SIZE}},
		]);
	});
	it('rejects a sticker one byte over the built-in ceiling', async () => {
		const service = createService(createSnapshot());
		const error = await captureValidationError(
			service.processSticker({
				errorPath: 'image',
				base64Image: encodedImage(STICKER_MAX_SIZE + 1),
				guildFeatures: [],
			}),
		);
		expect(error.getLocalizedErrors()).toEqual([
			{path: 'image', code: ValidationErrorCodes.IMAGE_SIZE_EXCEEDS_LIMIT, variables: {maxSize: STICKER_MAX_SIZE}},
		]);
	});
	it('applies a configured emoji_max_size rule in guild scope', async () => {
		const service = createService(createSnapshot([{id: 'emoji-ceiling', limits: {emoji_max_size: 1024}}]));
		const error = await captureValidationError(
			service.processEmoji({errorPath: 'image', base64Image: encodedImage(2048), guildFeatures: []}),
		);
		expect(error.getLocalizedErrors()).toEqual([
			{path: 'image', code: ValidationErrorCodes.IMAGE_SIZE_EXCEEDS_LIMIT, variables: {maxSize: 1024}},
		]);
	});
	it('applies a configured sticker_max_size rule in guild scope', async () => {
		const service = createService(createSnapshot([{id: 'sticker-ceiling', limits: {sticker_max_size: 1024}}]));
		const error = await captureValidationError(
			service.processSticker({errorPath: 'image', base64Image: encodedImage(2048), guildFeatures: []}),
		);
		expect(error.getLocalizedErrors()).toEqual([
			{path: 'image', code: ValidationErrorCodes.IMAGE_SIZE_EXCEEDS_LIMIT, variables: {maxSize: 1024}},
		]);
	});
	it('applies a guild-feature-filtered emoji_max_size rule only to a guild that carries the feature', async () => {
		const rules: Array<LimitRule> = [
			{id: 'big-emoji', filters: {guildFeatures: ['BIG_EMOJI']}, limits: {emoji_max_size: EMOJI_MAX_SIZE * 2}},
		];
		const service = createService(createSnapshot(rules));
		const error = await captureValidationError(
			service.processEmoji({
				errorPath: 'image',
				base64Image: encodedImage(EMOJI_MAX_SIZE + 1),
				guildFeatures: [],
			}),
		);
		expect(error.getLocalizedErrors()).toEqual([
			{path: 'image', code: ValidationErrorCodes.IMAGE_SIZE_EXCEEDS_LIMIT, variables: {maxSize: EMOJI_MAX_SIZE}},
		]);
		await expect(
			service.processEmoji({
				errorPath: 'image',
				base64Image: encodedImage(EMOJI_MAX_SIZE + 1),
				guildFeatures: ['BIG_EMOJI'],
			}),
		).rejects.toThrow('getMetadata should not be reached for an oversized image');
	});
});
