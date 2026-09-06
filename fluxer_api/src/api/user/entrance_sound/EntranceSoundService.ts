// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {
	ENTRANCE_SOUND_EXT_TO_MIME,
	ENTRANCE_SOUND_MAX_BYTES,
	ENTRANCE_SOUND_MAX_DURATION_MS,
	ENTRANCE_SOUND_MAX_PER_USER,
	ENTRANCE_SOUND_MIN_DURATION_MS,
	ENTRANCE_SOUND_NAME_MAX_LENGTH,
	type EntranceSoundExtension,
	entranceSoundExtensionFromFormat,
	entranceSoundExtensionFromMime,
	isValidEntranceSoundScopeId,
} from '@fluxer/constants/src/EntranceSoundConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {generateSnowflake} from '@fluxer/snowflake/src/Snowflake';
import {createEntranceSoundID, type EntranceSoundID, type UserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import type {IMediaService} from '../../infrastructure/IMediaService';
import type {IStorageService} from '../../infrastructure/IStorageService';
import {Logger} from '../../Logger';
import {EntranceSound, EntranceSoundSelection} from '../../models/EntranceSound';
import {resolveEntranceSoundDurationMs} from './EntranceSoundDurationProbe';
import type {EntranceSoundRepository} from './EntranceSoundRepository';

interface UploadEntranceSoundParams {
	userId: UserID;
	name: string;
	base64Audio: string;
}

interface RenameEntranceSoundParams {
	userId: UserID;
	soundId: EntranceSoundID;
	name: string;
}

interface SetSelectionParams {
	userId: UserID;
	scopeId: string;
	soundId: EntranceSoundID | null;
}

export interface EntranceSoundLibraryEntry {
	sound: EntranceSound;
	url: string;
}

const SOUND_PATH_PREFIX = 'entrance-sounds';

export class EntranceSoundService {
	constructor(
		private readonly repository: EntranceSoundRepository,
		private readonly storageService: IStorageService,
		private readonly mediaService: IMediaService,
	) {}

	cdnUrlFor(sound: EntranceSound): string {
		return `${Config.endpoints.media}/${SOUND_PATH_PREFIX}/${sound.userId}/${sound.hash}.${sound.extension}`;
	}

	private s3KeyFor(userId: UserID, hash: string, extension: EntranceSoundExtension): string {
		return `${SOUND_PATH_PREFIX}/${userId}/${hash}.${extension}`;
	}

	private async objectIsReferenced(
		userId: UserID,
		hash: string,
		extension: string,
		excludeSoundId?: EntranceSoundID,
	): Promise<boolean> {
		const sounds = await this.repository.listSounds(userId);
		return sounds.some(
			(sound) => sound.hash === hash && sound.extension === extension && sound.soundId !== excludeSoundId,
		);
	}

	async listLibrary(userId: UserID): Promise<Array<EntranceSoundLibraryEntry>> {
		const sounds = await this.repository.listSounds(userId);
		return sounds.map((sound) => ({sound, url: this.cdnUrlFor(sound)}));
	}

	async getSoundWithUrl(userId: UserID, soundId: EntranceSoundID): Promise<EntranceSoundLibraryEntry | null> {
		const sound = await this.repository.getSound(userId, soundId);
		if (!sound) return null;
		return {sound, url: this.cdnUrlFor(sound)};
	}

	async upload(params: UploadEntranceSoundParams): Promise<EntranceSoundLibraryEntry> {
		const {userId, base64Audio} = params;
		const name = this.normalizeName(params.name);
		const existing = await this.repository.listSounds(userId);
		if (existing.length >= ENTRANCE_SOUND_MAX_PER_USER) {
			throw InputValidationError.fromCode('audio', ValidationErrorCodes.ENTRANCE_SOUND_QUOTA_REACHED, {
				max: ENTRANCE_SOUND_MAX_PER_USER,
			});
		}
		const trimmed = base64Audio.includes(',') ? (base64Audio.split(',')[1] ?? '') : base64Audio;
		let bytes: Buffer;
		try {
			bytes = Buffer.from(trimmed, 'base64');
		} catch {
			throw InputValidationError.fromCode('audio', ValidationErrorCodes.INVALID_BASE64_FORMAT);
		}
		if (bytes.length === 0) {
			throw InputValidationError.fromCode('audio', ValidationErrorCodes.INVALID_BASE64_FORMAT);
		}
		if (bytes.length > ENTRANCE_SOUND_MAX_BYTES) {
			throw InputValidationError.fromCode('audio', ValidationErrorCodes.ENTRANCE_SOUND_SIZE_EXCEEDS_LIMIT, {
				max_bytes: ENTRANCE_SOUND_MAX_BYTES,
			});
		}
		const metadata = await this.mediaService.getMetadata({
			type: 'base64',
			base64: trimmed,
			version: 2,
			nsfw: 'allow',
		});
		if (!metadata) {
			throw InputValidationError.fromCode('audio', ValidationErrorCodes.ENTRANCE_SOUND_INVALID_FORMAT);
		}
		const extension =
			entranceSoundExtensionFromFormat(metadata.format) ?? entranceSoundExtensionFromMime(metadata.content_type);
		if (!extension) {
			throw InputValidationError.fromCode('audio', ValidationErrorCodes.ENTRANCE_SOUND_INVALID_FORMAT, {
				format: metadata.format ?? metadata.content_type ?? 'unknown',
			});
		}
		const metadataDurationSeconds = typeof metadata.duration === 'number' ? metadata.duration : null;
		const durationMs = await resolveEntranceSoundDurationMs({
			bytes,
			extension,
			metadataDurationSeconds,
		});
		if (durationMs == null) {
			throw InputValidationError.fromCode('audio', ValidationErrorCodes.ENTRANCE_SOUND_INVALID_FORMAT);
		}
		if (durationMs > ENTRANCE_SOUND_MAX_DURATION_MS) {
			throw InputValidationError.fromCode('audio', ValidationErrorCodes.ENTRANCE_SOUND_DURATION_EXCEEDS_LIMIT, {
				max_ms: ENTRANCE_SOUND_MAX_DURATION_MS,
			});
		}
		if (durationMs < ENTRANCE_SOUND_MIN_DURATION_MS) {
			throw InputValidationError.fromCode('audio', ValidationErrorCodes.ENTRANCE_SOUND_DURATION_EXCEEDS_LIMIT, {
				min_ms: ENTRANCE_SOUND_MIN_DURATION_MS,
			});
		}
		const hash = crypto.createHash('md5').update(bytes).digest('hex').slice(0, 16);
		const contentType = ENTRANCE_SOUND_EXT_TO_MIME[extension];
		const s3Key = this.s3KeyFor(userId, hash, extension);
		try {
			await this.storageService.uploadObject({
				bucket: Config.s3.buckets.cdn,
				key: s3Key,
				body: new Uint8Array(bytes),
				contentType,
			});
		} catch (error) {
			Logger.error({error, userId: userId.toString(), s3Key}, 'Failed to upload entrance sound to S3');
			throw InputValidationError.fromCode('audio', ValidationErrorCodes.FAILED_TO_UPLOAD_IMAGE);
		}
		const sound = new EntranceSound({
			user_id: userId,
			sound_id: createEntranceSoundID(generateSnowflake()),
			name,
			hash,
			extension,
			content_type: contentType,
			duration_ms: durationMs,
			size_bytes: bytes.length,
			created_at: new Date(),
			version: 1,
		});
		try {
			await this.repository.upsertSound(sound);
		} catch (error) {
			Logger.error({error, userId: userId.toString(), s3Key}, 'Failed to persist entrance sound; rolling back S3');
			if (!(await this.objectIsReferenced(userId, hash, extension).catch(() => true))) {
				await this.storageService.deleteObject(Config.s3.buckets.cdn, s3Key).catch(() => {});
			}
			throw error;
		}
		return {sound, url: this.cdnUrlFor(sound)};
	}

	async rename(params: RenameEntranceSoundParams): Promise<EntranceSoundLibraryEntry> {
		const name = this.normalizeName(params.name);
		const existing = await this.repository.getSound(params.userId, params.soundId);
		if (!existing) {
			throw InputValidationError.fromCode('sound_id', ValidationErrorCodes.ENTRANCE_SOUND_NOT_FOUND);
		}
		const next = new EntranceSound({
			...existing.toRow(),
			name,
			version: existing.version + 1,
		});
		await this.repository.upsertSound(next);
		return {sound: next, url: this.cdnUrlFor(next)};
	}

	async delete(userId: UserID, soundId: EntranceSoundID): Promise<void> {
		const existing = await this.repository.getSound(userId, soundId);
		if (!existing) return;
		await this.repository.deleteSelectionsForSound(userId, soundId);
		await this.repository.deleteSound(userId, soundId);
		if (await this.objectIsReferenced(userId, existing.hash, existing.extension, soundId).catch(() => true)) {
			return;
		}
		const s3Key = this.s3KeyFor(userId, existing.hash, existing.extension as EntranceSoundExtension);
		await this.storageService.deleteObject(Config.s3.buckets.cdn, s3Key).catch((error) => {
			Logger.error({error, userId: userId.toString(), s3Key}, 'Failed to delete entrance sound from S3');
		});
	}

	async listSelections(userId: UserID): Promise<Array<EntranceSoundSelection>> {
		return this.repository.listSelections(userId);
	}

	async setSelection(params: SetSelectionParams): Promise<EntranceSoundSelection | null> {
		const {userId, scopeId, soundId} = params;
		if (!isValidEntranceSoundScopeId(scopeId)) {
			throw InputValidationError.fromCode('scope_id', ValidationErrorCodes.ENTRANCE_SOUND_INVALID_SCOPE);
		}
		if (soundId === null) {
			await this.repository.deleteSelection(userId, scopeId);
			return null;
		}
		const sound = await this.repository.getSound(userId, soundId);
		if (!sound) {
			throw InputValidationError.fromCode('sound_id', ValidationErrorCodes.ENTRANCE_SOUND_NOT_FOUND);
		}
		const selection = new EntranceSoundSelection({
			user_id: userId,
			scope_id: scopeId,
			sound_id: soundId,
		});
		await this.repository.upsertSelection(selection);
		return selection;
	}

	private normalizeName(rawName: string): string {
		const trimmed = rawName.trim();
		if (trimmed.length === 0 || trimmed.length > ENTRANCE_SOUND_NAME_MAX_LENGTH) {
			throw InputValidationError.fromCode('name', ValidationErrorCodes.ENTRANCE_SOUND_NAME_LENGTH_INVALID, {
				max: ENTRANCE_SOUND_NAME_MAX_LENGTH,
			});
		}
		return trimmed;
	}
}
