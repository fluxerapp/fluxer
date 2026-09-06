// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it, vi} from 'vitest';
import {createUserID, type EntranceSoundID, type UserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import type {IMediaService} from '../../infrastructure/IMediaService';
import type {IStorageService} from '../../infrastructure/IStorageService';
import type {EntranceSound} from '../../models/EntranceSound';
import type {EntranceSoundRepository} from './EntranceSoundRepository';
import {EntranceSoundService} from './EntranceSoundService';

const USER_ID = createUserID(1234567890123456n);

function createWavBase64(sampleCount: number): string {
	const dataLength = sampleCount * 2;
	const buffer = Buffer.alloc(44 + dataLength);
	buffer.write('RIFF', 0, 'ascii');
	buffer.writeUInt32LE(36 + dataLength, 4);
	buffer.write('WAVE', 8, 'ascii');
	buffer.write('fmt ', 12, 'ascii');
	buffer.writeUInt32LE(16, 16);
	buffer.writeUInt16LE(1, 20);
	buffer.writeUInt16LE(1, 22);
	buffer.writeUInt32LE(8000, 24);
	buffer.writeUInt32LE(16000, 28);
	buffer.writeUInt16LE(2, 32);
	buffer.writeUInt16LE(16, 34);
	buffer.write('data', 36, 'ascii');
	buffer.writeUInt32LE(dataLength, 40);
	for (let index = 0; index < sampleCount; index += 1) {
		buffer.writeInt16LE(Math.round(8000 * Math.sin(index / 8)), 44 + index * 2);
	}
	return buffer.toString('base64');
}

const HALF_SECOND_WAV = createWavBase64(4000);
const OTHER_WAV = createWavBase64(6000);

class FakeEntranceSoundRepository {
	readonly sounds = new Map<string, EntranceSound>();
	failNextUpsert = false;

	async listSounds(_userId: UserID): Promise<Array<EntranceSound>> {
		return [...this.sounds.values()];
	}

	async getSound(_userId: UserID, soundId: EntranceSoundID): Promise<EntranceSound | null> {
		return this.sounds.get(soundId.toString()) ?? null;
	}

	async upsertSound(sound: EntranceSound): Promise<EntranceSound> {
		if (this.failNextUpsert) {
			this.failNextUpsert = false;
			throw new Error('Failed to persist entrance sound');
		}
		this.sounds.set(sound.soundId.toString(), sound);
		return sound;
	}

	async deleteSound(_userId: UserID, soundId: EntranceSoundID): Promise<void> {
		this.sounds.delete(soundId.toString());
	}

	async deleteSelectionsForSound(_userId: UserID, _soundId: EntranceSoundID): Promise<void> {}
}

function createService() {
	const repository = new FakeEntranceSoundRepository();
	const objects = new Set<string>();
	const deleteObject = vi.fn(async (_bucket: string, key: string) => {
		objects.delete(key);
	});
	const storageService = {
		uploadObject: async (params: {key: string}) => {
			objects.add(params.key);
		},
		deleteObject,
	} as unknown as IStorageService;
	const mediaService = {
		getMetadata: async () => ({
			format: 'wav',
			content_type: 'audio/wav',
			content_hash: 'content-hash',
			size: 0,
			duration: 0.5,
			nsfw: false,
		}),
	} as unknown as IMediaService;
	const service = new EntranceSoundService(
		repository as unknown as EntranceSoundRepository,
		storageService,
		mediaService,
	);
	return {service, repository, objects, deleteObject};
}

function keyFromUrl(url: string): string {
	return url.slice(`${Config.endpoints.media}/`.length);
}

describe('EntranceSoundService shared object references', () => {
	it('keeps the shared object when one of two entries with identical audio is deleted', async () => {
		const {service, objects, deleteObject} = createService();
		const first = await service.upload({userId: USER_ID, name: 'first', base64Audio: HALF_SECOND_WAV});
		const second = await service.upload({userId: USER_ID, name: 'second', base64Audio: HALF_SECOND_WAV});
		expect(keyFromUrl(second.url)).toBe(keyFromUrl(first.url));

		await service.delete(USER_ID, first.sound.soundId);

		expect(deleteObject).not.toHaveBeenCalled();
		expect(objects.has(keyFromUrl(second.url))).toBe(true);

		await service.delete(USER_ID, second.sound.soundId);

		expect(deleteObject).toHaveBeenCalledTimes(1);
		expect(deleteObject).toHaveBeenCalledWith(Config.s3.buckets.cdn, keyFromUrl(second.url));
		expect(objects.size).toBe(0);
	});

	it('keeps the shared object when a duplicate upload fails to persist', async () => {
		const {service, repository, objects, deleteObject} = createService();
		const first = await service.upload({userId: USER_ID, name: 'first', base64Audio: HALF_SECOND_WAV});

		repository.failNextUpsert = true;
		await expect(service.upload({userId: USER_ID, name: 'duplicate', base64Audio: HALF_SECOND_WAV})).rejects.toThrow(
			'Failed to persist entrance sound',
		);

		expect(deleteObject).not.toHaveBeenCalled();
		expect(objects.has(keyFromUrl(first.url))).toBe(true);
		await expect(service.listLibrary(USER_ID)).resolves.toHaveLength(1);
	});

	it('rolls back the object when the only entry referencing it fails to persist', async () => {
		const {service, repository, objects, deleteObject} = createService();

		repository.failNextUpsert = true;
		await expect(service.upload({userId: USER_ID, name: 'only', base64Audio: HALF_SECOND_WAV})).rejects.toThrow(
			'Failed to persist entrance sound',
		);

		expect(deleteObject).toHaveBeenCalledTimes(1);
		expect(objects.size).toBe(0);
	});

	it('deletes the object for a lone entry and leaves other entries alone', async () => {
		const {service, objects, deleteObject} = createService();
		const lone = await service.upload({userId: USER_ID, name: 'lone', base64Audio: HALF_SECOND_WAV});
		const other = await service.upload({userId: USER_ID, name: 'other', base64Audio: OTHER_WAV});
		expect(keyFromUrl(other.url)).not.toBe(keyFromUrl(lone.url));

		await service.delete(USER_ID, lone.sound.soundId);

		expect(deleteObject).toHaveBeenCalledTimes(1);
		expect(deleteObject).toHaveBeenCalledWith(Config.s3.buckets.cdn, keyFromUrl(lone.url));
		expect(objects.has(keyFromUrl(other.url))).toBe(true);
	});
});
