// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {Config} from '@app/api/Config';
import {stripAnimationPrefix} from '@app/api/infrastructure/AssetEntitlementUtils';
import type {IPurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import {S3ServiceException} from '@aws-sdk/client-s3';
import sharp from 'sharp';

const OPERATOR_PASS_WALL_AVATAR_BASE_SIZE = 48;

export const OPERATOR_PASS_WALL_TIER_SCALES: ReadonlyArray<number> = [1, 1.25, 1.5, 2];

export const OPERATOR_PASS_WALL_AVATAR_SIZES: ReadonlyArray<number> = [
	...new Set(
		OPERATOR_PASS_WALL_TIER_SCALES.flatMap((scale) =>
			[1, 2].map((density) => OPERATOR_PASS_WALL_AVATAR_BASE_SIZE * scale * density),
		),
	),
].sort((left, right) => left - right);

const PRIVATE_SIZE = Math.max(...OPERATOR_PASS_WALL_AVATAR_SIZES);
const AVATAR_SOURCE_MAX_BYTES = 16 * 1024 * 1024;
const SHA256_PATTERN = /^[0-9a-f]{64}$/u;

function operatorPassWallAvatarKey(sha256: string, size: number): string {
	return `operator-wall/${sha256}-${size}.webp`;
}

function operatorPassWallPrivateAvatarKey(sha256: string): string {
	return `operator-wall-private/${sha256}.webp`;
}

export function operatorPassWallSnapshotUrl(sha256: string, size: number): string {
	return `${Config.endpoints.media}/${operatorPassWallAvatarKey(sha256, size)}`;
}

function isOperatorPassWallAvatarHash(value: string): boolean {
	return SHA256_PATTERN.test(value);
}

export class OperatorPassWallAvatarStore {
	constructor(
		private readonly storage: IStorageService,
		private readonly purgeQueue: IPurgeQueue | null = null,
	) {}

	async snapshotUserAvatar(user: Pick<User, 'id' | 'avatarHash'>): Promise<string | null> {
		if (!user.avatarHash) {
			return null;
		}
		const key = `avatars/${user.id}/${stripAnimationPrefix(user.avatarHash)}`;
		let source: Uint8Array;
		try {
			source = await this.storage.readObject(Config.s3.buckets.cdn, key, AVATAR_SOURCE_MAX_BYTES);
		} catch (error) {
			Logger.warn({error, userId: user.id}, "Could not read the profile avatar for an Operators' wall snapshot");
			return null;
		}
		return this.storePrivate(source);
	}

	async storePrivate(source: Uint8Array): Promise<string> {
		const sha256 = crypto.createHash('sha256').update(source).digest('hex');
		const key = operatorPassWallPrivateAvatarKey(sha256);
		if (!(await this.exists(Config.s3.buckets.uploads, key))) {
			const body = await sharp(Buffer.from(source), {animated: false})
				.rotate()
				.resize(PRIVATE_SIZE, PRIVATE_SIZE, {fit: 'cover', position: 'centre'})
				.webp({quality: 92})
				.toBuffer();
			await this.storage.uploadObject({bucket: Config.s3.buckets.uploads, key, body, contentType: 'image/webp'});
		}
		return sha256;
	}

	async readPrivate(sha256: string): Promise<Uint8Array | null> {
		if (!isOperatorPassWallAvatarHash(sha256)) {
			return null;
		}
		try {
			return await this.storage.readObject(
				Config.s3.buckets.uploads,
				operatorPassWallPrivateAvatarKey(sha256),
				AVATAR_SOURCE_MAX_BYTES,
			);
		} catch (error) {
			if (error instanceof S3ServiceException && (error.name === 'NoSuchKey' || error.name === 'NotFound')) {
				return null;
			}
			throw error;
		}
	}

	async publish(sha256: string): Promise<boolean> {
		if (await this.exists(Config.s3.buckets.cdn, operatorPassWallAvatarKey(sha256, PRIVATE_SIZE))) {
			return true;
		}
		const source = await this.readPrivate(sha256);
		if (!source) {
			return false;
		}
		for (const size of OPERATOR_PASS_WALL_AVATAR_SIZES) {
			const key = operatorPassWallAvatarKey(sha256, size);
			if (await this.exists(Config.s3.buckets.cdn, key)) {
				continue;
			}
			const body = await sharp(Buffer.from(source)).resize(size, size, {fit: 'cover'}).webp({quality: 90}).toBuffer();
			await this.storage.uploadObject({bucket: Config.s3.buckets.cdn, key, body, contentType: 'image/webp'});
		}
		return true;
	}

	async unpublish(sha256: string): Promise<void> {
		for (const size of OPERATOR_PASS_WALL_AVATAR_SIZES) {
			await this.storage
				.deleteObject(Config.s3.buckets.cdn, operatorPassWallAvatarKey(sha256, size))
				.catch((error) => Logger.warn({error, sha256, size}, "Failed to delete a published Operators' wall avatar"));
		}
		await this.purgeQueue
			?.addUrls(OPERATOR_PASS_WALL_AVATAR_SIZES.map((size) => operatorPassWallSnapshotUrl(sha256, size)))
			.catch((error) => Logger.warn({error, sha256}, "Failed to queue the CDN purge of an Operators' wall avatar"));
	}

	async deletePrivate(sha256: string): Promise<void> {
		await this.storage
			.deleteObject(Config.s3.buckets.uploads, operatorPassWallPrivateAvatarKey(sha256))
			.catch((error) => Logger.warn({error, sha256}, "Failed to delete a private Operators' wall avatar"));
	}

	private async exists(bucket: string, key: string): Promise<boolean> {
		return (await this.storage.getObjectMetadata(bucket, key)) !== null;
	}
}
