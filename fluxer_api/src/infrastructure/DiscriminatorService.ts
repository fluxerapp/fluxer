/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IUserRepository} from '~/user/IUserRepository';

interface GenerateDiscriminatorParams {
	username: string;
	requestedDiscriminator?: number;
	isPremium?: boolean;
}

interface GenerateDiscriminatorResult {
	discriminator: number;
	available: boolean;
}

interface ResolveUsernameChangeParams {
	currentUsername: string;
	currentDiscriminator: number;
	newUsername: string;
	isPremium?: boolean;
	requestedDiscriminator?: number;
}

interface ResolveUsernameChangeResult {
	username: string;
	discriminator: number;
}

export class UsernameNotAvailableError extends Error {
	constructor(message = 'Username not available') {
		super(message);
		this.name = 'UsernameNotAvailableError';
	}
}

export interface IDiscriminatorService {
	generateDiscriminator(params: GenerateDiscriminatorParams): Promise<GenerateDiscriminatorResult>;
	isDiscriminatorAvailableForUsername(username: string, discriminator: number): Promise<boolean>;
	resolveUsernameChange(params: ResolveUsernameChangeParams): Promise<ResolveUsernameChangeResult>;
}

export class DiscriminatorService implements IDiscriminatorService {
	private static readonly LOCK_TTL_MS = 5000;
	private static readonly LOCK_RETRY_DELAY_MS = 50;
	private static readonly LOCK_MAX_WAIT_MS = 10000;
	private static readonly DISCRIM_CACHE_TTL_S = 30;

	constructor(
		private userRepository: IUserRepository,
		private cacheService: ICacheService,
	) {}

	async generateDiscriminator(params: GenerateDiscriminatorParams): Promise<GenerateDiscriminatorResult> {
		const {username, requestedDiscriminator, isPremium = false} = params;

		const usernameLower = username.toLowerCase();
		const lockKey = `discrim-lock:${usernameLower}`;
		const acquired = await this.acquireLockWithRetry(lockKey);

		if (!acquired) {
			return {discriminator: -1, available: false};
		}

		try {
			if (isPremium && requestedDiscriminator !== undefined) {
				const isAvailable = await this.isDiscriminatorAvailable(usernameLower, requestedDiscriminator);
				if (isAvailable) {
					await this.cacheClaimedDiscriminator(usernameLower, requestedDiscriminator);
					return {discriminator: requestedDiscriminator, available: true};
				}
				return {discriminator: requestedDiscriminator, available: false};
			}

			const discriminator = await this.generateRandomDiscriminator(usernameLower);
			if (discriminator === -1) {
				return {discriminator: -1, available: false};
			}

			await this.cacheClaimedDiscriminator(usernameLower, discriminator);
			return {discriminator, available: true};
		} finally {
			await this.releaseLock(lockKey);
		}
	}

	async isDiscriminatorAvailableForUsername(username: string, discriminator: number): Promise<boolean> {
		const usernameLower = username.toLowerCase();
		const lockKey = `discrim-lock:${usernameLower}`;
		const acquired = await this.acquireLockWithRetry(lockKey);

		if (!acquired) {
			return false;
		}

		try {
			return await this.isDiscriminatorAvailable(usernameLower, discriminator);
		} finally {
			await this.releaseLock(lockKey);
		}
	}

	async resolveUsernameChange(params: ResolveUsernameChangeParams): Promise<ResolveUsernameChangeResult> {
		const {currentUsername, currentDiscriminator, newUsername, isPremium = false, requestedDiscriminator} = params;

		if (
			currentUsername.toLowerCase() === newUsername.toLowerCase() &&
			(requestedDiscriminator === undefined || requestedDiscriminator === currentDiscriminator)
		) {
			return {username: newUsername, discriminator: currentDiscriminator};
		}

		const result = await this.generateDiscriminator({
			username: newUsername,
			requestedDiscriminator: isPremium ? requestedDiscriminator : undefined,
			isPremium,
		});

		if (!result.available || result.discriminator === -1) {
			throw new UsernameNotAvailableError();
		}

		return {username: newUsername, discriminator: result.discriminator};
	}

	private async acquireLockWithRetry(lockKey: string): Promise<boolean> {
		const startTime = Date.now();

		while (Date.now() - startTime < DiscriminatorService.LOCK_MAX_WAIT_MS) {
			const acquired = await this.acquireLock(lockKey);
			if (acquired) {
				return true;
			}

			await this.sleep(DiscriminatorService.LOCK_RETRY_DELAY_MS);
		}

		return false;
	}

	private async acquireLock(lockKey: string): Promise<boolean> {
		try {
			const lockValue = `${Date.now()}-${Math.random()}`;
			const ttlSeconds = Math.ceil(DiscriminatorService.LOCK_TTL_MS / 1000);

			const result = await this.cacheService.set(lockKey, lockValue, ttlSeconds);

			return result !== null;
		} catch (_error) {
			return false;
		}
	}

	private async releaseLock(lockKey: string): Promise<void> {
		try {
			await this.cacheService.delete(lockKey);
		} catch (_error) {}
	}

	private async isDiscriminatorAvailable(username: string, discriminator: number): Promise<boolean> {
		const cacheKey = `discrim-claimed:${username}:${discriminator}`;
		const cached = await this.cacheService.get(cacheKey);
		if (cached !== null) {
			return false;
		}

		const user = await this.userRepository.findByUsernameDiscriminator(username, discriminator);
		return user === null;
	}

	private async generateRandomDiscriminator(username: string): Promise<number> {
		const takenDiscriminators = await this.userRepository.findDiscriminatorsByUsername(username);

		const cachedDiscriminators = await this.getCachedDiscriminators(username);
		const allTaken = new Set([...takenDiscriminators, ...cachedDiscriminators]);

		if (allTaken.size >= 9999) {
			return -1;
		}

		for (let attempts = 0; attempts < 10; attempts++) {
			const randomDiscrim = Math.floor(Math.random() * 9999) + 1;
			if (!allTaken.has(randomDiscrim)) {
				return randomDiscrim;
			}
		}

		for (let i = 1; i <= 9999; i++) {
			if (!allTaken.has(i)) {
				return i;
			}
		}

		return -1;
	}

	private async cacheClaimedDiscriminator(username: string, discriminator: number): Promise<void> {
		const cacheKey = `discrim-claimed:${username}:${discriminator}`;
		await this.cacheService.set(cacheKey, '1', DiscriminatorService.DISCRIM_CACHE_TTL_S);
	}

	private async getCachedDiscriminators(username: string): Promise<Set<number>> {
		const discriminators = new Set<number>();

		for (let i = 0; i <= 9999; i++) {
			const cacheKey = `discrim-claimed:${username}:${i}`;
			const cached = await this.cacheService.get(cacheKey);
			if (cached !== null) {
				discriminators.add(i);
			}
		}

		return discriminators;
	}

	private sleep(ms: number): Promise<void> {
		return new Promise((resolve) => setTimeout(resolve, ms));
	}
}
