// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomInt} from 'node:crypto';
import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import type {User} from '@app/api/models/User';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {NON_SELF_HOSTED_RESERVED_DISCRIMINATORS} from '@fluxer/constants/src/DiscriminatorConstants';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {ms, seconds} from 'itty-time';

interface GenerateDiscriminatorParams {
	username: string;
	requestedDiscriminator?: number;
	user?: User | null;
}

interface GenerateDiscriminatorResult {
	discriminator: number;
	available: boolean;
}

interface ResolveUsernameChangeParams {
	currentUsername: string;
	currentDiscriminator: number;
	newUsername: string;
	user?: User | null;
	requestedDiscriminator?: number;
}

interface ResolveUsernameChangeResult {
	username: string;
	discriminator: number;
}

export class UsernameNotAvailableError extends BadRequestError {
	constructor() {
		super({code: APIErrorCodes.USERNAME_NOT_AVAILABLE});
		this.name = 'UsernameNotAvailableError';
	}
}

export interface IDiscriminatorService {
	generateDiscriminator(params: GenerateDiscriminatorParams): Promise<GenerateDiscriminatorResult>;
	isDiscriminatorAvailableForUsername(username: string, discriminator: number): Promise<boolean>;
	resolveUsernameChange(params: ResolveUsernameChangeParams): Promise<ResolveUsernameChangeResult>;
}

export class DiscriminatorService implements IDiscriminatorService {
	private static readonly LOCK_TTL_MS = ms('5 seconds');
	private static readonly LOCK_RETRY_DELAY_MS = 50;
	private static readonly LOCK_MAX_WAIT_MS = ms('10 seconds');
	private static readonly DISCRIM_CACHE_TTL_S = seconds('30 seconds');

	constructor(
		private userRepository: IUserRepository,
		private cacheService: ICacheService,
		private limitConfigService: LimitConfigService,
	) {}

	private async canUseCustomDiscriminator(user?: User | null): Promise<boolean> {
		if (Config.instance.selfHosted) {
			return true;
		}
		if (!user) {
			return false;
		}
		const ctx = createLimitMatchContext({user});
		const hasCustomDiscriminator = resolveLimitSafe(
			this.limitConfigService.getConfigSnapshot(),
			ctx,
			'feature_custom_discriminator',
			0,
		);
		return hasCustomDiscriminator > 0;
	}

	async generateDiscriminator(params: GenerateDiscriminatorParams): Promise<GenerateDiscriminatorResult> {
		const {username, requestedDiscriminator, user} = params;
		const usernameLower = username.toLowerCase();
		const lockKey = `discrim-lock:${usernameLower}`;
		const lockToken = await this.acquireLockWithRetry(lockKey);
		if (!lockToken) {
			return {discriminator: -1, available: false};
		}
		try {
			const allowCustomDiscriminator = await this.canUseCustomDiscriminator(user);
			if (allowCustomDiscriminator && requestedDiscriminator !== undefined) {
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
			await this.releaseLock(lockKey, lockToken);
		}
	}

	async isDiscriminatorAvailableForUsername(username: string, discriminator: number): Promise<boolean> {
		const usernameLower = username.toLowerCase();
		const lockKey = `discrim-lock:${usernameLower}`;
		const lockToken = await this.acquireLockWithRetry(lockKey);
		if (!lockToken) {
			return false;
		}
		try {
			return await this.isDiscriminatorAvailable(usernameLower, discriminator);
		} finally {
			await this.releaseLock(lockKey, lockToken);
		}
	}

	async resolveUsernameChange(params: ResolveUsernameChangeParams): Promise<ResolveUsernameChangeResult> {
		const {currentUsername, currentDiscriminator, newUsername, user, requestedDiscriminator} = params;
		if (
			currentUsername.toLowerCase() === newUsername.toLowerCase() &&
			(requestedDiscriminator === undefined || requestedDiscriminator === currentDiscriminator)
		) {
			return {username: newUsername, discriminator: currentDiscriminator};
		}
		const allowCustomDiscriminator = await this.canUseCustomDiscriminator(user);
		const discriminatorToRequest = allowCustomDiscriminator ? requestedDiscriminator : undefined;
		const result = await this.generateDiscriminator({
			username: newUsername,
			requestedDiscriminator: discriminatorToRequest,
			user,
		});
		if (!result.available || result.discriminator === -1) {
			throw new UsernameNotAvailableError();
		}
		return {username: newUsername, discriminator: result.discriminator};
	}

	private async acquireLockWithRetry(lockKey: string): Promise<string | null> {
		const startTime = Date.now();
		while (Date.now() - startTime < DiscriminatorService.LOCK_MAX_WAIT_MS) {
			const token = await this.acquireLock(lockKey);
			if (token) {
				return token;
			}
			try {
				await this.sleep(DiscriminatorService.LOCK_RETRY_DELAY_MS);
			} catch (error) {
				Logger.error({lockKey, error}, 'Error during lock retry sleep');
				return null;
			}
		}
		return null;
	}

	private async acquireLock(lockKey: string): Promise<string | null> {
		try {
			const ttlSeconds = Math.ceil(DiscriminatorService.LOCK_TTL_MS / 1000);
			return await this.cacheService.acquireLock(lockKey, ttlSeconds);
		} catch (error) {
			Logger.error({lockKey, error}, 'Failed to acquire discriminator lock');
			return null;
		}
	}

	private async releaseLock(lockKey: string, token: string): Promise<void> {
		try {
			await this.cacheService.releaseLock(lockKey, token);
		} catch (error) {
			Logger.error({lockKey, error}, 'Failed to release discriminator lock');
		}
	}

	private async isDiscriminatorAvailable(username: string, discriminator: number): Promise<boolean> {
		const cacheKey = `discrim-claimed:${username}`;
		const isCached = await this.cacheService.sismember(cacheKey, discriminator.toString());
		if (isCached) {
			return false;
		}
		const user = await this.userRepository.findByUsernameDiscriminator(username, discriminator);
		return user === null;
	}

	private async generateRandomDiscriminator(username: string): Promise<number> {
		const takenDiscriminators = await this.userRepository.findDiscriminatorsByUsername(username);
		const cachedDiscriminators = await this.getCachedDiscriminators(username);
		const allTaken = new Set([...takenDiscriminators, ...cachedDiscriminators]);
		if (!Config.instance.selfHosted) {
			for (const reservedDiscriminator of NON_SELF_HOSTED_RESERVED_DISCRIMINATORS) {
				allTaken.add(reservedDiscriminator);
			}
		}
		if (allTaken.size >= 9999) {
			return -1;
		}
		for (let attempts = 0; attempts < 10; attempts++) {
			const randomDiscrim = randomInt(1, 10000);
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
		const cacheKey = `discrim-claimed:${username}`;
		await this.cacheService.sadd(cacheKey, discriminator.toString(), DiscriminatorService.DISCRIM_CACHE_TTL_S);
	}

	private async getCachedDiscriminators(username: string): Promise<Set<number>> {
		const cacheKey = `discrim-claimed:${username}`;
		const members = await this.cacheService.smembers(cacheKey);
		const discriminators = new Set<number>();
		for (const member of members) {
			const discrim = parseInt(member, 10);
			if (!Number.isNaN(discrim)) {
				discriminators.add(discrim);
			}
		}
		return discriminators;
	}

	private sleep(ms: number): Promise<void> {
		return new Promise((resolve, reject) => {
			const timeout = setTimeout(() => {
				try {
					resolve();
				} catch (error) {
					reject(error);
				}
			}, ms);
			timeout.unref?.();
		});
	}
}
