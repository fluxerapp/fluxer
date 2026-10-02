// SPDX-License-Identifier: AGPL-3.0-or-later

import {getConfig} from '@app/api/Config';
import {createDefaultLimitConfig} from '@app/api/constants/LimitConfig';
import {DiscriminatorService} from '@app/api/infrastructure/DiscriminatorService';
import {getCachedInstancePremiumMode, setCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

function createService(): DiscriminatorService {
	const userRepository = {
		async findByUsernameDiscriminator() {
			return null;
		},
		async findDiscriminatorsByUsername() {
			return new Set<number>([42]);
		},
	} as unknown as IUserRepository;
	const cacheService = {
		async acquireLock() {
			return 'token';
		},
		async releaseLock() {},
		async sismember() {
			return false;
		},
		async sadd() {},
		async smembers() {
			return new Set<string>();
		},
	} as unknown as ICacheService;
	const limitConfigService = {
		getConfigSnapshot: () => createDefaultLimitConfig({selfHosted: true, premiumMode: 'mirror'}),
	} as unknown as LimitConfigService;
	return new DiscriminatorService(userRepository, cacheService, limitConfigService);
}

describe('DiscriminatorService on a self-hosted instance', () => {
	let originalSelfHosted: boolean;
	let originalPremiumMode: ReturnType<typeof getCachedInstancePremiumMode>;

	beforeEach(() => {
		originalSelfHosted = getConfig().instance.selfHosted;
		originalPremiumMode = getCachedInstancePremiumMode();
		getConfig().instance.selfHosted = true;
	});

	afterEach(() => {
		getConfig().instance.selfHosted = originalSelfHosted;
		setCachedInstancePremiumMode(originalPremiumMode);
	});

	test('lets anyone pick a discriminator when everyone is premium', async () => {
		setCachedInstancePremiumMode('everyone');
		const result = await createService().generateDiscriminator({username: 'someone', requestedDiscriminator: 42});
		expect(result).toEqual({discriminator: 42, available: true});
	});

	test('follows the custom discriminator limit in mirror mode', async () => {
		setCachedInstancePremiumMode('mirror');
		const result = await createService().generateDiscriminator({username: 'someone', requestedDiscriminator: 42});
		expect(result.available).toBe(true);
		expect(result.discriminator).not.toBe(42);
	});
});
