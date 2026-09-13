// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {EMPTY_USER_ROW} from '@app/api/database/types/UserTypes';
import {
	stripAnimationPrefixIfNoEntitlement,
	stripAvatarForUser,
	stripBannerForUser,
	stripGuildBannerForFeatures,
	stripGuildIconForFeatures,
	stripGuildSplashForFeatures,
	userHasAnimatedAvatarEntitlement,
} from '@app/api/infrastructure/AssetEntitlementUtils';
import {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {User} from '@app/api/models/User';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {PremiumFlags} from '@fluxer/constants/src/UserConstants';
import {InMemoryProvider} from '@pkgs/cache/src/providers/InMemoryProvider';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

let originalSelfHosted = true;

function ensureLimitConfigGlobal(): void {
	new LimitConfigService(new InstanceConfigRepository(), new InMemoryProvider()).setAsGlobalInstance();
}

function createUser(params: {
	userId: UserID;
	avatarHash: string | null;
	bannerHash?: string | null;
	premium?: boolean;
	isBot?: boolean;
}): User {
	const premiumFlags = params.premium ? PremiumFlags.ENABLED_OVERRIDE : 0;
	return new User({
		...EMPTY_USER_ROW,
		user_id: params.userId,
		username: `user-${params.userId.toString()}`,
		discriminator: 1234,
		avatar_hash: params.avatarHash,
		banner_hash: params.bannerHash ?? null,
		bot: params.isBot ?? false,
		flags: 0n,
		premium_flags: premiumFlags,
		version: 1,
	});
}

describe('AssetEntitlementUtils', () => {
	beforeEach(() => {
		const config = getConfig();
		originalSelfHosted = config.instance.selfHosted;
		config.instance.selfHosted = false;
		ensureLimitConfigGlobal();
	});
	afterEach(() => {
		getConfig().instance.selfHosted = originalSelfHosted;
	});
	describe('stripAnimationPrefixIfNoEntitlement', () => {
		test('returns null when input is null', () => {
			expect(stripAnimationPrefixIfNoEntitlement(null, false)).toBeNull();
			expect(stripAnimationPrefixIfNoEntitlement(null, true)).toBeNull();
		});
		test('keeps animated prefix when entitlement is present', () => {
			expect(stripAnimationPrefixIfNoEntitlement('a_abc123', true)).toBe('a_abc123');
		});
		test('strips animated prefix when entitlement is missing', () => {
			expect(stripAnimationPrefixIfNoEntitlement('a_abc123', false)).toBe('abc123');
		});
		test('leaves static hashes untouched regardless of entitlement', () => {
			expect(stripAnimationPrefixIfNoEntitlement('xyz789', false)).toBe('xyz789');
			expect(stripAnimationPrefixIfNoEntitlement('xyz789', true)).toBe('xyz789');
		});
	});
	describe('stripAvatarForUser', () => {
		test('strips a_ prefix on user without animated-avatar entitlement', () => {
			const user = createUser({userId: createUserID(1n), avatarHash: 'a_abc123'});
			expect(userHasAnimatedAvatarEntitlement(user)).toBe(false);
			expect(stripAvatarForUser(user)).toBe('abc123');
		});
		test('keeps a_ prefix on user with premium trait', () => {
			const user = createUser({userId: createUserID(2n), avatarHash: 'a_abc123', premium: true});
			expect(userHasAnimatedAvatarEntitlement(user)).toBe(true);
			expect(stripAvatarForUser(user)).toBe('a_abc123');
		});
		test('leaves non-animated hash unchanged regardless of entitlement', () => {
			const freeUser = createUser({userId: createUserID(3n), avatarHash: 'xyz789'});
			const premiumUser = createUser({userId: createUserID(4n), avatarHash: 'xyz789', premium: true});
			expect(stripAvatarForUser(freeUser)).toBe('xyz789');
			expect(stripAvatarForUser(premiumUser)).toBe('xyz789');
		});
		test('does not strip for bots (bot avatars are exempt from gating)', () => {
			const botUser = createUser({userId: createUserID(5n), avatarHash: 'a_botanim', isBot: true});
			expect(stripAvatarForUser(botUser)).toBe('a_botanim');
		});
		test('regrant restores animated form: stored a_ prefix returns when entitlement is granted', () => {
			const lapsedUser = createUser({userId: createUserID(6n), avatarHash: 'a_persist'});
			expect(stripAvatarForUser(lapsedUser)).toBe('persist');
			const regrantedUser = createUser({userId: createUserID(6n), avatarHash: 'a_persist', premium: true});
			expect(stripAvatarForUser(regrantedUser)).toBe('a_persist');
		});
	});
	describe('stripBannerForUser', () => {
		test('hides static banner when banner entitlement is missing', () => {
			const user = createUser({userId: createUserID(7n), avatarHash: null, bannerHash: 'banner123'});
			expect(stripBannerForUser(user)).toBeNull();
		});
		test('keeps static banner when banner entitlement is present', () => {
			const user = createUser({
				userId: createUserID(8n),
				avatarHash: null,
				bannerHash: 'banner123',
				premium: true,
			});
			expect(stripBannerForUser(user)).toBe('banner123');
		});
		test('keeps animated banner when banner entitlement is present', () => {
			const user = createUser({
				userId: createUserID(9n),
				avatarHash: null,
				bannerHash: 'a_banner123',
				premium: true,
			});
			expect(stripBannerForUser(user)).toBe('a_banner123');
		});
	});
	describe('guild asset feature sanitization', () => {
		test('strips animated guild icon prefix without ANIMATED_ICON feature', () => {
			expect(stripGuildIconForFeatures('a_icon123', new Set())).toBe('icon123');
		});
		test('keeps animated guild icon prefix with ANIMATED_ICON feature', () => {
			expect(stripGuildIconForFeatures('a_icon123', new Set([GuildFeatures.ANIMATED_ICON]))).toBe('a_icon123');
		});
		test('hides guild banner without BANNER feature', () => {
			expect(stripGuildBannerForFeatures('banner123', new Set())).toBeNull();
		});
		test('strips animated guild banner prefix without ANIMATED_BANNER feature', () => {
			expect(stripGuildBannerForFeatures('a_banner123', new Set([GuildFeatures.BANNER]))).toBe('banner123');
		});
		test('keeps animated guild banner prefix with ANIMATED_BANNER feature', () => {
			expect(
				stripGuildBannerForFeatures('a_banner123', new Set([GuildFeatures.BANNER, GuildFeatures.ANIMATED_BANNER])),
			).toBe('a_banner123');
		});
		test('hides guild splashes without INVITE_SPLASH feature', () => {
			expect(stripGuildSplashForFeatures('splash123', new Set())).toBeNull();
		});
		test('keeps guild splashes with INVITE_SPLASH feature', () => {
			expect(stripGuildSplashForFeatures('splash123', new Set([GuildFeatures.INVITE_SPLASH]))).toBe('splash123');
		});
	});
});
