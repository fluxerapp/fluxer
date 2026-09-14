// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserRow} from '@app/api/database/types/UserTypes';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import {contentModerationService} from '@app/api/infrastructure/ContentModerationService';
import type {EntityAssetService, PreparedAssetUpload} from '@app/api/infrastructure/EntityAssetService';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import {profileSubstringBlocklistCache} from '@app/api/middleware/ProfileSubstringBlocklistCache';
import type {User} from '@app/api/models/User';
import type {IUserAccountRepository} from '@app/api/user/repositories/IUserAccountRepository';
import {canUseProfileTimezone, isProfileSubstringExempt} from '@app/api/user/UserHelpers';
import {deriveDominantAvatarColor} from '@app/api/utils/AvatarColorUtils';
import * as EmojiUtils from '@app/api/utils/EmojiUtils';
import {MAX_BIO_LENGTH} from '@fluxer/constants/src/LimitConstants';
import {PremiumFlags, ProfileFieldPrivacyFlags, UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {isSupportedTimeZoneId} from '@fluxer/date_utils/src/TimeZoneUtils';
import {ContentBlockedError} from '@fluxer/errors/src/domains/content/ContentBlockedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingAccessError} from '@fluxer/errors/src/domains/core/MissingAccessError';
import type {UserUpdateRequest} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';
import {ms} from 'itty-time';

type UserFieldUpdates = Partial<UserRow>;

interface ProfileUpdateResult {
	updates: UserFieldUpdates;
	preparedAvatarUpload: PreparedAssetUpload | null;
	preparedBannerUpload: PreparedAssetUpload | null;
}

interface UserAccountProfileServiceDeps {
	userAccountRepository: IUserAccountRepository;
	guildRepository: IGuildRepositoryAggregate;
	entityAssetService: EntityAssetService;
	rateLimitService: IRateLimitService;
	limitConfigService: LimitConfigService;
}

const PREMIUM_BADGE_FIELDS = [
	{field: 'premium_badge_hidden', flag: PremiumFlags.BADGE_HIDDEN},
	{field: 'premium_badge_masked', flag: PremiumFlags.BADGE_MASKED},
	{field: 'premium_badge_timestamp_hidden', flag: PremiumFlags.BADGE_TIMESTAMP_HIDDEN},
	{field: 'premium_badge_sequence_hidden', flag: PremiumFlags.BADGE_SEQUENCE_HIDDEN},
	{field: 'premium_enabled_override', flag: PremiumFlags.ENABLED_OVERRIDE},
] as const;

export class UserAccountProfileService {
	constructor(private readonly deps: UserAccountProfileServiceDeps) {}

	async processProfileUpdates(params: {user: User; data: UserUpdateRequest}): Promise<ProfileUpdateResult> {
		const {user, data} = params;
		const updates: UserFieldUpdates = {
			avatar_hash: user.avatarHash,
			banner_hash: user.bannerHash,
			flags: user.flags,
		};
		let preparedAvatarUpload: PreparedAssetUpload | null = null;
		let preparedBannerUpload: PreparedAssetUpload | null = null;
		if (data.bio !== undefined) {
			contentModerationService.scanText(data.bio, {
				userId: user.id,
				guildId: null,
				channelId: null,
				messageId: null,
				surface: 'profile_field',
			});
			await this.processBioUpdate({user, bio: data.bio, updates});
		}
		if (data.pronouns !== undefined) {
			contentModerationService.scanText(data.pronouns, {
				userId: user.id,
				guildId: null,
				channelId: null,
				messageId: null,
				surface: 'profile_field',
			});
			await this.processPronounsUpdate({user, pronouns: data.pronouns, updates});
		}
		if (data.accent_color !== undefined) {
			await this.processAccentColorUpdate({user, accentColor: data.accent_color, updates});
		}
		const canUpdateProfileTimezone = canUseProfileTimezone(user);
		if (canUpdateProfileTimezone && data.timezone !== undefined) {
			const nextTimezone = this.processTimezoneUpdate({user, timezone: data.timezone, updates});
			if (nextTimezone !== null && user.timezone === null && data.timezone_privacy_flags === undefined) {
				updates.timezone_privacy_flags = ProfileFieldPrivacyFlags.EVERYONE;
			}
		}
		if (canUpdateProfileTimezone && data.timezone_privacy_flags !== undefined) {
			this.processTimezonePrivacyFlagsUpdate({
				user,
				privacyFlags: data.timezone_privacy_flags,
				updates,
			});
		}
		if (!user.isBot) {
			this.processPremiumBadgeFlags({user, data, updates});
		}
		if (data.avatar !== undefined) {
			preparedAvatarUpload = await this.processAvatarUpdate({user, avatar: data.avatar, updates});
		}
		if (data.banner !== undefined) {
			try {
				preparedBannerUpload = await this.processBannerUpdate({user, banner: data.banner, updates});
			} catch (error) {
				if (preparedAvatarUpload) {
					await this.deps.entityAssetService.rollbackAssetUpload(preparedAvatarUpload);
				}
				throw error;
			}
		}
		if (!user.isBot) {
			this.processPremiumOnboardingDismissal({data, updates});
			this.processGiftInventoryRead({user, data, updates});
		}
		if (data.mention_flags !== undefined) {
			updates.mention_flags = data.mention_flags;
		}
		return {updates, preparedAvatarUpload, preparedBannerUpload};
	}

	async commitAssetChanges(result: ProfileUpdateResult): Promise<void> {
		await this.deps.entityAssetService.commitAssetChanges([result.preparedAvatarUpload, result.preparedBannerUpload]);
	}

	private async processBioUpdate(params: {user: User; bio: string | null; updates: UserFieldUpdates}): Promise<void> {
		const {user, bio, updates} = params;
		if (bio !== user.bio) {
			const bioRateLimit = await this.deps.rateLimitService.checkLimit({
				identifier: `bio_change:${user.id}`,
				maxAttempts: 25,
				windowMs: ms('30 minutes'),
			});
			if (!bioRateLimit.allowed) {
				const minutes = Math.ceil((bioRateLimit.retryAfter || 0) / 60);
				throw InputValidationError.fromCode('bio', ValidationErrorCodes.BIO_CHANGED_TOO_MANY_TIMES, {minutes});
			}
			const ctx = createLimitMatchContext({user});
			const maxBioLength = resolveLimitSafe(
				this.deps.limitConfigService.getConfigSnapshot(),
				ctx,
				'max_bio_length',
				MAX_BIO_LENGTH,
			);
			if (bio && bio.length > maxBioLength) {
				throw InputValidationError.fromCode('bio', ValidationErrorCodes.CONTENT_EXCEEDS_MAX_LENGTH, {
					maxLength: maxBioLength,
				});
			}
			let sanitizedBio = bio;
			if (bio) {
				sanitizedBio = await EmojiUtils.sanitizeCustomEmojis({
					content: bio,
					userId: user.id,
					webhookId: null,
					guildId: null,
					userRepository: this.deps.userAccountRepository,
					guildRepository: this.deps.guildRepository,
					limitConfigService: this.deps.limitConfigService,
				});
			}
			if (
				sanitizedBio &&
				!isProfileSubstringExempt(user) &&
				profileSubstringBlocklistCache.containsBannedSubstring('bio', sanitizedBio)
			) {
				throw new ContentBlockedError();
			}
			updates.bio = sanitizedBio;
		}
	}

	private async processPronounsUpdate(params: {
		user: User;
		pronouns: string | null;
		updates: UserFieldUpdates;
	}): Promise<void> {
		const {user, pronouns, updates} = params;
		if (pronouns !== user.pronouns) {
			const pronounsRateLimit = await this.deps.rateLimitService.checkLimit({
				identifier: `pronouns_change:${user.id}`,
				maxAttempts: 25,
				windowMs: ms('30 minutes'),
			});
			if (!pronounsRateLimit.allowed) {
				const minutes = Math.ceil((pronounsRateLimit.retryAfter || 0) / 60);
				throw InputValidationError.fromCode('pronouns', ValidationErrorCodes.PRONOUNS_CHANGED_TOO_MANY_TIMES, {
					minutes,
				});
			}
			if (
				pronouns &&
				!isProfileSubstringExempt(user) &&
				profileSubstringBlocklistCache.containsBannedSubstring('pronouns', pronouns)
			) {
				throw new ContentBlockedError();
			}
			updates.pronouns = pronouns;
		}
	}

	private async processAccentColorUpdate(params: {
		user: User;
		accentColor: number | null;
		updates: UserFieldUpdates;
	}): Promise<void> {
		const {user, accentColor, updates} = params;
		if (accentColor !== user.accentColor) {
			const accentColorRateLimit = await this.deps.rateLimitService.checkLimit({
				identifier: `accent_color_change:${user.id}`,
				maxAttempts: 25,
				windowMs: ms('30 minutes'),
			});
			if (!accentColorRateLimit.allowed) {
				const minutes = Math.ceil((accentColorRateLimit.retryAfter || 0) / 60);
				throw InputValidationError.fromCode('accent_color', ValidationErrorCodes.ACCENT_COLOR_CHANGED_TOO_MANY_TIMES, {
					minutes,
				});
			}
			updates.accent_color = accentColor;
		}
	}

	private processTimezoneUpdate(params: {
		user: User;
		timezone: string | null;
		updates: UserFieldUpdates;
	}): string | null {
		const {user, timezone, updates} = params;
		const nextTimezone = timezone?.trim() || null;
		if (nextTimezone !== null && !isSupportedTimeZoneId(nextTimezone)) {
			throw InputValidationError.fromCode('timezone', ValidationErrorCodes.INVALID_TIMEZONE_IDENTIFIER);
		}
		if (nextTimezone !== user.timezone) {
			updates.timezone = nextTimezone;
		}
		return nextTimezone;
	}

	private processTimezonePrivacyFlagsUpdate(params: {
		user: User;
		privacyFlags: number;
		updates: UserFieldUpdates;
	}): void {
		const {user, privacyFlags, updates} = params;
		if (privacyFlags !== user.timezonePrivacyFlags) {
			updates.timezone_privacy_flags = privacyFlags;
		}
	}

	private async processAvatarUpdate(params: {
		user: User;
		avatar: string | null;
		updates: UserFieldUpdates;
	}): Promise<PreparedAssetUpload | null> {
		const {user, avatar, updates} = params;
		if (avatar === null) {
			updates.avatar_hash = null;
			updates.avatar_color = null;
			if (user.avatarHash) {
				return await this.deps.entityAssetService.prepareAssetUpload({
					assetType: 'avatar',
					entityType: 'user',
					entityId: user.id,
					previousHash: user.avatarHash,
					base64Image: null,
					errorPath: 'avatar',
				});
			}
			return null;
		}
		const avatarRateLimit = await this.deps.rateLimitService.checkLimit({
			identifier: `avatar_change:${user.id}`,
			maxAttempts: 25,
			windowMs: ms('30 minutes'),
		});
		if (!avatarRateLimit.allowed) {
			const minutes = Math.ceil((avatarRateLimit.retryAfter || 0) / 60);
			throw InputValidationError.fromCode('avatar', ValidationErrorCodes.AVATAR_CHANGED_TOO_MANY_TIMES, {minutes});
		}
		const prepared = await this.deps.entityAssetService.prepareAssetUpload({
			assetType: 'avatar',
			entityType: 'user',
			entityId: user.id,
			previousHash: user.avatarHash,
			base64Image: avatar,
			errorPath: 'avatar',
		});
		const ctx = createLimitMatchContext({user});
		const hasAnimatedAvatar = resolveLimitSafe(
			this.deps.limitConfigService.getConfigSnapshot(),
			ctx,
			'feature_animated_avatar',
			0,
		);
		if (prepared.isAnimated && hasAnimatedAvatar === 0) {
			await this.deps.entityAssetService.rollbackAssetUpload(prepared);
			throw InputValidationError.fromCode('avatar', ValidationErrorCodes.ANIMATED_AVATARS_REQUIRE_PREMIUM);
		}
		if (prepared.imageBuffer) {
			const derivedColor = await deriveDominantAvatarColor(prepared.imageBuffer);
			if (derivedColor !== user.avatarColor) {
				updates.avatar_color = derivedColor;
			}
		}
		if (prepared.newHash !== user.avatarHash) {
			updates.avatar_hash = prepared.newHash;
			return prepared;
		}
		return null;
	}

	private async processBannerUpdate(params: {
		user: User;
		banner: string | null;
		updates: UserFieldUpdates;
	}): Promise<PreparedAssetUpload | null> {
		const {user, banner, updates} = params;
		if (banner === null) {
			updates.banner_color = null;
		}
		const ctx = createLimitMatchContext({user});
		const hasAnimatedBanner = resolveLimitSafe(
			this.deps.limitConfigService.getConfigSnapshot(),
			ctx,
			'feature_animated_banner',
			0,
		);
		if (banner !== null && hasAnimatedBanner === 0) {
			throw InputValidationError.fromCode('banner', ValidationErrorCodes.BANNERS_REQUIRE_PREMIUM);
		}
		const bannerRateLimit = await this.deps.rateLimitService.checkLimit({
			identifier: `banner_change:${user.id}`,
			maxAttempts: 25,
			windowMs: ms('30 minutes'),
		});
		if (!bannerRateLimit.allowed) {
			const minutes = Math.ceil((bannerRateLimit.retryAfter || 0) / 60);
			throw InputValidationError.fromCode('banner', ValidationErrorCodes.BANNER_CHANGED_TOO_MANY_TIMES, {minutes});
		}
		const prepared = await this.deps.entityAssetService.prepareAssetUpload({
			assetType: 'banner',
			entityType: 'user',
			entityId: user.id,
			previousHash: user.bannerHash,
			base64Image: banner,
			errorPath: 'banner',
		});
		if (prepared.isAnimated && hasAnimatedBanner === 0) {
			await this.deps.entityAssetService.rollbackAssetUpload(prepared);
			throw InputValidationError.fromCode('banner', ValidationErrorCodes.ANIMATED_AVATARS_REQUIRE_PREMIUM);
		}
		if (banner !== null && prepared.imageBuffer) {
			const derivedColor = await deriveDominantAvatarColor(prepared.imageBuffer);
			if (derivedColor !== user.bannerColor) {
				updates.banner_color = derivedColor;
			}
		}
		if (prepared.newHash !== user.bannerHash) {
			updates.banner_hash = prepared.newHash;
			return prepared;
		}
		return null;
	}

	private processPremiumBadgeFlags(params: {user: User; data: UserUpdateRequest; updates: UserFieldUpdates}): void {
		const {user, data, updates} = params;
		if (data.premium_enabled_override !== undefined && !(user.flags & UserFlags.STAFF)) {
			throw new MissingAccessError();
		}
		for (const {field, flag} of PREMIUM_BADGE_FIELDS) {
			const enabled = data[field];
			if (enabled === undefined) continue;
			const flags = updates.premium_flags ?? user.premiumFlags;
			updates.premium_flags = enabled ? flags | flag : flags & ~flag;
		}
	}

	private processPremiumOnboardingDismissal(params: {data: UserUpdateRequest; updates: UserFieldUpdates}): void {
		const {data, updates} = params;
		if (data.has_dismissed_premium_onboarding) {
			updates.premium_onboarding_dismissed_at = new Date();
		}
	}

	private processGiftInventoryRead(params: {user: User; data: UserUpdateRequest; updates: UserFieldUpdates}): void {
		const {user, data, updates} = params;
		if (data.has_unread_gift_inventory === false) {
			updates.gift_inventory_client_seq = user.giftInventoryServerSeq;
		}
	}
}
