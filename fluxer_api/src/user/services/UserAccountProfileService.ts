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

import {UserFlags} from '~/Constants';
import type {PartialRowUpdate, UserRow} from '~/database/CassandraTypes';
import {InputValidationError, MissingAccessError} from '~/Errors';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {EntityAssetService, PreparedAssetUpload} from '~/infrastructure/EntityAssetService';
import type {IRateLimitService} from '~/infrastructure/IRateLimitService';
import type {User} from '~/Models';
import type {UserUpdateRequest} from '~/user/UserModel';
import {deriveDominantAvatarColor} from '~/utils/AvatarColorUtils';
import * as EmojiUtils from '~/utils/EmojiUtils';
import type {IUserAccountRepository} from '../repositories/IUserAccountRepository';
import type {UserAccountUpdatePropagator} from './UserAccountUpdatePropagator';

interface UserFieldUpdates extends PartialRowUpdate<UserRow> {
	invalidateAuthSessions?: boolean;
}

export interface ProfileUpdateResult {
	updates: UserFieldUpdates;
	preparedAvatarUpload: PreparedAssetUpload | null;
	preparedBannerUpload: PreparedAssetUpload | null;
}

interface UserAccountProfileServiceDeps {
	userAccountRepository: IUserAccountRepository;
	guildRepository: IGuildRepository;
	entityAssetService: EntityAssetService;
	rateLimitService: IRateLimitService;
	updatePropagator: UserAccountUpdatePropagator;
}

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
			await this.processBioUpdate({user, bio: data.bio, updates});
		}

		if (data.pronouns !== undefined) {
			await this.processPronounsUpdate({user, pronouns: data.pronouns, updates});
		}

		if (data.accent_color !== undefined) {
			await this.processAccentColorUpdate({user, accentColor: data.accent_color, updates});
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
			this.processPremiumBadgeFlags({user, data, updates});
			this.processPremiumOnboardingDismissal({user, data, updates});
			this.processGiftInventoryRead({user, data, updates});
			this.processUsedMobileClient({user, data, updates});
		}

		return {updates, preparedAvatarUpload, preparedBannerUpload};
	}

	async commitAssetChanges(result: ProfileUpdateResult): Promise<void> {
		if (result.preparedAvatarUpload) {
			await this.deps.entityAssetService.commitAssetChange({
				prepared: result.preparedAvatarUpload,
				deferDeletion: true,
			});
		}

		if (result.preparedBannerUpload) {
			await this.deps.entityAssetService.commitAssetChange({
				prepared: result.preparedBannerUpload,
				deferDeletion: true,
			});
		}
	}

	async rollbackAssetChanges(result: ProfileUpdateResult): Promise<void> {
		if (result.preparedAvatarUpload) {
			await this.deps.entityAssetService.rollbackAssetUpload(result.preparedAvatarUpload);
		}

		if (result.preparedBannerUpload) {
			await this.deps.entityAssetService.rollbackAssetUpload(result.preparedBannerUpload);
		}
	}

	private async processBioUpdate(params: {user: User; bio: string | null; updates: UserFieldUpdates}): Promise<void> {
		const {user, bio, updates} = params;

		if (bio !== user.bio) {
			const bioRateLimit = await this.deps.rateLimitService.checkLimit({
				identifier: `bio_change:${user.id}`,
				maxAttempts: 25,
				windowMs: 30 * 60 * 1000,
			});

			if (!bioRateLimit.allowed) {
				const minutes = Math.ceil((bioRateLimit.retryAfter || 0) / 60);
				throw InputValidationError.create(
					'bio',
					`You've changed your bio too many times recently. Please try again in ${minutes} minutes.`,
				);
			}

			if (bio && bio.length > 160 && !user.isPremium()) {
				throw InputValidationError.create('bio', 'Bio longer than 160 characters requires premium');
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
				});
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
				windowMs: 30 * 60 * 1000,
			});

			if (!pronounsRateLimit.allowed) {
				const minutes = Math.ceil((pronounsRateLimit.retryAfter || 0) / 60);
				throw InputValidationError.create(
					'pronouns',
					`You've changed your pronouns too many times recently. Please try again in ${minutes} minutes.`,
				);
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
				windowMs: 30 * 60 * 1000,
			});

			if (!accentColorRateLimit.allowed) {
				const minutes = Math.ceil((accentColorRateLimit.retryAfter || 0) / 60);
				throw InputValidationError.create(
					'accent_color',
					`You've changed your accent color too many times recently. Please try again in ${minutes} minutes.`,
				);
			}

			updates.accent_color = accentColor;
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
			windowMs: 30 * 60 * 1000,
		});

		if (!avatarRateLimit.allowed) {
			const minutes = Math.ceil((avatarRateLimit.retryAfter || 0) / 60);
			throw InputValidationError.create(
				'avatar',
				`You've changed your avatar too many times recently. Please try again in ${minutes} minutes.`,
			);
		}

		const prepared = await this.deps.entityAssetService.prepareAssetUpload({
			assetType: 'avatar',
			entityType: 'user',
			entityId: user.id,
			previousHash: user.avatarHash,
			base64Image: avatar,
			errorPath: 'avatar',
		});

		if (prepared.isAnimated && !user.isPremium()) {
			await this.deps.entityAssetService.rollbackAssetUpload(prepared);
			throw InputValidationError.create('avatar', 'Animated avatars require premium');
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

		if (banner && !user.isPremium()) {
			throw InputValidationError.create('banner', 'Banners require premium');
		}

		const bannerRateLimit = await this.deps.rateLimitService.checkLimit({
			identifier: `banner_change:${user.id}`,
			maxAttempts: 25,
			windowMs: 30 * 60 * 1000,
		});

		if (!bannerRateLimit.allowed) {
			const minutes = Math.ceil((bannerRateLimit.retryAfter || 0) / 60);
			throw InputValidationError.create(
				'banner',
				`You've changed your banner too many times recently. Please try again in ${minutes} minutes.`,
			);
		}

		const prepared = await this.deps.entityAssetService.prepareAssetUpload({
			assetType: 'banner',
			entityType: 'user',
			entityId: user.id,
			previousHash: user.bannerHash,
			base64Image: banner,
			errorPath: 'banner',
		});

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
		let flagsUpdated = false;
		let newFlags = user.flags;

		if (data.premium_badge_hidden !== undefined) {
			if (data.premium_badge_hidden) {
				newFlags = newFlags | UserFlags.PREMIUM_BADGE_HIDDEN;
			} else {
				newFlags = newFlags & ~UserFlags.PREMIUM_BADGE_HIDDEN;
			}
			flagsUpdated = true;
		}

		if (data.premium_badge_masked !== undefined) {
			if (data.premium_badge_masked) {
				newFlags = newFlags | UserFlags.PREMIUM_BADGE_MASKED;
			} else {
				newFlags = newFlags & ~UserFlags.PREMIUM_BADGE_MASKED;
			}
			flagsUpdated = true;
		}

		if (data.premium_badge_timestamp_hidden !== undefined) {
			if (data.premium_badge_timestamp_hidden) {
				newFlags = newFlags | UserFlags.PREMIUM_BADGE_TIMESTAMP_HIDDEN;
			} else {
				newFlags = newFlags & ~UserFlags.PREMIUM_BADGE_TIMESTAMP_HIDDEN;
			}
			flagsUpdated = true;
		}

		if (data.premium_badge_sequence_hidden !== undefined) {
			if (data.premium_badge_sequence_hidden) {
				newFlags = newFlags | UserFlags.PREMIUM_BADGE_SEQUENCE_HIDDEN;
			} else {
				newFlags = newFlags & ~UserFlags.PREMIUM_BADGE_SEQUENCE_HIDDEN;
			}
			flagsUpdated = true;
		}

		if (data.premium_enabled_override !== undefined) {
			if (!(user.flags & UserFlags.STAFF)) {
				throw new MissingAccessError();
			}

			if (data.premium_enabled_override) {
				newFlags = newFlags | UserFlags.PREMIUM_ENABLED_OVERRIDE;
			} else {
				newFlags = newFlags & ~UserFlags.PREMIUM_ENABLED_OVERRIDE;
			}
			flagsUpdated = true;
		}

		if (flagsUpdated) {
			updates.flags = newFlags;
		}
	}

	private processPremiumOnboardingDismissal(params: {
		user: User;
		data: UserUpdateRequest;
		updates: UserFieldUpdates;
	}): void {
		const {data, updates} = params;

		if (data.has_dismissed_premium_onboarding !== undefined) {
			if (data.has_dismissed_premium_onboarding) {
				updates.premium_onboarding_dismissed_at = new Date();
			}
		}
	}

	private processGiftInventoryRead(params: {user: User; data: UserUpdateRequest; updates: UserFieldUpdates}): void {
		const {user, data, updates} = params;

		if (data.has_unread_gift_inventory === false) {
			updates.gift_inventory_client_seq = user.giftInventoryServerSeq;
		}
	}

	private processUsedMobileClient(params: {user: User; data: UserUpdateRequest; updates: UserFieldUpdates}): void {
		const {user, data, updates} = params;

		if (data.used_mobile_client !== undefined) {
			let newFlags = updates.flags ?? user.flags;
			if (data.used_mobile_client) {
				newFlags = newFlags | UserFlags.USED_MOBILE_CLIENT;
			} else {
				newFlags = newFlags & ~UserFlags.USED_MOBILE_CLIENT;
			}
			updates.flags = newFlags;
		}
	}
}
