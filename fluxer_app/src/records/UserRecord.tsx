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

import DeveloperOptionsStore from '@app/stores/DeveloperOptionsStore';
import RuntimeConfigStore from '@app/stores/RuntimeConfigStore';
import {getMaxAttachmentFileSize} from '@app/utils/AttachmentUtils';
import {LimitResolver} from '@app/utils/limits/LimitResolverAdapter';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {
	MAX_ATTACHMENTS_PER_MESSAGE,
	MAX_BIO_LENGTH,
	MAX_BOOKMARKS_NON_PREMIUM,
	MAX_FAVORITE_MEME_TAGS,
	MAX_FAVORITE_MEMES_NON_PREMIUM,
	MAX_GROUP_DM_RECIPIENTS,
	MAX_GUILDS_NON_PREMIUM,
	MAX_MESSAGE_LENGTH_NON_PREMIUM,
	MAX_PRIVATE_CHANNELS_PER_USER,
	MAX_RELATIONSHIPS,
} from '@fluxer/constants/src/LimitConstants';
import {PublicUserFlags} from '@fluxer/constants/src/UserConstants';
import type {RequiredAction, User, UserPartial, UserPrivate} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';

export type PendingBulkMessageDeletion = Readonly<{
	scheduledAt: Date;
	channelCount: number;
	messageCount: number;
}>;

interface UserRecordOptions {
	instanceId?: string;
}

export class UserRecord {
	readonly instanceId: string;
	readonly id: string;
	readonly username: string;
	readonly discriminator: string;
	readonly globalName: string | null;
	readonly avatar: string | null;
	readonly avatarColor?: number | null;
	readonly bot: boolean;
	readonly system: boolean;
	readonly flags: number;
	private readonly _email?: string | null;
	private readonly _emailBounced?: boolean;
	readonly bio?: string | null;
	readonly banner?: string | null;
	readonly bannerColor?: number | null;
	readonly pronouns?: string | null;
	readonly accentColor?: number | null;
	readonly mfaEnabled?: boolean;
	readonly phone?: string | null;
	readonly authenticatorTypes?: ReadonlyArray<number>;
	private readonly _verified?: boolean;
	private readonly _premiumType?: number | null;
	private readonly _premiumSince?: Date | null;
	private readonly _premiumUntil?: Date | null;
	private readonly _premiumWillCancel?: boolean;
	private readonly _premiumBillingCycle?: string | null;
	private readonly _premiumLifetimeSequence?: number | null;
	readonly premiumBadgeHidden?: boolean;
	readonly premiumBadgeMasked?: boolean;
	readonly premiumBadgeTimestampHidden?: boolean;
	readonly premiumBadgeSequenceHidden?: boolean;
	readonly premiumPurchaseDisabled?: boolean;
	readonly premiumEnabledOverride?: boolean;
	readonly passwordLastChangedAt?: Date | null;
	readonly requiredActions?: Array<RequiredAction> | null;
	readonly pendingBulkMessageDeletion: PendingBulkMessageDeletion | null;
	private readonly _nsfwAllowed?: boolean;
	readonly hasDismissedPremiumOnboarding?: boolean;
	private readonly _hasEverPurchased?: boolean;
	private readonly _hasUnreadGiftInventory?: boolean;
	private readonly _unreadGiftInventoryCount?: number;
	private readonly _usedMobileClient?: boolean;
	private readonly _traits: ReadonlyArray<string>;

	constructor(user: User, options?: UserRecordOptions) {
		this.instanceId = options?.instanceId ?? RuntimeConfigStore.localInstanceDomain;
		this.id = user.id;
		this.username = user.username;
		this.discriminator = user.discriminator;
		this.globalName = user.global_name ?? null;
		this.avatar = user.avatar;
		if ('avatar_color' in user) this.avatarColor = user.avatar_color;
		this.bot = user.bot ?? false;
		this.system = user.system ?? false;
		this.flags = user.flags;

		if ('email' in user) this._email = user.email;
		if ('email_bounced' in user) this._emailBounced = user.email_bounced;
		if ('bio' in user) this.bio = user.bio;
		if ('banner' in user) this.banner = user.banner;
		if ('banner_color' in user) this.bannerColor = user.banner_color;
		if ('pronouns' in user) this.pronouns = user.pronouns;
		if ('accent_color' in user) this.accentColor = user.accent_color;
		if ('mfa_enabled' in user) this.mfaEnabled = user.mfa_enabled;
		if ('phone' in user) this.phone = user.phone;
		if ('authenticator_types' in user) this.authenticatorTypes = user.authenticator_types;
		if ('verified' in user) this._verified = user.verified;
		if ('premium_type' in user) this._premiumType = user.premium_type;
		if ('premium_since' in user) this._premiumSince = user.premium_since ? new Date(user.premium_since) : null;
		if ('premium_until' in user) this._premiumUntil = user.premium_until ? new Date(user.premium_until) : null;
		if ('premium_will_cancel' in user) this._premiumWillCancel = user.premium_will_cancel;
		if ('premium_billing_cycle' in user) this._premiumBillingCycle = user.premium_billing_cycle;
		if ('premium_lifetime_sequence' in user) this._premiumLifetimeSequence = user.premium_lifetime_sequence;
		if ('premium_badge_hidden' in user) this.premiumBadgeHidden = user.premium_badge_hidden;
		if ('premium_badge_masked' in user) this.premiumBadgeMasked = user.premium_badge_masked;
		if ('premium_badge_timestamp_hidden' in user)
			this.premiumBadgeTimestampHidden = user.premium_badge_timestamp_hidden;
		if ('premium_badge_sequence_hidden' in user) this.premiumBadgeSequenceHidden = user.premium_badge_sequence_hidden;
		if ('premium_purchase_disabled' in user) this.premiumPurchaseDisabled = user.premium_purchase_disabled;
		if ('premium_enabled_override' in user) this.premiumEnabledOverride = user.premium_enabled_override;
		if ('password_last_changed_at' in user)
			this.passwordLastChangedAt = user.password_last_changed_at ? new Date(user.password_last_changed_at) : null;
		if ('required_actions' in user) {
			const actions = user.required_actions;
			this.requiredActions = actions && actions.length > 0 ? [...actions] : null;
		}
		if ('pending_bulk_message_deletion' in user && user.pending_bulk_message_deletion) {
			this.pendingBulkMessageDeletion = {
				scheduledAt: new Date(user.pending_bulk_message_deletion.scheduled_at),
				channelCount: user.pending_bulk_message_deletion.channel_count,
				messageCount: user.pending_bulk_message_deletion.message_count,
			};
		} else {
			this.pendingBulkMessageDeletion = null;
		}
		if ('nsfw_allowed' in user) this._nsfwAllowed = user.nsfw_allowed;
		if ('has_dismissed_premium_onboarding' in user)
			this.hasDismissedPremiumOnboarding = user.has_dismissed_premium_onboarding;
		if ('has_ever_purchased' in user) this._hasEverPurchased = user.has_ever_purchased;
		if ('has_unread_gift_inventory' in user) this._hasUnreadGiftInventory = user.has_unread_gift_inventory;
		if ('unread_gift_inventory_count' in user) this._unreadGiftInventoryCount = user.unread_gift_inventory_count;
		if ('used_mobile_client' in user) this._usedMobileClient = user.used_mobile_client;
		if ('traits' in user) {
			this._traits = Object.freeze((user.traits ?? []).slice());
		} else {
			this._traits = [];
		}
	}

	get email(): string | null | undefined {
		if (DeveloperOptionsStore.unclaimedAccountOverride === true) {
			return null;
		}
		return this._email;
	}

	get emailBounced(): boolean | undefined {
		return this._emailBounced;
	}

	get verified(): boolean | undefined {
		const verifiedOverride = DeveloperOptionsStore.emailVerifiedOverride;
		if (verifiedOverride != null) {
			return verifiedOverride;
		}
		return this._verified;
	}

	get premiumType(): number | null {
		const override = DeveloperOptionsStore.premiumTypeOverride;
		return override != null ? override : (this._premiumType ?? null);
	}

	get premiumSince(): Date | null | undefined {
		const override = DeveloperOptionsStore.premiumSinceOverride;
		return override != null ? override : this._premiumSince;
	}

	get premiumUntil(): Date | null | undefined {
		const override = DeveloperOptionsStore.premiumUntilOverride;
		return override != null ? override : this._premiumUntil;
	}

	get premiumBillingCycle(): string | null | undefined {
		const override = DeveloperOptionsStore.premiumBillingCycleOverride;
		return override != null ? override : this._premiumBillingCycle;
	}

	get premiumLifetimeSequence(): number | null | undefined {
		const override = DeveloperOptionsStore.premiumLifetimeSequenceOverride;
		return override != null ? override : this._premiumLifetimeSequence;
	}

	get premiumWillCancel(): boolean | undefined {
		const override = DeveloperOptionsStore.premiumWillCancelOverride;
		return override != null ? override : this._premiumWillCancel;
	}

	getPendingBulkMessageDeletion(): PendingBulkMessageDeletion | null {
		return this.pendingBulkMessageDeletion;
	}

	hasPendingBulkMessageDeletion(): boolean {
		return this.pendingBulkMessageDeletion != null;
	}

	get hasEverPurchased(): boolean | undefined {
		const override = DeveloperOptionsStore.hasEverPurchasedOverride;
		return override != null ? override : this._hasEverPurchased;
	}

	get hasUnreadGiftInventory(): boolean | undefined {
		const override = DeveloperOptionsStore.hasUnreadGiftInventoryOverride;
		return override != null ? override : this._hasUnreadGiftInventory;
	}

	get unreadGiftInventoryCount(): number | undefined {
		const override = DeveloperOptionsStore.unreadGiftInventoryCountOverride;
		return override != null ? override : this._unreadGiftInventoryCount;
	}

	get nsfwAllowed(): boolean | undefined {
		return this._nsfwAllowed;
	}

	get usedMobileClient(): boolean | undefined {
		return this._usedMobileClient;
	}

	withUpdates(updates: Partial<User>, options?: {clearMissingOptionalFields?: boolean}): UserRecord {
		const clearMissingOptionalFields = options?.clearMissingOptionalFields ?? false;
		const baseFields: UserPartial = {
			id: updates.id ?? this.id,
			username: updates.username ?? this.username,
			discriminator: updates.discriminator ?? this.discriminator,
			global_name: 'global_name' in updates ? (updates.global_name as string | null) : this.globalName,
			avatar: 'avatar' in updates ? (updates.avatar as string | null) : this.avatar,
			avatar_color: 'avatar_color' in updates ? (updates.avatar_color as number | null) : (this.avatarColor ?? null),
			bot: updates.bot ?? this.bot,
			system: updates.system ?? this.system,
			flags: updates.flags ?? this.flags,
		};

		const pendingBulkMessageDeletionValue =
			'pending_bulk_message_deletion' in updates
				? updates.pending_bulk_message_deletion
				: this.pendingBulkMessageDeletion
					? {
							scheduled_at: this.pendingBulkMessageDeletion.scheduledAt.toISOString(),
							channel_count: this.pendingBulkMessageDeletion.channelCount,
							message_count: this.pendingBulkMessageDeletion.messageCount,
						}
					: null;

		const privateFields: Partial<UserPrivate> = {
			...(this._email !== undefined || updates.email !== undefined ? {email: updates.email ?? this._email} : {}),
			...(this._emailBounced !== undefined || updates.email_bounced !== undefined
				? {email_bounced: updates.email_bounced ?? this._emailBounced}
				: {}),
			...(this.bio !== undefined || 'bio' in updates
				? {bio: 'bio' in updates && updates.bio !== undefined ? (updates.bio as string | null) : this.bio}
				: {}),
			...(this.avatarColor !== undefined || 'avatar_color' in updates
				? {
						avatar_color:
							'avatar_color' in updates && updates.avatar_color !== undefined
								? (updates.avatar_color as number | null)
								: this.avatarColor,
					}
				: {}),
			...(this.banner !== undefined || 'banner' in updates
				? {
						banner:
							'banner' in updates && updates.banner !== undefined ? (updates.banner as string | null) : this.banner,
					}
				: {}),
			...(this.bannerColor !== undefined || 'banner_color' in updates
				? {
						banner_color:
							'banner_color' in updates && updates.banner_color !== undefined
								? (updates.banner_color as number | null)
								: this.bannerColor,
					}
				: {}),
			...(this.globalName !== undefined || 'global_name' in updates
				? {
						global_name:
							'global_name' in updates && updates.global_name !== undefined
								? (updates.global_name as string | null)
								: this.globalName,
					}
				: {}),
			...(this.pronouns !== undefined || 'pronouns' in updates
				? {
						pronouns:
							'pronouns' in updates && updates.pronouns !== undefined
								? (updates.pronouns as string | null)
								: this.pronouns,
					}
				: {}),
			...(this.accentColor !== undefined || 'accent_color' in updates
				? {
						accent_color:
							'accent_color' in updates && updates.accent_color !== undefined
								? (updates.accent_color as number | null)
								: this.accentColor,
					}
				: {}),
			...(this.mfaEnabled !== undefined || updates.mfa_enabled !== undefined
				? {mfa_enabled: updates.mfa_enabled ?? this.mfaEnabled}
				: {}),
			...(this.phone !== undefined || 'phone' in updates
				? {phone: 'phone' in updates && updates.phone !== undefined ? (updates.phone as string | null) : this.phone}
				: {}),
			...(this.authenticatorTypes !== undefined || updates.authenticator_types !== undefined
				? {authenticator_types: updates.authenticator_types ?? this.authenticatorTypes}
				: {}),
			...(this._verified !== undefined || updates.verified !== undefined
				? {verified: updates.verified ?? this._verified}
				: {}),
			...(this._premiumType !== undefined || updates.premium_type !== undefined
				? {premium_type: updates.premium_type ?? this._premiumType}
				: {}),
			...(this._premiumSince !== undefined || updates.premium_since !== undefined
				? {premium_since: updates.premium_since ?? (this._premiumSince ? this._premiumSince.toISOString() : null)}
				: {}),
			...(this._premiumUntil !== undefined || updates.premium_until !== undefined
				? {premium_until: updates.premium_until ?? (this._premiumUntil ? this._premiumUntil.toISOString() : null)}
				: {}),
			...(this._premiumWillCancel !== undefined || updates.premium_will_cancel !== undefined
				? {premium_will_cancel: updates.premium_will_cancel ?? this._premiumWillCancel}
				: {}),
			...(this._premiumBillingCycle !== undefined || updates.premium_billing_cycle !== undefined
				? {premium_billing_cycle: updates.premium_billing_cycle ?? this._premiumBillingCycle}
				: {}),
			...(this._premiumLifetimeSequence !== undefined || updates.premium_lifetime_sequence !== undefined
				? {premium_lifetime_sequence: updates.premium_lifetime_sequence ?? this._premiumLifetimeSequence}
				: {}),
			...(this.premiumBadgeHidden !== undefined || updates.premium_badge_hidden !== undefined
				? {premium_badge_hidden: updates.premium_badge_hidden ?? this.premiumBadgeHidden}
				: {}),
			...(this.premiumBadgeMasked !== undefined || updates.premium_badge_masked !== undefined
				? {premium_badge_masked: updates.premium_badge_masked ?? this.premiumBadgeMasked}
				: {}),
			...(this.premiumBadgeTimestampHidden !== undefined || updates.premium_badge_timestamp_hidden !== undefined
				? {premium_badge_timestamp_hidden: updates.premium_badge_timestamp_hidden ?? this.premiumBadgeTimestampHidden}
				: {}),
			...(this.premiumBadgeSequenceHidden !== undefined || updates.premium_badge_sequence_hidden !== undefined
				? {premium_badge_sequence_hidden: updates.premium_badge_sequence_hidden ?? this.premiumBadgeSequenceHidden}
				: {}),
			...(this.premiumPurchaseDisabled !== undefined || updates.premium_purchase_disabled !== undefined
				? {premium_purchase_disabled: updates.premium_purchase_disabled ?? this.premiumPurchaseDisabled}
				: {}),
			...(this.premiumEnabledOverride !== undefined || updates.premium_enabled_override !== undefined
				? {premium_enabled_override: updates.premium_enabled_override ?? this.premiumEnabledOverride}
				: {}),
			...(this.passwordLastChangedAt !== undefined || updates.password_last_changed_at !== undefined
				? {
						password_last_changed_at:
							updates.password_last_changed_at ?? this.passwordLastChangedAt?.toISOString() ?? null,
					}
				: {}),
			pending_bulk_message_deletion: pendingBulkMessageDeletionValue,
			...(this.requiredActions !== undefined || 'required_actions' in updates
				? 'required_actions' in updates
					? {
							required_actions:
								updates.required_actions && (updates.required_actions as Array<RequiredAction>).length > 0
									? (updates.required_actions as Array<RequiredAction>)
									: null,
						}
					: clearMissingOptionalFields
						? {}
						: {required_actions: this.requiredActions}
				: {}),
			...(this._nsfwAllowed !== undefined || updates.nsfw_allowed !== undefined
				? {nsfw_allowed: updates.nsfw_allowed ?? this._nsfwAllowed}
				: {}),
			...(this.hasDismissedPremiumOnboarding !== undefined || updates.has_dismissed_premium_onboarding !== undefined
				? {
						has_dismissed_premium_onboarding:
							updates.has_dismissed_premium_onboarding ?? this.hasDismissedPremiumOnboarding,
					}
				: {}),
			...(this._hasEverPurchased !== undefined || updates.has_ever_purchased !== undefined
				? {has_ever_purchased: updates.has_ever_purchased ?? this._hasEverPurchased}
				: {}),
			...(this._hasUnreadGiftInventory !== undefined || updates.has_unread_gift_inventory !== undefined
				? {has_unread_gift_inventory: updates.has_unread_gift_inventory ?? this._hasUnreadGiftInventory}
				: {}),
			...(this._unreadGiftInventoryCount !== undefined || updates.unread_gift_inventory_count !== undefined
				? {unread_gift_inventory_count: updates.unread_gift_inventory_count ?? this._unreadGiftInventoryCount}
				: {}),
			...(this._usedMobileClient !== undefined || updates.used_mobile_client !== undefined
				? {used_mobile_client: updates.used_mobile_client ?? this._usedMobileClient}
				: {}),
			traits: updates.traits ?? [...this.traits],
		};

		return new UserRecord(
			{
				...baseFields,
				...privateFields,
			},
			{instanceId: this.instanceId},
		);
	}

	get displayName(): string {
		return this.globalName || this.username;
	}

	get tag(): string {
		return `${this.username}#${this.discriminator}`;
	}

	get createdAt(): Date {
		return new Date(SnowflakeUtils.extractTimestamp(this.id));
	}

	get traits(): ReadonlyArray<string> {
		return this._traits;
	}

	isPremium(): boolean {
		return this.premiumType != null && this.premiumType > 0;
	}

	get maxGuilds(): number {
		return this.resolveRuntimeLimit('max_guilds', MAX_GUILDS_NON_PREMIUM);
	}

	get maxMessageLength(): number {
		return this.resolveRuntimeLimit('max_message_length', MAX_MESSAGE_LENGTH_NON_PREMIUM);
	}

	get maxAttachmentFileSize(): number {
		return this.resolveRuntimeLimit('max_attachment_file_size', getMaxAttachmentFileSize());
	}

	get maxAttachmentsPerMessage(): number {
		return this.resolveRuntimeLimit('max_attachments_per_message', MAX_ATTACHMENTS_PER_MESSAGE);
	}

	get maxBioLength(): number {
		return this.resolveRuntimeLimit('max_bio_length', MAX_BIO_LENGTH);
	}

	get maxBookmarks(): number {
		return this.resolveRuntimeLimit('max_bookmarks', MAX_BOOKMARKS_NON_PREMIUM);
	}

	get maxFavoriteMemes(): number {
		return this.resolveRuntimeLimit('max_favorite_memes', MAX_FAVORITE_MEMES_NON_PREMIUM);
	}

	get maxFavoriteMemeTags(): number {
		return this.resolveRuntimeLimit('max_favorite_meme_tags', MAX_FAVORITE_MEME_TAGS);
	}

	get maxGroupDmRecipients(): number {
		return this.resolveRuntimeLimit('max_group_dm_recipients', MAX_GROUP_DM_RECIPIENTS);
	}

	get maxPrivateChannels(): number {
		return this.resolveRuntimeLimit('max_private_channels_per_user', MAX_PRIVATE_CHANNELS_PER_USER);
	}

	get maxRelationships(): number {
		return this.resolveRuntimeLimit('max_relationships', MAX_RELATIONSHIPS);
	}

	private resolveRuntimeLimit(key: LimitKey, fallback: number): number {
		return LimitResolver.resolve({
			key,
			fallback,
			context: {
				traits: this.traits,
			},
		});
	}

	isStaff(): boolean {
		return (this.flags & PublicUserFlags.STAFF) !== 0;
	}

	isClaimed(): boolean {
		return !!this.email;
	}

	equals(other: UserRecord): boolean {
		return (
			this.instanceId === other.instanceId &&
			this.id === other.id &&
			this.username === other.username &&
			this.discriminator === other.discriminator &&
			this.avatar === other.avatar &&
			this.avatarColor === other.avatarColor &&
			this.bot === other.bot &&
			this.system === other.system &&
			this.flags === other.flags &&
			this._email === other._email &&
			this._emailBounced === other._emailBounced &&
			this.bio === other.bio &&
			this.banner === other.banner &&
			this.bannerColor === other.bannerColor &&
			this.pronouns === other.pronouns &&
			this.mfaEnabled === other.mfaEnabled &&
			this.phone === other.phone &&
			JSON.stringify(this.authenticatorTypes) === JSON.stringify(other.authenticatorTypes) &&
			this._verified === other._verified &&
			this._premiumType === other._premiumType &&
			this.premiumSince?.getTime() === other.premiumSince?.getTime() &&
			this.premiumUntil?.getTime() === other.premiumUntil?.getTime() &&
			this.premiumWillCancel === other.premiumWillCancel &&
			this._premiumBillingCycle === other._premiumBillingCycle &&
			this._premiumLifetimeSequence === other._premiumLifetimeSequence &&
			this.premiumBadgeHidden === other.premiumBadgeHidden &&
			this.premiumBadgeMasked === other.premiumBadgeMasked &&
			this.premiumBadgeTimestampHidden === other.premiumBadgeTimestampHidden &&
			this.premiumBadgeSequenceHidden === other.premiumBadgeSequenceHidden &&
			this.premiumPurchaseDisabled === other.premiumPurchaseDisabled &&
			this.premiumEnabledOverride === other.premiumEnabledOverride &&
			this.passwordLastChangedAt?.getTime() === other.passwordLastChangedAt?.getTime() &&
			JSON.stringify(this.requiredActions) === JSON.stringify(other.requiredActions) &&
			((this.pendingBulkMessageDeletion === null && other.pendingBulkMessageDeletion === null) ||
				(this.pendingBulkMessageDeletion != null &&
					other.pendingBulkMessageDeletion != null &&
					this.pendingBulkMessageDeletion.channelCount === other.pendingBulkMessageDeletion.channelCount &&
					this.pendingBulkMessageDeletion.messageCount === other.pendingBulkMessageDeletion.messageCount &&
					this.pendingBulkMessageDeletion.scheduledAt.getTime() ===
						other.pendingBulkMessageDeletion.scheduledAt.getTime())) &&
			this._nsfwAllowed === other._nsfwAllowed &&
			this.hasDismissedPremiumOnboarding === other.hasDismissedPremiumOnboarding &&
			this._hasUnreadGiftInventory === other._hasUnreadGiftInventory &&
			this._unreadGiftInventoryCount === other._unreadGiftInventoryCount &&
			this._usedMobileClient === other._usedMobileClient &&
			JSON.stringify(this.traits) === JSON.stringify(other.traits)
		);
	}

	toJSON(): User {
		const normalizeDate = (value: Date | string | number | null | undefined): string | null => {
			if (value === null || value === undefined) return null;
			const date = value instanceof Date ? value : new Date(value);
			return Number.isNaN(date.getTime()) ? null : date.toISOString();
		};

		const baseFields: UserPartial = {
			id: this.id,
			username: this.username,
			discriminator: this.discriminator,
			global_name: this.globalName,
			avatar: this.avatar,
			avatar_color: this.avatarColor ?? null,
			bot: this.bot,
			system: this.system,
			flags: this.flags,
		};

		const privateFields: Partial<UserPrivate> = {
			...(this._email !== undefined ? {email: this._email} : {}),
			...(this._emailBounced !== undefined ? {email_bounced: this._emailBounced} : {}),
			...(this.bio !== undefined ? {bio: this.bio} : {}),
			...(this.banner !== undefined ? {banner: this.banner} : {}),
			...(this.avatarColor !== undefined ? {avatar_color: this.avatarColor} : {}),
			...(this.bannerColor !== undefined ? {banner_color: this.bannerColor} : {}),
			...(this.pronouns !== undefined ? {pronouns: this.pronouns} : {}),
			...(this.accentColor !== undefined ? {accent_color: this.accentColor} : {}),
			...(this.mfaEnabled !== undefined ? {mfa_enabled: this.mfaEnabled} : {}),
			...(this.phone !== undefined ? {phone: this.phone} : {}),
			...(this.authenticatorTypes !== undefined ? {authenticator_types: this.authenticatorTypes} : {}),
			...(this._verified !== undefined ? {verified: this._verified} : {}),
			...(this._premiumType !== undefined ? {premium_type: this._premiumType} : {}),
			...(this._premiumSince !== undefined ? {premium_since: normalizeDate(this._premiumSince)} : {}),
			...(this._premiumUntil !== undefined ? {premium_until: normalizeDate(this._premiumUntil)} : {}),
			...(this._premiumWillCancel !== undefined ? {premium_will_cancel: this._premiumWillCancel} : {}),
			...(this._premiumBillingCycle !== undefined ? {premium_billing_cycle: this._premiumBillingCycle} : {}),
			...(this._premiumLifetimeSequence !== undefined
				? {premium_lifetime_sequence: this._premiumLifetimeSequence}
				: {}),
			...(this.premiumBadgeHidden !== undefined ? {premium_badge_hidden: this.premiumBadgeHidden} : {}),
			...(this.premiumBadgeMasked !== undefined ? {premium_badge_masked: this.premiumBadgeMasked} : {}),
			...(this.premiumBadgeTimestampHidden !== undefined
				? {premium_badge_timestamp_hidden: this.premiumBadgeTimestampHidden}
				: {}),
			...(this.premiumBadgeSequenceHidden !== undefined
				? {premium_badge_sequence_hidden: this.premiumBadgeSequenceHidden}
				: {}),
			...(this.premiumPurchaseDisabled !== undefined ? {premium_purchase_disabled: this.premiumPurchaseDisabled} : {}),
			...(this.premiumEnabledOverride !== undefined ? {premium_enabled_override: this.premiumEnabledOverride} : {}),
			...(this.passwordLastChangedAt !== undefined
				? {password_last_changed_at: normalizeDate(this.passwordLastChangedAt)}
				: {}),
			...(this.requiredActions !== undefined ? {required_actions: this.requiredActions} : {}),
			...(this.pendingBulkMessageDeletion !== undefined
				? {
						pending_bulk_message_deletion: this.pendingBulkMessageDeletion
							? {
									scheduled_at: this.pendingBulkMessageDeletion.scheduledAt.toISOString(),
									channel_count: this.pendingBulkMessageDeletion.channelCount,
									message_count: this.pendingBulkMessageDeletion.messageCount,
								}
							: null,
					}
				: {}),
			...(this._nsfwAllowed !== undefined ? {nsfw_allowed: this._nsfwAllowed} : {}),
			...(this.hasDismissedPremiumOnboarding !== undefined
				? {has_dismissed_premium_onboarding: this.hasDismissedPremiumOnboarding}
				: {}),
			...(this._hasUnreadGiftInventory !== undefined ? {has_unread_gift_inventory: this._hasUnreadGiftInventory} : {}),
			...(this._unreadGiftInventoryCount !== undefined
				? {unread_gift_inventory_count: this._unreadGiftInventoryCount}
				: {}),
			...(this._usedMobileClient !== undefined ? {used_mobile_client: this._usedMobileClient} : {}),
			traits: [...this.traits],
		};

		return {
			...baseFields,
			...privateFields,
		};
	}
}
