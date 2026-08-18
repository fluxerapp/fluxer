// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {
	type MentionReplyPreference,
	ProfileFieldPrivacyFlags,
	PublicUserFlags,
	UserPremiumTypes,
} from '@fluxer/constants/src/UserConstants';
import {MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';
import {DEFAULT_STOCK_LIMITS} from '@fluxer/limits/src/LimitDefaults';
import type {
	RequiredAction,
	UserPartial,
	UserPrivate,
	User as WireUser,
} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';

interface UserRecordOptions {
	instanceId?: string;
}

type MergeOptions = Readonly<{
	clearMissing: boolean;
}>;

type MutableWireUser = {
	-readonly [K in keyof WireUser]: WireUser[K];
};

const EMPTY_STRING_ARRAY: ReadonlyArray<string> = Object.freeze([]);
const EMPTY_REQUIRED_ACTIONS: ReadonlyArray<RequiredAction> = Object.freeze([]);
const EMPTY_AUTH_TYPES: ReadonlyArray<number> = Object.freeze([]);

function parseDateOrNull(value: string | number | Date | null | undefined): Date | null {
	if (value == null) return null;
	const date = value instanceof Date ? value : new Date(value);
	return Number.isNaN(date.getTime()) ? null : date;
}

function dateToIsoOrNull(value: Date | null | undefined): string | null {
	if (value == null) return null;
	return Number.isNaN(value.getTime()) ? null : value.toISOString();
}

function pickField<K extends keyof WireUser>(
	current: WireUser[K] | undefined,
	updates: Partial<WireUser>,
	key: K,
	opts: MergeOptions,
): WireUser[K] | undefined {
	if (key in updates) return updates[key];
	if (opts.clearMissing) return undefined;
	return current;
}

function pickDateField<K extends keyof WireUser>(
	current: Date | null | undefined,
	updates: Partial<WireUser>,
	key: K,
	opts: MergeOptions,
): Date | null | undefined {
	if (key in updates) {
		return parseDateOrNull(updates[key] as string | null | undefined);
	}
	if (opts.clearMissing) return undefined;
	return current;
}

function mergeTraitsArray(
	current: ReadonlyArray<string>,
	updates: Partial<WireUser>,
	opts: MergeOptions,
): ReadonlyArray<string> {
	if (!('traits' in updates)) {
		return opts.clearMissing ? EMPTY_STRING_ARRAY : current;
	}
	const raw = updates.traits;
	if (!Array.isArray(raw)) return current;
	const out = dedupeFiltered(raw, parseStringItem);
	if (out.length === 0 && !opts.clearMissing) {
		return current;
	}
	return out.length === 0 ? EMPTY_STRING_ARRAY : Object.freeze(out);
}

function mergeAuthoritativeArray<T>(
	current: ReadonlyArray<T>,
	updates: Partial<WireUser>,
	key: keyof WireUser,
	parseItem: (item: unknown) => T | null,
	opts: MergeOptions,
	emptySentinel: ReadonlyArray<T>,
): ReadonlyArray<T> {
	if (!(key in updates)) {
		return opts.clearMissing ? emptySentinel : current;
	}
	const raw = updates[key];
	if (!Array.isArray(raw)) return current;
	const out = dedupeFiltered(raw, parseItem);
	return out.length === 0 ? emptySentinel : Object.freeze(out);
}

function dedupeFiltered<T>(raw: ReadonlyArray<unknown>, parseItem: (item: unknown) => T | null): Array<T> {
	const seen = new Set<T>();
	const out: Array<T> = [];
	for (const item of raw) {
		const parsed = parseItem(item);
		if (parsed == null) continue;
		if (seen.has(parsed)) continue;
		seen.add(parsed);
		out.push(parsed);
	}
	return out;
}

const parseStringItem = (item: unknown): string | null => (typeof item === 'string' && item.length > 0 ? item : null);
const parseRequiredAction = (item: unknown): RequiredAction | null =>
	typeof item === 'string' && item.length > 0 ? (item as RequiredAction) : null;
const parseAuthenticatorType = (item: unknown): number | null => {
	if (typeof item !== 'number' || !Number.isFinite(item)) return null;
	return item;
};

function hasKey<K extends string>(obj: object, key: K): boolean {
	return key in obj;
}

export class User {
	readonly instanceId: string;
	readonly id: string;
	readonly username: string;
	readonly discriminator: string;
	readonly globalName: string | null;
	readonly avatar: string | null;
	readonly bot: boolean;
	readonly system: boolean;
	readonly flags: number;
	readonly mentionFlags: MentionReplyPreference;
	readonly avatarColor: number | null | undefined;
	private readonly _isStaff: boolean | undefined;
	private readonly _email: string | null | undefined;
	private readonly _emailBounced: boolean | undefined;
	readonly bio: string | null | undefined;
	readonly banner: string | null | undefined;
	readonly bannerColor: number | null | undefined;
	readonly pronouns: string | null | undefined;
	readonly accentColor: number | null | undefined;
	readonly timezone: string | null | undefined;
	readonly timezonePrivacyFlags: number | undefined;
	readonly mfaEnabled: boolean | undefined;
	readonly hasVerifiedPhone: boolean | undefined;
	readonly authenticatorTypes: ReadonlyArray<number> | undefined;
	private readonly _verified: boolean | undefined;
	private readonly _premiumType: number | null | undefined;
	private readonly _premiumSince: Date | null | undefined;
	private readonly _premiumUntil: Date | null | undefined;
	private readonly _premiumWillCancel: boolean | undefined;
	private readonly _premiumBillingCycle: string | null | undefined;
	private readonly _premiumLifetimeSequence: number | null | undefined;
	private readonly _premiumGraceEndsAt: Date | null | undefined;
	private readonly _premiumDiscriminator: boolean | undefined;
	readonly premiumBadgeHidden: boolean | undefined;
	readonly premiumBadgeMasked: boolean | undefined;
	readonly premiumBadgeTimestampHidden: boolean | undefined;
	readonly premiumBadgeSequenceHidden: boolean | undefined;
	readonly premiumEnabledOverride: boolean | undefined;
	readonly premiumPerksDisabled: boolean | undefined;
	readonly passwordLastChangedAt: Date | null | undefined;
	readonly lastVoiceActivitySharingChangeAt: Date | null | undefined;
	readonly requiredActions: ReadonlyArray<RequiredAction> | undefined;
	private readonly _nsfwAllowed: boolean | undefined;
	readonly hasDismissedPremiumOnboarding: boolean | undefined;
	private readonly _hasEverPurchased: boolean | undefined;
	private readonly _hasUnreadGiftInventory: boolean | undefined;
	private readonly _unreadGiftInventoryCount: number | undefined;
	private readonly _ageVerifiedAdult: boolean | undefined;
	private readonly _termsAgreedAt: Date | null | undefined;
	private readonly _privacyAgreedAt: Date | null | undefined;
	private readonly _traits: ReadonlyArray<string>;

	constructor(user: WireUser, options?: UserRecordOptions) {
		this.instanceId = options?.instanceId ?? RuntimeConfig.localInstanceDomain;
		this.id = user.id;
		this.username = user.username;
		this.discriminator = user.discriminator;
		this.globalName = user.global_name ?? null;
		this.avatar = user.avatar;
		this.bot = user.bot ?? false;
		this.system = user.system ?? false;
		this.flags = user.flags;
		this.mentionFlags = user.mention_flags ?? 0;
		this.avatarColor = hasKey(user, 'avatar_color') ? user.avatar_color : undefined;
		this._isStaff = hasKey(user, 'is_staff') ? user.is_staff : undefined;
		this._email = hasKey(user, 'email') ? (user.email ?? null) : undefined;
		this._emailBounced = hasKey(user, 'email_bounced') ? user.email_bounced : undefined;
		this.bio = hasKey(user, 'bio') ? (user.bio ?? null) : undefined;
		this.banner = hasKey(user, 'banner') ? (user.banner ?? null) : undefined;
		this.bannerColor = hasKey(user, 'banner_color') ? (user.banner_color ?? null) : undefined;
		this.pronouns = hasKey(user, 'pronouns') ? (user.pronouns ?? null) : undefined;
		this.accentColor = hasKey(user, 'accent_color') ? (user.accent_color ?? null) : undefined;
		const hasProfileTimezoneAccess = this._isStaff ?? (this.flags & PublicUserFlags.STAFF) !== 0;
		this.timezone = hasProfileTimezoneAccess && hasKey(user, 'timezone') ? (user.timezone ?? null) : undefined;
		this.timezonePrivacyFlags =
			hasProfileTimezoneAccess && hasKey(user, 'timezone_privacy_flags')
				? (user.timezone_privacy_flags ?? ProfileFieldPrivacyFlags.EVERYONE)
				: undefined;
		this.mfaEnabled = hasKey(user, 'mfa_enabled') ? user.mfa_enabled : undefined;
		this.hasVerifiedPhone = hasKey(user, 'has_verified_phone') ? user.has_verified_phone : undefined;
		this.authenticatorTypes = hasKey(user, 'authenticator_types')
			? Object.freeze(((user.authenticator_types ?? []) as ReadonlyArray<number>).slice())
			: undefined;
		this._verified = hasKey(user, 'verified') ? user.verified : undefined;
		this._premiumType = hasKey(user, 'premium_type') ? (user.premium_type ?? null) : undefined;
		this._premiumSince = hasKey(user, 'premium_since') ? parseDateOrNull(user.premium_since) : undefined;
		this._premiumUntil = hasKey(user, 'premium_until') ? parseDateOrNull(user.premium_until) : undefined;
		this._premiumWillCancel = hasKey(user, 'premium_will_cancel') ? user.premium_will_cancel : undefined;
		this._premiumBillingCycle = hasKey(user, 'premium_billing_cycle')
			? (user.premium_billing_cycle ?? null)
			: undefined;
		this._premiumGraceEndsAt = hasKey(user, 'premium_grace_ends_at')
			? parseDateOrNull(user.premium_grace_ends_at)
			: undefined;
		this._premiumLifetimeSequence = hasKey(user, 'premium_lifetime_sequence')
			? (user.premium_lifetime_sequence ?? null)
			: undefined;
		this._premiumDiscriminator = hasKey(user, 'premium_discriminator') ? user.premium_discriminator : undefined;
		this.premiumBadgeHidden = hasKey(user, 'premium_badge_hidden') ? user.premium_badge_hidden : undefined;
		this.premiumBadgeMasked = hasKey(user, 'premium_badge_masked') ? user.premium_badge_masked : undefined;
		this.premiumBadgeTimestampHidden = hasKey(user, 'premium_badge_timestamp_hidden')
			? user.premium_badge_timestamp_hidden
			: undefined;
		this.premiumBadgeSequenceHidden = hasKey(user, 'premium_badge_sequence_hidden')
			? user.premium_badge_sequence_hidden
			: undefined;
		this.premiumEnabledOverride = hasKey(user, 'premium_enabled_override') ? user.premium_enabled_override : undefined;
		this.premiumPerksDisabled = hasKey(user, 'premium_perks_disabled') ? user.premium_perks_disabled : undefined;
		this.passwordLastChangedAt = hasKey(user, 'password_last_changed_at')
			? parseDateOrNull(user.password_last_changed_at)
			: undefined;
		this.lastVoiceActivitySharingChangeAt = hasKey(user, 'last_voice_activity_sharing_change_at')
			? parseDateOrNull(user.last_voice_activity_sharing_change_at)
			: undefined;
		this.requiredActions = hasKey(user, 'required_actions')
			? mergeAuthoritativeArray(
					EMPTY_REQUIRED_ACTIONS,
					user,
					'required_actions',
					parseRequiredAction,
					{clearMissing: true},
					EMPTY_REQUIRED_ACTIONS,
				)
			: undefined;
		this._nsfwAllowed = hasKey(user, 'nsfw_allowed') ? user.nsfw_allowed : undefined;
		this.hasDismissedPremiumOnboarding = hasKey(user, 'has_dismissed_premium_onboarding')
			? user.has_dismissed_premium_onboarding
			: undefined;
		this._hasEverPurchased = hasKey(user, 'has_ever_purchased') ? user.has_ever_purchased : undefined;
		this._hasUnreadGiftInventory = hasKey(user, 'has_unread_gift_inventory')
			? user.has_unread_gift_inventory
			: undefined;
		this._unreadGiftInventoryCount = hasKey(user, 'unread_gift_inventory_count')
			? user.unread_gift_inventory_count
			: undefined;
		this._ageVerifiedAdult = hasKey(user, 'age_verified_adult') ? user.age_verified_adult : undefined;
		this._termsAgreedAt = hasKey(user, 'terms_agreed_at') ? parseDateOrNull(user.terms_agreed_at) : undefined;
		this._privacyAgreedAt = hasKey(user, 'privacy_agreed_at') ? parseDateOrNull(user.privacy_agreed_at) : undefined;
		this._traits = mergeTraitsArray(EMPTY_STRING_ARRAY, user, {clearMissing: true});
	}

	get email(): string | null | undefined {
		if (DeveloperOptions.unclaimedAccountOverride === true) {
			return null;
		}
		return this._email;
	}

	get emailBounced(): boolean | undefined {
		return this._emailBounced;
	}

	get verified(): boolean | undefined {
		const verifiedOverride = DeveloperOptions.emailVerifiedOverride;
		if (verifiedOverride != null) {
			return verifiedOverride;
		}
		return this._verified;
	}

	get premiumType(): number | null {
		const override = DeveloperOptions.premiumTypeOverride;
		const raw = override != null ? override : (this._premiumType ?? null);
		if (raw != null && raw > 0 && raw !== UserPremiumTypes.LIFETIME && this.isPremiumExpiredLocally()) {
			return UserPremiumTypes.NONE;
		}
		return raw;
	}

	get premiumSince(): Date | null | undefined {
		const override = DeveloperOptions.premiumSinceOverride;
		return override != null ? override : this._premiumSince;
	}

	get premiumUntil(): Date | null | undefined {
		const override = DeveloperOptions.premiumUntilOverride;
		return override != null ? override : this._premiumUntil;
	}

	get premiumBillingCycle(): string | null | undefined {
		const override = DeveloperOptions.premiumBillingCycleOverride;
		return override != null ? override : this._premiumBillingCycle;
	}

	get premiumLifetimeSequence(): number | null | undefined {
		const override = DeveloperOptions.premiumLifetimeSequenceOverride;
		return override != null ? override : this._premiumLifetimeSequence;
	}

	get premiumGraceEndsAt(): Date | null | undefined {
		return this._premiumGraceEndsAt;
	}

	get premiumWillCancel(): boolean | undefined {
		const override = DeveloperOptions.premiumWillCancelOverride;
		return override != null ? override : this._premiumWillCancel;
	}

	get premiumDiscriminator(): boolean {
		return this._premiumDiscriminator ?? false;
	}

	get hasEverPurchased(): boolean | undefined {
		const override = DeveloperOptions.hasEverPurchasedOverride;
		return override != null ? override : this._hasEverPurchased;
	}

	get hasUnreadGiftInventory(): boolean | undefined {
		const override = DeveloperOptions.hasUnreadGiftInventoryOverride;
		return override != null ? override : this._hasUnreadGiftInventory;
	}

	get unreadGiftInventoryCount(): number | undefined {
		const override = DeveloperOptions.unreadGiftInventoryCountOverride;
		return override != null ? override : this._unreadGiftInventoryCount;
	}

	get nsfwAllowed(): boolean | undefined {
		return this._nsfwAllowed;
	}

	get matureContentAllowed(): boolean | undefined {
		return this._nsfwAllowed;
	}

	get ageVerifiedAdult(): boolean | undefined {
		return this._ageVerifiedAdult;
	}

	get matureContentCheckComplete(): boolean | undefined {
		return this._ageVerifiedAdult;
	}

	get termsAgreedAt(): Date | null | undefined {
		return this._termsAgreedAt;
	}

	get privacyAgreedAt(): Date | null | undefined {
		return this._privacyAgreedAt;
	}

	get traits(): ReadonlyArray<string> {
		return this._traits;
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

	private buildMergedPayload(updates: Partial<WireUser>, opts: MergeOptions): WireUser {
		const u = updates;
		const result: MutableWireUser = {
			id: u.id ?? this.id,
			username: u.username ?? this.username,
			discriminator: u.discriminator ?? this.discriminator,
			global_name: hasKey(u, 'global_name') ? (u.global_name ?? null) : this.globalName,
			avatar: hasKey(u, 'avatar') ? (u.avatar ?? null) : this.avatar,
			avatar_color: hasKey(u, 'avatar_color') ? (u.avatar_color ?? null) : (this.avatarColor ?? null),
			bot: u.bot ?? this.bot,
			system: u.system ?? this.system,
			flags: u.flags ?? this.flags,
			mention_flags: hasKey(u, 'mention_flags') ? u.mention_flags : this.mentionFlags || undefined,
		};
		const isStaff = pickField(this._isStaff, u, 'is_staff', opts);
		if (isStaff !== undefined) result.is_staff = isStaff;
		const avatarColor = pickField(this.avatarColor, u, 'avatar_color', opts);
		if (avatarColor !== undefined) result.avatar_color = avatarColor ?? null;
		const email = pickField(this._email, u, 'email', opts);
		if (email !== undefined) result.email = email;
		const emailBounced = pickField(this._emailBounced, u, 'email_bounced', opts);
		if (emailBounced !== undefined) result.email_bounced = emailBounced;
		const bio = pickField(this.bio, u, 'bio', opts);
		if (bio !== undefined) result.bio = bio;
		const banner = pickField(this.banner, u, 'banner', opts);
		if (banner !== undefined) result.banner = banner;
		const bannerColor = pickField(this.bannerColor, u, 'banner_color', opts);
		if (bannerColor !== undefined) result.banner_color = bannerColor;
		const pronouns = pickField(this.pronouns, u, 'pronouns', opts);
		if (pronouns !== undefined) result.pronouns = pronouns;
		const accentColor = pickField(this.accentColor, u, 'accent_color', opts);
		if (accentColor !== undefined) result.accent_color = accentColor;
		const hasProfileTimezoneAccess = isStaff ?? (result.flags & PublicUserFlags.STAFF) !== 0;
		if (hasProfileTimezoneAccess) {
			const timezone = pickField(this.timezone, u, 'timezone', opts);
			if (timezone !== undefined) result.timezone = timezone;
			const timezonePrivacyFlags = pickField(this.timezonePrivacyFlags, u, 'timezone_privacy_flags', opts);
			if (timezonePrivacyFlags !== undefined) result.timezone_privacy_flags = timezonePrivacyFlags;
		}
		const mfaEnabled = pickField(this.mfaEnabled, u, 'mfa_enabled', opts);
		if (mfaEnabled !== undefined) result.mfa_enabled = mfaEnabled;
		const hasVerifiedPhone = pickField(this.hasVerifiedPhone, u, 'has_verified_phone', opts);
		if (hasVerifiedPhone !== undefined) result.has_verified_phone = hasVerifiedPhone;
		if (hasKey(u, 'authenticator_types') || this.authenticatorTypes !== undefined || opts.clearMissing) {
			result.authenticator_types = mergeAuthoritativeArray(
				this.authenticatorTypes ?? EMPTY_AUTH_TYPES,
				u,
				'authenticator_types',
				parseAuthenticatorType,
				opts,
				EMPTY_AUTH_TYPES,
			);
		}
		const verified = pickField(this._verified, u, 'verified', opts);
		if (verified !== undefined) result.verified = verified;
		const premiumType = pickField(this._premiumType, u, 'premium_type', opts);
		if (premiumType !== undefined) result.premium_type = premiumType ?? null;
		const premiumSince = pickDateField(this._premiumSince, u, 'premium_since', opts);
		if (premiumSince !== undefined) result.premium_since = dateToIsoOrNull(premiumSince);
		const premiumUntil = pickDateField(this._premiumUntil, u, 'premium_until', opts);
		if (premiumUntil !== undefined) result.premium_until = dateToIsoOrNull(premiumUntil);
		const premiumWillCancel = pickField(this._premiumWillCancel, u, 'premium_will_cancel', opts);
		if (premiumWillCancel !== undefined) result.premium_will_cancel = premiumWillCancel;
		const premiumBillingCycle = pickField(this._premiumBillingCycle, u, 'premium_billing_cycle', opts);
		if (premiumBillingCycle !== undefined) result.premium_billing_cycle = premiumBillingCycle;
		const premiumLifetimeSequence = pickField(this._premiumLifetimeSequence, u, 'premium_lifetime_sequence', opts);
		if (premiumLifetimeSequence !== undefined) result.premium_lifetime_sequence = premiumLifetimeSequence;
		const premiumGraceEndsAt = pickDateField(this._premiumGraceEndsAt, u, 'premium_grace_ends_at', opts);
		if (premiumGraceEndsAt !== undefined) result.premium_grace_ends_at = dateToIsoOrNull(premiumGraceEndsAt);
		const premiumDiscriminator = pickField(this._premiumDiscriminator, u, 'premium_discriminator', opts);
		if (premiumDiscriminator !== undefined) result.premium_discriminator = premiumDiscriminator;
		const premiumBadgeHidden = pickField(this.premiumBadgeHidden, u, 'premium_badge_hidden', opts);
		if (premiumBadgeHidden !== undefined) result.premium_badge_hidden = premiumBadgeHidden;
		const premiumBadgeMasked = pickField(this.premiumBadgeMasked, u, 'premium_badge_masked', opts);
		if (premiumBadgeMasked !== undefined) result.premium_badge_masked = premiumBadgeMasked;
		const premiumBadgeTimestampHidden = pickField(
			this.premiumBadgeTimestampHidden,
			u,
			'premium_badge_timestamp_hidden',
			opts,
		);
		if (premiumBadgeTimestampHidden !== undefined) result.premium_badge_timestamp_hidden = premiumBadgeTimestampHidden;
		const premiumBadgeSequenceHidden = pickField(
			this.premiumBadgeSequenceHidden,
			u,
			'premium_badge_sequence_hidden',
			opts,
		);
		if (premiumBadgeSequenceHidden !== undefined) result.premium_badge_sequence_hidden = premiumBadgeSequenceHidden;
		const premiumEnabledOverride = pickField(this.premiumEnabledOverride, u, 'premium_enabled_override', opts);
		if (premiumEnabledOverride !== undefined) result.premium_enabled_override = premiumEnabledOverride;
		const premiumPerksDisabled = pickField(this.premiumPerksDisabled, u, 'premium_perks_disabled', opts);
		if (premiumPerksDisabled !== undefined) result.premium_perks_disabled = premiumPerksDisabled;
		const passwordLastChangedAt = pickDateField(this.passwordLastChangedAt, u, 'password_last_changed_at', opts);
		if (passwordLastChangedAt !== undefined) result.password_last_changed_at = dateToIsoOrNull(passwordLastChangedAt);
		const lastVoiceActivitySharingChangeAt = pickDateField(
			this.lastVoiceActivitySharingChangeAt,
			u,
			'last_voice_activity_sharing_change_at',
			opts,
		);
		if (lastVoiceActivitySharingChangeAt !== undefined) {
			result.last_voice_activity_sharing_change_at = dateToIsoOrNull(lastVoiceActivitySharingChangeAt);
		}
		if (this.requiredActions !== undefined || hasKey(u, 'required_actions') || opts.clearMissing) {
			result.required_actions = mergeAuthoritativeArray(
				this.requiredActions ?? EMPTY_REQUIRED_ACTIONS,
				u,
				'required_actions',
				parseRequiredAction,
				opts,
				EMPTY_REQUIRED_ACTIONS,
			);
		}
		const nsfwAllowed = pickField(this._nsfwAllowed, u, 'nsfw_allowed', opts);
		if (nsfwAllowed !== undefined) result.nsfw_allowed = nsfwAllowed;
		const hasDismissedPremiumOnboarding = pickField(
			this.hasDismissedPremiumOnboarding,
			u,
			'has_dismissed_premium_onboarding',
			opts,
		);
		if (hasDismissedPremiumOnboarding !== undefined)
			result.has_dismissed_premium_onboarding = hasDismissedPremiumOnboarding;
		const hasEverPurchased = pickField(this._hasEverPurchased, u, 'has_ever_purchased', opts);
		if (hasEverPurchased !== undefined) result.has_ever_purchased = hasEverPurchased;
		const hasUnreadGiftInventory = pickField(this._hasUnreadGiftInventory, u, 'has_unread_gift_inventory', opts);
		if (hasUnreadGiftInventory !== undefined) result.has_unread_gift_inventory = hasUnreadGiftInventory;
		const unreadGiftInventoryCount = pickField(this._unreadGiftInventoryCount, u, 'unread_gift_inventory_count', opts);
		if (unreadGiftInventoryCount !== undefined) result.unread_gift_inventory_count = unreadGiftInventoryCount;
		const ageVerifiedAdult = pickField(this._ageVerifiedAdult, u, 'age_verified_adult', opts);
		if (ageVerifiedAdult !== undefined) result.age_verified_adult = ageVerifiedAdult;
		const termsAgreedAt = pickDateField(this._termsAgreedAt, u, 'terms_agreed_at', opts);
		if (termsAgreedAt !== undefined) result.terms_agreed_at = dateToIsoOrNull(termsAgreedAt);
		const privacyAgreedAt = pickDateField(this._privacyAgreedAt, u, 'privacy_agreed_at', opts);
		if (privacyAgreedAt !== undefined) result.privacy_agreed_at = dateToIsoOrNull(privacyAgreedAt);
		result.traits = mergeTraitsArray(this._traits, u, opts);
		return result;
	}

	withUpdates(
		updates: Partial<WireUser>,
		options?: {
			clearMissingOptionalFields?: boolean;
		},
	): User {
		const opts: MergeOptions = {clearMissing: options?.clearMissingOptionalFields ?? false};
		const merged = this.buildMergedPayload(updates, opts);
		return new User(merged, {instanceId: this.instanceId});
	}

	isPremium(): boolean {
		if (this.premiumPerksDisabled === true) return false;
		if (this.premiumType != null && this.premiumType > 0) return true;
		return this._traits.includes('premium');
	}

	private isPremiumExpiredLocally(): boolean {
		const premiumUntil = this._premiumUntil;
		if (!premiumUntil) return false;
		const t = premiumUntil.getTime();
		if (Number.isNaN(t)) return false;
		const gracePeriodMs = 3 * MS_PER_DAY;
		return Date.now() > t + gracePeriodMs;
	}

	get maxGuilds(): number {
		return this.resolveRuntimeLimit('max_guilds', DEFAULT_STOCK_LIMITS.max_guilds);
	}

	get maxMessageLength(): number {
		return this.resolveRuntimeLimit('max_message_length', DEFAULT_STOCK_LIMITS.max_message_length);
	}

	get maxAttachmentFileSize(): number {
		return this.resolveRuntimeLimit('max_attachment_file_size', DEFAULT_STOCK_LIMITS.max_attachment_file_size);
	}

	get maxAttachmentsPerMessage(): number {
		return this.resolveRuntimeLimit('max_attachments_per_message', DEFAULT_STOCK_LIMITS.max_attachments_per_message);
	}

	get maxBioLength(): number {
		return this.resolveRuntimeLimit('max_bio_length', DEFAULT_STOCK_LIMITS.max_bio_length);
	}

	get maxBookmarks(): number {
		return this.resolveRuntimeLimit('max_bookmarks', DEFAULT_STOCK_LIMITS.max_bookmarks);
	}

	get maxFavoriteMemes(): number {
		return this.resolveRuntimeLimit('max_favorite_memes', DEFAULT_STOCK_LIMITS.max_favorite_memes);
	}

	get maxFavoriteMemeTags(): number {
		return this.resolveRuntimeLimit('max_favorite_meme_tags', DEFAULT_STOCK_LIMITS.max_favorite_meme_tags);
	}

	get maxGroupDmRecipients(): number {
		return this.resolveRuntimeLimit('max_group_dm_recipients', DEFAULT_STOCK_LIMITS.max_group_dm_recipients);
	}

	get maxPrivateChannels(): number {
		return this.resolveRuntimeLimit(
			'max_private_channels_per_user',
			DEFAULT_STOCK_LIMITS.max_private_channels_per_user,
		);
	}

	get maxRelationships(): number {
		return this.resolveRuntimeLimit('max_relationships', DEFAULT_STOCK_LIMITS.max_relationships);
	}

	private resolveRuntimeLimit(key: LimitKey, fallback: number): number {
		return LimitResolver.resolve({
			key,
			fallback,
			context: {traits: this._traits},
		});
	}

	isStaff(): boolean {
		return this._isStaff ?? (this.flags & PublicUserFlags.STAFF) !== 0;
	}

	isClaimed(): boolean {
		return !!this.email;
	}

	equals(other: User): boolean {
		if (this === other) return true;
		return (
			this.instanceId === other.instanceId &&
			this.id === other.id &&
			this.username === other.username &&
			this.discriminator === other.discriminator &&
			this.globalName === other.globalName &&
			this.avatar === other.avatar &&
			this.avatarColor === other.avatarColor &&
			this.bot === other.bot &&
			this.system === other.system &&
			this.flags === other.flags &&
			this.mentionFlags === other.mentionFlags &&
			this._isStaff === other._isStaff &&
			this._email === other._email &&
			this._emailBounced === other._emailBounced &&
			this.bio === other.bio &&
			this.banner === other.banner &&
			this.bannerColor === other.bannerColor &&
			this.pronouns === other.pronouns &&
			this.accentColor === other.accentColor &&
			this.timezone === other.timezone &&
			this.timezonePrivacyFlags === other.timezonePrivacyFlags &&
			this.mfaEnabled === other.mfaEnabled &&
			this.hasVerifiedPhone === other.hasVerifiedPhone &&
			arraysShallowEqual(this.authenticatorTypes, other.authenticatorTypes) &&
			this._verified === other._verified &&
			this._premiumType === other._premiumType &&
			datesEqual(this._premiumSince, other._premiumSince) &&
			datesEqual(this._premiumUntil, other._premiumUntil) &&
			datesEqual(this._premiumGraceEndsAt, other._premiumGraceEndsAt) &&
			this._premiumWillCancel === other._premiumWillCancel &&
			this._premiumBillingCycle === other._premiumBillingCycle &&
			this._premiumLifetimeSequence === other._premiumLifetimeSequence &&
			this._premiumDiscriminator === other._premiumDiscriminator &&
			this.premiumBadgeHidden === other.premiumBadgeHidden &&
			this.premiumBadgeMasked === other.premiumBadgeMasked &&
			this.premiumBadgeTimestampHidden === other.premiumBadgeTimestampHidden &&
			this.premiumBadgeSequenceHidden === other.premiumBadgeSequenceHidden &&
			this.premiumEnabledOverride === other.premiumEnabledOverride &&
			this.premiumPerksDisabled === other.premiumPerksDisabled &&
			datesEqual(this.passwordLastChangedAt, other.passwordLastChangedAt) &&
			datesEqual(this.lastVoiceActivitySharingChangeAt, other.lastVoiceActivitySharingChangeAt) &&
			arraysShallowEqual(this.requiredActions, other.requiredActions) &&
			this._nsfwAllowed === other._nsfwAllowed &&
			this.hasDismissedPremiumOnboarding === other.hasDismissedPremiumOnboarding &&
			this._hasEverPurchased === other._hasEverPurchased &&
			this._hasUnreadGiftInventory === other._hasUnreadGiftInventory &&
			this._unreadGiftInventoryCount === other._unreadGiftInventoryCount &&
			this._ageVerifiedAdult === other._ageVerifiedAdult &&
			datesEqual(this._termsAgreedAt, other._termsAgreedAt) &&
			datesEqual(this._privacyAgreedAt, other._privacyAgreedAt) &&
			arraysShallowEqual(this._traits, other._traits)
		);
	}

	toJSON(): WireUser {
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
			mention_flags: this.mentionFlags || undefined,
		};
		const privateFields: Record<string, unknown> = {};
		const setOptional = <K extends keyof UserPrivate>(key: K, value: UserPrivate[K] | undefined): void => {
			if (value !== undefined) privateFields[key as string] = value;
		};
		setOptional('is_staff', this._isStaff);
		setOptional('email', this._email);
		setOptional('email_bounced', this._emailBounced);
		setOptional('bio', this.bio);
		setOptional('banner', this.banner);
		setOptional('banner_color', this.bannerColor);
		setOptional('pronouns', this.pronouns);
		setOptional('accent_color', this.accentColor);
		if (this.isStaff()) {
			setOptional('timezone', this.timezone);
			setOptional('timezone_privacy_flags', this.timezonePrivacyFlags);
		}
		setOptional('mfa_enabled', this.mfaEnabled);
		setOptional('has_verified_phone', this.hasVerifiedPhone);
		setOptional('authenticator_types', this.authenticatorTypes);
		setOptional('verified', this._verified);
		setOptional('premium_type', this._premiumType);
		setOptional('premium_since', this._premiumSince === undefined ? undefined : dateToIsoOrNull(this._premiumSince));
		setOptional('premium_until', this._premiumUntil === undefined ? undefined : dateToIsoOrNull(this._premiumUntil));
		setOptional('premium_will_cancel', this._premiumWillCancel);
		setOptional('premium_billing_cycle', this._premiumBillingCycle);
		setOptional('premium_lifetime_sequence', this._premiumLifetimeSequence);
		setOptional(
			'premium_grace_ends_at',
			this._premiumGraceEndsAt === undefined ? undefined : dateToIsoOrNull(this._premiumGraceEndsAt),
		);
		setOptional('premium_discriminator', this._premiumDiscriminator);
		setOptional('premium_badge_hidden', this.premiumBadgeHidden);
		setOptional('premium_badge_masked', this.premiumBadgeMasked);
		setOptional('premium_badge_timestamp_hidden', this.premiumBadgeTimestampHidden);
		setOptional('premium_badge_sequence_hidden', this.premiumBadgeSequenceHidden);
		setOptional('premium_enabled_override', this.premiumEnabledOverride);
		setOptional('premium_perks_disabled', this.premiumPerksDisabled);
		setOptional(
			'password_last_changed_at',
			this.passwordLastChangedAt === undefined ? undefined : dateToIsoOrNull(this.passwordLastChangedAt),
		);
		setOptional(
			'last_voice_activity_sharing_change_at',
			this.lastVoiceActivitySharingChangeAt === undefined
				? undefined
				: dateToIsoOrNull(this.lastVoiceActivitySharingChangeAt),
		);
		if (this.requiredActions !== undefined) privateFields.required_actions = this.requiredActions;
		setOptional('nsfw_allowed', this._nsfwAllowed);
		setOptional('has_dismissed_premium_onboarding', this.hasDismissedPremiumOnboarding);
		setOptional('has_ever_purchased', this._hasEverPurchased);
		setOptional('has_unread_gift_inventory', this._hasUnreadGiftInventory);
		setOptional('unread_gift_inventory_count', this._unreadGiftInventoryCount);
		setOptional('age_verified_adult', this._ageVerifiedAdult);
		setOptional(
			'terms_agreed_at',
			this._termsAgreedAt === undefined ? undefined : dateToIsoOrNull(this._termsAgreedAt),
		);
		setOptional(
			'privacy_agreed_at',
			this._privacyAgreedAt === undefined ? undefined : dateToIsoOrNull(this._privacyAgreedAt),
		);
		privateFields.traits = [...this._traits];
		return {
			...baseFields,
			...(privateFields as Partial<UserPrivate>),
		};
	}
}

function arraysShallowEqual<T>(a: ReadonlyArray<T> | undefined, b: ReadonlyArray<T> | undefined): boolean {
	if (a === b) return true;
	if (a === undefined || b === undefined) return false;
	if (a.length !== b.length) return false;
	for (let i = 0; i < a.length; i++) if (a[i] !== b[i]) return false;
	return true;
}

function datesEqual(a: Date | null | undefined, b: Date | null | undefined): boolean {
	if (a === b) return true;
	if (a == null || b == null) return a == null && b == null;
	const ta = a.getTime();
	const tb = b.getTime();
	if (Number.isNaN(ta) || Number.isNaN(tb)) return Number.isNaN(ta) && Number.isNaN(tb);
	return ta === tb;
}
