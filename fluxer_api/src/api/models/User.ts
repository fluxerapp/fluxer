// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {getGlobalLimitConfigSnapshot} from '@app/api/limits/LimitConfigService';
import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import {checkIsPremium, getEffectivePremiumUntil} from '@app/api/user/UserHelpers';
import {
	extractPremiumFlagsFromLegacyUserFlags,
	type MentionReplyPreference,
	ProfileFieldPrivacyFlags,
	type UserAuthenticatorType,
	type UserPremiumType,
} from '@fluxer/constants/src/UserConstants';
import {types} from 'cassandra-driver';

export class User {
	readonly id: UserID;
	readonly username: string;
	readonly discriminator: number;
	readonly globalName: string | null;
	readonly isBot: boolean;
	readonly isSystem: boolean;
	readonly email: string | null;
	readonly emailVerified: boolean;
	readonly emailBounced: boolean;
	readonly hasVerifiedPhone: boolean;
	readonly passwordHash: string | null;
	readonly passwordLastChangedAt: Date | null;
	readonly totpSecret: string | null;
	readonly authenticatorTypes: Set<UserAuthenticatorType>;
	readonly avatarHash: string | null;
	readonly avatarColor: number | null;
	readonly bannerHash: string | null;
	readonly bannerColor: number | null;
	readonly bio: string | null;
	readonly pronouns: string | null;
	readonly accentColor: number | null;
	readonly timezone: string | null;
	readonly timezonePrivacyFlags: number;
	readonly dateOfBirth: string | null;
	readonly locale: string | null;
	readonly flags: bigint;
	readonly premiumFlags: number;
	readonly premiumType: UserPremiumType | null;
	readonly premiumSince: Date | null;
	readonly premiumUntil: Date | null;
	readonly premiumGiftExtensionEndsAt: Date | null;
	readonly premiumWillCancel: boolean;
	readonly premiumBillingCycle: string | null;
	readonly premiumLifetimeSequence: number | null;
	readonly premiumGraceEndsAt: Date | null;
	readonly stripeSubscriptionId: string | null;
	readonly stripeCustomerId: string | null;
	readonly hasEverPurchased: boolean;
	readonly suspiciousActivityFlags: number;
	readonly termsAgreedAt: Date | null;
	readonly privacyAgreedAt: Date | null;
	readonly lastActiveAt: Date | null;
	readonly lastActiveIp: string | null;
	readonly tempBannedUntil: Date | null;
	readonly pendingBulkMessageDeletionAt: Date | null;
	readonly pendingBulkMessageDeletionChannelCount: number | null;
	readonly pendingBulkMessageDeletionMessageCount: number | null;
	readonly pendingDeletionAt: Date | null;
	readonly deletionStartedAt: Date | null;
	readonly deletionReasonCode: number | null;
	readonly deletionPublicReason: string | null;
	readonly deletionAuditLogReason: string | null;
	readonly acls: Set<string>;
	private readonly _traits: Set<string>;
	readonly firstRefundAt: Date | null;
	readonly giftInventoryServerSeq: number | null;
	readonly giftInventoryClientSeq: number | null;
	readonly premiumOnboardingDismissedAt: Date | null;
	readonly mentionFlags: MentionReplyPreference;
	readonly lastVoiceActivitySharingChangeAt: Date | null;
	readonly version: number;

	constructor(row: UserRow) {
		this.id = row.user_id;
		this.username = row.username;
		this.discriminator = row.discriminator;
		this.globalName = row.global_name ?? null;
		this.isBot = row.bot ?? false;
		this.isSystem = row.system ?? false;
		this.email = row.email ?? null;
		this.emailVerified = row.email_verified ?? false;
		this.emailBounced = row.email_bounced ?? false;
		this.hasVerifiedPhone = row.has_verified_phone ?? false;
		this.passwordHash = row.password_hash ?? null;
		this.passwordLastChangedAt = row.password_last_changed_at ?? null;
		this.totpSecret = row.totp_secret ?? null;
		this.authenticatorTypes = (row.authenticator_types ?? new Set()) as Set<UserAuthenticatorType>;
		this.avatarHash = row.avatar_hash ?? null;
		this.avatarColor = row.avatar_color ?? null;
		this.bannerHash = row.banner_hash ?? null;
		this.bannerColor = row.banner_color ?? null;
		this.bio = row.bio ?? null;
		this.pronouns = row.pronouns ?? null;
		this.accentColor = row.accent_color ?? null;
		this.timezone = row.timezone ?? null;
		this.timezonePrivacyFlags = row.timezone_privacy_flags ?? ProfileFieldPrivacyFlags.EVERYONE;
		this.dateOfBirth = row.date_of_birth ? row.date_of_birth.toString() : null;
		this.locale = row.locale ?? null;
		const rawFlags = row.flags ?? 0n;
		this.flags = rawFlags;
		this.premiumFlags = (row.premium_flags ?? 0) | extractPremiumFlagsFromLegacyUserFlags(rawFlags);
		this.premiumType = (row.premium_type ?? null) as UserPremiumType | null;
		this.premiumSince = row.premium_since ?? null;
		this.premiumUntil = row.premium_until ?? null;
		this.premiumGiftExtensionEndsAt = row.premium_gift_extension_ends_at ?? null;
		this.premiumWillCancel = row.premium_will_cancel ?? false;
		this.premiumBillingCycle = row.premium_billing_cycle ?? null;
		this.premiumLifetimeSequence = row.premium_lifetime_sequence ?? null;
		this.premiumGraceEndsAt = row.premium_grace_ends_at ?? null;
		this.stripeSubscriptionId = row.stripe_subscription_id ?? null;
		this.stripeCustomerId = row.stripe_customer_id ?? null;
		this.hasEverPurchased = row.has_ever_purchased ?? false;
		this.suspiciousActivityFlags = row.suspicious_activity_flags ?? 0;
		this.termsAgreedAt = row.terms_agreed_at ?? null;
		this.privacyAgreedAt = row.privacy_agreed_at ?? null;
		this.lastActiveAt = row.last_active_at ?? null;
		this.lastActiveIp = row.last_active_ip ?? null;
		this.tempBannedUntil = row.temp_banned_until ?? null;
		this.pendingBulkMessageDeletionAt = row.pending_bulk_message_deletion_at ?? null;
		this.pendingBulkMessageDeletionChannelCount = row.pending_bulk_message_deletion_channel_count ?? null;
		this.pendingBulkMessageDeletionMessageCount = row.pending_bulk_message_deletion_message_count ?? null;
		this.pendingDeletionAt = row.pending_deletion_at ?? null;
		this.deletionStartedAt = row.deletion_started_at ?? null;
		this.deletionReasonCode = row.deletion_reason_code ?? null;
		this.deletionPublicReason = row.deletion_public_reason ?? null;
		this.deletionAuditLogReason = row.deletion_audit_log_reason ?? null;
		this.acls = row.acls ?? new Set();
		this._traits = row.traits ?? new Set();
		this.firstRefundAt = row.first_refund_at ?? null;
		this.giftInventoryServerSeq = row.gift_inventory_server_seq ?? null;
		this.giftInventoryClientSeq = row.gift_inventory_client_seq ?? null;
		this.premiumOnboardingDismissedAt = row.premium_onboarding_dismissed_at ?? null;
		this.mentionFlags = row.mention_flags ?? 0;
		this.lastVoiceActivitySharingChangeAt = row.last_voice_activity_sharing_change_at ?? null;
		this.version = row.version;
	}

	get traits(): Set<string> {
		return new Set(this._traits);
	}

	isPremium(): boolean {
		return checkIsPremium(this);
	}

	get effectivePremiumUntil(): Date | null {
		return getEffectivePremiumUntil(this);
	}

	isUnclaimedAccount(): boolean {
		return this.passwordHash === null && !this.isBot && !this._traits.has('sso');
	}

	canUseGlobalExpressions(): boolean {
		if (this.isBot) {
			return true;
		}
		const ctx = createLimitMatchContext({user: this});
		const snapshot = getGlobalLimitConfigSnapshot();
		const hasGlobalExpressions = resolveLimitSafe(snapshot, ctx, 'feature_global_expressions', 0);
		return hasGlobalExpressions > 0;
	}

	toRow(): UserRow {
		return {
			user_id: this.id,
			username: this.username,
			discriminator: this.discriminator,
			global_name: this.globalName,
			bot: this.isBot,
			system: this.isSystem,
			email: this.email,
			email_verified: this.emailVerified,
			email_bounced: this.emailBounced,
			has_verified_phone: this.hasVerifiedPhone,
			password_hash: this.passwordHash,
			password_last_changed_at: this.passwordLastChangedAt,
			totp_secret: this.totpSecret,
			authenticator_types: this.authenticatorTypes.size > 0 ? this.authenticatorTypes : null,
			avatar_hash: this.avatarHash,
			avatar_color: this.avatarColor,
			banner_hash: this.bannerHash,
			banner_color: this.bannerColor,
			bio: this.bio,
			pronouns: this.pronouns,
			accent_color: this.accentColor,
			timezone: this.timezone,
			timezone_privacy_flags: this.timezonePrivacyFlags,
			date_of_birth: this.dateOfBirth ? types.LocalDate.fromString(this.dateOfBirth) : null,
			locale: this.locale,
			flags: this.flags,
			premium_flags: this.premiumFlags,
			premium_type: this.premiumType,
			premium_since: this.premiumSince,
			premium_until: this.premiumUntil,
			premium_gift_extension_ends_at: this.premiumGiftExtensionEndsAt,
			premium_will_cancel: this.premiumWillCancel,
			premium_billing_cycle: this.premiumBillingCycle,
			premium_lifetime_sequence: this.premiumLifetimeSequence,
			premium_grace_ends_at: this.premiumGraceEndsAt,
			stripe_subscription_id: this.stripeSubscriptionId,
			stripe_customer_id: this.stripeCustomerId,
			has_ever_purchased: this.hasEverPurchased,
			suspicious_activity_flags: this.suspiciousActivityFlags,
			terms_agreed_at: this.termsAgreedAt,
			privacy_agreed_at: this.privacyAgreedAt,
			last_active_at: this.lastActiveAt,
			last_active_ip: this.lastActiveIp,
			temp_banned_until: this.tempBannedUntil,
			pending_bulk_message_deletion_at: this.pendingBulkMessageDeletionAt,
			pending_bulk_message_deletion_channel_count: this.pendingBulkMessageDeletionChannelCount,
			pending_bulk_message_deletion_message_count: this.pendingBulkMessageDeletionMessageCount,
			pending_deletion_at: this.pendingDeletionAt,
			deletion_started_at: this.deletionStartedAt,
			deletion_reason_code: this.deletionReasonCode,
			deletion_public_reason: this.deletionPublicReason,
			deletion_audit_log_reason: this.deletionAuditLogReason,
			acls: this.acls.size > 0 ? this.acls : null,
			traits: this._traits.size > 0 ? this._traits : null,
			first_refund_at: this.firstRefundAt,
			gift_inventory_server_seq: this.giftInventoryServerSeq,
			gift_inventory_client_seq: this.giftInventoryClientSeq,
			premium_onboarding_dismissed_at: this.premiumOnboardingDismissedAt,
			mention_flags: this.mentionFlags,
			last_voice_activity_sharing_change_at: this.lastVoiceActivitySharingChangeAt,
			version: this.version,
		};
	}
}
