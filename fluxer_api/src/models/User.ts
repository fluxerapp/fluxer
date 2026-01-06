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

import {types} from 'cassandra-driver';
import type {UserRow} from '~/database/CassandraTypes';
import type {UserID} from '../BrandedTypes';
import {checkIsPremium} from '../user/UserHelpers';

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
	readonly phone: string | null;
	readonly passwordHash: string | null;
	readonly passwordLastChangedAt: Date | null;
	readonly totpSecret: string | null;
	readonly authenticatorTypes: Set<number>;
	readonly avatarHash: string | null;
	readonly avatarColor: number | null;
	readonly bannerHash: string | null;
	readonly bannerColor: number | null;
	readonly bio: string | null;
	readonly pronouns: string | null;
	readonly accentColor: number | null;
	readonly dateOfBirth: string | null;
	readonly locale: string | null;
	readonly flags: bigint;
	readonly premiumType: number | null;
	readonly premiumSince: Date | null;
	readonly premiumUntil: Date | null;
	readonly premiumWillCancel: boolean;
	readonly premiumBillingCycle: string | null;
	readonly premiumLifetimeSequence: number | null;
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
	readonly deletionReasonCode: number | null;
	readonly deletionPublicReason: string | null;
	readonly deletionAuditLogReason: string | null;
	readonly acls: Set<string>;
	readonly firstRefundAt: Date | null;
	readonly betaCodeAllowance: number;
	readonly betaCodeLastResetAt: Date | null;
	readonly giftInventoryServerSeq: number | null;
	readonly giftInventoryClientSeq: number | null;
	readonly premiumOnboardingDismissedAt: Date | null;
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
		this.phone = row.phone ?? null;
		this.passwordHash = row.password_hash ?? null;
		this.passwordLastChangedAt = row.password_last_changed_at ?? null;
		this.totpSecret = row.totp_secret ?? null;
		this.authenticatorTypes = row.authenticator_types ?? new Set();
		this.avatarHash = row.avatar_hash ?? null;
		this.avatarColor = row.avatar_color ?? null;
		this.bannerHash = row.banner_hash ?? null;
		this.bannerColor = row.banner_color ?? null;
		this.bio = row.bio ?? null;
		this.pronouns = row.pronouns ?? null;
		this.accentColor = row.accent_color ?? null;
		this.dateOfBirth = row.date_of_birth ? row.date_of_birth.toString() : null;
		this.locale = row.locale ?? null;
		this.flags = row.flags ?? 0n;
		this.premiumType = row.premium_type ?? null;
		this.premiumSince = row.premium_since ?? null;
		this.premiumUntil = row.premium_until ?? null;
		this.premiumWillCancel = row.premium_will_cancel ?? false;
		this.premiumBillingCycle = row.premium_billing_cycle ?? null;
		this.premiumLifetimeSequence = row.premium_lifetime_sequence ?? null;
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
		this.deletionReasonCode = row.deletion_reason_code ?? null;
		this.deletionPublicReason = row.deletion_public_reason ?? null;
		this.deletionAuditLogReason = row.deletion_audit_log_reason ?? null;
		this.acls = row.acls ?? new Set();
		this.firstRefundAt = row.first_refund_at ?? null;
		this.betaCodeAllowance = row.beta_code_allowance ?? 0;
		this.betaCodeLastResetAt = row.beta_code_last_reset_at ?? null;
		this.giftInventoryServerSeq = row.gift_inventory_server_seq ?? null;
		this.giftInventoryClientSeq = row.gift_inventory_client_seq ?? null;
		this.premiumOnboardingDismissedAt = row.premium_onboarding_dismissed_at ?? null;
		this.version = row.version;
	}

	isPremium(): boolean {
		return checkIsPremium(this);
	}

	isUnclaimedAccount(): boolean {
		return this.passwordHash === null && !this.isBot;
	}

	canUseGlobalExpressions(): boolean {
		return this.isPremium() || this.isBot;
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
			phone: this.phone,
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
			date_of_birth: this.dateOfBirth ? types.LocalDate.fromString(this.dateOfBirth) : null,
			locale: this.locale,
			flags: this.flags,
			premium_type: this.premiumType,
			premium_since: this.premiumSince,
			premium_until: this.premiumUntil,
			premium_will_cancel: this.premiumWillCancel,
			premium_billing_cycle: this.premiumBillingCycle,
			premium_lifetime_sequence: this.premiumLifetimeSequence,
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
			deletion_reason_code: this.deletionReasonCode,
			deletion_public_reason: this.deletionPublicReason,
			deletion_audit_log_reason: this.deletionAuditLogReason,
			acls: this.acls.size > 0 ? this.acls : null,
			first_refund_at: this.firstRefundAt,
			beta_code_allowance: this.betaCodeAllowance,
			beta_code_last_reset_at: this.betaCodeLastResetAt,
			gift_inventory_server_seq: this.giftInventoryServerSeq,
			gift_inventory_client_seq: this.giftInventoryClientSeq,
			premium_onboarding_dismissed_at: this.premiumOnboardingDismissedAt,
			version: this.version,
		};
	}
}
