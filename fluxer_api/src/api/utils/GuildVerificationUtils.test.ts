// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {User} from '@app/api/models/User';
import {checkGuildVerificationWithResponse} from '@app/api/utils/GuildVerificationUtils';
import {GuildFeatures, GuildVerificationLevel} from '@fluxer/constants/src/GuildConstants';
import {ProfileFieldPrivacyFlags} from '@fluxer/constants/src/UserConstants';
import {GuildEmailVerificationRequiredError} from '@fluxer/errors/src/domains/auth/EmailVerificationRequiredError';
import {GuildPhoneVerificationRequiredError} from '@fluxer/errors/src/domains/auth/GuildPhoneVerificationRequiredError';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {describe, expect, it} from 'vitest';

const TEST_USER_ID = createUserID(175928847299117063n);
const TEST_OWNER_ID = createUserID(275928847299117063n);
const TEST_JOINED_AT = new Date('2025-01-01T00:00:00.000Z').toISOString();

function createUser(params?: {emailVerified?: boolean; hasVerifiedPhone?: boolean}): User {
	return new User({
		user_id: TEST_USER_ID,
		username: 'member',
		discriminator: 1,
		global_name: null,
		bot: false,
		system: false,
		email: 'member@example.com',
		email_verified: params?.emailVerified ?? false,
		has_verified_phone: params?.hasVerifiedPhone ?? false,
		email_bounced: false,
		password_hash: 'hashed',
		password_last_changed_at: null,
		totp_secret: null,
		authenticator_types: null,
		avatar_hash: null,
		avatar_color: null,
		banner_hash: null,
		banner_color: null,
		bio: null,
		pronouns: null,
		accent_color: null,
		timezone: null,
		timezone_privacy_flags: ProfileFieldPrivacyFlags.EVERYONE,
		date_of_birth: null,
		locale: null,
		flags: 0n,
		premium_type: null,
		premium_since: null,
		premium_until: null,
		premium_gift_extension_ends_at: null,
		premium_will_cancel: false,
		premium_billing_cycle: null,
		premium_lifetime_sequence: null,
		premium_grace_ends_at: null,
		stripe_subscription_id: null,
		stripe_customer_id: null,
		has_ever_purchased: false,
		suspicious_activity_flags: 0,
		terms_agreed_at: null,
		privacy_agreed_at: null,
		last_active_at: null,
		last_active_ip: null,
		temp_banned_until: null,
		pending_bulk_message_deletion_at: null,
		pending_bulk_message_deletion_channel_count: null,
		pending_bulk_message_deletion_message_count: null,
		pending_deletion_at: null,
		deletion_reason_code: null,
		deletion_public_reason: null,
		deletion_audit_log_reason: null,
		acls: null,
		traits: null,
		first_refund_at: null,
		gift_inventory_server_seq: null,
		gift_inventory_client_seq: null,
		premium_onboarding_dismissed_at: null,
		last_voice_activity_sharing_change_at: null,
		version: 1,
	} as const);
}

function createGuildResponse(features: Array<string>): GuildResponse {
	return {
		id: '1',
		name: 'Guild',
		icon: null,
		banner: null,
		banner_width: null,
		banner_height: null,
		splash: null,
		splash_width: null,
		splash_height: null,
		embed_splash: null,
		embed_splash_width: null,
		embed_splash_height: null,
		splash_card_alignment: 0,
		vanity_url_code: null,
		owner_id: TEST_OWNER_ID.toString(),
		system_channel_id: null,
		system_channel_flags: 0,
		rules_channel_id: null,
		afk_channel_id: null,
		afk_timeout: 0,
		features,
		verification_level: GuildVerificationLevel.NONE,
		mfa_level: 0,
		nsfw_level: 0,
		explicit_content_filter: 0,
		default_message_notifications: 0,
		disabled_operations: 0,
		message_history_cutoff: null,
	} as GuildResponse;
}

function createMemberResponse(joinedAt = TEST_JOINED_AT): GuildMemberResponse {
	return {
		user: {
			id: TEST_USER_ID.toString(),
			username: 'member',
			discriminator: '0001',
			global_name: null,
			avatar: null,
			avatar_color: null,
			flags: 0,
		},
		nick: null,
		avatar: null,
		banner: null,
		accent_color: null,
		roles: [],
		joined_at: joinedAt,
		mute: false,
		deaf: false,
		communication_disabled_until: null,
		profile_flags: undefined,
	} as GuildMemberResponse;
}

const member = createMemberResponse();

describe('GuildVerificationUtils', () => {
	it('treats discoverable guilds as requiring at least verified email', () => {
		expect(() =>
			checkGuildVerificationWithResponse({
				user: createUser(),
				guild: createGuildResponse([GuildFeatures.DISCOVERABLE]),
				member,
			}),
		).toThrow(GuildEmailVerificationRequiredError);
	});
	it('keeps NONE unrestricted for non-discoverable guilds', () => {
		expect(() =>
			checkGuildVerificationWithResponse({
				user: createUser(),
				guild: createGuildResponse([]),
				member,
			}),
		).not.toThrow();
	});
	it('allows very high verification with only a verified phone', () => {
		const guild = createGuildResponse([]);
		guild.verification_level = GuildVerificationLevel.VERY_HIGH;
		expect(() =>
			checkGuildVerificationWithResponse({
				user: createUser({emailVerified: false, hasVerifiedPhone: true}),
				guild,
				member: createMemberResponse(new Date().toISOString()),
			}),
		).not.toThrow();
	});
	it('rejects very high verification without a verified phone', () => {
		const guild = createGuildResponse([]);
		guild.verification_level = GuildVerificationLevel.VERY_HIGH;
		expect(() =>
			checkGuildVerificationWithResponse({
				user: createUser({emailVerified: true, hasVerifiedPhone: false}),
				guild,
				member,
			}),
		).toThrow(GuildPhoneVerificationRequiredError);
	});
});
