// SPDX-License-Identifier: AGPL-3.0-or-later

import dns from 'node:dns';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {DEFERRED_PHONE_ON_COMMUNITY_JOIN} from '@fluxer/constants/src/UserConstants';
import type {UserAdminResponse} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {formatGeoipLocation} from '@pkgs/geoip/src/GeoipLookup';
import {seconds} from 'itty-time';
import type {User} from '../../models/User';
import {lookupGeoip} from '../../utils/IpUtils';

const REVERSE_DNS_CACHE_TTL_SECONDS = seconds('1 day');

async function reverseDnsLookup(ip: string, cacheService?: ICacheService): Promise<string | null> {
	const cacheKey = `reverse-dns:${ip}`;
	if (cacheService) {
		const cached = await cacheService.get<string | null>(cacheKey);
		if (cached !== null) {
			return cached === '' ? null : cached;
		}
	}
	let result: string | null = null;
	try {
		const hostnames = await dns.promises.reverse(ip);
		result = hostnames[0] ?? null;
	} catch {
		result = null;
	}
	if (cacheService) {
		await cacheService.set(cacheKey, result ?? '', REVERSE_DNS_CACHE_TTL_SECONDS);
	}
	return result;
}

function hasAcl(acls: ReadonlySet<string>, acl: string): boolean {
	return acls.has(acl) || acls.has(AdminACLs.WILDCARD);
}

export async function mapUserToAdminResponse(
	user: User,
	cacheService?: ICacheService,
	acls?: ReadonlySet<string>,
): Promise<UserAdminResponse> {
	const canViewEmail = !acls || hasAcl(acls, AdminACLs.USER_VIEW_EMAIL);
	const canViewDob = !acls || hasAcl(acls, AdminACLs.USER_VIEW_DOB);
	const canViewIp = !acls || hasAcl(acls, AdminACLs.USER_VIEW_IP);
	const lastActiveIpReverse =
		canViewIp && user.lastActiveIp ? await reverseDnsLookup(user.lastActiveIp, cacheService) : null;
	let lastActiveLocation: string | null = null;
	if (canViewIp && user.lastActiveIp) {
		try {
			const geoip = await lookupGeoip(user.lastActiveIp);
			const formattedLocation = formatGeoipLocation(geoip);
			lastActiveLocation = formattedLocation;
		} catch {
			lastActiveLocation = null;
		}
	}
	return {
		id: user.id.toString(),
		username: user.username,
		discriminator: user.discriminator,
		global_name: user.globalName,
		bot: user.isBot,
		system: user.isSystem,
		flags: user.flags.toString(),
		premium_flags: user.premiumFlags,
		avatar: user.avatarHash,
		banner: user.bannerHash,
		bio: user.bio,
		pronouns: user.pronouns,
		accent_color: user.accentColor,
		email: canViewEmail ? (user.email ?? null) : null,
		email_verified: canViewEmail ? user.emailVerified : false,
		email_bounced: canViewEmail ? user.emailBounced : false,
		has_verified_phone: user.hasVerifiedPhone,
		date_of_birth: canViewDob ? user.dateOfBirth : null,
		locale: user.locale,
		premium_type: user.premiumType,
		premium_since: user.premiumSince?.toISOString() ?? null,
		premium_until: user.premiumUntil?.toISOString() ?? null,
		premium_grace_ends_at: user.premiumGraceEndsAt?.toISOString() ?? null,
		premium_lifetime_sequence: user.premiumLifetimeSequence ?? null,
		suspicious_activity_flags: user.suspiciousActivityFlags,
		phone_verification_deferred: ((user.suspiciousActivityFlags ?? 0) & DEFERRED_PHONE_ON_COMMUNITY_JOIN) !== 0,
		temp_banned_until:
			user.tempBannedUntil && user.tempBannedUntil.getTime() > Date.now() ? user.tempBannedUntil.toISOString() : null,
		pending_deletion_at: user.pendingDeletionAt?.toISOString() ?? null,
		pending_bulk_message_deletion_at: user.pendingBulkMessageDeletionAt?.toISOString() ?? null,
		deletion_reason_code: user.deletionReasonCode,
		deletion_public_reason: user.deletionPublicReason,
		acls: user.acls ? Array.from(user.acls) : [],
		traits: Array.from(user.traits).sort(),
		has_totp: user.totpSecret !== null,
		authenticator_types: user.authenticatorTypes ? Array.from(user.authenticatorTypes) : [],
		last_active_at: user.lastActiveAt?.toISOString() ?? null,
		last_active_ip: canViewIp ? (user.lastActiveIp ?? null) : null,
		last_active_ip_reverse: canViewIp ? lastActiveIpReverse : null,
		last_active_location: canViewIp ? lastActiveLocation : null,
	};
}
