// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {hasAdminACL} from '@app/api/middleware/AdminMiddleware';
import type {User} from '@app/api/models/User';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {getIpAddressReverse, lookupGeoip} from '@app/api/utils/IpUtils';
import {AdminACLs, filterKnownAdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {AdminResolvedUser, UserAdminResponse} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {formatGeoipLocation} from '@pkgs/geoip/src/GeoipLookup';

export async function mapUserToAdminResponse(
	user: User,
	cacheService?: ICacheService,
	acls?: ReadonlySet<string>,
): Promise<UserAdminResponse> {
	const canViewEmail = !acls || hasAdminACL(acls, AdminACLs.USER_VIEW_EMAIL);
	const canViewDob = !acls || hasAdminACL(acls, AdminACLs.USER_VIEW_DOB);
	const canViewIp = !acls || hasAdminACL(acls, AdminACLs.USER_VIEW_IP);
	const canViewAuditLog = !acls || hasAdminACL(acls, AdminACLs.AUDIT_LOG_VIEW);
	const lastActiveIpReverse =
		canViewIp && user.lastActiveIp ? await getIpAddressReverse(user.lastActiveIp, cacheService) : null;
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
		date_of_birth: canViewDob ? user.dateOfBirth : null,
		locale: user.locale,
		premium_type: user.premiumType,
		premium_since: user.premiumSince?.toISOString() ?? null,
		premium_until: user.premiumUntil?.toISOString() ?? null,
		premium_grace_ends_at: user.premiumGraceEndsAt?.toISOString() ?? null,
		premium_lifetime_sequence: user.premiumLifetimeSequence ?? null,
		temp_banned_until:
			user.tempBannedUntil && user.tempBannedUntil.getTime() > Date.now() ? user.tempBannedUntil.toISOString() : null,
		pending_deletion_at: user.pendingDeletionAt?.toISOString() ?? null,
		pending_bulk_message_deletion_at: user.pendingBulkMessageDeletionAt?.toISOString() ?? null,
		deletion_reason_code: user.deletionReasonCode,
		deletion_public_reason: user.deletionPublicReason,
		deletion_audit_log_reason: canViewAuditLog ? user.deletionAuditLogReason : null,
		deletion_scheduled_by: user.deletionScheduledBy?.toString() ?? null,
		deletion_scheduled_at: user.deletionScheduledAt?.toISOString() ?? null,
		acls: user.acls ? filterKnownAdminACLs(user.acls) : [],
		traits: Array.from(user.traits).sort(),
		has_totp: user.totpSecret !== null,
		authenticator_types: user.authenticatorTypes ? Array.from(user.authenticatorTypes) : [],
		last_active_at: user.lastActiveAt?.toISOString() ?? null,
		last_active_ip: canViewIp ? (user.lastActiveIp ?? null) : null,
		last_active_ip_reverse: canViewIp ? lastActiveIpReverse : null,
		last_active_location: canViewIp ? lastActiveLocation : null,
	};
}

export async function resolveAdminUsers(
	userRepository: Pick<UserRepository, 'findUnique'>,
	userIds: Array<UserID>,
): Promise<Map<UserID, AdminResolvedUser>> {
	const results = new Map<UserID, AdminResolvedUser>();
	const users = await Promise.all(userIds.map((id) => userRepository.findUnique(id)));
	for (let i = 0; i < userIds.length; i++) {
		const user = users[i];
		if (user)
			results.set(userIds[i], {
				id: user.id.toString(),
				username: user.username,
				discriminator: String(user.discriminator).padStart(4, '0'),
				global_name: user.globalName,
				avatar: user.avatarHash,
			});
	}
	return results;
}
