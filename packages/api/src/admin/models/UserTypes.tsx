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

import dns from 'node:dns';
import type {User} from '@fluxer/api/src/models/User';
import {lookupGeoip} from '@fluxer/api/src/utils/IpUtils';
import type {ICacheService} from '@fluxer/cache/src/ICacheService';
import {formatGeoipLocation} from '@fluxer/geoip/src/GeoipLookup';
import type {UserAdminResponse} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import {seconds} from 'itty-time';

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

export async function mapUserToAdminResponse(user: User, cacheService?: ICacheService): Promise<UserAdminResponse> {
	const lastActiveIpReverse = user.lastActiveIp ? await reverseDnsLookup(user.lastActiveIp, cacheService) : null;
	let lastActiveLocation: string | null = null;
	if (user.lastActiveIp) {
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
		avatar: user.avatarHash,
		banner: user.bannerHash,
		bio: user.bio,
		pronouns: user.pronouns,
		accent_color: user.accentColor,
		email: user.email,
		email_verified: user.emailVerified,
		email_bounced: user.emailBounced,
		phone: user.phone,
		date_of_birth: user.dateOfBirth,
		locale: user.locale,
		premium_type: user.premiumType,
		premium_since: user.premiumSince?.toISOString() ?? null,
		premium_until: user.premiumUntil?.toISOString() ?? null,
		suspicious_activity_flags: user.suspiciousActivityFlags,
		temp_banned_until: user.tempBannedUntil?.toISOString() ?? null,
		pending_deletion_at: user.pendingDeletionAt?.toISOString() ?? null,
		pending_bulk_message_deletion_at: user.pendingBulkMessageDeletionAt?.toISOString() ?? null,
		deletion_reason_code: user.deletionReasonCode,
		deletion_public_reason: user.deletionPublicReason,
		acls: user.acls ? Array.from(user.acls) : [],
		traits: Array.from(user.traits).sort(),
		has_totp: user.totpSecret !== null,
		authenticator_types: user.authenticatorTypes ? Array.from(user.authenticatorTypes) : [],
		last_active_at: user.lastActiveAt?.toISOString() ?? null,
		last_active_ip: user.lastActiveIp,
		last_active_ip_reverse: lastActiveIpReverse,
		last_active_location: lastActiveLocation,
	};
}
