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
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {User} from '~/Models';
import {createStringType, Int64Type, z} from '~/Schema';
import * as IpUtils from '~/utils/IpUtils';

const REVERSE_DNS_CACHE_TTL_SECONDS = 86400;

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

export const mapUserToAdminResponse = async (user: User, cacheService?: ICacheService): Promise<UserAdminResponse> => {
	const lastActiveIpReverse = user.lastActiveIp ? await reverseDnsLookup(user.lastActiveIp, cacheService) : null;
	let lastActiveLocation: string | null = null;
	if (user.lastActiveIp) {
		try {
			const geoip = await IpUtils.getCountryCodeDetailed(user.lastActiveIp);
			const formattedLocation = IpUtils.formatGeoipLocation(geoip);
			lastActiveLocation = formattedLocation === IpUtils.UNKNOWN_LOCATION ? null : formattedLocation;
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
		has_totp: user.totpSecret !== null,
		authenticator_types: user.authenticatorTypes ? Array.from(user.authenticatorTypes) : [],
		last_active_at: user.lastActiveAt?.toISOString() ?? null,
		last_active_ip: user.lastActiveIp,
		last_active_ip_reverse: lastActiveIpReverse,
		last_active_location: lastActiveLocation,
	};
};

export const UserAdminResponse = z.object({
	id: z.string(),
	username: z.string(),
	discriminator: z.number(),
	global_name: z.string().nullable(),
	bot: z.boolean(),
	system: z.boolean(),
	flags: z.string(),
	avatar: z.string().nullable(),
	banner: z.string().nullable(),
	bio: z.string().nullable(),
	pronouns: z.string().nullable(),
	accent_color: z.number().nullable(),
	email: z.string().nullable(),
	email_verified: z.boolean(),
	email_bounced: z.boolean(),
	phone: z.string().nullable(),
	date_of_birth: z.string().nullable(),
	locale: z.string().nullable(),
	premium_type: z.number().nullable(),
	premium_since: z.string().nullable(),
	premium_until: z.string().nullable(),
	suspicious_activity_flags: z.number(),
	temp_banned_until: z.string().nullable(),
	pending_deletion_at: z.string().nullable(),
	pending_bulk_message_deletion_at: z.string().nullable(),
	deletion_reason_code: z.number().nullable(),
	deletion_public_reason: z.string().nullable(),
	acls: z.array(z.string()),
	has_totp: z.boolean(),
	authenticator_types: z.array(z.number()),
	last_active_at: z.string().nullable(),
	last_active_ip: z.string().nullable(),
	last_active_ip_reverse: z.string().nullable(),
	last_active_location: z.string().nullable(),
});

export type UserAdminResponse = z.infer<typeof UserAdminResponse>;

export const LookupUserRequest = z.object({
	query: createStringType(1, 1024),
});

export type LookupUserRequest = z.infer<typeof LookupUserRequest>;

export const SearchUsersRequest = z.object({
	query: createStringType(1, 1024).optional(),
	limit: z.number().default(50),
	offset: z.number().default(0),
});

export type SearchUsersRequest = z.infer<typeof SearchUsersRequest>;

export const ListUserSessionsRequest = z.object({
	user_id: Int64Type,
});

export type ListUserSessionsRequest = z.infer<typeof ListUserSessionsRequest>;

export const UserContactChangeLogEntry = z.object({
	event_id: z.string(),
	field: z.string(),
	old_value: z.string().nullable(),
	new_value: z.string().nullable(),
	reason: z.enum(['user_requested', 'admin_action']),
	actor_user_id: z.string().nullable(),
	event_at: z.string(),
});

export type UserContactChangeLogEntry = z.infer<typeof UserContactChangeLogEntry>;

export const ListUserChangeLogResponse = z.object({
	entries: z.array(UserContactChangeLogEntry),
	next_page_token: z.string().nullable().optional(),
});

export type ListUserChangeLogResponse = z.infer<typeof ListUserChangeLogResponse>;
