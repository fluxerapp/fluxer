// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {isIpBanExempt} from '@app/api/ban/IpBanExemptions';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {BannedFromGuildError} from '@fluxer/errors/src/domains/guild/BannedFromGuildError';
import {IpBannedFromGuildError} from '@fluxer/errors/src/domains/guild/IpBannedFromGuildError';
import {getSameIpDecisionKey, normalizeIpString} from '@fluxer/ip_utils/src/IpAddress';

const IP_BAN_KEYS_MAX_AGE_MS = 600_000;
const IP_BAN_KEYS_MAX_GUILDS = 1000;

interface GuildIpBanKeys {
	generation: string;
	expiresAt: number;
	keys: ReadonlySet<string>;
}

const ipBanKeysByGuild = new Map<GuildID, GuildIpBanKeys>();

function banGenerationKey(guildId: GuildID): string {
	return `guild:ban_gen:${guildId}`;
}

export async function readGuildBanGeneration(guildId: GuildID): Promise<string> {
	return (await getKVClient().get(banGenerationKey(guildId))) ?? '0';
}

export async function bumpGuildBanGeneration(guildId: GuildID): Promise<void> {
	await getKVClient().incr(banGenerationKey(guildId));
}

function ipBanKey(ip: string): string {
	return getSameIpDecisionKey(ip) ?? `raw:${normalizeIpString(ip)}`;
}

async function getGuildIpBanKeys(
	guildRepository: GuildRepository,
	guildId: GuildID,
	generation: string,
): Promise<ReadonlySet<string>> {
	const now = Date.now();
	const cached = ipBanKeysByGuild.get(guildId);
	if (cached && cached.generation === generation && cached.expiresAt > now) return cached.keys;
	const keys = new Set<string>();
	let expiresAt = now + IP_BAN_KEYS_MAX_AGE_MS;
	for (const ban of await guildRepository.listBans(guildId)) {
		if (!ban.ipAddress) continue;
		keys.add(ipBanKey(ban.ipAddress));
		if (ban.expiresAt) expiresAt = Math.min(expiresAt, ban.expiresAt.getTime());
	}
	ipBanKeysByGuild.delete(guildId);
	if (ipBanKeysByGuild.size >= IP_BAN_KEYS_MAX_GUILDS) {
		const oldest = ipBanKeysByGuild.keys().next();
		if (!oldest.done) ipBanKeysByGuild.delete(oldest.value);
	}
	ipBanKeysByGuild.set(guildId, {generation, expiresAt, keys});
	return keys;
}

export async function assertNoGuildBanMatch(
	guildRepository: GuildRepository,
	params: {
		guildId: GuildID;
		generation: string;
		userId: UserID;
		email: string | null | undefined;
		ips: ReadonlyArray<string | null | undefined>;
	},
): Promise<void> {
	const {guildId, generation, userId} = params;
	const [accountBan, ipBanKeys] = await Promise.all([
		guildRepository.getBan(guildId, userId),
		getGuildIpBanKeys(guildRepository, guildId, generation),
	]);
	if (accountBan) throw new BannedFromGuildError();
	for (const candidate of params.ips) {
		if (!candidate) continue;
		if (ipBanKeys.has(ipBanKey(candidate)) && !isIpBanExempt(candidate)) {
			throw new IpBannedFromGuildError();
		}
	}
	const email = params.email?.toLowerCase();
	if (email && (await guildRepository.getBanByEmail(guildId, email))) throw new BannedFromGuildError();
}

export async function assertNotBannedFromGuild(
	guildRepository: GuildRepository,
	userRepository: UserRepository,
	params: {userId: UserID; guildId: GuildID; ip?: string | null},
): Promise<void> {
	const {userId, guildId, ip} = params;
	const [generation, user] = await Promise.all([readGuildBanGeneration(guildId), userRepository.findUnique(userId)]);
	await assertNoGuildBanMatch(guildRepository, {
		guildId,
		generation,
		userId,
		email: user?.email,
		ips: [ip, user?.moderationIp],
	});
}
