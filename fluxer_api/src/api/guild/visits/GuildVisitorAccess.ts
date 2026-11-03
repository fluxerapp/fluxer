// SPDX-License-Identifier: AGPL-3.0-or-later

import {AsyncLocalStorage} from 'node:async_hooks';
import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {guildIdToRoleId} from '@app/api/BrandedTypes';
import type {GuildVisitRow} from '@app/api/database/types/GuildVisitTypes';
import {userGuildVisitsActive} from '@app/api/experiment/GuildVisitsGate';
import {assertNotBannedFromGuildVisit, endGuildVisit} from '@app/api/guild/visits/GuildVisitLifecycle';
import {guildVisitorRateLimitedTotal} from '@app/api/guild/visits/GuildVisitMetrics';
import {readGuildVisitorViewGeneration} from '@app/api/guild/visits/GuildVisitorViewGeneration';
import {GuildVisitRepository} from '@app/api/guild/visits/GuildVisitRepository';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import {Logger} from '@app/api/Logger';
import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {
	getGuildDiscoveryRepository,
	getGuildRepository,
	getLimitConfigService,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {Channel} from '@app/api/models/Channel';
import type {Guild} from '@app/api/models/Guild';
import type {User} from '@app/api/models/User';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {type AccountTier, getAccountTier} from '@app/api/user/AccountTier';
import {canUserAccessNsfwContent} from '@app/api/utils/AgeUtils';
import {getRequestClientIp} from '@app/api/utils/RequestClientIp';
import {ChannelOverwriteTypes} from '@fluxer/constants/src/ChannelConstants';
import {DiscoveryApplicationStatus} from '@fluxer/constants/src/DiscoveryConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {
	computeGuildVisitorChannelPermissions,
	GUILD_VISIT_OPEN_REFRESH_SECONDS,
	GuildVisitEndReasons,
	type GuildVisitorChannelInput,
	GuildVisitSources,
} from '@fluxer/constants/src/GuildVisitConstants';
import {MS_PER_DAY, MS_PER_HOUR} from '@fluxer/date_utils/src/DateConstants';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import {BannedFromGuildError} from '@fluxer/errors/src/domains/guild/BannedFromGuildError';
import {IpBannedFromGuildError} from '@fluxer/errors/src/domains/guild/IpBannedFromGuildError';
import {getSameIpDecisionKey} from '@fluxer/ip_utils/src/IpAddress';
import {DEFAULT_STOCK_LIMITS} from '@fluxer/limits/src/LimitDefaults';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';
import type {Context} from 'hono';
import {createMiddleware} from 'hono/factory';
import {ms} from 'itty-time';

const VISIT_ROW_CACHE_MS = 5_000;
const VISITOR_VIEW_CACHE_MS = 60_000;
const MAX_CACHE_ENTRIES = 20_000;
const HISTORY_READ_KEY_TTL_SECONDS = 3 * 3600;

interface GuildVisitorView {
	guild: Guild;
	everyonePermissions: bigint;
	discoveryApproved: boolean;
}

export interface GuildVisitorAccess {
	visit: GuildVisitRow;
	view: GuildVisitorView;
	user: User;
	tier: AccountTier;
	adultContentAllowed: boolean;
	historyCutoff: Date | null;
}

const visitRepository = new GuildVisitRepository();
const visitRowCache = new Map<string, {row: GuildVisitRow | null; expiresAt: number}>();
const viewCache = new Map<string, {generation: string; view: GuildVisitorView; expiresAt: number}>();

function boundedSet<V>(map: Map<string, V>, key: string, value: V): void {
	map.delete(key);
	map.set(key, value);
	if (map.size > MAX_CACHE_ENTRIES) {
		const oldest = map.keys().next().value;
		if (oldest !== undefined) {
			map.delete(oldest);
		}
	}
}

function visitKey(userId: UserID, guildId: GuildID): string {
	return `${userId}:${guildId}`;
}

export function guildVisitRepository(): GuildVisitRepository {
	return visitRepository;
}

export function forgetGuildVisitRow(userId: UserID, guildId: GuildID): void {
	visitRowCache.delete(visitKey(userId, guildId));
}

export function resetGuildVisitorCachesForTesting(): void {
	visitRowCache.clear();
	viewCache.clear();
}

async function findGuildVisitRowCached(userId: UserID, guildId: GuildID): Promise<GuildVisitRow | null> {
	const key = visitKey(userId, guildId);
	const now = Date.now();
	const cached = visitRowCache.get(key);
	if (cached && cached.expiresAt > now) {
		return cached.row;
	}
	const row = await visitRepository.get(userId, guildId);
	boundedSet(visitRowCache, key, {row, expiresAt: now + VISIT_ROW_CACHE_MS});
	return row;
}

export async function loadGuildVisitorView(guildId: GuildID): Promise<GuildVisitorView | null> {
	const generation = await readGuildVisitorViewGeneration(guildId);
	const key = guildId.toString();
	const now = Date.now();
	const cached = viewCache.get(key);
	if (cached && cached.generation === generation && cached.expiresAt > now) {
		return cached.view;
	}
	const guildRepository = getGuildRepository();
	const [guild, everyoneRole, discovery] = await Promise.all([
		guildRepository.findUnique(guildId),
		guildRepository.getRole(guildIdToRoleId(guildId), guildId),
		getGuildDiscoveryRepository().findByGuildId(guildId),
	]);
	if (!guild) {
		viewCache.delete(key);
		return null;
	}
	const view: GuildVisitorView = {
		guild,
		everyonePermissions: everyoneRole?.permissions ?? 0n,
		discoveryApproved: discovery?.status === DiscoveryApplicationStatus.APPROVED,
	};
	boundedSet(viewCache, key, {generation, view, expiresAt: now + VISITOR_VIEW_CACHE_MS});
	return view;
}

export function isGuildUnavailable(guild: Pick<Guild, 'features'>): boolean {
	return (
		guild.features.has(GuildFeatures.UNAVAILABLE_FOR_EVERYONE) ||
		guild.features.has(GuildFeatures.UNAVAILABLE_FOR_EVERYONE_BUT_STAFF)
	);
}

export function guildVisitAuthorizedByView(visit: Pick<GuildVisitRow, 'source'>, view: GuildVisitorView): boolean {
	if (isGuildUnavailable(view.guild)) {
		return false;
	}
	if (visit.source === GuildVisitSources.OPERATOR) {
		return true;
	}
	if (!view.guild.features.has(GuildFeatures.VISITS_ENABLED)) {
		return false;
	}
	if (visit.source === GuildVisitSources.DISCOVERY && !view.discoveryApproved) {
		return false;
	}
	return true;
}

function guildVisitHistoryDays(user: User): number {
	return resolveLimitSafe(
		getLimitConfigService().getConfigSnapshot(),
		createLimitMatchContext({user}),
		'guild_visit_history_days_unverified',
		DEFAULT_STOCK_LIMITS.guild_visit_history_days_unverified,
	);
}

function guildVisitHistoryCutoff(user: User, tier: AccountTier, now: number = Date.now()): Date | null {
	if (tier === 'verified') {
		return null;
	}
	return new Date(now - guildVisitHistoryDays(user) * MS_PER_DAY);
}

export async function resolveGuildVisitorAccess(params: {
	userId: UserID;
	guildId: GuildID;
	user?: User | null;
}): Promise<GuildVisitorAccess | null> {
	const {userId, guildId} = params;
	if (!userGuildVisitsActive(userId)) {
		return null;
	}
	const visit = await findGuildVisitRowCached(userId, guildId);
	if (!visit) {
		return null;
	}
	const now = Date.now();
	if (visit.invite_expires_at !== null && visit.invite_expires_at.getTime() <= now) {
		return null;
	}
	const [member, view, user] = await Promise.all([
		getGuildRepository().getMember(guildId, userId),
		loadGuildVisitorView(guildId),
		params.user ? Promise.resolve(params.user) : getUserRepository().findUnique(userId),
	]);
	if (member || !view || !user) {
		return null;
	}
	if (!guildVisitAuthorizedByView(visit, view)) {
		return null;
	}
	if (!(await visitorStillAllowedIn(guildId, user))) {
		return null;
	}
	const adultContentAllowed = canUserAccessNsfwContent(user);
	if (view.guild.nsfw && !adultContentAllowed) {
		return null;
	}
	const tier = getAccountTier(user);
	if (now - visit.opened_at.getTime() >= GUILD_VISIT_OPEN_REFRESH_SECONDS * 1000) {
		forgetGuildVisitRow(userId, guildId);
		void refreshGuildVisitOpenedAt(visit, new Date(now)).catch((error) => {
			Logger.warn({error, userId, guildId}, 'Failed to refresh guild visit activity');
		});
	}
	return {visit, view, user, tier, adultContentAllowed, historyCutoff: guildVisitHistoryCutoff(user, tier, now)};
}

const ENDED_VISIT_MARKER_TTL_SECONDS = 120;

function endedVisitMarkerKey(userId: UserID, guildId: GuildID): string {
	return `guild_visit:ended:${userId}:${guildId}`;
}

export async function markGuildVisitEnded(userId: UserID, guildId: GuildID): Promise<void> {
	await getKVClient().setex(endedVisitMarkerKey(userId, guildId), ENDED_VISIT_MARKER_TTL_SECONDS, '1');
}

async function refreshGuildVisitOpenedAt(visit: GuildVisitRow, now: Date): Promise<void> {
	if ((await getKVClient().get(endedVisitMarkerKey(visit.user_id, visit.guild_id))) !== null) {
		return;
	}
	await guildVisitRepository().touch(visit, now);
}

async function visitorStillAllowedIn(guildId: GuildID, user: User): Promise<boolean> {
	try {
		await assertNotBannedFromGuildVisit({guildId, user, ip: visitorRequestContext.getStore()?.ip ?? null});
		return true;
	} catch (error) {
		if (!(error instanceof BannedFromGuildError || error instanceof IpBannedFromGuildError)) {
			throw error;
		}
		await endGuildVisit({userId: user.id, guildId, reason: GuildVisitEndReasons.BANNED});
		return false;
	}
}

export function channelToVisitorInput(channel: Channel): GuildVisitorChannelInput {
	return {
		id: channel.id.toString(),
		type: channel.type,
		nsfw_override: channel.nsfwOverride,
		permission_overwrites: Array.from(channel.permissionOverwrites.entries()).map(([id, overwrite]) => ({
			id: id.toString(),
			type: overwrite.type,
			allow: overwrite.allow.toString(),
			deny: overwrite.deny.toString(),
		})),
	};
}

export function toVisitorChannelModel(channel: Channel): Channel {
	const kept = new Map(
		Array.from(channel.permissionOverwrites.entries())
			.filter(
				([id, overwrite]) =>
					id.toString() === channel.guildId?.toString() && overwrite.type === ChannelOverwriteTypes.ROLE,
			)
			.map(([id, overwrite]) => [id, overwrite.toPermissionOverwrite()] as const),
	);
	if (kept.size === channel.permissionOverwrites.size) {
		return channel;
	}
	return new Channel({...channel.toRow(), permission_overwrites: kept.size > 0 ? kept : null});
}

export function visitorHistoryCutoff(params: {
	canReadHistory: boolean;
	guildCutoff: string | null;
	windowCutoff: Date | null;
}): string | null {
	const {canReadHistory, guildCutoff, windowCutoff} = params;
	if (windowCutoff === null) {
		return guildCutoff;
	}
	if (canReadHistory) {
		return windowCutoff.toISOString();
	}
	if (guildCutoff === null) {
		return null;
	}
	return new Date(Math.max(new Date(guildCutoff).getTime(), windowCutoff.getTime())).toISOString();
}

export function computeVisitorChannelPermissionsFor(params: {
	access: Pick<GuildVisitorAccess, 'view' | 'adultContentAllowed'>;
	channel: Channel;
	parent: Channel | null;
	thread?: {private: boolean} | null;
}): bigint {
	const {access, channel, parent} = params;
	return computeGuildVisitorChannelPermissions({
		guildId: access.view.guild.id.toString(),
		everyonePermissions: access.view.everyonePermissions,
		guildNsfw: access.view.guild.nsfw,
		channel: channelToVisitorInput(channel),
		parent: parent ? {nsfw_override: parent.nsfwOverride} : null,
		thread: params.thread ?? null,
		adultContentAllowed: access.adultContentAllowed,
	});
}

interface VisitorRequestContext {
	ip: string | null;
	ipKey: string | null;
	rateLimitService: IRateLimitService | null;
	enforce: boolean;
}

const visitorRequestContext = new AsyncLocalStorage<VisitorRequestContext>();

export function createVisitorRequestContextMiddleware(shouldEnforce: (ctx: Context<HonoEnv>) => boolean) {
	return createMiddleware<HonoEnv>(async (ctx, next) => {
		const ip = getRequestClientIp(ctx);
		const ipKey = ip ? (getSameIpDecisionKey(ip) ?? ip) : null;
		await visitorRequestContext.run(
			{ip, ipKey, rateLimitService: ctx.get('rateLimitService') ?? null, enforce: shouldEnforce(ctx)},
			next,
		);
	});
}

interface VisitorBucket {
	bucket: string;
	limit: number;
	windowMs: number;
	key: 'user' | 'ip' | 'guild';
	unverifiedOnly?: boolean;
}

export const GUILD_VISITOR_READ_BUCKETS: ReadonlyArray<VisitorBucket> = [
	{bucket: 'channel:messages:visitor', limit: 10, windowMs: ms('10 seconds'), key: 'user'},
	{bucket: 'channel:messages:visitor:ip', limit: 30, windowMs: ms('10 seconds'), key: 'ip'},
	{
		bucket: 'channel:messages:visitor:guild',
		limit: 600,
		windowMs: ms('60 seconds'),
		key: 'guild',
		unverifiedOnly: true,
	},
];

export const GUILD_VISITOR_SEARCH_BUCKETS: ReadonlyArray<VisitorBucket> = [
	{bucket: 'channel:search:visitor', limit: 5, windowMs: ms('60 seconds'), key: 'user'},
	{bucket: 'channel:search:visitor:ip', limit: 15, windowMs: ms('60 seconds'), key: 'ip'},
];

export const GUILD_VISIT_START_IP_BUCKETS: ReadonlyArray<VisitorBucket> = [
	{bucket: 'guild:visit:start:ip', limit: 30, windowMs: ms('60 seconds'), key: 'ip'},
];

export async function chargeGuildVisitorBuckets(params: {
	buckets: ReadonlyArray<VisitorBucket>;
	userId: UserID;
	guildId: GuildID | null;
	tier: AccountTier;
}): Promise<void> {
	const context = visitorRequestContext.getStore();
	if (!context?.enforce || !context.rateLimitService) {
		return;
	}
	for (const bucket of params.buckets) {
		if (bucket.unverifiedOnly && params.tier === 'verified') {
			continue;
		}
		let subject: string | null;
		if (bucket.key === 'user') {
			subject = `user:${params.userId}`;
		} else if (bucket.key === 'ip') {
			subject = context.ipKey;
		} else {
			subject = params.guildId === null ? null : `guild:${params.guildId}`;
		}
		if (!subject) {
			continue;
		}
		const result = await context.rateLimitService.checkBucketLimit(`${subject}:${bucket.bucket}`, {
			limit: bucket.limit,
			windowMs: bucket.windowMs,
		});
		if (!result.allowed) {
			guildVisitorRateLimitedTotal.inc(`key="${bucket.key}"`);
			throw new RateLimitError({
				retryAfter: result.retryAfter ?? Math.max(1, Math.ceil((result.resetTime.getTime() - Date.now()) / 1000)),
				retryAfterDecimal: result.retryAfterDecimal,
				limit: result.limit,
				resetTime: result.resetTime,
				resetAfterDecimal: result.resetAfterDecimal,
				scope: bucket.key === 'guild' ? 'shared' : 'user',
			});
		}
	}
}

export async function recordGuildVisitHistoryRead(params: {
	userId: UserID;
	guildId: GuildID;
	tier: AccountTier;
	now?: number;
}): Promise<void> {
	const hour = Math.floor((params.now ?? Date.now()) / MS_PER_HOUR);
	const kv = getKVClient();
	const currentKey = `guild_visit:hr:${params.userId}:${params.guildId}:${hour}`;
	const pages = await kv.incr(currentKey);
	if (pages !== 1) {
		return;
	}
	await kv.expire(currentKey, HISTORY_READ_KEY_TTL_SECONDS);
	const previous = await kv.getdel(`guild_visit:hr:${params.userId}:${params.guildId}:${hour - 1}`);
	const previousPages = previous === null ? 0 : Number.parseInt(previous, 10);
	if (!Number.isFinite(previousPages) || previousPages <= 0) {
		return;
	}
	void emitActivity('guild_visit_history_read', params.userId.toString(), {
		user_id: params.userId.toString(),
		guild_id: params.guildId.toString(),
		pages: previousPages,
		tier: params.tier,
	});
}
