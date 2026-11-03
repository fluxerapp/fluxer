// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID} from '@app/api/BrandedTypes';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import {mapGuildToGuildResponse} from '@app/api/guild/GuildModel';
import {type GuildUpdateLocks, withGuildUpdateLock} from '@app/api/guild/GuildUpdateLock';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {workerMeta} from '@app/api/infrastructure/activity/ActivityMeta';
import {
	type ActionOutcome,
	type FeatureSource,
	INVITES_ALREADY_PAUSED,
} from '@app/api/infrastructure/activity/Contract.generated';
import {emitGuildFeaturesChanged} from '@app/api/infrastructure/activity/GuildEvents';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {Logger} from '@app/api/Logger';
import type {Guild} from '@app/api/models/Guild';
import {type ActionOf, outcomeOf} from '@app/api/user/services/AccountStateApplier';
import {serializeGuildForAudit} from '@app/api/utils/AuditSerializationUtils';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

export const GUILD_INVITE_PAUSE_SCHEDULE_KEY = 'guild:invite_pause:lifts';
const INVITE_PAUSE_FEATURES = [GuildFeatures.INVITES_DISABLED, GuildFeatures.RAID_DETECTED] as const;

export interface GuildInvitePauseDeps {
	guilds: Pick<GuildRepository, 'findUnique' | 'upsertPartial'>;
	locks: GuildUpdateLocks;
	schedule: Pick<IKVProvider, 'zadd' | 'zrem' | 'zrangebyscore'>;
	gateway: Pick<IGatewayService, 'dispatchGuild'>;
	auditLog: Pick<GuildAuditLogService, 'record' | 'computeChanges'>;
	now?: () => number;
}

export function memberEndsInvitePause(guild: Guild, nextFeatures: ReadonlySet<string>): boolean {
	return (
		guild.invitesPausedUntil !== null &&
		guild.features.has(GuildFeatures.INVITES_DISABLED) &&
		!nextFeatures.has(GuildFeatures.INVITES_DISABLED)
	);
}

export function staffEditTouchesInvitePause(guild: Guild, changed: Iterable<string>): boolean {
	if (guild.invitesPausedUntil === null) return false;
	for (const feature of changed) {
		if ((INVITE_PAUSE_FEATURES as ReadonlyArray<string>).includes(feature)) return true;
	}
	return false;
}

async function writePause(
	deps: GuildInvitePauseDeps,
	guild: Guild,
	features: Set<string>,
	until: Date | null,
	source: FeatureSource,
): Promise<void> {
	const updated = await deps.guilds.upsertPartial(guild.id, {features, invites_paused_until: until}, guild.toRow());
	try {
		await deps.gateway.dispatchGuild({
			guildId: guild.id,
			event: 'GUILD_UPDATE',
			data: mapGuildToGuildResponse(updated),
		});
	} catch (error) {
		Logger.error({error, guildId: guild.id}, 'Failed to dispatch guild update after invite pause change');
	}
	const changes = deps.auditLog
		.computeChanges(serializeGuildForAudit(guild), serializeGuildForAudit(updated))
		.filter((change) => change.key === 'features');
	if (changes.length > 0) {
		try {
			await deps.auditLog.record({
				guildId: guild.id,
				userId: SYSTEM_USER_ID,
				action: AuditLogActionType.GUILD_UPDATE,
				targetId: guild.id,
				changes,
			});
		} catch (error) {
			Logger.error({error, guildId: guild.id}, 'Failed to record invite pause change in the guild audit log');
		}
	}
	await emitGuildFeaturesChanged({
		previous: guild,
		updated,
		actorId: null,
		source,
		meta: workerMeta(),
	});
}

async function liftPause(deps: GuildInvitePauseDeps, guild: Guild, source: FeatureSource): Promise<void> {
	const features = new Set(guild.features);
	for (const feature of INVITE_PAUSE_FEATURES) features.delete(feature);
	await writePause(deps, guild, features, null, source);
	await deps.schedule.zrem(GUILD_INVITE_PAUSE_SCHEDULE_KEY, guild.id.toString());
}

export async function applyPauseGuildInvites(
	deps: GuildInvitePauseDeps,
	env: ActionOf<'pause_guild_invites'>,
): Promise<ActionOutcome> {
	const guildId = createGuildID(BigInt(env.guild_id));
	return withGuildUpdateLock(deps.locks, guildId, async () => {
		const guild = await deps.guilds.findUnique(guildId);
		if (!guild) return outcomeOf(env, 'ineligible');
		const current = guild.invitesPausedUntil?.getTime() ?? null;
		if (!env.on) {
			if (current === null) return outcomeOf(env, 'noop');
			if (env.until_ms > 0 && current > env.until_ms) return outcomeOf(env, 'noop', null, `until_ms=${current}`);
			await liftPause(deps, guild, 'action');
			return outcomeOf(env, 'applied');
		}
		if (current === null && guild.features.has(GuildFeatures.INVITES_DISABLED)) {
			return outcomeOf(env, 'noop', null, INVITES_ALREADY_PAUSED);
		}
		const now = deps.now?.() ?? Date.now();
		const until = Math.max(current ?? 0, env.until_ms);
		if (until <= now) return outcomeOf(env, 'noop');
		const features = new Set(guild.features);
		for (const feature of INVITE_PAUSE_FEATURES) features.add(feature);
		const detail = `until_ms=${until}`;
		await deps.schedule.zadd(GUILD_INVITE_PAUSE_SCHEDULE_KEY, until, guildId.toString());
		if (until === current && features.size === guild.features.size) return outcomeOf(env, 'noop', null, detail);
		await writePause(deps, guild, features, new Date(until), 'action');
		return outcomeOf(env, 'applied', null, detail);
	});
}

export async function liftExpiredGuildInvitePauses(deps: GuildInvitePauseDeps, limit: number): Promise<number> {
	const now = deps.now?.() ?? Date.now();
	const due = await deps.schedule.zrangebyscore(GUILD_INVITE_PAUSE_SCHEDULE_KEY, '-inf', now, 'LIMIT', 0, limit);
	let lifted = 0;
	for (const member of due) {
		const guildId = createGuildID(BigInt(member));
		try {
			const done = await withGuildUpdateLock(deps.locks, guildId, async () => {
				const guild = await deps.guilds.findUnique(guildId);
				const until = guild?.invitesPausedUntil?.getTime() ?? null;
				if (!guild || until === null) {
					await deps.schedule.zrem(GUILD_INVITE_PAUSE_SCHEDULE_KEY, member);
					return false;
				}
				if (until > now) {
					await deps.schedule.zadd(GUILD_INVITE_PAUSE_SCHEDULE_KEY, until, member);
					return false;
				}
				await liftPause(deps, guild, 'system');
				return true;
			});
			if (done) lifted++;
		} catch (error) {
			Logger.error({error, guildId: member}, 'Failed to lift expired guild invite pause');
		}
	}
	return lifted;
}
