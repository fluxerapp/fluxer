// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import {
	type FeatureSource,
	guildKey,
	type JoinSource,
	type Meta,
	type RemovalKind,
} from '@app/api/infrastructure/activity/Contract.generated';
import type {Guild} from '@app/api/models/Guild';
import type {Invite} from '@app/api/models/Invite';

export async function emitGuildCreated(guild: Guild): Promise<void> {
	await emitActivity(
		'guild_created',
		guild.ownerId.toString(),
		{
			guild_id: guild.id.toString(),
			owner_id: guild.ownerId.toString(),
			member_count: guild.memberCount,
		},
		null,
		guild.id.toString(),
	);
}

export async function emitGuildFeaturesChanged(params: {
	previous: Guild;
	updated: Guild;
	actorId: UserID | null;
	source: FeatureSource;
	meta?: Meta | null;
}): Promise<void> {
	const {previous, updated} = params;
	const added = [...updated.features].filter((feature) => !previous.features.has(feature)).sort();
	const removed = [...previous.features].filter((feature) => !updated.features.has(feature)).sort();
	const pausedUntilMs = updated.invitesPausedUntil?.getTime() ?? null;
	const pauseChanged = pausedUntilMs !== (previous.invitesPausedUntil?.getTime() ?? null);
	if (added.length === 0 && removed.length === 0 && !pauseChanged) return;
	const guildId = updated.id.toString();
	await emitActivity(
		'guild_features_changed',
		guildKey(guildId),
		{
			guild_id: guildId,
			actor_id: params.actorId?.toString() ?? null,
			source: params.source,
			added,
			removed,
			invites_paused_until_ms: pausedUntilMs,
		},
		params.meta ?? null,
		`${guildId}:${updated.version}`,
	);
}

export async function emitInviteCreated(invite: Invite): Promise<void> {
	if (!invite.guildId) return;
	const guildId = invite.guildId.toString();
	await emitActivity(
		'invite_created',
		guildKey(guildId),
		{
			guild_id: guildId,
			channel_id: invite.channelId?.toString() ?? null,
			code: invite.code,
			inviter_id: invite.inviterId?.toString() ?? null,
			max_uses: invite.maxUses,
			max_age_s: invite.maxAge,
			temporary: invite.temporary,
		},
		null,
		invite.code,
	);
}

export async function emitInviteDeleted(invite: Invite): Promise<void> {
	if (!invite.guildId) return;
	const guildId = invite.guildId.toString();
	await emitActivity('invite_deleted', guildKey(guildId), {guild_id: guildId, code: invite.code}, null, invite.code);
}

export async function emitGuildMemberRemoved(params: {
	guildId: GuildID;
	userId: UserID;
	actorId: UserID | null;
	kind: RemovalKind;
}): Promise<void> {
	await emitActivity('guild_member_removed', params.userId.toString(), {
		guild_id: params.guildId.toString(),
		user_id: params.userId.toString(),
		actor_id: params.actorId?.toString() ?? null,
		kind: params.kind,
	});
}

export async function emitGuildJoinRefused(params: {
	guildId: GuildID;
	userId: UserID;
	joinSource: JoinSource;
}): Promise<void> {
	await emitActivity('guild_join_refused', params.userId.toString(), {
		guild_id: params.guildId.toString(),
		user_id: params.userId.toString(),
		cause: 'invites_paused',
		join_source: params.joinSource,
	});
}
