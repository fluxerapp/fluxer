// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, InviteCode, UserID} from '../BrandedTypes';
import {createInviteCode} from '../BrandedTypes';
import {BatchBuilder, fetchMany, fetchOne, upsertOne} from '../database/CassandraQueryExecution';
import {Db} from '../database/CassandraTypes';
import type {InviteRow} from '../database/types/ChannelTypes';
import type {GuildInviteBundleRow} from '../database/types/InviteBundleTypes';
import {GuildInviteBundle} from '../models/GuildInviteBundle';
import {Invite} from '../models/Invite';
import {GuildInviteBundles, Invites, InvitesByChannel, InvitesByGuild} from '../Tables';
import {IInviteRepository} from './IInviteRepository';

const FETCH_INVITE_BY_CODE_CQL = Invites.selectCql({
	where: Invites.where.eq('code'),
	limit: 1,
});
const FETCH_INVITES_BY_CHANNEL_CQL = InvitesByChannel.selectCql({
	columns: ['code'],
	where: InvitesByChannel.where.eq('channel_id'),
});
const FETCH_INVITES_BY_GUILD_CQL = InvitesByGuild.selectCql({
	columns: ['code'],
	where: InvitesByGuild.where.eq('guild_id'),
});

interface CreateInviteParams {
	code: InviteCode;
	type: number;
	guild_id: GuildID;
	channel_id?: ChannelID | null;
	inviter_id?: UserID | null;
	uses: number;
	max_uses: number;
	max_age: number;
	temporary?: boolean;
}

export class InviteRepository extends IInviteRepository {
	async findUnique(code: InviteCode): Promise<Invite | null> {
		const invite = await fetchOne<InviteRow>(FETCH_INVITE_BY_CODE_CQL, {code});
		return invite ? new Invite(invite) : null;
	}

	async listChannelInvites(channelId: ChannelID): Promise<Array<Invite>> {
		const inviteCodes = await fetchMany<{
			code: string;
		}>(FETCH_INVITES_BY_CHANNEL_CQL, {channel_id: channelId});
		if (inviteCodes.length === 0) return [];
		const invites: Array<Invite> = [];
		for (const {code} of inviteCodes) {
			const invite = await this.findUnique(createInviteCode(code));
			if (invite) invites.push(invite);
		}
		return invites;
	}

	async listGuildInvites(guildId: GuildID): Promise<Array<Invite>> {
		const inviteCodes = await fetchMany<{
			code: string;
		}>(FETCH_INVITES_BY_GUILD_CQL, {guild_id: guildId});
		if (inviteCodes.length === 0) return [];
		const invites: Array<Invite> = [];
		for (const {code} of inviteCodes) {
			const invite = await this.findUnique(createInviteCode(code));
			if (invite) invites.push(invite);
		}
		return invites;
	}

	async create(data: CreateInviteParams): Promise<Invite> {
		const inviteRow: InviteRow = {
			code: data.code,
			type: data.type,
			guild_id: data.guild_id,
			channel_id: data.channel_id ?? null,
			inviter_id: data.inviter_id ?? null,
			created_at: new Date(),
			uses: data.uses,
			max_uses: data.max_uses,
			max_age: data.max_age,
			temporary: data.temporary ?? false,
			version: 1,
		};
		const batch = new BatchBuilder();
		const hasExpiry = inviteRow.max_age > 0;
		if (hasExpiry) {
			batch.addPrepared(Invites.insertWithTtlParam(inviteRow, 'max_age'));
		} else {
			batch.addPrepared(Invites.insert(inviteRow));
		}
		if (inviteRow.guild_id) {
			batch.addPrepared(
				hasExpiry
					? InvitesByGuild.insertWithTtl(
							{
								guild_id: inviteRow.guild_id,
								code: inviteRow.code,
							},
							inviteRow.max_age,
						)
					: InvitesByGuild.insert({
							guild_id: inviteRow.guild_id,
							code: inviteRow.code,
						}),
			);
		}
		if (inviteRow.channel_id) {
			batch.addPrepared(
				hasExpiry
					? InvitesByChannel.insertWithTtl(
							{
								channel_id: inviteRow.channel_id,
								code: inviteRow.code,
							},
							inviteRow.max_age,
						)
					: InvitesByChannel.insert({
							channel_id: inviteRow.channel_id,
							code: inviteRow.code,
						}),
			);
		}
		await batch.execute();
		if (hasExpiry) {
			await upsertOne(Invites.upsertAllWithTtl(inviteRow, inviteRow.max_age));
			if (inviteRow.guild_id) {
				await upsertOne(
					InvitesByGuild.upsertAllWithTtl(
						{
							guild_id: inviteRow.guild_id,
							code: inviteRow.code,
						},
						inviteRow.max_age,
					),
				);
			}
			if (inviteRow.channel_id) {
				await upsertOne(
					InvitesByChannel.upsertAllWithTtl(
						{
							channel_id: inviteRow.channel_id,
							code: inviteRow.code,
						},
						inviteRow.max_age,
					),
				);
			}
		} else {
			await upsertOne(Invites.upsertAll(inviteRow));
			if (inviteRow.guild_id) {
				await upsertOne(
					InvitesByGuild.upsertAll({
						guild_id: inviteRow.guild_id,
						code: inviteRow.code,
					}),
				);
			}
			if (inviteRow.channel_id) {
				await upsertOne(
					InvitesByChannel.upsertAll({
						channel_id: inviteRow.channel_id,
						code: inviteRow.code,
					}),
				);
			}
		}
		return new Invite(inviteRow);
	}

	async updateInviteUses(code: InviteCode, uses: number, invite: Invite): Promise<void> {
		if (invite.maxAge > 0) {
			const remainingTtl = Math.max(
				Math.floor((invite.createdAt.getTime() + invite.maxAge * 1000 - Date.now()) / 1000),
				1,
			);
			await upsertOne(
				Invites.patchByPkWithTtl(
					{code},
					{
						uses: Db.set(uses),
					},
					remainingTtl,
				),
			);
		} else {
			await upsertOne(
				Invites.patchByPk(
					{code},
					{
						uses: Db.set(uses),
					},
				),
			);
		}
	}

	async delete(code: InviteCode): Promise<void> {
		const invite = await this.findUnique(code);
		if (!invite) {
			return;
		}
		const batch = new BatchBuilder();
		batch.addPrepared(Invites.deleteByPk({code}));
		if (invite.guildId) {
			batch.addPrepared(InvitesByGuild.deleteByPk({guild_id: invite.guildId, code}));
		}
		if (invite.channelId) {
			batch.addPrepared(InvitesByChannel.deleteByPk({channel_id: invite.channelId, code}));
		}
		await batch.execute();
	}

	override async createGuildBundle(data: {
		code: InviteCode;
		max_uses: number;
		max_age: number;
		uses: number;
		invites: Array<{guild_id: GuildID; channel_id: ChannelID; code: InviteCode}>;
		inviter_id: UserID;
	}): Promise<GuildInviteBundle> {
		const guildInviteBundleRow: GuildInviteBundleRow = {
			code: data.code,
			max_uses: data.max_uses,
			max_age: data.max_age,
			uses: data.uses,
			invites: data.invites,
			inviter_id: data.inviter_id,
			created_at: new Date(),
		};

		const batch = new BatchBuilder();
		const hasExpiry = guildInviteBundleRow.max_age > 0;
		if (hasExpiry) {
			batch.addPrepared(GuildInviteBundles.insertWithTtlParam(guildInviteBundleRow, 'max_age'));
		} else {
			batch.addPrepared(GuildInviteBundles.insert(guildInviteBundleRow));
		}
		await batch.execute();
		if (hasExpiry) {
			await upsertOne(GuildInviteBundles.upsertAllWithTtl(guildInviteBundleRow, guildInviteBundleRow.max_age));
		} else {
			await upsertOne(GuildInviteBundles.upsertAll(guildInviteBundleRow));
		}

		return new GuildInviteBundle(guildInviteBundleRow);
	}
}
