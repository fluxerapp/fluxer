// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID, WebhookID, WebhookToken} from '@app/api/BrandedTypes';
import {
	BatchBuilder,
	deleteOneOrMany,
	fetchMany,
	fetchManyInChunks,
	fetchOne,
} from '@app/api/database/CassandraQueryExecution';
import {buildPatchFromData, executeVersionedUpdate} from '@app/api/database/CassandraVersionedUpdate';
import type {WebhookRow, WebhooksBySourceChannelRow} from '@app/api/database/types/ChannelTypes';
import {WEBHOOK_COLUMNS} from '@app/api/database/types/ChannelTypes';
import {Webhook} from '@app/api/models/Webhook';
import {Webhooks, WebhooksByChannel, WebhooksByGuild, WebhooksBySourceChannel} from '@app/api/Tables';

const FETCH_WEBHOOK_BY_ID_CQL = Webhooks.selectCql({
	where: Webhooks.where.eq('webhook_id'),
	limit: 1,
});
const FETCH_WEBHOOK_BY_TOKEN_CQL = Webhooks.selectCql({
	where: [Webhooks.where.eq('webhook_id'), Webhooks.where.eq('webhook_token')],
	limit: 1,
});
const FETCH_WEBHOOK_IDS_BY_GUILD_CQL = WebhooksByGuild.selectCql({
	columns: ['webhook_id'],
	where: WebhooksByGuild.where.eq('guild_id'),
});
const FETCH_WEBHOOK_IDS_BY_CHANNEL_CQL = WebhooksByChannel.selectCql({
	columns: ['webhook_id'],
	where: WebhooksByChannel.where.eq('channel_id'),
});
const FETCH_WEBHOOKS_BY_IDS_CQL = Webhooks.selectCql({
	where: Webhooks.where.in('webhook_id', 'webhook_ids'),
});
const FETCH_WEBHOOK_GUILDS_BY_SOURCE_CHANNEL_CQL = WebhooksBySourceChannel.selectCql({
	columns: ['webhook_id', 'guild_id'],
	where: WebhooksBySourceChannel.where.eq('source_channel_id'),
});

function createSourceChannelFirstPageQuery(limit: number) {
	return WebhooksBySourceChannel.select({
		columns: ['webhook_id', 'guild_id'],
		where: WebhooksBySourceChannel.where.eq('source_channel_id'),
		orderBy: {col: 'webhook_id', direction: 'ASC'},
		limit,
	});
}

function createSourceChannelPageQuery(limit: number) {
	return WebhooksBySourceChannel.select({
		columns: ['webhook_id', 'guild_id'],
		where: [WebhooksBySourceChannel.where.eq('source_channel_id'), WebhooksBySourceChannel.where.gt('webhook_id')],
		orderBy: {col: 'webhook_id', direction: 'ASC'},
		limit,
	});
}

export class WebhookRepository {
	async findUnique(webhookId: WebhookID): Promise<Webhook | null> {
		const result = await fetchOne<WebhookRow>(FETCH_WEBHOOK_BY_ID_CQL, {webhook_id: webhookId});
		return result ? new Webhook(result) : null;
	}

	async findByToken(webhookId: WebhookID, token: WebhookToken): Promise<Webhook | null> {
		const result = await fetchOne<WebhookRow>(FETCH_WEBHOOK_BY_TOKEN_CQL, {
			webhook_id: webhookId,
			webhook_token: token,
		});
		return result ? new Webhook(result) : null;
	}

	async create(data: {
		webhookId: WebhookID;
		token: WebhookToken;
		type: number;
		guildId: GuildID | null;
		channelId: ChannelID | null;
		creatorId: UserID | null;
		name: string;
		avatarHash: string | null;
		sourceGuildId?: GuildID | null;
		sourceChannelId?: ChannelID | null;
	}): Promise<Webhook> {
		const webhookData: WebhookRow = {
			webhook_id: data.webhookId,
			webhook_token: data.token,
			type: data.type,
			guild_id: data.guildId,
			channel_id: data.channelId,
			creator_id: data.creatorId,
			name: data.name,
			avatar_hash: data.avatarHash,
			source_guild_id: data.sourceGuildId ?? null,
			source_channel_id: data.sourceChannelId ?? null,
			version: 1,
		};
		const result = await executeVersionedUpdate<WebhookRow, 'webhook_id' | 'webhook_token'>(
			async () => {
				return await fetchOne<WebhookRow>(FETCH_WEBHOOK_BY_ID_CQL, {webhook_id: data.webhookId});
			},
			(current) => ({
				pk: {webhook_id: data.webhookId, webhook_token: data.token},
				patch: buildPatchFromData(webhookData, current, WEBHOOK_COLUMNS, ['webhook_id', 'webhook_token']),
			}),
			Webhooks,
		);
		const batch = new BatchBuilder();
		if (data.guildId) {
			batch.addPrepared(
				WebhooksByGuild.upsertAll({
					guild_id: data.guildId,
					webhook_id: data.webhookId,
				}),
			);
		}
		if (data.channelId) {
			batch.addPrepared(
				WebhooksByChannel.upsertAll({
					channel_id: data.channelId,
					webhook_id: data.webhookId,
				}),
			);
		}
		if (data.sourceChannelId && data.guildId) {
			batch.addPrepared(
				WebhooksBySourceChannel.upsertAll({
					source_channel_id: data.sourceChannelId,
					webhook_id: data.webhookId,
					guild_id: data.guildId,
				}),
			);
		}
		await batch.execute();
		return new Webhook({...webhookData, version: result.finalVersion ?? 1});
	}

	async update(
		webhookId: WebhookID,
		data: Partial<{
			token: WebhookToken;
			type: number;
			guildId: GuildID | null;
			channelId: ChannelID | null;
			creatorId: UserID | null;
			name: string;
			avatarHash: string | null;
		}>,
		oldData?: WebhookRow | null,
	): Promise<Webhook | null> {
		const existing = oldData !== undefined ? (oldData ? new Webhook(oldData) : null) : await this.findUnique(webhookId);
		if (!existing) return null;
		const updatedData: WebhookRow = {
			webhook_id: webhookId,
			webhook_token: data.token ?? existing.token,
			type: data.type ?? existing.type,
			guild_id: data.guildId !== undefined ? data.guildId : existing.guildId,
			channel_id: data.channelId !== undefined ? data.channelId : existing.channelId,
			creator_id: data.creatorId !== undefined ? data.creatorId : existing.creatorId,
			name: data.name ?? existing.name,
			avatar_hash: data.avatarHash !== undefined ? data.avatarHash : existing.avatarHash,
			source_guild_id: existing.sourceGuildId,
			source_channel_id: existing.sourceChannelId,
			version: existing.version,
		};
		const result = await executeVersionedUpdate<WebhookRow, 'webhook_id' | 'webhook_token'>(
			async () => fetchOne<WebhookRow>(FETCH_WEBHOOK_BY_ID_CQL, {webhook_id: webhookId}),
			(current) => ({
				pk: {webhook_id: webhookId, webhook_token: updatedData.webhook_token},
				patch: buildPatchFromData(updatedData, current, WEBHOOK_COLUMNS, ['webhook_id', 'webhook_token']),
			}),
			Webhooks,
			{initialData: oldData},
		);
		const batch = new BatchBuilder();
		if (existing.guildId !== updatedData.guild_id) {
			if (existing.guildId) {
				batch.addPrepared(
					WebhooksByGuild.deleteByPk({
						guild_id: existing.guildId,
						webhook_id: webhookId,
					}),
				);
			}
			if (updatedData.guild_id) {
				batch.addPrepared(
					WebhooksByGuild.upsertAll({
						guild_id: updatedData.guild_id,
						webhook_id: webhookId,
					}),
				);
			}
		}
		if (existing.channelId !== updatedData.channel_id) {
			if (existing.channelId) {
				batch.addPrepared(
					WebhooksByChannel.deleteByPk({
						channel_id: existing.channelId,
						webhook_id: webhookId,
					}),
				);
			}
			if (updatedData.channel_id) {
				batch.addPrepared(
					WebhooksByChannel.upsertAll({
						channel_id: updatedData.channel_id,
						webhook_id: webhookId,
					}),
				);
			}
		}
		await batch.execute();
		return new Webhook({...updatedData, version: result.finalVersion ?? 1});
	}

	async delete(webhookId: WebhookID): Promise<void> {
		const webhook = await this.findUnique(webhookId);
		if (!webhook) return;
		await deleteOneOrMany(
			Webhooks.deleteByPk({
				webhook_id: webhookId,
				webhook_token: webhook.token,
			}),
		);
		const batch = new BatchBuilder();
		if (webhook.guildId) {
			batch.addPrepared(
				WebhooksByGuild.deleteByPk({
					guild_id: webhook.guildId,
					webhook_id: webhookId,
				}),
			);
		}
		if (webhook.channelId) {
			batch.addPrepared(
				WebhooksByChannel.deleteByPk({
					channel_id: webhook.channelId,
					webhook_id: webhookId,
				}),
			);
		}
		if (webhook.sourceChannelId) {
			batch.addPrepared(
				WebhooksBySourceChannel.deleteByPk({
					source_channel_id: webhook.sourceChannelId,
					webhook_id: webhookId,
				}),
			);
		}
		await batch.execute();
	}

	async listIdsBySourceChannel(
		sourceChannelId: ChannelID,
		options: {afterWebhookId?: WebhookID; limit: number},
	): Promise<Array<{webhookId: WebhookID; guildId: GuildID}>> {
		const rows =
			options.afterWebhookId !== undefined
				? await fetchMany<Pick<WebhooksBySourceChannelRow, 'webhook_id' | 'guild_id'>>(
						createSourceChannelPageQuery(options.limit).bind({
							source_channel_id: sourceChannelId,
							webhook_id: options.afterWebhookId,
						}),
					)
				: await fetchMany<Pick<WebhooksBySourceChannelRow, 'webhook_id' | 'guild_id'>>(
						createSourceChannelFirstPageQuery(options.limit).bind({
							source_channel_id: sourceChannelId,
						}),
					);
		return rows.map((row) => ({webhookId: row.webhook_id, guildId: row.guild_id}));
	}

	async countBySourceChannel(sourceChannelId: ChannelID): Promise<{channelCount: number; guildCount: number}> {
		const rows = await fetchMany<Pick<WebhooksBySourceChannelRow, 'webhook_id' | 'guild_id'>>(
			FETCH_WEBHOOK_GUILDS_BY_SOURCE_CHANNEL_CQL,
			{source_channel_id: sourceChannelId},
		);
		return {
			channelCount: rows.length,
			guildCount: new Set(rows.map((row) => row.guild_id)).size,
		};
	}

	private async fetchWebhooksByIds(webhookIds: Array<bigint>): Promise<Array<Webhook>> {
		if (webhookIds.length === 0) {
			return [];
		}
		const rows = await fetchManyInChunks<WebhookRow>(FETCH_WEBHOOKS_BY_IDS_CQL, webhookIds, (chunk) => ({
			webhook_ids: chunk,
		}));
		return rows.map((row) => new Webhook(row));
	}

	async listByGuild(guildId: GuildID): Promise<Array<Webhook>> {
		const webhookIds = await fetchMany<{
			webhook_id: bigint;
		}>(FETCH_WEBHOOK_IDS_BY_GUILD_CQL, {guild_id: guildId});
		return this.fetchWebhooksByIds(webhookIds.map(({webhook_id}) => webhook_id));
	}

	async listByChannel(channelId: ChannelID): Promise<Array<Webhook>> {
		const webhookIds = await fetchMany<{
			webhook_id: bigint;
		}>(FETCH_WEBHOOK_IDS_BY_CHANNEL_CQL, {
			channel_id: channelId,
		});
		return this.fetchWebhooksByIds(webhookIds.map(({webhook_id}) => webhook_id));
	}

	async countByGuild(guildId: GuildID): Promise<number> {
		const webhookIds = await fetchMany<{
			webhook_id: bigint;
		}>(FETCH_WEBHOOK_IDS_BY_GUILD_CQL, {guild_id: guildId});
		return webhookIds.length;
	}

	async countByChannel(channelId: ChannelID): Promise<number> {
		const webhookIds = await fetchMany<{
			webhook_id: bigint;
		}>(FETCH_WEBHOOK_IDS_BY_CHANNEL_CQL, {
			channel_id: channelId,
		});
		return webhookIds.length;
	}
}
