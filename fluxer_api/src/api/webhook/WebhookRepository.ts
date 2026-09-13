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
import type {WebhookRow} from '@app/api/database/types/ChannelTypes';
import {WEBHOOK_COLUMNS} from '@app/api/database/types/ChannelTypes';
import {Webhook} from '@app/api/models/Webhook';
import {Webhooks, WebhooksByChannel, WebhooksByGuild} from '@app/api/Tables';
import {IWebhookRepository} from '@app/api/webhook/IWebhookRepository';

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

export class WebhookRepository extends IWebhookRepository {
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
		await batch.execute();
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
