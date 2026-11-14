// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID, WebhookID} from '@app/api/BrandedTypes';
import {
	deleteOneOrMany,
	executeConditional,
	fetchMany,
	fetchOne,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import type {CrosspostedMessageRow, CrosspostSourceByChannelRow} from '@app/api/database/types/ChannelTypes';
import {CrosspostedMessages, CrosspostSourcesByChannel} from '@app/api/Tables';

export interface CrosspostedMessageKey {
	sourceMessageId: MessageID;
	webhookId: WebhookID;
}

export interface CrosspostSyncState {
	targetMessageId: MessageID;
	sourceFingerprint: string | null;
}

export interface CrosspostSource {
	sourceChannelId: ChannelID;
	sourceMessageId: MessageID;
}

const FETCH_CROSSPOSTED_MESSAGE_CQL = CrosspostedMessages.selectCql({
	where: [CrosspostedMessages.where.eq('source_message_id'), CrosspostedMessages.where.eq('webhook_id')],
	limit: 1,
});

function createBySourceMessageFirstPageQuery(limit: number) {
	return CrosspostedMessages.select({
		where: CrosspostedMessages.where.eq('source_message_id'),
		orderBy: {col: 'webhook_id', direction: 'ASC'},
		limit,
	});
}

function createBySourceMessagePageQuery(limit: number) {
	return CrosspostedMessages.select({
		where: [CrosspostedMessages.where.eq('source_message_id'), CrosspostedMessages.where.gt('webhook_id')],
		orderBy: {col: 'webhook_id', direction: 'ASC'},
		limit,
	});
}

function createSourcesByChannelFirstPageQuery(limit: number) {
	return CrosspostSourcesByChannel.select({
		columns: ['source_message_id'],
		where: CrosspostSourcesByChannel.where.eq('source_channel_id'),
		orderBy: {col: 'source_message_id', direction: 'ASC'},
		limit,
	});
}

function createSourcesByChannelPageQuery(limit: number) {
	return CrosspostSourcesByChannel.select({
		columns: ['source_message_id'],
		where: [
			CrosspostSourcesByChannel.where.eq('source_channel_id'),
			CrosspostSourcesByChannel.where.gt('source_message_id'),
		],
		orderBy: {col: 'source_message_id', direction: 'ASC'},
		limit,
	});
}

function toPk(key: CrosspostedMessageKey): Pick<CrosspostedMessageRow, 'source_message_id' | 'webhook_id'> {
	return {source_message_id: key.sourceMessageId, webhook_id: key.webhookId};
}

export class CrosspostedMessageRepository {
	async get(sourceMessageId: MessageID, webhookId: WebhookID): Promise<CrosspostedMessageRow | null> {
		return fetchOne<CrosspostedMessageRow>(FETCH_CROSSPOSTED_MESSAGE_CQL, {
			source_message_id: sourceMessageId,
			webhook_id: webhookId,
		});
	}

	async insertPending(row: CrosspostedMessageRow): Promise<boolean> {
		return executeConditional(CrosspostedMessages.insertIfNotExists({...row, state: 'pending'}));
	}

	async reclaimPending(
		key: CrosspostedMessageKey,
		data: {
			fromTargetMessageId: MessageID;
			toTargetMessageId: MessageID;
			reservedAt: Date;
		},
	): Promise<boolean> {
		return executeConditional(
			CrosspostedMessages.conditionalPatchByPk(
				toPk(key),
				{
					target_message_id: Db.set(data.toTargetMessageId),
					reserved_at: Db.set(data.reservedAt),
				},
				{state: 'pending', target_message_id: data.fromTargetMessageId},
			),
		);
	}

	async markDelivered(key: CrosspostedMessageKey, data: CrosspostSyncState): Promise<boolean> {
		return executeConditional(
			CrosspostedMessages.conditionalPatchByPk(
				toPk(key),
				{
					state: Db.set('delivered'),
					source_fingerprint: Db.set(data.sourceFingerprint),
				},
				{target_message_id: data.targetMessageId},
			),
		);
	}

	async updateSynced(key: CrosspostedMessageKey, data: CrosspostSyncState): Promise<boolean> {
		return executeConditional(
			CrosspostedMessages.conditionalPatchByPk(
				toPk(key),
				{source_fingerprint: Db.set(data.sourceFingerprint)},
				{state: 'delivered', target_message_id: data.targetMessageId},
			),
		);
	}

	async listBySourceMessage(
		sourceMessageId: MessageID,
		options: {afterWebhookId?: WebhookID; limit: number},
	): Promise<Array<CrosspostedMessageRow>> {
		if (options.afterWebhookId !== undefined) {
			return fetchMany<CrosspostedMessageRow>(
				createBySourceMessagePageQuery(options.limit).bind({
					source_message_id: sourceMessageId,
					webhook_id: options.afterWebhookId,
				}),
			);
		}
		return fetchMany<CrosspostedMessageRow>(
			createBySourceMessageFirstPageQuery(options.limit).bind({source_message_id: sourceMessageId}),
		);
	}

	async delete(
		key: CrosspostedMessageKey,
		expected?: Partial<Pick<CrosspostedMessageRow, 'state' | 'target_message_id'>>,
	): Promise<boolean> {
		if (expected && Object.keys(expected).length > 0) {
			return executeConditional(CrosspostedMessages.conditionalDeleteByPk(toPk(key), expected));
		}
		await deleteOneOrMany(CrosspostedMessages.deleteByPk(toPk(key)));
		return true;
	}

	async addSource(source: CrosspostSource): Promise<void> {
		await upsertOne(
			CrosspostSourcesByChannel.upsertAll({
				source_channel_id: source.sourceChannelId,
				source_message_id: source.sourceMessageId,
			}),
		);
	}

	async listSourcesByChannel(
		sourceChannelId: ChannelID,
		options: {afterMessageId?: MessageID; limit: number},
	): Promise<Array<MessageID>> {
		const rows =
			options.afterMessageId !== undefined
				? await fetchMany<Pick<CrosspostSourceByChannelRow, 'source_message_id'>>(
						createSourcesByChannelPageQuery(options.limit).bind({
							source_channel_id: sourceChannelId,
							source_message_id: options.afterMessageId,
						}),
					)
				: await fetchMany<Pick<CrosspostSourceByChannelRow, 'source_message_id'>>(
						createSourcesByChannelFirstPageQuery(options.limit).bind({source_channel_id: sourceChannelId}),
					);
		return rows.map((row) => row.source_message_id);
	}

	async deleteSource(source: CrosspostSource): Promise<void> {
		await deleteOneOrMany(
			CrosspostSourcesByChannel.deleteByPk({
				source_channel_id: source.sourceChannelId,
				source_message_id: source.sourceMessageId,
			}),
		);
	}
}
