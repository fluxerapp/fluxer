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

import type {ChannelID, MessageID} from '@fluxer/api/src/BrandedTypes';
import type {ListMessagesOptions} from '@fluxer/api/src/channel/repositories/IMessageRepository';
import {BucketScanDirection, scanBucketsWithIndex} from '@fluxer/api/src/channel/repositories/message/BucketScanEngine';
import {
	BatchBuilder,
	buildPatchFromData,
	Db,
	deleteOneOrMany,
	executeConditional,
	executeVersionedUpdate,
	fetchMany,
	fetchOne,
	upsertOne,
} from '@fluxer/api/src/database/Cassandra';
import type {ChannelMessageBucketRow, ChannelStateRow, MessageRow} from '@fluxer/api/src/database/types/MessageTypes';
import {MESSAGE_COLUMNS} from '@fluxer/api/src/database/types/MessageTypes';
import {Logger} from '@fluxer/api/src/Logger';
import {Message} from '@fluxer/api/src/models/Message';
import {
	AttachmentLookup,
	ChannelEmptyBuckets,
	ChannelMessageBuckets,
	ChannelPins,
	ChannelState,
	Messages,
	MessagesByAuthorV2,
} from '@fluxer/api/src/Tables';
import {generateSnowflake} from '@fluxer/snowflake/src/Snowflake';
import * as BucketUtils from '@fluxer/snowflake/src/SnowflakeBuckets';

function getLogger() {
	return Logger.child({module: 'MessageDataRepository'});
}

const DEFAULT_MESSAGE_LIMIT = 50;
const DEFAULT_BUCKET_INDEX_PAGE_SIZE = 200;
const DEFAULT_CAS_RETRIES = 8;
const LEGACY_BUCKETS_TO_CHECK = [0];

const FETCH_MESSAGE_BY_CHANNEL_BUCKET_AND_MESSAGE_ID = Messages.select({
	where: [Messages.where.eq('channel_id'), Messages.where.eq('bucket'), Messages.where.eq('message_id')],
	limit: 1,
});

const FETCH_CHANNEL_STATE = ChannelState.select({
	where: ChannelState.where.eq('channel_id'),
	limit: 1,
});

export class MessageDataRepository {
	async listMessages(
		channelId: ChannelID,
		beforeMessageId?: MessageID,
		limit: number = DEFAULT_MESSAGE_LIMIT,
		afterMessageId?: MessageID,
		options?: ListMessagesOptions,
	): Promise<Array<Message>> {
		if (limit <= 0) return [];

		getLogger().debug(
			{
				channelId: channelId.toString(),
				before: beforeMessageId?.toString() ?? null,
				after: afterMessageId?.toString() ?? null,
				limit,
			},
			'listMessages start',
		);

		if (beforeMessageId && afterMessageId) {
			return this.listMessagesBetween(channelId, afterMessageId, beforeMessageId, limit, options);
		}
		if (beforeMessageId) {
			return this.listMessagesBefore(channelId, beforeMessageId, limit, options);
		}
		if (afterMessageId) {
			return this.listMessagesAfter(channelId, afterMessageId, limit, options);
		}
		return this.listMessagesLatest(channelId, limit);
	}

	private makeFetchMessagesBefore(limit: number) {
		return Messages.select({
			where: [
				Messages.where.eq('channel_id'),
				Messages.where.eq('bucket'),
				Messages.where.lt('message_id', 'before_message_id'),
			],
			orderBy: {col: 'message_id', direction: 'DESC'},
			limit,
		});
	}

	private makeFetchMessagesAfterDesc(limit: number) {
		return Messages.select({
			where: [
				Messages.where.eq('channel_id'),
				Messages.where.eq('bucket'),
				Messages.where.gt('message_id', 'after_message_id'),
			],
			orderBy: {col: 'message_id', direction: 'DESC'},
			limit,
		});
	}

	private makeFetchMessagesBetween(limit: number) {
		return Messages.select({
			where: [
				Messages.where.eq('channel_id'),
				Messages.where.eq('bucket'),
				Messages.where.gt('message_id', 'after_message_id'),
				Messages.where.lt('message_id', 'before_message_id'),
			],
			orderBy: {col: 'message_id', direction: 'DESC'},
			limit,
		});
	}

	private makeFetchMessagesLatestDesc(limit: number) {
		return Messages.select({
			where: [Messages.where.eq('channel_id'), Messages.where.eq('bucket')],
			orderBy: {col: 'message_id', direction: 'DESC'},
			limit,
		});
	}

	private makeFetchMessagesAfterAsc(limit: number) {
		return Messages.select({
			where: [
				Messages.where.eq('channel_id'),
				Messages.where.eq('bucket'),
				Messages.where.gt('message_id', 'after_message_id'),
			],
			orderBy: {col: 'message_id', direction: 'ASC'},
			limit,
		});
	}

	private makeFetchMessagesOldestAsc(limit: number) {
		return Messages.select({
			where: [Messages.where.eq('channel_id'), Messages.where.eq('bucket')],
			orderBy: {col: 'message_id', direction: 'ASC'},
			limit,
		});
	}

	private async listMessagesLatest(channelId: ChannelID, limit: number): Promise<Array<Message>> {
		const state = await this.getChannelState(channelId);

		const nowId = generateSnowflake();
		const maxBucket = BucketUtils.makeBucket(nowId);
		const minBucket = state?.created_bucket ?? BucketUtils.makeBucket(channelId);

		return this.scanBucketsDescForMessages(channelId, {
			limit,
			minBucket,
			maxBucket,
		});
	}

	private async listMessagesBefore(
		channelId: ChannelID,
		before: MessageID,
		limit: number,
		options?: ListMessagesOptions,
	): Promise<Array<Message>> {
		const state = await this.getChannelState(channelId);

		const maxBucket = BucketUtils.makeBucket(before);
		const minBucket = state?.created_bucket ?? BucketUtils.makeBucket(channelId);

		getLogger().debug(
			{
				channelId: channelId.toString(),
				before: before.toString(),
				limit,
				maxBucket,
				minBucket,
				stateCreatedBucket: state?.created_bucket ?? null,
				restrictToBeforeBucket: options?.restrictToBeforeBucket ?? null,
			},
			'listMessagesBefore: computed bucket range',
		);

		return this.scanBucketsDescForMessages(channelId, {
			limit,
			minBucket,
			maxBucket,
			before,
			restrictToBeforeBucket: options?.restrictToBeforeBucket,
		});
	}

	private async listMessagesAfter(
		channelId: ChannelID,
		after: MessageID,
		limit: number,
		options?: ListMessagesOptions,
	): Promise<Array<Message>> {
		const state = await this.getChannelState(channelId);

		const afterBucket = BucketUtils.makeBucket(after);
		const createdMin = state?.created_bucket ?? BucketUtils.makeBucket(channelId);
		const minBucket = Math.max(afterBucket, createdMin);

		const nowBucket = BucketUtils.makeBucket(generateSnowflake());
		const maxBucket = Math.max(nowBucket, minBucket);

		getLogger().debug(
			{
				channelId: channelId.toString(),
				action: 'listMessagesAfter',
				after: after.toString(),
				minBucket,
				maxBucket,
				limit,
				immediateAfter: options?.immediateAfter ?? false,
			},
			'listMessagesAfter parameters',
		);

		if (options?.immediateAfter) {
			const asc = await this.scanBucketsAscForMessages(channelId, {
				limit,
				minBucket,
				maxBucket,
				after,
			});
			return asc.reverse();
		}

		return this.scanBucketsDescForMessages(channelId, {
			limit,
			minBucket,
			maxBucket,
			after,
		});
	}

	private async listMessagesBetween(
		channelId: ChannelID,
		after: MessageID,
		before: MessageID,
		limit: number,
		options?: ListMessagesOptions,
	): Promise<Array<Message>> {
		const state = await this.getChannelState(channelId);

		const afterBucket = BucketUtils.makeBucket(after);
		const beforeBucket = BucketUtils.makeBucket(before);

		const high = Math.max(afterBucket, beforeBucket);
		const low = Math.min(afterBucket, beforeBucket);

		const createdMin = state?.created_bucket ?? BucketUtils.makeBucket(channelId);
		const minBucket = Math.max(low, createdMin);
		const maxBucket = high;

		getLogger().debug(
			{
				channelId: channelId.toString(),
				action: 'listMessagesBetween',
				after: after.toString(),
				before: before.toString(),
				minBucket,
				maxBucket,
				limit,
			},
			'listMessagesBetween parameters',
		);

		return this.scanBucketsDescForMessages(channelId, {
			limit,
			minBucket,
			maxBucket,
			after,
			before,
			restrictToBeforeBucket: options?.restrictToBeforeBucket,
		});
	}

	private async scanBucketsDescForMessages(
		channelId: ChannelID,
		opts: {
			limit: number;
			minBucket: number;
			maxBucket: number;
			before?: MessageID;
			after?: MessageID;
			restrictToBeforeBucket?: boolean;
		},
	): Promise<Array<Message>> {
		const beforeBucket = opts.before ? BucketUtils.makeBucket(opts.before) : null;
		const afterBucket = opts.after ? BucketUtils.makeBucket(opts.after) : null;

		const stopAfterBucket =
			opts.restrictToBeforeBucket === true && opts.before && !opts.after && beforeBucket !== null
				? beforeBucket
				: undefined;

		getLogger().debug(
			{
				channelId: channelId.toString(),
				minBucket: opts.minBucket,
				maxBucket: opts.maxBucket,
				beforeBucket,
				afterBucket,
				restrictToBeforeBucket: opts.restrictToBeforeBucket ?? null,
				stopAfterBucket: stopAfterBucket ?? null,
			},
			'scanBucketsDescForMessages: starting scan',
		);

		const {rows: out} = await scanBucketsWithIndex<MessageRow>(
			{
				listBucketsFromIndex: async (query) =>
					this.listBucketsDescFromIndex(channelId, {
						minBucket: query.minBucket,
						maxBucket: query.maxBucket,
						limit: query.limit,
					}),
				fetchRowsForBucket: async (bucket, limit) =>
					this.fetchRowsForBucket(channelId, bucket, limit, {
						before: opts.before,
						after: opts.after,
						beforeBucket,
						afterBucket,
					}),
				getRowId: (row) => row.message_id,
				onEmptyUnboundedBucket: async (bucket) => this.markBucketEmpty(channelId, bucket),
				onBucketHasRows: async (bucket) => this.touchBucketWithMessages(channelId, bucket),
			},
			{
				limit: opts.limit,
				minBucket: opts.minBucket,
				maxBucket: opts.maxBucket,
				direction: BucketScanDirection.Desc,
				indexPageSize: DEFAULT_BUCKET_INDEX_PAGE_SIZE,
				stopAfterBucket,
			},
		);

		if (out.length === 0) return [];

		let maxId: MessageID = out[0].message_id;
		let maxBucketForId = out[0].bucket;

		for (const row of out) {
			if (row.message_id > maxId) {
				maxId = row.message_id;
				maxBucketForId = row.bucket;
			}
		}

		await this.touchChannelHasMessages(channelId);
		await this.advanceChannelStateLastMessageIfNewer(channelId, maxId, maxBucketForId);

		return this.repairAndMapMessages(channelId, out);
	}

	private async scanBucketsAscForMessages(
		channelId: ChannelID,
		opts: {
			limit: number;
			minBucket: number;
			maxBucket: number;
			after: MessageID;
		},
	): Promise<Array<Message>> {
		const afterBucket = BucketUtils.makeBucket(opts.after);

		const {rows: out} = await scanBucketsWithIndex<MessageRow>(
			{
				listBucketsFromIndex: async (query) =>
					this.listBucketsAscFromIndex(channelId, {
						minBucket: query.minBucket,
						maxBucket: query.maxBucket,
						limit: query.limit,
					}),
				fetchRowsForBucket: async (bucket, limit) =>
					this.fetchRowsForBucketAsc(channelId, bucket, limit, {
						after: opts.after,
						afterBucket,
					}),
				getRowId: (row) => row.message_id,
				onEmptyUnboundedBucket: async (bucket) => this.markBucketEmpty(channelId, bucket),
				onBucketHasRows: async (bucket) => this.touchBucketWithMessages(channelId, bucket),
			},
			{
				limit: opts.limit,
				minBucket: opts.minBucket,
				maxBucket: opts.maxBucket,
				direction: BucketScanDirection.Asc,
				indexPageSize: DEFAULT_BUCKET_INDEX_PAGE_SIZE,
			},
		);

		if (out.length === 0) return [];

		let maxId: MessageID = out[0].message_id;
		let maxBucketForId = out[0].bucket;

		for (const row of out) {
			if (row.message_id > maxId) {
				maxId = row.message_id;
				maxBucketForId = row.bucket;
			}
		}

		await this.touchChannelHasMessages(channelId);
		await this.advanceChannelStateLastMessageIfNewer(channelId, maxId, maxBucketForId);

		return this.repairAndMapMessages(channelId, out);
	}

	private async fetchRowsForBucketAsc(
		channelId: ChannelID,
		bucket: number,
		limit: number,
		meta: {
			after: MessageID;
			afterBucket: number;
		},
	): Promise<{rows: Array<MessageRow>; unbounded: boolean}> {
		getLogger().debug(
			{
				channelId: channelId.toString(),
				bucket,
				limit,
				meta: {after: meta.after.toString(), afterBucket: meta.afterBucket},
			},
			'fetchRowsForBucketAsc parameters',
		);

		if (bucket === meta.afterBucket) {
			const q = this.makeFetchMessagesAfterAsc(limit);
			const rows = await fetchMany<MessageRow>(
				q.bind({
					channel_id: channelId,
					bucket,
					after_message_id: meta.after,
				}),
			);
			return {rows, unbounded: false};
		}

		const q = this.makeFetchMessagesOldestAsc(limit);
		const rows = await fetchMany<MessageRow>(q.bind({channel_id: channelId, bucket}));
		return {rows, unbounded: true};
	}

	private async fetchRowsForBucket(
		channelId: ChannelID,
		bucket: number,
		limit: number,
		meta: {
			before?: MessageID;
			after?: MessageID;
			beforeBucket: number | null;
			afterBucket: number | null;
		},
	): Promise<{rows: Array<MessageRow>; unbounded: boolean}> {
		getLogger().debug(
			{
				channelId: channelId.toString(),
				bucket,
				limit,
				meta: {
					before: meta.before?.toString() ?? null,
					after: meta.after?.toString() ?? null,
					beforeBucket: meta.beforeBucket,
					afterBucket: meta.afterBucket,
				},
			},
			'fetchRowsForBucket parameters',
		);

		if (meta.before && meta.after && meta.beforeBucket === bucket && meta.afterBucket === bucket) {
			const q = this.makeFetchMessagesBetween(limit);
			const rows = await fetchMany<MessageRow>(
				q.bind({
					channel_id: channelId,
					bucket,
					after_message_id: meta.after,
					before_message_id: meta.before,
				}),
			);
			return {rows, unbounded: false};
		}

		if (meta.before && meta.beforeBucket === bucket) {
			const q = this.makeFetchMessagesBefore(limit);
			const rows = await fetchMany<MessageRow>(
				q.bind({
					channel_id: channelId,
					bucket,
					before_message_id: meta.before,
				}),
			);
			return {rows, unbounded: false};
		}

		if (meta.after && meta.afterBucket === bucket) {
			const q = this.makeFetchMessagesAfterDesc(limit);
			const rows = await fetchMany<MessageRow>(
				q.bind({
					channel_id: channelId,
					bucket,
					after_message_id: meta.after,
				}),
			);
			return {rows, unbounded: false};
		}

		const q = this.makeFetchMessagesLatestDesc(limit);
		const rows = await fetchMany<MessageRow>(q.bind({channel_id: channelId, bucket}));
		return {rows, unbounded: true};
	}

	private async touchBucketWithMessages(channelId: ChannelID, bucket: number): Promise<void> {
		const batch = new BatchBuilder();

		batch.addPrepared(
			ChannelMessageBuckets.upsertAll({
				channel_id: channelId,
				bucket,
				updated_at: new Date(),
			}),
		);

		batch.addPrepared(
			ChannelEmptyBuckets.deleteByPk({
				channel_id: channelId,
				bucket,
			}),
		);

		await batch.execute(true);
	}

	private async markBucketEmpty(channelId: ChannelID, bucket: number): Promise<void> {
		const batch = new BatchBuilder();

		batch.addPrepared(
			ChannelMessageBuckets.deleteByPk({
				channel_id: channelId,
				bucket,
			}),
		);

		batch.addPrepared(
			ChannelEmptyBuckets.upsertAll({
				channel_id: channelId,
				bucket,
				updated_at: new Date(),
			}),
		);

		await batch.execute(true);
	}

	private async touchChannelHasMessages(channelId: ChannelID): Promise<void> {
		await upsertOne(
			ChannelState.patchByPk(
				{channel_id: channelId},
				{
					has_messages: Db.set(true),
					updated_at: Db.set(new Date()),
				},
			),
		);
	}

	private async advanceChannelStateLastMessageIfNewer(
		channelId: ChannelID,
		newLastMessageId: MessageID,
		newLastMessageBucket: number,
	): Promise<void> {
		for (let i = 0; i < DEFAULT_CAS_RETRIES; i++) {
			const state = await this.getChannelState(channelId);
			const prev = state?.last_message_id ?? null;

			if (prev !== null && newLastMessageId <= prev) return;

			const q = ChannelState.patchByPkIf(
				{channel_id: channelId},
				{
					has_messages: Db.set(true),
					last_message_id: Db.set(newLastMessageId),
					last_message_bucket: Db.set(newLastMessageBucket),
					updated_at: Db.set(new Date()),
				},
				{col: 'last_message_id', expectedParam: 'prev_last_message_id', expectedValue: prev},
			);

			const res = await executeConditional(q);
			if (res.applied) return;
		}

		Logger.warn(
			{channelId: channelId.toString(), messageId: newLastMessageId.toString()},
			'Failed to advance ChannelState.last_message_id after retries',
		);
	}

	private async getChannelState(channelId: ChannelID): Promise<ChannelStateRow | null> {
		return fetchOne<ChannelStateRow>(FETCH_CHANNEL_STATE.bind({channel_id: channelId}));
	}

	private async listBucketsDescFromIndex(
		channelId: ChannelID,
		opts: {minBucket?: number; maxBucket?: number; limit: number},
	): Promise<Array<number>> {
		const where = [ChannelMessageBuckets.where.eq('channel_id')];

		if (typeof opts.minBucket === 'number') where.push(ChannelMessageBuckets.where.gte('bucket', 'min_bucket'));
		if (typeof opts.maxBucket === 'number') where.push(ChannelMessageBuckets.where.lte('bucket', 'max_bucket'));

		const q = ChannelMessageBuckets.select({
			columns: ['bucket'],
			where,
			orderBy: {col: 'bucket', direction: 'DESC'},
			limit: opts.limit,
		});

		const params = {
			channel_id: channelId,
			...(typeof opts.minBucket === 'number' ? {min_bucket: opts.minBucket} : {}),
			...(typeof opts.maxBucket === 'number' ? {max_bucket: opts.maxBucket} : {}),
		};

		const rows = await fetchMany<Pick<ChannelMessageBucketRow, 'bucket'>>(q.bind(params));
		const buckets = rows.map((r) => r.bucket);

		getLogger().debug(
			{
				channelId: channelId.toString(),
				minBucket: opts.minBucket ?? null,
				maxBucket: opts.maxBucket ?? null,
				limit: opts.limit,
				bucketsFound: buckets,
			},
			'listBucketsDescFromIndex: query result',
		);

		return buckets;
	}

	private async listBucketsAscFromIndex(
		channelId: ChannelID,
		opts: {minBucket?: number; maxBucket?: number; limit: number},
	): Promise<Array<number>> {
		const where = [ChannelMessageBuckets.where.eq('channel_id')];

		if (typeof opts.minBucket === 'number') where.push(ChannelMessageBuckets.where.gte('bucket', 'min_bucket'));
		if (typeof opts.maxBucket === 'number') where.push(ChannelMessageBuckets.where.lte('bucket', 'max_bucket'));

		const q = ChannelMessageBuckets.select({
			columns: ['bucket'],
			where,
			orderBy: {col: 'bucket', direction: 'ASC'},
			limit: opts.limit,
		});

		const params = {
			channel_id: channelId,
			...(typeof opts.minBucket === 'number' ? {min_bucket: opts.minBucket} : {}),
			...(typeof opts.maxBucket === 'number' ? {max_bucket: opts.maxBucket} : {}),
		};

		const rows = await fetchMany<Pick<ChannelMessageBucketRow, 'bucket'>>(q.bind(params));
		return rows.map((r) => r.bucket);
	}

	async getMessage(channelId: ChannelID, messageId: MessageID): Promise<Message | null> {
		const bucket = BucketUtils.makeBucket(messageId);

		const message = await fetchOne<MessageRow>(
			FETCH_MESSAGE_BY_CHANNEL_BUCKET_AND_MESSAGE_ID.bind({
				channel_id: channelId,
				bucket,
				message_id: messageId,
			}),
		);

		if (message) return new Message(message);

		const repairedMessage = await this.attemptBucketReadRepair(channelId, messageId, bucket);
		return repairedMessage;
	}

	async upsertMessage(data: MessageRow, oldData?: MessageRow | null): Promise<Message> {
		const expectedBucket = BucketUtils.makeBucket(data.message_id);
		if (data.bucket !== expectedBucket) {
			throw new Error(
				`Invalid message bucket for ${data.message_id.toString()}: expected ${expectedBucket}, received ${data.bucket}`,
			);
		}

		const batch = new BatchBuilder();

		batch.addPrepared(
			ChannelEmptyBuckets.deleteByPk({
				channel_id: data.channel_id,
				bucket: data.bucket,
			}),
		);

		const result = await executeVersionedUpdate<MessageRow, 'channel_id' | 'bucket' | 'message_id'>(
			async () => {
				const pk = {
					channel_id: data.channel_id,
					bucket: data.bucket,
					message_id: data.message_id,
				};
				const existingMessage = await fetchOne<MessageRow>(FETCH_MESSAGE_BY_CHANNEL_BUCKET_AND_MESSAGE_ID.bind(pk));
				return existingMessage ?? null;
			},
			(current) => ({
				pk: {
					channel_id: data.channel_id,
					bucket: data.bucket,
					message_id: data.message_id,
				},
				patch: buildPatchFromData(data, current, MESSAGE_COLUMNS, ['channel_id', 'bucket', 'message_id']),
			}),
			Messages,
			{initialData: oldData},
		);

		if (!result.applied) {
			throw new Error(`Failed to upsert message ${data.message_id} after LWT retries`);
		}

		const finalVersion = result.finalVersion ?? 1;

		if (data.author_id != null) {
			batch.addPrepared(
				MessagesByAuthorV2.upsertAll({
					author_id: data.author_id,
					channel_id: data.channel_id,
					message_id: data.message_id,
				}),
			);
		}

		if (data.pinned_timestamp) {
			batch.addPrepared(
				ChannelPins.upsertAll({
					channel_id: data.channel_id,
					message_id: data.message_id,
					pinned_timestamp: data.pinned_timestamp,
				}),
			);
		}

		if (oldData?.pinned_timestamp && !data.pinned_timestamp) {
			batch.addPrepared(
				ChannelPins.deleteByPk({
					channel_id: data.channel_id,
					message_id: data.message_id,
					pinned_timestamp: oldData.pinned_timestamp,
				}),
			);
		}

		if (oldData?.attachments) {
			for (const attachment of oldData.attachments) {
				batch.addPrepared(
					AttachmentLookup.deleteByPk({
						channel_id: data.channel_id,
						attachment_id: attachment.attachment_id,
						filename: attachment.filename,
					}),
				);
			}
		}

		if (data.attachments) {
			for (const attachment of data.attachments) {
				batch.addPrepared(
					AttachmentLookup.upsertAll({
						channel_id: data.channel_id,
						attachment_id: attachment.attachment_id,
						filename: attachment.filename,
						message_id: data.message_id,
					}),
				);
			}
		}

		batch.addPrepared(
			ChannelMessageBuckets.upsertAll({
				channel_id: data.channel_id,
				bucket: data.bucket,
				updated_at: new Date(),
			}),
		);

		const createdBucket = BucketUtils.makeBucket(data.channel_id);

		batch.addPrepared(
			ChannelState.patchByPk(
				{channel_id: data.channel_id},
				{
					created_bucket: Db.set(createdBucket),
					has_messages: Db.set(true),
					updated_at: Db.set(new Date()),
				},
			),
		);

		await batch.execute();

		await this.advanceChannelStateLastMessageIfNewer(data.channel_id, data.message_id, data.bucket);

		return new Message({...data, version: finalVersion});
	}

	async updateEmbeds(message: Message): Promise<void> {
		await upsertOne(
			Messages.patchByPk(
				{
					channel_id: message.channelId,
					bucket: message.bucket,
					message_id: message.id,
				},
				{
					embeds: Db.set(message.embeds.length > 0 ? message.embeds.map((e) => e.toMessageEmbed()) : null),
				},
			),
		);
	}

	private async attemptBucketReadRepair(
		channelId: ChannelID,
		messageId: MessageID,
		expectedBucket: number,
	): Promise<Message | null> {
		for (const legacyBucket of LEGACY_BUCKETS_TO_CHECK) {
			if (legacyBucket === expectedBucket) continue;

			const legacyRow = await fetchOne<MessageRow>(
				FETCH_MESSAGE_BY_CHANNEL_BUCKET_AND_MESSAGE_ID.bind({
					channel_id: channelId,
					bucket: legacyBucket,
					message_id: messageId,
				}),
			);

			if (!legacyRow) continue;

			Logger.warn(
				{channelId: channelId.toString(), messageId: messageId.toString(), legacyBucket, expectedBucket},
				'Repairing message bucket mismatch',
			);

			const repairedRow: MessageRow = {
				...legacyRow,
				bucket: expectedBucket,
			};

			const repairedMessage = await this.upsertMessage(repairedRow, legacyRow);

			await deleteOneOrMany(
				Messages.deleteByPk({
					channel_id: channelId,
					bucket: legacyBucket,
					message_id: messageId,
				}),
			);

			return repairedMessage;
		}

		return null;
	}

	private async repairAndMapMessages(channelId: ChannelID, messages: Array<MessageRow>): Promise<Array<Message>> {
		if (messages.length === 0) return [];

		const repaired: Array<Message> = [];

		for (const message of messages) {
			const expectedBucket = BucketUtils.makeBucket(message.message_id);

			if (message.bucket === expectedBucket) {
				repaired.push(new Message(message));
				continue;
			}

			const repairedMessage = await this.attemptBucketReadRepair(channelId, message.message_id, expectedBucket);

			if (repairedMessage) {
				repaired.push(repairedMessage);
				continue;
			}

			Logger.warn(
				{
					channelId: channelId.toString(),
					messageId: message.message_id.toString(),
					legacyBucket: message.bucket,
					expectedBucket,
				},
				'Failed to repair message bucket mismatch during listMessages; returning legacy row',
			);

			repaired.push(new Message(message));
		}

		return repaired;
	}
}
