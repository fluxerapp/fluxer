// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {defineTable} from '@app/api/database/CassandraTableDsl';
import {getStatementMeta, type PreparedQuery} from '@app/api/database/CassandraTypes';
import {CaptureCodec} from '@app/api/database/capture/CaptureCodec';
import type {CapturedStatement, CaptureSink} from '@app/api/database/capture/CassandraWriteCapture';
import type {CdcRecord, KeyRecord} from '@app/api/database/capture/CdcRecords';
import {CaptureClosedError, type CdcCaptureLimits, CdcWriteCapture} from '@app/api/database/capture/CdcWriteCapture';
import cassandra from 'cassandra-driver';
import {describe, expect, it} from 'vitest';

interface Published {
	subject: string;
	msgID: string;
	record: CdcRecord;
}

class RecordingSink implements CaptureSink {
	readonly published: Array<Published> = [];
	failing: (subject: string) => boolean = () => false;
	gate: Promise<void> | null = null;
	closed = false;

	async publish(subject: string, body: string, msgID: string): Promise<void> {
		if (this.gate) await this.gate;
		if (this.failing(subject)) throw new Error('capture stream down');
		this.published.push({subject, msgID, record: JSON.parse(body) as CdcRecord});
	}

	async close(): Promise<void> {
		this.closed = true;
	}

	keyRecords(): Array<KeyRecord> {
		return this.published.flatMap(({record}) => (record.kind === 'k' ? [record] : []));
	}

	life(): Array<CdcRecord> {
		return this.published.flatMap(({record}) => (record.kind === 'life' ? [record] : []));
	}
}

const Users = defineTable<{user_id: bigint; username: string}, 'user_id'>({
	name: 'users',
	columns: ['user_id', 'username'],
	primaryKey: ['user_id'],
});

const ContactLogs = defineTable<{user_id: bigint; event_id: unknown; field: string}, 'user_id' | 'event_id', 'user_id'>(
	{
		name: 'user_contact_change_logs',
		columns: ['user_id', 'event_id', 'field'],
		primaryKey: ['user_id', 'event_id'],
		partitionKey: ['user_id'],
	},
);

const Mentions = defineTable<{user_id: bigint; message_id: bigint}, 'user_id' | 'message_id', 'user_id'>({
	name: 'recent_mentions',
	columns: ['user_id', 'message_id'],
	primaryKey: ['user_id', 'message_id'],
	partitionKey: ['user_id'],
});

const Relationships = defineTable<
	{source_user_id: bigint; target_user_id: bigint; type: number},
	'source_user_id' | 'target_user_id' | 'type',
	'source_user_id'
>({
	name: 'relationships',
	columns: ['source_user_id', 'target_user_id', 'type'],
	primaryKey: ['source_user_id', 'target_user_id', 'type'],
	partitionKey: ['source_user_id'],
});

const Invites = defineTable<{code: string; uses: number}, 'code'>({
	name: 'invites',
	columns: ['code', 'uses'],
	primaryKey: ['code'],
});

const MessagesByAuthor = defineTable<
	{author_id: bigint; message_id: bigint; channel_id: bigint},
	'author_id' | 'message_id'
>({
	name: 'messages_by_author_id_v2',
	columns: ['author_id', 'message_id', 'channel_id'],
	primaryKey: ['author_id', 'message_id'],
});

const fixture = JSON.parse(readFileSync(new URL('./fixtures/records.json', import.meta.url), 'utf8')) as Array<
	CdcRecord | {kind: string}
>;
const FIXTURE_PRODUCER = 'api-7f9c:2a4f7c1e-93d5-4b0a-8e61-5f2c9d7a3b84';
const USER_ID = 1_309_547_563_201_765_376n;

function statement(query: PreparedQuery): CapturedStatement {
	return {cql: query.cql, type: getStatementMeta(query.cql).type, params: query.params, meta: query.kvMeta};
}

const userWrite = statement(Users.upsertAll({user_id: USER_ID, username: 'fluxer'}));
const messageWrite = statement(MessagesByAuthor.insert({author_id: 1n, message_id: 2n, channel_id: 3n}));
const inviteClaim = statement(Invites.insertIfNotExists({code: 'fluxer', uses: 0}));

function capture(
	sink: RecordingSink,
	options: {producer?: string; timestamps?: Array<number>; limits?: Partial<CdcCaptureLimits>} = {},
): CdcWriteCapture {
	const timestamps = options.timestamps ?? [];
	return new CdcWriteCapture({
		process: 'api',
		producer: options.producer ?? 'api-0:boot',
		pod: 'api-0',
		codec: new CaptureCodec(cassandra.types),
		sink,
		nextTimestamp: () => timestamps.shift() ?? Date.now() * 1000,
		limits: {maxQueued: 20_000, retryMs: 5, aliveMs: 300_000, ...options.limits},
	});
}

const timeout = new cassandra.errors.ResponseError(0x1100, 'write timeout');
const unavailable = new cassandra.errors.ResponseError(0x1000, 'unavailable');

describe('cdc capture in mode behind', () => {
	it('builds the key records of the shared fixture', async () => {
		const sink = new RecordingSink();
		const writes = capture(sink, {
			producer: FIXTURE_PRODUCER,
			timestamps: [1_763_750_000_000_000, 1_763_750_000_000_001],
		});
		const eventId = cassandra.types.TimeUuid.fromString('d8a2f6a0-c71f-11f0-8de9-0242ac120002');
		const prefix = Relationships.delete({
			where: [Relationships.where.eq('source_user_id'), Relationships.where.eq('target_user_id')],
		}).bind({source_user_id: USER_ID, target_user_id: USER_ID + 1n});
		const sentAt: Array<number> = [];
		await writes.plain(
			[
				userWrite,
				messageWrite,
				statement(ContactLogs.insert({user_id: USER_ID, event_id: eventId, field: 'email'})),
				statement(Mentions.deletePartition({user_id: USER_ID})),
				statement(prefix),
			],
			true,
			async (timestamp) => {
				sentAt.push(timestamp);
			},
		);
		await writes.conditional(
			inviteClaim,
			async () => undefined,
			() => undefined,
		);
		await writes.close(1000);
		const expected = fixture.filter(
			(record): record is KeyRecord => record.kind === 'k' && (record as KeyRecord).producer === FIXTURE_PRODUCER,
		);
		expect(sink.keyRecords()).toEqual(expected);
		expect(sentAt).toEqual([1_763_750_000_000_000]);
		expect(sink.published.find(({record}) => record.kind === 'k')).toMatchObject({
			subject: 'cassandra.capture.api.k',
			msgID: `${FIXTURE_PRODUCER}:1:k`,
		});
	});

	it('spends no key seq on unsynced, failed or not applied writes', async () => {
		const sink = new RecordingSink();
		const writes = capture(sink);
		await writes.plain([userWrite], false, async () => undefined);
		await writes.plain([messageWrite], false, async () => undefined);
		await expect(writes.plain([userWrite], false, () => Promise.reject(unavailable))).rejects.toBe(unavailable);
		await writes.conditional(
			inviteClaim,
			async () => false,
			(applied) => applied,
		);
		await expect(writes.plain([userWrite], false, () => Promise.reject(timeout))).rejects.toBe(timeout);
		await writes.conditional(
			inviteClaim,
			async () => true,
			(applied) => applied,
		);
		await writes.close(1000);
		expect(sink.keyRecords().map(({seq, out, lwt}) => [seq, out, lwt ?? false])).toEqual([
			[1, 'applied', false],
			[2, 'unknown', false],
			[3, 'applied', true],
		]);
		expect(sink.life().at(-1)).toEqual({
			kind: 'life',
			producer: 'api-0:boot',
			state: 'closed',
			last_seq: 3,
			dropped: 0,
		});
	});

	it('returns every write without waiting on the stream', async () => {
		const sink = new RecordingSink();
		const gate = Promise.withResolvers<void>();
		sink.gate = gate.promise;
		const writes = capture(sink);
		writes.open();
		for (let index = 0; index < 100; index++) {
			await writes.plain([userWrite], false, async () => undefined);
		}
		expect(sink.published).toEqual([]);
		gate.resolve();
		await writes.close(1000);
		expect(sink.keyRecords().map(({seq}) => seq)).toEqual(Array.from({length: 100}, (_, index) => index + 1));
	});

	it('opens with the pod, then reports every key seq it dropped while the stream was down', async () => {
		const sink = new RecordingSink();
		sink.failing = () => true;
		const writes = capture(sink, {limits: {maxQueued: 3}});
		writes.open();
		for (let index = 0; index < 5; index++) {
			await writes.plain([userWrite], false, async () => undefined);
		}
		sink.failing = () => false;
		await writes.close(1000);
		expect(sink.keyRecords().map(({seq}) => seq)).toEqual([3, 4, 5]);
		expect(sink.life()).toEqual([
			{kind: 'life', producer: 'api-0:boot', state: 'open', pod: 'api-0'},
			{kind: 'life', producer: 'api-0:boot', state: 'lossy', from_seq: 1, to_seq: 2},
			{kind: 'life', producer: 'api-0:boot', state: 'closed', last_seq: 5, dropped: 2},
		]);
	});

	it('counts what is still queued at the drain deadline as dropped and closes the sink', async () => {
		const sink = new RecordingSink();
		sink.failing = (subject) => subject.endsWith('.k');
		const writes = capture(sink);
		await writes.plain([userWrite], false, async () => undefined);
		await writes.plain([userWrite], false, async () => undefined);
		await writes.close(30);
		sink.failing = () => false;
		let sent = false;
		await expect(
			writes.plain([userWrite], false, async () => {
				sent = true;
			}),
		).rejects.toBeInstanceOf(CaptureClosedError);
		expect(sent).toBe(false);
		expect(sink.keyRecords()).toEqual([]);
		expect(sink.life()).toEqual([{kind: 'life', producer: 'api-0:boot', state: 'closed', last_seq: 2, dropped: 2}]);
		expect(sink.closed).toBe(true);
	});

	it('refuses a write that starts while the close waits on a publish and leaves it out of the closed record', async () => {
		const sink = new RecordingSink();
		const gate = Promise.withResolvers<void>();
		sink.gate = gate.promise;
		const writes = capture(sink);
		await writes.plain([userWrite], false, async () => undefined);
		const closing = writes.close(30);
		await new Promise((resolve) => setTimeout(resolve, 60));
		let sent = false;
		await expect(
			writes.conditional(
				inviteClaim,
				async () => {
					sent = true;
					return true;
				},
				(applied) => applied,
			),
		).rejects.toBeInstanceOf(CaptureClosedError);
		gate.resolve();
		await closing;
		expect(sent).toBe(false);
		expect(sink.keyRecords().map(({seq}) => seq)).toEqual([1]);
		expect(sink.life()).toEqual([{kind: 'life', producer: 'api-0:boot', state: 'closed', last_seq: 1, dropped: 0}]);
	});

	it('waits for a write already sent when the close starts and records it before the closed record', async () => {
		const sink = new RecordingSink();
		const writes = capture(sink);
		const sending = Promise.withResolvers<void>();
		const write = writes.plain([userWrite], false, () => sending.promise);
		const closing = writes.close(1000);
		await new Promise((resolve) => setTimeout(resolve, 30));
		expect(sink.life().filter((record) => record.kind === 'life' && record.state === 'closed')).toEqual([]);
		sending.resolve();
		await write;
		await closing;
		const kinds = sink.published.map(({record}) => (record.kind === 'k' ? record.seq : record.state));
		expect(kinds).toEqual([1, 'closed']);
		expect(sink.life().at(-1)).toEqual({
			kind: 'life',
			producer: 'api-0:boot',
			state: 'closed',
			last_seq: 1,
			dropped: 0,
		});
	});

	it('counts a write still running at the close deadline as dropped', async () => {
		const sink = new RecordingSink();
		const writes = capture(sink);
		const sending = Promise.withResolvers<void>();
		const write = writes.plain([userWrite], false, () => sending.promise);
		await writes.close(30);
		expect(sink.life().at(-1)).toEqual({
			kind: 'life',
			producer: 'api-0:boot',
			state: 'closed',
			last_seq: 0,
			dropped: 1,
		});
		sending.resolve();
		await write;
		expect(sink.keyRecords()).toEqual([]);
	});

	it('closes after the queue drained with the last seq it queued', async () => {
		const sink = new RecordingSink();
		const writes = capture(sink);
		await writes.plain([userWrite], false, async () => undefined);
		await writes.plain([userWrite], false, async () => undefined);
		await writes.close(1000);
		const kinds = sink.published.map(({record}) => (record.kind === 'k' ? record.seq : record.state));
		expect(kinds).toEqual([1, 2, 'closed']);
		expect(sink.published.at(-1)?.msgID).toBe('api-0:boot:closed');
	});

	it('sends alive with the last seq it queued', async () => {
		const sink = new RecordingSink();
		const writes = capture(sink, {limits: {aliveMs: 20}});
		writes.open();
		await writes.plain([userWrite], false, async () => undefined);
		await new Promise((resolve) => setTimeout(resolve, 60));
		await writes.close(1000);
		expect(sink.life()).toContainEqual({kind: 'life', producer: 'api-0:boot', state: 'alive', last_seq: 1});
	});

	it('still sends a write whose key record it cannot encode and reports the lost seq', async () => {
		const sink = new RecordingSink();
		const writes = capture(sink);
		const unencodable = {...userWrite, params: {user_id: Symbol('not a cassandra value'), username: 'x'}};
		let sent = 0;
		await writes.plain([unencodable], false, async () => {
			sent++;
		});
		await writes.plain([userWrite], false, async () => undefined);
		await writes.close(1000);
		expect(sent).toBe(1);
		expect(sink.keyRecords().map(({seq}) => seq)).toEqual([2]);
		expect(sink.life()).toEqual([
			{kind: 'life', producer: 'api-0:boot', state: 'lossy', from_seq: 1, to_seq: 1},
			{kind: 'life', producer: 'api-0:boot', state: 'closed', last_seq: 2, dropped: 1},
		]);
	});
});
