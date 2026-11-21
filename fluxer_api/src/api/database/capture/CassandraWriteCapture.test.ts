// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineTable} from '@app/api/database/CassandraTableDsl';
import {getStatementMeta, type PreparedQuery} from '@app/api/database/CassandraTypes';
import {CaptureCodec} from '@app/api/database/capture/CaptureCodec';
import {classifyWriteError} from '@app/api/database/capture/CaptureOutcomes';
import type {CaptureOutcome, IntentRecord, LwtIntentRecord} from '@app/api/database/capture/CaptureRecords';
import {
	type CapturedStatement,
	type CaptureSink,
	WriteCapture,
	type WriteCaptureLimits,
} from '@app/api/database/capture/CassandraWriteCapture';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import cassandra from 'cassandra-driver';
import {describe, expect, it} from 'vitest';

interface Published {
	subject: string;
	msgID: string;
	record: Record<string, unknown>;
}

class RecordingSink implements CaptureSink {
	readonly published: Array<Published> = [];
	readonly events: Array<string> = [];
	failing: (subject: string) => boolean = () => false;
	gate: Promise<void> | null = null;

	async publish(subject: string, body: string, msgID: string): Promise<void> {
		if (this.gate && subject.endsWith('.o')) await this.gate;
		if (this.failing(subject)) throw new Error('capture stream down');
		this.events.push(subject);
		this.published.push({subject, msgID, record: JSON.parse(body) as Record<string, unknown>});
	}

	async close(): Promise<void> {}

	outcomes(): Array<[number, CaptureOutcome]> {
		return this.published.flatMap(({record}) =>
			record.kind === 'o' ? (record.outcomes as Array<[number, CaptureOutcome]>) : [],
		);
	}
}

interface CodeRow {
	code: string;
	uses: number;
}

const Codes = defineTable<CodeRow, 'code'>({
	name: 'capture_producer_codes',
	columns: ['code', 'uses'],
	primaryKey: ['code'],
});

function statement(query: PreparedQuery): CapturedStatement {
	return {cql: query.cql, type: getStatementMeta(query.cql).type, params: query.params, meta: query.kvMeta};
}

const insert = statement(Codes.insert({code: 'abc', uses: 0}));
const claim = statement(Codes.insertIfNotExists({code: 'abc', uses: 0}));

function capture(
	sink: RecordingSink | null,
	options: {timestamp?: () => number; limits?: WriteCaptureLimits} = {},
): WriteCapture {
	return new WriteCapture({
		process: 'api',
		producer: 'api-0:boot',
		codec: new CaptureCodec(cassandra.types),
		sink,
		nextTimestamp: options.timestamp ?? (() => Date.now() * 1000),
		...(options.limits ? {limits: options.limits} : {}),
	});
}

describe('write capture in mode on', () => {
	it('stores the intent before the write and sends the write at the intent timestamp', async () => {
		const sink = new RecordingSink();
		const writes = capture(sink);
		const timestamps: Array<number> = [];
		await writes.plain([insert], false, async (timestamp) => {
			sink.events.push('send');
			timestamps.push(timestamp);
		});
		await writes.close(1000);
		const [intent] = sink.published as [Published];
		const record = intent.record as unknown as IntentRecord;
		expect(sink.events).toEqual([
			'cassandra.capture.api.i',
			'send',
			'cassandra.capture.api.o',
			'cassandra.capture.api.life',
		]);
		expect(intent.msgID).toBe('api-0:boot:1:i');
		expect(record.ts).toBe(timestamps[0]);
		expect(record.statements[0]?.keys).toEqual([{s: 'row', c: {code: 'abc'}}]);
		expect(sink.outcomes()).toEqual([[1, 'applied']]);
		expect(sink.published.at(-1)?.record).toEqual({
			kind: 'life',
			producer: 'api-0:boot',
			state: 'closed',
			last_seq: 1,
		});
	});

	it('refuses the write when the intent cannot be stored and records it as failed', async () => {
		const sink = new RecordingSink();
		sink.failing = (subject) => subject.endsWith('.i');
		const writes = capture(sink);
		let sent = false;
		await expect(
			writes.plain([insert], false, async () => {
				sent = true;
			}),
		).rejects.toBeInstanceOf(ServiceUnavailableError);
		await writes.close(1000);
		expect(sent).toBe(false);
		expect(sink.outcomes()).toEqual([[1, 'failed']]);
	});

	it('refuses a write whose timestamp is already too old', async () => {
		const sink = new RecordingSink();
		const writes = capture(sink, {timestamp: () => (Date.now() - 6000) * 1000});
		let sent = false;
		await expect(
			writes.plain([insert], false, async () => {
				sent = true;
			}),
		).rejects.toBeInstanceOf(ServiceUnavailableError);
		await writes.close(1000);
		expect(sent).toBe(false);
		expect(sink.outcomes()).toEqual([[1, 'failed']]);
	});

	it('records driver errors as failed or unknown and rethrows them', async () => {
		const sink = new RecordingSink();
		const writes = capture(sink);
		const timeout = new cassandra.errors.ResponseError(0x1100, 'write timeout');
		const unavailable = new cassandra.errors.ResponseError(0x1000, 'unavailable');
		await expect(writes.plain([insert], true, () => Promise.reject(timeout))).rejects.toBe(timeout);
		await expect(writes.plain([insert], true, () => Promise.reject(unavailable))).rejects.toBe(unavailable);
		await writes.close(1000);
		expect(sink.outcomes()).toEqual([
			[1, 'unknown'],
			[2, 'failed'],
		]);
	});

	it('records the rows of a conditional write and whether it applied', async () => {
		const sink = new RecordingSink();
		const writes = capture(sink);
		for (const applied of [true, false, undefined]) {
			await writes.conditional(
				claim,
				async () => applied,
				(result) => result,
			);
		}
		await writes.close(1000);
		const intent = sink.published[0]?.record as unknown as LwtIntentRecord;
		expect(intent.rows).toEqual([{t: 'capture_producer_codes', c: {code: 'abc'}, insert: true}]);
		expect(sink.outcomes()).toEqual([
			[1, 'applied'],
			[2, 'not_applied'],
			[3, 'unknown'],
		]);
	});

	it('holds new writes while the outcome buffer is full, drops nothing and closes once it drains', async () => {
		const sink = new RecordingSink();
		const gate = Promise.withResolvers<void>();
		sink.gate = gate.promise;
		const writes = capture(sink, {limits: {maxInFlight: 8, maxBufferedOutcomes: 1, slotWaitMs: 20}});
		await writes.plain([insert], false, async () => undefined);
		await expect(writes.plain([insert], false, async () => undefined)).rejects.toBeInstanceOf(ServiceUnavailableError);
		const closed = writes.close(1000);
		gate.resolve();
		await closed;
		expect(sink.outcomes()).toEqual([[1, 'applied']]);
		expect(sink.published.at(-1)?.record).toMatchObject({state: 'closed', last_seq: 1});
		await expect(writes.plain([insert], false, async () => undefined)).rejects.toBeInstanceOf(ServiceUnavailableError);
	});
});

describe('write capture in mode measure', () => {
	it('passes every write through without checks', async () => {
		const writes = capture(null, {timestamp: () => (Date.now() - 60_000) * 1000});
		let sent = 0;
		await writes.plain([insert], false, async () => {
			sent++;
		});
		await writes.conditional(
			claim,
			async () => true,
			(result) => result,
		);
		await writes.close(1000);
		await writes.plain([insert], false, async () => {
			sent++;
		});
		expect(sent).toBe(2);
	});

	it('still sends a write whose record it cannot build', async () => {
		const unencodable = {...insert, params: {code: 'abc', uses: Symbol('not a cassandra value')}};
		let sent = 0;
		await capture(null).plain([unencodable], false, async () => {
			sent++;
		});
		expect(sent).toBe(1);
	});
});

describe('write capture in mode on, unencodable write', () => {
	it('refuses the write rather than send it uncaptured', async () => {
		const sink = new RecordingSink();
		const unencodable = {...insert, params: {code: 'abc', uses: Symbol('not a cassandra value')}};
		let sent = false;
		await expect(
			capture(sink).plain([unencodable], false, async () => {
				sent = true;
			}),
		).rejects.toThrow();
		expect(sent).toBe(false);
		expect(sink.published).toEqual([]);
	});
});

describe('write error classification', () => {
	const refused = Object.assign(new Error('connect ECONNREFUSED'), {code: 'ECONNREFUSED'});
	const cases: Array<[string, unknown, 'failed' | 'unknown']> = [
		['unavailable', new cassandra.errors.ResponseError(0x1000, ''), 'failed'],
		['overloaded', new cassandra.errors.ResponseError(0x1001, ''), 'failed'],
		['bootstrapping', new cassandra.errors.ResponseError(0x1002, ''), 'failed'],
		['invalid', new cassandra.errors.ResponseError(0x2200, ''), 'failed'],
		['syntax', new cassandra.errors.ResponseError(0x2000, ''), 'failed'],
		['unauthorized', new cassandra.errors.ResponseError(0x2100, ''), 'failed'],
		['config', new cassandra.errors.ResponseError(0x2300, ''), 'failed'],
		['unprepared', new cassandra.errors.ResponseError(0x2500, ''), 'failed'],
		['write timeout', new cassandra.errors.ResponseError(0x1100, ''), 'unknown'],
		['write failure', new cassandra.errors.ResponseError(0x1500, ''), 'unknown'],
		['server error', new cassandra.errors.ResponseError(0x0000, ''), 'unknown'],
		['client timeout', new cassandra.errors.OperationTimedOutError('timed out'), 'unknown'],
		['busy connection', new cassandra.errors.BusyConnectionError('h:9042', 2048, 4), 'failed'],
		['no host, refused', new cassandra.errors.NoHostAvailableError({'h:9042': refused}), 'failed'],
		[
			'no host, timed out',
			new cassandra.errors.NoHostAvailableError({'h:9042': new cassandra.errors.OperationTimedOutError('t')}),
			'unknown',
		],
		['anything else', new Error('socket hang up'), 'unknown'],
	];
	for (const [name, error, outcome] of cases) {
		it(`treats ${name} as ${outcome}`, () => {
			expect(classifyWriteError(error)).toBe(outcome);
		});
	}
});
