// SPDX-License-Identifier: AGPL-3.0-or-later

import {CdcConsumer} from '@app/api/capture/cdc/CdcMain';
import type {Consumer, JsMsg} from '@nats-io/jetstream';
import {describe, expect, it, vi} from 'vitest';

interface FakeMsg {
	msg: JsMsg;
	ack: ReturnType<typeof vi.fn>;
	nak: ReturnType<typeof vi.fn>;
}

function keyMessage(seq: number, events: Array<string>): FakeMsg {
	const ack = vi.fn(() => events.push(`ack:${seq}`));
	const nak = vi.fn(() => events.push(`nak:${seq}`));
	const record = {kind: 'k', producer: 'p1', seq, ts: Date.now(), out: 'ok', keys: []};
	const msg = {
		seq,
		redelivered: false,
		subject: 'capture.k',
		timestampNanos: BigInt(Date.now()) * 1_000_000n,
		json: () => record,
		ack,
		nak,
		working: vi.fn(),
	} as unknown as JsMsg;
	return {msg, ack, nak};
}

function batch(messages: Array<JsMsg>): AsyncIterable<JsMsg> & {stop(): void; close(): Promise<void>} {
	return {
		async *[Symbol.asyncIterator]() {
			for (const msg of messages) yield msg;
		},
		stop: () => {},
		close: async () => {},
	};
}

function runtime(events: Array<string>, flushSeconds: number) {
	return {
		config: {flushSeconds, webhookUrl: null},
		client: {},
		codec: {},
		encoder: {},
		consistencies: {quorum: 1, serial: 1},
		clickhouse: {
			select: vi.fn(async () => []),
			insert: vi.fn(async (table: string) => {
				events.push(`insert:${table}`);
			}),
			command: vi.fn(async () => {}),
		},
		state: {
			read: vi.fn(async () => new Map()),
			write: vi.fn(async () => {
				events.push('state');
			}),
		},
	};
}

function consumerOf(fetch: () => Promise<AsyncIterable<JsMsg>>): Consumer {
	return {fetch} as unknown as Consumer;
}

function cdcConsumer(events: Array<string>, flushSeconds: number): CdcConsumer {
	return new CdcConsumer(runtime(events, flushSeconds) as never, {tables: new Map(), unlisted: []} as never);
}

describe('CdcConsumer shutdown', () => {
	it('hands an unapplied message back at once when it stops mid window', async () => {
		const events: Array<string> = [];
		const cdc = cdcConsumer(events, 60);
		const first = keyMessage(1, events);
		let calls = 0;
		const consumer = consumerOf(async () => {
			calls += 1;
			if (calls === 1) return batch([first.msg]);
			cdc.stop();
			return batch([]);
		});

		await cdc.apply(consumer);

		expect(first.nak).toHaveBeenCalledTimes(1);
		expect(first.nak).toHaveBeenCalledWith();
		expect(first.ack).not.toHaveBeenCalled();
		expect(events).toEqual(['nak:1']);
	});

	it('acks a message only after its batch is written and hands back nothing after that', async () => {
		const events: Array<string> = [];
		const cdc = cdcConsumer(events, 0.05);
		const first = keyMessage(1, events);
		let calls = 0;
		const consumer = consumerOf(async () => {
			calls += 1;
			if (calls === 1) return batch([first.msg]);
			if (events.some((event) => event.startsWith('ack:'))) cdc.stop();
			await new Promise((resolve) => setTimeout(resolve, 10));
			return batch([]);
		});

		await cdc.apply(consumer);

		const acked = events.indexOf('ack:1');
		expect(acked).toBeGreaterThan(events.indexOf('insert:fluxer_cdc._recheck'));
		expect(acked).toBeGreaterThan(events.indexOf('state'));
		expect(first.nak).not.toHaveBeenCalled();
		expect(first.ack).toHaveBeenCalledTimes(1);
	});
});
