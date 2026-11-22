// SPDX-License-Identifier: AGPL-3.0-or-later

import {fetchOnce} from '@app/api/capture/ReplayFetch';
import type {Consumer, JsMsg} from '@nats-io/jetstream';
import {describe, expect, it} from 'vitest';

function fakeConsumer(seqs: Array<number>, failAfter: number | null): Pick<Consumer, 'fetch'> & {stopped: number} {
	const fake = {
		stopped: 0,
		fetch: async () => ({
			stop: () => {
				fake.stopped++;
			},
			async *[Symbol.asyncIterator]() {
				for (const [index, seq] of seqs.entries()) {
					if (failAfter !== null && index === failAfter) throw new Error('heartbeats missed');
					yield {seq} as unknown as JsMsg;
				}
			},
		}),
	};
	return fake as unknown as Pick<Consumer, 'fetch'> & {stopped: number};
}

describe('fetch once', () => {
	it('hands over every message of a fetch', async () => {
		const seen: Array<number> = [];
		await fetchOnce(fakeConsumer([1, 2, 3], null), {}, async (msg) => {
			seen.push(msg.seq);
			return true;
		});
		expect(seen).toEqual([1, 2, 3]);
	});

	it('returns instead of throwing when the fetch fails partway, so the loop fetches again', async () => {
		const seen: Array<number> = [];
		await expect(
			fetchOnce(fakeConsumer([1, 2, 3], 1), {}, async (msg) => {
				seen.push(msg.seq);
				return true;
			}),
		).resolves.toBeUndefined();
		expect(seen).toEqual([1]);
	});

	it('stops the fetch when the handler asks to', async () => {
		const consumer = fakeConsumer([1, 2, 3], null);
		const seen: Array<number> = [];
		await fetchOnce(consumer, {}, async (msg) => {
			seen.push(msg.seq);
			return msg.seq < 2;
		});
		expect(seen).toEqual([1, 2]);
		expect(consumer.stopped).toBe(1);
	});
});
