// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomInt, randomUUID} from 'node:crypto';
import {Config} from '@app/api/Config';
import {loadTableLayouts, readRows, UnreadableKey} from '@app/api/capture/CaptureTables';
import {
	IMAGE_SUBJECT,
	type ImageRecord,
	MARK_SUBJECT,
	type MarkRecord,
	PASS_SUBJECT,
	type PassRecord,
	SETTLE_SUBJECT,
} from '@app/api/capture/SettleRecords';
import type {TableLayout} from '@app/api/capture/SettleRules';
import {CaptureCodec, type CaptureValue} from '@app/api/database/capture/CaptureCodec';
import type {CaptureScope, IntentRecord, SettleRecord} from '@app/api/database/capture/CaptureRecords';
import {Logger} from '@app/api/Logger';
import {DeliverPolicy, type JetStreamClient, type JetStreamManager} from '@nats-io/jetstream';
import {getClient, getSerialConsistency, initCassandra, shutdownCassandra} from '@pkgs/cassandra/src/Client';
import {JetStreamConnectionManager} from '@pkgs/nats/src/JetStreamConnectionManager';
import type cassandra from 'cassandra-driver';

const SETTLE_MARGIN_US = 15 * 60 * 1_000_000;
const FINAL_MARGIN_US = 1_000_000;
const READ_CONCURRENCY = 32;
const SCAN_BATCH = 1000;

export interface SettleOptions {
	final: boolean;
	verify: number | null;
}

export interface SettleKey {
	t: string;
	s: CaptureScope;
	c: Record<string, CaptureValue>;
	serial: boolean;
	insert: boolean;
}

interface PassStart {
	prevH: number;
	scanFrom: number;
}

export function passStart(last: PassRecord | null): PassStart {
	if (last === null) return {prevH: 0, scanFrom: 1};
	return last.state === 'done'
		? {prevH: last.h, scanFrom: last.scan_from}
		: {prevH: last.prev_h, scanFrom: last.scan_from};
}

export function passHorizon(nowUs: number, mark: MarkRecord): number | null {
	return mark.watermark === null ? null : Math.min(nowUs - SETTLE_MARGIN_US, mark.watermark);
}

export function collectSettleKeys(
	records: Iterable<{seq: number; record: SettleRecord}>,
	from: number,
	until: number,
	scanEnd: number,
): {keys: Array<SettleKey>; nextScanFrom: number} {
	const keys = new Map<string, SettleKey>();
	let nextScanFrom = scanEnd + 1;
	for (const {seq, record} of records) {
		if (record.ts >= until) {
			nextScanFrom = Math.min(nextScanFrom, seq);
			continue;
		}
		if (record.ts < from) continue;
		const id = `${record.t}\u0000${record.s}\u0000${JSON.stringify(record.c)}`;
		const key = keys.get(id) ?? {t: record.t, s: record.s, c: record.c, serial: false, insert: false};
		key.serial ||= record.reason === 'lwt_unknown';
		key.insert ||= record.insert === true;
		keys.set(id, key);
	}
	return {keys: [...keys.values()], nextScanFrom};
}

async function lastOnSubject<T>(jsm: JetStreamManager, stream: string, subject: string): Promise<T | null> {
	const message = await jsm.streams.getMessage(stream, {last_by_subj: subject});
	return message?.json<T>() ?? null;
}

async function scanSettleRecords(
	js: JetStreamClient,
	stream: string,
	from: number,
	end: number,
): Promise<Array<{seq: number; record: SettleRecord}>> {
	const records: Array<{seq: number; record: SettleRecord}> = [];
	if (from > end) return records;
	const consumer = await js.consumers.get(stream, {
		filter_subjects: SETTLE_SUBJECT,
		deliver_policy: DeliverPolicy.StartSequence,
		opt_start_seq: from,
	});
	for (;;) {
		const batch = await consumer.fetch({max_messages: SCAN_BATCH, expires: 2000});
		let received = 0;
		let done = false;
		for await (const msg of batch) {
			received++;
			if (msg.seq > end) {
				done = true;
				break;
			}
			records.push({seq: msg.seq, record: msg.json<SettleRecord>()});
			if (msg.seq === end || msg.info.pending === 0) done = true;
		}
		if (done || received === 0) break;
	}
	await consumer.delete();
	return records;
}

async function sampleIntentKeys(jsm: JetStreamManager, stream: string, count: number): Promise<Array<SettleKey>> {
	const {state} = await jsm.streams.info(stream);
	const keys: Array<SettleKey> = [];
	for (let attempt = 0; keys.length < count && attempt < count * 20 && state.last_seq > 0; attempt++) {
		const message = await jsm.streams.getMessage(stream, {seq: randomInt(state.first_seq, state.last_seq + 1)});
		if (!message?.subject.endsWith('.i')) continue;
		for (const statement of message.json<IntentRecord>().statements) {
			for (const key of statement.keys ?? []) {
				if (key.s === 'row' && keys.length < count) {
					keys.push({t: statement.t, s: key.s, c: key.c, serial: false, insert: false});
				}
			}
		}
	}
	return keys;
}

async function imageOf(
	client: cassandra.Client,
	layouts: Map<string, TableLayout>,
	codec: CaptureCodec,
	key: SettleKey,
	pass: Pick<ImageRecord, 'pass' | 'h' | 'verify'>,
	serial: number,
): Promise<ImageRecord> {
	const image: ImageRecord = {
		kind: 'img',
		...pass,
		read_at: Date.now(),
		t: key.t,
		s: key.s,
		c: key.c,
		rows: [],
		...(key.insert ? {insert: true} : {}),
	};
	const layout = layouts.get(key.t);
	if (!layout) return {...image, unreadable: 'table does not exist'};
	try {
		const rows = await readRows(client, layout, codec.decodeRecord(key.c), key.serial ? serial : undefined);
		image.rows = rows.map((row) => ({
			c: codec.encodeRecord(row.key),
			cells: Object.fromEntries(
				Object.entries(row.cells).map(([column, cell]) => [column, {...cell, v: codec.encode(cell.v)}]),
			),
		}));
		return image;
	} catch (error) {
		if (error instanceof UnreadableKey) return {...image, unreadable: error.message};
		throw error;
	}
}

async function eachLimited<T>(
	items: ReadonlyArray<T>,
	limit: number,
	run: (item: T, index: number) => Promise<void>,
): Promise<void> {
	let next = 0;
	await Promise.all(
		Array.from({length: Math.min(limit, items.length)}, async () => {
			while (next < items.length) {
				const index = next++;
				await run(items[index] as T, index);
			}
		}),
	);
}

async function db1NowUs(client: cassandra.Client): Promise<number> {
	const result = await client.execute('SELECT toUnixTimestamp(now()) AS now FROM system.local');
	return Number(result.first()?.get('now')) * 1000;
}

export async function runSettle(options: SettleOptions): Promise<void> {
	const capture = Config.cassandraWriteCapture;
	await initCassandra({
		hosts: Config.cassandra.hosts.split(',').filter(Boolean),
		port: Config.cassandra.port,
		keyspace: Config.cassandra.keyspace,
		localDc: Config.cassandra.localDc,
		consistency: Config.cassandra.consistency,
		serialConsistency: Config.cassandra.serialConsistency,
		username: Config.cassandra.username || undefined,
		password: Config.cassandra.password || undefined,
	});
	const connection = new JetStreamConnectionManager({
		url: capture.natsUrl,
		token: capture.authToken || undefined,
		name: 'cassandra-settle',
	});
	try {
		await connection.connect();
		const client = getClient();
		const {default: driver} = await import('cassandra-driver');
		const codec = new CaptureCodec(driver.types);
		const js = connection.getJetStreamClient();
		const jsm = await connection.getJetStreamManager();
		const stream = capture.stream;
		const layouts = await loadTableLayouts(client, Config.cassandra.keyspace);
		const mark = await lastOnSubject<MarkRecord>(jsm, stream, MARK_SUBJECT);
		if (mark === null) throw new Error('The replayer has not published a watermark yet');
		const startedAt = Date.now();
		const pass = randomUUID();
		const serial = driver.types.consistencies[getSerialConsistency()];
		const publishImages = async (keys: Array<SettleKey>, h: number, verify: boolean) => {
			await eachLimited(keys, READ_CONCURRENCY, async (key, index) => {
				const image = await imageOf(client, layouts, codec, key, {pass, h, ...(verify ? {verify} : {})}, serial);
				await js.publish(IMAGE_SUBJECT, JSON.stringify(image), {msgID: `${pass}:${index}`});
			});
		};
		if (options.verify !== null) {
			const h = passHorizon(Date.now() * 1000, mark);
			if (h === null) throw new Error('The replayer has no watermark yet');
			const keys = await sampleIntentKeys(jsm, stream, options.verify);
			await publishImages(keys, h, true);
			Logger.info({pass, keys: keys.length, h}, 'Capture verify images published');
			return;
		}
		const {prevH, scanFrom} = passStart(await lastOnSubject<PassRecord>(jsm, stream, PASS_SUBJECT));
		if (options.final && !mark.drained) throw new Error('The final pass needs a drained replayer');
		const scanEnd = (await jsm.streams.info(stream)).state.last_seq;
		const records = await scanSettleRecords(js, stream, scanFrom, scanEnd);
		const h = options.final
			? Math.max(Date.now() * 1000, await db1NowUs(client), ...records.map(({record}) => record.ts)) + FINAL_MARGIN_US
			: passHorizon(Date.now() * 1000, mark);
		if (h === null || h <= prevH) {
			Logger.info({prevH, h}, 'No settle pass due');
			return;
		}
		const begin: PassRecord = {kind: 'pass', pass, state: 'begin', h, prev_h: prevH, scan_from: scanFrom};
		await js.publish(PASS_SUBJECT, JSON.stringify(begin));
		const {keys, nextScanFrom} = collectSettleKeys(records, prevH, h, scanEnd);
		await publishImages(keys, h, false);
		const seconds = (Date.now() - startedAt) / 1000;
		const done: PassRecord = {...begin, state: 'done', scan_from: nextScanFrom, keys: keys.length, seconds};
		await js.publish(PASS_SUBJECT, JSON.stringify(done));
		Logger.info({pass, h, prevH, keys: keys.length, seconds}, 'Settle pass published');
	} finally {
		await connection.drain();
		await shutdownCassandra();
	}
}
