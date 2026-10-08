// SPDX-License-Identifier: AGPL-3.0-or-later

import {currentActivityMeta} from '@app/api/infrastructure/activity/ActivityMeta';
import {
	ACTIVITY_SPOOL_KEY,
	type ActivityDrainResult,
	type ActivityPublisher,
	type ActivitySpoolEntry,
	drainActivitySpool,
	spoolActivity,
} from '@app/api/infrastructure/activity/ActivitySpool';
import {
	type Body,
	EVENT_CLASS,
	EVENT_MAJOR,
	EVENT_TTL,
	EVENTS_STREAM,
	type EventKind,
	eventSubject,
	HEADER,
	type Meta,
} from '@app/api/infrastructure/activity/Contract.generated';
import {Logger} from '@app/api/Logger';
import {createSnowflake} from '@fluxer/snowflake/src/Snowflake';
import {JetStreamApiError, type JetStreamClient, type JetStreamManager} from '@nats-io/jetstream';
import {headers} from '@nats-io/transport-node';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

export type ActivityData<K extends EventKind> = Extract<Body, {kind: K}>['data'];
type ActivityResult = 'published' | 'spooled' | 'dropped' | 'rejected';
type ActivityKv = Pick<IKVProvider, 'rpush' | 'llen' | 'lrange' | 'ltrim'>;

const PUBLISH_TIMEOUT_MS = 1000;
const RETRY_DELAYS_MS = [100, 400];
const QUEUE_CAPACITY = 20_000;
const QUEUE_CONCURRENCY = 64;
const STREAM_PROBE_INTERVAL_MS = 15_000;
const STREAM_REPROBE_INTERVAL_MS = 2_000;
const STREAM_NOT_FOUND = 10059;
const SPOOL_DEPTH_INTERVAL_MS = 15_000;

interface PendingActivity {
	entry: ActivitySpoolEntry;
	always: boolean;
}

type StreamState = 'present' | 'missing' | 'unknown';
type EmitMode = 'publish' | 'spool' | 'off';

interface ActivityRuntime {
	publisher: ActivityPublisher;
	kv: ActivityKv;
	mode: () => EmitMode;
	stop: () => void;
}

let runtime: ActivityRuntime | null = null;
const queue: Array<PendingActivity> = [];
let inFlight = 0;
let idleWaiters: Array<() => void> = [];
const counts = new Map<string, number>();
const drainCounts = new Map<string, number>();
let spoolDepth = 0;

function delay(ms: number): Promise<void> {
	return new Promise((resolve) => {
		setTimeout(resolve, ms).unref?.();
	});
}

function count(kind: EventKind, result: ActivityResult): void {
	const key = `kind="${kind}",result="${result}"`;
	counts.set(key, (counts.get(key) ?? 0) + 1);
}

function isRejection(error: unknown): boolean {
	return error instanceof JetStreamApiError || (error instanceof Error && error.name === 'JetStreamNotEnabled');
}

export function jetStreamActivityPublisher(js: JetStreamClient): ActivityPublisher {
	return {
		async publish(subject, payload, options) {
			const h = headers();
			if (options.spooled) h.set(HEADER.spooled, '1');
			await js.publish(subject, payload, {
				msgID: options.msgID,
				ttl: options.ttl,
				timeout: PUBLISH_TIMEOUT_MS,
				headers: h,
			});
		},
	};
}

function freshActivityId(nowMs = Date.now()): string {
	return createSnowflake({
		timestamp: nowMs,
		workerId: Math.floor(Math.random() * 1024),
		sequence: Math.floor(Math.random() * 4096),
	}).toString();
}

async function publishWithRetry(publisher: ActivityPublisher, entry: ActivitySpoolEntry): Promise<ActivityResult> {
	for (let attempt = 0; ; attempt++) {
		try {
			await publisher.publish(entry.subject, entry.data, {msgID: entry.id, ttl: entry.ttl, spooled: false});
			return 'published';
		} catch (error) {
			if (isRejection(error)) {
				Logger.warn({err: error, kind: entry.kind}, 'Activity event publish rejected');
				return 'rejected';
			}
			const wait = RETRY_DELAYS_MS[attempt];
			if (wait === undefined) {
				Logger.warn({err: error, kind: entry.kind}, 'Activity event publish failed');
				return 'spooled';
			}
			await delay(wait);
		}
	}
}

async function spool(kv: ActivityKv, entry: ActivitySpoolEntry, always: boolean): Promise<boolean> {
	try {
		const outcome = await spoolActivity(kv, entry, always);
		if (outcome === 'dropped') {
			count(entry.kind, 'dropped');
			return false;
		}
		spoolDepth++;
		return true;
	} catch (error) {
		Logger.error({err: error, kind: entry.kind, id: entry.id}, 'Activity event could not be spooled');
		count(entry.kind, 'dropped');
		return false;
	}
}

async function deliver(item: PendingActivity): Promise<void> {
	const active = runtime;
	if (!active) {
		count(item.entry.kind, 'dropped');
		return;
	}
	const result = await publishWithRetry(active.publisher, item.entry);
	if (result === 'published') {
		count(item.entry.kind, 'published');
		return;
	}
	if (result === 'rejected') count(item.entry.kind, 'rejected');
	if ((await spool(active.kv, item.entry, item.always)) && result === 'spooled') {
		count(item.entry.kind, 'spooled');
	}
}

function pump(): void {
	while (inFlight < QUEUE_CONCURRENCY && queue.length > 0) {
		const item = queue.shift()!;
		inFlight++;
		void deliver(item).finally(() => {
			inFlight--;
			pump();
		});
	}
	if (inFlight === 0 && queue.length === 0) {
		const waiters = idleWaiters;
		idleWaiters = [];
		for (const resolve of waiters) resolve();
	}
}

async function buildEntry<K extends EventKind>(
	kind: K,
	key: string,
	data: ActivityData<K>,
	meta: Meta | null,
	naturalId: string | null,
	atMs: number,
): Promise<ActivitySpoolEntry> {
	const id = `${kind}:${naturalId ?? freshActivityId(atMs)}`;
	const event = {
		v: EVENT_MAJOR[kind],
		id,
		at_ms: atMs,
		key,
		meta: meta ?? (await currentActivityMeta()),
		kind,
		data,
	};
	return {kind, subject: eventSubject(kind, key), id, ttl: EVENT_TTL[kind], at_ms: atMs, data: JSON.stringify(event)};
}

export async function emitActivity<K extends EventKind>(
	kind: K,
	key: string,
	data: ActivityData<K>,
	meta: Meta | null = null,
	naturalId: string | null = null,
): Promise<void> {
	const active = runtime;
	if (!active) return;
	const mode = active.mode();
	if (mode === 'off') {
		count(kind, 'dropped');
		return;
	}
	const fact = EVENT_CLASS[kind] === 'fact';
	try {
		const entry = await buildEntry(kind, key, data, meta, naturalId, Date.now());
		if (mode === 'spool') {
			if (await spool(active.kv, entry, fact)) count(kind, 'rejected');
			return;
		}
		if (fact) {
			await deliver({entry, always: true});
			return;
		}
		if (queue.length >= QUEUE_CAPACITY) {
			if (runtime && (await spool(runtime.kv, entry, false))) count(kind, 'spooled');
			return;
		}
		queue.push({entry, always: false});
		pump();
	} catch (error) {
		Logger.error({err: error, kind}, 'Activity event could not be built');
		count(kind, 'dropped');
	}
}

export function idleActivityEvents(): Promise<void> {
	if (inFlight === 0 && queue.length === 0) return Promise.resolve();
	return new Promise((resolve) => {
		idleWaiters.push(resolve);
	});
}

export async function drainActivitySpoolNow(
	options: {budgetMs?: number; keepLock?: () => Promise<boolean>} = {},
): Promise<ActivityDrainResult | null> {
	const active = runtime;
	if (active?.mode() !== 'publish') return null;
	const result = await drainActivitySpool(active.publisher, active.kv, options);
	spoolDepth = result.remaining;
	for (const [name, value] of [
		['published', result.published],
		['expired', result.expired],
		['failed', result.failed],
	] as const) {
		if (value > 0) drainCounts.set(name, (drainCounts.get(name) ?? 0) + value);
	}
	return result;
}

export interface ActivityEventsOptions {
	publisher: ActivityPublisher;
	kv: ActivityKv;
	jsm?: Pick<JetStreamManager, 'streams'> | null;
	spoolWhileMissing?: boolean;
}

async function probeStream(jsm: Pick<JetStreamManager, 'streams'>): Promise<StreamState> {
	try {
		await jsm.streams.info(EVENTS_STREAM);
		return 'present';
	} catch (error) {
		if (error instanceof JetStreamApiError && error.code === STREAM_NOT_FOUND) return 'missing';
		Logger.warn({err: error}, 'Activity stream probe failed');
		return 'unknown';
	}
}

export async function startActivityEvents(options: ActivityEventsOptions): Promise<void> {
	stopActivityEvents();
	const {jsm} = options;
	let state: StreamState = jsm ? await probeStream(jsm) : 'present';
	const timers: Array<ReturnType<typeof setTimeout>> = [];
	const whenMissing: EmitMode = options.spoolWhileMissing ? 'spool' : 'off';
	let stopped = false;
	const schedule = (probe: Pick<JetStreamManager, 'streams'>) => {
		if (stopped || state === 'present') return;
		const wait = state === 'unknown' ? STREAM_REPROBE_INTERVAL_MS : STREAM_PROBE_INTERVAL_MS;
		const timer = setTimeout(() => {
			void probeStream(probe).then((next) => {
				if (next !== state) Logger.info({stream: EVENTS_STREAM, state: next}, 'Activity stream state changed');
				state = next;
				schedule(probe);
			});
		}, wait);
		timer.unref?.();
		timers.push(timer);
	};
	if (jsm && state !== 'present') {
		Logger.info({stream: EVENTS_STREAM, state}, 'Activity stream not confirmed, probing until it exists');
		schedule(jsm);
	}
	const depth = setInterval(() => {
		void options.kv
			.llen(ACTIVITY_SPOOL_KEY)
			.then((value) => {
				spoolDepth = value;
			})
			.catch(() => undefined);
	}, SPOOL_DEPTH_INTERVAL_MS);
	depth.unref?.();
	timers.push(depth);
	runtime = {
		publisher: options.publisher,
		kv: options.kv,
		mode: () => (state === 'missing' ? whenMissing : 'publish'),
		stop: () => {
			stopped = true;
			for (const timer of timers) {
				clearTimeout(timer);
				clearInterval(timer);
			}
		},
	};
}

export async function shutdownActivityEvents(timeoutMs = 2000): Promise<void> {
	await Promise.race([idleActivityEvents(), delay(timeoutMs)]);
	stopActivityEvents();
}

export function stopActivityEvents(): void {
	runtime?.stop();
	runtime = null;
}

export function renderActivityMetrics(): string {
	const lines = [
		'# HELP fluxer_api_activity_events_total Activity events by kind and result',
		'# TYPE fluxer_api_activity_events_total counter',
		...[...counts].map(([labels, value]) => `fluxer_api_activity_events_total{${labels}} ${value}`),
		'# HELP fluxer_api_activity_spool_depth Activity events waiting in the spool',
		'# TYPE fluxer_api_activity_spool_depth gauge',
		`fluxer_api_activity_spool_depth ${spoolDepth}`,
		'# HELP fluxer_api_activity_spool_drained_total Spooled activity events by drain result',
		'# TYPE fluxer_api_activity_spool_drained_total counter',
		...[...drainCounts].map(
			([result, value]) => `fluxer_api_activity_spool_drained_total{result="${result}"} ${value}`,
		),
	];
	return lines.join('\n');
}

export function resetActivityEventsForTests(): void {
	stopActivityEvents();
	queue.length = 0;
	inFlight = 0;
	idleWaiters = [];
	counts.clear();
	drainCounts.clear();
	spoolDepth = 0;
}

export function activityCountForTests(kind: EventKind, result: ActivityResult): number {
	return counts.get(`kind="${kind}",result="${result}"`) ?? 0;
}
