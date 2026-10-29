// SPDX-License-Identifier: AGPL-3.0-or-later

import {LISTS_BUCKET, SHARED_LISTS} from '@app/api/infrastructure/activity/Contract.generated';
import {Logger} from '@app/api/Logger';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import {
	type ConsumerMessages,
	DeliverPolicy,
	JetStreamApiCodes,
	JetStreamApiError,
	type JetStreamClient,
} from '@nats-io/jetstream';

export type SharedListName = (typeof SHARED_LISTS)[number];

const RETRY_DELAY_MS = 30_000;
const SELF_HOSTED_RETRY_DELAY_MS = 300_000;
const STREAM_NAME = `KV_${LISTS_BUCKET}`;
const SUBJECT_PREFIX = `$KV.${LISTS_BUCKET}.`;
const KV_OPERATION_HEADER = 'KV-Operation';

const lists = new Map<string, ReadonlySet<string>>();
interface WatchState {
	stopped: boolean;
	retryDelayMs: number;
	logger: LoggerInterface;
	streamMissingLogged: boolean;
	messages: ConsumerMessages | null;
	retry: ReturnType<typeof setTimeout> | null;
}

let watcher: WatchState | null = null;

function isSharedListName(name: string): name is SharedListName {
	return (SHARED_LISTS as ReadonlyArray<string>).includes(name);
}

function parseSharedList(value: string): ReadonlySet<string> {
	const entries = new Set<string>();
	for (const line of value.split('\n')) {
		const entry = line.trim().toLowerCase();
		if (entry) entries.add(entry);
	}
	return entries;
}

export function applySharedListUpdate(name: string, value: string | null): void {
	if (!isSharedListName(name)) return;
	if (value === null) {
		lists.delete(name);
		return;
	}
	lists.set(name, parseSharedList(value));
}

export function sharedListHas(name: SharedListName, value: string | null | undefined): boolean {
	if (!value) return false;
	return lists.get(name)?.has(value.trim().toLowerCase()) ?? false;
}

export function isBlockedEmailDomain(domain: string | null | undefined): boolean {
	const tld = domain?.split('.').pop();
	return !!tld && sharedListHas('email_tld_blocked', tld);
}

function sharedListSizes(): Record<SharedListName, number | null> {
	const sizes = {} as Record<SharedListName, number | null>;
	for (const name of SHARED_LISTS) {
		sizes[name] = lists.get(name)?.size ?? null;
	}
	return sizes;
}

export function renderSharedListMetrics(): string {
	const lines = [
		'# HELP fluxer_api_shared_list_entries Entries in each shared list, absent while the list is missing',
		'# TYPE fluxer_api_shared_list_entries gauge',
	];
	for (const [name, size] of Object.entries(sharedListSizes())) {
		if (size !== null) lines.push(`fluxer_api_shared_list_entries{list="${name}"} ${size}`);
	}
	return lines.join('\n');
}

function isStreamNotFound(error: unknown): boolean {
	return error instanceof JetStreamApiError && error.code === JetStreamApiCodes.StreamNotFound;
}

async function watchOnce(js: JetStreamClient, state: WatchState): Promise<void> {
	await js.streams.get(STREAM_NAME);
	if (state.streamMissingLogged) {
		state.logger.info({stream: STREAM_NAME}, 'Shared list stream found, watching lists');
		state.streamMissingLogged = false;
	}
	if (state.stopped) return;
	const consumer = await js.consumers.get(STREAM_NAME, {
		filter_subjects: [`${SUBJECT_PREFIX}>`],
		deliver_policy: DeliverPolicy.LastPerSubject,
	});
	const messages = await consumer.consume();
	state.messages = messages;
	if (state.stopped) {
		await messages.close();
		return;
	}
	for await (const msg of messages) {
		const name = msg.subject.slice(SUBJECT_PREFIX.length);
		const operation = msg.headers?.get(KV_OPERATION_HEADER);
		applySharedListUpdate(name, operation === 'DEL' || operation === 'PURGE' ? null : msg.string());
	}
}

function scheduleWatch(js: JetStreamClient, state: WatchState, delayMs: number): void {
	state.retry = setTimeout(() => {
		state.retry = null;
		if (state.stopped) return;
		watchOnce(js, state)
			.catch((error: unknown) => {
				if (isStreamNotFound(error)) {
					if (!state.streamMissingLogged) {
						state.logger.info({stream: STREAM_NAME}, 'Shared list stream does not exist, checking again later');
						state.streamMissingLogged = true;
					}
					return;
				}
				state.logger.debug({err: error}, 'Shared list watch unavailable, retrying');
			})
			.finally(() => {
				state.messages = null;
				if (!state.stopped) scheduleWatch(js, state, state.retryDelayMs);
			});
	}, delayMs);
	state.retry.unref?.();
}

export function startSharedListWatch(
	js: JetStreamClient,
	options: {selfHosted: boolean; retryDelayMs?: number; logger?: LoggerInterface},
): void {
	stopSharedListWatch();
	const state: WatchState = {
		stopped: false,
		retryDelayMs: options.retryDelayMs ?? (options.selfHosted ? SELF_HOSTED_RETRY_DELAY_MS : RETRY_DELAY_MS),
		logger: options.logger ?? Logger,
		streamMissingLogged: false,
		messages: null,
		retry: null,
	};
	watcher = state;
	scheduleWatch(js, state, 0);
}

export function stopSharedListWatch(): void {
	if (!watcher) return;
	watcher.stopped = true;
	if (watcher.retry) clearTimeout(watcher.retry);
	void watcher.messages?.close();
	watcher = null;
}

export function resetSharedListsForTests(): void {
	stopSharedListWatch();
	lists.clear();
}
