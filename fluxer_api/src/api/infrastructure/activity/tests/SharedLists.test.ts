// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	resetSharedListsForTests,
	sharedListHas,
	startSharedListWatch,
} from '@app/api/infrastructure/activity/SharedLists';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import {JetStreamApiCodes, JetStreamApiError, type JetStreamClient} from '@nats-io/jetstream';
import {afterEach, describe, expect, it, vi} from 'vitest';

interface LogEntry {
	level: 'info' | 'debug';
	fields: object;
	message?: string;
}

class RecordingLogger implements LoggerInterface {
	readonly entries: Array<LogEntry> = [];
	trace(): void {}
	warn(): void {}
	error(): void {}
	fatal(): void {}
	info(fields: object | string, message?: string): void {
		this.record('info', fields, message);
	}
	debug(fields: object | string, message?: string): void {
		this.record('debug', fields, message);
	}
	child(): LoggerInterface {
		return this;
	}
	messages(level: LogEntry['level'], message: string): Array<LogEntry> {
		return this.entries.filter((entry) => entry.level === level && entry.message === message);
	}
	private record(level: LogEntry['level'], fields: object | string, message?: string): void {
		this.entries.push(typeof fields === 'string' ? {level, fields: {}, message: fields} : {level, fields, message});
	}
}

function isStreamNotFound(error: unknown): boolean {
	return error instanceof JetStreamApiError && error.code === JetStreamApiCodes.StreamNotFound;
}

interface FakeMessage {
	subject: string;
	headers: {get(name: string): string} | undefined;
	string(): string;
}

class FakeJetStream {
	streamExists = false;
	streamChecks = 0;
	consumerGets = 0;
	readonly pending: Array<FakeMessage> = [];

	get attempts(): number {
		return this.streamChecks + this.consumerGets;
	}

	readonly streams = {
		get: async (name: string) => {
			this.streamChecks++;
			if (!this.streamExists) {
				throw new JetStreamApiError({
					code: 404,
					err_code: JetStreamApiCodes.StreamNotFound,
					description: 'stream not found',
				});
			}
			return {name};
		},
	};

	readonly consumers = {
		get: async () => {
			this.consumerGets++;
			if (!this.streamExists) {
				throw new JetStreamApiError({
					code: 404,
					err_code: JetStreamApiCodes.StreamNotFound,
					description: 'stream not found',
				});
			}
			const messages = this.pending.splice(0);
			let close = () => {};
			const closed = new Promise<void>((resolve) => {
				close = resolve;
			});
			return {
				consume: async () => ({
					close: async () => close(),
					async *[Symbol.asyncIterator]() {
						yield* messages;
						await closed;
					},
				}),
			};
		},
	};

	client(): JetStreamClient {
		return this as unknown as JetStreamClient;
	}
}

function kvPut(name: string, value: string): FakeMessage {
	return {subject: `$KV.lists.${name}`, headers: undefined, string: () => value};
}

describe('shared list watch', () => {
	afterEach(() => {
		resetSharedListsForTests();
	});

	it('checks for the lists stream quietly and only consumes once it exists', async () => {
		const js = new FakeJetStream();
		const logger = new RecordingLogger();
		startSharedListWatch(js.client(), {selfHosted: true, retryDelayMs: 5, logger});

		await vi.waitFor(() => expect(js.attempts).toBeGreaterThanOrEqual(3));
		expect(
			logger
				.messages('debug', 'Shared list watch unavailable, retrying')
				.filter((entry) => isStreamNotFound((entry.fields as {err?: unknown}).err)),
		).toHaveLength(0);
		expect(logger.messages('info', 'Shared list stream does not exist, checking again later')).toHaveLength(1);
		expect(js.consumerGets).toBe(0);
		expect(sharedListHas('email_tld_blocked', 'blocked')).toBe(false);

		js.pending.push(kvPut('email_tld_blocked', 'blocked\n'));
		js.streamExists = true;

		await vi.waitFor(() => expect(sharedListHas('email_tld_blocked', 'blocked')).toBe(true));
		expect(js.consumerGets).toBe(1);
		expect(logger.messages('info', 'Shared list stream found, watching lists')).toHaveLength(1);
		expect(logger.messages('info', 'Shared list stream does not exist, checking again later')).toHaveLength(1);
	});
});
