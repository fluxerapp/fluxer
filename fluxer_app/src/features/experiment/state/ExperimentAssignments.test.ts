// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RestResponse} from '@app/features/platform/types/TransportTypes';
import type {DomainMigrationAssignmentResponse} from '@fluxer/schema/src/domains/admin/DomainMigrationSchemas';
import {
	type ExperimentAssignmentsResponse,
	INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE,
	readDomainMigrationAssignment,
} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		debug = vi.fn();
		info = vi.fn();
		warn = vi.fn();
		error = vi.fn();
	},
}));

vi.mock('@app/features/platform/transport/RestTransport', () => ({
	http: {get: vi.fn(), post: vi.fn()},
}));

const {http} = await import('@app/features/platform/transport/RestTransport');
const {ExperimentAssignments} = await import('@app/features/experiment/state/ExperimentAssignments');
const {ResettableStates} = await import('@app/features/app/state/ResettableStates');

const CANARY_ASSIGNMENT: DomainMigrationAssignmentResponse = {
	enabled: true,
};

const CANARY_ENVELOPE: ExperimentAssignmentsResponse = {
	poll_interval_seconds: 300,
	poll_jitter_percent: 15,
	assignments: {domain_migration: CANARY_ASSIGNMENT},
};

let visibility: DocumentVisibilityState = 'visible';

function reply(status: number, body: unknown, headers: Record<string, string> = {}): RestResponse<unknown> {
	return {ok: status >= 200 && status < 300, status, statusText: '', headers, body};
}

function requestHeaders(index: number): Record<string, string> {
	return vi.mocked(http.get).mock.calls[index]?.[1]?.headers ?? {};
}

function lastScheduledDelayMs(): number {
	const call = vi.mocked(setTimeout).mock.calls.at(-1);
	expect(call).toBeDefined();
	return call![1]!;
}

function setVisibility(next: DocumentVisibilityState): void {
	visibility = next;
	document.dispatchEvent(new Event('visibilitychange'));
}

async function settle(): Promise<void> {
	await vi.advanceTimersByTimeAsync(0);
}

function migrationEnabled(): boolean {
	return readDomainMigrationAssignment(ExperimentAssignments.response).enabled;
}

function deferredReply(): {resolve: (response: RestResponse<unknown>) => void} {
	let resolve!: (response: RestResponse<unknown>) => void;
	const pending = new Promise<RestResponse<unknown>>((complete) => {
		resolve = complete;
	});
	vi.mocked(http.get).mockReturnValueOnce(pending);
	return {resolve};
}

async function adopt(response: ExperimentAssignmentsResponse): Promise<void> {
	vi.mocked(http.get).mockResolvedValue(reply(200, response, {etag: 'W/"seed"'}));
	ExperimentAssignments.start();
	await settle();
}

beforeEach(() => {
	visibility = 'visible';
	Object.defineProperty(document, 'visibilityState', {configurable: true, get: () => visibility});
	vi.useFakeTimers();
	vi.spyOn(globalThis, 'setTimeout');
	vi.spyOn(Math, 'random').mockReturnValue(0.5);
	vi.mocked(http.get).mockReset();
});

afterEach(() => {
	ExperimentAssignments.reset();
	vi.restoreAllMocks();
	vi.useRealTimers();
});

describe('ExperimentAssignments cold start', () => {
	it('keeps the inert envelope when the endpoint is unreachable', async () => {
		vi.mocked(http.get).mockRejectedValue(new Error('network unreachable'));
		ExperimentAssignments.start();
		await settle();
		expect(ExperimentAssignments.response).toBe(INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE);
		expect(ExperimentAssignments.response.assignments.domain_migration).toBeUndefined();
	});

	it('keeps the inert envelope while unauthenticated and retries later', async () => {
		vi.mocked(http.get).mockResolvedValue(reply(401, {message: '401: Unauthorized'}));
		ExperimentAssignments.start();
		await settle();
		expect(ExperimentAssignments.response).toBe(INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE);
		expect(lastScheduledDelayMs()).toBeLessThanOrEqual(690_000);
		await vi.advanceTimersByTimeAsync(700_000);
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(2);
	});
});

describe('ExperimentAssignments response handling', () => {
	it('discards a malformed envelope and keeps the previous value', async () => {
		await adopt(CANARY_ENVELOPE);
		vi.mocked(http.get).mockResolvedValue(
			reply(200, {
				...CANARY_ENVELOPE,
				assignments: {domain_migration: {enabled: 'yes'}},
			}),
		);
		await vi.advanceTimersByTimeAsync(400_000);
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(2);
		expect(ExperimentAssignments.response).toEqual(CANARY_ENVELOPE);
	});

	it('discards a partial envelope rather than merging it', async () => {
		await adopt(CANARY_ENVELOPE);
		vi.mocked(http.get).mockResolvedValue(reply(200, {assignments: {}}));
		await vi.advanceTimersByTimeAsync(400_000);
		expect(ExperimentAssignments.response).toEqual(CANARY_ENVELOPE);
	});

	it('keeps the value and the stored etag across a 304', async () => {
		vi.mocked(http.get).mockResolvedValueOnce(reply(200, CANARY_ENVELOPE, {etag: 'W/"v7"'}));
		ExperimentAssignments.start();
		await settle();
		expect(requestHeaders(0)['If-None-Match']).toBeUndefined();
		vi.mocked(http.get).mockResolvedValue(reply(304, undefined));
		await vi.advanceTimersByTimeAsync(400_000);
		expect(requestHeaders(1)['If-None-Match']).toBe('W/"v7"');
		expect(ExperimentAssignments.response).toEqual(CANARY_ENVELOPE);
		await vi.advanceTimersByTimeAsync(400_000);
		expect(requestHeaders(2)['If-None-Match']).toBe('W/"v7"');
		expect(ExperimentAssignments.response).toEqual(CANARY_ENVELOPE);
	});
});

describe('ExperimentAssignments scheduling', () => {
	it('never schedules a poll below half the minimum interval', async () => {
		vi.spyOn(Math, 'random').mockReturnValue(0);
		await adopt({...CANARY_ENVELOPE, poll_interval_seconds: 60, poll_jitter_percent: 50});
		expect(lastScheduledDelayMs()).toBeGreaterThanOrEqual(30_000);
	});

	it('clamps an out-of-range poll interval into the supported window', async () => {
		vi.spyOn(Math, 'random').mockReturnValue(0.5);
		await adopt({...CANARY_ENVELOPE, poll_interval_seconds: 1});
		expect(lastScheduledDelayMs()).toBe(60_000);
		ExperimentAssignments.reset();
		await adopt({...CANARY_ENVELOPE, poll_interval_seconds: 999_999});
		expect(lastScheduledDelayMs()).toBe(86_400_000);
	});

	it('schedules one timer for repeated starts and cancels it on stop', async () => {
		vi.mocked(http.get).mockResolvedValue(reply(200, CANARY_ENVELOPE, {etag: 'W/"v7"'}));
		ExperimentAssignments.start();
		await settle();
		expect(vi.getTimerCount()).toBe(1);
		ExperimentAssignments.start();
		await settle();
		expect(vi.getTimerCount()).toBe(1);
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(1);
		ExperimentAssignments.stop();
		expect(vi.getTimerCount()).toBe(0);
		await vi.advanceTimersByTimeAsync(1_000_000);
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(1);
		ExperimentAssignments.start();
		await settle();
		expect(vi.getTimerCount()).toBe(1);
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(2);
	});

	it('grows the retry delay on failure and resets it on success', async () => {
		vi.spyOn(Math, 'random').mockReturnValue(0.5);
		vi.mocked(http.get).mockRejectedValue(new Error('network unreachable'));
		ExperimentAssignments.start();
		await settle();
		expect(lastScheduledDelayMs()).toBe(600_000);
		await vi.advanceTimersByTimeAsync(600_000);
		expect(lastScheduledDelayMs()).toBe(1_200_000);
		await vi.advanceTimersByTimeAsync(1_200_000);
		expect(lastScheduledDelayMs()).toBe(1_200_000);
		vi.mocked(http.get).mockResolvedValue(reply(200, CANARY_ENVELOPE, {etag: 'W/"v7"'}));
		await vi.advanceTimersByTimeAsync(1_200_000);
		expect(lastScheduledDelayMs()).toBe(300_000);
	});
});

describe('ExperimentAssignments visibility', () => {
	it('keeps polling while the document is hidden at four times the interval', async () => {
		vi.spyOn(Math, 'random').mockReturnValue(0.5);
		visibility = 'hidden';
		vi.mocked(http.get).mockResolvedValue(reply(200, CANARY_ENVELOPE, {etag: 'W/"v7"'}));
		ExperimentAssignments.start();
		await settle();
		expect(vi.mocked(http.get)).not.toHaveBeenCalled();
		expect(lastScheduledDelayMs()).toBe(1_200_000);
		await vi.advanceTimersByTimeAsync(1_200_000);
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(1);
		expect(ExperimentAssignments.response).toEqual(CANARY_ENVELOPE);
		expect(lastScheduledDelayMs()).toBe(1_200_000);
		await vi.advanceTimersByTimeAsync(1_200_000);
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(2);
		await vi.advanceTimersByTimeAsync(31_000);
		setVisibility('visible');
		await settle();
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(3);
		expect(lastScheduledDelayMs()).toBe(300_000);
	});

	it('collapses repeated visibility flips inside thirty seconds into one fetch', async () => {
		visibility = 'hidden';
		vi.mocked(http.get).mockResolvedValue(reply(200, CANARY_ENVELOPE, {etag: 'W/"v7"'}));
		ExperimentAssignments.start();
		await settle();
		setVisibility('visible');
		await settle();
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(1);
		setVisibility('hidden');
		await vi.advanceTimersByTimeAsync(29_000);
		setVisibility('visible');
		await settle();
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(1);
		setVisibility('hidden');
		await vi.advanceTimersByTimeAsync(2_000);
		setVisibility('visible');
		await settle();
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(2);
	});
});

describe('ExperimentAssignments lifecycle', () => {
	it.each(['stop', 'reset'] as const)(
		'aborts an active request on %s and starts a fresh request immediately',
		async (method) => {
			const stale = deferredReply();
			ExperimentAssignments.start();
			await settle();
			const signal = vi.mocked(http.get).mock.calls[0]?.[1]?.signal;
			expect(signal).toBeDefined();
			expect(signal!.aborted).toBe(false);

			ExperimentAssignments[method]();
			expect(signal!.aborted).toBe(true);
			vi.mocked(http.get).mockResolvedValue(reply(200, CANARY_ENVELOPE, {etag: 'W/"fresh"'}));
			ExperimentAssignments.start();
			await settle();
			expect(http.get).toHaveBeenCalledTimes(2);
			expect(ExperimentAssignments.response).toEqual(CANARY_ENVELOPE);
			expect(vi.getTimerCount()).toBe(1);

			stale.resolve(reply(200, INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE, {etag: 'W/"stale"'}));
			await settle();
			expect(ExperimentAssignments.response).toEqual(CANARY_ENVELOPE);
			expect(vi.getTimerCount()).toBe(1);
		},
	);

	it('keeps ownership of the replacement request when the stale request finishes first', async () => {
		const stale = deferredReply();
		ExperimentAssignments.start();
		ExperimentAssignments.stop();
		const replacement = deferredReply();
		ExperimentAssignments.start();
		const signal = vi.mocked(http.get).mock.calls[1]?.[1]?.signal;
		expect(signal).toBeDefined();

		stale.resolve(reply(200, CANARY_ENVELOPE));
		await settle();
		await vi.advanceTimersByTimeAsync(31_000);
		setVisibility('hidden');
		setVisibility('visible');
		await settle();
		expect(http.get).toHaveBeenCalledTimes(2);
		expect(signal!.aborted).toBe(false);

		ExperimentAssignments.stop();
		expect(signal!.aborted).toBe(true);
		replacement.resolve(reply(200, CANARY_ENVELOPE));
		await settle();
		expect(ExperimentAssignments.response).toBe(INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE);
		expect(vi.getTimerCount()).toBe(0);
	});

	it('does not keep the etag or the backoff of the previous session across a reset', async () => {
		vi.spyOn(Math, 'random').mockReturnValue(0.5);
		await adopt(CANARY_ENVELOPE);
		vi.mocked(http.get).mockResolvedValue(reply(500, {message: '500: Internal Server Error'}));
		await vi.advanceTimersByTimeAsync(400_000);
		expect(lastScheduledDelayMs()).toBe(600_000);
		ExperimentAssignments.reset();
		vi.mocked(http.get).mockResolvedValue(reply(200, CANARY_ENVELOPE, {etag: 'W/"fresh"'}));
		ExperimentAssignments.start();
		await settle();
		expect(requestHeaders(2)['If-None-Match']).toBeUndefined();
		expect(lastScheduledDelayMs()).toBe(300_000);
	});
});

describe('ExperimentAssignments endpoint failures', () => {
	it('backs off while the session is rejected', async () => {
		vi.spyOn(Math, 'random').mockReturnValue(0.5);
		await adopt(CANARY_ENVELOPE);
		vi.mocked(http.get).mockResolvedValue(reply(401, {message: '401: Unauthorized'}));
		await vi.advanceTimersByTimeAsync(300_000);
		expect(lastScheduledDelayMs()).toBe(600_000);
		await vi.advanceTimersByTimeAsync(600_000);
		expect(lastScheduledDelayMs()).toBe(1_200_000);
		await vi.advanceTimersByTimeAsync(1_200_000);
		expect(lastScheduledDelayMs()).toBe(1_200_000);
		expect(ExperimentAssignments.response).toEqual(CANARY_ENVELOPE);
	});

	it('stops polling for the session after three consecutive 404s', async () => {
		vi.spyOn(Math, 'random').mockReturnValue(0.5);
		vi.mocked(http.get).mockResolvedValue(reply(404, {message: '404: Not Found'}));
		ExperimentAssignments.start();
		await settle();
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(1);
		await vi.advanceTimersByTimeAsync(600_000);
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(2);
		await vi.advanceTimersByTimeAsync(1_200_000);
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(3);
		expect(vi.getTimerCount()).toBe(0);
		await vi.advanceTimersByTimeAsync(10_000_000);
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(3);
		ExperimentAssignments.start();
		await settle();
		setVisibility('visible');
		await settle();
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(3);
		expect(ExperimentAssignments.response).toBe(INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE);
	});
});

describe('ExperimentAssignments account transitions', () => {
	it('clears the previous account assignments and etag on an account switch', async () => {
		vi.mocked(http.get).mockResolvedValue(reply(200, CANARY_ENVELOPE, {etag: 'W/"a"'}));
		ExperimentAssignments.start('a');
		await settle();
		expect(migrationEnabled()).toBe(true);
		ResettableStates.prepareForAccountTransition('account-switch');
		expect(ExperimentAssignments.response).toBe(INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE);
		expect(ExperimentAssignments.ownerId).toBeNull();
		expect(vi.getTimerCount()).toBe(0);
		vi.mocked(http.get).mockResolvedValue(reply(200, CANARY_ENVELOPE, {etag: 'W/"b"'}));
		ExperimentAssignments.start('b');
		await settle();
		expect(ExperimentAssignments.ownerId).toBe('b');
		expect(requestHeaders(1)['If-None-Match']).toBeUndefined();
	});

	it('polls the next account after the previous instance disabled the endpoint', async () => {
		vi.mocked(http.get).mockResolvedValue(reply(404, {message: '404: Not Found'}));
		ExperimentAssignments.start('a');
		await settle();
		await vi.advanceTimersByTimeAsync(600_000);
		await vi.advanceTimersByTimeAsync(1_200_000);
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(3);
		ResettableStates.prepareForAccountTransition('account-switch');
		vi.mocked(http.get).mockResolvedValue(reply(200, CANARY_ENVELOPE, {etag: 'W/"b"'}));
		ExperimentAssignments.start('b');
		await settle();
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(4);
		expect(migrationEnabled()).toBe(true);
	});

	it('ignores a response for the previous account that lands after the switch', async () => {
		const pending = deferredReply();
		ExperimentAssignments.start('a');
		await settle();
		ResettableStates.prepareForAccountTransition('account-switch');
		pending.resolve(reply(200, CANARY_ENVELOPE, {etag: 'W/"a"'}));
		await settle();
		expect(ExperimentAssignments.response).toBe(INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE);
	});
});
