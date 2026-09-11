// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RestResponse} from '@app/features/platform/types/TransportTypes';
import type {VoiceNoiseSuppressionAssignmentResponse} from '@fluxer/schema/src/domains/admin/VoiceNoiseSuppressionSchemas';
import {
	type ExperimentAssignmentsResponse,
	INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE,
	readVoiceNoiseSuppressionAssignment,
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

const GUILD_ID = '1485064866382176262';

const CANARY_ASSIGNMENT: VoiceNoiseSuppressionAssignmentResponse = {
	enabled: true,
	config_version: 7,
	user_targeted: true,
	backend: 'rnnoise',
	source: 'canary',
	guild_overrides: [{guild_id: GUILD_ID, backend: 'speex'}],
	enabled_backends: ['none', 'speex', 'rnnoise', 'gtcrn'],
	allow_user_override: true,
	stereo_enabled: false,
	suppression_strength: 80,
};

const CANARY_ENVELOPE: ExperimentAssignmentsResponse = {
	poll_interval_seconds: 300,
	poll_jitter_percent: 15,
	assignments: {voice_noise_suppression: CANARY_ASSIGNMENT},
};

interface ScheduleRecorder {
	delays: Array<number>;
	restore: () => void;
}

function recordScheduledDelays(): ScheduleRecorder {
	const delays: Array<number> = [];
	const original = globalThis.setTimeout;
	const patched = (...args: Array<unknown>) => {
		const timeout = args[1];
		delays.push(typeof timeout === 'number' ? timeout : 0);
		return (original as unknown as (...callArgs: Array<unknown>) => NodeJS.Timeout)(...args);
	};
	globalThis.setTimeout = patched as unknown as typeof globalThis.setTimeout;
	return {
		delays,
		restore: () => {
			globalThis.setTimeout = original;
		},
	};
}

let recorder: ScheduleRecorder;
let visibility: DocumentVisibilityState = 'visible';
const unsubscribes: Array<() => void> = [];

function reply(status: number, body: unknown, headers: Record<string, string> = {}): RestResponse<unknown> {
	return {ok: status >= 200 && status < 300, status, statusText: '', headers, body};
}

function requestHeaders(index: number): Record<string, string> {
	return vi.mocked(http.get).mock.calls[index]?.[1]?.headers ?? {};
}

function lastScheduledDelayMs(): number {
	return recorder.delays[recorder.delays.length - 1] ?? Number.NaN;
}

function setVisibility(next: DocumentVisibilityState): void {
	visibility = next;
	document.dispatchEvent(new Event('visibilitychange'));
}

async function settle(): Promise<void> {
	for (let index = 0; index < 4; index++) {
		await vi.advanceTimersByTimeAsync(0);
	}
}

function subscribe(listener: () => void): () => void {
	const unsubscribe = ExperimentAssignments.subscribe(listener);
	unsubscribes.push(unsubscribe);
	return unsubscribe;
}

function voiceConfigVersion(): number {
	return readVoiceNoiseSuppressionAssignment(ExperimentAssignments.response).config_version;
}

function deferredReply(): {resolve: (response: RestResponse<unknown>) => void} {
	let resolve: (response: RestResponse<unknown>) => void = () => {};
	const pending = new Promise<RestResponse<unknown>>((settleWith) => {
		resolve = settleWith;
	});
	vi.mocked(http.get).mockReturnValue(pending);
	return {
		resolve: (response) => {
			resolve(response);
		},
	};
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
	recorder = recordScheduledDelays();
	vi.mocked(http.get).mockReset();
});

afterEach(() => {
	for (const unsubscribe of unsubscribes.splice(0)) {
		unsubscribe();
	}
	ExperimentAssignments.reset();
	recorder.restore();
	vi.useRealTimers();
	vi.restoreAllMocks();
});

describe('ExperimentAssignments cold start', () => {
	it('keeps the inert envelope when the endpoint is unreachable', async () => {
		vi.mocked(http.get).mockRejectedValue(new Error('network unreachable'));
		ExperimentAssignments.start();
		await settle();
		expect(ExperimentAssignments.response).toBe(INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE);
		expect(ExperimentAssignments.response.assignments.voice_noise_suppression).toBeUndefined();
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
	it('adopts a valid envelope', async () => {
		await adopt(CANARY_ENVELOPE);
		expect(ExperimentAssignments.response).toEqual(CANARY_ENVELOPE);
		expect(readVoiceNoiseSuppressionAssignment(ExperimentAssignments.response)).toEqual(CANARY_ASSIGNMENT);
	});

	it('requests the shared experiment endpoint', async () => {
		await adopt(CANARY_ENVELOPE);
		expect(vi.mocked(http.get).mock.calls[0]?.[0]).toBe('/experiments');
	});

	it('discards a malformed envelope and keeps the previous value', async () => {
		await adopt(CANARY_ENVELOPE);
		vi.mocked(http.get).mockResolvedValue(
			reply(200, {
				...CANARY_ENVELOPE,
				assignments: {voice_noise_suppression: {...CANARY_ASSIGNMENT, backend: 'telepathy'}},
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

	it('accepts an envelope that carries no voice noise suppression assignment', async () => {
		await adopt({poll_interval_seconds: 600, poll_jitter_percent: 0, assignments: {}});
		expect(ExperimentAssignments.response.assignments.voice_noise_suppression).toBeUndefined();
		expect(lastScheduledDelayMs()).toBe(600_000);
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
	it('takes the poll interval from the envelope and jitters it by fifteen percent', async () => {
		const observed: Array<number> = [];
		for (let run = 0; run < 64; run++) {
			ExperimentAssignments.reset();
			await adopt({...CANARY_ENVELOPE, poll_interval_seconds: 600});
			observed.push(lastScheduledDelayMs());
		}
		for (const delay of observed) {
			expect(delay).toBeGreaterThanOrEqual(510_000);
			expect(delay).toBeLessThanOrEqual(690_000);
		}
		expect(new Set(observed).size).toBeGreaterThan(1);
	});

	it('takes the jitter ratio from the envelope', async () => {
		const observed: Array<number> = [];
		for (let run = 0; run < 64; run++) {
			ExperimentAssignments.reset();
			await adopt({...CANARY_ENVELOPE, poll_interval_seconds: 600, poll_jitter_percent: 50});
			observed.push(lastScheduledDelayMs());
		}
		for (const delay of observed) {
			expect(delay).toBeGreaterThanOrEqual(300_000);
			expect(delay).toBeLessThanOrEqual(900_000);
		}
		expect(observed.some((delay) => delay < 510_000 || delay > 690_000)).toBe(true);
	});

	it('schedules an exact interval when the server disables jitter', async () => {
		for (let run = 0; run < 8; run++) {
			ExperimentAssignments.reset();
			await adopt({...CANARY_ENVELOPE, poll_interval_seconds: 600, poll_jitter_percent: 0});
			expect(lastScheduledDelayMs()).toBe(600_000);
		}
	});

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

	it('backs off on a server error the same way it does on a transport failure', async () => {
		vi.spyOn(Math, 'random').mockReturnValue(0.5);
		vi.mocked(http.get).mockResolvedValue(reply(500, {message: '500: Internal Server Error'}));
		ExperimentAssignments.start();
		await settle();
		expect(lastScheduledDelayMs()).toBe(600_000);
		await vi.advanceTimersByTimeAsync(600_000);
		expect(lastScheduledDelayMs()).toBe(1_200_000);
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

	it('fetches once the document becomes visible', async () => {
		visibility = 'hidden';
		vi.mocked(http.get).mockResolvedValue(reply(200, CANARY_ENVELOPE, {etag: 'W/"v7"'}));
		ExperimentAssignments.start();
		await settle();
		setVisibility('visible');
		await settle();
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(1);
		expect(ExperimentAssignments.response).toEqual(CANARY_ENVELOPE);
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

	it('stops listening for visibility changes after stop', async () => {
		visibility = 'hidden';
		vi.mocked(http.get).mockResolvedValue(reply(200, CANARY_ENVELOPE, {etag: 'W/"v7"'}));
		ExperimentAssignments.start();
		await settle();
		ExperimentAssignments.stop();
		setVisibility('visible');
		await settle();
		expect(vi.mocked(http.get)).not.toHaveBeenCalled();
	});
});

describe('ExperimentAssignments lifecycle', () => {
	it('discards a response that lands after reset', async () => {
		const deferred = deferredReply();
		ExperimentAssignments.start();
		await settle();
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(1);
		ExperimentAssignments.reset();
		deferred.resolve(reply(200, CANARY_ENVELOPE, {etag: 'W/"stale"'}));
		await settle();
		expect(ExperimentAssignments.response).toBe(INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE);
		expect(vi.getTimerCount()).toBe(0);
		await vi.advanceTimersByTimeAsync(2_000_000);
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(1);
	});

	it('does not carry the etag or the backoff of the previous session across a reset', async () => {
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

	it('discards a response that lands after stop and does not reschedule', async () => {
		const deferred = deferredReply();
		ExperimentAssignments.start();
		await settle();
		ExperimentAssignments.stop();
		deferred.resolve(reply(200, CANARY_ENVELOPE, {etag: 'W/"stale"'}));
		await settle();
		expect(ExperimentAssignments.response).toBe(INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE);
		expect(vi.getTimerCount()).toBe(0);
	});
});

describe('ExperimentAssignments subscribe', () => {
	it('fires once per envelope change and stops after unsubscribe', async () => {
		const seen: Array<number> = [];
		const unsubscribe = subscribe(() => seen.push(voiceConfigVersion()));
		await adopt(CANARY_ENVELOPE);
		expect(seen).toEqual([7]);
		vi.mocked(http.get).mockResolvedValue(reply(304, undefined));
		await vi.advanceTimersByTimeAsync(400_000);
		expect(seen).toEqual([7]);
		vi.mocked(http.get).mockResolvedValue(
			reply(
				200,
				{...CANARY_ENVELOPE, assignments: {voice_noise_suppression: {...CANARY_ASSIGNMENT, config_version: 8}}},
				{etag: 'W/"v8"'},
			),
		);
		await vi.advanceTimersByTimeAsync(400_000);
		expect(seen).toEqual([7, 8]);
		ExperimentAssignments.reset();
		expect(seen).toEqual([7, 8, 0]);
		unsubscribe();
		await adopt(CANARY_ENVELOPE);
		expect(seen).toEqual([7, 8, 0]);
	});

	it('does not fire when a malformed or rejected response is discarded', async () => {
		let calls = 0;
		subscribe(() => {
			calls += 1;
		});
		await adopt(CANARY_ENVELOPE);
		expect(calls).toBe(1);
		vi.mocked(http.get).mockResolvedValue(reply(200, {assignments: {}}));
		await vi.advanceTimersByTimeAsync(400_000);
		vi.mocked(http.get).mockResolvedValue(reply(401, {message: '401: Unauthorized'}));
		await vi.advanceTimersByTimeAsync(700_000);
		expect(calls).toBe(1);
	});

	it('keeps notifying the remaining listeners when one throws', async () => {
		const seen: Array<string> = [];
		subscribe(() => {
			seen.push('first');
			throw new Error('listener exploded');
		});
		subscribe(() => {
			seen.push('second');
		});
		await adopt(CANARY_ENVELOPE);
		expect(seen).toEqual(['first', 'second']);
		expect(ExperimentAssignments.response).toEqual(CANARY_ENVELOPE);
		expect(vi.getTimerCount()).toBe(1);
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

	it('backs off while the session is forbidden', async () => {
		vi.spyOn(Math, 'random').mockReturnValue(0.5);
		vi.mocked(http.get).mockResolvedValue(reply(403, {message: '403: Forbidden'}));
		ExperimentAssignments.start();
		await settle();
		expect(lastScheduledDelayMs()).toBe(600_000);
		await vi.advanceTimersByTimeAsync(600_000);
		expect(lastScheduledDelayMs()).toBe(1_200_000);
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

	it('keeps polling when a good response interrupts the 404 streak', async () => {
		vi.spyOn(Math, 'random').mockReturnValue(0.5);
		vi.mocked(http.get).mockResolvedValue(reply(404, {message: '404: Not Found'}));
		ExperimentAssignments.start();
		await settle();
		await vi.advanceTimersByTimeAsync(600_000);
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(2);
		vi.mocked(http.get).mockResolvedValue(reply(200, CANARY_ENVELOPE, {etag: 'W/"v7"'}));
		await vi.advanceTimersByTimeAsync(1_200_000);
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(3);
		vi.mocked(http.get).mockResolvedValue(reply(404, {message: '404: Not Found'}));
		await vi.advanceTimersByTimeAsync(300_000);
		await vi.advanceTimersByTimeAsync(600_000);
		expect(vi.mocked(http.get)).toHaveBeenCalledTimes(5);
		expect(vi.getTimerCount()).toBe(1);
	});
});
