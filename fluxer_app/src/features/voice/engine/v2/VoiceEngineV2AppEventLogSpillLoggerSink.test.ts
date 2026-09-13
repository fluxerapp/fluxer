// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createVoiceEngineV2AppEventLogSpillLoggerSink,
	type VoiceEngineV2AppEventLogSpillLogger,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppEventLogSpillLoggerSink';
import type {VoiceEngineV2EventLogEntry} from '@fluxer/voice_engine_v2';
import {describe, expect, it} from 'vitest';

function makeEntry(sequence: number, atMs: number = sequence * 10): VoiceEngineV2EventLogEntry {
	return {
		sequence,
		atMs,
		event: {
			type: 'inboundVideo.frameReceived',
			frame: {
				participantSid: 'p',
				trackSid: 't',
				width: 16,
				height: 16,
				timestampUs: sequence,
			},
		},
		commands: [],
	};
}

describe('VoiceEngineV2AppEventLogSpillLoggerSink', () => {
	it('logs the first evicted entry under the eventLogEvicted code', async () => {
		const warnings: Array<Record<string, unknown>> = [];
		const logger: VoiceEngineV2AppEventLogSpillLogger = {
			warn(payload) {
				warnings.push(payload);
			},
		};
		const sink = createVoiceEngineV2AppEventLogSpillLoggerSink({logger});

		const entry = makeEntry(7);
		await sink.write(entry);

		expect(warnings).toHaveLength(1);
		expect(warnings[0]).toEqual({
			code: 'eventLogEvicted',
			entry,
			evictedEntryCount: 1,
			suppressedEvictedEntryCount: 0,
		});
		expect(sink.swallowedErrorCount).toBe(0);
	});

	it('summarizes repeated evictions instead of warning for every frame', async () => {
		let nowMs = 1_000;
		const warnings: Array<Record<string, unknown>> = [];
		const logger: VoiceEngineV2AppEventLogSpillLogger = {
			warn(payload) {
				warnings.push(payload);
			},
		};
		const sink = createVoiceEngineV2AppEventLogSpillLoggerSink({
			logger,
			warnIntervalMs: 100,
			nowMs: () => nowMs,
		});

		const first = makeEntry(1);
		const second = makeEntry(2);
		const third = makeEntry(3);
		const fourth = makeEntry(4);

		await sink.write(first);
		nowMs = 1_050;
		await sink.write(second);
		nowMs = 1_099;
		await sink.write(third);
		nowMs = 1_100;
		await sink.write(fourth);

		expect(warnings).toHaveLength(2);
		expect(warnings[0]).toEqual({
			code: 'eventLogEvicted',
			entry: first,
			evictedEntryCount: 1,
			suppressedEvictedEntryCount: 0,
		});
		expect(warnings[1]).toEqual({
			code: 'eventLogEvicted',
			entry: fourth,
			evictedEntryCount: 4,
			suppressedEvictedEntryCount: 2,
		});
		expect(sink.swallowedErrorCount).toBe(0);
	});

	it('swallows logger errors and reports them through onLoggerError without rejecting the promise', async () => {
		const failures: Array<unknown> = [];
		const loggerError = new Error('logger offline');
		let nowMs = 1_000;
		const logger: VoiceEngineV2AppEventLogSpillLogger = {
			warn() {
				throw loggerError;
			},
		};
		const sink = createVoiceEngineV2AppEventLogSpillLoggerSink({
			logger,
			warnIntervalMs: 100,
			nowMs: () => nowMs,
			onLoggerError: (error) => failures.push(error),
		});

		await expect(sink.write(makeEntry(1))).resolves.toBeUndefined();
		nowMs = 1_100;
		await expect(sink.write(makeEntry(2))).resolves.toBeUndefined();

		expect(sink.swallowedErrorCount).toBe(2);
		expect(failures).toEqual([loggerError, loggerError]);
	});

	it('rejects malformed entries before reaching the logger', () => {
		const warnings: Array<Record<string, unknown>> = [];
		const logger: VoiceEngineV2AppEventLogSpillLogger = {
			warn(payload) {
				warnings.push(payload);
			},
		};
		const sink = createVoiceEngineV2AppEventLogSpillLoggerSink({logger});

		expect(() => sink.write({...makeEntry(0)})).toThrow(/sequence must be >= 1/);
		expect(warnings).toEqual([]);
	});
});
