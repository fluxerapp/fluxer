// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createVoiceEngineV2AppDiagnosticsAdapter,
	VOICE_ENGINE_V2_DIAGNOSTICS_CODE_MAX_LENGTH,
	VOICE_ENGINE_V2_DIAGNOSTICS_MESSAGE_MAX_LENGTH,
	type VoiceEngineV2AppDiagnosticsLogger,
	type VoiceEngineV2DiagnosticsLevel,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppDiagnosticsAdapter';
import {describe, expect, it} from 'vitest';

interface FakeLoggerCall {
	level: VoiceEngineV2DiagnosticsLevel;
	payload: Record<string, unknown>;
}

interface FakeLogger extends VoiceEngineV2AppDiagnosticsLogger {
	readonly calls: ReadonlyArray<FakeLoggerCall>;
}

function createFakeLogger(throwOn?: VoiceEngineV2DiagnosticsLevel, error?: unknown): FakeLogger {
	const calls: Array<FakeLoggerCall> = [];
	const make = (level: VoiceEngineV2DiagnosticsLevel) => (payload: Record<string, unknown>) => {
		calls.push({level, payload: {...payload}});
		if (throwOn === level) throw error ?? new Error('logger failure');
	};
	return {
		get calls() {
			return calls;
		},
		trace: make('trace'),
		debug: make('debug'),
		info: make('info'),
		warn: make('warn'),
		error: make('error'),
	};
}

describe('VoiceEngineV2AppDiagnosticsAdapter', () => {
	it('routes each known level to the matching logger method with code/message/detail payload', async () => {
		const logger = createFakeLogger();
		const adapter = createVoiceEngineV2AppDiagnosticsAdapter({logger});

		await adapter.log('trace', 'voice.trace', 'tick');
		await adapter.log('debug', 'voice.debug', 'detail', {a: 1});
		await adapter.log('info', 'voice.info', 'state changed');
		await adapter.log('warn', 'voice.warn', 'glitch');
		await adapter.log('error', 'voice.error', 'failure', {error: 'boom'});

		expect(logger.calls).toEqual([
			{level: 'trace', payload: {code: 'voice.trace', message: 'tick'}},
			{level: 'debug', payload: {code: 'voice.debug', message: 'detail', detail: {a: 1}}},
			{level: 'info', payload: {code: 'voice.info', message: 'state changed'}},
			{level: 'warn', payload: {code: 'voice.warn', message: 'glitch'}},
			{level: 'error', payload: {code: 'voice.error', message: 'failure', detail: {error: 'boom'}}},
		]);
	});

	it('normalises mixed-case level strings and falls back to the configured fallbackLevel for unknown levels', async () => {
		const logger = createFakeLogger();
		const adapter = createVoiceEngineV2AppDiagnosticsAdapter({logger, fallbackLevel: 'warn'});

		await adapter.log('INFO', 'voice.upper', 'message');
		await adapter.log('Fatal', 'voice.unknown', 'fell through');

		expect(logger.calls).toEqual([
			{level: 'info', payload: {code: 'voice.upper', message: 'message'}},
			{level: 'warn', payload: {code: 'voice.unknown', message: 'fell through'}},
		]);
	});

	it('rejects empty/oversized codes and oversized messages before the logger is touched', async () => {
		const logger = createFakeLogger();
		const adapter = createVoiceEngineV2AppDiagnosticsAdapter({logger});
		const oversizedCode = 'c'.repeat(VOICE_ENGINE_V2_DIAGNOSTICS_CODE_MAX_LENGTH + 1);
		const oversizedMessage = 'm'.repeat(VOICE_ENGINE_V2_DIAGNOSTICS_MESSAGE_MAX_LENGTH + 1);

		await expect(adapter.log('info', '', 'message')).rejects.toThrow(/must not be empty/);
		await expect(adapter.log('info', oversizedCode, 'message')).rejects.toThrow(/maximum length/);
		await expect(adapter.log('info', 'ok', oversizedMessage)).rejects.toThrow(/maximum length/);
		expect(logger.calls).toEqual([]);
	});

	it('swallows logger exceptions and reports them through onError without rejecting', async () => {
		const error = new Error('logger offline');
		const logger = createFakeLogger('warn', error);
		const errorEvents: Array<{code: string; error: unknown}> = [];
		const adapter = createVoiceEngineV2AppDiagnosticsAdapter({
			logger,
			onError: (code, e) => {
				errorEvents.push({code, error: e});
			},
		});

		await adapter.log('warn', 'voice.warn', 'glitch');

		expect(errorEvents).toEqual([{code: 'voice.warn', error}]);
	});

	it('emits the detail field only when the caller provides it (positive and negative space)', async () => {
		const logger = createFakeLogger();
		const adapter = createVoiceEngineV2AppDiagnosticsAdapter({logger});

		await adapter.log('info', 'voice.without_detail', 'no payload');
		await adapter.log('info', 'voice.with_detail', 'has payload', {ms: 12});

		expect(logger.calls[0]).toEqual({
			level: 'info',
			payload: {code: 'voice.without_detail', message: 'no payload'},
		});
		expect(logger.calls[1]).toEqual({
			level: 'info',
			payload: {code: 'voice.with_detail', message: 'has payload', detail: {ms: 12}},
		});
	});
});
