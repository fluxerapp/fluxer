// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {DiagnosticsPort} from '@fluxer/voice_engine_v2';

const VOICE_ENGINE_V2_DIAGNOSTICS_CODE_MAX_LENGTH = 128;
const VOICE_ENGINE_V2_DIAGNOSTICS_MESSAGE_MAX_LENGTH = 4096;

type VoiceEngineV2DiagnosticsLevel = 'trace' | 'debug' | 'info' | 'warn' | 'error';

const KNOWN_LEVELS: ReadonlySet<VoiceEngineV2DiagnosticsLevel> = new Set(['trace', 'debug', 'info', 'warn', 'error']);

export interface VoiceEngineV2AppDiagnosticsLogger {
	trace(payload: Record<string, unknown>): void;
	debug(payload: Record<string, unknown>): void;
	info(payload: Record<string, unknown>): void;
	warn(payload: Record<string, unknown>): void;
	error(payload: Record<string, unknown>): void;
}

export interface VoiceEngineV2AppDiagnosticsAdapterOptions {
	logger: VoiceEngineV2AppDiagnosticsLogger;
	fallbackLevel?: VoiceEngineV2DiagnosticsLevel;
	onError?: (code: string, error: unknown) => void;
}

function normaliseLevel(level: string, fallback: VoiceEngineV2DiagnosticsLevel): VoiceEngineV2DiagnosticsLevel {
	assert.equal(typeof level, 'string', 'diagnostics level must be a string');
	const lower = level.toLowerCase();
	if (KNOWN_LEVELS.has(lower as VoiceEngineV2DiagnosticsLevel)) {
		return lower as VoiceEngineV2DiagnosticsLevel;
	}
	return fallback;
}

function dispatch(
	logger: VoiceEngineV2AppDiagnosticsLogger,
	level: VoiceEngineV2DiagnosticsLevel,
	payload: Record<string, unknown>,
): void {
	switch (level) {
		case 'trace':
			logger.trace(payload);
			return;
		case 'debug':
			logger.debug(payload);
			return;
		case 'info':
			logger.info(payload);
			return;
		case 'warn':
			logger.warn(payload);
			return;
		case 'error':
			logger.error(payload);
			return;
		default: {
			const exhaustive: never = level;
			assert.fail(`voice-engine-v2 diagnostics adapter received an unhandled level: ${String(exhaustive)}`);
		}
	}
}

export function createVoiceEngineV2AppDiagnosticsAdapter(
	options: VoiceEngineV2AppDiagnosticsAdapterOptions,
): DiagnosticsPort {
	assert.ok(options !== null && typeof options === 'object', 'voice-engine-v2 diagnostics adapter requires options');
	assert.ok(options.logger !== null && typeof options.logger === 'object', 'diagnostics adapter requires a logger');
	const fallback = options.fallbackLevel ?? 'info';
	assert.ok(KNOWN_LEVELS.has(fallback), 'diagnostics adapter fallbackLevel must be a known level');
	const logger = options.logger;
	const onError = options.onError;
	return {
		async log(level: string, code: string, message: string, detail?: unknown): Promise<void> {
			assert.equal(typeof code, 'string', 'diagnostics code must be a string');
			assert.ok(code.length > 0, 'diagnostics code must not be empty');
			assert.ok(
				code.length <= VOICE_ENGINE_V2_DIAGNOSTICS_CODE_MAX_LENGTH,
				'diagnostics code exceeds the maximum length',
			);
			assert.equal(typeof message, 'string', 'diagnostics message must be a string');
			assert.ok(
				message.length <= VOICE_ENGINE_V2_DIAGNOSTICS_MESSAGE_MAX_LENGTH,
				'diagnostics message exceeds the maximum length',
			);
			const resolvedLevel = normaliseLevel(level, fallback);
			const payload: Record<string, unknown> = detail === undefined ? {code, message} : {code, message, detail};
			try {
				dispatch(logger, resolvedLevel, payload);
			} catch (error) {
				onError?.(code, error);
			}
		},
	};
}
