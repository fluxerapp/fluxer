// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CaptureOutcome} from '@app/api/database/capture/CaptureRecords';

const NOT_APPLIED_RESPONSE_CODES = {
	unavailable: 0x1000,
	overloaded: 0x1001,
	isBootstrapping: 0x1002,
	syntaxError: 0x2000,
	unauthorized: 0x2100,
	invalid: 0x2200,
	configError: 0x2300,
	unprepared: 0x2500,
};

const DEFINITE_RESPONSE_CODES = new Set<number>(Object.values(NOT_APPLIED_RESPONSE_CODES));

const CONNECT_FAILURE_CODES = new Set(['ECONNREFUSED', 'EHOSTUNREACH', 'ENETUNREACH', 'ENOTFOUND', 'EAI_AGAIN']);

function errorName(err: unknown): string | undefined {
	return err instanceof Error ? err.name : undefined;
}

function neverSent(err: unknown): boolean {
	if (errorName(err) === 'BusyConnectionError') return true;
	const code = (err as {code?: unknown} | null)?.code;
	return typeof code === 'string' && CONNECT_FAILURE_CODES.has(code);
}

export function classifyWriteError(err: unknown): Extract<CaptureOutcome, 'failed' | 'unknown'> {
	const name = errorName(err);
	if (name === 'ResponseError') {
		const code = (err as {code?: unknown}).code;
		return typeof code === 'number' && DEFINITE_RESPONSE_CODES.has(code) ? 'failed' : 'unknown';
	}
	if (name === 'NoHostAvailableError') {
		const inner = Object.values((err as {innerErrors?: Record<string, unknown>}).innerErrors ?? {});
		return inner.every(neverSent) ? 'failed' : 'unknown';
	}
	return neverSent(err) ? 'failed' : 'unknown';
}
