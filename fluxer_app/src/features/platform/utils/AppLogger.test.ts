// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAccountTransitionAbortError} from '@app/features/platform/state/AccountTransitionAbort';
import {Logger, LogLevel} from '@app/features/platform/utils/AppLogger';
import {afterEach, describe, expect, test, vi} from 'vitest';

afterEach(() => {
	vi.restoreAllMocks();
});

describe('Logger', () => {
	test('logs an account transition abort at debug with a readable description', () => {
		const error = vi.spyOn(console, 'error').mockImplementation(() => {});
		const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
		const debug = vi.spyOn(console, 'debug').mockImplementation(() => {});
		const logger = new Logger('Test', LogLevel.Debug);

		logger.error('Premium state fetch failed', createAccountTransitionAbortError());
		logger.warn('Failed to refresh premium state after READY', createAccountTransitionAbortError());

		expect(error).not.toHaveBeenCalled();
		expect(warn).not.toHaveBeenCalled();
		expect(debug).toHaveBeenCalledTimes(2);
		expect(debug.mock.calls[0]?.slice(2)).toEqual([
			'Premium state fetch failed',
			'(cancelled by an account switch or logout)',
		]);
	});

	test('keeps other aborts at error level and names the DOMException', () => {
		const error = vi.spyOn(console, 'error').mockImplementation(() => {});
		const logger = new Logger('Test', LogLevel.Debug);

		logger.error('Failed to fetch messages:', new DOMException('Request aborted', 'AbortError'));

		expect(error).toHaveBeenCalledTimes(1);
		expect(error.mock.calls[0]?.slice(2)).toEqual(['Failed to fetch messages:', 'AbortError: Request aborted']);
	});

	test('passes ordinary errors through unchanged', () => {
		const error = vi.spyOn(console, 'error').mockImplementation(() => {});
		const logger = new Logger('Test', LogLevel.Debug);
		const failure = new Error('boom');

		logger.error('Failed:', failure);

		expect(error.mock.calls[0]?.slice(2)).toEqual(['Failed:', failure]);
	});
});
