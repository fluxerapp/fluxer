// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveInstanceLabel} from '@app/features/auth/flow/instance_selector/InstanceDirectoryStorage';
import {describe, expect, test} from 'vitest';

describe('resolveInstanceLabel', () => {
	test('a self-hosted instance that reports the official product name is labelled by its host', () => {
		expect(resolveInstanceLabel('Fluxer', 'http://localhost:48090')).toBe('localhost:48090');
		expect(resolveInstanceLabel(' fluxer ', 'chat.example.com/fluxer')).toBe('chat.example.com');
	});

	test('a self-hosted instance with its own name keeps it', () => {
		expect(resolveInstanceLabel('Example Chat', 'chat.example.com')).toBe('Example Chat');
	});

	test('a missing name falls back to the host', () => {
		expect(resolveInstanceLabel(null, 'chat.example.com')).toBe('chat.example.com');
		expect(resolveInstanceLabel('localhost:48090/api', 'localhost:48090/api')).toBe('localhost:48090');
	});

	test('the official instance keeps the official name', () => {
		expect(resolveInstanceLabel('Fluxer', 'web.fluxer.app')).toBe('Fluxer');
		expect(resolveInstanceLabel(null, 'fluxer.app')).toBe('Fluxer');
	});
});
