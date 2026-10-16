// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveInstanceLabel} from '@app/features/auth/flow/instance_selector/InstanceDirectoryStorage';
import {isOfficialInstanceHost} from '@fluxer/instance_bootstrap/src/OfficialInstance';
import {describe, expect, test} from 'vitest';

describe('resolveInstanceLabel', () => {
	test('a self-hosted instance that reports the official product name is labelled by its host', () => {
		expect(resolveInstanceLabel('Fluxer', 'http://localhost:48090')).toBe('localhost:48090');
		expect(resolveInstanceLabel(' fluxer ', 'chat.example.com/fluxer')).toBe('chat.example.com');
	});
});

describe('isOfficialInstanceHost', () => {
	test('an official API endpoint with a path is official', () => {
		expect(isOfficialInstanceHost('https://web.canary.fluxer.app/api')).toBe(true);
		expect(isOfficialInstanceHost('https://canary.fluxer.com/api/')).toBe(true);
		expect(isOfficialInstanceHost('https://web.fluxer.app')).toBe(true);
	});

	test('other hosts are not official whatever the path', () => {
		expect(isOfficialInstanceHost('http://localhost:48090/api')).toBe(false);
		expect(isOfficialInstanceHost('https://fluxer.app.example.com/api')).toBe(false);
		expect(isOfficialInstanceHost('https://example.com/web.fluxer.app')).toBe(false);
		expect(isOfficialInstanceHost('https://web.fluxer.app/api?x=1')).toBe(false);
	});
});
