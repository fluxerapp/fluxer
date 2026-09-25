// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	PASSKEY_BRIDGE_REQUEST_TYPE,
	PASSKEY_BRIDGE_RESULT_TYPE,
	parsePasskeyBridgeRequest,
	parsePasskeyBridgeResult,
	resolvePasskeyBridgeLegacyOrigin,
	resolvePasskeyBridgeOpenerOrigin,
} from '@app/features/auth/utils/PasskeyBridgeProtocol';
import {describe, expect, it} from 'vitest';

function authenticateRequest(options: Record<string, unknown>): Record<string, unknown> {
	return {type: PASSKEY_BRIDGE_REQUEST_TYPE, v: 1, id: 'req', kind: 'authenticate', options};
}

function registerRequest(options: Record<string, unknown>): Record<string, unknown> {
	return {type: PASSKEY_BRIDGE_REQUEST_TYPE, v: 1, id: 'req', kind: 'register', options};
}

describe('resolvePasskeyBridgeOpenerOrigin', () => {
	it('pairs each official legacy origin with its target only', () => {
		expect(resolvePasskeyBridgeOpenerOrigin('https://web.fluxer.app', '/passkey-bridge')).toBe('https://fluxer.com');
		expect(resolvePasskeyBridgeOpenerOrigin('https://web.canary.fluxer.app', '/passkey-bridge')).toBe(
			'https://canary.fluxer.com',
		);
	});

	it('ignores other origins and paths', () => {
		expect(resolvePasskeyBridgeOpenerOrigin('https://fluxer.com', '/passkey-bridge')).toBeNull();
		expect(resolvePasskeyBridgeOpenerOrigin('https://fluxer.app', '/passkey-bridge')).toBeNull();
		expect(resolvePasskeyBridgeOpenerOrigin('https://chat.example.com', '/passkey-bridge')).toBeNull();
		expect(resolvePasskeyBridgeOpenerOrigin('https://web.fluxer.app', '/passkey-bridge/')).toBeNull();
		expect(resolvePasskeyBridgeOpenerOrigin('https://web.fluxer.app', '/login')).toBeNull();
	});
});

describe('resolvePasskeyBridgeLegacyOrigin', () => {
	it('resolves only from official target origins', () => {
		expect(resolvePasskeyBridgeLegacyOrigin('https://fluxer.com')).toBe('https://web.fluxer.app');
		expect(resolvePasskeyBridgeLegacyOrigin('https://canary.fluxer.com')).toBe('https://web.canary.fluxer.app');
		expect(resolvePasskeyBridgeLegacyOrigin('https://web.fluxer.app')).toBeNull();
		expect(resolvePasskeyBridgeLegacyOrigin('https://chat.example.com')).toBeNull();
	});
});

describe('parsePasskeyBridgeRequest', () => {
	it('accepts requests for the fluxer.app relying party', () => {
		expect(parsePasskeyBridgeRequest(authenticateRequest({challenge: 'c', rpId: 'fluxer.app'}))).not.toBeNull();
		expect(
			parsePasskeyBridgeRequest(registerRequest({challenge: 'c', rp: {id: 'fluxer.app', name: 'Fluxer'}, user: {}})),
		).not.toBeNull();
	});

	it('rejects any other relying party', () => {
		for (const rpId of ['evil.example', 'web.fluxer.app', 'fluxer.com', 'app', undefined]) {
			expect(parsePasskeyBridgeRequest(authenticateRequest({challenge: 'c', rpId}))).toBeNull();
			expect(parsePasskeyBridgeRequest(registerRequest({challenge: 'c', rp: {id: rpId}, user: {}}))).toBeNull();
		}
		expect(parsePasskeyBridgeRequest(registerRequest({challenge: 'c', rpId: 'fluxer.app', user: {}}))).toBeNull();
		expect(parsePasskeyBridgeRequest(authenticateRequest({challenge: 'c', rp: {id: 'fluxer.app'}}))).toBeNull();
	});

	it('rejects malformed envelopes', () => {
		const options = {challenge: 'c', rpId: 'fluxer.app'};
		expect(parsePasskeyBridgeRequest({...authenticateRequest(options), v: 2})).toBeNull();
		expect(parsePasskeyBridgeRequest({...authenticateRequest(options), type: 'other'})).toBeNull();
		expect(parsePasskeyBridgeRequest({...authenticateRequest(options), id: ''})).toBeNull();
		expect(parsePasskeyBridgeRequest({...authenticateRequest(options), kind: 'sign'})).toBeNull();
		expect(parsePasskeyBridgeRequest(authenticateRequest({rpId: 'fluxer.app'}))).toBeNull();
		expect(parsePasskeyBridgeRequest('request')).toBeNull();
	});
});

describe('parsePasskeyBridgeResult', () => {
	const response = {id: 'cred', rawId: 'cred', response: {}, type: 'public-key', clientExtensionResults: {}};

	it('accepts only the matching request id', () => {
		const result = {type: PASSKEY_BRIDGE_RESULT_TYPE, v: 1, id: 'req', ok: true, response};
		expect(parsePasskeyBridgeResult(result, 'req')).not.toBeNull();
		expect(parsePasskeyBridgeResult(result, 'other')).toBeNull();
	});

	it('normalises failures and rejects malformed successes', () => {
		expect(
			parsePasskeyBridgeResult({type: PASSKEY_BRIDGE_RESULT_TYPE, v: 1, id: 'req', ok: false, error: {}}, 'req'),
		).toMatchObject({ok: false, error: {name: 'UnknownError', message: ''}});
		expect(
			parsePasskeyBridgeResult({type: PASSKEY_BRIDGE_RESULT_TYPE, v: 1, id: 'req', ok: true, response: {}}, 'req'),
		).toBeNull();
	});
});
