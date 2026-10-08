// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {registerHooks} from 'node:module';
import {describe, test} from 'node:test';
import {installElectronStub, installTestModuleStub} from './LocalAppTestSupport.test.mjs';

const PROTOCOL_STATE_KEY = '__fluxerPasskeyProtocolTestState__';
const WEBAUTHN_STUB_URL = 'file:///fluxer-webauthn-passkey-test-stub.cjs';
const CEREMONY_STATE_KEY = '__fluxerPasskeyCeremonyTestState__';

const protocolState = {activePlan: null, cachedPlans: new Map()};
const ceremonyState = {calls: []};
globalThis[PROTOCOL_STATE_KEY] = protocolState;
globalThis[CEREMONY_STATE_KEY] = ceremonyState;

installTestModuleStub(
	'@electron/main/LocalAppProtocol',
	`const state = globalThis[${JSON.stringify(PROTOCOL_STATE_KEY)}];
export const getDesktopLocalAppProtocol = () => ({
	getActivePlan: () => state.activePlan,
	findPlanForRoute: (key) => (state.activePlan?.instanceKey === key ? state.activePlan : (state.cachedPlans.get(key) ?? null)),
});`,
);

registerHooks({
	resolve(specifier, context, nextResolve) {
		if (specifier === '@fluxer/webauthn') {
			return {shortCircuit: true, url: WEBAUTHN_STUB_URL};
		}
		return nextResolve(specifier, context);
	},
	load(url, context, nextLoad) {
		if (url === WEBAUTHN_STUB_URL) {
			return {
				shortCircuit: true,
				format: 'commonjs',
				source: `const state = globalThis[${JSON.stringify(CEREMONY_STATE_KEY)}];
const credential = {
	rawId: Buffer.from('credential-id'),
	response: Buffer.from(JSON.stringify({clientDataJSON: 'client-data'})),
	authenticatorAttachment: null,
};
module.exports = {
	isSupported: async () => true,
	get: async (options) => {
		state.calls.push(options);
		return credential;
	},
	create: async (options) => {
		state.calls.push(options);
		return credential;
	},
};`,
			};
		}
		return nextLoad(url, context);
	},
});

const handlers = new Map();
installElectronStub({
	ipcMain: {
		handle: (channel, listener) => handlers.set(channel, listener),
		removeHandler: (channel) => handlers.delete(channel),
	},
	BrowserWindow: {fromWebContents: () => null, getFocusedWindow: () => null},
});

const {registerPasskeyHandlers} = await import('./Passkeys.ts');

registerPasskeyHandlers();

const REQUEST_OPTIONS = {
	challenge: 'Y2hhbGxlbmdl',
	rpId: 'fluxer.app',
	allowCredentials: [],
};

const REGISTRATION_OPTIONS = {
	challenge: 'Y2hhbGxlbmdl',
	rp: {id: 'fluxer.app', name: 'Fluxer'},
	user: {id: 'dXNlcg', name: 'user', displayName: 'user'},
	pubKeyCredParams: [{type: 'public-key', alg: -7}],
};

function plan(apiEndpoint, webAppEndpoint) {
	return {
		instanceKey: apiEndpoint,
		document: {},
		selfHosted: false,
		desktopModulesEnabled: null,
		endpoints: {
			apiEndpoint,
			apiPublicEndpoint: apiEndpoint,
			webAppEndpoint,
			mediaEndpoint: null,
			staticCdnEndpoint: null,
			uploadRelayEndpoint: null,
			gatewayEndpoint: null,
			inviteEndpoint: null,
			giftEndpoint: null,
		},
	};
}

function localAppEvent() {
	const frame = {url: 'fluxer-app://app/'};
	return {senderFrame: frame, sender: {getURL: () => frame.url}};
}

async function authenticate(requestContext) {
	ceremonyState.calls = [];
	const handler = handlers.get('passkey-authenticate');
	assert.ok(handler != null);
	return await handler(localAppEvent(), REQUEST_OPTIONS, requestContext);
}

async function register(requestContext) {
	ceremonyState.calls = [];
	const handler = handlers.get('passkey-register');
	assert.ok(handler != null);
	return await handler(localAppEvent(), REGISTRATION_OPTIONS, requestContext);
}

describe('passkey ceremony origin', () => {
	test('an instance may only sign a ceremony for its own registrable site', async () => {
		protocolState.activePlan = plan('https://api.fluxer.app', 'https://web.fluxer.app');
		await authenticate();
		assert.equal(ceremonyState.calls[0]?.origin, 'https://web.fluxer.app');

		protocolState.activePlan = plan('http://localhost:8088/api', 'http://localhost:8088');
		await authenticate();
		assert.equal(ceremonyState.calls[0]?.origin, 'http://localhost:8088');
	});

	test('an instance may not name another site as the ceremony origin', async () => {
		protocolState.activePlan = plan('https://evil.example', 'https://web.fluxer.app');
		await assert.rejects(authenticate(), /UntrustedPasskeyCeremonyOriginError/u);
		assert.deepEqual(ceremonyState.calls, []);
	});

	test('a bare address or public suffix may not borrow another host', async () => {
		protocolState.activePlan = plan('https://10.0.0.1', 'https://20.0.0.1');
		await assert.rejects(authenticate(), /UntrustedPasskeyCeremonyOriginError/u);

		protocolState.activePlan = plan('https://api.fluxer.app', 'https://web.fluxer.app.');
		await assert.rejects(authenticate(), /UntrustedPasskeyCeremonyOriginError/u);

		assert.deepEqual(ceremonyState.calls, []);
	});

	test('sharing a public suffix is not sharing a site', async () => {
		for (const [api, webApp] of [
			['https://api.evil.co.uk', 'https://bank.co.uk'],
			['https://evil.github.io', 'https://victim.github.io'],
			['https://evil.vercel.app', 'https://victim.vercel.app'],
			['https://evil.pages.dev', 'https://victim.pages.dev'],
			['https://evil.fluxer.app', 'https://web.fluxer.app'],
		]) {
			protocolState.activePlan = plan(api, webApp);
			await assert.rejects(authenticate(), /UntrustedPasskeyCeremonyOriginError/u);
		}
		assert.deepEqual(ceremonyState.calls, []);
	});

	test('a ceremony without an active plan is refused', async () => {
		protocolState.activePlan = null;
		await assert.rejects(authenticate(), /requires a browser frame origin/u);
		assert.deepEqual(ceremonyState.calls, []);
	});

	test('a signed-out ceremony uses the plan of the instance that issued the options', async () => {
		protocolState.activePlan = null;
		const official = plan('https://api.fluxer.app', 'https://web.fluxer.app');
		protocolState.cachedPlans = new Map([[official.instanceKey, official]]);
		await authenticate({instanceKey: official.instanceKey});
		assert.equal(ceremonyState.calls[0]?.origin, 'https://web.fluxer.app');
		await register({instanceKey: official.instanceKey, pin: '1234'});
		assert.equal(ceremonyState.calls[0]?.origin, 'https://web.fluxer.app');
		assert.equal(ceremonyState.calls[0]?.pin, '1234');
		protocolState.cachedPlans = new Map();
	});

	test('the named instance wins over the active plan of another instance', async () => {
		protocolState.activePlan = plan('http://localhost:8088/api', 'http://localhost:8088');
		const official = plan('https://api.fluxer.app', 'https://web.fluxer.app');
		protocolState.cachedPlans = new Map([[official.instanceKey, official]]);
		await authenticate({instanceKey: official.instanceKey});
		assert.equal(ceremonyState.calls[0]?.origin, 'https://web.fluxer.app');
		protocolState.cachedPlans = new Map();
	});

	test('an unknown instance never falls back to the active plan', async () => {
		protocolState.activePlan = plan('https://api.fluxer.app', 'https://web.fluxer.app');
		protocolState.cachedPlans = new Map();
		await assert.rejects(
			authenticate({instanceKey: 'https://unknown.example/api'}),
			/requires a browser frame origin/u,
		);
		assert.deepEqual(ceremonyState.calls, []);
	});

	test('a keyed plan is still held to its own site', async () => {
		protocolState.activePlan = null;
		const hostile = plan('https://evil.example', 'https://web.fluxer.app');
		protocolState.cachedPlans = new Map([[hostile.instanceKey, hostile]]);
		await assert.rejects(authenticate({instanceKey: hostile.instanceKey}), /UntrustedPasskeyCeremonyOriginError/u);
		assert.deepEqual(ceremonyState.calls, []);
		protocolState.cachedPlans = new Map();
	});
});
