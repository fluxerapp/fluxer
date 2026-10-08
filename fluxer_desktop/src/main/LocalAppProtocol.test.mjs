// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import path from 'node:path';
import {describe, test} from 'node:test';
import {installElectronStub, REPOSITORY_ROOT} from './LocalAppTestSupport.test.mjs';

const DESKTOP_DIRECTORY = path.join(REPOSITORY_ROOT, 'fluxer_desktop');
const MODULE_URL = new URL('./LocalAppProtocol.ts', import.meta.url).href;

let appReady = true;
const registeredSchemes = [];
const handledSchemes = new Set();
let handleCalls = 0;

installElectronStub({
	app: {
		isReady: () => appReady,
		getAppPath: () => DESKTOP_DIRECTORY,
		getPath: () => path.join(DESKTOP_DIRECTORY, '.test-userdata'),
	},
	protocol: {
		registerSchemesAsPrivileged: (schemes) => {
			registeredSchemes.push(...schemes);
		},
		handle: (scheme) => {
			handleCalls += 1;
			handledSchemes.add(scheme);
		},
		unhandle: (scheme) => {
			handledSchemes.delete(scheme);
		},
		isProtocolHandled: (scheme) => handledSchemes.has(scheme),
	},
});

const bootstrapModule = await import(MODULE_URL);
const mainAppModule = await import(`${MODULE_URL}?instance=main-app`);
const {getDesktopOutboundHTTP} = await import('@electron/main/DesktopOutboundHTTP');

const REGISTERED_ORIGIN = 'http://127.0.0.1:7431';
const UNREGISTERED_ORIGIN = 'http://127.0.0.2:7432';

function runtimePlan(origin) {
	return {
		instanceKey: `${origin}/api`,
		document: {},
		selfHosted: true,
		desktopModulesEnabled: null,
		endpoints: {
			apiEndpoint: `${origin}/api`,
			apiPublicEndpoint: null,
			webAppEndpoint: null,
			mediaEndpoint: null,
			staticCdnEndpoint: null,
			uploadRelayEndpoint: null,
			gatewayEndpoint: null,
			inviteEndpoint: null,
			giftEndpoint: null,
		},
	};
}

describe('registerSchemes', () => {
	test('throws when the app is ready and the privileged scheme was never registered', () => {
		appReady = true;
		assert.throws(() => bootstrapModule.getDesktopLocalAppProtocol().registerSchemes(), {
			name: 'LocalAppProtocolSchemeRegistrationOrderError',
		});
		assert.equal(registeredSchemes.length, 0);
	});

	test('registers the privileged scheme once across repeated calls', () => {
		appReady = false;
		const protocolOwner = bootstrapModule.getDesktopLocalAppProtocol();
		protocolOwner.registerSchemes();
		protocolOwner.registerSchemes();
		assert.equal(registeredSchemes.length, 1);
		assert.equal(registeredSchemes[0].scheme, 'fluxer-app');
		assert.equal(registeredSchemes[0].privileges.standard, true);
		assert.equal(registeredSchemes[0].privileges.secure, true);
	});

	test('is a no-op once the app is ready', () => {
		appReady = true;
		bootstrapModule.getDesktopLocalAppProtocol().registerSchemes();
		assert.equal(registeredSchemes.length, 1);
	});

	test('is a no-op for a separate module instance that loaded after the app was ready', () => {
		appReady = true;
		mainAppModule.getDesktopLocalAppProtocol().registerSchemes();
		assert.equal(registeredSchemes.length, 1);
	});
});

describe('register', () => {
	test('takes ownership once and rejects a second owner', () => {
		appReady = true;
		const protocolOwner = bootstrapModule.getDesktopLocalAppProtocol();
		protocolOwner.register();
		assert.ok(handledSchemes.has('fluxer-app'));
		protocolOwner.register();
		assert.equal(handleCalls, 1);
		assert.throws(() => mainAppModule.getDesktopLocalAppProtocol().register(), {
			name: 'LocalAppProtocolHandlerOwnershipError',
		});
	});
});

describe('runtime plan admission', () => {
	test('a plan naming an origin instance discovery never registered cannot be activated or cached', () => {
		const protocolOwner = mainAppModule.getDesktopLocalAppProtocol();
		const plan = runtimePlan(UNREGISTERED_ORIGIN);
		const isUnregistered = {name: 'DesktopOutboundHTTPOriginNotRegisteredError'};

		assert.throws(() => protocolOwner.activateRuntimePlan(plan), isUnregistered);
		assert.equal(protocolOwner.getActivePlan(), null);
		assert.equal(protocolOwner.findPlanForRoute(plan.instanceKey), null);

		assert.throws(() => protocolOwner.cacheRuntimePlan(plan), isUnregistered);
		assert.equal(protocolOwner.findPlanForRoute(plan.instanceKey), null);
	});

	test('a plan whose origins were registered through discovery activates', async () => {
		await getDesktopOutboundHTTP().registerAnchoredOrigins({anchorOrigin: REGISTERED_ORIGIN, origins: []});
		const protocolOwner = mainAppModule.getDesktopLocalAppProtocol();
		const plan = runtimePlan(REGISTERED_ORIGIN);

		protocolOwner.activateRuntimePlan(plan);
		assert.equal(protocolOwner.getActivePlan(), plan);
		assert.equal(protocolOwner.findPlanForRoute(plan.instanceKey), plan);

		protocolOwner.deactivateRuntimePlan();
		assert.equal(protocolOwner.getActivePlan(), null);
	});
});
