// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, test, vi} from 'vitest';

const events: Array<string> = [];
const state = {desktop: true, resolveFails: false};

vi.mock('@app/features/app/state/RuntimeConfig', () => ({default: {}}));
vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: {create: () => ({warn: () => events.push('warn')})},
}));
vi.mock('@app/features/platform/DesktopLocalAppRuntime', () => ({
	isDesktopLocalAppDocument: () => state.desktop,
	desktopLocalApiEndpoint: (instanceKey: string) => `fluxer-app://app/api/${encodeURIComponent(instanceKey)}`,
}));
vi.mock('@app/features/app/state/InstanceSnapshotStore', () => ({
	runtimeInstanceKey: ({apiEndpoint}: {apiEndpoint: string}) => apiEndpoint.replace(/\/+$/u, ''),
	default: {
		resolve: async ({input}: {input: string}) => {
			events.push(`resolve ${input}`);
			if (state.resolveFails) {
				throw new Error('discovery unreachable');
			}
			return {};
		},
	},
}));
vi.mock('@app/features/platform/transport/RestTransport', () => ({
	http: {
		matchesConfiguredRouting: (apiEndpoint: string) => apiEndpoint === 'https://active.example/api',
		dispatch: async (method: string, path: string) => {
			events.push(`${method} ${path}`);
			return {ok: true, status: 200, headers: {}, body: {}};
		},
	},
}));

const {instanceRequest} = await import('@app/features/platform/transport/InstanceHTTP');

const BACKGROUND = {
	instanceKey: 'https://background.example/api',
	apiEndpoint: 'https://background.example/api',
	apiVersion: 1,
};
const ACTIVE = {instanceKey: 'https://active.example/api', apiEndpoint: 'https://active.example/api', apiVersion: 1};

describe('desktop instance requests after a restart', () => {
	beforeEach(() => {
		events.length = 0;
		state.desktop = true;
		state.resolveFails = false;
	});

	test('a request to a non-active instance first gives the main process its runtime plan', async () => {
		await instanceRequest({method: 'GET', path: '/users/@me', target: BACKGROUND});
		expect(events).toEqual([
			'resolve https://background.example/api',
			'GET fluxer-app://app/api/https%3A%2F%2Fbackground.example%2Fapi/v1/users/@me',
		]);
	});

	test('the active instance and the web build skip the extra resolution', async () => {
		await instanceRequest({method: 'GET', path: '/users/@me', target: ACTIVE});
		state.desktop = false;
		await instanceRequest({method: 'GET', path: '/users/@me', target: BACKGROUND});
		expect(events).toEqual(['GET /users/@me', 'GET https://background.example/api/v1/users/@me']);
	});

	test('an unreachable discovery still sends the request so its own failure is reported', async () => {
		state.resolveFails = true;
		await instanceRequest({method: 'GET', path: '/users/@me', target: BACKGROUND});
		expect(events).toEqual([
			'resolve https://background.example/api',
			'warn',
			'GET fluxer-app://app/api/https%3A%2F%2Fbackground.example%2Fapi/v1/users/@me',
		]);
	});
});
