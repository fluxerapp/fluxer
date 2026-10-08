// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {runInAction} from 'mobx';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const runtime = vi.hoisted(() => ({state: null as unknown as {statusPageUrl: string}}));

vi.mock('@app/features/app/state/RuntimeConfig', async () => {
	const {observable: makeObservable} = await import('mobx');
	runtime.state = makeObservable({statusPageUrl: ''});
	return {default: runtime.state};
});

vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		debug = vi.fn();
		info = vi.fn();
		warn = vi.fn();
		error = vi.fn();
	},
}));

const {StatusPage} = await import('@app/features/user/state/StatusPage');

const OFFICIAL = 'https://status.official.test';
const SELF_HOSTED = 'https://status.selfhosted.test';

interface PendingSummary {
	url: string;
	resolve: (body: unknown) => void;
}

const pending: Array<PendingSummary> = [];

function incidentSummary(name: string): unknown {
	return {
		activeIncidents: [{id: name, name, status: 'INVESTIGATING', impact: 'MAJOROUTAGE', url: `${name}/i`}],
	};
}

function setStatusPageUrl(url: string): void {
	runInAction(() => {
		runtime.state.statusPageUrl = url;
	});
}

async function settle(): Promise<void> {
	for (let i = 0; i < 10; i++) {
		await Promise.resolve();
	}
}

beforeEach(() => {
	vi.useFakeTimers();
	vi.stubGlobal(
		'fetch',
		vi.fn(
			(url: string) =>
				new Promise((resolve) => {
					pending.push({
						url,
						resolve: (body) => resolve({ok: true, json: () => Promise.resolve(body)}),
					});
				}),
		),
	);
	setStatusPageUrl(OFFICIAL);
});

afterEach(() => {
	pending.splice(0);
	vi.unstubAllGlobals();
	vi.useRealTimers();
});

describe('StatusPage instance switches', () => {
	it('clears the previous instance incident when the next instance has no status page', async () => {
		const store = new StatusPage();
		store.startPolling();
		pending.shift()?.resolve(incidentSummary('official'));
		await settle();
		expect(store.incident?.name).toBe('official');
		setStatusPageUrl('');
		expect(store.incident).toBeNull();
		expect(store.scheduledMaintenance).toBeNull();
		expect(vi.getTimerCount()).toBe(0);
		store.stopPolling();
	});

	it('drops a fetch for the previous instance that lands after the switch', async () => {
		const store = new StatusPage();
		store.startPolling();
		const stale = pending.shift();
		expect(stale?.url).toBe(`${OFFICIAL}/summary.json`);
		setStatusPageUrl(SELF_HOSTED);
		const fresh = pending.shift();
		expect(fresh?.url).toBe(`${SELF_HOSTED}/summary.json`);
		stale?.resolve(incidentSummary('official'));
		await settle();
		expect(store.incident).toBeNull();
		fresh?.resolve(incidentSummary('selfhosted'));
		await settle();
		expect(store.incident?.name).toBe('selfhosted');
		store.stopPolling();
	});
});
