// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

interface Entry {
	url: string;
	state: unknown;
}

class SessionHistory extends EventTarget {
	entries: Array<Entry> = [{url: 'http://localhost/channels/@me', state: null}];
	index = 0;
	readonly location = {
		get href(): string {
			return session.entries[session.index].url;
		},
	};
	readonly history = {
		get state(): unknown {
			return session.entries[session.index].state;
		},
		pushState: (state: unknown, _title: string, url: URL | string) => {
			this.entries.splice(this.index + 1, this.entries.length, {url: String(url), state});
			this.index += 1;
		},
		replaceState: (state: unknown, _title: string, url: URL | string) => {
			this.entries[this.index] = {url: String(url), state};
		},
		go: (delta: number) => this.traverse(delta),
		back: () => this.traverse(-1),
	};

	private traverse(delta: number): void {
		setTimeout(() => {
			const target = this.index + delta;
			if (target < 0 || target >= this.entries.length) return;
			this.index = target;
			this.dispatchEvent(new Event('popstate'));
		}, 0);
	}
}

let session: SessionHistory;

beforeEach(() => {
	vi.resetModules();
	session = new SessionHistory();
	vi.stubGlobal('window', session);
});

afterEach(() => {
	vi.unstubAllGlobals();
});

async function settle(): Promise<void> {
	await new Promise((resolve) => setTimeout(resolve, 5));
}

describe('browser history', () => {
	test('a replace issued while a back traversal is in flight lands after it', async () => {
		const {createBrowserHistory} = await import('@app/features/platform/components/router/RouterHistory');
		const history = createBrowserHistory();
		history.push(new URL('http://localhost/channels/@me'), {modal: 'switcher'});

		history.back();
		history.replace(new URL('http://localhost/channels/1/2'));
		await settle();

		expect(session.index).toBe(0);
		expect(session.location.href).toBe('http://localhost/channels/1/2');
		expect(session.history.state).toBeNull();
	});

	test('a push issued while a back traversal is in flight lands after it', async () => {
		const {createBrowserHistory} = await import('@app/features/platform/components/router/RouterHistory');
		const history = createBrowserHistory();
		history.push(new URL('http://localhost/channels/@me'), {modal: 'switcher'});

		history.back();
		history.push(new URL('http://localhost/channels/1/2'));
		await settle();

		expect(session.entries.map((entry) => entry.url)).toEqual([
			'http://localhost/channels/@me',
			'http://localhost/channels/1/2',
		]);
		expect(session.index).toBe(1);
	});

	test('a back traversal with nowhere to go does not hold later navigation forever', async () => {
		const {createBrowserHistory} = await import('@app/features/platform/components/router/RouterHistory');
		const history = createBrowserHistory();

		history.back();
		history.replace(new URL('http://localhost/channels/1/2'));
		await new Promise((resolve) => setTimeout(resolve, 600));

		expect(session.location.href).toBe('http://localhost/channels/1/2');
	});

	test('listeners hear the deferred navigation', async () => {
		const {createBrowserHistory} = await import('@app/features/platform/components/router/RouterHistory');
		const history = createBrowserHistory();
		const heard: Array<string> = [];
		history.listen((location) => heard.push(location.url.pathname));
		history.push(new URL('http://localhost/channels/@me'), {modal: 'switcher'});
		heard.length = 0;

		history.back();
		history.replace(new URL('http://localhost/channels/1/2'));
		await settle();

		expect(heard.at(-1)).toBe('/channels/1/2');
	});
});
