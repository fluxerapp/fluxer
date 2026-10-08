// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {RestClient} from '@app/features/platform/transport/RestTransport';
import type {RestInterceptor, RestResponse} from '@app/features/platform/types/TransportTypes';
import type {MessageDescriptor} from '@lingui/core';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: MessageDescriptor) => descriptor}));

const desktopDocument = vi.hoisted(() => ({active: false}));

vi.mock('@app/features/platform/DesktopLocalAppRuntime', async (importOriginal) => {
	const actual = await importOriginal<typeof import('@app/features/platform/DesktopLocalAppRuntime')>();
	return {...actual, isDesktopLocalAppDocument: () => desktopDocument.active};
});

vi.mock('@app/features/ui/utils/NativeUtils', () => ({
	getElectronAPI: () => (desktopDocument.active ? {localAppUpload: {subscribe: () => () => {}}} : undefined),
}));

interface SentRequest {
	method: string;
	url: string;
	headers: Record<string, string>;
	body: unknown;
}

interface Reply {
	status: number;
	body: unknown;
}

const replies: Array<Reply> = [];
const sent: Array<SentRequest> = [];

class StubXMLHttpRequest extends EventTarget {
	readonly upload = new EventTarget();
	private readonly headers: Record<string, string> = {};
	private method = '';
	private url = '';
	status = 200;
	statusText = 'OK';
	responseText = '';
	response: unknown = '';
	responseType = '';
	timeout = 0;

	open(method: string, url: string): void {
		this.method = method;
		this.url = url;
	}

	setRequestHeader(name: string, value: string): void {
		this.headers[name.toLowerCase()] = value;
	}

	getAllResponseHeaders(): string {
		return 'content-type: application/json\r\n';
	}

	abort(): void {
		this.dispatchEvent(new Event('abort'));
		this.dispatchEvent(new Event('loadend'));
	}

	send(body: unknown): void {
		sent.push({method: this.method, url: this.url, headers: {...this.headers}, body});
		const reply = replies.shift() ?? {status: 200, body: {}};
		this.status = reply.status;
		this.statusText = reply.status >= 200 && reply.status < 300 ? 'OK' : 'Error';
		this.responseText = JSON.stringify(reply.body);
		this.response = this.responseText;
		queueMicrotask(() => {
			this.dispatchEvent(new Event('load'));
			this.dispatchEvent(new Event('loadend'));
		});
	}
}

const CAPTCHA_REPLY: Reply = {status: 400, body: {code: 'CAPTCHA_REQUIRED'}};
const SUDO_REQUIRED_REPLY: Reply = {status: 403, body: {code: 'SUDO_MODE_REQUIRED'}};

function captchaRetryingInterceptor(token: string): RestInterceptor & ReturnType<typeof vi.fn> {
	return vi.fn((reply: RestResponse, retry: (extra: Record<string, string>) => Promise<RestResponse>) => {
		if (reply.status !== 400) return undefined;
		return retry({'X-Captcha-Token': token});
	});
}

function captchaCalls(interceptor: ReturnType<typeof vi.fn>): number {
	return interceptor.mock.calls.filter(([reply]) => (reply as RestResponse).status === 400).length;
}

function createClient(): RestClient {
	const client = new RestClient();
	client.configure({baseUrl: 'https://active.test/api', apiVersion: 1});
	return client;
}

beforeEach(() => {
	vi.stubGlobal('XMLHttpRequest', StubXMLHttpRequest);
	replies.length = 0;
	sent.length = 0;
});

afterEach(() => {
	vi.unstubAllGlobals();
	desktopDocument.active = false;
});

describe('RestClient desktop local upload tagging', () => {
	const uploadIdHeader = 'x-fluxer-local-upload-id';

	test('a remote upload from the desktop app never carries the local upload id', async () => {
		desktopDocument.active = true;
		await createClient().put('https://uploads.fluxer.app/v1/relay/abc?t=token', {
			body: new Blob(['x']),
			auth: 'none',
			onProgress: () => {},
		});
		expect(sent[0].headers[uploadIdHeader]).toBeUndefined();
	});

	test('an upload through the desktop local proxy is tagged for progress', async () => {
		desktopDocument.active = true;
		await createClient().put('fluxer-app://app/_remote/key?url=https%3A%2F%2Fuploads.test%2Fv1%2Frelay%2Fabc', {
			body: new Blob(['x']),
			auth: 'none',
			onProgress: () => {},
		});
		expect(sent[0].headers[uploadIdHeader]).toMatch(/^[0-9a-f]{32}$/u);
	});
});

describe('RestClient interceptor selection', () => {
	test('uses the global interceptor when the request has none', async () => {
		const client = createClient();
		const global = captchaRetryingInterceptor('global-token');
		client.installHooks({intercept: global});
		replies.push(CAPTCHA_REPLY, {status: 200, body: {ok: true}});

		const reply = await client.post('/auth/login', {body: {}});

		expect(reply.status).toBe(200);
		expect(global).toHaveBeenCalledTimes(1);
		expect(sent[1].headers['x-captcha-token']).toBe('global-token');
	});

	test('a per-request interceptor replaces the global one', async () => {
		const client = createClient();
		const global = captchaRetryingInterceptor('global-token');
		const own = captchaRetryingInterceptor('own-token');
		client.installHooks({intercept: global});
		replies.push(CAPTCHA_REPLY, {status: 200, body: {}});

		await client.post('https://other.test/api/v1/auth/register', {body: {}, intercept: own, auth: 'none'});

		expect(global).not.toHaveBeenCalled();
		expect(own).toHaveBeenCalledTimes(1);
		expect(sent.map((request) => request.url)).toEqual([
			'https://other.test/api/v1/auth/register',
			'https://other.test/api/v1/auth/register',
		]);
		expect(sent[1].headers['x-captcha-token']).toBe('own-token');
	});

	test('the interceptor retry runs with no interceptor at all', async () => {
		const client = createClient();
		const global = captchaRetryingInterceptor('global-token');
		const own = captchaRetryingInterceptor('own-token');
		client.installHooks({intercept: global});
		replies.push(CAPTCHA_REPLY, CAPTCHA_REPLY);

		await expect(client.post('/auth/register', {body: {}, intercept: own})).rejects.toMatchObject({status: 400});

		expect(own).toHaveBeenCalledTimes(1);
		expect(global).not.toHaveBeenCalled();
		expect(sent).toHaveLength(2);
	});

	test('the sudo reissue keeps the per-request interceptor', async () => {
		const client = createClient();
		const global = captchaRetryingInterceptor('global-token');
		const own = captchaRetryingInterceptor('own-token');
		client.installHooks({intercept: global});
		client.installSudo({
			tokenProvider: () => null,
			tokenListener: () => undefined,
			invalidate: () => undefined,
			prompt: async () => ({password: 'hunter2'}),
			onFailure: () => undefined,
		});
		replies.push(SUDO_REQUIRED_REPLY, CAPTCHA_REPLY, {status: 200, body: {}});

		const reply = await client.post('/users/@me/disable', {body: {}, intercept: own});

		expect(reply.status).toBe(200);
		expect(global).not.toHaveBeenCalled();
		expect(captchaCalls(own)).toBe(1);
		expect(sent).toHaveLength(3);
		expect(JSON.parse(sent[1].body as string)).toEqual({password: 'hunter2'});
		expect(JSON.parse(sent[2].body as string)).toEqual({password: 'hunter2'});
		expect(sent[2].headers['x-captcha-token']).toBe('own-token');
	});

	test('the sudo reissue falls back to the global interceptor', async () => {
		const client = createClient();
		const global = captchaRetryingInterceptor('global-token');
		client.installHooks({intercept: global});
		client.installSudo({
			tokenProvider: () => null,
			tokenListener: () => undefined,
			invalidate: () => undefined,
			prompt: async () => ({password: 'hunter2'}),
			onFailure: () => undefined,
		});
		replies.push(SUDO_REQUIRED_REPLY, CAPTCHA_REPLY, {status: 200, body: {}});

		const reply = await client.post('/users/@me/disable', {body: {}});

		expect(reply.status).toBe(200);
		expect(captchaCalls(global)).toBe(1);
		expect(sent[2].headers['x-captcha-token']).toBe('global-token');
	});
});

describe('RestClient account transition aborts', () => {
	test('a request waiting out a retry backoff rejects with the account transition abort', async () => {
		const {AccountScopedWork, AccountScopedWorkTransitionReason} = await import(
			'@app/features/platform/state/AccountScopedWork'
		);
		const {isAccountTransitionAbortError} = await import('@app/features/platform/state/AccountTransitionAbort');
		const client = createClient();
		replies.push({status: 502, body: {}});

		const request = client.get('/users/@me', {retries: 3});
		const settled = request.then(
			() => null,
			(error: unknown) => error,
		);
		await vi.waitFor(() => expect(sent).toHaveLength(1));
		await AccountScopedWork.runSuspended(AccountScopedWorkTransitionReason.ACCOUNT_SWITCH, async () => {});

		const error = await settled;
		expect(isAccountTransitionAbortError(error)).toBe(true);
		expect(sent).toHaveLength(1);
	});
});
