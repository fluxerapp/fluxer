// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	HARNESS_API_ENDPOINT,
	installHarnessBootstrap,
} from '@app/features/auth/state/__fixtures__/AccountSwitchHarness';
import type {MessageDescriptor} from '@lingui/core';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: MessageDescriptor) => descriptor}));

vi.mock('@app/features/ui/commands/ToastCommands', () => ({
	createToast: () => 'toast',
	destroyToast: () => undefined,
}));

const solveAltchaChallenge = vi.fn(async (challenge: {signature: string}) => `solved:${challenge.signature}`);

vi.mock('@app/features/auth/altcha/AltchaSolver', () => ({
	readAltchaChallenge: (body: unknown) => {
		const record = body as {captcha_provider?: string; altcha_challenge?: unknown};
		return record.captcha_provider === 'altcha' ? (record.altcha_challenge ?? null) : null;
	},
	solveAltchaChallenge: (challenge: {signature: string}) => solveAltchaChallenge(challenge),
}));

interface SentRequest {
	url: string;
	headers: Record<string, string>;
}

const replies: Array<{status: number; body: unknown}> = [];
const sent: Array<SentRequest> = [];

class StubXMLHttpRequest extends EventTarget {
	readonly upload = new EventTarget();
	private readonly headers: Record<string, string> = {};
	private url = '';
	status = 200;
	statusText = 'OK';
	responseText = '';
	response: unknown = '';
	responseType = '';
	timeout = 0;

	open(_method: string, url: string): void {
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

	send(): void {
		sent.push({url: this.url, headers: {...this.headers}});
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

installHarnessBootstrap();

await import('@app/features/auth/altcha/CaptchaInterceptor');
const {instanceRequest} = await import('@app/features/platform/transport/InstanceHTTP');
const {http} = await import('@app/features/platform/transport/RestTransport');

const FOREIGN_TARGET = {
	instanceKey: 'https://foreign.test/api',
	apiEndpoint: 'https://foreign.test/api',
	apiVersion: 1,
};

function captchaRequired(signature: string) {
	return {
		status: 400,
		body: {
			code: 'CAPTCHA_REQUIRED',
			captcha_provider: 'altcha',
			altcha_challenge: {parameters: {}, signature},
		},
	};
}

beforeEach(() => {
	vi.stubGlobal('XMLHttpRequest', StubXMLHttpRequest);
	replies.length = 0;
	sent.length = 0;
	solveAltchaChallenge.mockClear();
});

afterEach(() => {
	vi.unstubAllGlobals();
});

describe('CaptchaInterceptor on instance-targeted requests', () => {
	test('solves and retries against the targeted instance, not the active runtime', async () => {
		expect(http.matchesConfiguredRouting(HARNESS_API_ENDPOINT, FOREIGN_TARGET.apiVersion)).toBe(false);
		replies.push(captchaRequired('foreign-challenge'), {status: 200, body: {token: 'session'}});

		const reply = await instanceRequest<{token: string}>({
			method: 'POST',
			path: '/auth/register',
			target: FOREIGN_TARGET,
			body: {username: 'someone'},
			auth: 'none',
		});

		expect(reply.body).toEqual({token: 'session'});
		expect(solveAltchaChallenge).toHaveBeenCalledWith({parameters: {}, signature: 'foreign-challenge'});
		expect(sent.map((request) => request.url)).toEqual([
			'https://foreign.test/api/v1/auth/register',
			'https://foreign.test/api/v1/auth/register',
		]);
		expect(sent[0].headers['x-captcha-token']).toBeUndefined();
		expect(sent[1].headers['x-captcha-token']).toBe('solved:foreign-challenge');
		expect(sent[1].headers['x-captcha-type']).toBeUndefined();
		expect(sent[1].headers.authorization).toBeUndefined();
	});

	test('a second challenge from the targeted instance is solved once more on the same instance', async () => {
		replies.push(captchaRequired('first'), captchaRequired('second'), {status: 200, body: {}});

		await instanceRequest({method: 'POST', path: '/auth/login', target: FOREIGN_TARGET, body: {}, auth: 'none'});

		expect(solveAltchaChallenge.mock.calls.map(([challenge]) => challenge.signature)).toEqual(['first', 'second']);
		expect(sent.map((request) => request.url)).toEqual([
			'https://foreign.test/api/v1/auth/login',
			'https://foreign.test/api/v1/auth/login',
			'https://foreign.test/api/v1/auth/login',
		]);
		expect(sent[2].headers['x-captcha-token']).toBe('solved:second');
	});
});
