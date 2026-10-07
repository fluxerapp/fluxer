// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfig';
import type {LoginResult, LoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import {
	ForegroundGatewayConnectionRecoverableError,
	ForegroundGatewayRecoveryCause,
} from '@app/features/gateway/transport/ForegroundGatewayConnectionFailure';
import {act, createElement} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});
vi.mock('@lingui/react/macro', () => ({
	useLingui: () => ({
		i18n: {
			_: (descriptor: {message?: string}, values?: Record<string, string>) =>
				(descriptor.message ?? '').replace(/\{(\w+)\}/g, (_match, key: string) => values?.[key] ?? ''),
		},
	}),
}));

const harness = vi.hoisted(() => ({
	loginWithPassword: vi.fn<() => Promise<LoginResult>>(),
}));

vi.mock(import('@app/features/auth/state/AuthFlow'), async (importOriginal) => ({
	...(await importOriginal()),
	loginWithPassword: harness.loginWithPassword,
}));

const {useLoginFormController, LOGIN_CONNECTING_STATUS_DELAY_MS} = await import(
	'@app/features/auth/hooks/useLoginFlow'
);

type Controller = ReturnType<typeof useLoginFormController>;

(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;

const RUNTIME_SNAPSHOT = {
	apiEndpoint: 'https://chat.example.test/api',
	webAppEndpoint: 'https://chat.example.test/',
	features: {},
} as unknown as RuntimeConfigSnapshot;

const SUCCESS: LoginResult = {
	type: 'success',
	payload: {token: 'token', userId: '42'} as LoginSuccessPayload,
};

let container: HTMLDivElement;
let root: Root;
let controller: Controller | null = null;

function Probe({
	onLoginSuccess,
	runtimeSnapshot = RUNTIME_SNAPSHOT,
}: {
	onLoginSuccess: (payload: LoginSuccessPayload) => Promise<void>;
	runtimeSnapshot?: RuntimeConfigSnapshot;
}) {
	controller = useLoginFormController({
		runtimeSnapshot,
		onLoginSuccess,
		onDesktopPasskeyHandoff: () => {},
	});
	return null;
}

function current(): Controller {
	if (controller === null) {
		throw new Error('controller not mounted');
	}
	return controller;
}

async function mount(onLoginSuccess: (payload: LoginSuccessPayload) => Promise<void>): Promise<void> {
	await act(async () => {
		root.render(createElement(Probe, {onLoginSuccess}));
	});
	await act(async () => {
		current().form.setValue('email', 'ada@example.test');
		current().form.setValue('password', 'hunter22');
	});
}

async function submit(): Promise<{done: Promise<void>}> {
	let done: Promise<void> = Promise.resolve();
	await act(async () => {
		done = current().form.handleSubmit();
	});
	return {done};
}

describe('password login while the account waits for the live connection', () => {
	beforeEach(() => {
		vi.useFakeTimers({toFake: ['setTimeout', 'clearTimeout']});
		harness.loginWithPassword.mockReset().mockResolvedValue(SUCCESS);
		container = document.createElement('div');
		document.body.append(container);
		root = createRoot(container);
	});

	afterEach(async () => {
		await act(async () => root.unmount());
		container.remove();
		controller = null;
		vi.useRealTimers();
	});

	test('stays pending, ignores resubmits and shows connecting progress until ready', async () => {
		let resolveReady: () => void = () => {};
		const ready = new Promise<void>((resolve) => {
			resolveReady = resolve;
		});
		await mount(() => ready);
		const {done} = await submit();
		expect(current().isLoading).toBe(true);
		expect(current().connectingMessage).toBeNull();
		await submit();
		expect(harness.loginWithPassword).toHaveBeenCalledTimes(1);
		await act(async () => {
			vi.advanceTimersByTime(LOGIN_CONNECTING_STATUS_DELAY_MS);
		});
		expect(current().isLoading).toBe(true);
		expect(current().connectingMessage).toBe('Connecting to chat.example.test…');
		await act(async () => {
			resolveReady();
			await done;
		});
		expect(current().isLoading).toBe(false);
		expect(current().connectingMessage).toBeNull();
	});

	test('surfaces a connection error and allows a retry when ready never arrives', async () => {
		const onLoginSuccess = vi.fn(() =>
			Promise.reject(
				new ForegroundGatewayConnectionRecoverableError(
					'chat.example.test:42',
					ForegroundGatewayRecoveryCause.READINESS_TIMEOUT,
					new Error('timeout'),
				),
			),
		);
		await mount(onLoginSuccess);
		await act(async () => {
			await (await submit()).done;
		});
		expect(current().isLoading).toBe(false);
		expect(current().connectingMessage).toBeNull();
		expect(current().form.getError('email')).toBe("Couldn't connect to chat.example.test. Try again.");
		await act(async () => {
			await (await submit()).done;
		});
		expect(harness.loginWithPassword).toHaveBeenCalledTimes(2);
	});
});

describe('password login on an instance that signs in with usernames', () => {
	const USERNAME_SNAPSHOT = {
		...RUNTIME_SNAPSHOT,
		features: {account_identity: 'username'},
	} as unknown as RuntimeConfigSnapshot;

	beforeEach(() => {
		harness.loginWithPassword.mockReset().mockResolvedValue(SUCCESS);
		container = document.createElement('div');
		document.body.append(container);
		root = createRoot(container);
	});

	afterEach(async () => {
		await act(async () => root.unmount());
		container.remove();
		controller = null;
	});

	test('asks for the username and sends it as the login of the selected instance', async () => {
		await act(async () => {
			root.render(createElement(Probe, {onLoginSuccess: async () => {}, runtimeSnapshot: USERNAME_SNAPSHOT}));
		});
		expect(current().identifierField).toBe('login');
		await act(async () => {
			current().form.setValue('login', '  ada#0000 ');
			current().form.setValue('password', 'hunter22');
		});
		const {done} = await submit();
		await act(async () => {
			await done;
		});
		expect(harness.loginWithPassword).toHaveBeenCalledWith({
			login: 'ada#0000',
			password: 'hunter22',
			inviteCode: undefined,
			runtimeSnapshot: USERNAME_SNAPSHOT,
		});
	});

	test('keeps the email field for an instance that signs in with email', async () => {
		await act(async () => {
			root.render(createElement(Probe, {onLoginSuccess: async () => {}}));
		});
		expect(current().identifierField).toBe('email');
	});
});
