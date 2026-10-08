// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {installHarnessBootstrap} from '@app/features/auth/state/__fixtures__/AccountSwitchHarness';
import type {DesktopHandoffAPI, DesktopHandoffSession} from '@fluxer/desktop_ipc/src/BrowserHandoffContract';
import {act, createElement, type ReactNode} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});
vi.mock('@lingui/react/macro', () => ({
	Trans: ({children}: {children?: ReactNode}) => children ?? null,
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? ''}}),
}));
vi.mock('@app/features/ui/commands/TextCopyCommands', () => ({
	copy: (...args: Array<unknown>) => harness.copy(...args),
}));
vi.mock('@app/features/auth/hooks/useAuthForm', () => ({
	getAuthErrorMessage: (error: unknown) => (error instanceof Error ? error.message : 'error'),
}));
vi.mock('@app/features/platform/transport/InstanceHTTP', () => ({
	instanceTargetFromSnapshot: (snapshot: {apiEndpoint: string; apiCodeVersion: number}) => ({
		instanceKey: snapshot.apiEndpoint,
		apiEndpoint: snapshot.apiEndpoint,
		apiVersion: snapshot.apiCodeVersion,
	}),
}));

const harness = vi.hoisted(() => ({
	initiateDesktopHandoff: vi.fn(),
	pollDesktopHandoffStatus: vi.fn(),
	navigateToExternalURL: vi.fn(async (_url: string) => {}),
	copy: vi.fn(async (..._args: Array<unknown>) => true),
}));

vi.mock('@app/features/auth/commands/AuthenticationCommands', () => ({
	initiateDesktopHandoff: harness.initiateDesktopHandoff,
	pollDesktopHandoffStatus: harness.pollDesktopHandoffStatus,
	authResponseUserToUserData: (user?: {username?: string} | null) =>
		user == null ? undefined : {username: user.username, discriminator: '0001'},
}));
vi.mock(import('@app/features/ui/utils/NativeUtils'), async (importOriginal) => ({
	...(await importOriginal()),
	navigateToExternalURL: harness.navigateToExternalURL,
}));

installHarnessBootstrap();

const {useBrowserLoginHandoff} = await import('@app/features/auth/flow/browser_handoff/useBrowserLoginHandoff');
const {BrowserLoginHandoffTransportKind, resolveBrowserLoginHandoffTransport} = await import(
	'@app/features/auth/flow/browser_handoff/BrowserLoginHandoffTransport'
);

type Controller = ReturnType<typeof useBrowserLoginHandoff>;

(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;

const RUNTIME_SNAPSHOT = {
	apiEndpoint: 'https://self.hosted.example/api',
	apiCodeVersion: 1,
	webAppEndpoint: 'https://self.hosted.example/',
	features: {},
} as unknown as Parameters<typeof useBrowserLoginHandoff>[0]['runtimeSnapshot'];

const CODE = 'ABCDEF-123456';
const COMPLETED_USER = {username: 'ada', discriminator: '0001', global_name: 'Ada', avatar: null, email: null};

function installElectron(api: unknown): void {
	Object.defineProperty(window, 'electron', {value: api, configurable: true, writable: true});
}

function createDesktopHandoffAPI(): DesktopHandoffAPI & {
	readonly initiated: Array<unknown>;
	readonly polled: Array<string>;
} {
	const initiated: Array<unknown> = [];
	const polled: Array<string> = [];
	let completed = false;
	return {
		initiated,
		polled,
		initiate: async (instance) => {
			initiated.push(instance);
			const session: DesktopHandoffSession = {
				instance,
				code: CODE,
				expiresAt: new Date(Date.now() + 300_000).toISOString(),
				returnMethod: 'deep_link',
			};
			return session;
		},
		status: async (code) => {
			polled.push(code);
			if (!completed) {
				completed = true;
				return {status: 'pending'};
			}
			return {status: 'completed', token: 'ipc-token', userId: '77', user: COMPLETED_USER};
		},
	};
}

let container: HTMLDivElement;
let root: Root;
let controller: Controller | null = null;

function Probe({
	onEnded,
	onSuccess,
	runtimeSnapshot,
}: {
	onEnded: () => void;
	onSuccess: (payload: {token: string; userId: string}) => Promise<void> | void;
	runtimeSnapshot: typeof RUNTIME_SNAPSHOT;
}) {
	controller = useBrowserLoginHandoff({
		runtimeSnapshot,
		onEnded,
		onSuccess,
	});
	return null;
}

function requireController(): Controller {
	if (controller == null) {
		throw new Error('The browser handoff hook was never mounted');
	}
	return controller;
}

async function mountProbe(
	onSuccess: (payload: {token: string; userId: string}) => Promise<void> | void,
	runtimeSnapshot: typeof RUNTIME_SNAPSHOT = RUNTIME_SNAPSHOT,
	onEnded: () => void = () => {},
): Promise<void> {
	await act(async () => {
		root.render(createElement(Probe, {onEnded, onSuccess, runtimeSnapshot}));
	});
}

async function advancePollInterval(): Promise<void> {
	await act(async () => {
		await vi.advanceTimersByTimeAsync(2100);
	});
}

beforeEach(() => {
	vi.useFakeTimers();
	controller = null;
	harness.initiateDesktopHandoff.mockReset();
	harness.pollDesktopHandoffStatus.mockReset();
	harness.navigateToExternalURL.mockClear();
	harness.copy.mockReset();
	harness.copy.mockResolvedValue(true);
	harness.initiateDesktopHandoff.mockResolvedValue({
		code: CODE,
		expires_at: new Date(Date.now() + 300_000).toISOString(),
		poll_secret: 'server-poll-secret',
	});
	harness.pollDesktopHandoffStatus
		.mockResolvedValueOnce({status: 'pending'})
		.mockResolvedValue({status: 'completed', token: 'renderer-token', user_id: '42', user: COMPLETED_USER});
	installElectron(undefined);
	container = document.createElement('div');
	document.body.append(container);
	root = createRoot(container);
});

afterEach(() => {
	act(() => {
		root.unmount();
	});
	container.remove();
	vi.useRealTimers();
});

describe('browser handoff transport selection', () => {
	test('picks the renderer transport on a deployed shell whose preload has no desktopHandoff', () => {
		installElectron({platform: 'darwin'});

		expect(resolveBrowserLoginHandoffTransport().kind).toBe(BrowserLoginHandoffTransportKind.RENDERER);
	});

	test('picks the renderer transport when the shell exposes a partial desktopHandoff namespace', () => {
		installElectron({platform: 'darwin', desktopHandoff: {status: () => Promise.resolve({status: 'pending'})}});

		expect(resolveBrowserLoginHandoffTransport().kind).toBe(BrowserLoginHandoffTransportKind.RENDERER);
	});

	test('picks the renderer transport on the web, where there is no window.electron at all', () => {
		installElectron(undefined);

		expect(resolveBrowserLoginHandoffTransport().kind).toBe(BrowserLoginHandoffTransportKind.RENDERER);
	});

	test('picks the IPC transport once the shell exposes the whole desktopHandoff namespace', () => {
		installElectron({platform: 'darwin', desktopHandoff: createDesktopHandoffAPI()});

		expect(resolveBrowserLoginHandoffTransport().kind).toBe(BrowserLoginHandoffTransportKind.IPC);
	});
});

describe('browser sign-in against a deployed desktop shell', () => {
	test('completes over the renderer transport when window.electron carries no desktopHandoff', async () => {
		installElectron({platform: 'darwin'});
		const succeeded: Array<{token: string; userId: string}> = [];
		await mountProbe((payload) => {
			succeeded.push(payload);
		});

		await act(async () => {
			await requireController().openBrowser();
		});

		expect(harness.initiateDesktopHandoff).toHaveBeenCalledTimes(1);
		expect(harness.navigateToExternalURL).toHaveBeenCalledTimes(1);
		expect(harness.navigateToExternalURL.mock.calls[0]?.[0]).toBe(
			`https://self.hosted.example/login?handoff=1&code=${encodeURIComponent(CODE)}&api=${encodeURIComponent('https://self.hosted.example')}`,
		);

		await advancePollInterval();
		await advancePollInterval();

		expect(succeeded).toEqual([
			{token: 'renderer-token', userId: '42', userData: {username: 'ada', discriminator: '0001'}},
		]);
	});

	test('falls back to the unauthenticated status read against an instance that issues no poll secret', async () => {
		installElectron({platform: 'darwin'});
		harness.initiateDesktopHandoff.mockResolvedValue({
			code: CODE,
			expires_at: new Date(Date.now() + 300_000).toISOString(),
		});
		await mountProbe(() => {});

		await act(async () => {
			await requireController().openBrowser();
		});
		await advancePollInterval();

		expect(harness.pollDesktopHandoffStatus).toHaveBeenCalledTimes(1);
		expect(harness.pollDesktopHandoffStatus.mock.calls[0]?.[1]).toBeNull();
	});

	test('polls with the secret the instance issued rather than the unauthenticated fallback', async () => {
		installElectron({platform: 'darwin'});
		await mountProbe(() => {});

		await act(async () => {
			await requireController().openBrowser();
		});
		await advancePollInterval();

		expect(harness.pollDesktopHandoffStatus).toHaveBeenCalledTimes(1);
		expect(harness.pollDesktopHandoffStatus.mock.calls[0]).toEqual([
			CODE,
			'server-poll-secret',
			{
				instanceKey: 'https://self.hosted.example/api',
				apiEndpoint: 'https://self.hosted.example/api',
				apiVersion: 1,
			},
		]);
	});
});

describe('browser sign-in against an updated desktop shell', () => {
	test('routes the whole ceremony through the main process and completes', async () => {
		const api = createDesktopHandoffAPI();
		installElectron({platform: 'darwin', desktopHandoff: api});
		const succeeded: Array<{token: string; userId: string}> = [];
		await mountProbe((payload) => {
			succeeded.push(payload);
		});

		await act(async () => {
			await requireController().openBrowser();
		});

		expect(api.initiated).toEqual([
			{apiEndpoint: 'https://self.hosted.example/api', apiVersion: 1, webAppEndpoint: 'https://self.hosted.example'},
		]);
		expect(harness.initiateDesktopHandoff).not.toHaveBeenCalled();
		expect(requireController().session?.pollSecret).toBeNull();

		await advancePollInterval();
		await advancePollInterval();

		expect(api.polled).toEqual([CODE, CODE]);
		expect(harness.pollDesktopHandoffStatus).not.toHaveBeenCalled();
		expect(succeeded).toEqual([{token: 'ipc-token', userId: '77', userData: {username: 'ada', discriminator: '0001'}}]);
	});
});

describe('the handoff code the user is asked to type', () => {
	test('claims it was copied only when the clipboard actually took it', async () => {
		installElectron({platform: 'darwin'});
		await mountProbe(() => {});
		await act(async () => {
			await requireController().showManualCode();
		});

		harness.copy.mockResolvedValue(false);
		await act(async () => {
			requireController().copyCode();
		});

		expect(harness.copy).toHaveBeenCalledTimes(1);
		expect(requireController().copied).toBe(false);

		harness.copy.mockResolvedValue(true);
		await act(async () => {
			requireController().copyCode();
		});

		expect(requireController().copied).toBe(true);
	});

	test('reports itself expired once the countdown runs out, before the poll notices', async () => {
		installElectron({platform: 'darwin'});
		harness.initiateDesktopHandoff.mockResolvedValue({
			code: CODE,
			expires_at: new Date(Date.now() + 5_000).toISOString(),
			poll_secret: 'server-poll-secret',
		});
		harness.pollDesktopHandoffStatus.mockReset();
		harness.pollDesktopHandoffStatus.mockResolvedValue({status: 'pending'});
		await mountProbe(() => {});
		await act(async () => {
			await requireController().showManualCode();
		});

		expect(requireController().isExpired).toBe(false);

		await act(async () => {
			await vi.advanceTimersByTimeAsync(5_200);
		});

		expect(requireController().isExpired).toBe(true);
		expect(requireController().remainingSeconds).toBe(0);
		expect(requireController().hasCode).toBe(true);
	});

	test('mints a fresh code on demand rather than handing back the dead one', async () => {
		installElectron({platform: 'darwin'});
		await mountProbe(() => {});
		await act(async () => {
			await requireController().showManualCode();
		});
		expect(harness.initiateDesktopHandoff).toHaveBeenCalledTimes(1);

		harness.initiateDesktopHandoff.mockResolvedValue({
			code: 'ZZZZZZ999999',
			expires_at: new Date(Date.now() + 300_000).toISOString(),
			poll_secret: 'second-secret',
		});
		await act(async () => {
			await requireController().regenerateCode();
		});

		expect(harness.initiateDesktopHandoff).toHaveBeenCalledTimes(2);
		expect(requireController().code).toBe('ZZZZZZ999999');

		await advancePollInterval();

		expect(harness.pollDesktopHandoffStatus.mock.calls.at(-1)?.[1]).toBe('second-secret');
	});
});

describe('an instance that publishes no web address', () => {
	test('is reported as unable to start, and no code is burned finding out', async () => {
		installElectron({platform: 'darwin'});
		const snapshot = {
			apiEndpoint: 'https://self.hosted.example/api',
			apiCodeVersion: 1,
			webAppEndpoint: '',
			features: {},
		} as unknown as typeof RUNTIME_SNAPSHOT;

		await mountProbe(() => {}, snapshot);

		expect(requireController().canStart).toBe(false);
		expect(harness.initiateDesktopHandoff).not.toHaveBeenCalled();
	});

	test('still reports a reachable instance as able to start', async () => {
		installElectron({platform: 'darwin'});
		await mountProbe(() => {});

		expect(requireController().canStart).toBe(true);
	});
});

describe('when the instance stops answering the status poll', () => {
	test('stops the spinner instead of waiting forever, and resumes on request', async () => {
		installElectron({platform: 'darwin'});
		harness.pollDesktopHandoffStatus.mockReset();
		harness.pollDesktopHandoffStatus.mockRejectedValue(new Error('network down'));
		await mountProbe(() => {});
		await act(async () => {
			await requireController().openBrowser();
		});

		await advancePollInterval();
		await advancePollInterval();
		await advancePollInterval();

		expect(harness.pollDesktopHandoffStatus).toHaveBeenCalledTimes(3);
		expect(requireController().hasStoppedWaiting).toBe(true);

		await advancePollInterval();
		expect(harness.pollDesktopHandoffStatus).toHaveBeenCalledTimes(3);

		await act(async () => {
			requireController().resumeWaiting();
		});
		expect(requireController().hasStoppedWaiting).toBe(false);

		await advancePollInterval();
		expect(harness.pollDesktopHandoffStatus).toHaveBeenCalledTimes(4);
	});
});

describe('when finishing the approved sign-in fails', () => {
	test('stops the spinner and lets the same session be retried instead of reporting a poll error', async () => {
		installElectron({platform: 'darwin'});
		harness.pollDesktopHandoffStatus.mockReset();
		harness.pollDesktopHandoffStatus.mockResolvedValue({
			status: 'completed',
			token: 'renderer-token',
			user_id: '42',
			user: COMPLETED_USER,
		});
		let failCompletion = true;
		const attempts: Array<{token: string; userId: string}> = [];
		await mountProbe(async (payload) => {
			attempts.push(payload);
			if (failCompletion) {
				throw new Error('storing the account failed');
			}
		});
		await act(async () => {
			await requireController().openBrowser();
		});

		await advancePollInterval();

		expect(attempts).toHaveLength(1);
		expect(requireController().hasStoppedWaiting).toBe(true);
		expect(requireController().error).toBe('storing the account failed');

		await advancePollInterval();
		expect(harness.pollDesktopHandoffStatus).toHaveBeenCalledTimes(1);

		failCompletion = false;
		await act(async () => {
			requireController().resumeWaiting();
		});
		await advancePollInterval();

		expect(harness.pollDesktopHandoffStatus).toHaveBeenCalledTimes(2);
		expect(attempts).toHaveLength(2);
		expect(requireController().hasStoppedWaiting).toBe(false);
	});
});

describe('when the instance reports the code as dead', () => {
	test('stops polling, clears the session and reports the expiry exactly once', async () => {
		installElectron({platform: 'darwin'});
		harness.pollDesktopHandoffStatus.mockReset();
		harness.pollDesktopHandoffStatus.mockResolvedValue({status: 'expired'});
		let expiredCount = 0;
		await mountProbe(
			() => {},
			RUNTIME_SNAPSHOT,
			() => {
				expiredCount += 1;
			},
		);
		await act(async () => {
			await requireController().openBrowser();
		});
		expect(requireController().hasCode).toBe(true);

		await advancePollInterval();

		expect(expiredCount).toBe(1);
		expect(harness.pollDesktopHandoffStatus).toHaveBeenCalledTimes(1);
		expect(requireController().session).toBeNull();
		expect(requireController().code).toBeNull();
		expect(requireController().error).toBe('The sign-in request expired before your browser approved it.');

		await advancePollInterval();
		await advancePollInterval();

		expect(harness.pollDesktopHandoffStatus).toHaveBeenCalledTimes(1);
		expect(expiredCount).toBe(1);
	});
});
