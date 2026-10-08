// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ElectronAPI} from '@app/features/platform/types/Electron';
import {createDesktopModuleRequest} from '@app/features/platform/utils/DesktopModuleRequest';
import type {DesktopModuleEnsureResult, DesktopModuleEnsureStatus} from '@fluxer/desktop_ipc/src/ModuleContract';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const MODULE_NAME = 'fluxer_twemoji';
const BACKOFF_MAX_MS = 30_000;

type EnsureSpy = ReturnType<typeof vi.fn<(module: string) => Promise<DesktopModuleEnsureResult>>>;

function installElectron(api: Partial<ElectronAPI>): void {
	Object.defineProperty(window, 'electron', {value: api as ElectronAPI, configurable: true, writable: true});
}

function installEnsure(ensure: (module: string) => Promise<DesktopModuleEnsureResult>): EnsureSpy {
	const spy = vi.fn(ensure);
	installElectron({desktopModules: {ensure: spy}});
	return spy;
}

function settled(status: DesktopModuleEnsureStatus): (module: string) => Promise<DesktopModuleEnsureResult> {
	return (module: string) => Promise.resolve({module, status});
}

beforeEach(() => {
	vi.useFakeTimers();
	Reflect.deleteProperty(window, 'electron');
});

afterEach(() => {
	vi.useRealTimers();
});

describe('createDesktopModuleRequest', () => {
	it('settles ready and notifies once when there is no desktop shell', async () => {
		const request = createDesktopModuleRequest(MODULE_NAME);
		const listener = vi.fn();
		request.subscribe(listener);

		await expect(request.ensure()).resolves.toBeNull();

		expect(request.isSettled()).toBe(true);
		expect(listener).toHaveBeenCalledTimes(1);
	});

	it('holds off while the module is unavailable, then upgrades on the scheduled retry', async () => {
		let call = 0;
		const ensure = installEnsure((module) => {
			call += 1;
			return Promise.resolve({module, status: call === 1 ? 'unavailable' : 'installed'});
		});
		const request = createDesktopModuleRequest(MODULE_NAME);
		const listener = vi.fn();
		request.subscribe(listener);

		await expect(request.ensure()).resolves.toBe('unavailable');
		expect(request.isSettled()).toBe(false);
		expect(listener).toHaveBeenCalled();
		const notificationsBeforeRetry = listener.mock.calls.length;

		await vi.advanceTimersByTimeAsync(BACKOFF_MAX_MS);

		expect(ensure).toHaveBeenCalledTimes(2);
		expect(request.isSettled()).toBe(true);
		expect(listener.mock.calls.length).toBeGreaterThan(notificationsBeforeRetry);
		await expect(request.ensure()).resolves.toBeNull();
	});

	it('recovers on the scheduled retry after the shell rejects the request', async () => {
		let call = 0;
		const ensure = installEnsure((module) => {
			call += 1;
			if (call === 1) {
				return Promise.reject(new Error('ipc down'));
			}
			return Promise.resolve({module, status: 'installed'});
		});
		const request = createDesktopModuleRequest(MODULE_NAME);

		await expect(request.ensure()).resolves.toBeNull();
		expect(request.isSettled()).toBe(false);

		await vi.advanceTimersByTimeAsync(BACKOFF_MAX_MS);

		expect(ensure).toHaveBeenCalledTimes(2);
		expect(request.isSettled()).toBe(true);
		await expect(request.ensure()).resolves.toBeNull();
	});

	it('treats ready as terminal and never re-enters loading', async () => {
		const ensure = installEnsure(settled('installed'));
		const request = createDesktopModuleRequest(MODULE_NAME);

		await expect(request.ensure()).resolves.toBe('installed');
		await expect(request.ensure()).resolves.toBeNull();
		await expect(request.ensure()).resolves.toBeNull();

		expect(ensure).toHaveBeenCalledTimes(1);
		expect(request.isSettled()).toBe(true);
	});

	it('clears the armed retry timer on reset so no further load fires', async () => {
		const ensure = installEnsure(settled('unavailable'));
		const request = createDesktopModuleRequest(MODULE_NAME);

		await expect(request.ensure()).resolves.toBe('unavailable');
		expect(ensure).toHaveBeenCalledTimes(1);

		request.reset();
		await vi.advanceTimersByTimeAsync(BACKOFF_MAX_MS);

		expect(ensure).toHaveBeenCalledTimes(1);
		expect(request.isSettled()).toBe(false);
	});

	it('coalesces a re-entrant ensure fired from a notify listener into the in-flight load', async () => {
		const ensure = installEnsure(settled('unavailable'));
		const request = createDesktopModuleRequest(MODULE_NAME);
		let reentered = 0;
		request.subscribe(() => {
			reentered += 1;
			void request.ensure();
		});

		await expect(request.ensure()).resolves.toBe('unavailable');

		expect(ensure).toHaveBeenCalledTimes(1);
		expect(reentered).toBeGreaterThan(0);
		expect(request.isSettled()).toBe(false);
	});

	it('drops a stale in-flight resolution that lands after reset', async () => {
		let resolveEnsure: (result: DesktopModuleEnsureResult) => void = () => {};
		installEnsure(
			(module) =>
				new Promise<DesktopModuleEnsureResult>((resolve) => {
					resolveEnsure = () => resolve({module, status: 'installed'});
				}),
		);
		const request = createDesktopModuleRequest(MODULE_NAME);

		const settle = request.ensure();
		expect(request.isSettled()).toBe(false);

		request.reset();
		resolveEnsure({module: MODULE_NAME, status: 'installed'});
		await settle;

		expect(request.isSettled()).toBe(false);
	});
});
