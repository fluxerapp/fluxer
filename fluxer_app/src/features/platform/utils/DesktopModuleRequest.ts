// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {ExponentialBackoff} from '@app/features/platform/utils/RetryScheduler';
import type {DesktopModuleAPI} from '@fluxer/desktop_ipc/src/ModuleContract';
import {DesktopModuleEnsureStatus} from '@fluxer/desktop_ipc/src/ModuleContract';

const DESKTOP_MODULE_BACKOFF_MIN_MS = 1_000;
const DESKTOP_MODULE_BACKOFF_MAX_MS = 30_000;

type DesktopModuleState = 'idle' | 'loading' | 'ready' | 'failed';

export interface DesktopModuleRequest {
	readonly ensure: () => Promise<DesktopModuleEnsureStatus | null>;
	readonly isSettled: () => boolean;
	readonly subscribe: (listener: () => void) => () => void;
	readonly reset: () => void;
}

function getDesktopModules(): DesktopModuleAPI | null {
	return globalThis.window?.electron?.desktopModules ?? null;
}

export function createDesktopModuleRequest(moduleName: string): DesktopModuleRequest {
	const logger = new Logger(`DesktopModuleRequest:${moduleName}`);
	const listeners = new Set<() => void>();
	const backoff = new ExponentialBackoff({
		minDelay: DESKTOP_MODULE_BACKOFF_MIN_MS,
		maxDelay: DESKTOP_MODULE_BACKOFF_MAX_MS,
		factor: 2,
		jitter: true,
		jitterFactor: 0.2,
	});
	let state: DesktopModuleState = 'idle';
	let pending: Promise<DesktopModuleEnsureStatus | null> | null = null;
	let retryTimer: ReturnType<typeof setTimeout> | null = null;
	let generation = 0;

	const notify = (): void => {
		for (const listener of listeners) {
			try {
				listener();
			} catch (error) {
				logger.error('A desktop module listener threw', error);
			}
		}
	};

	const scheduleRetry = (loadGeneration: number): void => {
		if (retryTimer !== null) {
			clearTimeout(retryTimer);
		}
		retryTimer = setTimeout(() => {
			if (loadGeneration !== generation) {
				return;
			}
			retryTimer = null;
			void load();
		}, backoff.next());
	};

	const load = (): Promise<DesktopModuleEnsureStatus | null> => {
		const loadGeneration = ++generation;
		const desktopModules = getDesktopModules();
		if (!desktopModules) {
			state = 'ready';
			notify();
			return Promise.resolve(null);
		}
		state = 'loading';
		const request = desktopModules
			.ensure(moduleName)
			.then((result) => {
				if (loadGeneration !== generation) {
					return result.status;
				}
				if (result.status === DesktopModuleEnsureStatus.UNAVAILABLE) {
					state = 'failed';
					notify();
					scheduleRetry(loadGeneration);
					return result.status;
				}
				state = 'ready';
				backoff.reset();
				notify();
				return result.status;
			})
			.catch((error: unknown) => {
				if (loadGeneration !== generation) {
					return null;
				}
				logger.warn('Failed to request the module from the desktop shell', error);
				state = 'failed';
				notify();
				scheduleRetry(loadGeneration);
				return null;
			})
			.finally(() => {
				if (loadGeneration === generation) {
					pending = null;
				}
			});
		pending = request;
		notify();
		return request;
	};

	const ensure = (): Promise<DesktopModuleEnsureStatus | null> => {
		if (state === 'ready') {
			return Promise.resolve(null);
		}
		if (pending) {
			return pending;
		}
		if (state === 'failed') {
			return Promise.resolve(null);
		}
		return load();
	};

	return {
		ensure,
		isSettled: () => state === 'ready',
		subscribe: (listener) => {
			listeners.add(listener);
			return () => {
				listeners.delete(listener);
			};
		},
		reset: () => {
			pending = null;
			if (retryTimer !== null) {
				clearTimeout(retryTimer);
				retryTimer = null;
			}
			backoff.reset();
			state = 'idle';
			generation++;
			listeners.clear();
		},
	};
}
