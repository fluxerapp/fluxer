/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import UpdaterStore from '~/stores/UpdaterStore';

const CONTROLLER_CHANGE_TIMEOUT_MS = 4_000;

export const activateLatestServiceWorker = async (): Promise<void> => {
	if (!('serviceWorker' in navigator)) {
		return;
	}

	try {
		const registration = await navigator.serviceWorker.getRegistration();
		if (!registration) {
			return;
		}

		await registration.update().catch((error: unknown) => {
			console.warn('[Versioning] Failed to update service worker registration', error);
		});

		const postSkipWaiting = (worker: ServiceWorker | null) => {
			if (!worker) return;
			try {
				worker.postMessage({type: 'SKIP_WAITING'});
			} catch (error) {
				console.warn('[Versioning] Failed to postMessage SKIP_WAITING', error);
			}
		};

		if (registration.waiting) {
			postSkipWaiting(registration.waiting);
			await waitForActivation(registration.waiting);
		} else if (registration.installing) {
			const installing = registration.installing;

			await new Promise<void>((resolve) => {
				const handleStateChange = () => {
					if (installing.state === 'installed') {
						postSkipWaiting(registration.waiting);
						if (registration.waiting) {
							waitForActivation(registration.waiting).then(resolve);
						} else {
							resolve();
						}
					} else if (installing.state === 'activated') {
						resolve();
					}
				};

				if (installing.state === 'installed') {
					handleStateChange();
				} else if (installing.state === 'activated') {
					resolve();
				} else {
					installing.addEventListener('statechange', handleStateChange);
				}
			});
		}

		await waitForControllerChange();
	} catch (error) {
		console.warn('[Versioning] Failed to activate latest service worker', error);
	}
};

const waitForControllerChange = async (): Promise<void> => {
	if (!('serviceWorker' in navigator)) {
		return;
	}

	if (!navigator.serviceWorker.controller) {
		return;
	}

	await new Promise<void>((resolve) => {
		let settled = false;

		const timeoutId = window.setTimeout(() => {
			if (!settled) {
				settled = true;
				console.warn('[Versioning] Controller change timed out after', CONTROLLER_CHANGE_TIMEOUT_MS, 'ms');
				resolve();
			}
		}, CONTROLLER_CHANGE_TIMEOUT_MS);

		const handleControllerChange = () => {
			if (settled) return;
			settled = true;
			window.clearTimeout(timeoutId);
			navigator.serviceWorker.removeEventListener('controllerchange', handleControllerChange);
			resolve();
		};

		navigator.serviceWorker.addEventListener('controllerchange', handleControllerChange);
	});
};

const waitForActivation = async (worker: ServiceWorker): Promise<void> => {
	return new Promise<void>((resolve) => {
		if (worker.state === 'activated') {
			resolve();
			return;
		}

		const handleStateChange = () => {
			if (worker.state === 'activated') {
				worker.removeEventListener('statechange', handleStateChange);
				resolve();
			}
		};

		worker.addEventListener('statechange', handleStateChange);

		setTimeout(() => {
			if (worker.state !== 'activated') {
				console.warn('[Versioning] Service worker activation timed out, current state:', worker.state);
				worker.removeEventListener('statechange', handleStateChange);
				resolve();
			}
		}, CONTROLLER_CHANGE_TIMEOUT_MS);
	});
};

export const ensureLatestAssets = async (options: {force?: boolean} = {}): Promise<{updateFound: boolean}> => {
	await UpdaterStore.checkForUpdates(options.force ?? false);
	return {updateFound: UpdaterStore.updateInfo.web.available};
};
