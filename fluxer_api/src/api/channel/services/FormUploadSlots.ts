// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {ms} from 'itty-time';

const SLOT_WAIT_MS = ms('10 seconds');
const RETRY_AFTER_SECONDS = 5;

let activeSlots = 0;
const waiters: Array<() => void> = [];

function waitForSlot(): Promise<void> {
	return new Promise((resolve, reject) => {
		const waiter = () => {
			clearTimeout(timer);
			resolve();
		};
		const timer = setTimeout(() => {
			waiters.splice(waiters.indexOf(waiter), 1);
			reject(new ServiceUnavailableError({headers: {'Retry-After': String(RETRY_AFTER_SECONDS)}}));
		}, SLOT_WAIT_MS);
		waiters.push(waiter);
	});
}

export async function acquireFormUploadSlot(): Promise<() => void> {
	if (activeSlots < Config.maxConcurrentFormUploads) {
		activeSlots++;
	} else {
		await waitForSlot();
	}
	let released = false;
	return () => {
		if (released) return;
		released = true;
		const next = waiters.shift();
		if (next) {
			next();
		} else {
			activeSlots--;
		}
	};
}
