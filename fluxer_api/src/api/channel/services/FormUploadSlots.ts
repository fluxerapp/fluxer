// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {ms} from 'itty-time';

const SLOT_WAIT_MS = ms('10 seconds');
const RETRY_AFTER_SECONDS = 5;

export interface FormUploadHolder {
	sender: string;
	clientIp: string;
}

interface Waiter {
	holder: FormUploadHolder;
	grant: () => void;
}

let activeSlots = 0;
const slotsBySender = new Map<string, number>();
const slotsByClientIp = new Map<string, number>();
const waiters: Array<Waiter> = [];

function canGrant(holder: FormUploadHolder): boolean {
	const max = Config.maxConcurrentFormUploads;
	if (activeSlots >= max) return false;
	const senderShare = Math.max(1, Math.floor(max / 4));
	const clientIpShare = Math.max(senderShare, Math.floor(max / 2));
	return (
		(slotsBySender.get(holder.sender) ?? 0) < senderShare && (slotsByClientIp.get(holder.clientIp) ?? 0) < clientIpShare
	);
}

function adjust(counts: Map<string, number>, key: string, delta: number): void {
	const next = (counts.get(key) ?? 0) + delta;
	if (next > 0) {
		counts.set(key, next);
	} else {
		counts.delete(key);
	}
}

function grant(holder: FormUploadHolder): void {
	activeSlots++;
	adjust(slotsBySender, holder.sender, 1);
	adjust(slotsByClientIp, holder.clientIp, 1);
}

function grantWaiters(): void {
	for (let index = 0; index < waiters.length; ) {
		const waiter = waiters[index]!;
		if (!canGrant(waiter.holder)) {
			index++;
			continue;
		}
		waiters.splice(index, 1);
		grant(waiter.holder);
		waiter.grant();
	}
}

function waitForSlot(holder: FormUploadHolder): Promise<void> {
	return new Promise((resolve, reject) => {
		const waiter: Waiter = {
			holder,
			grant: () => {
				clearTimeout(timer);
				resolve();
			},
		};
		const timer = setTimeout(() => {
			waiters.splice(waiters.indexOf(waiter), 1);
			reject(new ServiceUnavailableError({headers: {'Retry-After': String(RETRY_AFTER_SECONDS)}}));
		}, SLOT_WAIT_MS);
		waiters.push(waiter);
	});
}

export async function acquireFormUploadSlot(holder: FormUploadHolder): Promise<() => void> {
	if (canGrant(holder)) {
		grant(holder);
	} else {
		await waitForSlot(holder);
	}
	let released = false;
	return () => {
		if (released) return;
		released = true;
		activeSlots--;
		adjust(slotsBySender, holder.sender, -1);
		adjust(slotsByClientIp, holder.clientIp, -1);
		grantWaiters();
	};
}
