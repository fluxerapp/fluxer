// SPDX-License-Identifier: AGPL-3.0-or-later

import {ExternalE2EEKeyProvider, type Room} from 'livekit-client';

const roomE2EEWorkers = new WeakMap<Room, Worker>();

export function createE2EEWorker(): Worker {
	return new Worker(
		new URL(/* webpackChunkName: "livekit-e2ee.worker" */ 'livekit-client/e2ee-worker', import.meta.url),
		{
			type: 'module',
			name: 'livekit-e2ee-worker',
		},
	);
}

export function createE2EEKeyProvider(): ExternalE2EEKeyProvider {
	return new ExternalE2EEKeyProvider();
}

export function ownE2EEWorker(room: Room, worker: Worker | null): void {
	if (worker === null) return;
	const previous = roomE2EEWorkers.get(room) ?? null;
	if (previous !== null && previous !== worker) previous.terminate();
	roomE2EEWorkers.set(room, worker);
}

export function releaseE2EEWorker(room: Room | null): boolean {
	if (room === null) return false;
	const worker = roomE2EEWorkers.get(room) ?? null;
	if (worker === null) return false;
	roomE2EEWorkers.delete(room);
	worker.terminate();
	return true;
}
