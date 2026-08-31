// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {describe, expect, it, vi} from 'vitest';
import type {ILogger} from '../../ILogger';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {ILiveKitService} from '../../infrastructure/ILiveKitService';
import type {IVoiceRoomStore} from '../../infrastructure/IVoiceRoomStore';
import {VoiceReconciliationWorker} from '../VoiceReconciliationWorker';

function createLogger(): ILogger {
	const logger = {
		trace: vi.fn(),
		debug: vi.fn(),
		info: vi.fn(),
		warn: vi.fn(),
		error: vi.fn(),
		child: () => logger,
	};
	return logger as unknown as ILogger;
}

function createHarness() {
	const releaseLock = vi.fn().mockResolvedValue(true);
	const kvClient = {
		acquireLock: vi.fn().mockResolvedValue(true),
		extendLock: vi.fn().mockResolvedValue(true),
		releaseLock,
		setnx: vi.fn().mockResolvedValue(true),
		get: vi.fn().mockResolvedValue(null),
		setex: vi.fn().mockResolvedValue(undefined),
	} as unknown as IKVProvider;
	let finishDiscovery: () => void = () => {};
	const discovery = new Promise<{rooms: []}>((resolve) => {
		finishDiscovery = () => resolve({rooms: []});
	});
	const getActiveVoiceRooms = vi.fn().mockReturnValue(discovery);
	const worker = new VoiceReconciliationWorker({
		gatewayService: {getActiveVoiceRooms} as unknown as IGatewayService,
		liveKitService: {
			listActiveRooms: async () => ({rooms: [], errors: [], completed: true, searchedServers: 0}),
		} as unknown as ILiveKitService,
		voiceRoomStore: {listPinnedRooms: async () => []} as unknown as IVoiceRoomStore,
		kvClient,
		logger: createLogger(),
		intervalMs: 60000,
		staggerDelayMs: 0,
	});
	return {worker, releaseLock, getActiveVoiceRooms, finishDiscovery: () => finishDiscovery()};
}

describe('VoiceReconciliationWorker stop', () => {
	it('releases the reconciliation lock before stop resolves', async () => {
		const {worker, releaseLock, getActiveVoiceRooms, finishDiscovery} = createHarness();

		worker.start();
		await vi.waitFor(() => expect(getActiveVoiceRooms).toHaveBeenCalled());

		setTimeout(finishDiscovery, 0);
		await worker.stop();

		expect(releaseLock).toHaveBeenCalledTimes(1);
	});
});
