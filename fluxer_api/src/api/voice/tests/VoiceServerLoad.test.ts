// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import type {GatewayVoiceStateCounts, IGatewayService} from '../../infrastructure/IGatewayService';
import {VoiceServerLoadTracker} from '../VoiceServerLoad';

function createGatewayService(respond: () => Promise<GatewayVoiceStateCounts>): {
	gatewayService: IGatewayService;
	callCount: () => number;
} {
	let calls = 0;
	const gatewayService = {
		getVoiceStateCounts: () => {
			calls += 1;
			return respond();
		},
	} as IGatewayService;
	return {gatewayService, callCount: () => calls};
}

function counts(servers: Array<{server_id: string; voice_state_count: number}>): GatewayVoiceStateCounts {
	return {
		total_voice_states: servers.reduce((total, server) => total + server.voice_state_count, 0),
		regions: [],
		servers,
	};
}

describe('VoiceServerLoadTracker', () => {
	it('reports no counts until the first refresh resolves', () => {
		const {gatewayService} = createGatewayService(async () => counts([{server_id: 'server-1', voice_state_count: 7}]));
		const tracker = new VoiceServerLoadTracker({gatewayService});
		expect(tracker.getConnectionCounts().size).toBe(0);
	});
	it('reports the counts the gateway returned', async () => {
		const {gatewayService} = createGatewayService(async () => counts([{server_id: 'server-1', voice_state_count: 7}]));
		const tracker = new VoiceServerLoadTracker({gatewayService});
		await tracker.refresh();
		expect(tracker.getConnectionCounts().get('server-1')).toBe(7);
	});
	it('keeps the last counts when a refresh fails', async () => {
		let shouldFail = false;
		const {gatewayService} = createGatewayService(async () => {
			if (shouldFail) {
				throw new Error('gateway unavailable');
			}
			return counts([{server_id: 'server-1', voice_state_count: 7}]);
		});
		const tracker = new VoiceServerLoadTracker({gatewayService});
		await tracker.refresh();
		shouldFail = true;
		await tracker.refresh();
		expect(tracker.getConnectionCounts().get('server-1')).toBe(7);
	});
	it('refreshes no more often than the refresh interval', async () => {
		let currentTime = 1000;
		const {gatewayService, callCount} = createGatewayService(async () =>
			counts([{server_id: 'server-1', voice_state_count: 7}]),
		);
		const tracker = new VoiceServerLoadTracker({
			gatewayService,
			refreshIntervalMs: 5000,
			now: () => currentTime,
		});
		await tracker.refresh();
		tracker.getConnectionCounts();
		currentTime += 4999;
		tracker.getConnectionCounts();
		expect(callCount()).toBe(1);
		currentTime += 1;
		tracker.getConnectionCounts();
		expect(callCount()).toBe(2);
	});
	it('drops counts that are too old to place against', async () => {
		let currentTime = 1000;
		const {gatewayService} = createGatewayService(async () => counts([{server_id: 'server-1', voice_state_count: 7}]));
		const tracker = new VoiceServerLoadTracker({
			gatewayService,
			refreshIntervalMs: 5000,
			now: () => currentTime,
		});
		await tracker.refresh();
		expect(tracker.getConnectionCounts().get('server-1')).toBe(7);
		currentTime += 20001;
		expect(tracker.getConnectionCounts().size).toBe(0);
	});
});
