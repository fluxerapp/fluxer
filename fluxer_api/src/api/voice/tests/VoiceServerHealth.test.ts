// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceRegionRecord, VoiceServerRecord} from '@app/api/voice/VoiceModel';
import {VoiceEndpointError} from '@app/api/voice/VoiceServerEndpoints';
import {VoiceServerHealthMonitor} from '@app/api/voice/VoiceServerHealth';
import {describe, expect, it} from 'vitest';

function server(serverId: string, isActive = true): VoiceServerRecord {
	return {
		regionId: 'fra',
		serverId,
		endpoint: `wss://${serverId}.example.com`,
		apiKey: serverId,
		apiSecret: 'secret',
		latitude: null,
		longitude: null,
		isActive,
		softConnectionLimit: null,
		notes: null,
		internalEndpoint: null,
		pinnedHostname: null,
		pinnedSoftLimit: false,
		draining: false,
		restrictions: {
			vipOnly: false,
			requiredGuildFeatures: new Set(),
			allowedGuildIds: new Set(),
			allowedUserIds: new Set(),
		},
		createdAt: null,
		updatedAt: null,
	};
}

function topology(servers: Array<VoiceServerRecord>) {
	return {
		getAllRegions: () => [{id: 'fra'} as VoiceRegionRecord],
		getServersForRegion: () => servers,
	};
}

describe('VoiceServerHealthMonitor', () => {
	it('marks a server down after two failures and up after one success', async () => {
		const failing = new Set(['a']);
		const servers = [server('a'), server('b')];
		const monitor = new VoiceServerHealthMonitor(
			topology(servers),
			async (s) => {
				if (failing.has(s.serverId)) throw new VoiceEndpointError('unreachable', 'HTTP 502');
			},
			0,
		);
		expect(monitor.get(servers[0]!).status).toBe('unknown');
		await monitor.checkAll();
		expect(monitor.get(servers[0]!)).toMatchObject({status: 'unknown', error: 'HTTP 502'});
		expect(monitor.get(servers[1]!).status).toBe('up');
		await monitor.checkAll();
		expect(monitor.get(servers[0]!).status).toBe('down');
		failing.clear();
		await monitor.checkAll();
		expect(monitor.get(servers[0]!)).toMatchObject({status: 'up', error: null});
	});

	it('never marks a server down for a refused address and skips inactive servers', async () => {
		const probed: Array<string> = [];
		const servers = [server('private'), server('off', false)];
		const monitor = new VoiceServerHealthMonitor(
			topology(servers),
			async (s) => {
				probed.push(s.serverId);
				throw new VoiceEndpointError('private', 'not public');
			},
			0,
		);
		await monitor.checkAll();
		await monitor.checkAll();
		expect(monitor.get(servers[0]!)).toMatchObject({status: 'unknown', error: 'not public'});
		expect(probed).toEqual(['private', 'private']);
	});
});
