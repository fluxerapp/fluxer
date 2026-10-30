// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {resolveLivekitEndpoint, VoiceDataInitializer} from '@app/api/voice/VoiceDataInitializer';
import type {VoiceServerRecord} from '@app/api/voice/VoiceModel';
import {VoiceRepository} from '@app/api/voice/VoiceRepository';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const BUNDLED_KEY = 'bundled-key';
const BUNDLED_SECRET = 's'.repeat(64);

function joinedServer(overrides: Partial<VoiceServerRecord> = {}): VoiceServerRecord {
	return {
		regionId: 'default',
		serverId: 'default-server-1',
		endpoint: 'wss://203.0.113.7/livekit-default-server-1',
		apiKey: 'joined-key',
		apiSecret: 'j'.repeat(64),
		latitude: null,
		longitude: null,
		isActive: true,
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
		createdAt: new Date(),
		updatedAt: new Date(),
		...overrides,
	};
}

describe('VoiceDataInitializer', () => {
	let before: typeof Config.voice;

	beforeEach(() => {
		before = {...Config.voice};
		Config.voice.enabled = true;
		Config.voice.apiKey = BUNDLED_KEY;
		Config.voice.apiSecret = BUNDLED_SECRET;
		Config.voice.defaultRegion = {id: 'default', name: 'Default', emoji: ':earth_africa:', latitude: 0, longitude: 0};
		Config.voice.bundled = true;
	});

	afterEach(() => {
		Object.assign(Config.voice, before);
	});

	it('seeds the default region and the bundled server', async () => {
		await new VoiceDataInitializer().initialize();
		const repository = new VoiceRepository();
		expect(await repository.getRegion('default')).toMatchObject({id: 'default', isDefault: true});
		expect(await repository.getServer('default', 'default-server-1')).toMatchObject({
			apiKey: BUNDLED_KEY,
			endpoint: resolveLivekitEndpoint(Config.voice.url, Config.endpoints.apiPublic),
		});
	});

	it('seeds only the default region when the bundled server is off', async () => {
		Config.voice.bundled = false;
		await new VoiceDataInitializer().initialize();
		const repository = new VoiceRepository();
		expect(await repository.getRegion('default')).toMatchObject({id: 'default', isDefault: true});
		expect(await repository.listServers('default')).toEqual([]);
	});

	it('seeds the region without LiveKit keys when the bundled server is off', async () => {
		Config.voice.bundled = false;
		Config.voice.apiKey = undefined;
		Config.voice.apiSecret = undefined;
		await new VoiceDataInitializer().initialize();
		expect(await new VoiceRepository().getRegion('default')).not.toBeNull();
	});

	it('removes the bundled server once the bundled LiveKit is turned off', async () => {
		await new VoiceDataInitializer().initialize();
		Config.voice.bundled = false;
		await new VoiceDataInitializer().initialize();
		const repository = new VoiceRepository();
		expect(await repository.getServer('default', 'default-server-1')).toBeNull();
		expect(await repository.getRegion('default')).not.toBeNull();
	});

	it('leaves a joined server with the bundled ID unchanged', async () => {
		await new VoiceDataInitializer().initialize();
		const repository = new VoiceRepository();
		const joined = joinedServer();
		await repository.upsertServer(joined);
		await new VoiceDataInitializer().initialize();
		expect(await repository.getServer('default', 'default-server-1')).toMatchObject({
			endpoint: joined.endpoint,
			apiKey: joined.apiKey,
			apiSecret: joined.apiSecret,
		});
		Config.voice.bundled = false;
		await new VoiceDataInitializer().initialize();
		expect(await repository.getServer('default', 'default-server-1')).toMatchObject({apiKey: joined.apiKey});
	});

	it('syncs new LiveKit keys onto the bundled server', async () => {
		await new VoiceDataInitializer().initialize();
		Config.voice.apiKey = 'rotated-key';
		await new VoiceDataInitializer().initialize();
		expect(await new VoiceRepository().getServer('default', 'default-server-1')).toMatchObject({
			apiKey: 'rotated-key',
		});
	});
});
