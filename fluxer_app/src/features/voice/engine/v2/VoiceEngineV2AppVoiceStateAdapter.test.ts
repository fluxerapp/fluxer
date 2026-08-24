// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import {autorun} from 'mobx';
import {describe, expect, it} from 'vitest';
import {VoiceEngineV2AppVoiceStateAdapter} from './VoiceEngineV2AppVoiceStateAdapter';

function voiceState(overrides: Partial<VoiceState> = {}): VoiceState {
	return {
		guild_id: 'guild-1',
		channel_id: 'channel-1',
		user_id: 'user-1',
		connection_id: 'connection-1',
		mute: false,
		deaf: false,
		self_mute: false,
		self_deaf: false,
		self_video: false,
		self_stream: false,
		viewer_stream_keys: [],
		...overrides,
	};
}

function guild(id: string, voiceStates: ReadonlyArray<VoiceState> = []): GuildReadyData {
	return {
		id,
		properties: {} as GuildReadyData['properties'],
		channels: [],
		emojis: [],
		members: [],
		member_count: 0,
		voice_states: voiceStates,
		roles: [],
		joined_at: '2026-01-01T00:00:00.000Z',
	};
}

describe('VoiceEngineV2AppVoiceStateAdapter', () => {
	it('keeps gateway voice-state indexes as immutable replacement projections', () => {
		const adapter = new VoiceEngineV2AppVoiceStateAdapter();
		adapter.handleGatewayReady([
			guild('guild-1', [voiceState({connection_id: 'connection-1', channel_id: 'channel-1'})]),
		]);
		const before = adapter.getConnectionVoiceStates();
		const beforeConnection = before['connection-1'];

		adapter.handleGatewayVoiceStateUpdate(
			'guild-1',
			voiceState({connection_id: 'connection-1', channel_id: 'channel-2'}),
		);

		const after = adapter.getConnectionVoiceStates();
		expect(after).not.toBe(before);
		expect(after['connection-1']).not.toBe(beforeConnection);
		expect(adapter.getAllVoiceStatesInChannel('guild-1', 'channel-1')['connection-1']).toBeUndefined();
		expect(adapter.getAllVoiceStatesInChannel('guild-1', 'channel-2')['connection-1']).toBe(after['connection-1']);
	});

	it('does not reinsert locally removed stale connections', () => {
		const adapter = new VoiceEngineV2AppVoiceStateAdapter();
		adapter.handleGatewayVoiceStateUpdate('guild-1', voiceState({connection_id: 'stale-connection'}));
		expect(adapter.getVoiceStateByConnectionId('stale-connection')).toBeDefined();

		adapter.removeVoiceStateConnection('stale-connection');
		adapter.handleGatewayVoiceStateUpdate('guild-1', voiceState({connection_id: 'stale-connection'}));

		expect(adapter.isConnectionIgnored('stale-connection')).toBe(true);
		expect(adapter.getVoiceStateByConnectionId('stale-connection')).toBeNull();
		expect(adapter.getAllVoiceStatesInChannel('guild-1', 'channel-1')['stale-connection']).toBeUndefined();
	});

	it('publishes self-video true-to-false changes through MobX observation', () => {
		const adapter = new VoiceEngineV2AppVoiceStateAdapter();
		const observed: Array<boolean | undefined> = [];
		const dispose = autorun(() => {
			observed.push(adapter.getAllVoiceStatesInChannel('guild-1', 'channel-1')['connection-1']?.self_video);
		});

		adapter.handleGatewayVoiceStateUpdate('guild-1', voiceState({self_video: true}));
		adapter.handleGatewayVoiceStateUpdate('guild-1', voiceState({self_video: false}));

		dispose();
		expect(observed).toEqual([undefined, true, false]);
		expect(adapter.getAllVoiceStatesInChannel('guild-1', 'channel-1')['connection-1']?.self_video).toBe(false);
	});
});
