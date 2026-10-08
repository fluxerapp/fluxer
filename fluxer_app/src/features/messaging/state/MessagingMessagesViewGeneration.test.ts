// SPDX-License-Identifier: AGPL-3.0-or-later
// @vitest-environment happy-dom

import {instanceDiscoveryFixture} from '@app/features/platform/state/__fixtures__/RuntimeBootstrapFixture';
import {afterEach, describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => ({
	msg: (descriptor: unknown) => descriptor,
	t: (descriptor: unknown) => descriptor,
}));
vi.mock('@lingui/react/macro', () => ({Trans: () => null, useLingui: () => ({i18n: {_: () => ''}})}));

vi.mock('@app/features/platform/transport/RestTransport', () => ({http: {get: vi.fn()}}));

vi.mock('@app/features/app/state/GeoIP', () => ({default: {latitude: null, longitude: null}}));

vi.mock('@app/features/gateway/transport/GatewayConnection', () => ({
	default: {isReady: false, isConnected: false, connectionEpoch: 1},
}));

vi.mock('@app/features/navigation/state/SelectedChannel', () => ({
	default: {currentChannelId: null, selectedChannelIds: new Map<string, string>()},
}));

vi.mock('@app/features/channel/state/Channels', () => ({
	default: {
		getChannel: (id: string) => ({id, guildId: null, lastMessageId: null, isPrivate: () => true}),
		handleMessageCreate: vi.fn(),
	},
}));

const {default: RuntimeConfig} = await import('@app/features/app/state/RuntimeConfig');
const {runtimeSnapshotFromDiscovery} = await import('@app/features/app/state/InstanceSnapshotStore');
const {parseInstanceDiscoveryDocument} = await import('@fluxer/instance_bootstrap/src/Discovery');
const {default: Messages} = await import('@app/features/messaging/state/MessagingMessages');

RuntimeConfig.applySnapshot(
	runtimeSnapshotFromDiscovery(parseInstanceDiscoveryDocument(instanceDiscoveryFixture('https://one.example/api'))),
);

const CHANNEL_ID = '1555752678099779590';

describe('message list view generation', () => {
	afterEach(() => {
		Messages.handleSessionInvalidated();
	});

	function loadOneMessage(): void {
		Messages.handleLoadMessagesSuccess({
			channelId: CHANNEL_ID,
			messages: [
				{
					id: '1555923739894349824',
					channel_id: CHANNEL_ID,
					author: {
						id: '100',
						username: 'nera1',
						discriminator: '0001',
						global_name: null,
						avatar: null,
						avatar_color: null,
						flags: 0,
					},
					type: 0,
					flags: 0,
					pinned: false,
					tts: false,
					mention_everyone: false,
					mentions: [],
					mention_roles: [],
					content: 'scrolled away under the first account',
					timestamp: '2026-10-04T18:50:00.000Z',
				},
			],
		});
	}

	it('restarts the message list view when loaded messages are discarded', () => {
		loadOneMessage();
		expect(Messages.getMessages(CHANNEL_ID).length).toBe(1);
		const generation = Messages.cacheGeneration;
		Messages.handleSessionInvalidated();
		expect(Messages.cacheGeneration).toBe(generation + 1);
		expect(Messages.getMessages(CHANNEL_ID).length).toBe(0);
	});

	it('keeps the view when the next session invalidation finds nothing loaded', () => {
		loadOneMessage();
		Messages.handleSessionInvalidated();
		Messages.getMessages(CHANNEL_ID);
		const generation = Messages.cacheGeneration;
		Messages.handleSessionInvalidated();
		expect(Messages.cacheGeneration).toBe(generation);
	});
});
