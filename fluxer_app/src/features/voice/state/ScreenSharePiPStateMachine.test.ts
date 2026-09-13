// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createScreenSharePiPSnapshot,
	type ScreenSharePiPConditions,
	type ScreenSharePiPScreenShare,
	type ScreenSharePiPSnapshot,
	selectScreenSharePiPCommands,
	selectScreenSharePiPMode,
	transitionScreenSharePiPSnapshot,
} from '@app/features/voice/state/ScreenSharePiPStateMachine';
import {describe, expect, it} from 'vitest';

function makeShare(overrides: Partial<ScreenSharePiPScreenShare> = {}): ScreenSharePiPScreenShare {
	return {
		participantIdentity: 'user_123_connection-1',
		userId: '123',
		connectionId: 'connection-1',
		...overrides,
	};
}

function makeConditions(overrides: Partial<ScreenSharePiPConditions> = {}): ScreenSharePiPConditions {
	return {
		connectedChannelId: null,
		connectedGuildId: null,
		screenShare: null,
		selectedChannelId: null,
		isMobile: false,
		disabledBySetting: false,
		disabledBySession: false,
		...overrides,
	};
}

function feed(snapshot: ScreenSharePiPSnapshot, overrides: Partial<ScreenSharePiPConditions>): ScreenSharePiPSnapshot {
	return transitionScreenSharePiPSnapshot(snapshot, {
		type: 'conditions.changed',
		conditions: makeConditions(overrides),
	});
}

describe('screenSharePiPStateMachine', () => {
	it('starts closed with no commands', () => {
		const snapshot = createScreenSharePiPSnapshot();
		expect(selectScreenSharePiPMode(snapshot).kind).toBe('closed');
		expect(selectScreenSharePiPCommands(snapshot)).toEqual([]);
	});

	it('emits open when a screen share appears in a voice channel the user is not viewing', () => {
		const snapshot = feed(createScreenSharePiPSnapshot(), {
			connectedChannelId: 'channel-1',
			connectedGuildId: 'guild-1',
			screenShare: makeShare(),
			selectedChannelId: 'channel-2',
		});
		expect(selectScreenSharePiPMode(snapshot).kind).toBe('open');
		expect(selectScreenSharePiPCommands(snapshot)).toEqual([
			{
				type: 'open',
				content: {
					type: 'stream',
					participantIdentity: 'user_123_connection-1',
					channelId: 'channel-1',
					guildId: 'guild-1',
					connectionId: 'connection-1',
					userId: '123',
				},
			},
		]);
	});

	it('keeps PiP closed while the user is viewing the originating channel', () => {
		const snapshot = feed(createScreenSharePiPSnapshot(), {
			connectedChannelId: 'channel-1',
			connectedGuildId: 'guild-1',
			screenShare: makeShare(),
			selectedChannelId: 'channel-1',
		});
		expect(selectScreenSharePiPMode(snapshot).kind).toBe('closed');
		expect(selectScreenSharePiPCommands(snapshot)).toEqual([]);
	});

	it('opens PiP when the user navigates away from the originating channel', () => {
		const viewing = feed(createScreenSharePiPSnapshot(), {
			connectedChannelId: 'channel-1',
			screenShare: makeShare(),
			selectedChannelId: 'channel-1',
		});
		const navigated = feed(viewing, {
			connectedChannelId: 'channel-1',
			screenShare: makeShare(),
			selectedChannelId: 'channel-2',
		});
		expect(selectScreenSharePiPMode(navigated).kind).toBe('open');
		expect(selectScreenSharePiPCommands(navigated)).toEqual([
			{type: 'open', content: expect.objectContaining({channelId: 'channel-1'})},
		]);
	});

	it('closes PiP when the user navigates back to the originating channel', () => {
		const open = feed(createScreenSharePiPSnapshot(), {
			connectedChannelId: 'channel-1',
			screenShare: makeShare(),
			selectedChannelId: 'channel-2',
		});
		const back = feed(open, {
			connectedChannelId: 'channel-1',
			screenShare: makeShare(),
			selectedChannelId: 'channel-1',
		});
		expect(selectScreenSharePiPMode(back).kind).toBe('closed');
		expect(selectScreenSharePiPCommands(back)).toEqual([{type: 'close'}]);
	});

	it('does not emit redundant commands when conditions repeat', () => {
		const open = feed(createScreenSharePiPSnapshot(), {
			connectedChannelId: 'channel-1',
			screenShare: makeShare(),
			selectedChannelId: 'channel-2',
		});
		const again = feed(open, {
			connectedChannelId: 'channel-1',
			screenShare: makeShare(),
			selectedChannelId: 'channel-2',
		});
		expect(selectScreenSharePiPCommands(again)).toEqual([]);
	});

	it('reopens with new content when the sharing participant changes', () => {
		const first = feed(createScreenSharePiPSnapshot(), {
			connectedChannelId: 'channel-1',
			screenShare: makeShare({participantIdentity: 'user_1_connection-a', userId: '1', connectionId: 'connection-a'}),
			selectedChannelId: 'channel-2',
		});
		const second = feed(first, {
			connectedChannelId: 'channel-1',
			screenShare: makeShare({participantIdentity: 'user_2_connection-b', userId: '2', connectionId: 'connection-b'}),
			selectedChannelId: 'channel-2',
		});
		expect(selectScreenSharePiPCommands(second)).toEqual([
			{type: 'close'},
			{
				type: 'open',
				content: expect.objectContaining({
					participantIdentity: 'user_2_connection-b',
					userId: '2',
					connectionId: 'connection-b',
				}),
			},
		]);
	});

	it('closes PiP when the screen share ends', () => {
		const open = feed(createScreenSharePiPSnapshot(), {
			connectedChannelId: 'channel-1',
			screenShare: makeShare(),
			selectedChannelId: 'channel-2',
		});
		const ended = feed(open, {
			connectedChannelId: 'channel-1',
			screenShare: null,
			selectedChannelId: 'channel-2',
		});
		expect(selectScreenSharePiPCommands(ended)).toEqual([{type: 'close'}]);
	});

	it('keeps PiP open when an ended share is still represented as a watched stream', () => {
		const open = feed(createScreenSharePiPSnapshot(), {
			connectedChannelId: 'channel-1',
			screenShare: makeShare(),
			selectedChannelId: 'channel-2',
		});
		const watchedPlaceholder = feed(open, {
			connectedChannelId: 'channel-1',
			screenShare: makeShare({source: 'watched-stream'}),
			selectedChannelId: 'channel-2',
		});
		expect(selectScreenSharePiPMode(watchedPlaceholder)).toEqual(
			expect.objectContaining({
				kind: 'open',
				content: expect.objectContaining({
					participantIdentity: 'user_123_connection-1',
					connectionId: 'connection-1',
				}),
			}),
		);
		expect(selectScreenSharePiPCommands(watchedPlaceholder)).toEqual([]);
	});

	it('closes PiP when the user disconnects from voice', () => {
		const open = feed(createScreenSharePiPSnapshot(), {
			connectedChannelId: 'channel-1',
			screenShare: makeShare(),
			selectedChannelId: 'channel-2',
		});
		const disconnected = feed(open, {
			connectedChannelId: null,
			screenShare: null,
			selectedChannelId: 'channel-2',
		});
		expect(selectScreenSharePiPCommands(disconnected)).toEqual([{type: 'close'}]);
	});

	it('suppresses PiP on mobile', () => {
		const snapshot = feed(createScreenSharePiPSnapshot(), {
			connectedChannelId: 'channel-1',
			screenShare: makeShare(),
			selectedChannelId: 'channel-2',
			isMobile: true,
		});
		expect(selectScreenSharePiPMode(snapshot).kind).toBe('closed');
		expect(selectScreenSharePiPCommands(snapshot)).toEqual([]);
	});

	it('suppresses PiP when disabled by setting', () => {
		const snapshot = feed(createScreenSharePiPSnapshot(), {
			connectedChannelId: 'channel-1',
			screenShare: makeShare(),
			selectedChannelId: 'channel-2',
			disabledBySetting: true,
		});
		expect(selectScreenSharePiPCommands(snapshot)).toEqual([]);
	});

	it('suppresses PiP when disabled for the session', () => {
		const snapshot = feed(createScreenSharePiPSnapshot(), {
			connectedChannelId: 'channel-1',
			screenShare: makeShare(),
			selectedChannelId: 'channel-2',
			disabledBySession: true,
		});
		expect(selectScreenSharePiPCommands(snapshot)).toEqual([]);
	});

	it('closes the open PiP when the setting is enabled mid-session', () => {
		const open = feed(createScreenSharePiPSnapshot(), {
			connectedChannelId: 'channel-1',
			screenShare: makeShare(),
			selectedChannelId: 'channel-2',
		});
		const disabled = feed(open, {
			connectedChannelId: 'channel-1',
			screenShare: makeShare(),
			selectedChannelId: 'channel-2',
			disabledBySetting: true,
		});
		expect(selectScreenSharePiPCommands(disabled)).toEqual([{type: 'close'}]);
	});

	it('opens PiP for the new channel when the user joins a different voice channel mid-share', () => {
		const first = feed(createScreenSharePiPSnapshot(), {
			connectedChannelId: 'channel-1',
			screenShare: makeShare({participantIdentity: 'user_1_connection-a', userId: '1', connectionId: 'connection-a'}),
			selectedChannelId: 'channel-3',
		});
		const switched = feed(first, {
			connectedChannelId: 'channel-2',
			screenShare: makeShare({participantIdentity: 'user_9_connection-z', userId: '9', connectionId: 'connection-z'}),
			selectedChannelId: 'channel-3',
		});
		expect(selectScreenSharePiPCommands(switched)).toEqual([
			{type: 'close'},
			{
				type: 'open',
				content: expect.objectContaining({channelId: 'channel-2', participantIdentity: 'user_9_connection-z'}),
			},
		]);
	});
});
