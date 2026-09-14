// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getDirectCallFullscreenScopeKey,
	isTextChatVisibleForAutoAck,
	isVoiceCallFullscreenScopeForChannel,
} from '@app/features/notification/utils/AutoAckVisibility';
import {getGuildVoiceCallExpansionKey} from '@app/features/voice/state/CompactVoiceCallHeight';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it} from 'vitest';

describe('isTextChatVisibleForAutoAck', () => {
	it('allows normal text channels to auto-ack when no fullscreen call view is active', () => {
		expect(
			isTextChatVisibleForAutoAck({
				channelId: 'text-1',
				channelType: ChannelTypes.GUILD_TEXT,
				isGuildVoiceCallExpanded: true,
				activeVoiceCallFullscreenScopeKey: null,
			}),
		).toBe(true);
	});
	it('blocks guild voice auto-ack while the call view has hidden chat', () => {
		expect(
			isTextChatVisibleForAutoAck({
				channelId: 'voice-1',
				channelType: ChannelTypes.GUILD_VOICE,
				isGuildVoiceCallExpanded: true,
				activeVoiceCallFullscreenScopeKey: null,
			}),
		).toBe(false);
	});
	it('allows guild voice auto-ack when chat is visible', () => {
		expect(
			isTextChatVisibleForAutoAck({
				channelId: 'voice-1',
				channelType: ChannelTypes.GUILD_VOICE,
				isGuildVoiceCallExpanded: false,
				activeVoiceCallFullscreenScopeKey: null,
			}),
		).toBe(true);
	});
	it('blocks auto-ack when the selected channel is in a fullscreen call view', () => {
		expect(
			isTextChatVisibleForAutoAck({
				channelId: 'voice-1',
				channelType: ChannelTypes.GUILD_VOICE,
				isGuildVoiceCallExpanded: false,
				activeVoiceCallFullscreenScopeKey: getGuildVoiceCallExpansionKey('voice-1'),
			}),
		).toBe(false);
		expect(
			isTextChatVisibleForAutoAck({
				channelId: 'dm-1',
				channelType: ChannelTypes.DM,
				isGuildVoiceCallExpanded: false,
				activeVoiceCallFullscreenScopeKey: getDirectCallFullscreenScopeKey('dm-1'),
			}),
		).toBe(false);
	});
	it('ignores fullscreen call scopes for other channels', () => {
		expect(isVoiceCallFullscreenScopeForChannel('voice-1', getGuildVoiceCallExpansionKey('voice-2'))).toBe(false);
		expect(
			isTextChatVisibleForAutoAck({
				channelId: 'voice-1',
				channelType: ChannelTypes.GUILD_VOICE,
				isGuildVoiceCallExpanded: false,
				activeVoiceCallFullscreenScopeKey: getGuildVoiceCallExpansionKey('voice-2'),
			}),
		).toBe(true);
	});
});
