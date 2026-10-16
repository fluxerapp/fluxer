// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KeybindCommand} from '@app/features/input/state/InputKeybind';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';

const VOICE_CALL_FULLSCREEN_ALLOWED_ACTIONS = new Set<KeybindCommand>([
	'system_open_theme_studio_popout',
	'voice_toggle_mute',
	'voice_toggle_deafen',
	'voice_push_to_talk',
	'voice_push_to_talk_priority',
	'voice_push_to_mute',
	'voice_priority_vad',
	'voice_toggle_vad',
	'voice_toggle_camera',
	'voice_disconnect',
]);
export const isKeybindAllowedDuringVoiceCallFullscreen = (action: KeybindCommand): boolean =>
	VOICE_CALL_FULLSCREEN_ALLOWED_ACTIONS.has(action);

const COMPACT_CALL_TEXTAREA_ACTIONS = new Set<KeybindCommand>([
	'message_focus_textarea',
	'message_reply_prev',
	'message_reply_next',
	'message_edit_prev',
	'message_edit_next',
	'chat_focus_textarea',
	'chat_upload',
	'chat_toggle_emoji',
	'chat_toggle_gif',
	'chat_toggle_sticker',
	'chat_toggle_saved_media',
	'chat_send_voice_message',
]);

interface CompactVoiceCallKeybindBlockOptions {
	action: KeybindCommand;
	channelType: number | null | undefined;
	isPrivateChannel: boolean;
	isGuildVoiceCallExpanded: boolean;
	isConnectedToPrivateCall: boolean;
	isPrivateCompactCallExpanded: boolean;
}

export function isKeybindBlockedByCompactVoiceCallView({
	action,
	channelType,
	isPrivateChannel,
	isGuildVoiceCallExpanded,
	isConnectedToPrivateCall,
	isPrivateCompactCallExpanded,
}: CompactVoiceCallKeybindBlockOptions): boolean {
	if (!COMPACT_CALL_TEXTAREA_ACTIONS.has(action)) return false;
	if (channelType === ChannelTypes.GUILD_VOICE) {
		return isGuildVoiceCallExpanded;
	}
	return isPrivateChannel && isConnectedToPrivateCall && isPrivateCompactCallExpanded;
}
