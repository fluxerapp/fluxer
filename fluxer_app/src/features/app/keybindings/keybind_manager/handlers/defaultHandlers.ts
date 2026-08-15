// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import type {HandlerHost} from '@app/features/app/keybindings/keybind_manager/handlers/types';
import {
	PUSH_TO_TALK_WHILE_DEAFENED_DESCRIPTION_DESCRIPTOR,
	PUSH_TO_TALK_WHILE_DEAFENED_TITLE_DESCRIPTOR,
	YOU_CAN_T_UNDEAFEN_YOURSELF_BECAUSE_A_MODERATOR_DEAFENED_DESCRIPTOR,
	YOU_CAN_T_UNMUTE_YOURSELF_BECAUSE_A_MODERATOR_DESCRIPTOR,
} from '@app/features/app/keybindings/keybind_manager/shared';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {requestChannelComposerAffordanceDismissal} from '@app/features/channel/components/ChannelComposerDismissal';
import {CreateDMModal} from '@app/features/channel/components/modals/CreateDMModal';
import Channels from '@app/features/channel/state/Channels';
import * as VoiceStateCommands from '@app/features/devtools/commands/VoiceStateCommands';
import {AddGuildModal} from '@app/features/guild/components/modals/AddGuildModal';
import {CANCEL_DESCRIPTOR, OKAY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as InboxCommands from '@app/features/inbox/commands/InboxCommands';
import Inbox from '@app/features/inbox/state/Inbox';
import {KeyboardShortcutsCheatsheetModal} from '@app/features/input/components/modals/KeyboardShortcutsCheatsheetModal';
import Keybind, {type KeybindCommand} from '@app/features/input/state/InputKeybind';
import MessageEdit from '@app/features/messaging/state/MessageEdit';
import SavedMessages from '@app/features/messaging/state/SavedMessages';
import {openExternalUrlWithWarning} from '@app/features/messaging/utils/ExternalLinkUtils';
import {buildChannelLink} from '@app/features/messaging/utils/MessageLinkUtils';
import {goToMessage} from '@app/features/messaging/utils/MessageNavigator';
import Navigation from '@app/features/navigation/state/Navigation';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import MentionFeed from '@app/features/notification/state/MentionFeed';
import Permission from '@app/features/permissions/state/Permission';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import ReadStates from '@app/features/read_state/state/ReadStates';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import * as ThemeStudioCommands from '@app/features/theme_studio/commands/ThemeStudioCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import {openVoiceMessageComposerModal} from '@app/features/voice/components/VoiceMessageComposerModal';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import * as CallUtils from '@app/features/voice/utils/CallUtils';
import {
	VOICE_DEAFENED_BY_MODERATORS_DESCRIPTOR,
	VOICE_MUTED_BY_MODERATORS_DESCRIPTOR,
	VOICE_UNDEAFEN_SELF_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {JumpTypes} from '@fluxer/constants/src/JumpConstants';
import type {I18n} from '@lingui/core';
import React from 'react';

function canOpenVoiceMessageComposerForChannel(channelId: string): boolean {
	const channel = Channels.getChannel(channelId);
	if (!channel) return false;
	if (channel.isPrivate()) return true;
	return Permission.can(Permissions.SEND_MESSAGES, channel) && Permission.can(Permissions.ATTACH_FILES, channel);
}

const KEYBOARD_SHORTCUTS_CHEATSHEET_MODAL_KEY = 'keyboard-shortcuts-cheatsheet';
const PUSH_TO_TALK_DEAFENED_MODAL_KEY = 'push-to-talk-deafened';

function showPushToTalkDeafenedModalIfNeeded(host: HandlerHost, i18n: I18n): void {
	const voiceState = MediaEngine.getVoiceState(MediaEngine.guildId);
	const isGuildDeafened = voiceState?.deaf ?? false;
	if (!isGuildDeafened && !LocalVoiceState.getSelfDeaf()) return;
	if (isGuildDeafened) {
		ModalCommands.pushWithKey(
			modal(() =>
				React.createElement(ConfirmModal, {
					title: i18n._(VOICE_DEAFENED_BY_MODERATORS_DESCRIPTOR),
					description: i18n._(YOU_CAN_T_UNDEAFEN_YOURSELF_BECAUSE_A_MODERATOR_DEAFENED_DESCRIPTOR),
					primaryText: i18n._(OKAY_DESCRIPTOR),
					primaryVariant: 'primary',
					secondaryText: false,
					onPrimary: () => {},
					hideCloseButton: true,
				}),
			),
			PUSH_TO_TALK_DEAFENED_MODAL_KEY,
		);
		return;
	}
	ModalCommands.pushWithKey(
		modal(() =>
			React.createElement(ConfirmModal, {
				title: i18n._(PUSH_TO_TALK_WHILE_DEAFENED_TITLE_DESCRIPTOR),
				description: i18n._(PUSH_TO_TALK_WHILE_DEAFENED_DESCRIPTION_DESCRIPTOR),
				primaryText: i18n._(VOICE_UNDEAFEN_SELF_DESCRIPTOR),
				primaryVariant: 'primary',
				secondaryText: i18n._(CANCEL_DESCRIPTOR),
				onPrimary: () => {
					VoiceStateCommands.toggleSelfDeaf(null).catch((error) =>
						host.logger.error('push-to-talk undeafen failed', error),
					);
				},
				onSecondary: () => {},
			}),
		),
		PUSH_TO_TALK_DEAFENED_MODAL_KEY,
	);
}

export function registerDefaultKeybindHandlers(host: HandlerHost, i18n: I18n): void {
	host.register('nav_quick_switcher', ({type}) => {
		if (type !== 'press') return;
		if (QuickSwitcher.getIsOpen()) QuickSwitcher.hide();
		else QuickSwitcher.show();
	});
	host.register('system_toggle_shortcuts_overlay', ({type}) => {
		if (type !== 'press') return;
		if (ModalCommands.getTopModalKey() === KEYBOARD_SHORTCUTS_CHEATSHEET_MODAL_KEY) {
			ModalCommands.popWithKey(KEYBOARD_SHORTCUTS_CHEATSHEET_MODAL_KEY);
			return;
		}
		ModalCommands.pushWithKey(
			modal(() => React.createElement(KeyboardShortcutsCheatsheetModal)),
			KEYBOARD_SHORTCUTS_CHEATSHEET_MODAL_KEY,
		);
	});
	host.register('misc_help', ({type}) => {
		if (type !== 'press') return;
		openExternalUrlWithWarning(Routes.help());
	});
	host.register('misc_search', ({type}) => {
		if (type !== 'press') return;
		ComponentDispatch.dispatch('MESSAGE_SEARCH_OPEN');
	});
	host.register('voice_toggle_mute', ({type}) => {
		if (type !== 'press') return;
		const connectedGuildId = MediaEngine.guildId;
		const voiceState = MediaEngine.getVoiceState(connectedGuildId);
		const isGuildMuted = voiceState?.mute ?? false;
		if (isGuildMuted) {
			ModalCommands.push(
				modal(() =>
					React.createElement(ConfirmModal, {
						title: i18n._(VOICE_MUTED_BY_MODERATORS_DESCRIPTOR),
						description: i18n._(YOU_CAN_T_UNMUTE_YOURSELF_BECAUSE_A_MODERATOR_DESCRIPTOR),
						primaryText: i18n._(OKAY_DESCRIPTOR),
						primaryVariant: 'primary',
						secondaryText: false,
						onPrimary: () => {},
						hideCloseButton: true,
					}),
				),
			);
			return;
		}
		VoiceStateCommands.toggleSelfMute(null).catch((error) =>
			host.logger.error('voice_toggle_mute keybind failed', error),
		);
	});
	host.register('voice_toggle_deafen', ({type}) => {
		if (type !== 'press') return;
		const connectedGuildId = MediaEngine.guildId;
		const voiceState = MediaEngine.getVoiceState(connectedGuildId);
		const isGuildDeafened = voiceState?.deaf ?? false;
		if (isGuildDeafened) {
			ModalCommands.push(
				modal(() =>
					React.createElement(ConfirmModal, {
						title: i18n._(VOICE_DEAFENED_BY_MODERATORS_DESCRIPTOR),
						description: i18n._(YOU_CAN_T_UNDEAFEN_YOURSELF_BECAUSE_A_MODERATOR_DEAFENED_DESCRIPTOR),
						primaryText: i18n._(OKAY_DESCRIPTOR),
						primaryVariant: 'primary',
						secondaryText: false,
						onPrimary: () => {},
						hideCloseButton: true,
					}),
				),
			);
			return;
		}
		VoiceStateCommands.toggleSelfDeaf(null).catch((error) =>
			host.logger.error('voice_toggle_deafen keybind failed', error),
		);
	});
	host.register('voice_answer_call', ({type}) => {
		if (type !== 'press') return;
		const channelId = host.getIncomingCallChannelId();
		if (!channelId) return;
		host.acceptIncomingCall(channelId);
	});
	host.register('voice_decline_call', ({type}) => {
		if (type !== 'press') return;
		const channelId = host.getIncomingCallChannelId();
		if (!channelId) return;
		host.declineIncomingCall(channelId);
	});
	host.register('system_toggle_settings', ({type}) => {
		if (type !== 'press') return;
		ModalCommands.push(modal(() => React.createElement(UserSettingsModal)));
	});
	host.register('system_open_theme_studio_popout', ({type}) => {
		if (type !== 'press') return;
		ThemeStudioCommands.openThemeStudioPopout();
	});
	host.register('voice_push_to_talk', ({type}) => {
		if (!Keybind.isPushToTalkEffective()) return;
		if (type === 'press') {
			showPushToTalkDeafenedModalIfNeeded(host, i18n);
			if (host.pttReleaseTimer) {
				clearTimeout(host.pttReleaseTimer);
				host.pttReleaseTimer = null;
			}
			const shouldUnmute = Keybind.handlePushToTalkPress();
			if (shouldUnmute) {
				MediaEngine.applyPushToTalkHold(true);
			}
		} else {
			const shouldMute = Keybind.handlePushToTalkRelease();
			if (shouldMute) {
				const delay = Keybind.pushToTalkReleaseDelay;
				host.pttReleaseTimer = setTimeout(() => {
					host.pttReleaseTimer = null;
					MediaEngine.applyPushToTalkHold(false);
				}, delay);
			}
		}
	});
	host.register('voice_push_to_mute', ({type}) => {
		if (Keybind.isPushToTalkEffective()) return;
		MediaEngine.applyPushToMuteHold(type === 'press');
	});
	host.register('voice_push_to_talk_priority', ({type}) => {
		if (!Keybind.isPushToTalkEffective()) return;
		if (type === 'press') {
			showPushToTalkDeafenedModalIfNeeded(host, i18n);
			if (host.pttReleaseTimer) {
				clearTimeout(host.pttReleaseTimer);
				host.pttReleaseTimer = null;
			}
			Keybind.handlePushToTalkPress();
			Keybind.setPrioritySpeakerHeld(true);
			MediaEngine.applyPushToTalkHold(true);
		} else {
			Keybind.handlePushToTalkRelease();
			Keybind.setPrioritySpeakerHeld(false);
			const delay = Keybind.pushToTalkReleaseDelay;
			host.pttReleaseTimer = setTimeout(() => {
				host.pttReleaseTimer = null;
				MediaEngine.applyPushToTalkHold(false);
			}, delay);
		}
	});
	host.register('voice_priority_vad', ({type}) => {
		if (Keybind.isPushToTalkEffective()) return;
		Keybind.setPrioritySpeakerHeld(type === 'press');
	});
	host.register('voice_toggle_vad', ({type}) => {
		if (type !== 'press') return;
		Keybind.toggleTransmitMode();
	});
	host.register('voice_toggle_camera', ({type}) => {
		if (type !== 'press') return;
		void MediaEngine.toggleCameraFromKeybind().catch((error) =>
			host.logger.error('voice_toggle_camera keybind failed', error),
		);
	});
	host.register('voice_switch_channel', ({type}) => {
		if (type !== 'press') return;
		const channelId = host.currentChannelId;
		if (!channelId) return;
		const channel = Channels.getChannel(channelId);
		if (!channel) return;
		if (channel.type !== ChannelTypes.GUILD_VOICE) {
			return;
		}
		void MediaEngine.connectToVoiceChannel(channel.guildId ?? null, channel.id).catch((error) =>
			host.logger.error('voice_switch_channel keybind failed', error),
		);
	});
	host.register('voice_disconnect', ({type}) => {
		if (type !== 'press') return;
		void MediaEngine.disconnectFromVoiceChannel('user').catch((error) =>
			host.logger.error('voice_disconnect keybind failed', error),
		);
	});
	host.register('voice_toggle_soundboard', ({type}) => {
		if (type !== 'press') return;
		ComponentDispatch.dispatch('SOUNDBOARD_TOGGLE');
	});
	host.register('voice_toggle_compact_call_view', ({type}) => {
		if (type !== 'press') return;
		const channelId = host.currentChannelId;
		if (!channelId) return;
		ComponentDispatch.dispatch('COMPACT_VOICE_CALL_EXPANSION_TOGGLE', {channelId});
	});
	host.register('chat_copy_channel_link', ({type}) => {
		if (type !== 'press') return;
		const channelId = host.currentChannelId;
		if (!channelId) return;
		const channel = Channels.getChannel(channelId);
		if (!channel) return;
		const link = buildChannelLink({guildId: channel.guildId, channelId: channel.id});
		TextCopyCommands.copy(i18n, link);
	});
	host.register('message_focus_textarea', ({type}) => {
		if (type !== 'press') return;
		const channelId = host.currentChannelId;
		if (!channelId) return;
		ComponentDispatch.dispatch('FOCUS_TEXTAREA', {channelId});
	});
	host.register('chat_scroll_up', ({type}) => {
		if (type !== 'press') return;
		ComponentDispatch.dispatch('SCROLL_PAGE_UP');
	});
	host.register('chat_scroll_down', ({type}) => {
		if (type !== 'press') return;
		ComponentDispatch.dispatch('SCROLL_PAGE_DOWN');
	});
	host.register('chat_jump_oldest_unread', ({type}) => {
		if (type !== 'press') return;
		const channelId = host.currentChannelId;
		if (!channelId) return;
		const targetId = ReadStates.getOldestUnreadMessageId(channelId);
		if (!targetId) return;
		goToMessage(channelId, targetId, {jumpType: JumpTypes.ANIMATED});
	});
	host.register('chat_mark_channel_read', ({type}) => {
		if (type !== 'press') return;
		const channelId = host.currentChannelId;
		if (!channelId) return;
		if (requestChannelComposerAffordanceDismissal(channelId)) return;
		if (ReadStates.hasUnread(channelId)) {
			ComponentDispatch.dispatch('ESCAPE_PRESSED', {channelId});
		}
	});
	host.register('chat_mark_guild_read', ({type}) => {
		if (type !== 'press') return;
		const guildId = host.currentGuildId;
		if (!guildId) return;
		const channels = Channels.getGuildChannels(guildId);
		const channelIds = channels.filter((channel) => ReadStates.hasUnread(channel.id)).map((channel) => channel.id);
		if (channelIds.length > 0) {
			void ReadStateCommands.bulkAckChannels(channelIds);
		}
	});
	host.register('chat_mark_all_inbox_read', ({type}) => {
		if (type !== 'press') return;
		InboxCommands.markAllInboxChannelsAsRead(i18n);
	});
	host.register('chat_mark_inbox_read', ({type}) => {
		if (type !== 'press') return;
		const inboxTab = Inbox.selectedTab;
		if (inboxTab === 'unreadChannels') {
			const unreadChannelId = ReadStates.getChannelIds().find((channelId) => ReadStates.hasUnreadOrMentions(channelId));
			if (unreadChannelId) {
				ReadStateCommands.ack(unreadChannelId, true, true);
			}
		} else if (inboxTab === 'bookmarks') {
			const savedMessages = SavedMessages.savedMessages;
			const unreadBookmark = savedMessages.find((message) => {
				return ReadStates.hasUnread(message.channelId);
			});
			if (unreadBookmark) {
				ReadStateCommands.ack(unreadBookmark.channelId, true, true);
			}
		} else {
			const mentions = MentionFeed.recentMentions;
			const unreadMention = mentions.find((message) => {
				return ReadStates.hasUnread(message.channelId);
			});
			if (unreadMention) {
				ReadStateCommands.ack(unreadMention.channelId, true, true);
			}
		}
	});
	host.register('nav_history_back', ({type}) => {
		if (type !== 'press') return;
		if (SelectedChannel.navigateViewedChannelHistory(-1)) return;
		const history = RouterUtils.getHistory();
		if (history?.go) history.go(-1);
	});
	host.register('nav_history_forward', ({type}) => {
		if (type !== 'press') return;
		if (SelectedChannel.navigateViewedChannelHistory(1)) return;
		const history = RouterUtils.getHistory();
		if (history?.go) history.go(1);
	});
	host.register('nav_current_call', ({type}) => {
		if (type !== 'press') return;
		const channelId = MediaEngine.channelId;
		const guildId = MediaEngine.guildId;
		if (!channelId) return;
		host.navigateToChannel(guildId, channelId);
	});
	host.register('nav_toggle_last_guild_dms', ({type}) => {
		if (type !== 'press') return;
		if (host.currentGuildId) {
			host.navigateToDirectMessages();
			return;
		}
		host.navigateToLastCommunityChannel();
	});
	host.register('nav_guild_tab_next', ({type}) => {
		if (type !== 'press') return;
		host.cycleGuildLikeSlot(1);
	});
	host.register('nav_guild_tab_prev', ({type}) => {
		if (type !== 'press') return;
		host.cycleGuildLikeSlot(-1);
	});
	const guildSlotActions: Array<[KeybindCommand, number]> = [
		['nav_guild_slot_1', 0],
		['nav_guild_slot_2', 1],
		['nav_guild_slot_3', 2],
		['nav_guild_slot_4', 3],
		['nav_guild_slot_5', 4],
		['nav_guild_slot_6', 5],
		['nav_guild_slot_7', 6],
		['nav_guild_slot_8', 7],
		['nav_guild_slot_9', 8],
	];
	for (const [action, slotIndex] of guildSlotActions) {
		host.register(action, ({type}) => {
			if (type !== 'press') return;
			host.navigateToGuildLikeSlot(slotIndex);
		});
	}
	host.register('nav_channel_next', ({type}) => {
		if (type !== 'press') return;
		host.cycleChannelInCurrentContext(1);
	});
	host.register('nav_channel_prev', ({type}) => {
		if (type !== 'press') return;
		host.cycleChannelInCurrentContext(-1);
	});
	host.register('nav_guild_next', ({type}) => {
		if (type !== 'press') return;
		host.cycleGuildLikeSlot(1);
	});
	host.register('nav_guild_prev', ({type}) => {
		if (type !== 'press') return;
		host.cycleGuildLikeSlot(-1);
	});
	host.register('nav_unread_next', ({type}) => {
		if (type !== 'press') return;
		host.cycleFilteredChannelInCurrentGuild((c) => ReadStates.hasUnread(c.id), 1);
	});
	host.register('nav_unread_prev', ({type}) => {
		if (type !== 'press') return;
		host.cycleFilteredChannelInCurrentGuild((c) => ReadStates.hasUnread(c.id), -1);
	});
	host.register('nav_mention_next', ({type}) => {
		if (type !== 'press') return;
		host.cycleFilteredChannelInCurrentGuild((c) => ReadStates.getMentionCount(c.id) > 0, 1);
	});
	host.register('nav_mention_prev', ({type}) => {
		if (type !== 'press') return;
		host.cycleFilteredChannelInCurrentGuild((c) => ReadStates.getMentionCount(c.id) > 0, -1);
	});
	host.register('voice_start_dm_call', ({type, shiftKey}) => {
		if (type !== 'press') return;
		const channelId = host.currentChannelId;
		if (!channelId) return;
		const channel = Channels.getChannel(channelId);
		if (!channel || channel.guildId) return;
		void CallUtils.requestStartCall(i18n, channelId, CallUtils.getCallStartRequestOptions({shiftKey}, {kind: 'voice'}));
	});
	host.register('chat_toggle_pins', ({type}) => {
		if (type !== 'press') return;
		ComponentDispatch.dispatch('CHANNEL_PINS_OPEN');
	});
	host.register('chat_toggle_inbox', ({type}) => {
		if (type !== 'press') return;
		ComponentDispatch.dispatch('INBOX_OPEN');
	});
	host.register('chat_toggle_member_list', ({type}) => {
		if (type !== 'press') return;
		ComponentDispatch.dispatch('CHANNEL_MEMBER_LIST_TOGGLE');
	});
	host.register('chat_toggle_emoji', ({type}) => {
		if (type !== 'press') return;
		const channelId = host.currentChannelId;
		if (!channelId) return;
		const editingMessageId = MessageEdit.getEditingMessageId(channelId);
		if (editingMessageId) {
			ComponentDispatch.dispatch('EDITING_EXPRESSION_PICKER_TAB_TOGGLE', {
				channelId,
				messageId: editingMessageId,
				tab: 'emojis',
			});
			return;
		}
		ComponentDispatch.dispatch('EXPRESSION_PICKER_TAB_TOGGLE', {channelId, tab: 'emojis'});
	});
	host.register('chat_toggle_gif', ({type}) => {
		if (type !== 'press') return;
		const channelId = host.currentChannelId;
		if (!channelId) return;
		ComponentDispatch.dispatch('EXPRESSION_PICKER_TAB_TOGGLE', {channelId, tab: 'gifs'});
	});
	host.register('chat_toggle_sticker', ({type}) => {
		if (type !== 'press') return;
		const channelId = host.currentChannelId;
		if (!channelId) return;
		ComponentDispatch.dispatch('EXPRESSION_PICKER_TAB_TOGGLE', {channelId, tab: 'stickers'});
	});
	host.register('chat_toggle_saved_media', ({type}) => {
		if (type !== 'press') return;
		const channelId = host.currentChannelId;
		if (!channelId) return;
		ComponentDispatch.dispatch('EXPRESSION_PICKER_TAB_TOGGLE', {channelId, tab: 'memes'});
	});
	host.register('chat_send_voice_message', ({type}) => {
		if (type !== 'press') return;
		const channelId = host.currentChannelId ?? Navigation.channelId;
		if (!channelId) return;
		const textareaResult = ComponentDispatch.dispatchToFirstResult(
			'TEXTAREA_SEND_VOICE_MESSAGE',
			{channelId},
			(result) => result === true || result === false,
		);
		if (textareaResult !== undefined) {
			return;
		}
		if (!canOpenVoiceMessageComposerForChannel(channelId)) return;
		openVoiceMessageComposerModal(channelId);
	});
	host.register('nav_add_guild', ({type}) => {
		if (type !== 'press') return;
		if (RuntimeConfig.singleCommunityEnabled) return;
		ModalCommands.push(modal(() => React.createElement(AddGuildModal)));
	});
	host.register('chat_new_dm', ({type}) => {
		if (type !== 'press') return;
		if (RuntimeConfig.directMessagesDisabled) return;
		ModalCommands.push(modal(() => React.createElement(CreateDMModal)));
	});
	host.register('chat_focus_textarea', ({type}) => {
		if (type !== 'press') return;
		const channelId = host.currentChannelId;
		if (!channelId) return;
		ComponentDispatch.dispatch('FOCUS_TEXTAREA', {channelId});
	});
	host.register('chat_upload', ({type}) => {
		if (type !== 'press') return;
		const channelId = host.currentChannelId;
		if (!channelId) return;
		ComponentDispatch.dispatch('TEXTAREA_UPLOAD_FILE', {channelId});
	});
	host.register('system_zoom_in', ({type}) => {
		if (type !== 'press') return;
		void Accessibility.adjustZoom(1);
	});
	host.register('system_zoom_out', ({type}) => {
		if (type !== 'press') return;
		void Accessibility.adjustZoom(-1);
	});
	host.register('system_zoom_reset', ({type}) => {
		if (type !== 'press') return;
		Accessibility.updateSettings({zoomLevel: 1.0});
	});
}
