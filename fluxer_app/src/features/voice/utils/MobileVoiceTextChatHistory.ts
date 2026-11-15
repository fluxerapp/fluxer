// SPDX-License-Identifier: AGPL-3.0-or-later

const MOBILE_VOICE_TEXT_CHAT_HISTORY_KEY = '__fluxer_mobile_voice_text_chat';

interface MobileVoiceTextChatHistoryState {
	[MOBILE_VOICE_TEXT_CHAT_HISTORY_KEY]: {
		channelId: string;
	};
}

function getCurrentPath(): string {
	return window.location.pathname + window.location.search + window.location.hash;
}

function isMobileVoiceTextChatHistoryState(state: unknown, channelId: string): boolean {
	if (typeof state !== 'object' || state === null) {
		return false;
	}
	const value = (state as Partial<MobileVoiceTextChatHistoryState>)[MOBILE_VOICE_TEXT_CHAT_HISTORY_KEY];
	return value?.channelId === channelId;
}

export function isCurrentMobileVoiceTextChatHistoryEntry(channelId: string): boolean {
	if (typeof window === 'undefined') {
		return false;
	}
	return isMobileVoiceTextChatHistoryState(window.history.state, channelId);
}

export function pushMobileVoiceTextChatHistoryEntry(channelId: string): void {
	if (typeof window === 'undefined') {
		return;
	}
	if (isCurrentMobileVoiceTextChatHistoryEntry(channelId)) {
		return;
	}
	const nextState: MobileVoiceTextChatHistoryState = {
		[MOBILE_VOICE_TEXT_CHAT_HISTORY_KEY]: {
			channelId,
		},
	};
	window.history.pushState(nextState, '', getCurrentPath());
}

export function goBackFromMobileVoiceTextChatHistoryEntry(channelId: string): boolean {
	if (typeof window === 'undefined') {
		return false;
	}
	if (!isCurrentMobileVoiceTextChatHistoryEntry(channelId)) {
		return false;
	}
	if (window.history.length <= 1) {
		return false;
	}
	window.history.back();
	return true;
}
