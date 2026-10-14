// SPDX-License-Identifier: AGPL-3.0-or-later
// @vitest-environment happy-dom

import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {Message} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const ME = '100';
const OTHER = '200';
const SELECTED_CHANNEL = '1000';
const THREAD_CHANNEL = '2000';
const OTHER_CHANNEL = '3000';

const state = {
	enableTTSCommand: true,
	selfDeaf: false,
	navigationChannelId: null as string | null,
	selectedChannelId: SELECTED_CHANNEL as string | null,
	threadId: null as string | null,
	mutedChannelIds: new Set<string>(),
};

const speak = vi.fn();

vi.mock('@lingui/core/macro', () => ({
	msg: (descriptor: unknown) => descriptor,
	t: (descriptor: unknown) => descriptor,
}));

vi.mock('@lingui/react/macro', () => ({
	Trans: () => null,
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? ''}}),
}));

vi.mock('@app/features/voice/utils/TtsSpeechUtils', () => ({
	nativeApiPresent: () => true,
	nativeHasVoices: () => true,
	resumeNative: vi.fn(),
	cancel: vi.fn(),
	speak: (utterance: {text: string}) => speak(utterance.text),
	createUtterances: (text: string) => [{text}],
	getVoices: () => [],
	selectPreferredVoice: () => null,
}));

vi.mock('@app/features/ui/utils/NativeUtils', async (importOriginal) => ({
	...(await importOriginal<typeof import('@app/features/ui/utils/NativeUtils')>()),
	getNativePlatformSync: () => 'web',
}));

vi.mock('@app/features/navigation/state/Navigation', () => ({
	default: {
		get channelId() {
			return state.navigationChannelId;
		},
		get threadId() {
			return state.threadId;
		},
	},
}));

vi.mock('@app/features/accessibility/state/Accessibility', () => ({
	default: {
		get enableTTSCommand() {
			return state.enableTTSCommand;
		},
		ttsRate: 1,
	},
}));

vi.mock('@app/features/auth/state/Authentication', () => ({default: {currentUserId: ME}}));

vi.mock('@app/features/navigation/state/SelectedChannel', () => ({
	default: {
		get currentChannelId() {
			return state.selectedChannelId;
		},
	},
}));

vi.mock('@app/features/relationship/state/Relationships', () => ({default: {isBlocked: () => false}}));

vi.mock('@app/features/channel/state/Channels', () => ({
	default: {getChannel: (id: string) => ({id, guildId: '1'})},
}));

vi.mock('@app/features/user/state/UserGuildSettings', () => ({
	default: {
		isGuildOrChannelMuted: (_guildId: string | null, channelId: string) => state.mutedChannelIds.has(channelId),
		isParentCategoryMuted: () => false,
	},
}));

vi.mock('@app/features/user/state/UserSettings', () => ({default: {locale: 'en-US'}}));
vi.mock('@app/features/user/state/Users', () => ({default: {getUser: (id: string) => ({id})}}));
vi.mock('@app/features/user/utils/NicknameUtils', () => ({getNickname: (user: {id: string}) => user.id}));
vi.mock('@app/features/voice/utils/TtsTextFormatter', () => ({formatMessageForTts: (content: string) => content}));
vi.mock('@app/features/messaging/state/MessageReferences', () => ({
	default: {getMessageReference: () => ({state: 0})},
	MessageReferenceState: {LOADED: 1},
}));
vi.mock('@app/features/messaging/utils/SystemMessageUtils', () => ({
	SystemMessageUtils: {stringify: () => 'system'},
}));

Object.defineProperty(window, 'speechSynthesis', {
	configurable: true,
	value: {addEventListener: vi.fn(), removeEventListener: vi.fn()},
});

const {default: LocalVoiceState} = await import('@app/features/voice/state/LocalVoiceState');
vi.spyOn(LocalVoiceState, 'getSelfDeaf').mockImplementation(() => state.selfDeaf);
const {default: Notification, TTSNotificationMode} = await import('@app/features/ui/state/Notification');
const {default: TtsUtils} = await import('@app/features/voice/utils/VoiceTtsUtils');

let nextId = 1;

function message(overrides: {channelId?: string; authorId?: string; tts?: boolean; type?: number}): Message {
	return {
		id: String(nextId++),
		channel_id: overrides.channelId ?? SELECTED_CHANNEL,
		author: {id: overrides.authorId ?? OTHER},
		type: overrides.type ?? MessageTypes.DEFAULT,
		tts: overrides.tts ?? false,
		content: 'hello',
	} as unknown as Message;
}

function spoken(input: Parameters<typeof message>[0]): boolean {
	speak.mockClear();
	TtsUtils.handleIncomingTtsMessage(message(input));
	return speak.mock.calls.length > 0;
}

describe('incoming message text-to-speech gate', () => {
	beforeEach(() => {
		Notification.setTTSNotificationMode(TTSNotificationMode.NEVER);
		state.navigationChannelId = null;
		state.enableTTSCommand = true;
		state.selfDeaf = false;
		state.selectedChannelId = SELECTED_CHANNEL;
		state.threadId = null;
		state.mutedChannelIds = new Set();
	});

	afterEach(() => {
		TtsUtils.dispose();
	});

	it('keeps /tts from a channel you are not viewing silent when narration is off', () => {
		expect(spoken({channelId: OTHER_CHANNEL, tts: true})).toBe(false);
	});

	it('speaks /tts in the selected channel when narration is off', () => {
		expect(spoken({tts: true})).toBe(true);
	});

	it('speaks /tts in the thread open in the side panel when narration is off', () => {
		state.threadId = THREAD_CHANNEL;
		expect(spoken({channelId: THREAD_CHANNEL, tts: true})).toBe(true);
	});

	it('follows the routed channel over a stale selected channel', () => {
		state.navigationChannelId = OTHER_CHANNEL;
		expect(spoken({channelId: OTHER_CHANNEL, tts: true})).toBe(true);
		expect(spoken({tts: true})).toBe(false);
	});

	it('speaks your own /tts in the selected channel', () => {
		expect(spoken({authorId: ME, tts: true})).toBe(true);
	});

	it('keeps /tts silent when /tts playback is disabled', () => {
		state.enableTTSCommand = false;
		expect(spoken({tts: true})).toBe(false);
	});

	it('does not narrate your own plain messages', () => {
		Notification.setTTSNotificationMode(TTSNotificationMode.FOR_ALL_CHANNELS);
		expect(spoken({authorId: ME})).toBe(false);
	});

	it('does not narrate your own system messages', () => {
		Notification.setTTSNotificationMode(TTSNotificationMode.FOR_ALL_CHANNELS);
		expect(spoken({authorId: ME, type: MessageTypes.CHANNEL_PINNED_MESSAGE})).toBe(false);
	});

	it('narrates other channels only in every-channel mode', () => {
		Notification.setTTSNotificationMode(TTSNotificationMode.FOR_ALL_CHANNELS);
		expect(spoken({channelId: OTHER_CHANNEL})).toBe(true);
		Notification.setTTSNotificationMode(TTSNotificationMode.FOR_CURRENT_CHANNEL);
		expect(spoken({channelId: OTHER_CHANNEL})).toBe(false);
	});

	it('narrates the side-panel thread in current-channel mode', () => {
		Notification.setTTSNotificationMode(TTSNotificationMode.FOR_CURRENT_CHANNEL);
		state.threadId = THREAD_CHANNEL;
		expect(spoken({channelId: THREAD_CHANNEL})).toBe(true);
	});

	it('stays silent in every mode while self-deafened', () => {
		state.selfDeaf = true;
		for (const mode of [
			TTSNotificationMode.NEVER,
			TTSNotificationMode.FOR_CURRENT_CHANNEL,
			TTSNotificationMode.FOR_ALL_CHANNELS,
		]) {
			Notification.setTTSNotificationMode(mode);
			expect(spoken({tts: true})).toBe(false);
		}
	});

	it('stays silent in a muted channel', () => {
		state.mutedChannelIds.add(SELECTED_CHANNEL);
		expect(spoken({tts: true})).toBe(false);
	});
});
