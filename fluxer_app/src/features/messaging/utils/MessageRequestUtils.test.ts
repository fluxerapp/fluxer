// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildMessageCreateRequest,
	buildMessageEditRequest,
	type ComposerSubmitSignals,
	canSubmitComposerContent,
	canSubmitMessage,
	getComposerMessageContent,
	hasVisibleMessageContent,
	normalizeMessageContent,
} from '@app/features/messaging/utils/MessageRequestUtils';
import {MessageFlags} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, describe, expect, it, vi} from 'vitest';

const chatInputSettings = vi.hoisted(() => ({convertEmoticons: false}));

vi.mock('@app/features/messaging/state/ChatInputSettings', () => ({default: chatInputSettings}));
vi.mock('@app/features/user/state/UserSettings', () => ({default: {getSanitizeUrls: () => false}}));
vi.mock('@app/features/messaging/utils/EmoticonConversionUtils', () => ({
	convertEmoticonsToEmoji: (content: string) => content.replaceAll(':)', '\u{1F642}'),
}));

afterEach(() => {
	chatInputSettings.convertEmoticons = false;
});

describe('normalizeMessageContent', () => {
	it('still strips @silent followed by a space and suppresses notifications', () => {
		expect(normalizeMessageContent('@silent hello')).toEqual({
			content: 'hello',
			flags: MessageFlags.SUPPRESS_NOTIFICATIONS,
		});
	});

	it('strips @silent on its own and suppresses notifications', () => {
		expect(normalizeMessageContent('@silent')).toEqual({content: '', flags: MessageFlags.SUPPRESS_NOTIFICATIONS});
	});

	it.each([
		['a newline', '\n'],
		['a no-break space', '\u00a0'],
		['a tab', '\t'],
	])('strips @silent followed by %s and suppresses notifications', (_name, whitespace) => {
		expect(normalizeMessageContent(`@silent${whitespace}hello`)).toEqual({
			content: 'hello',
			flags: MessageFlags.SUPPRESS_NOTIFICATIONS,
		});
	});

	it('strips leading whitespace before @silent', () => {
		expect(normalizeMessageContent('  @silent hello')).toEqual({
			content: 'hello',
			flags: MessageFlags.SUPPRESS_NOTIFICATIONS,
		});
	});

	it('still strips only one whitespace character and one @silent', () => {
		expect(normalizeMessageContent('@silent  hello')).toEqual({
			content: ' hello',
			flags: MessageFlags.SUPPRESS_NOTIFICATIONS,
		});
		expect(normalizeMessageContent('@silent @silent hello')).toEqual({
			content: '@silent hello',
			flags: MessageFlags.SUPPRESS_NOTIFICATIONS,
		});
	});

	it('still drops invisible content left after @silent', () => {
		expect(normalizeMessageContent('@silent \u200b')).toEqual({
			content: '',
			flags: MessageFlags.SUPPRESS_NOTIFICATIONS,
		});
	});

	it.each([
		'@silently hello',
		'@Silent hello',
		'hello @silent',
		'hello',
	])('still leaves %j as it is with no flags', (content) => {
		expect(normalizeMessageContent(content)).toEqual({content, flags: 0});
	});

	it('still converts emoticons after stripping @silent', () => {
		chatInputSettings.convertEmoticons = true;
		expect(normalizeMessageContent('@silent :)')).toEqual({
			content: '\u{1F642}',
			flags: MessageFlags.SUPPRESS_NOTIFICATIONS,
		});
	});
});

describe('canSubmitMessage', () => {
	it('rejects a message that is only @silent', () => {
		expect(canSubmitMessage('@silent', false)).toBe(false);
		expect(canSubmitMessage('@silent\n', false)).toBe(false);
		expect(canSubmitMessage('@silent \u200b', false)).toBe(false);
	});

	it('rejects blank content with nothing attached', () => {
		expect(canSubmitMessage('   ', false)).toBe(false);
	});

	it('accepts @silent with attachments, a sticker or a favourite meme', () => {
		expect(canSubmitMessage('@silent', true)).toBe(true);
	});

	it('accepts visible content with or without @silent', () => {
		expect(canSubmitMessage('@silent hi', false)).toBe(true);
		expect(canSubmitMessage('hello', false)).toBe(true);
	});
});

function composerSignals(overrides: Partial<ComposerSubmitSignals> = {}): ComposerSubmitSignals {
	return {
		inputDisabled: false,
		isSubmissionBlockedBySlowmode: false,
		isOverCharacterLimit: false,
		hasMessageContent: false,
		hasAttachments: false,
		hasPendingSticker: false,
		isEditingMessageOnMobile: false,
		...overrides,
	};
}

describe('canSubmitComposerContent', () => {
	it('refuses an empty composer that is not editing anything', () => {
		expect(canSubmitComposerContent(composerSignals())).toBe(false);
	});

	it('accepts text, attachments and stickers on their own', () => {
		expect(canSubmitComposerContent(composerSignals({hasMessageContent: true}))).toBe(true);
		expect(canSubmitComposerContent(composerSignals({hasAttachments: true}))).toBe(true);
		expect(canSubmitComposerContent(composerSignals({hasPendingSticker: true}))).toBe(true);
	});

	it('accepts an emptied edit on mobile so that it can ask to delete the message', () => {
		expect(canSubmitComposerContent(composerSignals({isEditingMessageOnMobile: true}))).toBe(true);
	});

	it('refuses a disabled, slowed or over-long composer even while editing on mobile', () => {
		expect(canSubmitComposerContent(composerSignals({isEditingMessageOnMobile: true, inputDisabled: true}))).toBe(
			false,
		);
		expect(
			canSubmitComposerContent(composerSignals({isEditingMessageOnMobile: true, isSubmissionBlockedBySlowmode: true})),
		).toBe(false);
		expect(
			canSubmitComposerContent(composerSignals({isEditingMessageOnMobile: true, isOverCharacterLimit: true})),
		).toBe(false);
	});
});

describe('getComposerMessageContent', () => {
	it('measures the content that will be sent when not editing on mobile', () => {
		expect(getComposerMessageContent('@silent', false)).toBe('');
		expect(getComposerMessageContent('@silent hello', false)).toBe('hello');
		expect(hasVisibleMessageContent(getComposerMessageContent('@silent', false))).toBe(false);
	});

	it('strips @silent without running the outgoing conversions', () => {
		chatInputSettings.convertEmoticons = true;
		expect(getComposerMessageContent('@silent :)', false)).toBe(':)');
	});

	it('keeps @silent as text when editing on mobile', () => {
		expect(getComposerMessageContent('@silent', true)).toBe('@silent');
		expect(getComposerMessageContent('@silent hello', true)).toBe('@silent hello');
		expect(hasVisibleMessageContent(getComposerMessageContent('@silent', true))).toBe(true);
	});
});

describe('buildMessageCreateRequest', () => {
	it('still omits empty content but keeps the silent flag for an attachment-only message', () => {
		const attachment = {id: '1', filename: 'a.png', title: 'a.png'};
		expect(
			buildMessageCreateRequest({
				content: '',
				nonce: 'n',
				attachments: [attachment],
				flags: MessageFlags.SUPPRESS_NOTIFICATIONS,
			}),
		).toEqual({nonce: 'n', attachments: [attachment], flags: MessageFlags.SUPPRESS_NOTIFICATIONS});
	});

	it('still sends a /tts message after @silent as silent text-to-speech', () => {
		const {content, flags} = normalizeMessageContent('@silent hi');
		expect(buildMessageCreateRequest({content, nonce: 'n', flags, tts: true})).toEqual({
			content: 'hi',
			nonce: 'n',
			flags: MessageFlags.SUPPRESS_NOTIFICATIONS,
			tts: true,
		});
	});
});

describe('buildMessageEditRequest', () => {
	it('still sends @silent verbatim in an edit with no flags', () => {
		expect(buildMessageEditRequest({content: '@silent hello'})).toEqual({content: '@silent hello'});
		expect(buildMessageEditRequest({content: '@silent'})).toEqual({content: '@silent'});
	});
});
