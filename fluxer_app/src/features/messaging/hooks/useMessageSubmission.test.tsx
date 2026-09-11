// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import {type SendMessageFunction, useMessageSubmission} from '@app/features/messaging/hooks/useMessageSubmission';
import {MessageFlags} from '@fluxer/constants/src/ChannelConstants';
import {act, createElement} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const messageCommands = vi.hoisted(() => ({
	reserveSend: vi.fn(() => true),
	send: vi.fn(() => Promise.resolve(null)),
	createOptimistic: vi.fn(),
	stopReply: vi.fn(),
}));
const draftCommands = vi.hoisted(() => ({deleteDraft: vi.fn()}));
const cloudUpload = vi.hoisted(() => ({
	getTextareaAttachments: vi.fn((): Array<unknown> => []),
	claimAttachmentsForMessage: vi.fn(() => []),
}));

vi.mock('@app/features/messaging/commands/MessageCommands', () => messageCommands);
vi.mock('@app/features/messaging/commands/DraftCommands', () => draftCommands);
vi.mock('@app/features/messaging/upload/CloudUpload', () => ({CloudUpload: cloudUpload}));
vi.mock('@app/features/messaging/models/MessagingMessage', () => ({
	Message: class {
		constructor(data: object) {
			Object.assign(this, data);
		}
		toJSON(): object {
			return {...this};
		}
	},
}));
vi.mock('@app/features/messaging/state/ChatInputSettings', () => ({default: {convertEmoticons: false}}));
vi.mock('@app/features/user/state/UserSettings', () => ({default: {getSanitizeUrls: () => false}}));
vi.mock('@app/features/messaging/utils/EmoticonConversionUtils', () => ({
	convertEmoticonsToEmoji: (content: string) => content,
}));
vi.mock('@app/features/user/state/Users', () => ({default: {getCurrentUser: () => ({toJSON: () => ({id: 'me'})})}}));
vi.mock('@app/features/permissions/state/Permission', () => ({default: {can: () => true}}));
vi.mock('@app/features/slowmode/state/Slowmode', () => ({default: {getSlowmodeRemaining: () => 0}}));
vi.mock('@app/features/slowmode/commands/SlowmodeCommands', () => ({
	prepareMessageSend: vi.fn(),
	recordPendingMessageSend: vi.fn(() => null),
	confirmMessageSend: vi.fn(),
	discardPendingMessageSend: vi.fn(),
}));
vi.mock('@app/features/slowmode/components/alerts/SlowmodeRateLimitedModal', () => ({
	SlowmodeRateLimitedModal: () => null,
}));
vi.mock('@app/features/ui/commands/ModalCommands', () => ({modal: (render: unknown) => render, push: vi.fn()}));
vi.mock('@app/features/typing/utils/TypingUtils', () => ({TypingUtils: {clear: vi.fn()}}));
vi.mock('@app/features/platform/utils/ComponentBus', () => ({ComponentBus: {dispatch: vi.fn()}}));
vi.mock('@lingui/core/macro', () => ({msg: (value: unknown) => value}));
vi.mock('@lingui/react/macro', () => ({useLingui: () => ({i18n: {_: () => ''}})}));

const channel = {id: 'c'} as unknown as Channel;

let host: HTMLDivElement;
let root: Root;

function renderSendMessage(): SendMessageFunction {
	let sendMessage: SendMessageFunction | null = null;
	const Probe = () => {
		sendMessage = useMessageSubmission({channel, referencedMessage: null, replyingMessage: null}).sendMessage;
		return null;
	};
	act(() => {
		root.render(createElement(Probe));
	});
	return sendMessage!;
}

beforeEach(() => {
	host = document.createElement('div');
	document.body.append(host);
	root = createRoot(host);
});

afterEach(() => {
	act(() => {
		root.unmount();
	});
	document.body.replaceChildren();
	vi.clearAllMocks();
	cloudUpload.getTextareaAttachments.mockReturnValue([]);
	messageCommands.reserveSend.mockReturnValue(true);
});

describe('useMessageSubmission', () => {
	it('sends nothing and keeps the draft for a message that is only @silent', () => {
		const sendMessage = renderSendMessage();
		expect(sendMessage('@silent', false)).toBe(false);
		expect(messageCommands.reserveSend).not.toHaveBeenCalled();
		expect(draftCommands.deleteDraft).not.toHaveBeenCalled();
		expect(messageCommands.send).not.toHaveBeenCalled();
	});

	it('sends nothing and keeps the draft for a /tts command that is only @silent', () => {
		const sendMessage = renderSendMessage();
		expect(sendMessage('@silent', false, true)).toBe(false);
		expect(messageCommands.reserveSend).not.toHaveBeenCalled();
		expect(draftCommands.deleteDraft).not.toHaveBeenCalled();
		expect(messageCommands.send).not.toHaveBeenCalled();
	});

	it('sends nothing and keeps the draft for a sticker send that is only @silent', () => {
		const sendMessage = renderSendMessage();
		expect(sendMessage('@silent', false, [])).toBe(false);
		expect(messageCommands.reserveSend).not.toHaveBeenCalled();
		expect(draftCommands.deleteDraft).not.toHaveBeenCalled();
	});

	it('sends an attachment captioned only by @silent with no content and the silent flag', () => {
		cloudUpload.getTextareaAttachments.mockReturnValue([{id: 1}]);
		const sendMessage = renderSendMessage();
		expect(sendMessage('@silent', false)).toBe(true);
		expect(draftCommands.deleteDraft).toHaveBeenCalledWith('c');
		expect(messageCommands.send).toHaveBeenCalledWith(
			'c',
			expect.objectContaining({content: '', flags: MessageFlags.SUPPRESS_NOTIFICATIONS}),
		);
	});

	it('sends the text after @silent with the silent flag', () => {
		const sendMessage = renderSendMessage();
		expect(sendMessage('@silent hi', false)).toBe(true);
		expect(draftCommands.deleteDraft).toHaveBeenCalledWith('c');
		expect(messageCommands.send).toHaveBeenCalledWith(
			'c',
			expect.objectContaining({content: 'hi', flags: MessageFlags.SUPPRESS_NOTIFICATIONS}),
		);
	});
});
