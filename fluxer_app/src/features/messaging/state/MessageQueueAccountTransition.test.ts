// SPDX-License-Identifier: AGPL-3.0-or-later
// @vitest-environment happy-dom

import {afterEach, describe, expect, it, vi} from 'vitest';

const mocks = vi.hoisted(() => ({
	accountKey: 'https://one.example/api::100' as string | null,
	post: vi.fn(),
}));

vi.mock('@lingui/core/macro', () => ({
	msg: (descriptor: unknown) => descriptor,
	t: (descriptor: unknown) => descriptor,
}));
vi.mock('@lingui/react/macro', () => ({Trans: () => null, useLingui: () => ({i18n: {_: () => ''}})}));

vi.mock('@app/features/platform/transport/RestTransport', () => ({http: {post: mocks.post, get: vi.fn()}}));

vi.mock('@app/features/platform/state/AuthSession', () => ({
	default: {
		get currentAccountKey() {
			return mocks.accountKey;
		},
		userId: null,
	},
}));

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

const {AccountScopedWork, AccountScopedWorkTransitionReason} = await import(
	'@app/features/platform/state/AccountScopedWork'
);
const MessageCommands = await import('@app/features/messaging/commands/MessageCommands');
const {default: ChannelSearch} = await import('@app/features/channel/state/ChannelSearch');
const {default: ChannelSticker} = await import('@app/features/channel/state/ChannelSticker');
const {default: MessageEdit} = await import('@app/features/messaging/state/MessageEdit');
const {default: Drafts} = await import('@app/features/messaging/state/MessagingDrafts');
const {default: MessageEditMobile} = await import('@app/features/messaging/state/MessageEditMobile');
const {default: MessageFocus} = await import('@app/features/messaging/state/MessageFocus');
const {default: MessageReply} = await import('@app/features/messaging/state/MessageReply');
const {default: TextareaSelection} = await import('@app/features/messaging/state/TextareaSelection');
const {CloudUpload} = await import('@app/features/messaging/upload/CloudUpload');
const {default: KeyboardMode} = await import('@app/features/ui/state/KeyboardMode');

const ACCOUNT_A = 'https://one.example/api::100';
const ACCOUNT_B = 'https://one.example/api::200';
const CHANNEL_ID = '1555752678099779590';
const MESSAGE_ID = '1555923739894349824';

interface Deferred<T> {
	readonly promise: Promise<T>;
	readonly resolve: (value: T) => void;
}

function deferred<T>(): Deferred<T> {
	let resolve: (value: T) => void = () => undefined;
	const promise = new Promise<T>((settle) => {
		resolve = settle;
	});
	return {promise, resolve};
}

async function settle(): Promise<void> {
	for (let i = 0; i < 10; i++) {
		await Promise.resolve();
	}
	await new Promise((resolve) => setTimeout(resolve, 5));
}

function send(nonce: string, content: string): Promise<unknown> {
	expect(MessageCommands.reserveSend(CHANNEL_ID, nonce)).toBe(true);
	return MessageCommands.send(CHANNEL_ID, {content, nonce});
}

describe('composer state across an account transition', () => {
	afterEach(() => {
		mocks.accountKey = ACCOUNT_A;
		mocks.post.mockReset();
	});

	it('never sends a message queued by the previous account', async () => {
		const first = deferred<{body: {id: string; channel_id: string}}>();
		mocks.post.mockImplementationOnce(() => first.promise);
		mocks.post.mockResolvedValue({body: {id: '2', channel_id: CHANNEL_ID}});
		const firstSend = send('1001', 'first message from account A');
		const queuedSend = send('1002', 'queued message from account A');
		await settle();
		expect(mocks.post).toHaveBeenCalledTimes(1);

		mocks.accountKey = ACCOUNT_B;
		first.resolve({body: {id: '1', channel_id: CHANNEL_ID}});
		await firstSend;
		await settle();

		expect(await queuedSend).toBeNull();
		expect(mocks.post).toHaveBeenCalledTimes(1);
	});

	it('drops every pending upload with the composer and keeps the typed draft when the account changes', async () => {
		const otherChannelId = '1555752678099779591';
		Drafts.createDraft(CHANNEL_ID, 'draft typed by account A');
		await CloudUpload.addFiles(CHANNEL_ID, [new File(['one'], 'pending-one.txt', {type: 'text/plain'})]);
		await CloudUpload.addFiles(otherChannelId, [new File(['two'], 'pending-two.txt', {type: 'text/plain'})]);
		const claimed = CloudUpload.claimAttachmentsForMessage(CHANNEL_ID, '2001', undefined, {
			content: 'message being sent by account A',
		});
		expect(claimed).toHaveLength(1);
		expect(CloudUpload.getMessageUpload('2001')).not.toBeNull();
		expect(CloudUpload.getTextareaAttachments(otherChannelId)).toHaveLength(1);

		await AccountScopedWork.runSuspended(AccountScopedWorkTransitionReason.ACCOUNT_SWITCH, async () => {});

		expect(CloudUpload.getTextareaAttachments(CHANNEL_ID)).toHaveLength(0);
		expect(CloudUpload.getTextareaAttachments(otherChannelId)).toHaveLength(0);
		expect(CloudUpload.getMessageUpload('2001')).toBeNull();
		expect(Drafts.getDraft(CHANNEL_ID)).toBe('draft typed by account A');
		Drafts.deleteDraft(CHANNEL_ID);
	});

	it('clears reply, edit, search, sticker, selection, attachment and message focus state when the account changes', async () => {
		MessageEdit.startEditing(CHANNEL_ID, MESSAGE_ID, 'edit typed by account A');
		MessageEditMobile.startEditingMobile(CHANNEL_ID, MESSAGE_ID);
		ChannelSearch.setSearchInput(CHANNEL_ID, 'query typed by account A', []);
		ChannelSearch.setActiveSearch(CHANNEL_ID, 'query typed by account A', []);
		ChannelSticker.pendingStickers.set(CHANNEL_ID, {id: '1'} as never);
		MessageReply.replyingMessageIds[CHANNEL_ID] = {messageId: MESSAGE_ID, mentioning: true, snapshot: {} as never};
		TextareaSelection.setChannelSelection(CHANNEL_ID, {anchor: 1, focus: 1} as never);
		await CloudUpload.addFiles(CHANNEL_ID, [new File(['attachment'], 'from-account-a.txt', {type: 'text/plain'})]);
		expect(CloudUpload.getTextareaAttachments(CHANNEL_ID)).toHaveLength(1);
		KeyboardMode.enterKeyboardMode(false);
		MessageFocus.focusMessage(CHANNEL_ID, MESSAGE_ID);
		expect(MessageFocus.focusedMessageId).toBe(MESSAGE_ID);

		await AccountScopedWork.runSuspended(AccountScopedWorkTransitionReason.ACCOUNT_SWITCH, async () => {});

		expect(MessageEdit.getEditingMessageId(CHANNEL_ID)).toBeNull();
		expect(MessageEdit.getDraftContent(MESSAGE_ID)).toBeNull();
		expect(MessageEditMobile.getEditingMobileMessageId(CHANNEL_ID)).toBeNull();
		expect(ChannelSearch.getContext(CHANNEL_ID).searchQuery).toBe('');
		expect(ChannelSearch.getContext(CHANNEL_ID).isSearchActive).toBe(false);
		expect(ChannelSticker.getPendingSticker(CHANNEL_ID)).toBeNull();
		expect(MessageReply.getReplyingMessage(CHANNEL_ID)).toBeNull();
		expect(TextareaSelection.getChannelSelection(CHANNEL_ID)).toBeNull();
		expect(CloudUpload.getTextareaAttachments(CHANNEL_ID)).toHaveLength(0);
		expect(MessageFocus.focusedMessageId).toBeNull();
		expect(MessageFocus.focusedChannelId).toBeNull();
	});
});
