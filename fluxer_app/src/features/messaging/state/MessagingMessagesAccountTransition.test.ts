// SPDX-License-Identifier: AGPL-3.0-or-later
// @vitest-environment happy-dom

import {afterEach, describe, expect, it, vi} from 'vitest';

const mocks = vi.hoisted(() => ({
	selectedChannelId: null as string | null,
	gatewayReady: false,
	get: vi.fn(),
}));

vi.mock('@lingui/core/macro', () => ({
	msg: (descriptor: unknown) => descriptor,
	t: (descriptor: unknown) => descriptor,
}));
vi.mock('@lingui/react/macro', () => ({Trans: () => null, useLingui: () => ({i18n: {_: () => ''}})}));

vi.mock('@app/features/platform/transport/RestTransport', () => ({http: {get: mocks.get}}));

vi.mock('@app/features/gateway/transport/GatewayConnection', () => ({
	default: {
		get isReady() {
			return mocks.gatewayReady;
		},
		get isConnected() {
			return mocks.gatewayReady;
		},
		connectionEpoch: 1,
	},
}));

vi.mock('@app/features/navigation/state/SelectedChannel', () => ({
	default: {
		get currentChannelId() {
			return mocks.selectedChannelId;
		},
		selectedChannelIds: new Map<string, string>(),
	},
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
const {default: Messages} = await import('@app/features/messaging/state/MessagingMessages');
const MessageCommands = await import('@app/features/messaging/commands/MessageCommands');

const CHANNEL_ID = '1555752678099779590';

function respondThroughAdmission(): void {
	mocks.get.mockImplementation(async () => {
		const ticket = AccountScopedWork.begin();
		ticket.dispose();
		return {body: []};
	});
}

async function settle(): Promise<void> {
	for (let i = 0; i < 10; i++) {
		await Promise.resolve();
	}
	await new Promise((resolve) => setTimeout(resolve, 0));
}

describe('message hydration across an account transition', () => {
	afterEach(() => {
		Messages.handleSessionInvalidated();
		mocks.selectedChannelId = null;
		mocks.gatewayReady = false;
		mocks.get.mockReset();
	});

	it('loads the restored channel after READY arrives inside the transition', async () => {
		respondThroughAdmission();
		await AccountScopedWork.runSuspended(AccountScopedWorkTransitionReason.ACCOUNT_SWITCH, async () => {
			Messages.handleSessionInvalidated();
			mocks.selectedChannelId = CHANNEL_ID;
			mocks.gatewayReady = true;
			Messages.handleGatewayReady();
			await settle();
			expect(mocks.get).not.toHaveBeenCalled();
			expect(Messages.getMessages(CHANNEL_ID).error).toBe(false);
		});
		await settle();
		expect(mocks.get).toHaveBeenCalledTimes(1);
		const messages = Messages.getMessages(CHANNEL_ID);
		expect(messages.error).toBe(false);
		expect(messages.ready).toBe(true);
	});

	it('loads the restored channel when it is selected inside the transition on a ready gateway', async () => {
		respondThroughAdmission();
		await AccountScopedWork.runSuspended(AccountScopedWorkTransitionReason.ACCOUNT_SWITCH, async () => {
			mocks.gatewayReady = true;
			Messages.handleGatewayReady();
			mocks.selectedChannelId = CHANNEL_ID;
			Messages.handleChannelSelect({channelId: CHANNEL_ID});
			await settle();
			expect(mocks.get).not.toHaveBeenCalled();
			const held = Messages.getMessages(CHANNEL_ID);
			expect(held.error).toBe(false);
			expect(held.loadingMore).toBe(false);
		});
		await settle();
		expect(mocks.get).toHaveBeenCalledTimes(1);
		const messages = Messages.getMessages(CHANNEL_ID);
		expect(messages.error).toBe(false);
		expect(messages.ready).toBe(true);
	});

	it('does not churn message state for fetches requested while admission is suspended', async () => {
		respondThroughAdmission();
		await AccountScopedWork.runSuspended(AccountScopedWorkTransitionReason.ACCOUNT_SWITCH, async () => {
			mocks.selectedChannelId = CHANNEL_ID;
			Messages.getMessages(CHANNEL_ID);
			const version = Messages.version;
			await MessageCommands.fetchMessages(CHANNEL_ID, null, null, 50);
			await MessageCommands.fetchMessages(CHANNEL_ID, null, null, 50);
			expect(Messages.version).toBe(version);
			expect(mocks.get).not.toHaveBeenCalled();
		});
	});

	it('keeps a load interrupted by the transition out of the failed state and reloads it on release', async () => {
		mocks.selectedChannelId = CHANNEL_ID;
		mocks.gatewayReady = true;
		mocks.get.mockImplementationOnce(() => {
			const ticket = AccountScopedWork.begin();
			return new Promise((_resolve, reject) => {
				ticket.signal.addEventListener('abort', () => {
					ticket.dispose();
					reject(ticket.signal.reason);
				});
			});
		});
		const interrupted = MessageCommands.fetchMessages(CHANNEL_ID, null, null, 50);
		await settle();
		expect(Messages.getMessages(CHANNEL_ID).loadingMore).toBe(true);
		respondThroughAdmission();
		await AccountScopedWork.runSuspended(AccountScopedWorkTransitionReason.ACCOUNT_SWITCH, async () => {
			await interrupted;
			const held = Messages.getMessages(CHANNEL_ID);
			expect(held.error).toBe(false);
			expect(held.loadingMore).toBe(false);
		});
		await settle();
		expect(mocks.get).toHaveBeenCalledTimes(2);
		const messages = Messages.getMessages(CHANNEL_ID);
		expect(messages.error).toBe(false);
		expect(messages.ready).toBe(true);
	});

	it('still marks the channel failed for an ordinary request failure', async () => {
		mocks.selectedChannelId = CHANNEL_ID;
		mocks.gatewayReady = true;
		mocks.get.mockRejectedValue(new Error('boom'));
		await MessageCommands.fetchMessages(CHANNEL_ID, null, null, 50);
		expect(Messages.getMessages(CHANNEL_ID).error).toBe(true);
	});

	it('leaves the channel alone on release while the gateway is not ready', async () => {
		respondThroughAdmission();
		mocks.selectedChannelId = CHANNEL_ID;
		Messages.getMessages(CHANNEL_ID);
		await AccountScopedWork.runSuspended(AccountScopedWorkTransitionReason.LOGOUT, async () => {});
		await settle();
		expect(mocks.get).not.toHaveBeenCalled();
	});
});
