// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
import {deferUntilModulesLoaded} from '@app/features/platform/utils/DeferUntilModulesLoaded';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import {autorun, makeAutoObservable} from 'mobx';

class MessageFocus {
	focusedChannelId: string | null = null;
	focusedChannel: Channel | null = null;
	focusedMessageId: string | null = null;
	focusedMessage: Message | null = null;
	retainFocus = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		deferUntilModulesLoaded(() => {
			autorun(() => {
				if (!KeyboardMode.keyboardModeEnabled) {
					this.clearFocus();
				}
			});
		});
	}

	focusMessage(channelId: string, messageId: string, message?: Message, channel?: Channel | null): void {
		if (!KeyboardMode.keyboardModeEnabled) {
			return;
		}
		if (this.focusedChannelId === channelId && this.focusedMessageId === messageId) {
			this.retainFocus = false;
			if (channel) {
				this.focusedChannel = channel;
			}
			if (message) {
				this.focusedMessage = message;
			}
			return;
		}
		this.focusedChannelId = channelId;
		this.focusedChannel = channel ?? null;
		this.focusedMessageId = messageId;
		this.focusedMessage = message ?? null;
		this.retainFocus = false;
	}

	blurMessage(channelId: string, messageId: string): void {
		if (this.focusedChannelId !== channelId || this.focusedMessageId !== messageId) {
			return;
		}
		if (this.retainFocus) {
			return;
		}
		this.clearFocus();
	}

	holdContextFocus(channelId: string, messageId: string, message?: Message, channel?: Channel | null): void {
		if (!KeyboardMode.keyboardModeEnabled) {
			return;
		}
		this.focusMessage(channelId, messageId, message, channel);
		this.retainFocus = true;
	}

	releaseContextFocus(channelId: string, messageId: string): void {
		if (this.focusedChannelId === channelId && this.focusedMessageId === messageId && this.retainFocus) {
			this.retainFocus = false;
		}
	}

	clearFocusedMessageIfMatches(channelId: string, messageId: string): void {
		if (this.focusedChannelId === channelId && this.focusedMessageId === messageId) {
			this.clearFocus();
		}
	}

	clearFocus(): void {
		this.focusedChannelId = null;
		this.focusedChannel = null;
		this.focusedMessageId = null;
		this.focusedMessage = null;
		this.retainFocus = false;
	}

	getFocusedChannel(): Channel | null {
		if (this.focusedChannel && this.focusedChannelId === this.focusedChannel.id) {
			return this.focusedChannel;
		}
		return null;
	}

	getFocusedMessage(): Message | null {
		if (
			this.focusedMessage &&
			this.focusedMessageId === this.focusedMessage.id &&
			this.focusedChannelId === this.focusedMessage.channelId
		) {
			return this.focusedMessage;
		}
		if (!this.focusedChannelId || !this.focusedMessageId) {
			return null;
		}
		return Messages.getMessage(this.focusedChannelId, this.focusedMessageId) ?? null;
	}
}

const messageFocus = new MessageFocus();

AccountScopedWork.registerCancellation(() => messageFocus.clearFocus());

export default messageFocus;
