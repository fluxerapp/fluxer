// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import TypingIndicator from '@app/features/typing/state/TypingIndicator';
import {autorun, type IReactionDisposer} from 'mobx';

const SELF_TYPING_REFRESH_MS = 5000;

class ShowMyselfTypingHelper {
	private intervalId: NodeJS.Timeout | null = null;
	private disposer: IReactionDisposer | null = null;
	private activeChannelId: string | null = null;

	start(): void {
		if (this.disposer) {
			return;
		}
		this.disposer = autorun(() => {
			const enabled = DeveloperOptions.showMyselfTyping;
			const channelId = SelectedChannel.currentChannelId;
			const userId = Authentication.currentUserId;
			const shouldMirror = Boolean(enabled && channelId && userId);
			if (!shouldMirror) {
				this.reset();
				return;
			}
			if (channelId !== this.activeChannelId) {
				this.activeChannelId = channelId!;
				this.trigger(channelId!, userId!);
				this.restartInterval(channelId!, userId!);
				return;
			}
			if (!this.intervalId) {
				this.restartInterval(channelId!, userId!);
			}
		});
	}

	stop(): void {
		this.reset();
		if (this.disposer) {
			this.disposer();
			this.disposer = null;
		}
	}

	private trigger(channelId: string, userId: string): void {
		TypingIndicator.startRemoteTyping(channelId, userId);
	}

	private restartInterval(channelId: string, userId: string): void {
		if (this.intervalId) {
			clearInterval(this.intervalId);
		}
		this.intervalId = setInterval(() => this.trigger(channelId, userId), SELF_TYPING_REFRESH_MS);
	}

	private reset(): void {
		if (this.intervalId) {
			clearInterval(this.intervalId);
			this.intervalId = null;
		}
		this.activeChannelId = null;
	}
}

export const showMyselfTypingHelper = new ShowMyselfTypingHelper();
