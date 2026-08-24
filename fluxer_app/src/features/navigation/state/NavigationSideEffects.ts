// SPDX-License-Identifier: AGPL-3.0-or-later

import Messages from '@app/features/messaging/state/MessagingMessages';
import Navigation from '@app/features/navigation/state/Navigation';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Notification from '@app/features/ui/state/Notification';
import {reaction} from 'mobx';

const logger = new Logger('NavigationSideEffects');

class NavigationSideEffects {
	private lastChannelId: string | null = null;
	private lastMessageId: string | null = null;
	private disposer: (() => void) | null = null;

	initialize(): void {
		if (this.disposer) return;
		this.disposer = reaction(
			() => ({
				guildId: Navigation.guildId,
				channelId: Navigation.channelId,
				messageId: Navigation.messageId,
			}),
			({guildId, channelId, messageId}) => {
				this.handleRouteChange(guildId, channelId, messageId);
			},
			{fireImmediately: true},
		);
	}

	private handleRouteChange(guildId: string | null, channelId: string | null, messageId: string | null): void {
		const channelChanged = channelId !== this.lastChannelId;
		const messageChanged = messageId !== this.lastMessageId;
		if (!channelChanged && !messageChanged) return;
		this.lastChannelId = channelId;
		this.lastMessageId = messageId;
		if (!channelId) return;
		logger.debug(`Route change: guild=${guildId}, channel=${channelId}, message=${messageId}`);
		Messages.handleChannelSelect({
			guildId: guildId ?? undefined,
			channelId,
			messageId: messageId ?? undefined,
		});
		Notification.handleChannelSelect({channelId});
	}

	destroy(): void {
		this.disposer?.();
		this.disposer = null;
	}
}

export default new NavigationSideEffects();
