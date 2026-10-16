// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import Messages from '@app/features/messaging/state/MessagingMessages';
import Navigation from '@app/features/navigation/state/Navigation';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {ensureThreadLoaded} from '@app/features/threads/commands/ThreadCommands';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import ThreadPanel from '@app/features/threads/state/ThreadPanel';
import Notification from '@app/features/ui/state/Notification';
import {reaction} from 'mobx';

const logger = new Logger('NavigationSideEffects');

class NavigationSideEffects {
	private lastChannelId: string | null = null;
	private lastMessageId: string | null = null;
	private lastThreadKey: string | null = null;
	private disposer: (() => void) | null = null;

	initialize(): void {
		if (this.disposer) return;
		this.disposer = reaction(
			() => ({
				guildId: Navigation.guildId,
				channelId: Navigation.channelId,
				messageId: Navigation.messageId,
				threadId: Navigation.threadId,
				threadMessageId: Navigation.threadMessageId,
				threadsActive: Navigation.threadId != null && ThreadGuilds.isActive(Navigation.guildId),
			}),
			({guildId, channelId, messageId, threadId, threadMessageId, threadsActive}) => {
				this.handleRouteChange(guildId, channelId, messageId);
				this.handleThreadRouteChange(guildId, threadsActive ? threadId : null, threadMessageId);
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
		const createTarget = ThreadPanel.createTarget;
		if (createTarget && createTarget.parentId !== channelId) {
			ThreadPanel.closeCreate();
		}
		if (!channelId) return;
		logger.debug(`Route change: guild=${guildId}, channel=${channelId}, message=${messageId}`);
		Messages.handleChannelSelect({
			guildId: guildId ?? undefined,
			channelId,
			messageId: messageId ?? undefined,
		});
		Notification.handleChannelSelect({channelId});
	}

	handleGatewayReady(): void {
		const {guildId, threadId, threadMessageId} = Navigation;
		if (threadId == null || Channels.getChannel(threadId)) return;
		this.lastThreadKey = null;
		this.handleThreadRouteChange(guildId, ThreadGuilds.isActive(guildId) ? threadId : null, threadMessageId);
	}

	private handleThreadRouteChange(guildId: string | null, threadId: string | null, messageId: string | null): void {
		const key = threadId ? `${threadId}:${messageId ?? ''}` : null;
		if (key === this.lastThreadKey) return;
		this.lastThreadKey = key;
		if (!guildId || !threadId) return;
		void ensureThreadLoaded(guildId, threadId).then((loaded) => {
			if (!loaded || Navigation.threadId !== threadId) return;
			Messages.handleChannelSelect({guildId, channelId: threadId, messageId: messageId ?? undefined});
			Notification.handleChannelSelect({channelId: threadId});
		});
	}
}

export default new NavigationSideEffects();
