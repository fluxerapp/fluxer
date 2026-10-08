// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import type {Channel} from '@app/features/channel/models/Channel';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import Navigation from '@app/features/navigation/state/Navigation';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import ThreadPanel from '@app/features/threads/state/ThreadPanel';
import MobileLayout from '@app/features/ui/state/MobileLayout';

export function openThread(thread: Channel, messageId?: string): void {
	const guildId = thread.guildId;
	if (!guildId || !thread.parentId) return;
	ThreadPanel.closeCreate();
	if (MobileLayout.enabled) {
		NavigationCommands.selectChannel(guildId, thread.id, messageId);
		return;
	}
	const path = Routes.threadPanel(guildId, thread.parentId, thread.id, messageId);
	if (Navigation.threadId != null && Navigation.channelId === thread.parentId) {
		RouterUtils.replaceWith(path);
	} else {
		RouterUtils.transitionTo(path);
	}
}

export function openThreadFullView(thread: Channel, messageId?: string): void {
	if (!thread.guildId) return;
	ThreadPanel.closeCreate();
	NavigationCommands.selectChannel(thread.guildId, thread.id, messageId);
}

export function closeThreadPanel(): void {
	ThreadPanel.closeCreate();
	const guildId = Navigation.guildId;
	const channelId = Navigation.channelId;
	if (!guildId || !channelId || Navigation.threadId == null) return;
	RouterUtils.replaceWith(Routes.guildChannel(guildId, channelId));
}

export function openCreateThread(parent: Channel, messageId: string | null): void {
	const guildId = parent.guildId;
	if (!guildId) return;
	ThreadPanel.openCreate(parent.id, messageId);
	if (Navigation.channelId !== parent.id || Navigation.threadId != null) {
		RouterUtils.transitionTo(Routes.guildChannel(guildId, parent.id));
	}
}
