/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import * as MessageActionCreators from '~/actions/MessageActionCreators';
import {type JumpTypes, ME} from '~/Constants';
import {Routes} from '~/Routes';
import ChannelStore from '~/stores/ChannelStore';
import * as RouterUtils from '~/utils/RouterUtils';

interface MessageJumpOptions {
	flash?: boolean;
	offset?: number;
	returnTargetId?: string;
	jumpType?: JumpTypes;
	viewContext?: 'favorites';
}

export const buildMessagePath = (
	channelId: string,
	messageId: string,
	viewContext?: MessageJumpOptions['viewContext'],
): string => {
	if (viewContext === 'favorites') {
		return Routes.favoritesChannelMessage(channelId, messageId);
	}

	const channel = ChannelStore.getChannel(channelId);
	const guildId = channel?.guildId;

	if (guildId && guildId !== ME) {
		return Routes.channelMessage(guildId, channelId, messageId);
	}

	return Routes.dmChannelMessage(channelId, messageId);
};

export const goToMessage = (channelId: string, messageId: string, options?: MessageJumpOptions): void => {
	const path = buildMessagePath(channelId, messageId, options?.viewContext);
	RouterUtils.transitionTo(path);
	MessageActionCreators.jumpToMessage(
		channelId,
		messageId,
		options?.flash ?? true,
		options?.offset,
		options?.returnTargetId,
		options?.jumpType,
	);
};

export const parseMessagePath = (path: string): {channelId: string; messageId: string} | null => {
	const parts = path.split('/').filter(Boolean);
	if (parts.length < 4) return null;
	if (parts[0] !== 'channels') return null;

	const channelId = parts[2];
	const messageId = parts[3];

	if (!channelId || !messageId) return null;
	return {channelId, messageId};
};
