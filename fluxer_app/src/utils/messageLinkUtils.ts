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

import {ME} from '~/Constants';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';

interface BuildMessageLinkOptions {
	guildId?: string | null;
	channelId: string;
	messageId: string;
	includeProtocol?: boolean;
}

const DEFAULT_PROTOCOL = 'https:';

interface BuildChannelLinkOptions {
	guildId?: string | null;
	channelId: string;
	includeProtocol?: boolean;
}

const getProtocol = (includeProtocol?: boolean) => {
	if (!includeProtocol) {
		return DEFAULT_PROTOCOL;
	}

	if (typeof location !== 'undefined' && location.protocol) {
		return location.protocol;
	}

	return DEFAULT_PROTOCOL;
};

export function buildMessageJumpLink({
	guildId,
	channelId,
	messageId,
	includeProtocol = true,
}: BuildMessageLinkOptions): string {
	const resolvedGuildId = guildId ?? ME;
	const protocol = getProtocol(includeProtocol);

	return `${protocol}//${RuntimeConfigStore.marketingHost}/channels/${resolvedGuildId}/${channelId}/${messageId}`;
}

export function buildChannelLink({guildId, channelId, includeProtocol = true}: BuildChannelLinkOptions): string {
	const resolvedGuildId = guildId ?? ME;
	const protocol = getProtocol(includeProtocol);

	return `${protocol}//${RuntimeConfigStore.marketingHost}/channels/${resolvedGuildId}/${channelId}`;
}
