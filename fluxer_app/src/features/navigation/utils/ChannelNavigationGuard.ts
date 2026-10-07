// SPDX-License-Identifier: AGPL-3.0-or-later

import * as LinkChannelCommands from '@app/features/channel/commands/LinkChannelCommands';
import Channels from '@app/features/channel/state/Channels';
import {resolveDocumentURLFromRoot} from '@app/features/platform/URLOriginUtils';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';

export interface ChannelNavigationTarget {
	guildId: string | null;
	channelId: string;
	messageId: string | null;
}

export function parseChannelNavigationPath(path: string): ChannelNavigationTarget | null {
	let url: URL;
	try {
		url = resolveDocumentURLFromRoot(path);
	} catch {
		return null;
	}
	const segments = url.pathname.split('/').filter(Boolean);
	if (segments[0] !== 'channels') {
		return null;
	}
	const [, guildId, channelId, messageId] = segments;
	if (!guildId || !channelId) {
		return null;
	}
	if (guildId === ME && segments.length > 4) {
		return null;
	}
	if (guildId !== ME && segments.length > 4) {
		return null;
	}
	return {
		guildId,
		channelId,
		messageId: messageId ?? null,
	};
}

export function tryInterceptChannelNavigationPath(path: string): boolean {
	const target = parseChannelNavigationPath(path);
	if (!target) {
		return false;
	}
	const channel = Channels.getChannel(target.channelId);
	if (!channel || channel.isPrivate()) {
		return false;
	}
	if (
		target.guildId !== '@favorites' &&
		target.guildId !== ME &&
		channel.guildId &&
		channel.guildId !== target.guildId
	) {
		return false;
	}
	if (channel.type === ChannelTypes.GUILD_CATEGORY) {
		return false;
	}
	if (channel.type === ChannelTypes.GUILD_LINK) {
		return LinkChannelCommands.openLinkChannel(channel);
	}
	return false;
}
