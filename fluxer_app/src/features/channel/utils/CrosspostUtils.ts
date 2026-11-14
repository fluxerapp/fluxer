// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';

function communityNameFromAuthor(name: string): string {
	return name.replace(/\s#[^\s#]+$/, '').trim() || name;
}

export function getCrosspostSourceGuildId(message: Message): string | null {
	return message.messageReference?.guild_id ?? null;
}

export function getCrosspostSourceDisplayName(message: Message): string {
	const name = message.type === MessageTypes.CHANNEL_FOLLOW_ADD ? message.content : message.author.username;
	return communityNameFromAuthor(name);
}
