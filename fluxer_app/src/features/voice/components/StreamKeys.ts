// SPDX-License-Identifier: AGPL-3.0-or-later

import {ME} from '@fluxer/constants/src/AppConstants';

interface ParsedStreamKey {
	guildId: string | null;
	channelId: string | null;
	connectionId: string;
}

export function getStreamKey(
	guildId: string | null | undefined,
	channelId: string | null | undefined,
	connectionId: string,
): string {
	const normalizedGuildId = guildId === ME ? null : guildId;
	if (channelId && normalizedGuildId) return `${normalizedGuildId}:${channelId}:${connectionId}`;
	if (channelId) return `dm:${channelId}:${connectionId}`;
	return `stream:${connectionId}`;
}

export function parseStreamKey(streamKey: string): ParsedStreamKey | null {
	const parts = streamKey.split(':');
	if (parts.length === 2 && parts[0] === 'stream') {
		return {guildId: null, channelId: null, connectionId: parts[1]};
	}
	if (parts.length === 3 && parts[0] === 'dm') {
		return {guildId: null, channelId: parts[1], connectionId: parts[2]};
	}
	if (parts.length === 3 && parts[0] === ME) {
		return {guildId: null, channelId: parts[1], connectionId: parts[2]};
	}
	if (parts.length === 3) {
		return {guildId: parts[0], channelId: parts[1], connectionId: parts[2]};
	}
	return null;
}
