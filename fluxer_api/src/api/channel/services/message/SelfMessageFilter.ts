// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {
	BulkDeleteSelfMessagesGuildFilterMode,
	BulkDeleteSelfMessagesScope,
} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import type {UserID} from '../../../BrandedTypes';
import type {Channel} from '../../../models/Channel';

export interface SelfMessageFilter {
	scope: BulkDeleteSelfMessagesScope;
	includeDms: boolean;
	includeDmsClosed: boolean;
	includeGroupDms: boolean;
	includeGuilds: boolean;
	guildFilterMode: BulkDeleteSelfMessagesGuildFilterMode;
	excludedGuildIds: ReadonlySet<string>;
	includedGuildIds: ReadonlySet<string>;
	startTimestamp: number | null;
	endTimestamp: number | null;
}

export interface SelfMessageEligibilityContext {
	currentGuildIds: ReadonlySet<string>;
	openDmChannelIds: ReadonlySet<string>;
}

export function isChannelEligible(
	channel: Channel,
	userId: UserID,
	filter: SelfMessageFilter,
	context: SelfMessageEligibilityContext,
): boolean {
	if (channel.type === ChannelTypes.DM_PERSONAL_NOTES) {
		return false;
	}
	const channelIdStr = channel.id.toString();
	if (channel.type === ChannelTypes.DM) {
		if (filter.scope === 'inaccessible_only') {
			return false;
		}
		const isOpen = context.openDmChannelIds.has(channelIdStr);
		return isOpen ? filter.includeDms : filter.includeDmsClosed;
	}
	if (channel.type === ChannelTypes.GROUP_DM) {
		const stillMember = channel.recipientIds.has(userId);
		if (filter.scope === 'inaccessible_only') {
			return !stillMember;
		}
		return filter.includeGroupDms && stillMember;
	}
	if (channel.guildId == null) {
		return false;
	}
	const guildIdStr = channel.guildId.toString();
	const isCurrentMember = context.currentGuildIds.has(guildIdStr);
	if (filter.scope === 'inaccessible_only') {
		return !isCurrentMember;
	}
	if (!filter.includeGuilds || !isCurrentMember) {
		return false;
	}
	if (filter.guildFilterMode === 'include_only') {
		return filter.includedGuildIds.has(guildIdStr);
	}
	return !filter.excludedGuildIds.has(guildIdStr);
}

export function isTimestampInWindow(timestampMs: number, filter: SelfMessageFilter): boolean {
	const start = filter.startTimestamp ?? Number.NEGATIVE_INFINITY;
	const end = filter.endTimestamp ?? Number.POSITIVE_INFINITY;
	return timestampMs >= start && timestampMs < end;
}
