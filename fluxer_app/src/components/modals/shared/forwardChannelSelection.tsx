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

import {useLingui} from '@lingui/react/macro';
import * as React from 'react';
import {ChannelTypes} from '~/Constants';
import type {ChannelRecord} from '~/records/ChannelRecord';
import ChannelStore from '~/stores/ChannelStore';
import GuildStore from '~/stores/GuildStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import * as ChannelUtils from '~/utils/ChannelUtils';

type TFn = (literals: TemplateStringsArray, ...placeholders: Array<unknown>) => string;

const FORWARDABLE_CHANNEL_TYPES = new Set<number>(
	Object.values(ChannelTypes)
		.filter((value) => typeof value === 'number' && value !== ChannelTypes.GUILD_CATEGORY)
		.map((value) => Number(value)),
);

export const getForwardChannelDisplayName = (channel: ChannelRecord, t?: TFn): string => {
	if (
		channel.type === ChannelTypes.DM_PERSONAL_NOTES ||
		channel.type === ChannelTypes.DM ||
		channel.type === ChannelTypes.GROUP_DM
	) {
		return ChannelUtils.getDMDisplayName(channel);
	}

	if (channel.name) return channel.name;

	if (!t) return `Channel ${channel.id}`;

	const id = channel.id;
	return t`Channel ${id}`;
};

export const getForwardChannelGuildName = (channel: ChannelRecord): string | null => {
	if (!channel.guildId) return null;
	const guild = GuildStore.getGuild(channel.guildId);
	return guild?.name ?? null;
};

export const getForwardChannelCategoryName = (channel: ChannelRecord): string | null => {
	if (!channel.parentId) return null;
	const category = ChannelStore.getChannel(channel.parentId);
	if (!category) return null;
	return category.name || null;
};

interface UseForwardChannelSelectionOptions {
	excludedChannelId: string;
	maxSelections?: number;
}

export const useForwardChannelSelection = ({
	excludedChannelId,
	maxSelections = 5,
}: UseForwardChannelSelectionOptions) => {
	const {t} = useLingui();
	const recentChannelIds = SelectedChannelStore.recentChannels;

	const [searchQuery, setSearchQuery] = React.useState('');
	const [selectedChannelIds, setSelectedChannelIds] = React.useState<Set<string>>(new Set());

	const allChannels = React.useMemo(() => {
		const channels = ChannelStore.allChannels.filter((channel) => FORWARDABLE_CHANNEL_TYPES.has(channel.type));

		return channels.sort((a, b) => {
			const aIsSource = a.id === excludedChannelId;
			const bIsSource = b.id === excludedChannelId;

			if (aIsSource && bIsSource) return 0;
			if (aIsSource) return 1;
			if (bIsSource) return -1;

			const aIndex = recentChannelIds.indexOf(a.id);
			const bIndex = recentChannelIds.indexOf(b.id);

			if (aIndex !== -1 && bIndex !== -1) return aIndex - bIndex;
			if (aIndex !== -1) return -1;
			if (bIndex !== -1) return 1;

			const aName = getForwardChannelDisplayName(a, t).toLowerCase();
			const bName = getForwardChannelDisplayName(b, t).toLowerCase();
			return aName.localeCompare(bName);
		});
	}, [recentChannelIds, excludedChannelId, t]);

	const filteredChannels = React.useMemo(() => {
		if (!searchQuery.trim()) return allChannels;

		const query = searchQuery.toLowerCase();
		return allChannels.filter((channel) => {
			const channelName = (channel.name || '').toLowerCase();
			const displayName = getForwardChannelDisplayName(channel, t).toLowerCase();
			const guildName = channel.guildId ? (GuildStore.getGuild(channel.guildId)?.name ?? '').toLowerCase() : '';

			if (channel.type === ChannelTypes.DM_PERSONAL_NOTES) {
				const notes = t`Personal Notes`.toLowerCase();
				if (notes.includes(query)) return true;
			}

			return displayName.includes(query) || channelName.includes(query) || guildName.includes(query);
		});
	}, [allChannels, searchQuery, t]);

	const handleToggleChannel = React.useCallback(
		(channelId: string) => {
			setSelectedChannelIds((prev) => {
				const next = new Set(prev);

				if (next.has(channelId)) {
					next.delete(channelId);
					return next;
				}

				if (next.size >= maxSelections) return prev;

				next.add(channelId);
				return next;
			});
		},
		[maxSelections],
	);

	const isChannelDisabled = React.useCallback(
		(channelId: string) => !selectedChannelIds.has(channelId) && selectedChannelIds.size >= maxSelections,
		[selectedChannelIds, maxSelections],
	);

	return {
		filteredChannels,
		handleToggleChannel,
		isChannelDisabled,
		searchQuery,
		selectedChannelIds,
		setSearchQuery,
		maxSelections,
	};
};
