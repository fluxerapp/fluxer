// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ForwardChannelIndex,
	type ForwardChannelOption,
	type ForwardMessageMediaSelection,
	resolveForwardMessageMediaSelection,
} from '@app/features/app/components/dialogs/shared/ForwardChannelIndex';
import {useForwardChannelObservations} from '@app/features/app/components/dialogs/shared/UseForwardChannelObservations';
import {useShallowStableArray} from '@app/features/app/hooks/useShallowStableArray';
import Channels from '@app/features/channel/state/Channels';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import Users from '@app/features/user/state/Users';
import {useLingui} from '@lingui/react/macro';
import {type Dispatch, type SetStateAction, useCallback, useMemo, useState} from 'react';

interface UseForwardChannelSelectionOptions {
	readonly excludedChannelId: string;
	readonly message: Message;
	readonly maxSelections?: number;
	readonly mediaSelection?: ForwardMessageMediaSelection;
}

export interface ForwardChannelSelectionState {
	readonly filteredChannels: ReadonlyArray<ForwardChannelOption>;
	readonly handleToggleChannel: (channelId: string) => void;
	readonly isChannelSelectionDisabled: (option: ForwardChannelOption) => boolean;
	readonly maxSelections: number;
	readonly searchQuery: string;
	readonly selectedChannelIds: ReadonlySet<string>;
	readonly setSearchQuery: Dispatch<SetStateAction<string>>;
	readonly slowmodeActiveSelectedChannelOptions: ReadonlyArray<ForwardChannelOption>;
	readonly slowmodeEnabledSelectedChannelOptions: ReadonlyArray<ForwardChannelOption>;
}

function toggleForwardChannelSelection(
	channelId: string,
	maxSelections: number,
	previousChannelIds: Set<string>,
): Set<string> {
	const nextChannelIds = new Set(previousChannelIds);
	if (nextChannelIds.has(channelId)) {
		nextChannelIds.delete(channelId);
		return nextChannelIds;
	}
	if (nextChannelIds.size >= maxSelections) return previousChannelIds;
	nextChannelIds.add(channelId);
	return nextChannelIds;
}

export function useForwardChannelSelection({
	excludedChannelId,
	message,
	maxSelections = 5,
	mediaSelection,
}: UseForwardChannelSelectionOptions): ForwardChannelSelectionState {
	const {i18n} = useLingui();
	const locale = i18n.locale;
	const allKnownChannels = useShallowStableArray(Channels.allChannels);
	const recentChannelIds = useShallowStableArray(SelectedChannel.recentChannels);
	const currentUser = Users.currentUser;
	const currentUserId = currentUser ? currentUser.id : null;
	const resolvedMediaSelection = useMemo(
		() => resolveForwardMessageMediaSelection({message, override: mediaSelection}),
		[message, mediaSelection],
	);
	const observations = useForwardChannelObservations({channels: allKnownChannels, currentUserId, i18n});
	const channelIndex = useMemo(
		() =>
			new ForwardChannelIndex({
				excludedChannelId,
				i18n,
				mediaSelection: resolvedMediaSelection,
				observations,
				recentChannelIds,
			}),
		[excludedChannelId, i18n, locale, observations, recentChannelIds, resolvedMediaSelection],
	);
	const [searchQuery, setSearchQuery] = useState('');
	const [selectedChannelIds, setSelectedChannelIds] = useState<Set<string>>(new Set());
	const filteredChannels = useMemo(() => channelIndex.filter(searchQuery), [channelIndex, searchQuery]);
	const handleToggleChannel = useCallback(
		(channelId: string) => {
			setSelectedChannelIds((previousChannelIds) =>
				toggleForwardChannelSelection(channelId, maxSelections, previousChannelIds),
			);
		},
		[maxSelections],
	);
	const isChannelSelectionDisabled = useCallback(
		(option: ForwardChannelOption) => channelIndex.isSelectionDisabled({maxSelections, option, selectedChannelIds}),
		[maxSelections, channelIndex, selectedChannelIds],
	);
	const selectedChannelOptions = useMemo(
		() => channelIndex.select(selectedChannelIds),
		[channelIndex, selectedChannelIds],
	);
	const slowmodeEnabledSelectedChannelOptions = useMemo(
		() => selectedChannelOptions.filter((option) => option.slowmodeEnabled),
		[selectedChannelOptions],
	);
	const slowmodeActiveSelectedChannelOptions = useMemo(
		() => selectedChannelOptions.filter((option) => option.slowmodeRemainingMs > 0),
		[selectedChannelOptions],
	);

	return {
		filteredChannels,
		handleToggleChannel,
		isChannelSelectionDisabled,
		maxSelections,
		searchQuery,
		selectedChannelIds,
		setSearchQuery,
		slowmodeActiveSelectedChannelOptions,
		slowmodeEnabledSelectedChannelOptions,
	};
}
