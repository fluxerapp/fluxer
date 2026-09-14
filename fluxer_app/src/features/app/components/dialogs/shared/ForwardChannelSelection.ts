// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ForwardChannelIndex,
	type ForwardChannelOption,
	type ForwardMessageMediaSelection,
	resolveForwardMessageMediaSelection,
} from '@app/features/app/components/dialogs/shared/ForwardChannelIndex';
import {useForwardChannelObservations} from '@app/features/app/components/dialogs/shared/UseForwardChannelObservations';
import {useShallowStableArray} from '@app/features/app/hooks/useShallowStableArray';
import type {Channel} from '@app/features/channel/models/Channel';
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
	readonly handleToggleChannel: (key: string) => void;
	readonly isChannelSelectionDisabled: (option: ForwardChannelOption) => boolean;
	readonly maxSelections: number;
	readonly mostRecentlySelectedChannel: Channel | null;
	readonly searchQuery: string;
	readonly selectedChannelOptions: ReadonlyArray<ForwardChannelOption>;
	readonly selectedKeys: ReadonlySet<string>;
	readonly setSearchQuery: Dispatch<SetStateAction<string>>;
	readonly slowmodeActiveSelectedChannelOptions: ReadonlyArray<ForwardChannelOption>;
	readonly slowmodeEnabledSelectedChannelOptions: ReadonlyArray<ForwardChannelOption>;
}

function toggleForwardChannelSelection(key: string, maxSelections: number, previousKeys: Set<string>): Set<string> {
	const nextKeys = new Set(previousKeys);
	if (nextKeys.has(key)) {
		nextKeys.delete(key);
		return nextKeys;
	}
	if (nextKeys.size >= maxSelections) return previousKeys;
	nextKeys.add(key);
	return nextKeys;
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
	const [selectedKeys, setSelectedKeys] = useState<Set<string>>(new Set());
	const filteredChannels = useMemo(() => channelIndex.filter(searchQuery), [channelIndex, searchQuery]);
	const handleToggleChannel = useCallback(
		(key: string) => {
			setSelectedKeys((previousKeys) => toggleForwardChannelSelection(key, maxSelections, previousKeys));
		},
		[maxSelections],
	);
	const isChannelSelectionDisabled = useCallback(
		(option: ForwardChannelOption) => channelIndex.isSelectionDisabled({maxSelections, option, selectedKeys}),
		[maxSelections, channelIndex, selectedKeys],
	);
	const selectedChannelOptions = useMemo(() => channelIndex.select(selectedKeys), [channelIndex, selectedKeys]);
	const slowmodeEnabledSelectedChannelOptions = useMemo(
		() => selectedChannelOptions.filter((option) => option.slowmodeEnabled),
		[selectedChannelOptions],
	);
	const slowmodeActiveSelectedChannelOptions = useMemo(
		() => selectedChannelOptions.filter((option) => option.slowmodeRemainingMs > 0),
		[selectedChannelOptions],
	);
	const mostRecentlySelectedChannel = useMemo(() => {
		let mostRecent: Channel | null = null;
		for (const option of selectedChannelOptions) {
			mostRecent = option.channel;
		}
		return mostRecent;
	}, [selectedChannelOptions]);

	return {
		filteredChannels,
		handleToggleChannel,
		isChannelSelectionDisabled,
		maxSelections,
		mostRecentlySelectedChannel,
		searchQuery,
		selectedChannelOptions,
		selectedKeys,
		setSearchQuery,
		slowmodeActiveSelectedChannelOptions,
		slowmodeEnabledSelectedChannelOptions,
	};
}
