// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import selectorStyles from '@app/features/app/components/dialogs/shared/SelectorModalStyles.module.css';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import styles from '@app/features/expressions/components/modals/AddFavoriteChannelModal.module.css';
import Guilds from '@app/features/guild/state/Guilds';
import {
	HIDE_MUTED_CHANNELS_DESCRIPTOR,
	UNCATEGORIZED_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Favorites from '@app/features/messaging/state/Favorites';
import {Button} from '@app/features/ui/button/Button';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import {Combobox, type ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Scroller} from '@app/features/ui/components/Scroller';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React, {useMemo, useState} from 'react';

const ADD_FAVORITE_CHANNELS_DESCRIPTOR = msg({
	message: 'Add favorite channels',
	comment: 'Modal or page title for the add-favorite-channels flow.',
});
const SEARCH_CHANNELS_DESCRIPTOR = msg({
	message: 'Search channels',
	comment: 'Search input placeholder for filtering the channel list.',
});
const SELECT_A_COMMUNITY_DESCRIPTOR = msg({
	message: 'Select a community',
	comment: 'Form field label or placeholder for selecting a community.',
});
const CHOOSE_A_COMMUNITY_DESCRIPTOR = msg({
	message: 'Choose a community',
	comment: 'Form field label or placeholder for choosing a community.',
});
const UNKNOWN_COMMUNITY_DESCRIPTOR = msg({
	message: 'Unknown community',
	comment: 'Fallback label in the add favorite channels modal when a community name is unavailable.',
});
const NO_CHANNELS_AVAILABLE_DESCRIPTOR = msg({
	message: 'No channels available',
	comment: 'Empty state shown when no channels match the current filter.',
});
const REMOVE_DESCRIPTOR = msg({
	message: 'Remove',
	comment: 'Action label for removing the selected item from a list or relationship.',
});
const ADD_DESCRIPTOR = msg({
	message: 'Add',
	comment: 'Action label for adding the current selection.',
});

interface ChannelWithCategory {
	channel: Channel;
	categoryName: string | null;
}

export const AddFavoriteChannelModal = observer(({categoryId}: {categoryId?: string | null} = {}) => {
	const {i18n} = useLingui();
	const guilds = Guilds.getGuilds();
	const firstGuildId = guilds.length > 0 ? guilds[0].id : null;
	const [selectedGuildId, setSelectedGuildId] = useState<string | null>(firstGuildId);
	const [hideMutedChannels, setHideMutedChannels] = useState(false);
	const [searchQuery, setSearchQuery] = useState('');
	const guildOptions: Array<ComboboxOption<string>> = useMemo(
		() =>
			guilds.map((guild) => ({
				value: guild.id,
				label: guild.name ?? i18n._(UNKNOWN_COMMUNITY_DESCRIPTOR),
			})),
		[guilds, i18n.locale],
	);
	const selectedGuild = selectedGuildId ? Guilds.getGuild(selectedGuildId) : null;
	const channels = useMemo(() => {
		if (!selectedGuild) return [];
		const guildChannels = Channels.getGuildChannels(selectedGuild.id);
		const result: Array<ChannelWithCategory> = [];
		const query = searchQuery.toLowerCase().trim();
		for (const channel of guildChannels) {
			if (
				channel.type !== ChannelTypes.GUILD_TEXT &&
				channel.type !== ChannelTypes.GUILD_ANNOUNCEMENT &&
				channel.type !== ChannelTypes.GUILD_VOICE &&
				channel.type !== ChannelTypes.GUILD_LINK
			) {
				continue;
			}
			if (hideMutedChannels && UserGuildSettings.isCategoryOrChannelMuted(selectedGuild.id, channel.id)) {
				continue;
			}
			if (query && !channel.name?.toLowerCase().includes(query)) {
				continue;
			}
			let categoryName: string | null = null;
			if (channel.parentId) {
				const category = Channels.getChannel(channel.parentId);
				if (category) {
					categoryName = category.name ?? null;
				}
			}
			result.push({channel, categoryName});
		}
		return result.sort((a, b) => {
			if (a.categoryName === b.categoryName) {
				return (a.channel.position ?? 0) - (b.channel.position ?? 0);
			}
			if (!a.categoryName) return -1;
			if (!b.categoryName) return 1;
			return a.categoryName.localeCompare(b.categoryName);
		});
	}, [selectedGuild, hideMutedChannels, searchQuery]);
	const handleToggleChannel = (channelId: string) => {
		if (!selectedGuild) return;
		const isAlreadyFavorite = !!Favorites.getChannel(channelId);
		if (isAlreadyFavorite) {
			Favorites.removeChannel(channelId);
		} else {
			Favorites.addChannel(channelId, selectedGuild.id, categoryId ?? null);
		}
	};
	return (
		<Modal.Root size="small" centered data-flx="expressions.add-favorite-channel-modal.modal-root">
			<Modal.Header
				title={i18n._(ADD_FAVORITE_CHANNELS_DESCRIPTOR)}
				data-flx="expressions.add-favorite-channel-modal.modal-header"
			>
				<div className={selectorStyles.headerSearch} data-flx="expressions.add-favorite-channel-modal.div">
					<Input
						value={searchQuery}
						onChange={(e) => setSearchQuery(e.target.value)}
						placeholder={i18n._(SEARCH_CHANNELS_DESCRIPTOR)}
						leftIcon={
							<MagnifyingGlassIcon
								weight="bold"
								className={selectorStyles.searchIcon}
								data-flx="expressions.add-favorite-channel-modal.magnifying-glass-icon"
							/>
						}
						className={selectorStyles.headerSearchInput}
						data-flx="expressions.add-favorite-channel-modal.input.set-search-query"
					/>
				</div>
			</Modal.Header>
			<Modal.Content data-flx="expressions.add-favorite-channel-modal.modal-content">
				<Modal.ContentLayout data-flx="expressions.add-favorite-channel-modal.modal-content-layout">
					<div className={styles.selectContainer} data-flx="expressions.add-favorite-channel-modal.select-container">
						<Combobox
							label={i18n._(SELECT_A_COMMUNITY_DESCRIPTOR)}
							value={selectedGuildId ?? ''}
							options={guildOptions}
							onChange={(value) => setSelectedGuildId(value || null)}
							placeholder={i18n._(CHOOSE_A_COMMUNITY_DESCRIPTOR)}
							data-flx="expressions.add-favorite-channel-modal.select"
						/>
					</div>
					{selectedGuild && (
						<>
							<Checkbox
								className={styles.checkboxRow}
								checked={hideMutedChannels}
								onChange={(checked) => setHideMutedChannels(checked)}
								data-flx="expressions.add-favorite-channel-modal.checkbox-row.set-hide-muted-channels"
							>
								<span className={styles.checkboxText} data-flx="expressions.add-favorite-channel-modal.checkbox-text">
									{i18n._(HIDE_MUTED_CHANNELS_DESCRIPTOR)}
								</span>
							</Checkbox>
							<Scroller
								className={styles.scrollerContainer}
								key="add-favorite-channel-scroller"
								data-flx="expressions.add-favorite-channel-modal.scroller-container"
							>
								<div className={styles.channelList} data-flx="expressions.add-favorite-channel-modal.channel-list">
									{channels.length === 0 ? (
										<div className={styles.emptyState} data-flx="expressions.add-favorite-channel-modal.empty-state">
											{i18n._(NO_CHANNELS_AVAILABLE_DESCRIPTOR)}
										</div>
									) : (
										channels.map(({channel, categoryName}, index) => {
											const prevCategoryName = index > 0 ? channels[index - 1].categoryName : null;
											const showCategoryHeader = categoryName !== prevCategoryName;
											const isAlreadyFavorite = !!Favorites.getChannel(channel.id);
											return (
												<React.Fragment key={channel.id}>
													{showCategoryHeader && (
														<div
															className={styles.categoryHeader}
															data-flx="expressions.add-favorite-channel-modal.category-header"
														>
															{categoryName || i18n._(UNCATEGORIZED_DESCRIPTOR)}
														</div>
													)}
													<div
														className={styles.channelRow}
														data-flx="expressions.add-favorite-channel-modal.channel-row"
													>
														<div
															className={styles.channelIconContainer}
															data-flx="expressions.add-favorite-channel-modal.channel-icon-container"
														>
															{ChannelUtils.getIcon(channel, {
																className: styles.channelIcon,
															})}
														</div>
														<span
															className={styles.channelName}
															data-flx="expressions.add-favorite-channel-modal.channel-name"
														>
															{channel.name}
														</span>
														<div
															className={styles.channelActions}
															data-flx="expressions.add-favorite-channel-modal.channel-actions"
														>
															<Button
																variant={isAlreadyFavorite ? 'secondary' : 'primary'}
																small={true}
																onClick={() => handleToggleChannel(channel.id)}
																data-flx="expressions.add-favorite-channel-modal.button.toggle-channel"
															>
																{isAlreadyFavorite ? i18n._(REMOVE_DESCRIPTOR) : i18n._(ADD_DESCRIPTOR)}
															</Button>
														</div>
													</div>
												</React.Fragment>
											);
										})
									)}
								</div>
							</Scroller>
						</>
					)}
				</Modal.ContentLayout>
			</Modal.Content>
		</Modal.Root>
	);
});
