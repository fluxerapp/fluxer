// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {PASSWORD_MANAGER_IGNORE_ATTRIBUTES} from '@app/features/platform/utils/PasswordManagerAutocomplete';
import styles from '@app/features/search/components/search/ChannelFilterSheet.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {Button} from '@app/features/ui/button/Button';
import {Scroller} from '@app/features/ui/components/Scroller';
import {GUILD_TEXT_BASED_CHANNEL_TYPES} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CheckIcon, MagnifyingGlassIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo, useState} from 'react';

const FILTER_BY_CHANNEL_DESCRIPTOR = msg({
	message: 'Filter by channel',
	comment: 'Title of the mobile in: (channel) filter sheet in message search. Title Case.',
});
const SEARCH_CHANNELS_DESCRIPTOR = msg({
	message: 'Search channels',
	comment: 'Placeholder text in the mobile channel filter sheet search input. Sentence case.',
});

interface ChannelFilterSheetProps {
	isOpen: boolean;
	onClose: () => void;
	guildId: string;
	selectedChannelIds: Array<string>;
	onChannelsChange: (channelIds: Array<string>) => void;
	title?: string;
}

export const ChannelFilterSheet: React.FC<ChannelFilterSheetProps> = observer(
	({isOpen, onClose, guildId, selectedChannelIds, onChannelsChange, title}) => {
		const {i18n} = useLingui();
		const [searchTerm, setSearchTerm] = useState('');
		useEffect(() => {
			if (isOpen) {
				setSearchTerm('');
			}
		}, [isOpen]);
		const availableChannels = useMemo((): Array<Channel> => {
			return Channels.getGuildChannels(guildId).filter((c) => GUILD_TEXT_BASED_CHANNEL_TYPES.has(c.type));
		}, [guildId]);
		const filteredChannels = useMemo(() => {
			if (!searchTerm.trim()) {
				return availableChannels.slice(0, 100);
			}
			return matchSorter(availableChannels, searchTerm, {keys: ['name']}).slice(0, 100);
		}, [availableChannels, searchTerm]);
		const toggleChannel = (channelId: string) => {
			if (selectedChannelIds.includes(channelId)) {
				onChannelsChange(selectedChannelIds.filter((id) => id !== channelId));
			} else {
				onChannelsChange([...selectedChannelIds, channelId]);
			}
		};
		return (
			<BottomSheet
				isOpen={isOpen}
				onClose={onClose}
				snapPoints={[0, 1]}
				initialSnap={1}
				title={title ?? i18n._(FILTER_BY_CHANNEL_DESCRIPTOR)}
				disablePadding
				data-flx="search.search.channel-filter-sheet.bottom-sheet"
			>
				<div className={styles.container} data-flx="search.search.channel-filter-sheet.container">
					<div className={styles.searchContainer} data-flx="search.search.channel-filter-sheet.search-container">
						<div
							className={styles.searchInputWrapper}
							data-flx="search.search.channel-filter-sheet.search-input-wrapper"
						>
							<MagnifyingGlassIcon
								size={remFromPx(20)}
								className={styles.searchIcon}
								weight="regular"
								data-flx="search.search.channel-filter-sheet.search-icon"
							/>
							<input
								type="text"
								className={styles.searchInput}
								placeholder={i18n._(SEARCH_CHANNELS_DESCRIPTOR)}
								value={searchTerm}
								onChange={(e) => setSearchTerm(e.target.value)}
								data-flx="search.search.channel-filter-sheet.search-input.set-search-term.text"
								{...PASSWORD_MANAGER_IGNORE_ATTRIBUTES}
								autoComplete="off"
								autoCorrect="off"
								autoCapitalize="off"
							/>
							{searchTerm.length > 0 && (
								<button
									type="button"
									className={styles.clearButton}
									onClick={() => setSearchTerm('')}
									data-flx="search.search.channel-filter-sheet.clear-button.set-search-term"
								>
									<XIcon size={remFromPx(18)} weight="bold" data-flx="search.search.channel-filter-sheet.x-icon" />
								</button>
							)}
						</div>
					</div>
					<Scroller
						key="channel-filter-scroller"
						className={styles.scroller}
						fade={false}
						data-flx="search.search.channel-filter-sheet.scroller"
					>
						<div className={styles.listContent} data-flx="search.search.channel-filter-sheet.list-content">
							{filteredChannels.length === 0 ? (
								<div className={styles.emptyState} data-flx="search.search.channel-filter-sheet.empty-state">
									{searchTerm ? <Trans>No channels found</Trans> : <Trans>No channels available</Trans>}
								</div>
							) : (
								filteredChannels.map((channelOption) => {
									const isSelected = selectedChannelIds.includes(channelOption.id);
									const parent = channelOption.parentId ? Channels.getChannel(channelOption.parentId) : null;
									return (
										<button
											key={channelOption.id}
											type="button"
											aria-pressed={isSelected}
											className={clsx(styles.channelItem, isSelected && styles.channelItemSelected)}
											onClick={() => toggleChannel(channelOption.id)}
											data-flx="search.search.channel-filter-sheet.channel-item.toggle-channel.button"
										>
											{ChannelUtils.getIcon(channelOption, {className: styles.channelIcon})}
											<div className={styles.channelInfo} data-flx="search.search.channel-filter-sheet.channel-info">
												<span className={styles.channelName} data-flx="search.search.channel-filter-sheet.channel-name">
													{channelOption.name || <Trans>Unnamed channel</Trans>}
												</span>
												{parent?.name && (
													<span
														className={styles.categoryName}
														data-flx="search.search.channel-filter-sheet.category-name"
													>
														{parent.name}
													</span>
												)}
											</div>
											{isSelected && (
												<CheckIcon
													size={remFromPx(20)}
													className={styles.checkIcon}
													weight="bold"
													data-flx="search.search.channel-filter-sheet.check-icon"
												/>
											)}
										</button>
									);
								})
							)}
						</div>
					</Scroller>
					<div className={styles.footer} data-flx="search.search.channel-filter-sheet.footer">
						<Button variant="primary" onClick={onClose} data-flx="search.search.channel-filter-sheet.button.close">
							<Trans>Done</Trans>
						</Button>
					</div>
				</div>
			</BottomSheet>
		);
	},
);
