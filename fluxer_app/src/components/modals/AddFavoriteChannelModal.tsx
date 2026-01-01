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
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {ChannelTypes} from '~/Constants';
import {Input} from '~/components/form/Input';
import {Select, type SelectOption} from '~/components/form/Select';
import styles from '~/components/modals/AddFavoriteChannelModal.module.css';
import * as Modal from '~/components/modals/Modal';
import selectorStyles from '~/components/modals/shared/SelectorModalStyles.module.css';
import {Button} from '~/components/uikit/Button/Button';
import {Checkbox} from '~/components/uikit/Checkbox/Checkbox';
import {Scroller} from '~/components/uikit/Scroller';
import type {ChannelRecord} from '~/records/ChannelRecord';
import ChannelStore from '~/stores/ChannelStore';
import FavoritesStore from '~/stores/FavoritesStore';
import GuildStore from '~/stores/GuildStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import * as ChannelUtils from '~/utils/ChannelUtils';

interface ChannelWithCategory {
	channel: ChannelRecord;
	categoryName: string | null;
}

export const AddFavoriteChannelModal = observer(({categoryId}: {categoryId?: string | null} = {}) => {
	const {t} = useLingui();
	const guilds = GuildStore.getGuilds();
	const firstGuildId = guilds.length > 0 ? guilds[0].id : null;

	const [selectedGuildId, setSelectedGuildId] = React.useState<string | null>(firstGuildId);
	const [hideMutedChannels, setHideMutedChannels] = React.useState(false);
	const [searchQuery, setSearchQuery] = React.useState('');

	const guildOptions: Array<SelectOption<string>> = React.useMemo(
		() =>
			guilds.map((guild) => ({
				value: guild.id,
				label: guild.name ?? 'Unknown Guild',
			})),
		[guilds],
	);

	const selectedGuild = selectedGuildId ? GuildStore.getGuild(selectedGuildId) : null;

	const channels = React.useMemo(() => {
		if (!selectedGuild) return [];

		const guildChannels = ChannelStore.getGuildChannels(selectedGuild.id);
		const result: Array<ChannelWithCategory> = [];
		const query = searchQuery.toLowerCase().trim();

		for (const channel of guildChannels) {
			if (channel.type !== ChannelTypes.GUILD_TEXT && channel.type !== ChannelTypes.GUILD_VOICE) {
				continue;
			}

			if (hideMutedChannels && UserGuildSettingsStore.isGuildOrChannelMuted(selectedGuild.id, channel.id)) {
				continue;
			}

			if (query && !channel.name?.toLowerCase().includes(query)) {
				continue;
			}

			let categoryName: string | null = null;
			if (channel.parentId) {
				const category = ChannelStore.getChannel(channel.parentId);
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

		const isAlreadyFavorite = !!FavoritesStore.getChannel(channelId);
		if (isAlreadyFavorite) {
			FavoritesStore.removeChannel(channelId);
		} else {
			FavoritesStore.addChannel(channelId, selectedGuild.id, categoryId ?? null);
		}
	};

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={t`Add Favorite Channels`}>
				<div className={selectorStyles.headerSearch}>
					<Input
						value={searchQuery}
						onChange={(e) => setSearchQuery(e.target.value)}
						placeholder={t`Search channels`}
						leftIcon={<MagnifyingGlassIcon weight="bold" className={selectorStyles.searchIcon} />}
						className={selectorStyles.headerSearchInput}
					/>
				</div>
			</Modal.Header>
			<Modal.Content className={styles.content}>
				<div className={styles.selectContainer}>
					<Select
						label={t`Select a Community`}
						value={selectedGuildId ?? ''}
						options={guildOptions}
						onChange={(value) => setSelectedGuildId(value || null)}
						placeholder={t`Choose a community...`}
					/>
				</div>

				{selectedGuild && (
					<>
						<Checkbox
							className={styles.checkboxRow}
							checked={hideMutedChannels}
							onChange={(checked) => setHideMutedChannels(checked)}
						>
							<span className={styles.checkboxText}>{t`Hide muted channels`}</span>
						</Checkbox>

						<Scroller className={styles.scrollerContainer} key="add-favorite-channel-scroller">
							<div className={styles.channelList}>
								{channels.length === 0 ? (
									<div className={styles.emptyState}>{t`No channels available`}</div>
								) : (
									channels.map(({channel, categoryName}, index) => {
										const prevCategoryName = index > 0 ? channels[index - 1].categoryName : null;
										const showCategoryHeader = categoryName !== prevCategoryName;
										const isAlreadyFavorite = !!FavoritesStore.getChannel(channel.id);

										return (
											<React.Fragment key={channel.id}>
												{showCategoryHeader && (
													<div className={styles.categoryHeader}>{categoryName || t`Uncategorized`}</div>
												)}
												<div className={styles.channelRow}>
													<div className={styles.channelIconContainer}>
														{ChannelUtils.getIcon(channel, {
															className: styles.channelIcon,
														})}
													</div>
													<span className={styles.channelName}>{channel.name}</span>
													<div className={styles.channelActions}>
														<Button
															variant={isAlreadyFavorite ? 'secondary' : 'primary'}
															small={true}
															onClick={() => handleToggleChannel(channel.id)}
														>
															{isAlreadyFavorite ? t`Remove` : t`Add`}
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
			</Modal.Content>
			<Modal.Footer>
				<Button onClick={ModalActionCreators.pop}>{t`Close`}</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
