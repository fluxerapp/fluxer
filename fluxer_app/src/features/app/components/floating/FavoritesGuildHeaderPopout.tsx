// SPDX-License-Identifier: AGPL-3.0-or-later

import {useRovingFocusList} from '@app/features/app/hooks/useRovingFocusList';
import {AddFavoriteChannelModal} from '@app/features/expressions/components/modals/AddFavoriteChannelModal';
import {CreateFavoriteCategoryModal} from '@app/features/expressions/components/modals/CreateFavoriteCategoryModal';
import {
	GuildHeaderPopoutCheckboxItem,
	GuildHeaderPopoutItem,
} from '@app/features/guild/components/popouts/GuildHeaderPopout';
import styles from '@app/features/guild/components/popouts/GuildHeaderPopout.module.css';
import {
	ADD_CHANNEL_DESCRIPTOR,
	CREATE_CATEGORY_DESCRIPTOR,
	HIDE_MUTED_CHANNELS_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Favorites from '@app/features/messaging/state/Favorites';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {useLingui} from '@lingui/react/macro';
import {FolderPlusIcon, PlusCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

export const FavoritesGuildHeaderPopout = observer(() => {
	const {i18n} = useLingui();
	const hideMutedChannels = Favorites.hideMutedChannels;
	const handleToggleHideMutedChannels = useCallback((checked: boolean) => {
		Favorites.setHideMutedChannels(checked);
	}, []);
	const listRef = useRovingFocusList<HTMLDivElement>({
		focusableSelector: '[data-roving-focus="true"]',
	});
	return (
		<div
			className={styles.container}
			ref={listRef}
			role="menu"
			aria-orientation="vertical"
			tabIndex={-1}
			data-autofocus
			data-flx="app.floating.favorites-guild-header-popout.container"
		>
			<GuildHeaderPopoutItem
				icon={PlusCircleIcon}
				title={i18n._(ADD_CHANNEL_DESCRIPTOR)}
				onClick={() =>
					ModalCommands.push(
						modal(() => (
							<AddFavoriteChannelModal data-flx="app.floating.favorites-guild-header-popout.add-favorite-channel-modal" />
						)),
					)
				}
				data-flx="app.floating.favorites-guild-header-popout.guild-header-popout-item.push"
			/>
			<GuildHeaderPopoutItem
				icon={FolderPlusIcon}
				title={i18n._(CREATE_CATEGORY_DESCRIPTOR)}
				onClick={() =>
					ModalCommands.push(
						modal(() => (
							<CreateFavoriteCategoryModal data-flx="app.floating.favorites-guild-header-popout.create-favorite-category-modal" />
						)),
					)
				}
				data-flx="app.floating.favorites-guild-header-popout.guild-header-popout-item.push--2"
			/>
			<GuildHeaderPopoutCheckboxItem
				title={i18n._(HIDE_MUTED_CHANNELS_DESCRIPTOR)}
				checked={hideMutedChannels}
				onChange={handleToggleHideMutedChannels}
				data-flx="app.floating.favorites-guild-header-popout.guild-header-popout-checkbox-item.toggle-hide-muted-channels"
			/>
		</div>
	);
});
