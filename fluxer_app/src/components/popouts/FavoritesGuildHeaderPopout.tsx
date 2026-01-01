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
import {FolderPlusIcon, type Icon, PlusCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as PopoutActionCreators from '~/actions/PopoutActionCreators';
import {AddFavoriteChannelModal} from '~/components/modals/AddFavoriteChannelModal';
import {CreateFavoriteCategoryModal} from '~/components/modals/CreateFavoriteCategoryModal';
import {Checkbox} from '~/components/uikit/Checkbox/Checkbox';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {useRovingFocusList} from '~/hooks/useRovingFocusList';
import FavoritesStore from '~/stores/FavoritesStore';
import styles from './FavoritesGuildHeaderPopout.module.css';

const FavoritesHeaderPopoutItem = observer(
	(props: {title: string; icon: Icon; onClick?: () => void; danger?: boolean}) => {
		const handleSelect = React.useCallback(() => {
			PopoutActionCreators.close();
			props.onClick?.();
		}, [props]);

		const handleMouseEnter = React.useCallback((event: React.MouseEvent<HTMLButtonElement>) => {
			event.currentTarget.focus();
		}, []);

		return (
			<FocusRing offset={-2}>
				<button
					type="button"
					className={clsx(styles.item, props.danger && styles.itemDanger)}
					onClick={handleSelect}
					onMouseEnter={handleMouseEnter}
					data-roving-focus="true"
				>
					<span>{props.title}</span>
					<props.icon className={styles.iconMedium} />
				</button>
			</FocusRing>
		);
	},
);

const FavoritesHeaderPopoutCheckboxItem = observer(
	(props: {title: string; checked: boolean; onChange: (checked: boolean) => void}) => {
		const [isHovered, setIsHovered] = React.useState(false);
		const [isFocused, setIsFocused] = React.useState(false);

		const handleChange = React.useCallback(
			(checked: boolean) => {
				props.onChange(checked);
			},
			[props],
		);

		const handleClick = React.useCallback(() => {
			props.onChange(!props.checked);
		}, [props]);

		return (
			<FocusRing offset={-2}>
				<div
					className={styles.checkboxContainer}
					onMouseEnter={(event) => {
						setIsHovered(true);
						event.currentTarget.focus();
					}}
					onMouseLeave={() => setIsHovered(false)}
					onClick={handleClick}
					onFocus={() => setIsFocused(true)}
					onBlur={() => setIsFocused(false)}
					onKeyDown={(e) => {
						if (e.key === 'Enter' || e.key === ' ') {
							e.preventDefault();
							handleClick();
						}
					}}
					role="button"
					tabIndex={0}
					data-roving-focus="true"
				>
					<span>{props.title}</span>
					<div className={styles.checkboxIcon}>
						<Checkbox
							checked={props.checked}
							onChange={handleChange}
							noFocus
							size={18.75}
							inverted={isHovered || isFocused}
							aria-hidden={true}
						/>
					</div>
				</div>
			</FocusRing>
		);
	},
);

export const FavoritesGuildHeaderPopout = observer(() => {
	const {t} = useLingui();
	const hideMutedChannels = FavoritesStore.hideMutedChannels;

	const handleToggleHideMutedChannels = React.useCallback((checked: boolean) => {
		FavoritesStore.setHideMutedChannels(checked);
	}, []);

	const listRef = useRovingFocusList<HTMLDivElement>({
		autoFocusFirst: true,
		focusableSelector: '[data-roving-focus="true"]',
	});

	return (
		<div className={styles.container} ref={listRef}>
			<FavoritesHeaderPopoutItem
				icon={PlusCircleIcon}
				title={t`Add Channel`}
				onClick={() => ModalActionCreators.push(modal(() => <AddFavoriteChannelModal />))}
			/>
			<FavoritesHeaderPopoutItem
				icon={FolderPlusIcon}
				title={t`Create Category`}
				onClick={() => ModalActionCreators.push(modal(() => <CreateFavoriteCategoryModal />))}
			/>
			<FavoritesHeaderPopoutCheckboxItem
				title={t`Hide Muted Channels`}
				checked={hideMutedChannels}
				onChange={handleToggleHideMutedChannels}
			/>
		</div>
	);
});
