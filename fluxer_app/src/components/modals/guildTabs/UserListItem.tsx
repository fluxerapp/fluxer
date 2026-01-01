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
import {CaretRightIcon, DotsThreeVerticalIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {LongPressable} from '~/components/LongPressable';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {usePressable} from '~/hooks/usePressable';
import styles from './MemberListStyles.module.css';

interface UserData {
	id: string;
	username: string;
	avatar?: string | null;
}

interface UserListItemProps {
	user: UserData;
	avatarUrl?: string;
	displayName?: string;
	tag?: string;
	nameColor?: string;
	isMobile: boolean;
	isMenuActive?: boolean;
	trailingContent?: React.ReactNode;
	onContextMenu: (event: React.MouseEvent<HTMLElement>, fromButton?: boolean) => void;
	onActivate?: () => void;
	onLongPress?: () => void;
}

export const UserListItem: React.FC<UserListItemProps> = observer(
	({
		user,
		avatarUrl,
		displayName,
		tag,
		nameColor,
		isMobile,
		isMenuActive = false,
		trailingContent,
		onContextMenu,
		onActivate,
		onLongPress,
	}) => {
		const {t} = useLingui();
		const {isPressed, pressableProps} = usePressable({disabled: !isMobile});

		const handleActivate = React.useCallback(() => {
			if (isMobile && onActivate) {
				onActivate();
			}
		}, [isMobile, onActivate]);

		const handleContextMenu = React.useCallback(
			(e: React.MouseEvent<HTMLElement>) => {
				if (!isMobile) {
					e.preventDefault();
					onContextMenu(e);
				}
			},
			[isMobile, onContextMenu],
		);

		const handleLongPress = React.useCallback(() => {
			onLongPress?.();
		}, [onLongPress]);

		const sharedContent = (
			<>
				<div className={styles.memberMain}>
					<div className={styles.avatarWrapper}>
						{avatarUrl ? (
							<img src={avatarUrl} alt="" className={styles.avatar} loading="lazy" />
						) : (
							<div className={styles.avatarPlaceholder}>{(displayName || user.username)[0].toUpperCase()}</div>
						)}
					</div>
					<div className={styles.memberInfo}>
						<div className={styles.nameRow}>
							<span className={styles.displayName} style={nameColor ? {color: nameColor} : undefined}>
								{displayName || user.username}
							</span>
							{trailingContent}
						</div>
						{tag && <span className={styles.tag}>{tag}</span>}
					</div>
				</div>

				{isMobile ? (
					<CaretRightIcon weight="bold" size={20} className={styles.chevron} />
				) : (
					<div className={styles.memberActions}>
						<Tooltip text={t`More options`}>
							<button
								type="button"
								className={clsx(styles.moreButton, isMenuActive && styles.moreButtonActive)}
								onClick={(e) => {
									e.stopPropagation();
									onContextMenu(e, true);
								}}
							>
								<DotsThreeVerticalIcon weight="bold" className={styles.moreButtonIcon} />
							</button>
						</Tooltip>
					</div>
				)}
			</>
		);

		if (isMobile) {
			const content = (
				<button
					type="button"
					className={clsx(styles.memberItem, styles.memberItemInteractive, isPressed && styles.memberItemPressed)}
					onClick={handleActivate}
					onContextMenu={handleContextMenu}
					{...pressableProps}
				>
					{sharedContent}
				</button>
			);

			if (onLongPress) {
				return (
					<LongPressable onLongPress={handleLongPress} delay={500} pressedClassName={styles.memberItemPressed}>
						{content}
					</LongPressable>
				);
			}

			return content;
		}

		return (
			<div className={styles.memberItemWrapper}>
				{/* biome-ignore lint/a11y/noStaticElementInteractions: desktop row should only open context menu */}
				<div className={styles.memberItem} data-non-interactive="true" onContextMenu={(e) => onContextMenu(e, false)}>
					{sharedContent}
				</div>
			</div>
		);
	},
);
