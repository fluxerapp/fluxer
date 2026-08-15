// SPDX-License-Identifier: AGPL-3.0-or-later

import {LongPressable} from '@app/features/app/components/LongPressable';
import {usePressable} from '@app/features/app/hooks/usePressable';
import styles from '@app/features/guild/components/modals/guild_tabs/MemberListStyles.module.css';
import {MORE_OPTIONS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {useLingui} from '@lingui/react/macro';
import {CaretRightIcon, DotsThreeVerticalIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

interface UserData {
	id: string;
	username: string;
	globalName?: string | null;
	global_name?: string | null;
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
		const {i18n} = useLingui();
		const {isPressed, pressableProps} = usePressable({disabled: !isMobile});
		const handleActivate = useCallback(() => {
			if (isMobile && onActivate) {
				onActivate();
			}
		}, [isMobile, onActivate]);
		const handleContextMenu = useCallback(
			(e: React.MouseEvent<HTMLElement>) => {
				if (!isMobile) {
					e.preventDefault();
					onContextMenu(e);
				}
			},
			[isMobile, onContextMenu],
		);
		const handleLongPress = useCallback(() => {
			onLongPress?.();
		}, [onLongPress]);
		const resolvedDisplayName = displayName || NicknameUtils.getDisplayName(user);
		const sharedContent = (
			<>
				<div className={styles.memberMain} data-flx="guild.guild-tabs.user-list-item.member-main">
					<div className={styles.avatarWrapper} data-flx="guild.guild-tabs.user-list-item.avatar-wrapper">
						{avatarUrl ? (
							<img
								src={avatarUrl}
								alt=""
								className={styles.avatar}
								loading="lazy"
								data-flx="guild.guild-tabs.user-list-item.avatar"
							/>
						) : (
							<div className={styles.avatarPlaceholder} data-flx="guild.guild-tabs.user-list-item.avatar-placeholder">
								{resolvedDisplayName[0].toUpperCase()}
							</div>
						)}
					</div>
					<div className={styles.memberInfo} data-flx="guild.guild-tabs.user-list-item.member-info">
						<div className={styles.nameRow} data-flx="guild.guild-tabs.user-list-item.name-row">
							<span
								className={styles.displayName}
								style={nameColor ? {color: nameColor} : undefined}
								data-flx="guild.guild-tabs.user-list-item.display-name"
							>
								{resolvedDisplayName}
							</span>
							{trailingContent}
						</div>
						{tag && (
							<span className={styles.tag} data-flx="guild.guild-tabs.user-list-item.tag">
								{tag}
							</span>
						)}
					</div>
				</div>
				{isMobile ? (
					<CaretRightIcon
						weight="bold"
						size={remFromPx(20)}
						className={styles.chevron}
						data-flx="guild.guild-tabs.user-list-item.chevron"
					/>
				) : (
					<div className={styles.memberActions} data-flx="guild.guild-tabs.user-list-item.member-actions">
						<Tooltip text={i18n._(MORE_OPTIONS_DESCRIPTOR)} data-flx="guild.guild-tabs.user-list-item.tooltip">
							<button
								type="button"
								className={clsx(styles.moreButton, isMenuActive && styles.moreButtonActive)}
								onClick={(e) => {
									e.stopPropagation();
									onContextMenu(e, true);
								}}
								data-flx="guild.guild-tabs.user-list-item.more-button.stop-propagation"
							>
								<DotsThreeVerticalIcon
									weight="bold"
									className={styles.moreButtonIcon}
									data-flx="guild.guild-tabs.user-list-item.more-button-icon"
								/>
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
					data-flx="guild.guild-tabs.user-list-item.member-item.activate.button"
					{...pressableProps}
				>
					{sharedContent}
				</button>
			);
			if (onLongPress) {
				return (
					<LongPressable
						onLongPress={handleLongPress}
						delay={500}
						pressedClassName={styles.memberItemPressed}
						data-flx="guild.guild-tabs.user-list-item.long-pressable"
					>
						{content}
					</LongPressable>
				);
			}
			return content;
		}
		return (
			<div className={styles.memberItemWrapper} data-flx="guild.guild-tabs.user-list-item.member-item-wrapper">
				<div
					role="group"
					className={styles.memberItem}
					data-non-interactive="true"
					onContextMenu={(e) => onContextMenu(e, false)}
					data-flx="guild.guild-tabs.user-list-item.member-item.context-menu"
				>
					{sharedContent}
				</div>
			</div>
		);
	},
);
