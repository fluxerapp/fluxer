// SPDX-License-Identifier: AGPL-3.0-or-later

import {PreloadableUserPopout} from '@app/features/channel/components/PreloadableUserPopout';
import styles from '@app/features/ui/avatars/AvatarStack.module.css';
import {
	AVATAR_STACK_DEFAULT_MAX_VISIBLE,
	AVATAR_STACK_DEFAULT_SIZE_PX,
	resolveAvatarStackGeometry,
} from '@app/features/ui/avatars/AvatarStackGeometry';
import {Avatar} from '@app/features/ui/components/Avatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';

const OPEN_PROFILE_FOR_DESCRIPTOR = msg({
	message: 'Open profile for {displayName}',
	comment: 'Accessible label for the avatar button that opens a user profile.',
});

export interface AvatarStackProps {
	children?: React.ReactNode;
	users?: ReadonlyArray<User>;
	size?: number;
	maxVisible?: number;
	overlap?: number;
	className?: string;
	guildId?: string | null;
	channelId?: string | null;
	renderAvatar?: (user: User, size: number, index: number) => React.ReactNode;
	enableProfileModal?: boolean;
	showTooltips?: boolean;
	remainingContent?: React.ReactNode;
	onUserContextMenu?: (event: React.MouseEvent<HTMLElement>, user: User, index: number) => void;
}

interface AvatarStackEntry {
	key: string;
	node: React.ReactNode;
}

export const AvatarStack: React.FC<AvatarStackProps> = observer(
	({
		children,
		users,
		size = AVATAR_STACK_DEFAULT_SIZE_PX,
		maxVisible = AVATAR_STACK_DEFAULT_MAX_VISIBLE,
		overlap,
		className,
		guildId,
		channelId,
		renderAvatar,
		enableProfileModal = true,
		showTooltips = true,
		remainingContent,
		onUserContextMenu,
	}) => {
		const {i18n} = useLingui();
		const childEntries: Array<AvatarStackEntry> = React.Children.toArray(children)
			.filter(Boolean)
			.map((child, index) => ({
				key: React.isValidElement(child) && child.key != null ? child.key : `avatar-stack-child-${index}`,
				node: child,
			}));
		const wrapWithContextMenu = (node: React.ReactNode, user: User, index: number, displayName: string) => {
			if (!onUserContextMenu) return node;
			return (
				<div
					className={styles.avatarContextMenuWrap}
					onContextMenu={(e) => onUserContextMenu(e, user, index)}
					role="group"
					aria-label={displayName}
					data-flx="ui.avatars.avatar-stack.wrap-with-context-menu.avatar-context-menu-wrap.user-context-menu"
				>
					{node}
				</div>
			);
		};
		const userEntries: Array<AvatarStackEntry> = [];
		const userKeyCounts = new Map<string, number>();
		users?.forEach((user, index) => {
			const displayName = NicknameUtils.getNickname(user, guildId ?? null, channelId ?? undefined);
			const avatarNode = renderAvatar?.(user, size, index) ?? (
				<Avatar user={user} size={size} guildId={guildId ?? undefined} data-flx="ui.avatars.avatar-stack.avatar" />
			);
			if (!avatarNode) return;
			let node: React.ReactNode;
			if (enableProfileModal) {
				node = (
					<PreloadableUserPopout
						user={user}
						isWebhook={false}
						guildId={guildId ?? undefined}
						channelId={channelId ?? undefined}
						disableContextMenu={true}
						tooltip={showTooltips ? displayName : undefined}
						data-flx="ui.avatars.avatar-stack.preloadable-user-popout"
					>
						<FocusRing offset={-2} data-flx="ui.avatars.avatar-stack.focus-ring">
							<button
								type="button"
								className={styles.avatarButton}
								aria-label={i18n._(OPEN_PROFILE_FOR_DESCRIPTOR, {displayName})}
								data-flx="ui.avatars.avatar-stack.avatar-button"
							>
								{avatarNode}
							</button>
						</FocusRing>
					</PreloadableUserPopout>
				);
			} else {
				const content = (
					<div className={styles.avatarContent} data-flx="ui.avatars.avatar-stack.avatar-content">
						{avatarNode}
					</div>
				);
				node = showTooltips ? (
					<Tooltip text={displayName} data-flx="ui.avatars.avatar-stack.tooltip">
						{content}
					</Tooltip>
				) : (
					content
				);
			}
			const occurrence = userKeyCounts.get(user.id) ?? 0;
			userKeyCounts.set(user.id, occurrence + 1);
			userEntries.push({
				key: occurrence === 0 ? user.id : `${user.id}:${occurrence}`,
				node: wrapWithContextMenu(node, user, index, displayName),
			});
		});
		const resolvedEntries = users ? userEntries : childEntries;
		const remainingCount = Math.max(0, resolvedEntries.length - maxVisible);
		const visibleEntries = resolvedEntries.slice(0, maxVisible);
		const geometry = resolveAvatarStackGeometry(size, overlap);
		const cssVars = {
			'--avatar-size': geometry.sizeRem,
			'--avatar-overlap': geometry.overlapRem,
			'--avatar-outline': geometry.outlineRem,
		} as React.CSSProperties;
		return (
			<div className={clsx(styles.container, className)} style={cssVars} data-flx="ui.avatars.avatar-stack.container">
				{visibleEntries.map((entry, index) => (
					<div
						key={entry.key}
						className={clsx(
							styles.avatar,
							(index < visibleEntries.length - 1 || remainingCount > 0) && styles.withMask,
						)}
						data-flx="ui.avatars.avatar-stack.avatar--2"
					>
						{entry.node}
					</div>
				))}
				{remainingCount > 0 &&
					(remainingContent ?? (
						<div className={styles.remainingCount} data-flx="ui.avatars.avatar-stack.remaining-count">
							+{remainingCount}
						</div>
					))}
			</div>
		);
	},
);
