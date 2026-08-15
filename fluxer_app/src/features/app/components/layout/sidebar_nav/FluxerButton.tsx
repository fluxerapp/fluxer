// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import styles from '@app/features/app/components/layout/GuildsLayout.module.css';
import {getFluxerButtonBadgeCount} from '@app/features/app/components/layout/sidebar_nav/FluxerButtonBadgeUtils';
import {useHover} from '@app/features/app/hooks/useHover';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Channels from '@app/features/channel/state/Channels';
import {DIRECT_MESSAGES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import ReadStates from '@app/features/read_state/state/ReadStates';
import Relationships from '@app/features/relationship/state/Relationships';
import {FluxerButtonContextMenu} from '@app/features/ui/action_menu/FluxerButtonContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {FluxerSymbol} from '@app/features/ui/components/icons/FluxerSymbol';
import {MentionBadgeAnimated} from '@app/features/ui/components/MentionBadge';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import SidebarPreferences from '@app/features/ui/state/SidebarPreferences';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {getAppZoomFactor} from '@app/features/ui/utils/AppZoomUtils';
import {ME} from '@fluxer/constants/src/AppConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useRef} from 'react';

const MESSAGE_1_UNREAD_DM_DESCRIPTOR = msg({
	message: '1 unread DM',
	comment: 'Short label in the sidebar navigation fluxer button.',
});
const UNREAD_DMS_DESCRIPTOR = msg({
	message: '{displayedUnreadDmCount} unread DMs',
	comment:
		'Short label in the sidebar navigation fluxer button. Preserve {displayedUnreadDmCount}; it is inserted by code.',
});
const MESSAGE_1_FRIEND_REQUEST_DESCRIPTOR = msg({
	message: '1 friend request',
	comment: 'Short label in the sidebar navigation fluxer button.',
});
const FRIEND_REQUESTS_DESCRIPTOR = msg({
	message: '{displayedIncomingFriendRequestCount} friend requests',
	comment:
		'Sidebar Fluxer-home button tooltip showing how many incoming friend requests are pending. Count is interpolated.',
});
export const FluxerButton = observer(() => {
	const {i18n} = useLingui();
	const [hoverRef, isHovering] = useHover();
	const buttonRef = useRef<HTMLButtonElement | null>(null);
	const iconRef = useRef<HTMLDivElement | null>(null);
	const mergedButtonRef = useMergeRefs([hoverRef, buttonRef]);
	const location = useLocation();
	const isSelected = location.pathname.startsWith(Routes.ME) || Routes.isSpecialPage(location.pathname);
	const selectedChannel = SelectedChannel.selectedChannelIds.get(ME);
	const inlineDmsCollapsed = SidebarPreferences.inlineDmsCollapsed;
	const showCollapsedUnreadDmsBadge = SidebarPreferences.showCollapsedUnreadDmsBadge;
	const showIncomingFriendRequestBadge = SidebarPreferences.showIncomingFriendRequestBadge;
	const dmChannels = Channels.dmChannels;
	const readStateVersion = ReadStates.version;
	const relationships = Relationships.getRelationships();
	const incomingFriendRequestCount = Object.values(relationships).filter(
		({type}) => type === RelationshipTypes.INCOMING_REQUEST,
	).length;
	const unreadDmCount = useMemo(() => {
		if (!inlineDmsCollapsed || !showCollapsedUnreadDmsBadge) return 0;
		let count = 0;
		for (const channel of dmChannels) {
			if (ReadStates.hasUnread(channel.id)) count++;
		}
		return count;
	}, [dmChannels, inlineDmsCollapsed, readStateVersion, showCollapsedUnreadDmsBadge]);
	const badgeCount = getFluxerButtonBadgeCount({
		incomingFriendRequestCount,
		inlineDmsCollapsed,
		showCollapsedUnreadDmsBadge,
		showIncomingFriendRequestBadge,
		unreadDmCount,
	});
	const displayedUnreadDmCount = inlineDmsCollapsed && showCollapsedUnreadDmsBadge ? unreadDmCount : 0;
	const displayedIncomingFriendRequestCount = showIncomingFriendRequestBadge ? incomingFriendRequestCount : 0;
	const directMessagesLabel = useMemo(() => {
		const parts = [i18n._(DIRECT_MESSAGES_DESCRIPTOR)];
		if (displayedUnreadDmCount === 1) {
			parts.push(i18n._(MESSAGE_1_UNREAD_DM_DESCRIPTOR));
		} else if (displayedUnreadDmCount > 1) {
			parts.push(i18n._(UNREAD_DMS_DESCRIPTOR, {displayedUnreadDmCount}));
		}
		if (displayedIncomingFriendRequestCount === 1) {
			parts.push(i18n._(MESSAGE_1_FRIEND_REQUEST_DESCRIPTOR));
		} else if (displayedIncomingFriendRequestCount > 1) {
			parts.push(i18n._(FRIEND_REQUESTS_DESCRIPTOR, {displayedIncomingFriendRequestCount}));
		}
		return parts.join(', ');
	}, [displayedIncomingFriendRequestCount, displayedUnreadDmCount, i18n.locale]);
	const handleSelect = () => {
		const isMobile = MobileLayout.isMobileLayout();
		const route = (() => {
			if (isMobile) return Routes.ME;
			if (selectedChannel) return Routes.dmChannel(selectedChannel);
			return Routes.ME;
		})();
		RouterUtils.transitionTo(route);
	};
	const handleContextMenu = useCallback((event: React.MouseEvent) => {
		if (MobileLayout.isMobileLayout()) return;
		ContextMenuCommands.openFromEvent(event, (props) => (
			<FluxerButtonContextMenu
				onClose={props.onClose}
				data-flx="app.sidebar-nav.fluxer-button.handle-context-menu.fluxer-button-context-menu"
			/>
		));
	}, []);
	const indicatorHeight =
		(() => {
			if (isSelected) return 40;
			if (isHovering) return 20;
			return 8;
		})() * getAppZoomFactor();
	const isActive = isHovering || isSelected;
	if (RuntimeConfig.directMessagesDisabled) {
		return null;
	}
	return (
		<Tooltip
			position="right"
			size="large"
			text={i18n._(DIRECT_MESSAGES_DESCRIPTOR)}
			data-flx="app.sidebar-nav.fluxer-button.tooltip"
		>
			<FocusRing
				offset={-2}
				focusTarget={buttonRef}
				ringTarget={iconRef}
				data-flx="app.sidebar-nav.fluxer-button.focus-ring"
			>
				<button
					type="button"
					className={styles.fluxerButton}
					aria-label={directMessagesLabel}
					aria-current={isSelected ? 'page' : undefined}
					data-guild-list-focus-item="true"
					onClick={handleSelect}
					onContextMenu={handleContextMenu}
					ref={mergedButtonRef}
					data-flx="app.sidebar-nav.fluxer-button.fluxer-button.select"
				>
					<AnimatePresence data-flx="app.sidebar-nav.fluxer-button.animate-presence">
						{(isSelected || isHovering) && (
							<div className={styles.guildIndicator} data-flx="app.sidebar-nav.fluxer-button.guild-indicator">
								<motion.span
									className={styles.guildIndicatorBar}
									initial={false}
									animate={{opacity: 1, scale: 1, height: indicatorHeight}}
									exit={{opacity: 0, scale: 0, height: 0}}
									transition={{duration: 0.2, ease: [0.25, 0.1, 0.25, 1]}}
									data-flx="app.sidebar-nav.fluxer-button.guild-indicator-bar"
								/>
							</div>
						)}
					</AnimatePresence>
					<div className={styles.relative} data-flx="app.sidebar-nav.fluxer-button.relative">
						<motion.div
							ref={iconRef}
							className={clsx(styles.fluxerButtonIcon, isSelected && styles.fluxerButtonIconSelected)}
							animate={{borderRadius: isActive ? '30%' : '50%'}}
							initial={false}
							transition={{duration: 0.07, ease: 'easeOut'}}
							data-flx="app.sidebar-nav.fluxer-button.fluxer-button-icon"
						>
							<FluxerSymbol
								className={styles.fluxerSymbolIcon}
								data-flx="app.sidebar-nav.fluxer-button.fluxer-symbol-icon"
							/>
						</motion.div>
						<div
							className={clsx(styles.guildBadge, badgeCount > 0 && styles.guildBadgeActive)}
							data-flx="app.sidebar-nav.fluxer-button.guild-badge"
						>
							<MentionBadgeAnimated
								mentionCount={badgeCount}
								size="small"
								data-flx="app.sidebar-nav.fluxer-button.mention-badge-animated"
							/>
						</div>
					</div>
				</button>
			</FocusRing>
		</Tooltip>
	);
});
