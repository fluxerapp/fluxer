// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {CustomStatusDisplay} from '@app/features/app/components/shared/custom_status_display/CustomStatusDisplay';
import {getStatusTypeLabel} from '@app/features/app/constants/AppConstants';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import {ActionButton} from '@app/features/channel/components/friends/ActionButton';
import styles from '@app/features/channel/components/friends/FriendListItem.module.css';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey, stopPropagationOnEnterSpace} from '@app/features/input/utils/KeyboardUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {usePresenceCustomStatus} from '@app/features/presence/hooks/usePresenceCustomStatus';
import Presence from '@app/features/presence/state/Presence';
import * as RelationshipCommands from '@app/features/relationship/commands/RelationshipCommands';
import * as RelationshipActionUtils from '@app/features/relationship/utils/RelationshipActionUtils';
import {
	ACCEPT_FRIEND_REQUEST_ACTION_DESCRIPTOR,
	CANCEL_FRIEND_REQUEST_DESCRIPTOR,
	IGNORE_FRIEND_REQUEST_ACTION_DESCRIPTOR,
	IGNORE_FRIEND_REQUEST_DESCRIPTOR,
	INCOMING_FRIEND_REQUEST_STATUS_DESCRIPTOR,
	OUTGOING_FRIEND_REQUEST_STATUS_DESCRIPTOR,
} from '@app/features/relationship/utils/RelationshipMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {StartVideoCallMenuItem, StartVoiceCallMenuItem} from '@app/features/ui/action_menu/items/CallMenuItems';
import {RemoveFriendMenuItem} from '@app/features/ui/action_menu/items/RelationshipMenuItems';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {UserContextMenu} from '@app/features/ui/action_menu/UserContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {useContextMenuTrigger} from '@app/features/ui/hooks/useContextMenuTrigger';
import ContextMenu, {isContextMenuNodeTarget} from '@app/features/ui/state/ContextMenu';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import type {StatusType} from '@fluxer/constants/src/StatusConstants';
import {isOfflineStatus} from '@fluxer/constants/src/StatusConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ChatTeardropIcon, CheckIcon, DotsThreeVerticalIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

const ARE_YOU_SURE_YOU_WANT_TO_IGNORE_THE_DESCRIPTOR = msg({
	message: 'Ignore the friend request from {displayName}?',
	comment:
		'Confirmation prompt in the channel and chat friend list item. Preserve {displayName}; it is inserted by code.',
});
const ARE_YOU_SURE_YOU_WANT_TO_CANCEL_YOUR_DESCRIPTOR = msg({
	message: 'Cancel your friend request to {displayName}?',
	comment:
		'Confirmation prompt in the channel and chat friend list item. Preserve {displayName}; it is inserted by code.',
});
const SEND_MESSAGE_DESCRIPTOR = msg({
	message: 'Send message',
	comment: 'Button or menu action label in the channel and chat friend list item. Keep it concise.',
});
const MORE_DESCRIPTOR = msg({
	message: 'More',
	comment: 'Short label in the channel and chat friend list item. Keep it concise.',
});
const logger = new Logger('FriendListItem');

interface FriendAction {
	icon: React.ReactNode;
	tooltip: string;
	onClick: (e: React.MouseEvent<HTMLButtonElement>, target: HTMLButtonElement) => void;
	className?: string;
	danger?: boolean;
}

interface FriendListItemProps {
	userId: string;
	relationshipType: number;
	openProfile: (userId: string) => void;
}

export const FriendListItem: React.FC<FriendListItemProps> = observer((props) => {
	const {i18n} = useLingui();
	const {userId, relationshipType, openProfile} = props;
	const itemRef = useRef<HTMLDivElement>(null);
	const [status, setStatus] = useState(() => Presence.getStatus(userId));
	useEffect(() => {
		const handlePresenceUpdate = (_userId: string, newStatus: StatusType) => {
			setStatus(newStatus);
		};
		const unsubscribe = Presence.subscribeToUserStatus(userId, handlePresenceUpdate);
		return () => {
			unsubscribe();
		};
	}, [userId]);
	const contextMenu = ContextMenu.contextMenu;
	const contextMenuTarget = contextMenu?.target.target;
	const contextMenuTargetElement = isContextMenuNodeTarget(contextMenuTarget) ? contextMenuTarget : null;
	const contextMenuOpen = Boolean(contextMenuTargetElement && itemRef.current?.contains(contextMenuTargetElement));
	const {isOpen: moreMenuOpen, withTracking} = useContextMenuTrigger();
	const getStatusText = useCallback(() => {
		switch (relationshipType) {
			case RelationshipTypes.INCOMING_REQUEST:
				return i18n._(INCOMING_FRIEND_REQUEST_STATUS_DESCRIPTOR);
			case RelationshipTypes.OUTGOING_REQUEST:
				return i18n._(OUTGOING_FRIEND_REQUEST_STATUS_DESCRIPTOR);
			default:
				return null;
		}
	}, [i18n, relationshipType]);
	const getStatusClassName = useCallback(() => {
		const isOffline = isOfflineStatus(status);
		if (relationshipType === RelationshipTypes.FRIEND) {
			return isOffline ? styles.friendStatusOffline : styles.friendStatusOnline;
		}
		return styles.friendStatusOffline;
	}, [relationshipType, status]);
	const createDMChannel = useCallback(
		async (e: React.MouseEvent) => {
			e.stopPropagation();
			try {
				await PrivateChannelCommands.openDMChannel(userId);
			} catch (error) {
				logger.error('Failed to open DM channel:', error);
			}
		},
		[userId],
	);
	const ignoreIncomingFriendRequest = useCallback(
		(e: React.MouseEvent) => {
			e.stopPropagation();
			const user = Users.getUser(userId);
			if (!user) return;
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(IGNORE_FRIEND_REQUEST_DESCRIPTOR)}
						description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_IGNORE_THE_DESCRIPTOR, {
							displayName: NicknameUtils.getDisplayName(user),
						})}
						primaryText={i18n._(IGNORE_FRIEND_REQUEST_ACTION_DESCRIPTOR)}
						primaryVariant="danger"
						onPrimary={() => RelationshipCommands.removeRelationship(userId)}
						data-flx="channel.friends.friend-list-item.ignore-incoming-friend-request.confirm-modal"
					/>
				)),
			);
		},
		[userId],
	);
	const cancelOutgoingFriendRequest = useCallback(
		(e: React.MouseEvent) => {
			e.stopPropagation();
			const user = Users.getUser(userId);
			if (!user) return;
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(CANCEL_FRIEND_REQUEST_DESCRIPTOR)}
						description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_CANCEL_YOUR_DESCRIPTOR, {
							displayName: NicknameUtils.getDisplayName(user),
						})}
						primaryText={i18n._(CANCEL_FRIEND_REQUEST_DESCRIPTOR)}
						primaryVariant="danger"
						onPrimary={() => RelationshipCommands.removeRelationship(userId)}
						data-flx="channel.friends.friend-list-item.cancel-outgoing-friend-request.confirm-modal"
					/>
				)),
			);
		},
		[userId],
	);
	const acceptFriendRequest = useCallback(
		(e: React.MouseEvent) => {
			e.stopPropagation();
			const user = Users.getUser(userId);
			if (!user) return;
			RelationshipActionUtils.showAcceptFriendRequestConfirmation(i18n, user, {
				bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(e),
				showShiftBypassConfirmationTip: true,
			});
		},
		[i18n, userId],
	);
	const handleContextMenuClick = useCallback(
		(e: React.MouseEvent<HTMLButtonElement>, target: HTMLButtonElement) => {
			e.stopPropagation();
			const user = Users.getUser(userId);
			if (!user) return;
			e.preventDefault();
			const nativeEvent = e.nativeEvent;
			const hasPointerCoords = !(
				e.clientX === 0 &&
				e.clientY === 0 &&
				nativeEvent.detail === 0 &&
				nativeEvent.button === 0
			);
			const point = hasPointerCoords
				? {x: e.clientX + 2, y: e.clientY + 2}
				: (() => {
						const rect = target.getBoundingClientRect();
						return {x: rect.left + rect.width / 2 + 2, y: rect.top + rect.height / 2 + 2};
					})();
			ContextMenuCommands.openAtPoint(
				point,
				({onClose}) => (
					<>
						<MenuGroup data-flx="channel.friends.friend-list-item.handle-context-menu-click.menu-group">
							<StartVoiceCallMenuItem
								user={user}
								onClose={onClose}
								data-flx="channel.friends.friend-list-item.handle-context-menu-click.start-voice-call-menu-item"
							/>
							<StartVideoCallMenuItem
								user={user}
								onClose={onClose}
								data-flx="channel.friends.friend-list-item.handle-context-menu-click.start-video-call-menu-item"
							/>
						</MenuGroup>
						<MenuGroup data-flx="channel.friends.friend-list-item.handle-context-menu-click.menu-group--2">
							<RemoveFriendMenuItem
								user={user}
								onClose={onClose}
								data-flx="channel.friends.friend-list-item.handle-context-menu-click.remove-friend-menu-item"
							/>
						</MenuGroup>
					</>
				),
				withTracking(),
				target,
			);
		},
		[userId, withTracking],
	);
	const handleUserContextMenu = useCallback(
		(e: React.MouseEvent) => {
			e.preventDefault();
			e.stopPropagation();
			const user = Users.getUser(userId);
			if (!user) return;
			ContextMenuCommands.openFromEvent(e, ({onClose}) => (
				<UserContextMenu
					user={user}
					onClose={onClose}
					data-flx="channel.friends.friend-list-item.handle-user-context-menu.user-context-menu"
				/>
			));
		},
		[userId],
	);
	const getFriendActions = useCallback((): Array<FriendAction> => {
		switch (relationshipType) {
			case RelationshipTypes.FRIEND:
				return [
					{
						icon: (
							<ChatTeardropIcon
								weight="fill"
								className={styles.iconSize}
								data-flx="channel.friends.friend-list-item.get-friend-actions.icon-size"
							/>
						),
						tooltip: i18n._(SEND_MESSAGE_DESCRIPTOR),
						onClick: createDMChannel,
						className: styles.actionButtonMessage,
					},
					{
						icon: (
							<DotsThreeVerticalIcon
								weight="bold"
								className={styles.iconSize}
								data-flx="channel.friends.friend-list-item.get-friend-actions.icon-size--2"
							/>
						),
						tooltip: i18n._(MORE_DESCRIPTOR),
						onClick: handleContextMenuClick,
						className: clsx(styles.actionButtonMore, moreMenuOpen && styles.contextMenuButtonActive),
					},
				];
			case RelationshipTypes.INCOMING_REQUEST:
				return [
					{
						icon: (
							<CheckIcon
								weight="bold"
								size={remFromPx(20)}
								data-flx="channel.friends.friend-list-item.get-friend-actions.check-icon"
							/>
						),
						tooltip: i18n._(ACCEPT_FRIEND_REQUEST_ACTION_DESCRIPTOR),
						onClick: acceptFriendRequest,
						className: styles.actionButtonAccept,
					},
					{
						icon: (
							<XIcon
								weight="bold"
								size={remFromPx(20)}
								data-flx="channel.friends.friend-list-item.get-friend-actions.x-icon"
							/>
						),
						tooltip: i18n._(IGNORE_FRIEND_REQUEST_ACTION_DESCRIPTOR),
						onClick: ignoreIncomingFriendRequest,
						className: styles.actionButtonIgnore,
					},
				];
			case RelationshipTypes.OUTGOING_REQUEST:
				return [
					{
						icon: (
							<XIcon
								weight="bold"
								size={remFromPx(20)}
								data-flx="channel.friends.friend-list-item.get-friend-actions.x-icon--2"
							/>
						),
						tooltip: i18n._(CANCEL_DESCRIPTOR),
						onClick: cancelOutgoingFriendRequest,
						className: styles.actionButtonCancel,
					},
				];
			default:
				return [];
		}
	}, [
		relationshipType,
		createDMChannel,
		handleContextMenuClick,
		acceptFriendRequest,
		ignoreIncomingFriendRequest,
		cancelOutgoingFriendRequest,
		moreMenuOpen,
		i18n,
	]);
	const user = Users.getUser(userId);
	const customStatus = usePresenceCustomStatus({
		userId,
		enabled: relationshipType === RelationshipTypes.FRIEND,
	});
	if (!user) return null;
	const actions = getFriendActions();
	const hasCustomStatus = customStatus !== null;
	return (
		<FocusRing data-flx="channel.friends.friend-list-item.focus-ring">
			<div
				ref={itemRef}
				className={clsx(styles.friendListItem, contextMenuOpen && styles.contextMenuActive)}
				onClick={() => openProfile(userId)}
				onContextMenu={handleUserContextMenu}
				role="button"
				tabIndex={0}
				onKeyDown={(e) => {
					if (!isKeyboardActivationKey(e.key)) return;
					e.preventDefault();
					openProfile(userId);
				}}
				data-flx="channel.friends.friend-list-item.friend-list-item.open-profile"
			>
				<div className={styles.friendInfo} data-flx="channel.friends.friend-list-item.friend-info">
					<StatusAwareAvatar
						user={user}
						size={36}
						status={status}
						data-flx="channel.friends.friend-list-item.status-aware-avatar"
					/>
					<div className={styles.friendDetails} data-flx="channel.friends.friend-list-item.friend-details">
						<div className={styles.friendNameRow} data-flx="channel.friends.friend-list-item.friend-name-row">
							<span className={styles.friendName} data-flx="channel.friends.friend-list-item.friend-name">
								{NicknameUtils.getNickname(user)}
							</span>
							<span className={styles.friendTag} data-flx="channel.friends.friend-list-item.friend-tag">
								{NicknameUtils.formatTagForStreamerMode(user.tag)}
							</span>
						</div>
						{hasCustomStatus ? (
							<CustomStatusDisplay
								customStatus={customStatus}
								className={styles.friendSubtext}
								showTooltip
								constrained
								animateOnParentHover
								data-flx="channel.friends.friend-list-item.friend-subtext"
							/>
						) : (
							<span
								className={clsx(styles.friendSubtext, getStatusClassName())}
								data-flx="channel.friends.friend-list-item.friend-subtext--2"
							>
								{getStatusText() || (
									<StatusLabel status={status} data-flx="channel.friends.friend-list-item.status-label" />
								)}
							</span>
						)}
					</div>
				</div>
				<div
					className={styles.friendActions}
					role="group"
					onClick={(e) => e.stopPropagation()}
					onKeyDown={stopPropagationOnEnterSpace}
					data-flx="channel.friends.friend-list-item.friend-actions.stop-propagation"
				>
					{actions.map((action, index) => (
						<ActionButton
							key={index}
							tooltip={action.tooltip}
							onClick={action.onClick}
							className={action.className}
							danger={action.danger}
							data-flx="channel.friends.friend-list-item.action-button.click"
						>
							{action.icon}
						</ActionButton>
					))}
				</div>
			</div>
		</FocusRing>
	);
});
const StatusLabel = observer(function StatusLabel({status}: {status: string}) {
	const {i18n} = useLingui();
	return <>{getStatusTypeLabel(i18n, status)}</>;
});
