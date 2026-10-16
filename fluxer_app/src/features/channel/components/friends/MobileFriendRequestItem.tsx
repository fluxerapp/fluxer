// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {LongPressable} from '@app/features/app/components/LongPressable';
import styles from '@app/features/channel/components/friends/MobileFriendRequestItem.module.css';
import {VIEW_PROFILE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as RelationshipCommands from '@app/features/relationship/commands/RelationshipCommands';
import * as RelationshipActionUtils from '@app/features/relationship/utils/RelationshipActionUtils';
import {
	ACCEPT_FRIEND_REQUEST_ACTION_DESCRIPTOR,
	IGNORE_FRIEND_REQUEST_ACTION_DESCRIPTOR,
	IGNORE_FRIEND_REQUEST_DESCRIPTOR,
	INCOMING_FRIEND_REQUEST_STATUS_DESCRIPTOR,
	OUTGOING_FRIEND_REQUEST_STATUS_DESCRIPTOR,
} from '@app/features/relationship/utils/RelationshipMessageDescriptors';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import type {MenuGroupType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {MenuBottomSheet} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CheckIcon, DotsThreeVerticalIcon, UserIcon, XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useState} from 'react';

const ARE_YOU_SURE_YOU_WANT_TO_IGNORE_THE_DESCRIPTOR = msg({
	message: 'Ignore the friend request from {displayName}?',
	comment:
		'Confirmation prompt in the channel and chat mobile friend request item. Preserve {displayName}; it is inserted by code.',
});
const CANCEL_REQUEST_DESCRIPTOR = msg({
	message: 'Cancel request',
	comment: 'Button or menu action label in the channel and chat mobile friend request item. Keep it concise.',
});
const FRIEND_REQUEST_ACTIONS_FOR_DESCRIPTOR = msg({
	message: 'Friend request actions for {displayName}',
	comment:
		'Label in the channel and chat mobile friend request item. {displayName} is the friend request sender or recipient name.',
});

interface MobileFriendRequestItemProps {
	userId: string;
	relationshipType: number;
}

export const MobileFriendRequestItem: React.FC<MobileFriendRequestItemProps> = observer(
	({userId, relationshipType}) => {
		const {i18n} = useLingui();
		const [menuOpen, setMenuOpen] = useState(false);
		const user = Users.getUser(userId);
		if (!user) return null;
		const closeMenu = () => setMenuOpen(false);
		const handleViewProfile = () => {
			ModalCommands.runAfterBottomSheetClose(closeMenu, () => UserProfileCommands.openUserProfile(user.id));
		};
		const handleAccept = () => {
			ModalCommands.runAfterBottomSheetClose(closeMenu, () =>
				RelationshipActionUtils.showAcceptFriendRequestConfirmation(i18n, user),
			);
		};
		const handleIgnore = () => {
			ModalCommands.pushAfterBottomSheetClose(
				closeMenu,
				modal(() => (
					<ConfirmModal
						title={i18n._(IGNORE_FRIEND_REQUEST_DESCRIPTOR)}
						description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_IGNORE_THE_DESCRIPTOR, {
							displayName: NicknameUtils.getDisplayName(user),
						})}
						primaryText={i18n._(IGNORE_FRIEND_REQUEST_ACTION_DESCRIPTOR)}
						onPrimary={() => RelationshipCommands.removeRelationship(userId)}
						data-flx="channel.friends.mobile-friend-request-item.handle-ignore.confirm-modal"
					/>
				)),
			);
		};
		const handleCancel = () => {
			RelationshipCommands.removeRelationship(userId);
			closeMenu();
		};
		const menuGroups: Array<MenuGroupType> = [
			{
				items: [
					{
						icon: (
							<UserIcon
								weight="fill"
								className={styles.iconSize}
								data-flx="channel.friends.mobile-friend-request-item.icon-size"
							/>
						),
						label: i18n._(VIEW_PROFILE_DESCRIPTOR),
						onClick: handleViewProfile,
					},
				],
			},
		];
		if (relationshipType === RelationshipTypes.INCOMING_REQUEST) {
			menuGroups.push({
				items: [
					{
						icon: (
							<CheckIcon
								weight="bold"
								className={styles.iconSize}
								data-flx="channel.friends.mobile-friend-request-item.icon-size--2"
							/>
						),
						label: i18n._(ACCEPT_FRIEND_REQUEST_ACTION_DESCRIPTOR),
						onClick: handleAccept,
					},
					{
						icon: (
							<XIcon
								weight="bold"
								className={styles.iconSize}
								data-flx="channel.friends.mobile-friend-request-item.icon-size--3"
							/>
						),
						label: i18n._(IGNORE_FRIEND_REQUEST_ACTION_DESCRIPTOR),
						onClick: handleIgnore,
						danger: true,
					},
				],
			});
		} else if (relationshipType === RelationshipTypes.OUTGOING_REQUEST) {
			menuGroups.push({
				items: [
					{
						icon: (
							<XIcon
								weight="bold"
								className={styles.iconSize}
								data-flx="channel.friends.mobile-friend-request-item.icon-size--4"
							/>
						),
						label: i18n._(CANCEL_REQUEST_DESCRIPTOR),
						onClick: handleCancel,
						danger: true,
					},
				],
			});
		}
		const statusText = i18n._(
			relationshipType === RelationshipTypes.INCOMING_REQUEST
				? INCOMING_FRIEND_REQUEST_STATUS_DESCRIPTOR
				: OUTGOING_FRIEND_REQUEST_STATUS_DESCRIPTOR,
		);
		return (
			<>
				<LongPressable
					className={styles.requestItem}
					onLongPress={() => setMenuOpen(true)}
					data-flx="channel.friends.mobile-friend-request-item.request-item"
				>
					<StatusAwareAvatar
						user={user}
						size={40}
						data-flx="channel.friends.mobile-friend-request-item.status-aware-avatar"
					/>
					<div className={styles.userInfo} data-flx="channel.friends.mobile-friend-request-item.user-info">
						<span className={styles.userName} data-flx="channel.friends.mobile-friend-request-item.user-name">
							{NicknameUtils.getNickname(user, null)}
						</span>
						<span className={styles.requestStatus} data-flx="channel.friends.mobile-friend-request-item.request-status">
							{statusText}
						</span>
					</div>
					<FocusRing offset={-2} data-flx="channel.friends.mobile-friend-request-item.focus-ring">
						<button
							type="button"
							onClick={() => setMenuOpen(true)}
							className={styles.actionButton}
							aria-label={i18n._(FRIEND_REQUEST_ACTIONS_FOR_DESCRIPTOR, {
								displayName: NicknameUtils.getNickname(user, null),
							})}
							aria-haspopup="dialog"
							data-flx="channel.friends.mobile-friend-request-item.action-button.set-menu-open"
						>
							<DotsThreeVerticalIcon
								weight="bold"
								className={styles.iconSize}
								data-flx="channel.friends.mobile-friend-request-item.icon-size--5"
							/>
						</button>
					</FocusRing>
				</LongPressable>
				<MenuBottomSheet
					isOpen={menuOpen}
					onClose={() => setMenuOpen(false)}
					groups={menuGroups}
					data-flx="channel.friends.mobile-friend-request-item.menu-bottom-sheet"
				/>
			</>
		);
	},
);
