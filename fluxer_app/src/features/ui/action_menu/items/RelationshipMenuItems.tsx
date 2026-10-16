// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {CHANGE_FRIEND_NICKNAME_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {BLOCK_DESCRIPTOR} from '@app/features/moderation/utils/ModerationMessageDescriptors';
import {ChangeFriendNicknameModal} from '@app/features/relationship/components/modals/ChangeFriendNicknameModal';
import Relationships from '@app/features/relationship/state/Relationships';
import * as RelationshipActionUtils from '@app/features/relationship/utils/RelationshipActionUtils';
import {
	ACCEPT_FRIEND_REQUEST_DESCRIPTOR,
	ADD_FRIEND_DESCRIPTOR,
	CANCEL_FRIEND_REQUEST_DESCRIPTOR,
	IGNORE_FRIEND_REQUEST_DESCRIPTOR,
	OUTGOING_FRIEND_REQUEST_STATUS_DESCRIPTOR,
	REMOVE_FRIEND_DESCRIPTOR,
	UNBLOCK_USER_ACTION_DESCRIPTOR,
} from '@app/features/relationship/utils/RelationshipMessageDescriptors';
import {
	AcceptFriendRequestIcon,
	BlockUserIcon,
	CancelFriendRequestIcon,
	ChangeNicknameIcon,
	IgnoreFriendRequestIcon,
	RemoveFriendIcon,
	SendFriendRequestIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import {PublicUserFlags, RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useState} from 'react';

const CLAIM_YOUR_ACCOUNT_TO_SEND_FRIEND_REQUESTS_DESCRIPTOR = msg({
	message: 'Claim your account to send friend requests.',
	comment: 'Tooltip shown when an unclaimed account tries to send a friend request.',
});
const VERIFY_YOUR_EMAIL_ADDRESS_TO_SEND_FRIEND_REQUESTS_DESCRIPTOR = msg({
	message: 'Verify your email address to send friend requests.',
	comment: 'Tooltip shown when an unverified account tries to send a friend request.',
});

interface SendFriendRequestMenuItemProps {
	user: User;
	onClose: () => void;
}

const SendFriendRequestMenuItem: React.FC<SendFriendRequestMenuItemProps> = observer(({user, onClose}) => {
	const {i18n} = useLingui();
	const relationshipType = Relationships.getRelationship(user.id)?.type;
	const [submitting, setSubmitting] = useState(false);
	const showFriendRequestSent = relationshipType === RelationshipTypes.OUTGOING_REQUEST;
	const isCurrentUserStaff = Users.currentUser?.isStaff() ?? false;
	const isCurrentUserUnclaimed = !(Users.currentUser?.isClaimed() ?? true);
	const isCurrentUserUnverified = Users.currentUser?.verified === false;
	const handleSendFriendRequest = useCallback(async () => {
		if (submitting || showFriendRequestSent) return;
		if (isCurrentUserStaff) {
			ModalCommands.runAfterBottomSheetClose(onClose, () =>
				RelationshipActionUtils.showSendFriendRequestConfirmation(i18n, user),
			);
			return;
		}
		setSubmitting(true);
		try {
			await RelationshipActionUtils.sendFriendRequest(i18n, user.id);
		} finally {
			setSubmitting(false);
		}
	}, [i18n, isCurrentUserStaff, onClose, showFriendRequestSent, submitting, user]);
	if (RuntimeConfig.directMessagesDisabled) {
		return null;
	}
	if (isCurrentUserUnclaimed || isCurrentUserUnverified) {
		const tooltip = isCurrentUserUnclaimed
			? i18n._(CLAIM_YOUR_ACCOUNT_TO_SEND_FRIEND_REQUESTS_DESCRIPTOR)
			: i18n._(VERIFY_YOUR_EMAIL_ADDRESS_TO_SEND_FRIEND_REQUESTS_DESCRIPTOR);
		return (
			<Tooltip
				text={tooltip}
				maxWidth="xl"
				data-flx="ui.action-menu.items.relationship-menu-items.send-friend-request-menu-item.tooltip"
			>
				<div data-flx="ui.action-menu.items.relationship-menu-items.send-friend-request-menu-item.div">
					<MenuItem
						icon={
							<SendFriendRequestIcon data-flx="ui.action-menu.items.relationship-menu-items.send-friend-request-menu-item.send-friend-request-icon" />
						}
						onClick={handleSendFriendRequest}
						disabled={true}
						closeOnSelect={false}
						data-flx="ui.action-menu.items.relationship-menu-items.send-friend-request-menu-item.menu-item.send-friend-request"
					>
						{showFriendRequestSent ? i18n._(OUTGOING_FRIEND_REQUEST_STATUS_DESCRIPTOR) : i18n._(ADD_FRIEND_DESCRIPTOR)}
					</MenuItem>
				</div>
			</Tooltip>
		);
	}
	return (
		<MenuItem
			icon={
				<SendFriendRequestIcon data-flx="ui.action-menu.items.relationship-menu-items.send-friend-request-menu-item.send-friend-request-icon--2" />
			}
			onClick={handleSendFriendRequest}
			disabled={submitting || showFriendRequestSent}
			closeOnSelect={false}
			data-flx="ui.action-menu.items.relationship-menu-items.send-friend-request-menu-item.menu-item.send-friend-request--2"
		>
			{showFriendRequestSent ? i18n._(OUTGOING_FRIEND_REQUEST_STATUS_DESCRIPTOR) : i18n._(ADD_FRIEND_DESCRIPTOR)}
		</MenuItem>
	);
});

interface AcceptFriendRequestMenuItemProps {
	user: User;
	onClose: () => void;
}

const AcceptFriendRequestMenuItem: React.FC<AcceptFriendRequestMenuItemProps> = observer(({user, onClose}) => {
	const {i18n} = useLingui();
	const handleAcceptFriendRequest = useCallback(
		(event?: {shiftKey?: boolean}) => {
			ModalCommands.runAfterBottomSheetClose(onClose, () =>
				RelationshipActionUtils.showAcceptFriendRequestConfirmation(i18n, user, {
					bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
					showShiftBypassConfirmationTip: true,
				}),
			);
		},
		[i18n, onClose, user],
	);
	if (RuntimeConfig.directMessagesDisabled) {
		return null;
	}
	return (
		<MenuItem
			icon={
				<AcceptFriendRequestIcon data-flx="ui.action-menu.items.relationship-menu-items.accept-friend-request-menu-item.accept-friend-request-icon" />
			}
			onClick={handleAcceptFriendRequest}
			data-flx="ui.action-menu.items.relationship-menu-items.accept-friend-request-menu-item.menu-item.accept-friend-request"
		>
			{i18n._(ACCEPT_FRIEND_REQUEST_DESCRIPTOR)}
		</MenuItem>
	);
});

interface RemoveFriendMenuItemProps {
	user: User;
	onClose: () => void;
	danger?: boolean;
}

export const RemoveFriendMenuItem: React.FC<RemoveFriendMenuItemProps> = observer(({user, onClose, danger = true}) => {
	const {i18n} = useLingui();
	const handleRemoveFriend = useCallback(
		(event?: {shiftKey?: boolean}) => {
			ModalCommands.runAfterBottomSheetClose(onClose, () =>
				RelationshipActionUtils.showRemoveFriendConfirmation(i18n, user, {
					bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
					showShiftBypassConfirmationTip: true,
				}),
			);
		},
		[i18n, user, onClose],
	);
	if (RuntimeConfig.directMessagesDisabled) {
		return null;
	}
	return (
		<MenuItem
			icon={
				<RemoveFriendIcon data-flx="ui.action-menu.items.relationship-menu-items.remove-friend-menu-item.remove-friend-icon" />
			}
			onClick={handleRemoveFriend}
			danger={danger}
			data-flx="ui.action-menu.items.relationship-menu-items.remove-friend-menu-item.menu-item.remove-friend"
		>
			{i18n._(REMOVE_FRIEND_DESCRIPTOR)}
		</MenuItem>
	);
});

interface ChangeFriendNicknameMenuItemProps {
	user: User;
	onClose: () => void;
}

export const ChangeFriendNicknameMenuItem: React.FC<ChangeFriendNicknameMenuItemProps> = observer(({user, onClose}) => {
	const {i18n} = useLingui();
	const relationship = Relationships.getRelationship(user.id);
	const handleChangeNickname = useCallback(() => {
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<ChangeFriendNicknameModal
					user={user}
					data-flx="ui.action-menu.items.relationship-menu-items.handle-change-nickname.change-friend-nickname-modal"
				/>
			)),
		);
	}, [onClose, user]);
	if (RuntimeConfig.directMessagesDisabled) {
		return null;
	}
	if (relationship?.type !== RelationshipTypes.FRIEND) {
		return null;
	}
	return (
		<MenuItem
			icon={
				<ChangeNicknameIcon data-flx="ui.action-menu.items.relationship-menu-items.change-friend-nickname-menu-item.change-nickname-icon" />
			}
			onClick={handleChangeNickname}
			data-flx="ui.action-menu.items.relationship-menu-items.change-friend-nickname-menu-item.menu-item.change-nickname"
		>
			{i18n._(CHANGE_FRIEND_NICKNAME_DESCRIPTOR)}
		</MenuItem>
	);
});

interface IgnoreFriendRequestMenuItemProps {
	user: User;
	onClose: () => void;
}

const IgnoreFriendRequestMenuItem: React.FC<IgnoreFriendRequestMenuItemProps> = observer(({user, onClose}) => {
	const {i18n} = useLingui();
	const handleIgnoreFriendRequest = useCallback(() => {
		onClose();
		RelationshipActionUtils.ignoreFriendRequest(i18n, user.id);
	}, [i18n, user.id, onClose]);
	if (RuntimeConfig.directMessagesDisabled) {
		return null;
	}
	return (
		<MenuItem
			icon={
				<IgnoreFriendRequestIcon data-flx="ui.action-menu.items.relationship-menu-items.ignore-friend-request-menu-item.ignore-friend-request-icon" />
			}
			onClick={handleIgnoreFriendRequest}
			data-flx="ui.action-menu.items.relationship-menu-items.ignore-friend-request-menu-item.menu-item.ignore-friend-request"
		>
			{i18n._(IGNORE_FRIEND_REQUEST_DESCRIPTOR)}
		</MenuItem>
	);
});

interface CancelFriendRequestMenuItemProps {
	user: User;
	onClose: () => void;
}

const CancelFriendRequestMenuItem: React.FC<CancelFriendRequestMenuItemProps> = observer(({user, onClose}) => {
	const {i18n} = useLingui();
	const handleCancelFriendRequest = useCallback(() => {
		onClose();
		RelationshipActionUtils.cancelFriendRequest(i18n, user.id);
	}, [i18n, user.id, onClose]);
	if (RuntimeConfig.directMessagesDisabled) {
		return null;
	}
	return (
		<MenuItem
			icon={
				<CancelFriendRequestIcon data-flx="ui.action-menu.items.relationship-menu-items.cancel-friend-request-menu-item.cancel-friend-request-icon" />
			}
			onClick={handleCancelFriendRequest}
			data-flx="ui.action-menu.items.relationship-menu-items.cancel-friend-request-menu-item.menu-item.cancel-friend-request"
		>
			{i18n._(CANCEL_FRIEND_REQUEST_DESCRIPTOR)}
		</MenuItem>
	);
});

interface BlockUserMenuItemProps {
	user: User;
	onClose: () => void;
}

export const BlockUserMenuItem: React.FC<BlockUserMenuItemProps> = observer(({user, onClose}) => {
	const {i18n} = useLingui();
	const handleBlockUser = useCallback(
		(event?: {shiftKey?: boolean}) => {
			ModalCommands.runAfterBottomSheetClose(onClose, () =>
				RelationshipActionUtils.showBlockUserConfirmation(i18n, user, {
					bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
					showShiftBypassConfirmationTip: true,
				}),
			);
		},
		[i18n, user, onClose],
	);
	if (user.system) {
		return null;
	}
	return (
		<MenuItem
			icon={
				<BlockUserIcon data-flx="ui.action-menu.items.relationship-menu-items.block-user-menu-item.block-user-icon" />
			}
			onClick={handleBlockUser}
			danger
			data-flx="ui.action-menu.items.relationship-menu-items.block-user-menu-item.menu-item.block-user"
		>
			{i18n._(BLOCK_DESCRIPTOR)}
		</MenuItem>
	);
});

interface UnblockUserMenuItemProps {
	user: User;
	onClose: () => void;
}

export const UnblockUserMenuItem: React.FC<UnblockUserMenuItemProps> = observer(({user, onClose}) => {
	const {i18n} = useLingui();
	const handleUnblockUser = useCallback(
		(event?: {shiftKey?: boolean}) => {
			ModalCommands.runAfterBottomSheetClose(onClose, () =>
				RelationshipActionUtils.showUnblockUserConfirmation(i18n, user, {
					bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
					showShiftBypassConfirmationTip: true,
				}),
			);
		},
		[i18n, user, onClose],
	);
	if (user.system) {
		return null;
	}
	return (
		<MenuItem
			icon={
				<BlockUserIcon data-flx="ui.action-menu.items.relationship-menu-items.unblock-user-menu-item.block-user-icon" />
			}
			onClick={handleUnblockUser}
			data-flx="ui.action-menu.items.relationship-menu-items.unblock-user-menu-item.menu-item.unblock-user"
		>
			{i18n._(UNBLOCK_USER_ACTION_DESCRIPTOR)}
		</MenuItem>
	);
});

interface RelationshipActionMenuItemProps {
	user: User;
	onClose: () => void;
}

export const RelationshipActionMenuItem: React.FC<RelationshipActionMenuItemProps> = observer(({user, onClose}) => {
	const {i18n} = useLingui();
	const relationship = Relationships.getRelationship(user.id);
	const relationshipType = relationship?.type;
	if (RuntimeConfig.directMessagesDisabled) {
		return null;
	}
	const isFriendlyBot = user.bot && (user.flags & PublicUserFlags.FRIENDLY_BOT) === PublicUserFlags.FRIENDLY_BOT;
	if (user.bot && !isFriendlyBot) {
		if (relationshipType === RelationshipTypes.FRIEND) {
			return (
				<RemoveFriendMenuItem
					user={user}
					onClose={onClose}
					danger={false}
					data-flx="ui.action-menu.items.relationship-menu-items.relationship-action-menu-item.remove-friend-menu-item"
				/>
			);
		}
		if (relationshipType === RelationshipTypes.INCOMING_REQUEST) {
			return (
				<IgnoreFriendRequestMenuItem
					user={user}
					onClose={onClose}
					data-flx="ui.action-menu.items.relationship-menu-items.relationship-action-menu-item.ignore-friend-request-menu-item"
				/>
			);
		}
		if (relationshipType === RelationshipTypes.OUTGOING_REQUEST) {
			return (
				<CancelFriendRequestMenuItem
					user={user}
					onClose={onClose}
					data-flx="ui.action-menu.items.relationship-menu-items.relationship-action-menu-item.cancel-friend-request-menu-item"
				/>
			);
		}
		return null;
	}
	switch (relationshipType) {
		case RelationshipTypes.FRIEND:
			return (
				<RemoveFriendMenuItem
					user={user}
					onClose={onClose}
					danger={false}
					data-flx="ui.action-menu.items.relationship-menu-items.relationship-action-menu-item.remove-friend-menu-item--2"
				/>
			);
		case RelationshipTypes.INCOMING_REQUEST:
			return (
				<>
					<AcceptFriendRequestMenuItem
						user={user}
						onClose={onClose}
						data-flx="ui.action-menu.items.relationship-menu-items.relationship-action-menu-item.accept-friend-request-menu-item"
					/>
					<IgnoreFriendRequestMenuItem
						user={user}
						onClose={onClose}
						data-flx="ui.action-menu.items.relationship-menu-items.relationship-action-menu-item.ignore-friend-request-menu-item--2"
					/>
				</>
			);
		case RelationshipTypes.OUTGOING_REQUEST:
			return (
				<MenuItem
					icon={
						<SendFriendRequestIcon data-flx="ui.action-menu.items.relationship-menu-items.relationship-action-menu-item.send-friend-request-icon" />
					}
					disabled
					closeOnSelect={false}
					data-flx="ui.action-menu.items.relationship-menu-items.relationship-action-menu-item.menu-item"
				>
					{i18n._(OUTGOING_FRIEND_REQUEST_STATUS_DESCRIPTOR)}
				</MenuItem>
			);
		case RelationshipTypes.BLOCKED:
			return null;
		default:
			return (
				<SendFriendRequestMenuItem
					user={user}
					onClose={onClose}
					data-flx="ui.action-menu.items.relationship-menu-items.relationship-action-menu-item.send-friend-request-menu-item"
				/>
			);
	}
});
