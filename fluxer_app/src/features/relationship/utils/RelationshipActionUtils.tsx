// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {BLOCK_DESCRIPTOR} from '@app/features/moderation/utils/ModerationMessageDescriptors';
import * as RelationshipCommands from '@app/features/relationship/commands/RelationshipCommands';
import {
	showAcceptFriendRequestErrorModal,
	showBlockUserErrorModal,
	showCancelFriendRequestErrorModal,
	showIgnoreFriendRequestErrorModal,
	showRemoveFriendErrorModal,
	showSendFriendRequestErrorModal,
	showUnblockUserErrorModal,
} from '@app/features/relationship/components/alerts/RelationshipErrorModalUtils';
import Relationships from '@app/features/relationship/state/Relationships';
import {
	ACCEPT_FRIEND_REQUEST_ACTION_DESCRIPTOR,
	ACCEPT_FRIEND_REQUEST_DESCRIPTOR,
	ADD_FRIEND_DESCRIPTOR,
	FRIEND_ADDED_DESCRIPTOR,
	OUTGOING_FRIEND_REQUEST_STATUS_DESCRIPTOR,
	REMOVE_FRIEND_DESCRIPTOR,
	UNBLOCK_USER_ACTION_DESCRIPTOR,
} from '@app/features/relationship/utils/RelationshipMessageDescriptors';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const THIS_USER_ISN_T_ACCEPTING_FRIEND_REQUESTS_RIGHT_DESCRIPTOR = msg({
	message: "They're not accepting friend requests right now.",
	comment:
		'Error toast when sending a friend request fails because the target user has disabled friend requests. Keep tone plain and not blaming.',
});
const UNBLOCK_THIS_USER_BEFORE_SENDING_A_FRIEND_REQUEST_DESCRIPTOR = msg({
	message: 'Unblock them first to send a friend request.',
	comment:
		'Error toast when sending a friend request fails because the current user has the target blocked. Instructs the user to unblock first.',
});
const YOU_CAN_T_SEND_A_FRIEND_REQUEST_TO_DESCRIPTOR = msg({
	message: "You can't send a friend request to yourself.",
	comment: 'Error toast when sending a friend request fails because the target is the current user.',
});
const YOU_RE_ALREADY_FRIENDS_WITH_THIS_USER_DESCRIPTOR = msg({
	message: "You're already friends with this user.",
	comment: 'Error toast when sending a friend request fails because the users are already friends.',
});
const YOU_NEED_TO_CLAIM_YOUR_ACCOUNT_TO_SEND_DESCRIPTOR = msg({
	message: 'Finish signing up to send friend requests.',
	comment:
		'Error toast when sending a friend request fails because the current user is on an unclaimed (guest) account. "Claim your account" means complete sign-up.',
});
const VERIFY_EMAIL_TO_SEND_FRIEND_REQUESTS_DESCRIPTOR = msg({
	message: 'Verify your email before sending friend requests.',
	comment: 'Error toast when sending a friend request fails because the current account email is unverified.',
});
const FAILED_TO_SEND_FRIEND_REQUEST_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: "Couldn't send the friend request. Try again.",
	comment: 'Generic error toast when sending a friend request fails with no recognized API error code.',
});
const SEND_A_FRIEND_REQUEST_TO_OR_USE_THE_DESCRIPTOR = msg({
	message: 'Send a friend request to {userName}, or use the staff override to add them immediately.',
	comment:
		'Body of the staff-only add-friend confirmation modal. The staff override is a Fluxer-staff-only feature that bypasses the normal request flow. {userName} is the target user.',
});
const ADD_IMMEDIATELY_WITH_STAFF_OVERRIDE_DESCRIPTOR = msg({
	message: 'Add immediately with staff override',
	comment:
		'Checkbox label inside the staff-only add-friend confirmation modal. Enabling it skips the friend-request step and friends the user instantly. Staff-only.',
});
const ARE_YOU_SURE_YOU_WANT_TO_ACCEPT_THE_DESCRIPTOR = msg({
	message: 'Accept the friend request from {userName}?',
	comment:
		'Confirmation body shown before accepting an incoming friend request. {userName} is the requester. Plain confirmation tone.',
});
const ARE_YOU_SURE_YOU_WANT_TO_REMOVE_AS_DESCRIPTOR = msg({
	message: 'Remove {userName} from your friends?',
	comment: 'Confirmation body for the destructive remove-friend action. {userName} is the friend being removed.',
});
const BLOCK_USER_DESCRIPTOR = msg({
	message: 'Block user',
	comment: 'Title of the destructive confirmation modal for blocking a user. Short command title.',
});
const ARE_YOU_SURE_YOU_WANT_TO_BLOCK_THEY_DESCRIPTOR = msg({
	message: "Block {userName}? They won't be able to message or friend you.",
	comment:
		'Body of the destructive confirmation modal for blocking a user. Explains the consequences. {userName} is the target user.',
});
const UNBLOCK_USER_DESCRIPTOR = msg({
	message: 'Unblock user',
	comment: 'Title of the confirmation modal for unblocking a previously blocked user. Short command title.',
});
const ARE_YOU_SURE_YOU_WANT_TO_UNBLOCK_DESCRIPTOR = msg({
	message: 'Unblock {userName}?',
	comment: 'Body of the confirmation modal for unblocking a previously blocked user. {userName} is the target user.',
});

export interface RelationshipConfirmationOptions {
	bypassConfirm?: boolean;
	showShiftBypassConfirmationTip?: boolean;
}

export interface RelationshipConfirmationEvent {
	shiftKey?: boolean;
}

export interface SendFriendRequestOptions {
	staffForceAccept?: boolean;
}

export function shouldBypassRelationshipConfirmation(event?: RelationshipConfirmationEvent | null): boolean {
	return Boolean(event?.shiftKey);
}

export function getSendFriendRequestErrorMessage(
	i18n: I18n,
	apiCode: string | null | undefined,
	apiMessage: string | null | undefined,
): string {
	if (apiMessage) {
		return apiMessage;
	}
	switch (apiCode) {
		case APIErrorCodes.FRIEND_REQUEST_BLOCKED:
			return i18n._(THIS_USER_ISN_T_ACCEPTING_FRIEND_REQUESTS_RIGHT_DESCRIPTOR);
		case APIErrorCodes.CANNOT_SEND_FRIEND_REQUEST_TO_BLOCKED_USER:
			return i18n._(UNBLOCK_THIS_USER_BEFORE_SENDING_A_FRIEND_REQUEST_DESCRIPTOR);
		case APIErrorCodes.CANNOT_SEND_FRIEND_REQUEST_TO_SELF:
			return i18n._(YOU_CAN_T_SEND_A_FRIEND_REQUEST_TO_DESCRIPTOR);
		case APIErrorCodes.ALREADY_FRIENDS:
			return i18n._(YOU_RE_ALREADY_FRIENDS_WITH_THIS_USER_DESCRIPTOR);
		case APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_SEND_FRIEND_REQUESTS:
			return i18n._(YOU_NEED_TO_CLAIM_YOUR_ACCOUNT_TO_SEND_DESCRIPTOR);
		case APIErrorCodes.FRIEND_REQUEST_EMAIL_VERIFICATION_REQUIRED:
			return i18n._(VERIFY_EMAIL_TO_SEND_FRIEND_REQUESTS_DESCRIPTOR);
		default:
			return i18n._(FAILED_TO_SEND_FRIEND_REQUEST_PLEASE_TRY_AGAIN_DESCRIPTOR);
	}
}

export function canSendFriendRequest(userId: string, isBot: boolean): boolean {
	if (isBot) {
		return false;
	}
	const relationship = Relationships.getRelationship(userId);
	if (!relationship) {
		return true;
	}
	const blockedTypes = [
		RelationshipTypes.FRIEND,
		RelationshipTypes.BLOCKED,
		RelationshipTypes.OUTGOING_REQUEST,
		RelationshipTypes.INCOMING_REQUEST,
	] as const;
	return !blockedTypes.some((type) => type === relationship.type);
}

export async function sendFriendRequest(
	i18n: I18n,
	userId: string,
	options: SendFriendRequestOptions = {},
): Promise<boolean> {
	try {
		await RelationshipCommands.sendFriendRequest(userId, options);
		ToastCommands.success(
			i18n._(options.staffForceAccept ? FRIEND_ADDED_DESCRIPTOR : OUTGOING_FRIEND_REQUEST_STATUS_DESCRIPTOR),
		);
		return true;
	} catch (err) {
		showSendFriendRequestErrorModal(err);
		return false;
	}
}

export function showSendFriendRequestConfirmation(i18n: I18n, user: User): void {
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(ADD_FRIEND_DESCRIPTOR)}
				description={i18n._(SEND_A_FRIEND_REQUEST_TO_OR_USE_THE_DESCRIPTOR, {
					userName: NicknameUtils.getNickname(user, null),
				})}
				primaryText={i18n._(ADD_FRIEND_DESCRIPTOR)}
				onPrimary={async (staffForceAccept) => {
					await sendFriendRequest(i18n, user.id, {staffForceAccept});
				}}
				checkboxContent={
					<Checkbox data-flx="relationship.relationship-action-utils.show-send-friend-request-confirmation.checkbox">
						{i18n._(ADD_IMMEDIATELY_WITH_STAFF_OVERRIDE_DESCRIPTOR)}
					</Checkbox>
				}
				data-flx="relationship.relationship-action-utils.show-send-friend-request-confirmation.confirm-modal"
			/>
		)),
	);
}

export async function acceptFriendRequest(_i18n: I18n, userId: string): Promise<boolean> {
	try {
		await RelationshipCommands.acceptFriendRequest(userId);
		return true;
	} catch (error) {
		showAcceptFriendRequestErrorModal(error);
		return false;
	}
}

export function showAcceptFriendRequestConfirmation(
	i18n: I18n,
	user: User,
	options: RelationshipConfirmationOptions = {},
): void {
	if (options.bypassConfirm) {
		void acceptFriendRequest(i18n, user.id);
		return;
	}
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(ACCEPT_FRIEND_REQUEST_DESCRIPTOR)}
				description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_ACCEPT_THE_DESCRIPTOR, {
					userName: NicknameUtils.getNickname(user, null),
				})}
				primaryText={i18n._(ACCEPT_FRIEND_REQUEST_ACTION_DESCRIPTOR)}
				primaryVariant="primary"
				onPrimary={async () => {
					await acceptFriendRequest(i18n, user.id);
				}}
				showShiftBypassConfirmationTip={options.showShiftBypassConfirmationTip ?? false}
				data-flx="relationship.relationship-action-utils.show-accept-friend-request-confirmation.confirm-modal"
			/>
		)),
	);
}

export async function cancelFriendRequest(_i18n: I18n, userId: string): Promise<boolean> {
	try {
		await RelationshipCommands.removeRelationship(userId);
		return true;
	} catch (error) {
		showCancelFriendRequestErrorModal(error);
		return false;
	}
}

export async function removeFriend(_i18n: I18n, userId: string): Promise<boolean> {
	try {
		await RelationshipCommands.removeRelationship(userId);
		return true;
	} catch (error) {
		showRemoveFriendErrorModal(error);
		return false;
	}
}

export function showRemoveFriendConfirmation(
	i18n: I18n,
	user: User,
	options: RelationshipConfirmationOptions = {},
): void {
	if (options.bypassConfirm) {
		void removeFriend(i18n, user.id);
		return;
	}
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(REMOVE_FRIEND_DESCRIPTOR)}
				description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_REMOVE_AS_DESCRIPTOR, {
					userName: NicknameUtils.getNickname(user, null),
				})}
				primaryText={i18n._(REMOVE_FRIEND_DESCRIPTOR)}
				primaryVariant="danger"
				onPrimary={async () => {
					await removeFriend(i18n, user.id);
				}}
				showShiftBypassConfirmationTip={options.showShiftBypassConfirmationTip ?? false}
				data-flx="relationship.relationship-action-utils.show-remove-friend-confirmation.confirm-modal"
			/>
		)),
	);
}

export async function blockUser(_i18n: I18n, userId: string): Promise<boolean> {
	try {
		await RelationshipCommands.blockUser(userId);
		return true;
	} catch (error) {
		showBlockUserErrorModal(error);
		return false;
	}
}

export function showBlockUserConfirmation(i18n: I18n, user: User, options: RelationshipConfirmationOptions = {}): void {
	if (options.bypassConfirm) {
		void blockUser(i18n, user.id);
		return;
	}
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(BLOCK_USER_DESCRIPTOR)}
				description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_BLOCK_THEY_DESCRIPTOR, {
					userName: NicknameUtils.getNickname(user, null),
				})}
				primaryText={i18n._(BLOCK_DESCRIPTOR)}
				primaryVariant="danger"
				onPrimary={async () => {
					await blockUser(i18n, user.id);
				}}
				showShiftBypassConfirmationTip={options.showShiftBypassConfirmationTip ?? false}
				data-flx="relationship.relationship-action-utils.show-block-user-confirmation.confirm-modal"
			/>
		)),
	);
}

export async function unblockUser(_i18n: I18n, userId: string): Promise<boolean> {
	try {
		await RelationshipCommands.removeRelationship(userId);
		return true;
	} catch (error) {
		showUnblockUserErrorModal(error);
		return false;
	}
}

export function showUnblockUserConfirmation(
	i18n: I18n,
	user: User,
	options: RelationshipConfirmationOptions = {},
): void {
	if (options.bypassConfirm) {
		void unblockUser(i18n, user.id);
		return;
	}
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(UNBLOCK_USER_DESCRIPTOR)}
				description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_UNBLOCK_DESCRIPTOR, {
					userName: NicknameUtils.getNickname(user, null),
				})}
				primaryText={i18n._(UNBLOCK_USER_ACTION_DESCRIPTOR)}
				primaryVariant="primary"
				onPrimary={async () => {
					await unblockUser(i18n, user.id);
				}}
				showShiftBypassConfirmationTip={options.showShiftBypassConfirmationTip ?? false}
				data-flx="relationship.relationship-action-utils.show-unblock-user-confirmation.confirm-modal"
			/>
		)),
	);
}

export async function ignoreFriendRequest(_i18n: I18n, userId: string): Promise<boolean> {
	try {
		await RelationshipCommands.removeRelationship(userId);
		return true;
	} catch (error) {
		showIgnoreFriendRequestErrorModal(error);
		return false;
	}
}
