// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {promptForAccountTier} from '@app/features/auth/utils/AccountTierPrompt';
import {failureCode, failureMessage} from '@app/features/platform/utils/ResponseInspection';
import {FRIENDS_DISABLED_DESCRIPTOR} from '@app/features/relationship/utils/RelationshipMessageDescriptors';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {msg} from '@lingui/core/macro';

const SEND_FRIEND_REQUEST_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't send friend request",
	comment: 'Title of the error modal shown when sending a friend request fails.',
});
const NOT_ACCEPTING_FRIEND_REQUESTS_MESSAGE_DESCRIPTOR = msg({
	message: "They're not accepting friend requests right now.",
	comment:
		'Body of the friend-request error modal when the target user has friend requests disabled. Keep tone plain and not blaming.',
});
const UNBLOCK_FIRST_MESSAGE_DESCRIPTOR = msg({
	message: 'Unblock them first to send a friend request.',
	comment: 'Body of the friend-request error modal when the current user has the target blocked.',
});
const CANNOT_FRIEND_SELF_MESSAGE_DESCRIPTOR = msg({
	message: "You can't send a friend request to yourself.",
	comment: 'Body of the friend-request error modal when the target is the current user.',
});
const ALREADY_FRIENDS_MESSAGE_DESCRIPTOR = msg({
	message: "You're already friends with this user.",
	comment: 'Body of the friend-request error modal when the users are already friends.',
});
const FINISH_SIGNING_UP_TO_SEND_MESSAGE_DESCRIPTOR = msg({
	message: 'Finish signing up to send friend requests.',
	comment:
		'Body of the friend-request error modal when the current account is an unclaimed (guest) account. "Finish signing up" means complete account claim.',
});
const VERIFY_EMAIL_TO_SEND_MESSAGE_DESCRIPTOR = msg({
	message: 'Confirm your email before sending friend requests.',
	comment: 'Body of the friend-request error modal when the current account email is unverified.',
});
const FRIENDS_LIST_FULL_MESSAGE_DESCRIPTOR = msg({
	message: 'Your friends list is full, or theirs is. Remove someone and try again.',
	comment: 'Body of the friend-request error modal when the relationship limit is reached for either user.',
});
const SEND_FRIEND_REQUEST_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: "Couldn't send the friend request. Try again.",
	comment: 'Body of the generic fallback friend-request error modal.',
});

const ACCEPT_FRIEND_REQUEST_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't accept friend request",
	comment: 'Title of the error modal shown when accepting an incoming friend request fails.',
});
const REQUEST_NO_LONGER_AVAILABLE_MESSAGE_DESCRIPTOR = msg({
	message: 'That friend request is no longer available. Refresh and try again.',
	comment: 'Body of the accept-friend-request error modal when the request no longer exists.',
});
const CLAIM_TO_ACCEPT_MESSAGE_DESCRIPTOR = msg({
	message: 'Finish signing up to accept friend requests.',
	comment: 'Body of the accept-friend-request error modal when the current account is an unclaimed (guest) account.',
});
const ACCEPT_FRIEND_REQUEST_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: "Couldn't accept the friend request. Try again.",
	comment: 'Body of the generic fallback accept-friend-request error modal.',
});

const CANCEL_FRIEND_REQUEST_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't cancel friend request",
	comment: 'Title of the error modal shown when cancelling an outgoing friend request fails.',
});
const CANCEL_FRIEND_REQUEST_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: "Couldn't cancel the friend request. Try again.",
	comment: 'Body of the cancel-friend-request error modal.',
});

const REMOVE_FRIEND_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't remove friend",
	comment: 'Title of the error modal shown when removing a friend fails.',
});
const REMOVE_FRIEND_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: "Couldn't remove this friend. Try again.",
	comment: 'Body of the remove-friend error modal.',
});

const BLOCK_USER_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't block user",
	comment: 'Title of the error modal shown when blocking a user fails.',
});
const CANNOT_BLOCK_SYSTEM_USER_MESSAGE_DESCRIPTOR = msg({
	message: "This account can't be blocked.",
	comment: 'Body of the block-user error modal when the target is a system account that cannot be blocked.',
});
const BLOCK_USER_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: "Couldn't block this user. Try again.",
	comment: 'Body of the generic fallback block-user error modal.',
});

const UNBLOCK_USER_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't unblock user",
	comment: 'Title of the error modal shown when unblocking a user fails.',
});
const UNBLOCK_USER_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: "Couldn't unblock this user. Try again.",
	comment: 'Body of the unblock-user error modal.',
});

const IGNORE_FRIEND_REQUEST_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't ignore friend request",
	comment: 'Title of the error modal shown when ignoring an incoming friend request fails.',
});
const IGNORE_FRIEND_REQUEST_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: "Couldn't ignore the friend request. Try again.",
	comment: 'Body of the ignore-friend-request error modal.',
});

export function showSendFriendRequestErrorModal(error: unknown): void {
	if (promptForAccountTier(error)) {
		return;
	}
	const code = failureCode(error);
	const apiMessage = failureMessage(error);
	showGenericErrorModal({
		getContent: () => {
			const title = i18n._(SEND_FRIEND_REQUEST_FAILED_TITLE_DESCRIPTOR);
			let message: string;
			switch (code) {
				case APIErrorCodes.FRIEND_REQUEST_BLOCKED:
					message = i18n._(NOT_ACCEPTING_FRIEND_REQUESTS_MESSAGE_DESCRIPTOR);
					break;
				case APIErrorCodes.CANNOT_SEND_FRIEND_REQUEST_TO_BLOCKED_USER:
					message = i18n._(UNBLOCK_FIRST_MESSAGE_DESCRIPTOR);
					break;
				case APIErrorCodes.CANNOT_SEND_FRIEND_REQUEST_TO_SELF:
					message = i18n._(CANNOT_FRIEND_SELF_MESSAGE_DESCRIPTOR);
					break;
				case APIErrorCodes.ALREADY_FRIENDS:
					message = i18n._(ALREADY_FRIENDS_MESSAGE_DESCRIPTOR);
					break;
				case APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_SEND_FRIEND_REQUESTS:
					message = i18n._(FINISH_SIGNING_UP_TO_SEND_MESSAGE_DESCRIPTOR);
					break;
				case APIErrorCodes.FRIEND_REQUEST_EMAIL_VERIFICATION_REQUIRED:
					message = i18n._(VERIFY_EMAIL_TO_SEND_MESSAGE_DESCRIPTOR);
					break;
				case APIErrorCodes.MAX_FRIENDS:
					message = i18n._(FRIENDS_LIST_FULL_MESSAGE_DESCRIPTOR);
					break;
				case APIErrorCodes.FRIENDS_DISABLED:
					message = i18n._(FRIENDS_DISABLED_DESCRIPTOR);
					break;
				default:
					message = apiMessage || i18n._(SEND_FRIEND_REQUEST_FAILED_MESSAGE_DESCRIPTOR);
					break;
			}
			return {title, message};
		},
		dataFlx: 'relationship.error-modal.send-friend-request',
	});
}

export function showAcceptFriendRequestErrorModal(error: unknown): void {
	if (promptForAccountTier(error)) {
		return;
	}
	const code = failureCode(error);
	showGenericErrorModal({
		getContent: () => {
			const title = i18n._(ACCEPT_FRIEND_REQUEST_FAILED_TITLE_DESCRIPTOR);
			let message: string;
			switch (code) {
				case APIErrorCodes.UNKNOWN_USER:
				case APIErrorCodes.FRIEND_REQUEST_BLOCKED:
					message = i18n._(REQUEST_NO_LONGER_AVAILABLE_MESSAGE_DESCRIPTOR);
					break;
				case APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_ACCEPT_FRIEND_REQUESTS:
					message = i18n._(CLAIM_TO_ACCEPT_MESSAGE_DESCRIPTOR);
					break;
				case APIErrorCodes.MAX_FRIENDS:
					message = i18n._(FRIENDS_LIST_FULL_MESSAGE_DESCRIPTOR);
					break;
				case APIErrorCodes.FRIENDS_DISABLED:
					message = i18n._(FRIENDS_DISABLED_DESCRIPTOR);
					break;
				default:
					message = i18n._(ACCEPT_FRIEND_REQUEST_FAILED_MESSAGE_DESCRIPTOR);
					break;
			}
			return {title, message};
		},
		dataFlx: 'relationship.error-modal.accept-friend-request',
	});
}

export function showCancelFriendRequestErrorModal(_error: unknown): void {
	showGenericErrorModal({
		getContent: () => ({
			title: i18n._(CANCEL_FRIEND_REQUEST_FAILED_TITLE_DESCRIPTOR),
			message: i18n._(CANCEL_FRIEND_REQUEST_FAILED_MESSAGE_DESCRIPTOR),
		}),
		dataFlx: 'relationship.error-modal.cancel-friend-request',
	});
}

export function showRemoveFriendErrorModal(_error: unknown): void {
	showGenericErrorModal({
		getContent: () => ({
			title: i18n._(REMOVE_FRIEND_FAILED_TITLE_DESCRIPTOR),
			message: i18n._(REMOVE_FRIEND_FAILED_MESSAGE_DESCRIPTOR),
		}),
		dataFlx: 'relationship.error-modal.remove-friend',
	});
}

export function showBlockUserErrorModal(error: unknown): void {
	const code = failureCode(error);
	showGenericErrorModal({
		getContent: () => ({
			title: i18n._(BLOCK_USER_FAILED_TITLE_DESCRIPTOR),
			message:
				code === APIErrorCodes.CANNOT_BLOCK_SYSTEM_USER
					? i18n._(CANNOT_BLOCK_SYSTEM_USER_MESSAGE_DESCRIPTOR)
					: i18n._(BLOCK_USER_FAILED_MESSAGE_DESCRIPTOR),
		}),
		dataFlx: 'relationship.error-modal.block-user',
	});
}

export function showUnblockUserErrorModal(_error: unknown): void {
	showGenericErrorModal({
		getContent: () => ({
			title: i18n._(UNBLOCK_USER_FAILED_TITLE_DESCRIPTOR),
			message: i18n._(UNBLOCK_USER_FAILED_MESSAGE_DESCRIPTOR),
		}),
		dataFlx: 'relationship.error-modal.unblock-user',
	});
}

export function showIgnoreFriendRequestErrorModal(_error: unknown): void {
	showGenericErrorModal({
		getContent: () => ({
			title: i18n._(IGNORE_FRIEND_REQUEST_FAILED_TITLE_DESCRIPTOR),
			message: i18n._(IGNORE_FRIEND_REQUEST_FAILED_MESSAGE_DESCRIPTOR),
		}),
		dataFlx: 'relationship.error-modal.ignore-friend-request',
	});
}
