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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as RelationshipActionCreators from '~/actions/RelationshipActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {APIErrorCodes, RelationshipTypes} from '~/Constants';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import type {UserRecord} from '~/records/UserRecord';
import RelationshipStore from '~/stores/RelationshipStore';
import {getApiErrorCode} from '~/utils/ApiErrorUtils';

export function getSendFriendRequestErrorMessage(i18n: I18n, code: string | undefined): string {
	switch (code) {
		case APIErrorCodes.FRIEND_REQUEST_BLOCKED:
			return i18n._(msg`This user isn't accepting friend requests right now.`);
		case APIErrorCodes.CANNOT_SEND_FRIEND_REQUEST_TO_BLOCKED_USER:
			return i18n._(msg`Unblock this user before sending a friend request.`);
		case APIErrorCodes.BOTS_CANNOT_HAVE_FRIENDS:
			return i18n._(msg`You can't send friend requests to bots.`);
		case APIErrorCodes.CANNOT_SEND_FRIEND_REQUEST_TO_SELF:
			return i18n._(msg`You can't send a friend request to yourself.`);
		case APIErrorCodes.ALREADY_FRIENDS:
			return i18n._(msg`You're already friends with this user.`);
		case APIErrorCodes.UNCLAIMED_ACCOUNT_RESTRICTED:
			return i18n._(msg`You need to claim your account to send friend requests.`);
		default:
			return i18n._(msg`Failed to send friend request. Please try again.`);
	}
}

export function canSendFriendRequest(userId: string, isBot: boolean): boolean {
	if (isBot) {
		return false;
	}
	const relationship = RelationshipStore.getRelationship(userId);
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

export async function sendFriendRequest(i18n: I18n, userId: string): Promise<boolean> {
	try {
		await RelationshipActionCreators.sendFriendRequest(userId);
		ToastActionCreators.success(i18n._(msg`Friend request sent`));
		return true;
	} catch (err) {
		ToastActionCreators.error(getSendFriendRequestErrorMessage(i18n, getApiErrorCode(err)));
		return false;
	}
}

export async function acceptFriendRequest(i18n: I18n, userId: string): Promise<boolean> {
	try {
		await RelationshipActionCreators.acceptFriendRequest(userId);
		return true;
	} catch (_error) {
		ToastActionCreators.error(i18n._(msg`Failed to accept friend request. Please try again.`));
		return false;
	}
}

export async function cancelFriendRequest(i18n: I18n, userId: string): Promise<boolean> {
	try {
		await RelationshipActionCreators.removeRelationship(userId);
		return true;
	} catch (_error) {
		ToastActionCreators.error(i18n._(msg`Failed to cancel friend request. Please try again.`));
		return false;
	}
}

export async function removeFriend(i18n: I18n, userId: string): Promise<boolean> {
	try {
		await RelationshipActionCreators.removeRelationship(userId);
		return true;
	} catch (_error) {
		ToastActionCreators.error(i18n._(msg`Failed to remove friend. Please try again.`));
		return false;
	}
}

export function showRemoveFriendConfirmation(i18n: I18n, user: UserRecord): void {
	ModalActionCreators.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(msg`Remove Friend`)}
				description={i18n._(msg`Are you sure you want to remove ${user.username} as a friend?`)}
				primaryText={i18n._(msg`Remove Friend`)}
				primaryVariant="danger-primary"
				onPrimary={async () => {
					await removeFriend(i18n, user.id);
				}}
			/>
		)),
	);
}

export async function blockUser(i18n: I18n, userId: string): Promise<boolean> {
	try {
		await RelationshipActionCreators.blockUser(userId);
		return true;
	} catch (_error) {
		ToastActionCreators.error(i18n._(msg`Failed to block user. Please try again.`));
		return false;
	}
}

export function showBlockUserConfirmation(i18n: I18n, user: UserRecord): void {
	ModalActionCreators.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(msg`Block User`)}
				description={i18n._(
					msg`Are you sure you want to block ${user.username}? They won't be able to message you or send you friend requests.`,
				)}
				primaryText={i18n._(msg`Block`)}
				primaryVariant="danger-primary"
				onPrimary={async () => {
					await blockUser(i18n, user.id);
				}}
			/>
		)),
	);
}

export async function unblockUser(i18n: I18n, userId: string): Promise<boolean> {
	try {
		await RelationshipActionCreators.removeRelationship(userId);
		return true;
	} catch (_error) {
		ToastActionCreators.error(i18n._(msg`Failed to unblock user. Please try again.`));
		return false;
	}
}

export function showUnblockUserConfirmation(i18n: I18n, user: UserRecord): void {
	ModalActionCreators.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(msg`Unblock User`)}
				description={i18n._(msg`Are you sure you want to unblock ${user.username}?`)}
				primaryText={i18n._(msg`Unblock`)}
				primaryVariant="primary"
				onPrimary={async () => {
					await unblockUser(i18n, user.id);
				}}
			/>
		)),
	);
}

export async function ignoreFriendRequest(i18n: I18n, userId: string): Promise<boolean> {
	try {
		await RelationshipActionCreators.removeRelationship(userId);
		return true;
	} catch (_error) {
		ToastActionCreators.error(i18n._(msg`Failed to ignore friend request. Please try again.`));
		return false;
	}
}
