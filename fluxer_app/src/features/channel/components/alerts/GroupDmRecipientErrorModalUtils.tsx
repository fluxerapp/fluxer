// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {failureCode, failureMessage} from '@app/features/platform/utils/ResponseInspection';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {ACCOUNT_LIMITED_NOTICE_DESCRIPTOR} from '@app/features/user/utils/AccountLimitUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {msg} from '@lingui/core/macro';

const ADD_TO_GROUP_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't add to group",
	comment: 'Title of the error modal shown when adding a friend to a group DM fails.',
});
const GROUP_IS_FULL_DESCRIPTOR = msg({
	message: 'This group is full. Remove someone before adding more people.',
	comment: 'Body of the error modal shown when a group DM is already at the maximum number of recipients.',
});
const RATE_LIMITED_DESCRIPTOR = msg({
	message: "You're going too fast. Wait a moment and try again.",
	comment: 'Body of the error modal shown when adding a friend to a group DM is rate limited.',
});
const ADD_TO_GROUP_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: "Couldn't add this friend to the group. Please try again.",
	comment: 'Body of the generic fallback error modal shown when adding a friend to a group DM fails.',
});

const INVITE_LINK_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't create invite link",
	comment: 'Title of the error modal shown when generating a group DM invite link fails.',
});
const INVITE_LINK_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: "Couldn't generate an invite link. Please try again.",
	comment: 'Body of the error modal shown when generating a group DM invite link fails.',
});

export function showGroupRecipientAddFailedModal(error: unknown): void {
	const code = failureCode(error);
	ModalCommands.push(
		modal(() => {
			let message: string;
			switch (code) {
				case APIErrorCodes.MAX_GROUP_DM_RECIPIENTS:
					message = i18n._(GROUP_IS_FULL_DESCRIPTOR);
					break;
				case APIErrorCodes.RATE_LIMITED:
					message = i18n._(RATE_LIMITED_DESCRIPTOR);
					break;
				case APIErrorCodes.ACCOUNT_LIMITED:
					message = failureMessage(error) || i18n._(ACCOUNT_LIMITED_NOTICE_DESCRIPTOR);
					break;
				default:
					message = i18n._(ADD_TO_GROUP_FAILED_MESSAGE_DESCRIPTOR);
					break;
			}
			return (
				<GenericErrorModal
					title={i18n._(ADD_TO_GROUP_FAILED_TITLE_DESCRIPTOR)}
					message={message}
					data-flx="channel.group-dm-recipient-error.add-recipient.generic-error-modal"
				/>
			);
		}),
	);
}

export function showGroupInviteCreateFailedModal(): void {
	ModalCommands.push(
		modal(() => (
			<GenericErrorModal
				title={i18n._(INVITE_LINK_FAILED_TITLE_DESCRIPTOR)}
				message={i18n._(INVITE_LINK_FAILED_MESSAGE_DESCRIPTOR)}
				data-flx="channel.group-dm-recipient-error.create-invite.generic-error-modal"
			/>
		)),
	);
}
