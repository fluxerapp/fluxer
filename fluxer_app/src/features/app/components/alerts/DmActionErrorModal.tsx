// SPDX-License-Identifier: AGPL-3.0-or-later

import i18nGlobal from '@app/app/I18n';
import {
	GENERIC_ERROR_BODY_DESCRIPTOR,
	RATE_LIMITED_ERROR_BODY_DESCRIPTOR,
	RATE_LIMITED_ERROR_TITLE_DESCRIPTOR,
} from '@app/features/app/components/alerts/CommonErrorModalDescriptors';
import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {failureCode, failureMessage} from '@app/features/platform/utils/ResponseInspection';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {
	ACCOUNT_LIMITED_NOTICE_DESCRIPTOR,
	ACCOUNT_LIMITED_TITLE_DESCRIPTOR,
} from '@app/features/user/utils/AccountLimitUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {msg} from '@lingui/core/macro';

const CANNOT_MESSAGE_USER_TITLE_DESCRIPTOR = msg({
	message: "You can't message this user",
	comment: 'Title of the error modal shown when a DM cannot be opened (blocked or no shared community).',
});
const CANNOT_MESSAGE_USER_MESSAGE_DESCRIPTOR = msg({
	message: 'They might not be accepting messages from you, or you no longer share a community.',
	comment: 'Body of the error modal shown when a DM cannot be opened (blocked or no shared community).',
});
const USER_NOT_FOUND_TITLE_DESCRIPTOR = msg({
	message: 'User not found',
	comment: 'Title of the error modal shown when the recipient of a DM no longer exists.',
});
const USER_NOT_FOUND_MESSAGE_DESCRIPTOR = msg({
	message: 'This user no longer exists. Refresh and try again.',
	comment: 'Body of the error modal shown when the recipient of a DM no longer exists.',
});
const CLAIM_ACCOUNT_TITLE_DESCRIPTOR = msg({
	message: 'Claim your account',
	comment: 'Title of the error modal shown when an unclaimed account tries to start a DM.',
});
const CLAIM_ACCOUNT_MESSAGE_DESCRIPTOR = msg({
	message: 'You need to claim your account before you can start a direct message.',
	comment: 'Body of the error modal shown when an unclaimed account tries to start a DM.',
});
const VERIFY_EMAIL_TITLE_DESCRIPTOR = msg({
	message: 'Verify your email',
	comment: 'Title of the error modal shown when an unverified account tries to start a DM.',
});
const VERIFY_EMAIL_MESSAGE_DESCRIPTOR = msg({
	message: 'You need to verify your email address before you can start a direct message.',
	comment: 'Body of the error modal shown when an unverified account tries to start a DM.',
});
const TOO_MANY_PEOPLE_TITLE_DESCRIPTOR = msg({
	message: 'This group has too many people',
	comment: 'Title of the error modal shown when a group DM would exceed the maximum number of recipients.',
});
const TOO_MANY_PEOPLE_MESSAGE_DESCRIPTOR = msg({
	message: "You've added more people than a group DM allows. Remove a few and try again.",
	comment: 'Body of the error modal shown when a group DM would exceed the maximum number of recipients.',
});
const GROUP_DM_LIMIT_TITLE_DESCRIPTOR = msg({
	message: "You've reached the group DM limit",
	comment: 'Title of the error modal shown when the user or a recipient has too many open group DMs.',
});
const GROUP_DM_LIMIT_MESSAGE_DESCRIPTOR = msg({
	message: "You or someone you're adding already has the maximum number of group DMs open. Leave one and try again.",
	comment: 'Body of the error modal shown when the user or a recipient has too many open group DMs.',
});
const GENERIC_TITLE_DESCRIPTOR = msg({
	message: "Couldn't start this conversation",
	comment: 'Title of the generic fallback error modal shown when opening or creating a DM fails.',
});

function resolveDmActionErrorContent(code: string | undefined, apiMessage?: string): {title: string; message: string} {
	switch (code) {
		case APIErrorCodes.NEW_CONVERSATIONS_LIMITED:
			return {
				title: i18nGlobal._(GENERIC_TITLE_DESCRIPTOR),
				message: apiMessage || i18nGlobal._(GENERIC_ERROR_BODY_DESCRIPTOR),
			};
		case APIErrorCodes.ACCOUNT_LIMITED:
			return {
				title: i18nGlobal._(ACCOUNT_LIMITED_TITLE_DESCRIPTOR),
				message: apiMessage || i18nGlobal._(ACCOUNT_LIMITED_NOTICE_DESCRIPTOR),
			};
		case APIErrorCodes.CANNOT_SEND_MESSAGES_TO_USER:
			return {
				title: i18nGlobal._(CANNOT_MESSAGE_USER_TITLE_DESCRIPTOR),
				message: i18nGlobal._(CANNOT_MESSAGE_USER_MESSAGE_DESCRIPTOR),
			};
		case APIErrorCodes.UNKNOWN_USER:
			return {
				title: i18nGlobal._(USER_NOT_FOUND_TITLE_DESCRIPTOR),
				message: i18nGlobal._(USER_NOT_FOUND_MESSAGE_DESCRIPTOR),
			};
		case APIErrorCodes.RATE_LIMITED:
			return {
				title: i18nGlobal._(RATE_LIMITED_ERROR_TITLE_DESCRIPTOR),
				message: i18nGlobal._(RATE_LIMITED_ERROR_BODY_DESCRIPTOR),
			};
		case APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_SEND_DIRECT_MESSAGES:
			return {
				title: i18nGlobal._(CLAIM_ACCOUNT_TITLE_DESCRIPTOR),
				message: i18nGlobal._(CLAIM_ACCOUNT_MESSAGE_DESCRIPTOR),
			};
		case APIErrorCodes.DIRECT_MESSAGE_EMAIL_VERIFICATION_REQUIRED:
			return {
				title: i18nGlobal._(VERIFY_EMAIL_TITLE_DESCRIPTOR),
				message: i18nGlobal._(VERIFY_EMAIL_MESSAGE_DESCRIPTOR),
			};
		case APIErrorCodes.MAX_GROUP_DM_RECIPIENTS:
			return {
				title: i18nGlobal._(TOO_MANY_PEOPLE_TITLE_DESCRIPTOR),
				message: i18nGlobal._(TOO_MANY_PEOPLE_MESSAGE_DESCRIPTOR),
			};
		case APIErrorCodes.MAX_GROUP_DMS:
			return {
				title: i18nGlobal._(GROUP_DM_LIMIT_TITLE_DESCRIPTOR),
				message: i18nGlobal._(GROUP_DM_LIMIT_MESSAGE_DESCRIPTOR),
			};
		default:
			return {
				title: i18nGlobal._(GENERIC_TITLE_DESCRIPTOR),
				message: i18nGlobal._(GENERIC_ERROR_BODY_DESCRIPTOR),
			};
	}
}

export function showDmActionErrorModal(error: unknown): void {
	const code = failureCode(error);
	const apiMessage = failureMessage(error);
	ModalCommands.push(
		modal(() => {
			const {title, message} = resolveDmActionErrorContent(code, apiMessage);
			return (
				<GenericErrorModal title={title} message={message} data-flx="app.dm-action-error-modal.generic-error-modal" />
			);
		}),
	);
}
