// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {failureCode, failureMessage} from '@app/features/platform/utils/ResponseInspection';
import Users from '@app/features/user/state/Users';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {msg} from '@lingui/core/macro';

export const ACCOUNT_LIMITED_NOTICE_DESCRIPTOR = msg({
	message: 'Your account is limited. Check your email for how to lift it.',
	comment:
		'Notice shown in place of the message composer, and as an error, when the current account is limited and cannot post, react, join communities or change its profile.',
});
const ACCOUNT_LIMITED_TITLE_DESCRIPTOR = msg({
	message: 'Your account is limited',
	comment: 'Title of the error modal shown when an action fails because the current account is limited.',
});

export function showAccountLimitedModal(serverMessage?: string): void {
	showGenericErrorModal({
		title: () => i18n._(ACCOUNT_LIMITED_TITLE_DESCRIPTOR),
		message: () => serverMessage || i18n._(ACCOUNT_LIMITED_NOTICE_DESCRIPTOR),
		dataFlx: 'user.account-limit-utils.account-limited-modal',
	});
}

export function handleAccountLimitedError(error: unknown): boolean {
	if (failureCode(error) !== APIErrorCodes.ACCOUNT_LIMITED) return false;
	showAccountLimitedModal(failureMessage(error));
	return true;
}

export function blockIfAccountLimited(): boolean {
	if (Users.currentUser?.accountLimited !== true) return false;
	showAccountLimitedModal();
	return true;
}
