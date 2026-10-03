// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {failureCode, failureMessage} from '@app/features/platform/utils/ResponseInspection';
import Users from '@app/features/user/state/Users';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {msg} from '@lingui/core/macro';

export const ACCOUNT_LIMITED_NOTICE_DESCRIPTOR = msg({
	message: 'Messaging is paused on your account. Check your email for a quick step to continue.',
	comment:
		'Notice shown in place of the message composer, as a banner at the top of the app, and as an error, when messaging is paused on the current account until the user completes one quick step described in an email. While paused, the account cannot post, react, join communities or change its profile. Keep the tone calm and friendly.',
});
export const ACCOUNT_LIMITED_TITLE_DESCRIPTOR = msg({
	message: 'Messaging is paused',
	comment:
		'Title of the error modal shown when an action fails because messaging is paused on the current account until the user completes one quick step described in an email. Keep the tone calm and friendly.',
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
