// SPDX-License-Identifier: AGPL-3.0-or-later

import {SUPPORT_EMAIL_MAILTO} from '@app/features/app/config/I18nDisplayConstants';
import type {User} from '@app/features/user/models/User';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export const TEXT_DID_NOT_GO_THROUGH_DESCRIPTOR = msg({
	message: "My text didn't go through",
	comment:
		'Button shown while messaging is paused on the current account, and the title of the help dialog it opens. The email describing the quick step asks the user to send a text message from their phone. This is for users who could not send that text, and also for users who sent it and nothing happened. The translation should cover both. Also used as the subject of the support email.',
});
const ACCOUNT_ID_LINE_DESCRIPTOR = msg({
	message: 'Account ID: {accountId}',
	comment: 'One line of the support email written for the user. accountId is the numeric account ID.',
});
const USERNAME_LINE_DESCRIPTOR = msg({
	message: 'Username: {username}',
	comment: 'One line of the support email written for the user. username is the account username with its tag.',
});
const ACCOUNT_CREATED_LINE_DESCRIPTOR = msg({
	message: 'Account created: {createdAt}',
	comment:
		'One line of the support email written for the user. createdAt is the account creation time as an ISO 8601 UTC timestamp.',
});
const COUNTRY_LINE_DESCRIPTOR = msg({
	message: 'Country: {country}',
	comment: 'One line of the support email written for the user. country is the country the user typed.',
});

export function buildMessagingPausedHelpEmail(
	i18n: I18n,
	user: Pick<User, 'id' | 'tag' | 'createdAt'>,
	country: string,
): {body: string; mailto: string} {
	const subject = i18n._(TEXT_DID_NOT_GO_THROUGH_DESCRIPTOR);
	const body = [
		subject,
		'',
		i18n._(ACCOUNT_ID_LINE_DESCRIPTOR, {accountId: user.id}),
		i18n._(USERNAME_LINE_DESCRIPTOR, {username: user.tag}),
		i18n._(ACCOUNT_CREATED_LINE_DESCRIPTOR, {createdAt: user.createdAt.toISOString()}),
		i18n._(COUNTRY_LINE_DESCRIPTOR, {country: country.trim()}),
	].join('\n');
	const mailto = `${SUPPORT_EMAIL_MAILTO}?subject=${encodeURIComponent(subject)}&body=${encodeURIComponent(body)}`;
	return {body, mailto};
}
