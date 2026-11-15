// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import loginStyles from '@app/features/auth/components/pages/LoginPage.module.css';
import styles from '@app/features/auth/flow/identifier_first/AuthIdentifierFirstStep.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';

const AGE_BLOCKED_TITLE_DESCRIPTOR = msg({
	message: "You can't use {productName} yet",
	comment:
		'Heading shown after someone enters a date of birth below the minimum age. Calm and neutral. productName is the app name.',
});
export const AGE_BLOCKED_BODY_DESCRIPTOR = msg({
	message: 'You need to be a little older to have an account.',
	comment:
		'Text shown instead of account creation for a day after someone entered a date of birth below the minimum age.',
});
const AGE_BLOCKED_DELETION_DESCRIPTOR = msg({
	message:
		"You need to be a little older to have an account. We've signed you out, and this account and the date you entered will be deleted.",
	comment:
		'Text under the heading shown right after someone enters a date of birth below the minimum age. Calm and neutral.',
});

export function AgeBlockedNotice({accountDeleted = false}: {accountDeleted?: boolean}) {
	const {i18n} = useLingui();
	return (
		<flx-auth-age-blocked-notice className="flx-element" role="status" data-flx="auth.age-blocked-notice">
			<h1 className={loginStyles.title} dir="auto" data-flx="auth.age-blocked-notice.title">
				{i18n._(AGE_BLOCKED_TITLE_DESCRIPTOR, {productName: RuntimeConfig.productName})}
			</h1>
			<p className={styles.description} dir="auto" data-flx="auth.age-blocked-notice.body">
				{i18n._(accountDeleted ? AGE_BLOCKED_DELETION_DESCRIPTOR : AGE_BLOCKED_BODY_DESCRIPTOR)}
			</p>
		</flx-auth-age-blocked-notice>
	);
}
