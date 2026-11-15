// SPDX-License-Identifier: AGPL-3.0-or-later

import {AgeBlockedNotice} from '@app/features/auth/components/AgeBlockedNotice';
import {SIGN_IN_TO_EXISTING_ACCOUNT_DESCRIPTOR} from '@app/features/auth/flow/identifier_first/AuthIdentifierFirstStep';
import styles from '@app/features/auth/flow/identifier_first/AuthIdentifierFirstStep.module.css';
import {QuietButton} from '@app/features/auth/flow/QuietButton';
import {isAgeBlocked, takeAgeBlockedAccountRemoved} from '@app/features/auth/state/AccountEntryStorage';
import {useLingui} from '@lingui/react/macro';
import {type ReactNode, useState} from 'react';

export function AgeBlockedRemovalGate({children}: {children: ReactNode}) {
	const {i18n} = useLingui();
	const [accountRemoved, setAccountRemoved] = useState(() => isAgeBlocked() && takeAgeBlockedAccountRemoved());
	if (!accountRemoved) {
		return children;
	}
	return (
		<>
			<AgeBlockedNotice accountDeleted data-flx="auth.age-blocked-removal-gate.notice" />
			<div className={styles.quietActions} data-flx="auth.age-blocked-removal-gate.actions">
				<QuietButton onClick={() => setAccountRemoved(false)} data-flx="auth.age-blocked-removal-gate.sign-in-existing">
					{i18n._(SIGN_IN_TO_EXISTING_ACCOUNT_DESCRIPTOR)}
				</QuietButton>
			</div>
		</>
	);
}
