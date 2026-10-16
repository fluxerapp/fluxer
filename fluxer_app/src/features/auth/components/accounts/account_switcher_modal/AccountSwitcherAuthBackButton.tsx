// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuthFloatingBackButton} from '@app/features/auth/flow/AuthFloatingBackButton';
import {GO_BACK_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';

const BACK_TO_ACCOUNTS_DESCRIPTOR = msg({
	message: 'Back to accounts',
	comment:
		'Accessible label for the account switcher back button when it leaves the sign-in form for the account list.',
});

interface AccountSwitcherAuthBackButtonProps {
	readonly returnsToAccountList: boolean;
	readonly onBack: () => void;
}

export function AccountSwitcherAuthBackButton({
	returnsToAccountList,
	onBack,
}: AccountSwitcherAuthBackButtonProps): React.ReactElement {
	const {i18n} = useLingui();
	let label = i18n._(GO_BACK_DESCRIPTOR);
	if (returnsToAccountList) {
		label = i18n._(BACK_TO_ACCOUNTS_DESCRIPTOR);
	}
	return (
		<AuthFloatingBackButton
			ariaLabel={label}
			onBack={onBack}
			data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-back-button.auth-floating-back-button"
		/>
	);
}
