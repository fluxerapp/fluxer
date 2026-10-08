// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ACCOUNT_DETAILS_UNAVAILABLE_DESCRIPTOR,
	type AccountDisplayLabels,
	getAccountDisplayLabels,
} from '@app/features/auth/AccountDisplayUtils';
import type {Account} from '@app/features/platform/state/AuthSession';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useCallback} from 'react';

interface AccountDisplayIdentityView {
	readonly isAvailable: boolean;
	readonly displayLabel: string;
	readonly tagLabel: string;
	readonly discriminatorLabel: string | null;
}

function createAccountDisplayIdentityView(
	labels: AccountDisplayLabels,
	unavailableLabel: string,
): AccountDisplayIdentityView {
	if (!labels.available) {
		return {isAvailable: false, displayLabel: unavailableLabel, tagLabel: unavailableLabel, discriminatorLabel: null};
	}
	return {
		isAvailable: true,
		displayLabel: labels.displayLabel,
		tagLabel: labels.tagLabel,
		discriminatorLabel: labels.discriminatorLabel,
	};
}

export function useAccountDisplayIdentityView(account: Account): AccountDisplayIdentityView {
	const {i18n} = useLingui();
	return createAccountDisplayIdentityView(
		getAccountDisplayLabels(account),
		i18n._(ACCOUNT_DETAILS_UNAVAILABLE_DESCRIPTOR),
	);
}

export function useAccountDisplayIdentityViewFormatter(): (account: Account) => AccountDisplayIdentityView {
	const {i18n} = useLingui();
	return useCallback(
		(account: Account) =>
			createAccountDisplayIdentityView(
				getAccountDisplayLabels(account),
				i18n._(ACCOUNT_DETAILS_UNAVAILABLE_DESCRIPTOR),
			),
		[i18n],
	);
}

interface AccountIdentityTextProps {
	readonly identityView: AccountDisplayIdentityView;
	readonly discriminatorClassName: string;
}

export function AccountIdentityText({
	identityView,
	discriminatorClassName,
}: AccountIdentityTextProps): React.ReactElement {
	const discriminatorLabel = identityView.discriminatorLabel;
	return (
		<>
			{identityView.displayLabel}
			{discriminatorLabel == null ? null : (
				<span className={discriminatorClassName} data-flx="auth.accounts.account-identity.discriminator">
					{discriminatorLabel}
				</span>
			)}
		</>
	);
}
