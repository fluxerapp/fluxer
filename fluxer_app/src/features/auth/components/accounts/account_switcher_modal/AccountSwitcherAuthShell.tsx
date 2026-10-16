// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountSwitcherAuthBackButton} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthBackButton';
import authStyles from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthFlow.module.css';
import {
	ACCOUNT_SWITCHER_AUTH_STEP_ORDER,
	type AccountSwitcherAuthStep,
	getAccountSwitcherAuthStepLabel,
} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthTypes';
import {AuthCardContainer} from '@app/features/auth/flow/AuthCardContainer';
import {
	type AuthCardVariant,
	AuthLayoutContext,
	type AuthLayoutContextType,
} from '@app/features/auth/state/AuthLayoutContext';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {flxElementClassName} from '@app/lib/react';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';

interface AccountSwitcherAuthShellProps {
	readonly cardVariant: AuthCardVariant;
	readonly layoutContextValue: AuthLayoutContextType;
	readonly step: AccountSwitcherAuthStep;
	readonly returnsToAccountList: boolean;
	readonly onBack: () => void;
	readonly children: React.ReactNode;
}

export function AccountSwitcherAuthShell({
	cardVariant,
	layoutContextValue,
	step,
	returnsToAccountList,
	onBack,
	children,
}: AccountSwitcherAuthShellProps): React.ReactElement {
	const {i18n} = useLingui();
	return (
		<AuthLayoutContext.Provider value={layoutContextValue}>
			<AuthCardContainer
				variant={cardVariant}
				className={authStyles.authCardContainer}
				cardClassName={authStyles.authCard}
				contentClassName={authStyles.authCardContent}
				data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-shell.auth-card-container"
			>
				<AccountSwitcherAuthBackButton
					returnsToAccountList={returnsToAccountList}
					onBack={onBack}
					data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-shell.back-button"
				/>
				<flx-auth-account-switcher-auth-shell-viewport
					className={flxElementClassName(authStyles.authCarouselViewport)}
					data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-shell.viewport"
				>
					<SteppedCarousel
						step={step}
						steps={ACCOUNT_SWITCHER_AUTH_STEP_ORDER}
						focusOnStepChange
						ariaLabel={i18n._(getAccountSwitcherAuthStepLabel(step))}
						data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-shell.carousel"
					>
						{children}
					</SteppedCarousel>
				</flx-auth-account-switcher-auth-shell-viewport>
			</AuthCardContainer>
		</AuthLayoutContext.Provider>
	);
}
