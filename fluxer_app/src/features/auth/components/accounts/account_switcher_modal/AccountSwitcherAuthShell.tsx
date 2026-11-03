// SPDX-License-Identifier: AGPL-3.0-or-later

import authStyles from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthFlow.module.css';
import {
	ACCOUNT_SWITCHER_AUTH_STEP_ORDER,
	type AccountSwitcherAuthStep,
	getAccountSwitcherAuthStepLabel,
} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthTypes';
import {AuthLayoutContext, type AuthLayoutContextType} from '@app/features/auth/state/AuthLayoutContext';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {ArrowLeftIcon} from '@app/features/ui/icons/DirectionalIcons';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {flxElementClassName} from '@app/lib/react';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';

interface AccountSwitcherAuthShellProps {
	readonly layoutContextValue: AuthLayoutContextType;
	readonly step: AccountSwitcherAuthStep;
	readonly back: {readonly label: string; readonly onBack: () => void} | null;
	readonly rootRef: React.Ref<HTMLElement>;
	readonly children: React.ReactNode;
}

export function AccountSwitcherAuthShell({
	layoutContextValue,
	step,
	back,
	rootRef,
	children,
}: AccountSwitcherAuthShellProps): React.ReactElement {
	const {i18n} = useLingui();
	return (
		<AuthLayoutContext.Provider value={layoutContextValue}>
			<flx-auth-account-switcher-auth-shell
				ref={rootRef}
				className={flxElementClassName(authStyles.shell)}
				data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-shell.root"
			>
				{back == null ? null : (
					<Button
						variant="ghost"
						small
						fitContent
						className={authStyles.backButton}
						leftIcon={
							<ArrowLeftIcon
								size={remFromPx(16)}
								weight="bold"
								data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-shell.arrow-left-icon"
							/>
						}
						onClick={back.onBack}
						data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-shell.button.back"
					>
						{back.label}
					</Button>
				)}
				<SteppedCarousel
					step={step}
					steps={ACCOUNT_SWITCHER_AUTH_STEP_ORDER}
					focusOnStepChange
					ariaLabel={i18n._(getAccountSwitcherAuthStepLabel(step))}
					data-flx="auth.accounts.account-switcher-modal.account-switcher-auth-shell.carousel"
				>
					{children}
				</SteppedCarousel>
			</flx-auth-account-switcher-auth-shell>
		</AuthLayoutContext.Provider>
	);
}
