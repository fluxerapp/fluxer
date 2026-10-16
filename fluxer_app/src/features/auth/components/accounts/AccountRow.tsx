// SPDX-License-Identifier: AGPL-3.0-or-later

import {useAccountAvatarURL} from '@app/features/auth/AccountAvatarUtils';
import {
	AccountIdentityText,
	useAccountDisplayIdentityView,
} from '@app/features/auth/components/accounts/AccountIdentity';
import styles from '@app/features/auth/components/accounts/AccountRow.module.css';
import type {Account} from '@app/features/platform/state/AuthSession';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {MockAvatar} from '@app/features/ui/components/MockAvatar';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CaretRightIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';

const EXPIRED_DESCRIPTOR = msg({
	message: 'Expired',
	comment: 'Short label in the authentication account row. Keep the tone plain and specific.',
});

interface AccountRowProps {
	account: Account;
	isCurrent?: boolean;
	isExpired?: boolean;
	onClick?: () => void;
	disabled?: boolean;
	showCaretIndicator?: boolean;
}

export const AccountRow = observer(
	({
		account,
		isCurrent = false,
		isExpired = false,
		onClick,
		disabled = false,
		showCaretIndicator = false,
	}: AccountRowProps) => {
		const {i18n} = useLingui();
		const avatarUrl = useAccountAvatarURL(account);
		const identityView = useAccountDisplayIdentityView(account);
		const displayName = identityView.displayLabel;
		const isClickable = typeof onClick === 'function';
		const MainButtonComponent = isClickable ? 'button' : 'div';
		return (
			<div className={clsx(styles.row, styles.manage)} data-flx="auth.accounts.account-row.row">
				<MainButtonComponent
					type={isClickable ? 'button' : undefined}
					className={clsx(styles.mainButton, isClickable && !disabled && styles.clickable, disabled && styles.disabled)}
					onClick={isClickable && !disabled ? onClick : undefined}
					disabled={isClickable ? disabled : undefined}
					data-flx="auth.accounts.account-row.main-button.click"
				>
					<div className={styles.avatarWrap} data-flx="auth.accounts.account-row.avatar-wrap">
						<MockAvatar
							size={40}
							avatarUrl={avatarUrl}
							userTag={displayName}
							data-flx="auth.accounts.account-row.mock-avatar"
						/>
					</div>
					<div className={styles.body} data-flx="auth.accounts.account-row.body">
						<div className={styles.titleRow} data-flx="auth.accounts.account-row.title-row">
							<span
								className={clsx('user-text', 'truncate', styles.primaryLine, isCurrent && styles.currentName)}
								data-flx="auth.accounts.account-row.user-text--2"
							>
								<AccountIdentityText
									identityView={identityView}
									discriminatorClassName={styles.discriminator}
									data-flx="auth.accounts.account-row.account-identity-text"
								/>
							</span>
						</div>
						{isCurrent ? (
							<span className={styles.currentFlag} data-flx="auth.accounts.account-row.current-flag">
								<Trans>Active account</Trans>
							</span>
						) : null}
						{isExpired && (
							<span className={styles.expired} data-flx="auth.accounts.account-row.expired">
								{i18n._(EXPIRED_DESCRIPTOR)}
							</span>
						)}
					</div>
					{showCaretIndicator ? (
						<div className={styles.caretIndicator} data-flx="auth.accounts.account-row.caret-indicator">
							<CaretRightIcon
								size={remFromPx(18)}
								weight="bold"
								data-flx="auth.accounts.account-row.caret-right-icon"
							/>
						</div>
					) : null}
				</MainButtonComponent>
			</div>
		);
	},
);
