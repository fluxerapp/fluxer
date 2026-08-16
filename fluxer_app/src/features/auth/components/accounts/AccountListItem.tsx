// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/auth/components/accounts/AccountListItem.module.css';
import type {Account} from '@app/features/platform/state/AuthSession';
import {MockAvatar} from '@app/features/ui/components/MockAvatar';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {formatLastActive} from '@fluxer/date_utils/src/DateFormatting';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import clsx from 'clsx';
import type {ReactNode} from 'react';

const EMAIL_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'Email unavailable',
	comment: "Account switcher fallback when an account's email is unknown.",
});

interface AccountListItemProps {
	account: Account;
	disabled?: boolean;
	isCurrent?: boolean;
	onClick?: () => void;
	variant?: 'default' | 'compact';
	badge?: ReactNode;
	meta?: ReactNode;
}

export const getAccountAvatarUrl = (account: Account): string | undefined => {
	const avatar = account.userData?.avatar ?? null;
	try {
		const mediaEndpoint = RuntimeConfig.mediaEndpoint;
		if (mediaEndpoint) {
			return AvatarUtils.getUserAvatarURLWithProxy({id: account.userId, avatar}, mediaEndpoint, false) ?? undefined;
		}
		return AvatarUtils.getUserAvatarURL({id: account.userId, avatar}, false) ?? undefined;
	} catch {
		return undefined;
	}
};
export const getAccountDisplayName = (account: Account, fallback: string): string => {
	return account.userData ? NicknameUtils.getDisplayName(account.userData) : fallback;
};
export const AccountListItem = ({
	account,
	disabled = false,
	isCurrent = false,
	onClick,
	variant = 'default',
	badge,
	meta,
}: AccountListItemProps) => {
	const {i18n} = useLingui();
	const displayName = getAccountDisplayName(account, '???');
	const avatarUrl = getAccountAvatarUrl(account);
	const avatarSize = variant === 'compact' ? 32 : 40;
	const emailUnavailableLabel = i18n._(EMAIL_UNAVAILABLE_DESCRIPTOR);
	const defaultMeta =
		variant === 'compact' ? (
			isCurrent ? (
				(account.userData?.email ?? emailUnavailableLabel)
			) : (
				<Trans comment="Account switcher metadata showing when a saved account was last used.">
					Last active {{lastActive: formatLastActive(account.lastActive, getCurrentLocale())}}
				</Trans>
			)
		) : (
			(account.userData?.email ?? emailUnavailableLabel)
		);
	return (
		<button
			className={clsx(styles.accountItem, isCurrent && styles.current, variant === 'compact' && styles.compact)}
			onClick={isCurrent && !onClick ? undefined : onClick}
			disabled={disabled || (isCurrent && !onClick)}
			type="button"
			data-flx="auth.accounts.account-list-item.account-item.undefined.button"
		>
			<div className={styles.accountItemContent} data-flx="auth.accounts.account-list-item.account-item-content">
				<MockAvatar
					size={avatarSize}
					avatarUrl={avatarUrl}
					userTag={displayName || account.userId}
					data-flx="auth.accounts.account-list-item.mock-avatar"
				/>
				<div className={styles.accountInfo} data-flx="auth.accounts.account-list-item.account-info">
					<span className={styles.accountName} data-flx="auth.accounts.account-list-item.account-name">
						{displayName}
					</span>
					<span className={styles.accountMeta} data-flx="auth.accounts.account-list-item.account-meta">
						{meta ?? defaultMeta}
					</span>
				</div>
			</div>
			{badge}
		</button>
	);
};
export const AccountListItemBadge = ({variant, children}: {variant: 'active' | 'expired'; children: ReactNode}) => {
	return (
		<span
			className={clsx(styles.badge, styles[variant])}
			data-flx="auth.accounts.account-list-item.account-list-item-badge.badge"
		>
			{children}
		</span>
	);
};
