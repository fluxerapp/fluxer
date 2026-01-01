/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans, useLingui} from '@lingui/react/macro';
import clsx from 'clsx';
import type {ReactNode} from 'react';
import {MockAvatar} from '~/components/uikit/MockAvatar';
import type {AccountSummary} from '~/stores/AccountManager';
import RuntimeConfigStore, {describeApiEndpoint} from '~/stores/RuntimeConfigStore';
import * as AvatarUtils from '~/utils/AvatarUtils';
import styles from './AccountListItem.module.css';

export interface AccountListItemProps {
	account: AccountSummary;
	disabled?: boolean;
	isCurrent?: boolean;
	onClick?: () => void;
	variant?: 'default' | 'compact';
	showInstance?: boolean;
	badge?: ReactNode;
	meta?: ReactNode;
}

export const getAccountAvatarUrl = (account: AccountSummary): string | undefined => {
	const avatar = account.userData?.avatar ?? null;
	try {
		const mediaEndpoint = account.instance?.mediaEndpoint ?? RuntimeConfigStore.getSnapshot().mediaEndpoint;
		if (mediaEndpoint) {
			return AvatarUtils.getUserAvatarURLWithProxy({id: account.userId, avatar}, mediaEndpoint, false) ?? undefined;
		}
		return AvatarUtils.getUserAvatarURL({id: account.userId, avatar}, false) ?? undefined;
	} catch {
		return undefined;
	}
};

export const formatLastActive = (timestamp: number): string => {
	const formatter = new Intl.DateTimeFormat(undefined, {dateStyle: 'medium', timeStyle: 'short'});
	return formatter.format(new Date(timestamp));
};

export const AccountListItem = ({
	account,
	disabled = false,
	isCurrent = false,
	onClick,
	variant = 'default',
	showInstance = false,
	badge,
	meta,
}: AccountListItemProps) => {
	const {t} = useLingui();
	const displayName = account.userData?.username ?? t`Unknown user`;
	const avatarUrl = getAccountAvatarUrl(account);
	const avatarSize = variant === 'compact' ? 32 : 40;

	const defaultMeta =
		variant === 'compact' ? (
			isCurrent ? (
				(account.userData?.email ?? t`Email unavailable`)
			) : (
				<Trans>Last active {formatLastActive(account.lastActive)}</Trans>
			)
		) : (
			(account.userData?.email ?? t`Email unavailable`)
		);

	return (
		<button
			className={clsx(styles.accountItem, isCurrent && styles.current, variant === 'compact' && styles.compact)}
			onClick={isCurrent && !onClick ? undefined : onClick}
			disabled={disabled || (isCurrent && !onClick)}
			type="button"
		>
			<div className={styles.accountItemContent}>
				<MockAvatar size={avatarSize} avatarUrl={avatarUrl} userTag={account.userData?.username ?? account.userId} />
				<div className={styles.accountInfo}>
					<span className={styles.accountName}>{displayName}</span>
					<span className={styles.accountMeta}>{meta ?? defaultMeta}</span>
					{showInstance && account.instance && (
						<span className={styles.instanceLabel}>{describeApiEndpoint(account.instance.apiEndpoint)}</span>
					)}
				</div>
			</div>
			{badge}
		</button>
	);
};

export const AccountListItemBadge = ({variant, children}: {variant: 'active' | 'expired'; children: ReactNode}) => {
	return <span className={clsx(styles.badge, styles[variant])}>{children}</span>;
};
