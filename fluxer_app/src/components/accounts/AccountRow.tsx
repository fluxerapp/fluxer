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
import {CaretRightIcon, CheckIcon, DotsThreeIcon, GlobeIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {MockAvatar} from '~/components/uikit/MockAvatar';
import {Tooltip} from '~/components/uikit/Tooltip';
import type {AccountSummary} from '~/stores/AccountManager';
import {getAccountAvatarUrl} from './AccountListItem';
import styles from './AccountRow.module.css';

const STANDARD_INSTANCES = new Set(['web.fluxer.app', 'web.canary.fluxer.app']);

function getInstanceHost(account: AccountSummary): string | null {
	const endpoint = account.instance?.apiEndpoint;
	if (!endpoint) {
		return null;
	}

	try {
		return new URL(endpoint).hostname;
	} catch (error) {
		console.error('Failed to parse instance host:', error);
		return null;
	}
}

function getInstanceEndpoint(account: AccountSummary): string | null {
	return account.instance?.apiEndpoint ?? null;
}

type AccountRowVariant = 'default' | 'manage' | 'compact';

interface AccountRowProps {
	account: AccountSummary;
	variant?: AccountRowVariant;
	isCurrent?: boolean;
	isExpired?: boolean;
	showInstance?: boolean;
	onMenuClick?: (event: React.MouseEvent<HTMLButtonElement>) => void;
	onClick?: () => void;
	showCaretIndicator?: boolean;
	className?: string;
	meta?: React.ReactNode;
}

export const AccountRow = observer(
	({
		account,
		variant = 'default',
		isCurrent = false,
		isExpired = false,
		showInstance = false,
		onMenuClick,
		onClick,
		showCaretIndicator = false,
		className,
		meta,
	}: AccountRowProps) => {
		const {t} = useLingui();
		const avatarUrl = getAccountAvatarUrl(account);
		const displayName = account.userData?.username ?? t`Unknown user`;
		const discriminator = account.userData?.discriminator ?? '0000';
		const instanceHost = showInstance ? getInstanceHost(account) : null;
		const instanceEndpoint = showInstance ? getInstanceEndpoint(account) : null;
		const shouldShowInstance = typeof instanceHost === 'string' && !STANDARD_INSTANCES.has(instanceHost);

		const handleMenuClick = React.useCallback(
			(event: React.MouseEvent<HTMLButtonElement>) => {
				event.stopPropagation();
				event.preventDefault();
				onMenuClick?.(event);
			},
			[onMenuClick],
		);

		const avatarSize = variant === 'compact' ? 32 : 40;
		const variantClassName = variant === 'manage' ? styles.manage : variant === 'compact' ? styles.compact : undefined;
		const isClickable = typeof onClick === 'function';
		const MainButtonComponent = isClickable ? 'button' : 'div';

		return (
			<div className={clsx(styles.row, variantClassName, className)}>
				<MainButtonComponent
					type={isClickable ? 'button' : undefined}
					className={clsx(styles.mainButton, isClickable && styles.clickable)}
					onClick={isClickable ? onClick : undefined}
				>
					<div className={styles.avatarWrap}>
						<MockAvatar size={avatarSize} avatarUrl={avatarUrl} userTag={displayName} />
					</div>
					<div className={styles.body}>
						{variant === 'compact' ? (
							<div className={styles.compactRow}>
								<span className={clsx('user-text', 'truncate', styles.primaryLine, isCurrent && styles.currentName)}>
									{displayName}
									<span className={styles.discriminator}>#{discriminator}</span>
								</span>
								{shouldShowInstance && instanceEndpoint ? (
									<Tooltip text={instanceEndpoint} position="right">
										<span className={styles.globeButtonCompact}>
											<GlobeIcon size={12} weight="bold" />
										</span>
									</Tooltip>
								) : null}
							</div>
						) : (
							<>
								<div className={styles.titleRow}>
									{variant === 'manage' ? (
										<span
											className={clsx('user-text', 'truncate', styles.primaryLine, isCurrent && styles.currentName)}
										>
											{displayName}
											<span className={styles.discriminator}>#{discriminator}</span>
										</span>
									) : (
										<span className={clsx('user-text', styles.displayName, isCurrent && styles.currentName)}>
											{displayName}
										</span>
									)}
									{shouldShowInstance && instanceEndpoint ? (
										<Tooltip text={instanceEndpoint} position="right">
											<span className={styles.globeButtonCompact}>
												<GlobeIcon size={12} weight="bold" />
											</span>
										</Tooltip>
									) : null}
								</div>
								{variant !== 'manage' ? (
									<span className={clsx('user-text', styles.tag)}>
										{displayName}
										<span className={styles.discriminator}>#{discriminator}</span>
									</span>
								) : null}
								{variant === 'manage' && isCurrent ? (
									<span className={styles.currentFlag}>
										<Trans>Active account</Trans>
									</span>
								) : null}
								{meta && <span className={styles.meta}>{meta}</span>}
								{isExpired && <span className={styles.expired}>{t`Expired`}</span>}
							</>
						)}
					</div>
					{isCurrent && variant !== 'manage' ? (
						<div className={styles.checkIndicator}>
							<CheckIcon size={10} weight="bold" />
						</div>
					) : null}
					{showCaretIndicator ? (
						<div className={styles.caretIndicator}>
							<CaretRightIcon size={18} weight="bold" />
						</div>
					) : null}
					{onMenuClick && variant !== 'compact' && !showCaretIndicator ? (
						<FocusRing offset={-2}>
							<button type="button" className={styles.menuButton} onClick={handleMenuClick} aria-label={t`More`}>
								<DotsThreeIcon size={20} weight="bold" className={styles.menuIcon} />
							</button>
						</FocusRing>
					) : null}
				</MainButtonComponent>
			</div>
		);
	},
);
