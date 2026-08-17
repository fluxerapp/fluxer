// SPDX-License-Identifier: AGPL-3.0-or-later

import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import {getAccountAvatarUrl, getAccountDisplayName} from '@app/features/auth/components/accounts/AccountListItem';
import styles from '@app/features/auth/components/accounts/AccountRow.module.css';
import type {Account} from '@app/features/platform/state/AuthSession';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {MockAvatar} from '@app/features/ui/components/MockAvatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CaretRightIcon, CheckIcon, DotsThreeIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useRef} from 'react';

const EXPIRED_DESCRIPTOR = msg({
	message: 'Expired',
	comment: 'Short label in the authentication account row. Keep the tone plain and specific.',
});
const MORE_DESCRIPTOR = msg({
	message: 'More',
	comment: 'Short label in the authentication account row. Keep the tone plain and specific.',
});
type AccountRowVariant = 'default' | 'manage' | 'compact';

interface AccountRowProps {
	account: Account;
	variant?: AccountRowVariant;
	isCurrent?: boolean;
	isExpired?: boolean;
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
		onMenuClick,
		onClick,
		showCaretIndicator = false,
		className,
		meta,
	}: AccountRowProps) => {
		const {i18n} = useLingui();
		const avatarUrl = getAccountAvatarUrl(account);
		const displayName = getAccountDisplayName(account, '???');
		const userData = account.userData;
		const accountTag = userData
			? NicknameUtils.formatTagForStreamerMode(`${userData.username}#${userData.discriminator}`)
			: displayName;
		const menuButtonRef = useRef<HTMLButtonElement | null>(null);
		const isContextMenuOpen = useContextMenuHoverState(menuButtonRef, Boolean(onMenuClick));
		const handleMenuClick = useCallback(
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
		const showMenuButton = Boolean(onMenuClick && variant !== 'compact' && !showCaretIndicator);
		return (
			<div
				className={clsx(styles.row, variantClassName, showMenuButton && styles.withMenu, className)}
				data-flx="auth.accounts.account-row.row"
			>
				<MainButtonComponent
					type={isClickable ? 'button' : undefined}
					className={clsx(styles.mainButton, isClickable && styles.clickable)}
					onClick={isClickable ? onClick : undefined}
					data-flx="auth.accounts.account-row.main-button.click"
				>
					<div className={styles.avatarWrap} data-flx="auth.accounts.account-row.avatar-wrap">
						<MockAvatar
							size={avatarSize}
							avatarUrl={avatarUrl}
							userTag={displayName}
							data-flx="auth.accounts.account-row.mock-avatar"
						/>
					</div>
					<div className={styles.body} data-flx="auth.accounts.account-row.body">
						{variant === 'compact' ? (
							<div className={styles.compactRow} data-flx="auth.accounts.account-row.compact-row">
								<span
									className={clsx('user-text', 'truncate', styles.primaryLine, isCurrent && styles.currentName)}
									data-flx="auth.accounts.account-row.user-text"
								>
									{displayName}
									<span className={styles.discriminator} data-flx="auth.accounts.account-row.discriminator">
										{accountTag}
									</span>
								</span>
							</div>
						) : (
							<>
								<div className={styles.titleRow} data-flx="auth.accounts.account-row.title-row">
									{variant === 'manage' ? (
										<span
											className={clsx('user-text', 'truncate', styles.primaryLine, isCurrent && styles.currentName)}
											data-flx="auth.accounts.account-row.user-text--2"
										>
											{displayName}
											<span className={styles.discriminator} data-flx="auth.accounts.account-row.discriminator--2">
												{accountTag}
											</span>
										</span>
									) : (
										<span
											className={clsx('user-text', styles.displayName, isCurrent && styles.currentName)}
											data-flx="auth.accounts.account-row.user-text--3"
										>
											{displayName}
										</span>
									)}
								</div>
								{variant !== 'manage' ? (
									<span className={clsx('user-text', styles.tag)} data-flx="auth.accounts.account-row.user-text--4">
										{accountTag}
									</span>
								) : null}
								{variant === 'manage' && isCurrent ? (
									<span className={styles.currentFlag} data-flx="auth.accounts.account-row.current-flag">
										<Trans>Active account</Trans>
									</span>
								) : null}
								{meta && (
									<span className={styles.meta} data-flx="auth.accounts.account-row.meta">
										{meta}
									</span>
								)}
								{isExpired && (
									<span className={styles.expired} data-flx="auth.accounts.account-row.expired">
										{i18n._(EXPIRED_DESCRIPTOR)}
									</span>
								)}
							</>
						)}
					</div>
					{isCurrent && variant !== 'manage' ? (
						<div className={styles.checkIndicator} data-flx="auth.accounts.account-row.check-indicator">
							<CheckIcon size={remFromPx(10)} weight="bold" data-flx="auth.accounts.account-row.check-icon" />
						</div>
					) : null}
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
				{showMenuButton ? (
					<FocusRing offset={-2} data-flx="auth.accounts.account-row.focus-ring">
						<button
							ref={menuButtonRef}
							type="button"
							className={clsx(styles.menuButton, isContextMenuOpen && styles.menuButtonActive)}
							onClick={handleMenuClick}
							aria-label={i18n._(MORE_DESCRIPTOR)}
							data-flx="auth.accounts.account-row.menu-button.menu-click"
						>
							<DotsThreeIcon
								size={remFromPx(20)}
								weight="bold"
								className={styles.menuIcon}
								data-flx="auth.accounts.account-row.menu-icon"
							/>
						</button>
					</FocusRing>
				) : null}
			</div>
		);
	},
);
