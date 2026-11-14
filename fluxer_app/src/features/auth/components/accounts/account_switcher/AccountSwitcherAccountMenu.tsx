// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	MOVE_LEFT_DESCRIPTOR,
	MOVE_RIGHT_DESCRIPTOR,
} from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherMessages';
import {SIGN_IN_AGAIN_DESCRIPTOR} from '@app/features/auth/components/accounts/account_switcher_modal/AccountPickerShared';
import {COPY_USER_ID_DESCRIPTOR, SIGN_OUT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {CopyIdIcon, LeaveIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon, ArrowRightIcon, SignInIcon} from '@phosphor-icons/react';
import type React from 'react';

interface AccountSwitcherAccountMenuProps {
	readonly isExpired: boolean;
	readonly onMoveLeft: (() => void) | null;
	readonly onMoveRight: (() => void) | null;
	readonly onSignInAgain: () => void;
	readonly onCopyUserId: () => void;
	readonly onSignOut: () => void;
}

export function AccountSwitcherAccountMenu({
	isExpired,
	onMoveLeft,
	onMoveRight,
	onSignInAgain,
	onCopyUserId,
	onSignOut,
}: AccountSwitcherAccountMenuProps): React.ReactElement {
	const {i18n} = useLingui();
	return (
		<>
			<MenuGroup data-flx="auth.accounts.account-switcher.account-menu.group">
				{isExpired ? (
					<MenuItem
						icon={
							<SignInIcon
								size={remFromPx(16)}
								weight="fill"
								data-flx="auth.accounts.account-switcher.account-menu.sign-in-icon"
							/>
						}
						onClick={onSignInAgain}
						data-flx="auth.accounts.account-switcher.account-menu.menu-item.sign-in-again"
					>
						{i18n._(SIGN_IN_AGAIN_DESCRIPTOR)}
					</MenuItem>
				) : null}
				<MenuItem
					icon={<CopyIdIcon data-flx="auth.accounts.account-switcher.account-menu.copy-id-icon" />}
					onClick={onCopyUserId}
					data-flx="auth.accounts.account-switcher.account-menu.menu-item.copy-user-id"
				>
					{i18n._(COPY_USER_ID_DESCRIPTOR)}
				</MenuItem>
			</MenuGroup>
			<MenuGroup data-flx="auth.accounts.account-switcher.account-menu.move-group">
				{onMoveLeft == null ? null : (
					<MenuItem
						icon={
							<ArrowLeftIcon
								size={remFromPx(16)}
								weight="bold"
								data-flx="auth.accounts.account-switcher.account-menu.move-left-icon"
							/>
						}
						onClick={onMoveLeft}
						data-flx="auth.accounts.account-switcher.account-menu.menu-item.move-left"
					>
						{i18n._(MOVE_LEFT_DESCRIPTOR)}
					</MenuItem>
				)}
				{onMoveRight == null ? null : (
					<MenuItem
						icon={
							<ArrowRightIcon
								size={remFromPx(16)}
								weight="bold"
								data-flx="auth.accounts.account-switcher.account-menu.move-right-icon"
							/>
						}
						onClick={onMoveRight}
						data-flx="auth.accounts.account-switcher.account-menu.menu-item.move-right"
					>
						{i18n._(MOVE_RIGHT_DESCRIPTOR)}
					</MenuItem>
				)}
			</MenuGroup>
			<MenuGroup data-flx="auth.accounts.account-switcher.account-menu.danger-group">
				<MenuItem
					icon={<LeaveIcon data-flx="auth.accounts.account-switcher.account-menu.sign-out-icon" />}
					danger
					onClick={onSignOut}
					data-flx="auth.accounts.account-switcher.account-menu.menu-item.sign-out"
				>
					{i18n._(SIGN_OUT_DESCRIPTOR)}
				</MenuItem>
			</MenuGroup>
		</>
	);
}
