// SPDX-License-Identifier: AGPL-3.0-or-later

import AccountEngine from '@app/features/account_engine/state/AccountEngine';
import {useAccountAvatarURL} from '@app/features/auth/AccountAvatarUtils';
import {resolveAccountInstanceKey, resolveAccountInstanceLabel} from '@app/features/auth/AccountDisplayUtils';
import {
	AccountIdentityText,
	useAccountDisplayIdentityView,
} from '@app/features/auth/components/accounts/AccountIdentity';
import styles from '@app/features/auth/components/accounts/AccountList.module.css';
import {useSavedAccountCheck} from '@app/features/auth/components/accounts/account_switcher/useSavedAccountCheck';
import {
	ACCOUNT_OPTIONS_DESCRIPTOR,
	CONFIRM_SIGN_OUT_DESCRIPTOR,
	SAVED_ACCOUNTS_DESCRIPTOR,
	SESSION_EXPIRED_DESCRIPTOR,
	SIGN_IN_AGAIN_DESCRIPTOR,
	SIGN_OUT_EXPIRED_ACCOUNTS_DESCRIPTOR,
} from '@app/features/auth/components/accounts/account_switcher_modal/AccountPickerShared';
import {InstanceBrandMark} from '@app/features/auth/components/InstanceBrandMark';
import {resolveInstanceBrandIconUrl} from '@app/features/auth/InstanceBranding';
import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import {accountsSpanSeveralInstances} from '@app/features/auth/state/AccountSwitcherAccounts';
import {
	ACTIVE_ACCOUNT_DESCRIPTOR,
	CANCEL_DESCRIPTOR,
	SIGN_OUT_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {Account} from '@app/features/platform/state/AuthSession';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {DataMenuRenderer} from '@app/features/ui/action_menu/DataMenuRenderer';
import {Button} from '@app/features/ui/button/Button';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {MentionBadge} from '@app/features/ui/components/MentionBadge';
import {MockAvatar} from '@app/features/ui/components/MockAvatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {useContextMenuTrigger} from '@app/features/ui/hooks/useContextMenuTrigger';
import {
	MenuBottomSheet,
	type MenuGroupType,
	type MenuItemType,
} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import LayerManager, {LayerType} from '@app/features/ui/state/LayerManager';
import {getNextTabIndex, getTabNavigationDirection} from '@app/features/ui/tabs/TabKeyboardNavigation';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import {isOfficialInstanceHost, OFFICIAL_INSTANCE_DISPLAY_NAME} from '@fluxer/instance_bootstrap/src/OfficialInstance';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {
	CheckCircleIcon,
	CheckIcon,
	DotsThreeIcon,
	SignInIcon,
	SignOutIcon,
	WarningCircleIcon,
} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useId, useMemo, useRef, useState} from 'react';

export const SIGN_IN_AGAIN_AS_DESCRIPTOR = msg({
	message: 'Sign in again as {accountLabel}',
	comment:
		'Heading above the sign-in form when a saved account signs in again after its session expired. Shown in the account switcher, on the sign-in page and on the app authorization page. accountLabel is the account display name.',
});
const SIGN_OUT_OF_ACCOUNT_DESCRIPTOR = msg({
	message: 'Sign out of {displayName}',
	comment:
		'Accessible label for the button that signs one saved account out of this device. displayName is the account name.',
});

interface AccountInstanceLabel {
	readonly label: string;
	readonly isOfficial: boolean;
	readonly iconUrl: string | null;
}

function getAccountInstanceLabel(account: Account): AccountInstanceLabel | null {
	const instanceKey = resolveAccountInstanceKey(account);
	const host = resolveAccountInstanceLabel(account);
	if (instanceKey == null || host == null) {
		return null;
	}
	if (isOfficialInstanceHost(instanceKey)) {
		return {label: OFFICIAL_INSTANCE_DISPLAY_NAME, isOfficial: true, iconUrl: null};
	}
	return {label: host, isOfficial: false, iconUrl: resolveInstanceBrandIconUrl(account.instance)};
}

function rowButton(list: HTMLElement | null, index: number): HTMLButtonElement | undefined {
	return list?.querySelectorAll<HTMLButtonElement>('[data-account-row-main]')[index];
}

function isMenuShortcut(event: React.KeyboardEvent): boolean {
	return event.key === 'ContextMenu' || (event.shiftKey && event.key === 'F10');
}

function moveFocus(element: HTMLElement | null | undefined): void {
	KeyboardMode.enterKeyboardMode(false);
	element?.focus();
}

function inlineArrowKey(element: HTMLElement, side: 'start' | 'end'): string {
	const isRtl = element.ownerDocument.documentElement.dir === 'rtl';
	return (side === 'end') !== isRtl ? 'ArrowRight' : 'ArrowLeft';
}

interface AccountListRowProps {
	readonly account: Account;
	readonly index: number;
	readonly isActiveRow: boolean;
	readonly activeRowRef: React.Ref<HTMLButtonElement> | undefined;
	readonly isCurrent: boolean;
	readonly isSelected: boolean;
	readonly isSelectable: boolean;
	readonly isConfirming: boolean;
	readonly isSigningOut: boolean;
	readonly disabled: boolean;
	readonly showInstance: boolean;
	readonly canSignOut: boolean;
	readonly onSelect: (account: Account) => void;
	readonly onFocusRow: (index: number) => void;
	readonly onRequestSignOut: (account: Account) => void;
	readonly onCancelSignOut: () => void;
	readonly onConfirmSignOut: (account: Account) => void;
}

const AccountListRow = observer(function AccountListRow({
	account,
	index,
	isActiveRow,
	activeRowRef,
	isCurrent,
	isSelected,
	isSelectable,
	isConfirming,
	isSigningOut,
	disabled,
	showInstance,
	canSignOut,
	onSelect,
	onFocusRow,
	onRequestSignOut,
	onCancelSignOut,
	onConfirmSignOut,
}: AccountListRowProps): React.ReactElement {
	const {i18n} = useLingui();
	const avatarUrl = useAccountAvatarURL(account);
	const identityView = useAccountDisplayIdentityView(account);
	const displayName = identityView.displayLabel;
	const isExpired = account.isValid === false;
	const accountKey = getAccountKey(account);
	const mentionCount = AccountEngine.getMentionCount(accountKey);
	const instance = showInstance ? getAccountInstanceLabel(account) : null;
	const menuButtonRef = useRef<HTMLButtonElement | null>(null);
	const mainButtonRef = useRef<HTMLButtonElement | null>(null);
	const menuReturnFocusRef = useRef<HTMLElement | null>(null);
	const cancelButtonRef = useRef<HTMLButtonElement | null>(null);
	const wasConfirmingRef = useRef(false);
	useEffect(() => {
		if (isConfirming) {
			cancelButtonRef.current?.focus();
		} else if (wasConfirmingRef.current) {
			mainButtonRef.current?.focus();
		}
		wasConfirmingRef.current = isConfirming;
	}, [isConfirming]);
	const signOutLabel = i18n._(SIGN_OUT_OF_ACCOUNT_DESCRIPTOR, {displayName});
	const rowTabIndex = isActiveRow ? 0 : -1;
	const {isOpen: isMenuOpen, withTracking} = useContextMenuTrigger();
	const hasMenu = canSignOut || isExpired;
	const [isSheetOpen, setIsSheetOpen] = useState(false);
	const closeSheet = useCallback(() => setIsSheetOpen(false), []);
	const buildMenuGroups = (onClose: () => void): Array<MenuGroupType> => {
		const items: Array<MenuItemType> = [];
		if (isExpired) {
			items.push({
				icon: <SignInIcon size={20} data-flx="auth.accounts.account-list.row.sign-in-again-icon" />,
				label: i18n._(SIGN_IN_AGAIN_DESCRIPTOR),
				onClick: () => {
					onClose();
					onSelect(account);
				},
			});
		}
		if (canSignOut) {
			items.push({
				icon: <SignOutIcon size={20} data-flx="auth.accounts.account-list.row.sign-out-icon" />,
				label: i18n._(SIGN_OUT_DESCRIPTOR),
				danger: true,
				onClick: () => {
					onClose();
					if (isExpired) {
						onConfirmSignOut(account);
						return;
					}
					onRequestSignOut(account);
				},
			});
		}
		return [{items}];
	};
	const openMenu = (event: React.MouseEvent<HTMLButtonElement>) => {
		const returnFocusTarget = menuReturnFocusRef.current;
		menuReturnFocusRef.current = null;
		if (isMobileExperienceEnabled()) {
			setIsSheetOpen(true);
			return;
		}
		ContextMenuCommands.openFromElementBottomRight(
			event,
			({onClose}) => (
				<DataMenuRenderer groups={buildMenuGroups(onClose)} data-flx="auth.accounts.account-list.row.menu" />
			),
			withTracking(returnFocusTarget == null ? undefined : {returnFocusTarget}),
		);
	};
	const handleMainKeyDown = (event: React.KeyboardEvent<HTMLButtonElement>) => {
		const menuButton = menuButtonRef.current;
		if (menuButton == null || menuButton.disabled) {
			return;
		}
		if (isMenuShortcut(event)) {
			event.preventDefault();
			event.stopPropagation();
			menuReturnFocusRef.current = event.currentTarget;
			menuButton.click();
			return;
		}
		if (event.key === inlineArrowKey(event.currentTarget, 'end') && !event.altKey) {
			event.preventDefault();
			moveFocus(menuButton);
		}
	};
	const handleMenuKeyDown = (event: React.KeyboardEvent<HTMLButtonElement>) => {
		if (isMenuShortcut(event)) {
			event.preventDefault();
			event.stopPropagation();
			event.currentTarget.click();
			return;
		}
		if (event.key === inlineArrowKey(event.currentTarget, 'start') && !event.altKey) {
			event.preventDefault();
			moveFocus(mainButtonRef.current);
		}
	};
	const setMainButtonRef = useCallback(
		(element: HTMLButtonElement | null) => {
			mainButtonRef.current = element;
			if (!isActiveRow || activeRowRef == null) {
				return;
			}
			if (typeof activeRowRef === 'function') {
				activeRowRef(element);
				return;
			}
			(activeRowRef as React.MutableRefObject<HTMLButtonElement | null>).current = element;
		},
		[activeRowRef, isActiveRow],
	);
	const renderStatus = (): React.ReactNode => {
		if (isExpired) {
			return (
				<span className={clsx(styles.status, styles.statusExpired)} data-flx="auth.accounts.account-list.row.status">
					<WarningCircleIcon
						size={remFromPx(14)}
						weight="fill"
						aria-hidden="true"
						className={styles.statusIcon}
						data-flx="auth.accounts.account-list.row.status-expired-icon"
					/>
					{i18n._(SESSION_EXPIRED_DESCRIPTOR)}
				</span>
			);
		}
		if (!isCurrent || isSelectable) {
			return null;
		}
		return (
			<span className={clsx(styles.status, styles.statusActive)} data-flx="auth.accounts.account-list.row.status">
				<CheckCircleIcon
					size={remFromPx(14)}
					weight="fill"
					aria-hidden="true"
					className={styles.statusIcon}
					data-flx="auth.accounts.account-list.row.status-active-icon"
				/>
				{i18n._(ACTIVE_ACCOUNT_DESCRIPTOR)}
			</span>
		);
	};
	return (
		<li
			className={clsx(
				styles.row,
				isSelected && styles.rowSelected,
				isExpired && styles.rowExpired,
				isConfirming && styles.rowConfirming,
			)}
			data-flx="auth.accounts.account-list.row"
		>
			<FocusRing offset={-2} data-flx="auth.accounts.account-list.row.focus-ring">
				<button
					ref={setMainButtonRef}
					type="button"
					className={styles.main}
					tabIndex={rowTabIndex}
					disabled={disabled || isConfirming}
					aria-current={isCurrent ? 'true' : undefined}
					aria-pressed={isSelectable && !isExpired ? isSelected : undefined}
					aria-keyshortcuts={hasMenu ? 'Shift+F10' : undefined}
					onClick={() => onSelect(account)}
					onKeyDown={handleMainKeyDown}
					onFocus={() => onFocusRow(index)}
					data-account-row-main=""
					data-flx="auth.accounts.account-list.row.button.select"
				>
					<span className={styles.avatar} data-flx="auth.accounts.account-list.row.avatar">
						<MockAvatar
							size={40}
							avatarUrl={avatarUrl}
							userTag={identityView.tagLabel}
							data-flx="auth.accounts.account-list.row.mock-avatar"
						/>
					</span>
					<span className={styles.identity} data-flx="auth.accounts.account-list.row.identity">
						<span className={styles.nameLine} data-flx="auth.accounts.account-list.row.name-line">
							<span
								className={clsx('user-text', styles.name)}
								dir="auto"
								data-flx="auth.accounts.account-list.row.name"
							>
								{displayName}
							</span>
							{mentionCount > 0 ? (
								<MentionBadge
									mentionCount={mentionCount}
									size="small"
									data-flx="auth.accounts.account-list.row.mention-badge"
								/>
							) : null}
						</span>
						{identityView.isAvailable ? (
							<span
								className={clsx('user-text', styles.username)}
								dir="auto"
								data-flx="auth.accounts.account-list.row.username"
							>
								<AccountIdentityText
									identityView={identityView}
									discriminatorClassName={styles.discriminator}
									data-flx="auth.accounts.account-list.row.account-identity-text"
								/>
							</span>
						) : null}
						{instance == null ? null : (
							<span className={styles.instance} data-flx="auth.accounts.account-list.row.instance">
								<span className={styles.instanceMark} data-flx="auth.accounts.account-list.row.instance-mark">
									<InstanceBrandMark
										isOfficial={instance.isOfficial}
										iconUrl={instance.iconUrl}
										size={12}
										data-flx="auth.accounts.account-list.row.instance-brand-mark"
									/>
								</span>
								<span
									className={styles.instanceLabel}
									dir="auto"
									data-flx="auth.accounts.account-list.row.instance-label"
								>
									{instance.label}
								</span>
							</span>
						)}
						{renderStatus()}
					</span>
					{isSelectable ? (
						<span
							className={clsx(styles.selection, isSelected && styles.selectionChecked)}
							aria-hidden="true"
							data-flx="auth.accounts.account-list.row.selection"
						>
							{isSelected ? (
								<CheckIcon
									size={remFromPx(12)}
									weight="bold"
									data-flx="auth.accounts.account-list.row.selection-icon"
								/>
							) : null}
						</span>
					) : null}
				</button>
			</FocusRing>
			{hasMenu ? (
				<span className={styles.trailing} data-flx="auth.accounts.account-list.row.trailing">
					<Button
						ref={menuButtonRef}
						variant="ghost"
						small
						square
						className={clsx(styles.menuButton, (isMenuOpen || isSheetOpen) && styles.menuButtonOpen)}
						icon={
							<DotsThreeIcon size={remFromPx(20)} weight="bold" data-flx="auth.accounts.account-list.row.menu-icon" />
						}
						tabIndex={-1}
						disabled={disabled || isConfirming}
						submitting={isSigningOut && !isConfirming}
						aria-label={i18n._(ACCOUNT_OPTIONS_DESCRIPTOR, {displayName})}
						aria-haspopup="menu"
						aria-expanded={isMenuOpen || isSheetOpen}
						onClick={openMenu}
						onKeyDown={handleMenuKeyDown}
						onFocus={() => onFocusRow(index)}
						data-account-row-menu=""
						data-flx="auth.accounts.account-list.row.button.menu"
					/>
				</span>
			) : null}
			{isSheetOpen ? (
				<MenuBottomSheet
					isOpen={true}
					onClose={closeSheet}
					title={displayName}
					groups={buildMenuGroups(closeSheet)}
					data-flx="auth.accounts.account-list.row.menu-sheet"
				/>
			) : null}
			{isConfirming ? (
				<div
					className={styles.confirm}
					role="group"
					aria-label={signOutLabel}
					data-flx="auth.accounts.account-list.row.confirm"
				>
					<span className={styles.confirmText} dir="auto" data-flx="auth.accounts.account-list.row.confirm-text">
						{i18n._(CONFIRM_SIGN_OUT_DESCRIPTOR, {displayName})}
					</span>
					<span className={styles.confirmActions} data-flx="auth.accounts.account-list.row.confirm-actions">
						<Button
							ref={cancelButtonRef}
							variant="secondary"
							small
							disabled={isSigningOut}
							onClick={onCancelSignOut}
							data-flx="auth.accounts.account-list.row.button.cancel-sign-out"
						>
							{i18n._(CANCEL_DESCRIPTOR)}
						</Button>
						<Button
							variant="danger"
							small
							submitting={isSigningOut}
							onClick={() => onConfirmSignOut(account)}
							data-flx="auth.accounts.account-list.row.button.confirm-sign-out"
						>
							{i18n._(SIGN_OUT_DESCRIPTOR)}
						</Button>
					</span>
				</div>
			) : null}
		</li>
	);
});

export interface AccountListProps {
	readonly accounts: ReadonlyArray<Account>;
	readonly currentAccountKey: string | null;
	readonly selectedAccountKey?: string | null;
	readonly initialActiveAccountKey?: string | null;
	readonly activeRowRef?: React.Ref<HTMLButtonElement>;
	readonly emptyFocusRef?: React.RefObject<HTMLElement | null>;
	readonly error?: string | null;
	readonly disabled: boolean;
	readonly onSelect: (account: Account) => void;
	readonly onSignOut: ((account: Account) => Promise<void>) | null;
	readonly canSignOut?: (account: Account, isCurrent: boolean) => boolean;
}

interface FocusAfterSignOut {
	readonly accountKey: string | null;
	readonly index: number;
}

function allowEverySignOut(): boolean {
	return true;
}

export const AccountList = observer(function AccountList({
	accounts,
	currentAccountKey,
	selectedAccountKey,
	initialActiveAccountKey = null,
	activeRowRef,
	emptyFocusRef,
	error = null,
	disabled,
	onSelect,
	onSignOut,
	canSignOut = allowEverySignOut,
}: AccountListProps): React.ReactElement {
	const {i18n} = useLingui();
	const listRef = useRef<HTMLUListElement | null>(null);
	const [confirmingKey, setConfirmingKey] = useState<string | null>(null);
	const [signingOutKeys, setSigningOutKeys] = useState<ReadonlySet<string>>(() => new Set());
	const [focusAfterSignOut, setFocusAfterSignOut] = useState<FocusAfterSignOut | null>(null);
	const isSelectable = selectedAccountKey !== undefined;
	const effectiveSelectedKey = isSelectable ? selectedAccountKey : currentAccountKey;
	const [activeIndex, setActiveIndex] = useState(() => {
		const initialKey = initialActiveAccountKey ?? effectiveSelectedKey;
		return Math.max(
			0,
			accounts.findIndex((account) => getAccountKey(account) === initialKey),
		);
	});
	const clampedActiveIndex = Math.min(activeIndex, Math.max(0, accounts.length - 1));
	const showInstance = useMemo(() => accountsSpanSeveralInstances(accounts), [accounts]);
	useSavedAccountCheck(accounts, currentAccountKey);
	const confirmLayerKey = useId();
	useEffect(() => {
		if (confirmingKey == null) {
			return;
		}
		LayerManager.addLayer(LayerType.MODAL, confirmLayerKey, () => setConfirmingKey(null));
		return () => LayerManager.removeLayer(LayerType.MODAL, confirmLayerKey);
	}, [confirmLayerKey, confirmingKey]);
	useEffect(() => {
		if (focusAfterSignOut == null) {
			return;
		}
		setFocusAfterSignOut(null);
		const focused = document.activeElement;
		if (focused != null && focused !== document.body) {
			return;
		}
		if (accounts.length === 0) {
			emptyFocusRef?.current?.focus();
			return;
		}
		const keptIndex =
			focusAfterSignOut.accountKey == null
				? -1
				: accounts.findIndex((account) => getAccountKey(account) === focusAfterSignOut.accountKey);
		const index = keptIndex >= 0 ? keptIndex : Math.min(focusAfterSignOut.index, accounts.length - 1);
		setActiveIndex(index);
		rowButton(listRef.current, index)?.focus();
	}, [accounts, emptyFocusRef, focusAfterSignOut]);
	const signOutCandidates = useMemo(
		() =>
			accounts.filter(
				(account) =>
					account.isValid === false && getAccountKey(account) !== currentAccountKey && canSignOut(account, false),
			),
		[accounts, canSignOut, currentAccountKey],
	);
	const runSignOut = useCallback(
		async (targets: ReadonlyArray<Account>, focusTarget: FocusAfterSignOut) => {
			if (onSignOut == null || targets.length === 0) {
				return;
			}
			const keys = targets.map(getAccountKey);
			setSigningOutKeys((existing) => new Set([...existing, ...keys]));
			try {
				for (const account of targets) {
					await onSignOut(account);
				}
			} finally {
				setSigningOutKeys((existing) => new Set([...existing].filter((key) => !keys.includes(key))));
				setConfirmingKey((key) => (key != null && keys.includes(key) ? null : key));
				setFocusAfterSignOut(focusTarget);
			}
		},
		[onSignOut],
	);
	const signOutAccount = (target: Account) => {
		const targetKey = getAccountKey(target);
		const index = accounts.findIndex((account) => getAccountKey(account) === targetKey);
		void runSignOut([target], {accountKey: null, index: Math.max(0, index)});
	};
	const signOutExpiredAccounts = () => {
		const activeAccount = accounts[clampedActiveIndex];
		const activeKey = activeAccount == null ? null : getAccountKey(activeAccount);
		const keepsActive = activeKey != null && !signOutCandidates.some((account) => getAccountKey(account) === activeKey);
		void runSignOut(signOutCandidates, {accountKey: keepsActive ? activeKey : null, index: clampedActiveIndex});
	};
	const handleListKeyDown = (event: React.KeyboardEvent<HTMLUListElement>) => {
		const target = event.target;
		if (
			!(target instanceof HTMLElement) ||
			!(target.hasAttribute('data-account-row-main') || target.hasAttribute('data-account-row-menu'))
		) {
			return;
		}
		if (event.altKey || event.ctrlKey || event.metaKey || event.shiftKey) {
			return;
		}
		const direction = getTabNavigationDirection(event.key, 'vertical');
		const nextIndex = direction == null ? null : getNextTabIndex(clampedActiveIndex, accounts.length, direction);
		if (nextIndex == null) {
			return;
		}
		event.preventDefault();
		setActiveIndex(nextIndex);
		moveFocus(rowButton(listRef.current, nextIndex));
	};
	return (
		<div className={styles.root} data-flx="auth.accounts.account-list.root">
			{error == null ? null : (
				<div className={styles.error} role="alert" data-flx="auth.accounts.account-list.error">
					{error}
				</div>
			)}
			<ul
				ref={listRef}
				className={styles.list}
				aria-label={i18n._(SAVED_ACCOUNTS_DESCRIPTOR)}
				aria-busy={disabled}
				onKeyDown={handleListKeyDown}
				data-flx="auth.accounts.account-list.list"
			>
				{accounts.map((account, index) => {
					const accountKey = getAccountKey(account);
					const isCurrent = accountKey === currentAccountKey;
					return (
						<AccountListRow
							key={accountKey}
							account={account}
							index={index}
							isActiveRow={index === clampedActiveIndex}
							activeRowRef={activeRowRef}
							isCurrent={isCurrent}
							isSelected={accountKey === effectiveSelectedKey}
							isSelectable={isSelectable}
							isConfirming={confirmingKey === accountKey}
							isSigningOut={signingOutKeys.has(accountKey)}
							disabled={disabled}
							showInstance={showInstance}
							canSignOut={onSignOut != null && canSignOut(account, isCurrent)}
							onSelect={onSelect}
							onFocusRow={setActiveIndex}
							onRequestSignOut={(target) => setConfirmingKey(getAccountKey(target))}
							onCancelSignOut={() => setConfirmingKey(null)}
							onConfirmSignOut={signOutAccount}
							data-flx="auth.accounts.account-list.account-list-row"
						/>
					);
				})}
			</ul>
			{onSignOut != null && signOutCandidates.length > 1 ? (
				<Button
					variant="secondary"
					small
					fitContainer
					className={styles.bulkSignOut}
					leftIcon={
						<SignOutIcon size={remFromPx(16)} weight="bold" data-flx="auth.accounts.account-list.bulk-sign-out-icon" />
					}
					disabled={disabled}
					submitting={signOutCandidates.some((account) => signingOutKeys.has(getAccountKey(account)))}
					onClick={signOutExpiredAccounts}
					data-flx="auth.accounts.account-list.button.sign-out-expired-accounts"
				>
					<span className={styles.bulkSignOutLabel} data-flx="auth.accounts.account-list.bulk-sign-out-label">
						{i18n._(SIGN_OUT_EXPIRED_ACCOUNTS_DESCRIPTOR)}
					</span>
				</Button>
			) : null}
		</div>
	);
});
