// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {getAccountDisplayLabels, resolveAccountInstanceLabel} from '@app/features/auth/AccountDisplayUtils';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {AccountSwitcherAccountMenu} from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherAccountMenu';
import {
	AccountSwitcherAccountsView,
	AccountSwitcherInteraction,
} from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherAccountsView';
import type {AccountSwitcherGrid} from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherGrid';
import {
	ACCOUNT_MOVED_DESCRIPTOR,
	CHOOSE_AN_ACCOUNT_DESCRIPTOR,
	MANAGE_ACCOUNTS_DESCRIPTOR,
	NO_ACCOUNTS_MATCH_DESCRIPTOR,
	SEARCH_ACCOUNTS_DESCRIPTOR,
} from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherMessages';
import {
	ACCOUNT_SWITCHER_SEARCH_THRESHOLD,
	accountMatchesSearch,
	advanceTypeAhead,
	EMPTY_TYPE_AHEAD,
	focusKeyAfterRemoval,
	gridTargetIndex,
	initialFocusKey,
	isGridKey,
	isTypeAheadCharacter,
	SwitcherEscapeAction,
	slotIndexFromKeyEvent,
	switcherAvatarMediaSize,
	switcherEscapeAction,
	switcherTileSizePx,
	type TypeAheadState,
	typeAheadIsActive,
	typeAheadMatch,
} from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherNavigation';
import {AccountSwitcherOverlay} from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherOverlay';
import styles from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherOverlay.module.css';
import {
	type AccountMenuTrigger,
	ADD_ACCOUNT_TILE_KEY,
} from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherTile';
import {useSavedAccountCheck} from '@app/features/auth/components/accounts/account_switcher/useSavedAccountCheck';
import {
	CONFIRM_SIGN_OUT_DESCRIPTOR,
	SIGN_OUT_EXPIRED_ACCOUNTS_DESCRIPTOR,
} from '@app/features/auth/components/accounts/account_switcher_modal/AccountPickerShared';
import {
	AccountSwitcherAuthFlow,
	type AccountSwitcherAuthFrame,
} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthFlow';
import {
	type AccountSwitcherAuthPurpose,
	AccountSwitcherAuthPurposeType,
} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthTypes';
import {useKnownInstances} from '@app/features/auth/flow/instance_selector/useKnownInstances';
import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import {
	accountsSpanSeveralInstances,
	mostRecentOtherAccount,
	saveSwitcherOrder,
} from '@app/features/auth/state/AccountSwitcherAccounts';
import {moveKey} from '@app/features/auth/state/AccountSwitcherOrder';
import Accounts from '@app/features/auth/state/Accounts';
import {AuthRegisterDraftContext, useAuthRegisterDraft} from '@app/features/auth/state/AuthRegisterDraftContext';
import {useAccountSwitcherLogic} from '@app/features/auth/utils/AccountSwitcherModalUtils';
import {DONE_DESCRIPTOR, SIGN_OUT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {Account} from '@app/features/platform/state/AuthSession';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {Scroller} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {usePrefersReducedMotion} from '@app/features/ui/hooks/usePrefersReducedMotion';
import {ArrowLeftIcon} from '@app/features/ui/icons/DirectionalIcons';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {ModalStackContext} from '@app/features/ui/utils/ModalStackContext';
import Window from '@app/features/window/state/Window';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useContext, useEffect, useId, useMemo, useRef, useState} from 'react';

export type AccountSwitcherInitialView = 'accounts' | 'manage';

export interface AccountSwitcherModalProps {
	readonly closeCurrentAccountOnSelect: boolean;
	readonly initialReloginAccount: Account | null;
	readonly redirectAfterLogin: string | null;
	readonly switchAccount: ((accountKey: string) => Promise<void>) | null;
	readonly initialView?: AccountSwitcherInitialView;
	readonly focusAccountKey?: string | null;
	readonly 'data-flx'?: string;
}

const SIGN_BACK_IN_LATER_DESCRIPTOR = msg({
	message: 'You can sign back in at any time.',
	comment: 'Body of the dialog that asks before signing one saved account out of this device.',
});
const AUTH_FOCUS_SELECTOR = '[data-step-focus="true"], input:not([type="hidden"]):not([disabled])';

function isEditableTarget(target: EventTarget | null): boolean {
	if (!(target instanceof HTMLElement)) {
		return false;
	}
	return target.isContentEditable || target.closest('input, textarea, select') != null;
}

function tileElement(grid: HTMLElement | null, key: string): HTMLButtonElement | null {
	if (grid == null) {
		return null;
	}
	for (const element of grid.querySelectorAll<HTMLButtonElement>('[data-switcher-tile]')) {
		if (element.dataset.switcherTile === key) {
			return element;
		}
	}
	return null;
}

function accountSearchLabels(account: Account): Array<string> {
	const labels: Array<string> = [];
	const display = getAccountDisplayLabels(account);
	if (display.available) {
		labels.push(display.displayLabel, display.usernameLabel);
	}
	const host = resolveAccountInstanceLabel(account);
	if (host != null) {
		labels.push(host);
	}
	return labels;
}

function accountNameLabels(account: Account): Array<string> {
	const display = getAccountDisplayLabels(account);
	return display.available ? [display.displayLabel, display.usernameLabel] : [];
}

function initialPurpose(initialReloginAccount: Account | null): AccountSwitcherAuthPurpose | null {
	if (initialReloginAccount == null) {
		return null;
	}
	return {type: AccountSwitcherAuthPurposeType.RELOGIN, account: initialReloginAccount};
}

function focusKeyAfterAuth(purpose: AccountSwitcherAuthPurpose): string {
	if (purpose.type === AccountSwitcherAuthPurposeType.RELOGIN) {
		return getAccountKey(purpose.account);
	}
	return ADD_ACCOUNT_TILE_KEY;
}

function accountDisplayName(account: Account): string {
	const labels = getAccountDisplayLabels(account);
	return labels.available ? labels.displayLabel : account.userId;
}

const AccountSwitcherModal = observer(
	({
		closeCurrentAccountOnSelect,
		initialReloginAccount,
		redirectAfterLogin,
		switchAccount,
		initialView = 'accounts',
		focusAccountKey = null,
	}: AccountSwitcherModalProps) => {
		const {i18n} = useLingui();
		const authRegisterDraftContextValue = useAuthRegisterDraft();
		const headingId = useId();
		const authTitleId = useId();
		const searchId = useId();
		const [authPurpose, setAuthPurpose] = useState<AccountSwitcherAuthPurpose | null>(() =>
			initialPurpose(initialReloginAccount),
		);
		const initiallyManaging = initialView === 'manage';
		const reducedMotion = usePrefersReducedMotion();
		const [pendingKey, setPendingKey] = useState<string | null>(null);
		const [signingOutKeys, setSigningOutKeys] = useState<ReadonlySet<string>>(() => new Set());
		const [gridNode, setGridNode] = useState<HTMLUListElement | null>(null);
		const [cardNode, setCardNode] = useState<HTMLDivElement | null>(null);
		const searchRef = useRef<HTMLInputElement | null>(null);
		const typeAheadRef = useRef<TypeAheadState>(EMPTY_TYPE_AHEAD);
		const startRelogin = useCallback((account: Account) => {
			setAuthPurpose({type: AccountSwitcherAuthPurposeType.RELOGIN, account});
		}, []);
		const logic = useAccountSwitcherLogic({
			onSessionExpired: startRelogin,
			switchAccount,
		});
		const {accounts, currentAccountKey, isBusy} = logic;
		useSavedAccountCheck(accounts, currentAccountKey);
		const showInstance = accountsSpanSeveralInstances(accounts);
		const knownInstances = useKnownInstances(showInstance);
		const accountKeys = useMemo(() => accounts.map(getAccountKey), [accounts]);
		const slotByKey = useMemo(() => {
			const slots = new Map<string, number>();
			accountKeys.slice(0, 9).forEach((key, index) => slots.set(key, index));
			return slots;
		}, [accountKeys]);
		const showSearch = accounts.length >= ACCOUNT_SWITCHER_SEARCH_THRESHOLD;
		const [interaction] = useState(
			() =>
				new AccountSwitcherInteraction({
					isManaging: initiallyManaging,
					activeKey:
						initialFocusKey({
							accountKeys,
							requestedKey: focusAccountKey,
							mostRecentOtherKey: (() => {
								const recent = mostRecentOtherAccount();
								return recent == null ? null : getAccountKey(recent);
							})(),
							currentKey: currentAccountKey,
						}) ?? ADD_ACCOUNT_TILE_KEY,
				}),
		);
		const {
			isManaging,
			menuKey,
			dragOrder,
			announcement,
			query,
			focusRequest,
			activeKey,
			setIsManaging,
			setActiveKey,
			setDragOrder,
			setAnnouncement,
			setQuery,
			setFocusRequest,
			requestFocus,
		} = interaction;
		const visibleAccounts = useMemo(
			() =>
				showSearch && query.trim().length > 0
					? accounts.filter((account) => accountMatchesSearch(accountSearchLabels(account), query))
					: accounts,
			[accounts, query, showSearch],
		);
		const orderedVisibleAccounts = useMemo(() => {
			if (dragOrder == null) {
				return visibleAccounts;
			}
			const byKey = new Map(visibleAccounts.map((account) => [getAccountKey(account), account]));
			return dragOrder.flatMap((key) => {
				const account = byKey.get(key);
				return account == null ? [] : [account];
			});
		}, [dragOrder, visibleAccounts]);
		const visibleKeys = useMemo(() => orderedVisibleAccounts.map(getAccountKey), [orderedVisibleAccounts]);
		const tileKeys = useMemo(() => [...visibleKeys, ADD_ACCOUNT_TILE_KEY], [visibleKeys]);
		const canReorder =
			!isBusy && pendingKey == null && query.trim().length === 0 && visibleAccounts.length > 1 && authPurpose == null;

		const effectiveActiveKey = activeKey != null && tileKeys.includes(activeKey) ? activeKey : tileKeys[0];
		const mediaSize = switcherAvatarMediaSize(
			switcherTileSizePx(Window.windowSize.width),
			globalThis.window?.devicePixelRatio ?? 1,
		);
		const expiredOthers = useMemo(
			() => accounts.filter((account) => account.isValid === false && getAccountKey(account) !== currentAccountKey),
			[accounts, currentAccountKey],
		);
		const commitOrder = useCallback(
			(orderedKeys: ReadonlyArray<string>, movedKey: string) => {
				const account = accounts.find((candidate) => getAccountKey(candidate) === movedKey);
				if (account != null) {
					setAnnouncement(
						i18n._(ACCOUNT_MOVED_DESCRIPTOR, {
							displayName: accountDisplayName(account),
							position: orderedKeys.indexOf(movedKey) + 1,
							count: orderedKeys.length,
						}),
					);
				}
				void saveSwitcherOrder(orderedKeys);
			},
			[accounts, i18n],
		);
		const moveAccountBy = useCallback(
			(accountKey: string, delta: number) => {
				const from = visibleKeys.indexOf(accountKey);
				const to = from + delta;
				if (!canReorder || from < 0 || to < 0 || to >= visibleKeys.length) {
					return;
				}
				commitOrder(moveKey(visibleKeys, accountKey, to), accountKey);
				requestFocus(accountKey);
			},
			[canReorder, commitOrder, requestFocus, visibleKeys],
		);
		const draggedKeyRef = useRef<string | null>(null);
		const dragOrderRef = useRef<ReadonlyArray<string> | null>(null);
		dragOrderRef.current = dragOrder;
		const handleMoveTile = useCallback(
			(draggedKey: string, index: number) => {
				draggedKeyRef.current = draggedKey;
				setDragOrder((current) => {
					const base = current ?? visibleKeys;
					return base.indexOf(draggedKey) === index ? current : moveKey(base, draggedKey, index);
				});
			},
			[visibleKeys],
		);
		const handleDropTile = useCallback(() => {
			const order = dragOrderRef.current;
			const movedKey = draggedKeyRef.current;
			if (order != null && movedKey != null) {
				commitOrder(order, movedKey);
			}
		}, [commitOrder]);
		const handleDragEnd = useCallback(() => {
			draggedKeyRef.current = null;
			setDragOrder(null);
		}, []);
		const {isTopmost} = useContext(ModalStackContext);
		const effectiveActiveKeyRef = useRef(effectiveActiveKey);
		effectiveActiveKeyRef.current = effectiveActiveKey;
		useEffect(() => {
			if (gridNode == null) {
				return;
			}
			setFocusRequest((request) => request ?? {key: effectiveActiveKeyRef.current, serial: 0});
		}, [gridNode]);
		useEffect(() => {
			if (focusRequest == null || gridNode == null || !isTopmost) {
				return;
			}
			const element = tileElement(gridNode, focusRequest.key) ?? tileElement(gridNode, ADD_ACCOUNT_TILE_KEY);
			if (element == null) {
				return;
			}
			setFocusRequest(null);
			element.scrollIntoView?.({block: 'nearest'});
			element.focus({preventScroll: true});
		}, [focusRequest, gridNode, isTopmost]);
		useEffect(() => {
			if (cardNode == null) {
				return;
			}
			const target =
				cardNode.querySelector<HTMLElement>(AUTH_FOCUS_SELECTOR) ??
				cardNode.querySelector<HTMLElement>('[data-switcher-back]');
			target?.focus();
		}, [cardNode]);
		const [openedAccountKey] = useState(() => Accounts.currentAccountKey);
		const hasSwitchedAccount = useCallback(() => Accounts.currentAccountKey !== openedAccountKey, [openedAccountKey]);
		const closeSwitcher = useCallback(() => {
			ModalCommands.pop();
		}, []);
		const backToAccounts = useCallback(() => {
			AuthenticationCommands.clearMfaTicket();
			if (authPurpose != null) {
				const key = focusKeyAfterAuth(authPurpose);
				requestFocus(key);
			}
			setAuthPurpose(null);
		}, [authPurpose, requestFocus]);
		const startAddAccount = useCallback(() => {
			setAuthPurpose({type: AccountSwitcherAuthPurposeType.ADD, instance: null});
		}, []);
		const toggleManaging = useCallback(() => {
			setIsManaging((value) => !value);
		}, []);
		const selectAccount = useCallback(
			(account: Account) => {
				if (isBusy) {
					return;
				}
				const accountKey = getAccountKey(account);
				if (account.isValid === false) {
					startRelogin(account);
					return;
				}
				if (accountKey === currentAccountKey) {
					if (closeCurrentAccountOnSelect) {
						ModalCommands.pop();
					}
					return;
				}
				setPendingKey(accountKey);
				void logic.handleSwitchAccount(accountKey).finally(() => setPendingKey(null));
			},
			[closeCurrentAccountOnSelect, currentAccountKey, isBusy, logic, startRelogin],
		);
		const signOutAccounts = useCallback(
			async (targets: ReadonlyArray<Account>) => {
				if (targets.length === 0) {
					return;
				}
				const previousKeys = tileKeys;
				const keys = targets.map(getAccountKey);
				setSigningOutKeys((existing) => new Set([...existing, ...keys]));
				try {
					for (const account of targets) {
						if (getAccountKey(account) === currentAccountKey) {
							await logic.handleLogout();
						} else {
							await logic.handleLogoutStoredAccount(account);
						}
					}
				} finally {
					setSigningOutKeys((existing) => new Set([...existing].filter((key) => !keys.includes(key))));
				}
				const remaining = previousKeys.filter((key) => !keys.includes(key));
				const nextFocus = keys.reduce<string | null>(
					(focusKey, key) => focusKey ?? focusKeyAfterRemoval(previousKeys, key, remaining),
					null,
				);
				requestFocus(nextFocus ?? ADD_ACCOUNT_TILE_KEY);
			},
			[currentAccountKey, logic, requestFocus, tileKeys],
		);
		const confirmSignOut = useCallback(
			(account: Account) => {
				const displayName = accountDisplayName(account);
				requestFocus(getAccountKey(account));
				ModalCommands.push(
					ModalCommands.modal(() => (
						<ConfirmModal
							title={i18n._(CONFIRM_SIGN_OUT_DESCRIPTOR, {displayName})}
							description={i18n._(SIGN_BACK_IN_LATER_DESCRIPTOR)}
							primaryText={i18n._(SIGN_OUT_DESCRIPTOR)}
							primaryVariant="danger"
							onPrimary={() => signOutAccounts([account])}
							data-flx="auth.accounts.account-switcher-modal.confirm-sign-out"
						/>
					)),
				);
			},
			[i18n, requestFocus, signOutAccounts],
		);
		const openAccountMenu = useCallback(
			(accountKey: string, trigger: AccountMenuTrigger) => {
				const account = accounts.find((candidate) => getAccountKey(candidate) === accountKey);
				if (account == null || isBusy) {
					return;
				}
				setActiveKey(accountKey);
				const isRtl = gridNode != null && getComputedStyle(gridNode).direction === 'rtl';
				const moveHandler = (onClose: () => void, key: string, delta: number): (() => void) | null => {
					const target = visibleKeys.indexOf(key) + delta;
					if (!canReorder || target < 0 || target >= visibleKeys.length) {
						return null;
					}
					return () => {
						onClose();
						moveAccountBy(key, delta);
					};
				};
				interaction.openMenu(accountKey);
				const renderMenu = ({onClose}: {onClose: () => void}) => (
					<AccountSwitcherAccountMenu
						isExpired={account.isValid === false}
						onMoveLeft={moveHandler(onClose, accountKey, isRtl ? 1 : -1)}
						onMoveRight={moveHandler(onClose, accountKey, isRtl ? -1 : 1)}
						onSignInAgain={() => {
							onClose();
							startRelogin(account);
						}}
						onCopyUserId={() => {
							onClose();
							void TextCopyCommands.copy(i18n, account.userId);
						}}
						onSignOut={() => {
							onClose();
							confirmSignOut(account);
						}}
						data-flx="auth.accounts.account-switcher-modal.account-menu"
					/>
				);
				const onClose = () => interaction.closeMenu(accountKey);
				if ('nativeEvent' in trigger) {
					ContextMenuCommands.openFromEvent(trigger, renderMenu, {onClose});
					return;
				}
				ContextMenuCommands.openForElement(trigger, renderMenu, {config: {onClose, returnFocusTarget: trigger}});
			},
			[
				accounts,
				canReorder,
				confirmSignOut,
				gridNode,
				i18n,
				interaction,
				isBusy,
				moveAccountBy,
				startRelogin,
				visibleKeys,
			],
		);
		const activateTile = useCallback(
			(accountKey: string, event: React.MouseEvent<HTMLButtonElement>) => {
				if (isManaging) {
					openAccountMenu(accountKey, event);
					return;
				}
				const account = accounts.find((candidate) => getAccountKey(candidate) === accountKey);
				if (account == null) {
					return;
				}
				setActiveKey(accountKey);
				selectAccount(account);
			},
			[accounts, isManaging, openAccountMenu, selectAccount],
		);
		const handleEscape = useCallback(() => {
			const action = switcherEscapeAction({
				hasSearchText: query.length > 0,
				manage: isManaging,
				auth: authPurpose != null,
			});
			switch (action) {
				case SwitcherEscapeAction.LEAVE_AUTH:
					backToAccounts();
					return;
				case SwitcherEscapeAction.CLEAR_SEARCH:
					setQuery('');
					return;
				case SwitcherEscapeAction.LEAVE_MANAGE:
					setIsManaging(false);
					if (effectiveActiveKey != null) {
						requestFocus(effectiveActiveKey);
					}
					return;
				case SwitcherEscapeAction.CLOSE:
					closeSwitcher();
					return;
			}
		}, [authPurpose, backToAccounts, closeSwitcher, effectiveActiveKey, isManaging, query, requestFocus]);
		const handleEmptyAreaClick = useCallback(() => {
			if (authPurpose != null) {
				backToAccounts();
				return;
			}
			closeSwitcher();
		}, [authPurpose, backToAccounts, closeSwitcher]);
		const moveInGrid = useCallback(
			(key: string) => {
				if (gridNode == null || !isGridKey(key)) {
					return false;
				}
				const tiles = [...gridNode.querySelectorAll<HTMLElement>('[data-switcher-tile]')];
				const index = gridTargetIndex({
					rects: tiles.map((tile) => tile.getBoundingClientRect()),
					index: tileKeys.indexOf(effectiveActiveKey),
					key,
					rtl: getComputedStyle(gridNode).direction === 'rtl',
				});
				const target = tileKeys[index];
				if (target == null) {
					return false;
				}
				KeyboardMode.enterKeyboardMode(false);
				requestFocus(target);
				return true;
			},
			[effectiveActiveKey, gridNode, requestFocus, tileKeys],
		);
		const handleGridKeyDown = useCallback(
			(event: React.KeyboardEvent<HTMLUListElement>) => {
				if (!(event.target instanceof HTMLElement) || event.target.dataset.switcherTile == null) {
					return;
				}
				const tileKey = event.target.dataset.switcherTile;
				if (
					event.altKey &&
					!event.ctrlKey &&
					!event.metaKey &&
					!event.shiftKey &&
					(event.key === 'ArrowLeft' || event.key === 'ArrowRight') &&
					tileKey !== ADD_ACCOUNT_TILE_KEY
				) {
					event.preventDefault();
					event.stopPropagation();
					const rtl = gridNode != null && getComputedStyle(gridNode).direction === 'rtl';
					moveAccountBy(tileKey, (event.key === 'ArrowRight') !== rtl ? 1 : -1);
					return;
				}
				if (event.altKey || event.ctrlKey || event.metaKey || event.shiftKey) {
					return;
				}
				if (moveInGrid(event.key)) {
					event.preventDefault();
					event.stopPropagation();
				}
			},
			[gridNode, moveAccountBy, moveInGrid],
		);
		const handleDialogKeyDown = useCallback(
			(event: React.KeyboardEvent<HTMLDivElement>) => {
				if (authPurpose != null || event.key === 'Escape') {
					return;
				}
				if (isEditableTarget(event.target)) {
					if (event.target === searchRef.current && event.key === 'ArrowDown') {
						event.preventDefault();
						KeyboardMode.enterKeyboardMode(false);
						requestFocus(tileKeys[0]);
					}
					return;
				}
				const slot = slotIndexFromKeyEvent(event);
				if (slot != null) {
					const account = accounts[slot];
					if (account == null) {
						return;
					}
					event.preventDefault();
					event.stopPropagation();
					if (isManaging) {
						requestFocus(getAccountKey(account));
						return;
					}
					selectAccount(account);
					return;
				}
				const plain = !event.altKey && !event.ctrlKey && !event.metaKey && !event.shiftKey;
				const now = performance.now();
				if (plain && !typeAheadIsActive(typeAheadRef.current, now)) {
					if (event.code === 'KeyM') {
						event.preventDefault();
						event.stopPropagation();
						toggleManaging();
						return;
					}
					if (event.code === 'KeyN') {
						event.preventDefault();
						event.stopPropagation();
						if (!isBusy) {
							startAddAccount();
						}
						return;
					}
				}
				if (
					plain &&
					isManaging &&
					(event.key === 'Delete' || event.key === 'Backspace') &&
					effectiveActiveKey !== ADD_ACCOUNT_TILE_KEY
				) {
					event.preventDefault();
					event.stopPropagation();
					const account = accounts.find((candidate) => getAccountKey(candidate) === effectiveActiveKey);
					if (account != null && !isBusy) {
						confirmSignOut(account);
					}
					return;
				}
				if (event.key === ' ' || !isTypeAheadCharacter(event)) {
					return;
				}
				typeAheadRef.current = advanceTypeAhead(typeAheadRef.current, event.key, now);
				const index = typeAheadMatch(
					visibleAccounts.map(accountNameLabels),
					typeAheadRef.current.buffer,
					visibleAccounts.findIndex((account) => getAccountKey(account) === effectiveActiveKey),
				);
				event.preventDefault();
				event.stopPropagation();
				if (index != null) {
					KeyboardMode.enterKeyboardMode(false);
					requestFocus(getAccountKey(visibleAccounts[index]));
				}
			},
			[
				accounts,
				authPurpose,
				confirmSignOut,
				effectiveActiveKey,
				isBusy,
				isManaging,
				requestFocus,
				selectAccount,
				startAddAccount,
				tileKeys,
				toggleManaging,
				visibleAccounts,
			],
		);
		const handleAuthLoginComplete = useCallback(() => {
			ModalCommands.popAll();
		}, []);
		const renderAuthFrame = (frame: AccountSwitcherAuthFrame): React.ReactElement => (
			<div
				ref={setCardNode}
				className={styles.card}
				data-switcher-card=""
				data-flx="auth.accounts.account-switcher-modal.auth-card"
			>
				<div className={styles.cardHeader} data-flx="auth.accounts.account-switcher-modal.auth-card.header">
					<Tooltip
						text={frame.backLabel}
						position="bottom"
						data-flx="auth.accounts.account-switcher-modal.auth-card.back-tooltip"
					>
						<FocusRing offset={-2} data-flx="auth.accounts.account-switcher-modal.auth-card.back-focus-ring">
							<button
								type="button"
								className={styles.backButton}
								aria-label={frame.backLabel}
								onClick={frame.onBack}
								data-switcher-back=""
								data-flx="auth.accounts.account-switcher-modal.auth-card.button.back"
							>
								<ArrowLeftIcon
									size={20}
									weight="bold"
									data-flx="auth.accounts.account-switcher-modal.auth-card.back-icon"
								/>
							</button>
						</FocusRing>
					</Tooltip>
					<h2
						id={authTitleId}
						className={styles.cardTitle}
						data-flx="auth.accounts.account-switcher-modal.auth-card.title"
					>
						{frame.title}
					</h2>
				</div>
				<Scroller
					className={styles.cardBody}
					contentClassName={styles.cardBodyContent}
					fade={false}
					data-flx="auth.accounts.account-switcher-modal.auth-card.body"
				>
					{frame.content}
				</Scroller>
			</div>
		);
		const renderAccountsView = () => {
			const heading = i18n._(isManaging ? MANAGE_ACCOUNTS_DESCRIPTOR : CHOOSE_AN_ACCOUNT_DESCRIPTOR);
			const searchPlaceholder = showSearch ? i18n._(SEARCH_ACCOUNTS_DESCRIPTOR) : undefined;
			const searchLabel = showSearch ? i18n._(SEARCH_ACCOUNTS_DESCRIPTOR) : undefined;
			const emptyMessage =
				visibleAccounts.length === 0 && query.trim().length > 0
					? i18n._(NO_ACCOUNTS_MATCH_DESCRIPTOR, {query: query.trim()})
					: null;
			const gridProps: React.ComponentProps<typeof AccountSwitcherGrid> = {
				gridRef: setGridNode,
				accounts: orderedVisibleAccounts,
				slotByKey: slotByKey,
				currentAccountKey: currentAccountKey,
				showInstance: showInstance,
				knownInstances: knownInstances,
				mediaSize: mediaSize,
				reorder: {
					order: visibleKeys,
					enabled: canReorder,
					isReordering: dragOrder != null,
					animateLayout: !reducedMotion,
					onMoveTile: handleMoveTile,
					onDragEnd: handleDragEnd,
					onDropTile: handleDropTile,
				},
				showAddTile: true,
				activeKey: effectiveActiveKey,
				isManaging: isManaging,
				pendingKey: pendingKey,
				menuKey: menuKey,
				isBusy: isBusy,
				onFocusTile: setActiveKey,
				onActivate: activateTile,
				onOpenMenu: openAccountMenu,
				onAdd: startAddAccount,
				onKeyDown: handleGridKeyDown,
			};
			const showSignOutExpired = isManaging && expiredOthers.length > 1;
			const signOutExpiredPending =
				showSignOutExpired && expiredOthers.some((account) => signingOutKeys.has(getAccountKey(account)));
			const signOutExpiredLabel = showSignOutExpired ? i18n._(SIGN_OUT_EXPIRED_ACCOUNTS_DESCRIPTOR) : null;
			const manageLabel = i18n._(isManaging ? DONE_DESCRIPTOR : MANAGE_ACCOUNTS_DESCRIPTOR);
			return (
				<AccountSwitcherAccountsView
					headingId={headingId}
					heading={heading}
					isManaging={isManaging}
					showSlotHint={accounts.length > 1}
					showSearch={showSearch}
					searchRef={searchRef}
					searchId={searchId}
					query={query}
					searchPlaceholder={searchPlaceholder}
					searchLabel={searchLabel}
					setQuery={setQuery}
					emptyMessage={emptyMessage}
					gridProps={gridProps}
					gridNode={gridNode}
					announcement={announcement}
					signOutExpiredLabel={signOutExpiredLabel}
					signOutExpiredPending={signOutExpiredPending}
					onSignOutExpired={() => void signOutAccounts(expiredOthers)}
					isBusy={isBusy}
					toggleManaging={toggleManaging}
					manageLabel={manageLabel}
					data-flx="auth.accounts.account-switcher-modal.accounts-view"
				/>
			);
		};
		return (
			<AuthRegisterDraftContext.Provider value={authRegisterDraftContextValue}>
				<AccountSwitcherOverlay
					viewKey={authPurpose == null ? 'accounts' : 'auth'}
					labelledBy={authPurpose == null ? headingId : authTitleId}
					isBusy={isBusy}
					hasSwitchedAccount={hasSwitchedAccount}
					onEmptyAreaClick={handleEmptyAreaClick}
					onEscape={handleEscape}
					onKeyDown={handleDialogKeyDown}
					data-flx="auth.accounts.account-switcher-modal.overlay"
				>
					{authPurpose == null ? (
						renderAccountsView()
					) : (
						<AccountSwitcherAuthFlow
							purpose={authPurpose}
							renderFrame={renderAuthFrame}
							redirectAfterLogin={redirectAfterLogin}
							focusOnEntry={false}
							exitCancels={false}
							onExit={backToAccounts}
							onLoginComplete={handleAuthLoginComplete}
							data-flx="auth.accounts.account-switcher-modal.auth-flow"
						/>
					)}
				</AccountSwitcherOverlay>
			</AuthRegisterDraftContext.Provider>
		);
	},
);

export default AccountSwitcherModal;
