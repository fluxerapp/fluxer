// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {DragItemType} from '@app/features/app/components/layout/types/DndTypes';
import {AccountSwitcherGrid} from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherGrid';
import {
	MANAGE_HINT_DESCRIPTOR,
	OPEN_SHORTCUT_HINT_DESCRIPTOR,
	SLOT_KEYS_HINT_DESCRIPTOR,
} from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherMessages';
import styles from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherOverlay.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {Scroller} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {useDragAutoScroll} from '@app/features/ui/hooks/useDragAutoScroll';
import {KeybindHint} from '@app/features/ui/keybind_hint/KeybindHint';
import {useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {makeAutoObservable, observableRef} from 'mobx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {type SetStateAction, useCallback} from 'react';
import {useDragLayer} from 'react-dnd';

interface FocusRequest {
	readonly key: string;
	readonly serial: number;
}
interface InteractionInitialState {
	readonly isManaging: boolean;
	readonly activeKey: string;
}
export class AccountSwitcherInteraction {
	isManaging: boolean;
	activeKey: string | null;
	menuKey: string | null = null;
	dragOrder: ReadonlyArray<string> | null = null;
	announcement = '';
	query = '';
	focusRequest: FocusRequest | null = null;
	private focusSerial = 0;
	private menuOwner: string | null = null;
	constructor({isManaging, activeKey}: InteractionInitialState) {
		this.isManaging = isManaging;
		this.activeKey = activeKey;
		makeAutoObservable<this, 'focusSerial' | 'menuOwner'>(
			this,
			{
				dragOrder: observableRef,
				focusRequest: observableRef,
				focusSerial: false,
				menuOwner: false,
			},
			{autoBind: true},
		);
	}
	setIsManaging(value: SetStateAction<boolean>): void {
		this.isManaging = typeof value === 'function' ? value(this.isManaging) : value;
	}
	setActiveKey(value: string | null): void {
		this.activeKey = value;
	}
	setDragOrder(value: SetStateAction<ReadonlyArray<string> | null>): void {
		this.dragOrder = typeof value === 'function' ? value(this.dragOrder) : value;
	}
	setAnnouncement(value: string): void {
		this.announcement = value;
	}
	setQuery(value: string): void {
		this.query = value;
	}
	setFocusRequest(value: SetStateAction<FocusRequest | null>): void {
		this.focusRequest = typeof value === 'function' ? value(this.focusRequest) : value;
	}
	requestFocus(key: string): void {
		this.focusSerial += 1;
		this.setActiveKey(key);
		this.setFocusRequest({key, serial: this.focusSerial});
	}
	openMenu(key: string): void {
		this.menuOwner = key;
		this.menuKey = key;
	}
	closeMenu(key: string): void {
		if (this.menuOwner !== key) {
			return;
		}
		this.menuOwner = null;
		this.menuKey = null;
		this.requestFocus(key);
	}
}
interface AccountsViewProps {
	readonly headingId: string;
	readonly heading: string;
	readonly isManaging: boolean;
	readonly showSlotHint: boolean;
	readonly showSearch: boolean;
	readonly searchRef: React.RefObject<HTMLInputElement | null>;
	readonly searchId: string;
	readonly query: string;
	readonly searchPlaceholder: string | undefined;
	readonly searchLabel: string | undefined;
	readonly setQuery: (query: string) => void;
	readonly emptyMessage: string | null;
	readonly gridProps: React.ComponentProps<typeof AccountSwitcherGrid>;
	readonly gridNode: HTMLUListElement | null;
	readonly announcement: string;
	readonly signOutExpiredLabel: string | null;
	readonly signOutExpiredPending: boolean;
	readonly onSignOutExpired: () => void;
	readonly isBusy: boolean;
	readonly toggleManaging: () => void;
	readonly manageLabel: string;
}
export function AccountSwitcherAccountsView({
	headingId,
	heading,
	isManaging,
	showSlotHint,
	showSearch,
	searchRef,
	searchId,
	query,
	searchPlaceholder,
	searchLabel,
	setQuery,
	emptyMessage,
	gridProps,
	gridNode,
	announcement,
	signOutExpiredLabel,
	signOutExpiredPending,
	onSignOutExpired,
	isBusy,
	toggleManaging,
	manageLabel,
}: AccountsViewProps): React.ReactElement {
	return (
		<div className={styles.stage} data-flx="auth.accounts.account-switcher-modal.stage">
			<div className={styles.header} data-flx="auth.accounts.account-switcher-modal.header">
				<h2 id={headingId} className={styles.heading} data-flx="auth.accounts.account-switcher-modal.heading">
					{heading}
				</h2>
				<SwitcherSubline
					isManaging={isManaging}
					showSlotHint={showSlotHint}
					data-flx="auth.accounts.account-switcher-modal.switcher-subline"
				/>
			</div>
			{showSearch ? (
				<FocusRing within offset={-2} data-flx="auth.accounts.account-switcher-modal.search-focus-ring">
					<div className={styles.search} data-flx="auth.accounts.account-switcher-modal.search">
						<MagnifyingGlassIcon
							className={styles.searchIcon}
							size={remFromPx(16)}
							weight="bold"
							aria-hidden="true"
							data-flx="auth.accounts.account-switcher-modal.search-glass"
						/>
						<input
							ref={searchRef}
							id={searchId}
							type="search"
							className={styles.searchInput}
							value={query}
							placeholder={searchPlaceholder}
							aria-label={searchLabel}
							autoComplete="off"
							spellCheck={false}
							onChange={(event) => setQuery(event.target.value)}
							data-flx="auth.accounts.account-switcher-modal.search-input"
						/>
					</div>
				</FocusRing>
			) : null}
			<Scroller
				className={styles.scroller}
				contentClassName={styles.scrollerContent}
				fade={false}
				data-flx="auth.accounts.account-switcher-modal.scroller"
			>
				{emptyMessage !== null ? (
					<p className={styles.empty} role="status" data-flx="auth.accounts.account-switcher-modal.empty">
						{emptyMessage}
					</p>
				) : null}
				<AccountSwitcherGrid {...gridProps} data-flx="auth.accounts.account-switcher-modal.grid" />
			</Scroller>
			<AccountDragAutoScroll gridNode={gridNode} data-flx="auth.accounts.account-switcher-modal.drag-auto-scroll" />
			<span
				className={styles.srOnly}
				role="status"
				aria-live="polite"
				data-flx="auth.accounts.account-switcher-modal.announcement"
			>
				{announcement}
			</span>
			<div className={styles.actions} data-flx="auth.accounts.account-switcher-modal.actions">
				{signOutExpiredLabel !== null ? (
					<Button
						variant="secondary"
						disabled={isBusy}
						submitting={signOutExpiredPending}
						onClick={onSignOutExpired}
						data-flx="auth.accounts.account-switcher-modal.button.sign-out-expired"
					>
						{signOutExpiredLabel}
					</Button>
				) : null}
				<Button
					variant={isManaging ? 'primary' : 'secondary'}
					disabled={isBusy}
					aria-pressed={isManaging}
					onClick={toggleManaging}
					data-flx="auth.accounts.account-switcher-modal.button.manage"
				>
					{manageLabel}
				</Button>
			</div>
		</div>
	);
}

function AccountDragAutoScroll({gridNode}: {readonly gridNode: HTMLElement | null}): null {
	const isDraggingAccount = useDragLayer(
		(monitor) => monitor.isDragging() && monitor.getItemType() === DragItemType.ACCOUNT_TILE,
	);
	const getScrollElement = useCallback(
		() => gridNode?.closest<HTMLElement>('[data-fluxer-scroll-container="true"]') ?? null,
		[gridNode],
	);
	useDragAutoScroll({active: isDraggingAccount, getScrollElement});
	return null;
}

const SwitcherSubline = observer(function SwitcherSubline({
	isManaging,
	showSlotHint,
}: {
	readonly isManaging: boolean;
	readonly showSlotHint: boolean;
}): React.ReactElement | null {
	const {i18n} = useLingui();
	if (isManaging) {
		return (
			<p className={styles.subline} data-flx="auth.accounts.account-switcher-modal.subline">
				{i18n._(MANAGE_HINT_DESCRIPTOR)}
			</p>
		);
	}
	if (Accessibility.hideKeyboardHints) {
		return null;
	}
	return (
		<p className={clsx(styles.subline, styles.sublineKeys)} data-flx="auth.accounts.account-switcher-modal.subline">
			{showSlotHint ? (
				<span data-flx="auth.accounts.account-switcher-modal.slot-hint">{i18n._(SLOT_KEYS_HINT_DESCRIPTOR)}</span>
			) : null}
			<span className={styles.sublineShortcut} data-flx="auth.accounts.account-switcher-modal.shortcut-hint">
				{i18n._(OPEN_SHORTCUT_HINT_DESCRIPTOR)}
				<KeybindHint
					action="system_open_account_switcher"
					data-flx="auth.accounts.account-switcher-modal.keybind-hint"
				/>
			</span>
		</p>
	);
});
