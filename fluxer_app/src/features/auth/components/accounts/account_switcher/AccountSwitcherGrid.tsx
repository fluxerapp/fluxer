// SPDX-License-Identifier: AGPL-3.0-or-later

import {useAccountAvatarURL} from '@app/features/auth/AccountAvatarUtils';
import {useAccountDisplayIdentityView} from '@app/features/auth/components/accounts/AccountIdentity';
import {
	AccountTileConnection,
	getAccountTileActivity,
} from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherActivity';
import styles from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherGrid.module.css';
import {
	CONNECTION_OFFLINE_DESCRIPTOR,
	CURRENT_ACCOUNT_DESCRIPTOR,
	HAS_UNREAD_MESSAGES_DESCRIPTOR,
	ON_INSTANCE_DESCRIPTOR,
	RECONNECTING_DESCRIPTOR,
	SLOT_POSITION_DESCRIPTOR,
} from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherMessages';
import type {SwitcherAvatarMediaSize} from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherNavigation';
import {
	type AccountMenuTrigger,
	AccountSwitcherAddTile,
	type AccountSwitcherTileModel,
	AccountSwitcherTileView,
	AccountTileLineTone,
	ADD_ACCOUNT_TILE_KEY,
} from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherTile';
import {useAccountInstanceBadge} from '@app/features/auth/components/accounts/account_switcher/useAccountInstanceBadge';
import {
	type AccountTileReorderHandlers,
	useAccountGridReorder,
	useAccountTileReorder,
} from '@app/features/auth/components/accounts/account_switcher/useAccountTileReorder';
import {
	SAVED_ACCOUNTS_DESCRIPTOR,
	SESSION_EXPIRED_DESCRIPTOR,
} from '@app/features/auth/components/accounts/account_switcher_modal/AccountPickerShared';
import {ADD_AN_ACCOUNT_DESCRIPTOR} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherAuthFlow';
import type {InstanceInfo} from '@app/features/auth/flow/instance_selector/InstanceDirectoryStorage';
import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import {MENTION_COUNT_ARIA_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {Account} from '@app/features/platform/state/AuthSession';
import type {I18n} from '@lingui/core';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

interface SecondLine {
	readonly text: string;
	readonly tone: AccountTileLineTone;
}

interface SecondLineInput {
	readonly i18n: I18n;
	readonly isExpired: boolean;
	readonly connection: AccountTileConnection;
	readonly showInstance: boolean;
	readonly instanceHost: string | null;
	readonly usernameLine: string;
	readonly displayName: string;
}

function resolveSecondLine({
	i18n,
	isExpired,
	connection,
	showInstance,
	instanceHost,
	usernameLine,
	displayName,
}: SecondLineInput): SecondLine {
	if (isExpired) {
		return {text: i18n._(SESSION_EXPIRED_DESCRIPTOR), tone: AccountTileLineTone.DANGER};
	}
	if (connection === AccountTileConnection.RECONNECTING) {
		return {text: i18n._(RECONNECTING_DESCRIPTOR), tone: AccountTileLineTone.WARNING};
	}
	if (connection === AccountTileConnection.OFFLINE) {
		return {text: i18n._(CONNECTION_OFFLINE_DESCRIPTOR), tone: AccountTileLineTone.WARNING};
	}
	if (showInstance && instanceHost != null) {
		return {text: instanceHost, tone: AccountTileLineTone.MUTED};
	}
	return {text: usernameLine === displayName ? '' : usernameLine, tone: AccountTileLineTone.MUTED};
}

interface AriaLabelInput {
	readonly i18n: I18n;
	readonly displayName: string;
	readonly usernameLine: string;
	readonly instanceLabel: string | null;
	readonly isCurrent: boolean;
	readonly statusLine: string | null;
	readonly mentionCount: number;
	readonly hasUnread: boolean;
	readonly slot: number | null;
}

function buildTileAriaLabel({
	i18n,
	displayName,
	usernameLine,
	instanceLabel,
	isCurrent,
	statusLine,
	mentionCount,
	hasUnread,
	slot,
}: AriaLabelInput): string {
	const parts = [displayName];
	if (usernameLine.length > 0 && usernameLine !== displayName) {
		parts.push(usernameLine);
	}
	if (instanceLabel != null) {
		parts.push(i18n._(ON_INSTANCE_DESCRIPTOR, {instanceLabel}));
	}
	if (isCurrent) {
		parts.push(i18n._(CURRENT_ACCOUNT_DESCRIPTOR));
	}
	if (statusLine != null) {
		parts.push(statusLine);
	}
	if (mentionCount > 0) {
		parts.push(i18n._(MENTION_COUNT_ARIA_DESCRIPTOR, {mentionCount}));
	} else if (hasUnread) {
		parts.push(i18n._(HAS_UNREAD_MESSAGES_DESCRIPTOR));
	}
	if (slot != null) {
		parts.push(i18n._(SLOT_POSITION_DESCRIPTOR, {slot: slot + 1}));
	}
	return parts.join(', ');
}

interface AccountSwitcherGridReorder extends AccountTileReorderHandlers {
	readonly order: ReadonlyArray<string>;
	readonly enabled: boolean;
	readonly isReordering: boolean;
	readonly animateLayout: boolean;
}

interface AccountSwitcherGridState {
	readonly activeKey: string | null;
	readonly isManaging: boolean;
	readonly pendingKey: string | null;
	readonly menuKey: string | null;
	readonly isBusy: boolean;
}

interface AccountTileProps extends AccountSwitcherGridState {
	readonly account: Account;
	readonly slot: number | null;
	readonly currentAccountKey: string | null;
	readonly showInstance: boolean;
	readonly knownInstances: ReadonlyArray<InstanceInfo>;
	readonly mediaSize: SwitcherAvatarMediaSize;
	readonly reorder: AccountSwitcherGridReorder;
	readonly onFocusTile: (key: string) => void;
	readonly onActivate: (accountKey: string, event: React.MouseEvent<HTMLButtonElement>) => void;
	readonly onOpenMenu: (accountKey: string, trigger: AccountMenuTrigger) => void;
}

const AccountTile = observer(function AccountTile({
	account,
	slot,
	currentAccountKey,
	showInstance,
	knownInstances,
	mediaSize,
	reorder,
	activeKey,
	isManaging,
	pendingKey,
	menuKey,
	isBusy,
	onFocusTile,
	onActivate,
	onOpenMenu,
}: AccountTileProps): React.ReactElement {
	const {i18n} = useLingui();
	const accountKey = getAccountKey(account);
	const isCurrent = accountKey === currentAccountKey;
	const identity = useAccountDisplayIdentityView(account);
	const avatarUrl = useAccountAvatarURL(account, mediaSize);
	const instanceBadge = useAccountInstanceBadge(account, knownInstances, showInstance);
	const activity = getAccountTileActivity(accountKey, isCurrent);
	const isExpired = account.isValid === false;
	const {isDragging, dragRef, dropRef} = useAccountTileReorder({
		accountKey,
		enabled: reorder.enabled,
		order: reorder.order,
		onMoveTile: reorder.onMoveTile,
		onDropTile: reorder.onDropTile,
		onDragEnd: reorder.onDragEnd,
	});
	const usernameLine =
		identity.discriminatorLabel == null
			? identity.usernameLabel
			: `${identity.usernameLabel}${identity.discriminatorLabel}`;
	const secondLine = resolveSecondLine({
		i18n,
		isExpired,
		connection: activity.connection,
		showInstance,
		instanceHost: instanceBadge?.title ?? null,
		usernameLine,
		displayName: identity.displayLabel,
	});
	const tile: AccountSwitcherTileModel = {
		accountKey,
		displayName: identity.displayLabel,
		secondLine: secondLine.text,
		secondLineTone: secondLine.tone,
		avatarUrl,
		instanceBadge,
		isCurrent,
		isExpired,
		mentionCount: isExpired ? 0 : activity.mentionCount,
		hasUnread: !isExpired && activity.hasUnread,
		ariaLabel: buildTileAriaLabel({
			i18n,
			displayName: identity.displayLabel,
			usernameLine,
			instanceLabel: instanceBadge?.label ?? null,
			isCurrent,
			statusLine: secondLine.tone === AccountTileLineTone.MUTED ? null : secondLine.text,
			mentionCount: isExpired ? 0 : activity.mentionCount,
			hasUnread: !isExpired && activity.hasUnread,
			slot,
		}),
	};
	return (
		<AccountSwitcherTileView
			tile={tile}
			drag={{
				isDragging,
				isReordering: reorder.isReordering,
				animateLayout: reorder.animateLayout,
				dragRef,
				dropRef,
			}}
			isActive={activeKey === accountKey}
			isManaging={isManaging}
			isPending={pendingKey === accountKey}
			isDimmed={pendingKey != null && pendingKey !== accountKey}
			isMenuOpen={menuKey === accountKey}
			disabled={isBusy && pendingKey !== accountKey}
			onFocusTile={onFocusTile}
			onActivate={onActivate}
			onOpenMenu={onOpenMenu}
			data-flx="auth.accounts.account-switcher.grid.tile"
		/>
	);
});

interface AccountSwitcherGridProps extends AccountSwitcherGridState {
	readonly gridRef: (node: HTMLUListElement | null) => void;
	readonly accounts: ReadonlyArray<Account>;
	readonly slotByKey: ReadonlyMap<string, number>;
	readonly currentAccountKey: string | null;
	readonly showInstance: boolean;
	readonly knownInstances: ReadonlyArray<InstanceInfo>;
	readonly mediaSize: SwitcherAvatarMediaSize;
	readonly reorder: AccountSwitcherGridReorder;
	readonly showAddTile: boolean;
	readonly onFocusTile: (key: string) => void;
	readonly onActivate: (accountKey: string, event: React.MouseEvent<HTMLButtonElement>) => void;
	readonly onOpenMenu: (accountKey: string, trigger: AccountMenuTrigger) => void;
	readonly onAdd: () => void;
	readonly onKeyDown: (event: React.KeyboardEvent<HTMLUListElement>) => void;
}

export const AccountSwitcherGrid = observer(function AccountSwitcherGrid({
	gridRef,
	accounts,
	slotByKey,
	currentAccountKey,
	showInstance,
	knownInstances,
	mediaSize,
	reorder,
	showAddTile,
	activeKey,
	isManaging,
	pendingKey,
	menuKey,
	isBusy,
	onFocusTile,
	onActivate,
	onOpenMenu,
	onAdd,
	onKeyDown,
}: AccountSwitcherGridProps): React.ReactElement {
	const {i18n} = useLingui();
	const dropRef = useAccountGridReorder({
		order: reorder.order,
		onMoveTile: reorder.onMoveTile,
		onDropTile: reorder.onDropTile,
	});
	const setGridNode = useCallback(
		(node: HTMLUListElement | null) => {
			gridRef(node);
			dropRef(node);
		},
		[dropRef, gridRef],
	);
	return (
		<ul
			ref={setGridNode}
			className={styles.grid}
			aria-label={i18n._(SAVED_ACCOUNTS_DESCRIPTOR)}
			aria-busy={isBusy}
			onKeyDown={onKeyDown}
			data-flx="auth.accounts.account-switcher.grid"
		>
			{accounts.map((account) => {
				const accountKey = getAccountKey(account);
				return (
					<AccountTile
						key={accountKey}
						account={account}
						slot={slotByKey.get(accountKey) ?? null}
						currentAccountKey={currentAccountKey}
						showInstance={showInstance}
						knownInstances={knownInstances}
						mediaSize={mediaSize}
						reorder={reorder}
						activeKey={activeKey}
						isManaging={isManaging}
						pendingKey={pendingKey}
						menuKey={menuKey}
						isBusy={isBusy}
						onFocusTile={onFocusTile}
						onActivate={onActivate}
						onOpenMenu={onOpenMenu}
						data-flx="auth.accounts.account-switcher.grid.account-tile"
					/>
				);
			})}
			{showAddTile ? (
				<AccountSwitcherAddTile
					label={i18n._(ADD_AN_ACCOUNT_DESCRIPTOR)}
					isActive={activeKey === ADD_ACCOUNT_TILE_KEY}
					isDimmed={pendingKey != null}
					disabled={isBusy}
					onFocusTile={onFocusTile}
					onAdd={onAdd}
					data-flx="auth.accounts.account-switcher.grid.add-tile"
				/>
			) : null}
		</ul>
	);
});
