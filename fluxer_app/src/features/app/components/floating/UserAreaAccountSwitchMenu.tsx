// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/floating/UserAreaAccountSwitchMenu.module.css';
import {useAccountAvatarURL} from '@app/features/auth/AccountAvatarUtils';
import {resolveAccountInstanceKey, resolveAccountInstanceLabel} from '@app/features/auth/AccountDisplayUtils';
import {
	AccountIdentityText,
	useAccountDisplayIdentityView,
} from '@app/features/auth/components/accounts/AccountIdentity';
import {getAccountMentionCount} from '@app/features/auth/components/accounts/account_switcher_modal/AccountMentionCounts';
import {SESSION_EXPIRED_DESCRIPTOR} from '@app/features/auth/components/accounts/account_switcher_modal/AccountPickerShared';
import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import {ACTIVE_ACCOUNT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {Account} from '@app/features/platform/state/AuthSession';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import {MentionBadge} from '@app/features/ui/components/MentionBadge';
import {MockAvatar} from '@app/features/ui/components/MockAvatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {flxElementClassName} from '@app/lib/react';
import {Trans, useLingui} from '@lingui/react/macro';
import {CheckIcon, GearIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

const UNKNOWN_INSTANCE_GROUP_KEY = 'unknown';

interface AccountInstanceGroup {
	readonly groupKey: string;
	readonly instanceLabel: string | null;
	readonly accounts: Array<Account>;
}

interface AccountInstanceGroups {
	readonly groups: Array<AccountInstanceGroup>;
	readonly hasMultipleInstances: boolean;
}

function createAccountInstanceGroups(accounts: ReadonlyArray<Account>): Array<AccountInstanceGroup> {
	const groups: Array<AccountInstanceGroup> = [];
	const groupsByKey = new Map<string, AccountInstanceGroup>();
	let unknownGroup: AccountInstanceGroup | null = null;
	for (const account of accounts) {
		const instanceKey = resolveAccountInstanceKey(account);
		if (instanceKey == null) {
			if (unknownGroup == null) {
				unknownGroup = {groupKey: UNKNOWN_INSTANCE_GROUP_KEY, instanceLabel: null, accounts: []};
			}
			unknownGroup.accounts.push(account);
			continue;
		}
		const groupKey = `instance:${instanceKey}`;
		let group = groupsByKey.get(groupKey);
		if (group == null) {
			group = {groupKey, instanceLabel: resolveAccountInstanceLabel(account), accounts: []};
			groupsByKey.set(groupKey, group);
			groups.push(group);
		}
		group.accounts.push(account);
	}
	if (unknownGroup != null) {
		groups.push(unknownGroup);
	}
	return groups;
}

function useAccountInstanceGroups(accounts: ReadonlyArray<Account>): AccountInstanceGroups {
	const groups = useMemo(() => createAccountInstanceGroups(accounts), [accounts]);
	return {groups, hasMultipleInstances: groups.length > 1};
}

interface SwitchAccountMenuItemProps {
	readonly account: Account;
	readonly currentAccountKey: string | null;
	readonly onSelect: (account: Account) => void;
	readonly onClose: () => void;
}

const SwitchAccountMenuItem = observer(
	({account, currentAccountKey, onSelect, onClose}: SwitchAccountMenuItemProps): React.ReactElement => {
		const {i18n} = useLingui();
		const accountKey = getAccountKey(account);
		const isCurrent = accountKey === currentAccountKey;
		const isExpired = account.isValid === false;
		const avatarUrl = useAccountAvatarURL(account);
		const identityView = useAccountDisplayIdentityView(account);
		const mentionCount = getAccountMentionCount(accountKey);
		let metaLabel: string | null = null;
		if (isExpired) {
			metaLabel = i18n._(SESSION_EXPIRED_DESCRIPTOR);
		} else if (isCurrent) {
			metaLabel = i18n._(ACTIVE_ACCOUNT_DESCRIPTOR);
		}
		return (
			<FocusRing offset={-2} data-flx="app.floating.user-area-account-switch-menu.focus-ring">
				<button
					type="button"
					className={styles.accountMenuItem}
					onClick={() => {
						if (!isCurrent) {
							onSelect(account);
						}
						onClose();
						PopoutCommands.close();
					}}
					data-flx="app.floating.user-area-account-switch-menu.account-menu-item.select.button"
				>
					<flx-app-account-switch-item-avatar
						className={flxElementClassName(styles.accountMenuAvatar)}
						data-flx="app.floating.user-area-account-switch-menu.account-menu-avatar"
					>
						<MockAvatar
							size={24}
							avatarUrl={avatarUrl}
							userTag={identityView.tagLabel}
							data-flx="app.floating.user-area-account-switch-menu.mock-avatar"
						/>
					</flx-app-account-switch-item-avatar>
					<flx-app-account-switch-item-info
						className={flxElementClassName(styles.accountMenuInfo)}
						data-flx="app.floating.user-area-account-switch-menu.account-menu-info"
					>
						<span
							className={styles.accountMenuTag}
							data-flx="app.floating.user-area-account-switch-menu.account-menu-tag"
						>
							<span
								className={styles.accountMenuNameText}
								data-flx="app.floating.user-area-account-switch-menu.account-menu-name-text"
							>
								<AccountIdentityText
									identityView={identityView}
									discriminatorClassName={styles.accountMenuDiscriminator}
									data-flx="app.floating.user-area-account-switch-menu.account-identity-text"
								/>
							</span>
							<MentionBadge
								mentionCount={mentionCount}
								size="small"
								data-flx="app.floating.user-area-account-switch-menu.mention-badge"
							/>
						</span>
						{metaLabel == null ? null : (
							<span
								className={clsx(styles.accountMenuMeta, isExpired && styles.accountMenuMetaExpired)}
								data-flx="app.floating.user-area-account-switch-menu.account-menu-meta"
							>
								{metaLabel}
							</span>
						)}
					</flx-app-account-switch-item-info>
					{isCurrent ? (
						<flx-app-account-switch-item-check
							className={flxElementClassName(styles.accountMenuCheck)}
							data-flx="app.floating.user-area-account-switch-menu.account-menu-check"
						>
							<CheckIcon
								size={remFromPx(10)}
								weight="bold"
								data-flx="app.floating.user-area-account-switch-menu.check-icon"
							/>
						</flx-app-account-switch-item-check>
					) : null}
				</button>
			</FocusRing>
		);
	},
);

interface UserAreaAccountSwitchMenuProps {
	readonly accounts: Array<Account>;
	readonly currentAccountKey: string | null;
	readonly onSelect: (account: Account) => void;
	readonly onManage: () => void;
	readonly onClose: () => void;
	readonly 'data-flx'?: string;
}

export const UserAreaAccountSwitchMenu = observer(
	({accounts, currentAccountKey, onSelect, onManage, onClose}: UserAreaAccountSwitchMenuProps) => {
		const {groups, hasMultipleInstances} = useAccountInstanceGroups(accounts);
		const renderAccount = (account: Account): React.ReactNode => (
			<SwitchAccountMenuItem
				key={getAccountKey(account)}
				account={account}
				currentAccountKey={currentAccountKey}
				onSelect={onSelect}
				onClose={onClose}
				data-flx="app.floating.user-area-account-switch-menu.switch-account-menu-item"
			/>
		);
		const renderInstanceGroup = (group: AccountInstanceGroup): React.ReactNode => (
			<flx-app-account-switch-menu-instance
				key={group.groupKey}
				className={flxElementClassName(styles.instanceGroup)}
				data-flx="app.floating.user-area-account-switch-menu.instance-group"
			>
				{group.instanceLabel == null ? null : (
					<flx-app-account-switch-menu-instance-title
						className={flxElementClassName(styles.instanceHeading)}
						data-flx="app.floating.user-area-account-switch-menu.instance-heading"
					>
						<span
							className={styles.instanceHeadingLabel}
							data-flx="app.floating.user-area-account-switch-menu.instance-heading-label"
						>
							{group.instanceLabel}
						</span>
					</flx-app-account-switch-menu-instance-title>
				)}
				<flx-app-account-switch-menu-accounts
					className={flxElementClassName(styles.instanceAccounts)}
					data-flx="app.floating.user-area-account-switch-menu.instance-accounts"
				>
					{group.accounts.map(renderAccount)}
				</flx-app-account-switch-menu-accounts>
			</flx-app-account-switch-menu-instance>
		);
		const renderAccounts = (): React.ReactNode => {
			if (!hasMultipleInstances) {
				return accounts.map(renderAccount);
			}
			return groups.map(renderInstanceGroup);
		};
		return (
			<flx-app-account-switch-menu
				className={flxElementClassName(styles.switchMenu)}
				data-flx="app.floating.user-area-account-switch-menu.switch-menu"
			>
				<flx-app-account-switch-menu-list
					className={flxElementClassName(styles.switchMenuList)}
					data-flx="app.floating.user-area-account-switch-menu.switch-menu-list"
				>
					{renderAccounts()}
				</flx-app-account-switch-menu-list>
				<flx-app-account-switch-menu-footer
					className={flxElementClassName(styles.switchMenuFooter)}
					data-flx="app.floating.user-area-account-switch-menu.switch-menu-footer"
				>
					<FocusRing offset={-2} data-flx="app.floating.user-area-account-switch-menu.focus-ring--2">
						<button
							type="button"
							className={styles.manageAccountsButton}
							onClick={() => {
								onClose();
								onManage();
							}}
							data-flx="app.floating.user-area-account-switch-menu.manage-accounts-button.manage"
						>
							<GearIcon
								size={remFromPx(16)}
								weight="bold"
								data-flx="app.floating.user-area-account-switch-menu.gear-icon"
							/>
							<Trans>Manage accounts</Trans>
						</button>
					</FocusRing>
				</flx-app-account-switch-menu-footer>
			</flx-app-account-switch-menu>
		);
	},
);
