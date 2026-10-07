// SPDX-License-Identifier: AGPL-3.0-or-later

import {useAccountAvatarURL} from '@app/features/auth/AccountAvatarUtils';
import {ACCOUNT_DETAILS_UNAVAILABLE_DESCRIPTOR, getAccountDisplayLabels} from '@app/features/auth/AccountDisplayUtils';
import {INSTANCE_UNAVAILABLE_DESCRIPTOR} from '@app/features/auth/AuthMessageDescriptors';
import {AccountInstanceBadge} from '@app/features/auth/components/accounts/account_switcher_modal/AccountInstanceBadge';
import {getAccountMentionCount} from '@app/features/auth/components/accounts/account_switcher_modal/AccountMentionCounts';
import {
	type AccountPickerContext,
	type AccountPickerDisabledPredicate,
	getAccountPickerContext,
	getAccountPickerStatusDescriptor,
	resolveAccountPickerDisabled,
	SAVED_ACCOUNTS_DESCRIPTOR,
} from '@app/features/auth/components/accounts/account_switcher_modal/AccountPickerShared';
import styles from '@app/features/auth/components/accounts/account_switcher_modal/AccountProfilePicker.module.css';
import {
	type AccountInstanceBadgeInfo,
	useAccountInstanceBadge,
} from '@app/features/auth/components/accounts/account_switcher_modal/useAccountInstanceBadge';
import type {InstanceInfo} from '@app/features/auth/flow/instance_selector/InstanceDirectoryStorage';
import {useKnownInstances} from '@app/features/auth/flow/instance_selector/useKnownInstances';
import {
	ADD_ACCOUNT_DESCRIPTOR,
	COPY_USER_ID_DESCRIPTOR,
	MORE_OPTIONS_DESCRIPTOR,
	OFFICIAL_INSTANCE_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {Account} from '@app/features/platform/state/AuthSession';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {CopyIdIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {BaseAvatar} from '@app/features/ui/components/BaseAvatar';
import {MentionBadge} from '@app/features/ui/components/MentionBadge';
import {Scroller} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import Window from '@app/features/window/state/Window';
import {flxElementClassName} from '@app/lib/react';
import type {MediaProxyImageSize} from '@fluxer/constants/src/MediaProxyImageSizes';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {DotsThreeIcon, PlusIcon, SignOutIcon, TrashIcon, WarningCircleIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useRef} from 'react';

const SELECT_ACCOUNT_DESCRIPTOR = msg({
	message: 'Use {accountTag}',
	comment: 'Accessible label for selecting a saved account from the profile-style account picker.',
});
const SELECT_ACCOUNT_ON_INSTANCE_DESCRIPTOR = msg({
	message: 'Use {accountTag} on {instanceName}',
	comment:
		'Accessible label for selecting a saved account from the profile-style account picker. instanceName is a server or instance display name.',
});
const REMOVE_ACCOUNT_DESCRIPTOR = msg({
	message: 'Remove {accountTag}',
	comment: 'Accessible label for removing a saved account from the profile-style account picker.',
});
const SIGN_OUT_ACCOUNT_DESCRIPTOR = msg({
	message: 'Sign out {accountTag}',
	comment: 'Accessible label for signing out the current account from the profile-style account picker.',
});

const MIN_AVATAR_SIZE = 128;
const MAX_AVATAR_SIZE = 220;
const AVATAR_VIEWPORT_WIDTH_RATIO = 0.125;
const AVATAR_VIEWPORT_HEIGHT_RATIO = 0.28;
const MAX_AVATAR_MEDIA_DEVICE_PIXEL_RATIO = 2;
const AVATAR_MEDIA_SIZES: ReadonlyArray<MediaProxyImageSize> = [160, 240, 256, 320, 480];
const MAX_AVATAR_MEDIA_SIZE: MediaProxyImageSize = 480;

type AccountProfilePickerStyle = React.CSSProperties & {
	'--account-profile-frame-size': string;
	'--account-profile-slot-width': string;
	'--account-profile-instance-badge-size': string;
	'--account-profile-state-badge-size': string;
};

function resolveResponsiveAvatarSize(viewportWidth: number, viewportHeight: number): number {
	const viewportTarget = Math.min(
		viewportWidth * AVATAR_VIEWPORT_WIDTH_RATIO,
		viewportHeight * AVATAR_VIEWPORT_HEIGHT_RATIO,
	);
	return Math.round(Math.min(MAX_AVATAR_SIZE, Math.max(MIN_AVATAR_SIZE, viewportTarget)));
}

function resolveAvatarMediaSize(avatarSize: number): MediaProxyImageSize {
	let devicePixelRatio = 1;
	const reportedDevicePixelRatio = globalThis.window?.devicePixelRatio ?? 1;
	if (reportedDevicePixelRatio > 1) {
		devicePixelRatio = Math.min(MAX_AVATAR_MEDIA_DEVICE_PIXEL_RATIO, reportedDevicePixelRatio);
	}
	const targetSize = avatarSize * devicePixelRatio;
	for (const size of AVATAR_MEDIA_SIZES) {
		if (size >= targetSize) {
			return size;
		}
	}
	return MAX_AVATAR_MEDIA_SIZE;
}

interface ResolveSelectLabelArgs {
	readonly i18n: I18n;
	readonly showInstance: boolean;
	readonly accountTag: string;
	readonly instanceLabel: string | null;
}

function resolveSelectLabel({i18n, showInstance, accountTag, instanceLabel}: ResolveSelectLabelArgs): string {
	if (showInstance && instanceLabel != null) {
		return i18n._(SELECT_ACCOUNT_ON_INSTANCE_DESCRIPTOR, {accountTag, instanceName: instanceLabel});
	}
	return i18n._(SELECT_ACCOUNT_DESCRIPTOR, {accountTag});
}

function resolveRemoveAccountDescriptor(isCurrent: boolean): MessageDescriptor {
	return isCurrent ? SIGN_OUT_ACCOUNT_DESCRIPTOR : REMOVE_ACCOUNT_DESCRIPTOR;
}

function resolveARIADisabled(tileDisabled: boolean, primaryDisabled: boolean): boolean {
	return tileDisabled ? false : primaryDisabled;
}

interface RenderInstanceBadgeArgs {
	readonly fallbackInstanceLabel: string;
	readonly instanceBadge: AccountInstanceBadgeInfo | null;
	readonly officialLabel: string;
	readonly showInstance: boolean;
}

function renderInstanceBadge({
	fallbackInstanceLabel,
	instanceBadge,
	officialLabel,
	showInstance,
}: RenderInstanceBadgeArgs): React.ReactNode {
	if (!showInstance) {
		return null;
	}
	return (
		<AccountInstanceBadge
			badge={instanceBadge}
			fallbackLabel={fallbackInstanceLabel}
			officialLabel={officialLabel}
			data-flx="auth.accounts.account-switcher-modal.account-profile-picker.render-instance-badge.account-instance-badge"
		/>
	);
}

function renderExpiredAccountStatus(isExpired: boolean, statusLabel: string): React.ReactNode {
	if (!isExpired) {
		return null;
	}
	return (
		<Tooltip
			text={statusLabel}
			type="error"
			position="top"
			data-flx="auth.accounts.account-switcher-modal.account-profile-picker.render-expired-account-status.tooltip.error"
		>
			<span
				className={clsx(styles.stateBadge, styles.stateBadgeExpired)}
				role="img"
				aria-label={statusLabel}
				data-flx="auth.accounts.account-switcher-modal.account-profile-picker.render-expired-account-status.state-badge"
			>
				<WarningCircleIcon
					size={remFromPx(17)}
					weight="fill"
					data-flx="auth.accounts.account-switcher-modal.account-profile-picker.render-expired-account-status.warning-circle-icon"
				/>
			</span>
		</Tooltip>
	);
}

interface AccountProfilePickerProps {
	readonly accounts: Array<Account>;
	readonly currentAccountKey: string | null;
	readonly disabled: boolean;
	readonly showInstance: boolean;
	readonly primaryActionDisabled: boolean | AccountPickerDisabledPredicate;
	readonly onPrimaryAction: (account: Account) => void;
	readonly secondaryActionDisabled: boolean | AccountPickerDisabledPredicate;
	readonly onSecondaryAction: ((account: Account) => void) | null;
	readonly onAddAccount: (() => void) | null;
	readonly className: string | null;
	readonly scrollerClassName: string | null;
}

interface AccountProfileTileProps {
	readonly account: Account;
	readonly context: AccountPickerContext;
	readonly disabled: boolean;
	readonly showInstance: boolean;
	readonly knownInstances: ReadonlyArray<InstanceInfo>;
	readonly avatarSize: number;
	readonly avatarMediaSize: MediaProxyImageSize;
	readonly primaryActionDisabled: boolean | AccountPickerDisabledPredicate;
	readonly onPrimaryAction: (account: Account) => void;
	readonly secondaryActionDisabled: boolean | AccountPickerDisabledPredicate;
	readonly onSecondaryAction: ((account: Account) => void) | null;
}

const AccountProfileTile = observer(function AccountProfileTile({
	account,
	context,
	disabled,
	showInstance,
	knownInstances,
	avatarSize,
	avatarMediaSize,
	primaryActionDisabled,
	onPrimaryAction,
	secondaryActionDisabled,
	onSecondaryAction,
}: AccountProfileTileProps): React.ReactElement {
	const {i18n} = useLingui();
	const avatarURL = useAccountAvatarURL(account, avatarMediaSize);
	const labels = getAccountDisplayLabels(account);
	const unavailableLabel = i18n._(ACCOUNT_DETAILS_UNAVAILABLE_DESCRIPTOR);
	const displayLabel = labels.available ? labels.displayLabel : unavailableLabel;
	const tagLabel = labels.available ? labels.tagLabel : unavailableLabel;
	const instanceBadge = useAccountInstanceBadge(account, knownInstances, showInstance);
	const mentionCount = getAccountMentionCount(context.accountKey);
	const fallbackInstanceLabel = i18n._(INSTANCE_UNAVAILABLE_DESCRIPTOR);
	const officialLabel = i18n._(OFFICIAL_INSTANCE_DESCRIPTOR);
	const instanceLabel = instanceBadge?.label ?? null;
	const statusLabel = i18n._(getAccountPickerStatusDescriptor(context));
	const primaryDisabled = disabled || resolveAccountPickerDisabled(primaryActionDisabled, account, context);
	const secondaryDisabled = disabled || resolveAccountPickerDisabled(secondaryActionDisabled, account, context);
	const selectLabel = resolveSelectLabel({i18n, showInstance, accountTag: tagLabel, instanceLabel});
	const removeLabel = i18n._(resolveRemoveAccountDescriptor(context.isCurrent), {accountTag: tagLabel});
	const avatarFrameRef = useRef<HTMLSpanElement | null>(null);
	const handlePrimaryAction = useCallback(() => {
		if (primaryDisabled) {
			return;
		}
		onPrimaryAction(account);
	}, [account, onPrimaryAction, primaryDisabled]);
	const handleSecondaryAction = useCallback(() => {
		if (secondaryDisabled || onSecondaryAction == null) {
			return;
		}
		onSecondaryAction(account);
	}, [account, onSecondaryAction, secondaryDisabled]);
	const handleCopyUserId = useCallback(() => {
		TextCopyCommands.copy(i18n, account.userId, true);
	}, [account.userId, i18n]);
	const renderTileContextMenu = (onClose: () => void): React.ReactNode => (
		<MenuGroup data-flx="auth.accounts.account-switcher-modal.account-profile-picker.render-tile-context-menu.menu-group">
			<MenuItem
				icon={
					<CopyIdIcon
						size={18}
						data-flx="auth.accounts.account-switcher-modal.account-profile-picker.render-tile-context-menu.copy-id-icon"
					/>
				}
				onClick={() => {
					onClose();
					handleCopyUserId();
				}}
				data-flx="auth.accounts.account-switcher-modal.account-profile-picker.render-tile-context-menu.menu-item.close"
			>
				{i18n._(COPY_USER_ID_DESCRIPTOR)}
			</MenuItem>
			{onSecondaryAction == null ? null : (
				<MenuItem
					danger
					disabled={secondaryDisabled}
					icon={
						context.isCurrent ? (
							<SignOutIcon
								size={18}
								data-flx="auth.accounts.account-switcher-modal.account-profile-picker.render-tile-context-menu.sign-out-icon"
							/>
						) : (
							<TrashIcon
								size={18}
								data-flx="auth.accounts.account-switcher-modal.account-profile-picker.render-tile-context-menu.trash-icon"
							/>
						)
					}
					onClick={() => {
						onClose();
						handleSecondaryAction();
					}}
					data-flx="auth.accounts.account-switcher-modal.account-profile-picker.render-tile-context-menu.menu-item.close--2"
				>
					{removeLabel}
				</MenuItem>
			)}
		</MenuGroup>
	);
	const handleTileContextMenu = (event: React.MouseEvent<HTMLButtonElement>): void => {
		const ownerWindow = event.currentTarget.ownerDocument.defaultView;
		const avatarFrame = avatarFrameRef.current;
		if (ownerWindow == null || avatarFrame == null) {
			return;
		}
		const target = event.target;
		if (target !== event.currentTarget && (!(target instanceof ownerWindow.Node) || !avatarFrame.contains(target))) {
			return;
		}
		ContextMenuCommands.openFromEvent(event, ({onClose}) => renderTileContextMenu(onClose));
	};
	const handleMoreClick = (event: React.MouseEvent<HTMLButtonElement>): void => {
		ContextMenuCommands.openFromElementBottomRight(event, ({onClose}) => renderTileContextMenu(onClose));
	};
	return (
		<div
			className={clsx(styles.tileSlot, context.isExpired && styles.tileSlotExpired)}
			role="listitem"
			data-flx="auth.accounts.account-switcher-modal.account-profile-picker.account-profile-tile.tile-slot"
		>
			<FocusRing
				offset={-2}
				ringTarget={avatarFrameRef}
				data-flx="auth.accounts.account-switcher-modal.account-profile-picker.account-profile-tile.focus-ring"
			>
				<button
					type="button"
					className={clsx(
						styles.tile,
						primaryDisabled && styles.tilePrimaryDisabled,
						context.isCurrent && styles.tileCurrent,
						context.isExpired && styles.tileExpired,
					)}
					onClick={handlePrimaryAction}
					onContextMenu={handleTileContextMenu}
					disabled={disabled}
					aria-disabled={resolveARIADisabled(disabled, primaryDisabled)}
					aria-current={context.isCurrent}
					aria-label={`${selectLabel}, ${statusLabel}`}
					data-flx="auth.accounts.account-switcher-modal.account-profile-picker.account-profile-tile.tile.primary-action.button"
				>
					<span
						ref={avatarFrameRef}
						className={styles.avatarFrame}
						data-flx="auth.accounts.account-switcher-modal.account-profile-picker.account-profile-tile.avatar-frame"
					>
						<BaseAvatar
							size={avatarSize}
							avatarUrl={avatarURL}
							userTag={tagLabel}
							data-flx="auth.accounts.account-switcher-modal.account-profile-picker.account-profile-tile.base-avatar"
						/>
						{renderInstanceBadge({fallbackInstanceLabel, instanceBadge, officialLabel, showInstance})}
						{renderExpiredAccountStatus(context.isExpired, statusLabel)}
					</span>
				</button>
			</FocusRing>
			<div
				className={styles.nameRow}
				data-flx="auth.accounts.account-switcher-modal.account-profile-picker.account-profile-tile.name-row"
			>
				<span
					className={styles.name}
					aria-hidden="true"
					data-flx="auth.accounts.account-switcher-modal.account-profile-picker.account-profile-tile.name"
				>
					<span
						className={styles.nameText}
						data-flx="auth.accounts.account-switcher-modal.account-profile-picker.account-profile-tile.name-text"
					>
						{displayLabel}
					</span>
					{mentionCount > 0 ? (
						<MentionBadge
							mentionCount={mentionCount}
							size="small"
							data-flx="auth.accounts.account-switcher-modal.account-profile-picker.account-profile-tile.mention-badge"
						/>
					) : null}
				</span>
				<FocusRing
					offset={-2}
					data-flx="auth.accounts.account-switcher-modal.account-profile-picker.account-profile-tile.more-focus-ring"
				>
					<button
						type="button"
						className={styles.moreButton}
						onClick={handleMoreClick}
						disabled={disabled}
						aria-haspopup="menu"
						aria-label={`${i18n._(MORE_OPTIONS_DESCRIPTOR)}, ${tagLabel}`}
						data-flx="auth.accounts.account-switcher-modal.account-profile-picker.account-profile-tile.more.button"
					>
						<DotsThreeIcon
							size={remFromPx(12)}
							weight="bold"
							data-flx="auth.accounts.account-switcher-modal.account-profile-picker.account-profile-tile.dots-three-icon"
						/>
					</button>
				</FocusRing>
			</div>
		</div>
	);
});

interface AddAccountTileProps {
	readonly disabled: boolean;
	readonly avatarSize: number;
	readonly onAddAccount: () => void;
}

function AddAccountTile({disabled, avatarSize, onAddAccount}: AddAccountTileProps): React.ReactElement {
	const {i18n} = useLingui();
	const avatarRef = useRef<HTMLSpanElement | null>(null);
	return (
		<div
			className={styles.tileSlot}
			role="listitem"
			data-flx="auth.accounts.account-switcher-modal.account-profile-picker.add-account-tile.tile-slot"
		>
			<FocusRing
				offset={-2}
				ringTarget={avatarRef}
				data-flx="auth.accounts.account-switcher-modal.account-profile-picker.add-account-tile.focus-ring"
			>
				<button
					type="button"
					className={clsx(styles.tile, styles.tileAdd)}
					onClick={onAddAccount}
					disabled={disabled}
					aria-label={i18n._(ADD_ACCOUNT_DESCRIPTOR)}
					data-flx="auth.accounts.account-switcher-modal.account-profile-picker.add-account-tile.tile.add-account.button"
				>
					<span
						ref={avatarRef}
						className={styles.tileAddAvatar}
						data-flx="auth.accounts.account-switcher-modal.account-profile-picker.add-account-tile.tile-add-avatar"
					>
						<PlusIcon
							size={remFromPx(Math.round(avatarSize * 0.34))}
							weight="bold"
							data-flx="auth.accounts.account-switcher-modal.account-profile-picker.add-account-tile.plus-icon"
						/>
					</span>
				</button>
			</FocusRing>
		</div>
	);
}

export const AccountProfilePicker = observer(function AccountProfilePicker({
	accounts,
	currentAccountKey,
	disabled,
	showInstance,
	primaryActionDisabled,
	onPrimaryAction,
	secondaryActionDisabled,
	onSecondaryAction,
	onAddAccount,
	className,
	scrollerClassName,
}: AccountProfilePickerProps): React.ReactElement | null {
	const {i18n} = useLingui();
	const knownInstances = useKnownInstances(showInstance);
	const {width, height} = Window.windowSize;
	const avatarSize = resolveResponsiveAvatarSize(width, height);
	const avatarMediaSize = useMemo(() => resolveAvatarMediaSize(avatarSize), [avatarSize]);
	const style = useMemo<AccountProfilePickerStyle>(
		() => ({
			'--account-profile-frame-size': remFromPx(avatarSize),
			'--account-profile-slot-width': remFromPx(Math.round(avatarSize * 1.24)),
			'--account-profile-instance-badge-size': remFromPx(Math.round(Math.max(34, avatarSize * 0.24))),
			'--account-profile-state-badge-size': remFromPx(Math.round(Math.max(26, avatarSize * 0.22))),
		}),
		[avatarSize],
	);
	if (accounts.length === 0 && onAddAccount == null) {
		return null;
	}
	return (
		<flx-auth-account-profile-picker
			className={flxElementClassName(styles.root, className)}
			style={style}
			data-flx="auth.accounts.account-switcher-modal.account-profile-picker.root"
		>
			<Scroller
				className={clsx(styles.scroller, scrollerClassName)}
				orientation="horizontal"
				data-flx="auth.accounts.account-switcher-modal.account-profile-picker.scroller"
			>
				<div
					className={styles.list}
					role="list"
					aria-label={i18n._(SAVED_ACCOUNTS_DESCRIPTOR)}
					data-flx="auth.accounts.account-switcher-modal.account-profile-picker.list"
				>
					{accounts.map((account) => {
						const context = getAccountPickerContext(account, currentAccountKey);
						return (
							<AccountProfileTile
								key={context.accountKey}
								account={account}
								context={context}
								disabled={disabled}
								showInstance={showInstance}
								knownInstances={knownInstances}
								avatarSize={avatarSize}
								avatarMediaSize={avatarMediaSize}
								primaryActionDisabled={primaryActionDisabled}
								onPrimaryAction={onPrimaryAction}
								secondaryActionDisabled={secondaryActionDisabled}
								onSecondaryAction={onSecondaryAction}
								data-flx="auth.accounts.account-switcher-modal.account-profile-picker.account-profile-tile"
							/>
						);
					})}
					{onAddAccount == null ? null : (
						<AddAccountTile
							disabled={disabled}
							avatarSize={avatarSize}
							onAddAccount={onAddAccount}
							data-flx="auth.accounts.account-switcher-modal.account-profile-picker.add-account-tile"
						/>
					)}
				</div>
			</Scroller>
		</flx-auth-account-profile-picker>
	);
});
