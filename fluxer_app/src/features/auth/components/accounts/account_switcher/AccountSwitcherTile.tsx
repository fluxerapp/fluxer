// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountInstanceBadge} from '@app/features/auth/components/accounts/account_switcher/AccountInstanceBadge';
import styles from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherTile.module.css';
import type {AccountInstanceBadgeInfo} from '@app/features/auth/components/accounts/account_switcher/useAccountInstanceBadge';
import {
	type AvatarCutoutMask,
	useAvatarBadgeCutouts,
} from '@app/features/auth/components/accounts/account_switcher/useAvatarBadgeCutouts';
import {MentionBadge} from '@app/features/ui/components/MentionBadge';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {CheckIcon, PencilSimpleIcon, PlusIcon, WarningCircleIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {motion} from 'framer-motion';
import type React from 'react';
import {useId, useRef} from 'react';

export const ADD_ACCOUNT_TILE_KEY = 'add-account';

export const AccountTileLineTone = Object.freeze({
	MUTED: 'muted',
	WARNING: 'warning',
	DANGER: 'danger',
} as const);

export type AccountTileLineTone = (typeof AccountTileLineTone)[keyof typeof AccountTileLineTone];

export type AccountMenuTrigger = React.MouseEvent<HTMLButtonElement> | HTMLButtonElement;

export interface AccountSwitcherTileModel {
	readonly accountKey: string;
	readonly displayName: string;
	readonly secondLine: string;
	readonly secondLineTone: AccountTileLineTone;
	readonly avatarUrl: string;
	readonly instanceBadge: AccountInstanceBadgeInfo | null;
	readonly isCurrent: boolean;
	readonly isExpired: boolean;
	readonly mentionCount: number;
	readonly hasUnread: boolean;
	readonly ariaLabel: string;
}

interface TileInteraction {
	readonly isActive: boolean;
	readonly isManaging: boolean;
	readonly isPending: boolean;
	readonly isDimmed: boolean;
	readonly isMenuOpen: boolean;
	readonly disabled: boolean;
	readonly onFocusTile: (key: string) => void;
}

interface AccountTileDrag {
	readonly isDragging: boolean;
	readonly isReordering: boolean;
	readonly animateLayout: boolean;
	readonly dragRef: (node: HTMLElement | null) => void;
	readonly dropRef: (node: HTMLElement | null) => void;
}

const TILE_LAYOUT_TRANSITION = {duration: 0.18, ease: 'easeOut'} as const;

interface AccountSwitcherTileViewProps extends TileInteraction {
	readonly tile: AccountSwitcherTileModel;
	readonly drag: AccountTileDrag;
	readonly onActivate: (accountKey: string, event: React.MouseEvent<HTMLButtonElement>) => void;
	readonly onOpenMenu: (accountKey: string, trigger: AccountMenuTrigger) => void;
}

function isContextMenuKey(event: React.KeyboardEvent): boolean {
	return event.key === 'ContextMenu' || (event.shiftKey && event.key === 'F10');
}

function renderActivityBadge(tile: AccountSwitcherTileModel): React.ReactNode {
	if (tile.isExpired) {
		return (
			<span
				className={clsx(styles.cornerBadge, styles.expiredBadge)}
				data-badge-cutout=""
				aria-hidden="true"
				data-flx="auth.accounts.account-switcher.tile.expired-badge"
			>
				<WarningCircleIcon weight="fill" data-flx="auth.accounts.account-switcher.tile.expired-icon" />
			</span>
		);
	}
	if (tile.mentionCount > 0) {
		return (
			<span
				className={clsx(styles.cornerBadge, styles.mentionBadge)}
				data-badge-cutout=""
				aria-hidden="true"
				data-flx="auth.accounts.account-switcher.tile.mention-badge"
			>
				<MentionBadge
					mentionCount={tile.mentionCount}
					size="medium"
					data-flx="auth.accounts.account-switcher.tile.mention-count"
				/>
			</span>
		);
	}
	if (tile.hasUnread) {
		return (
			<span
				className={clsx(styles.cornerBadge, styles.unreadDot)}
				data-badge-cutout=""
				aria-hidden="true"
				data-flx="auth.accounts.account-switcher.tile.unread-dot"
			/>
		);
	}
	return null;
}

function tileBadgeKey(tile: AccountSwitcherTileModel): string {
	return [
		tile.isCurrent ? 'current' : '',
		tile.instanceBadge == null ? '' : 'instance',
		tile.isExpired ? 'expired' : '',
		tile.mentionCount > 0 ? `mentions-${tile.mentionCount}` : '',
		tile.hasUnread ? 'unread' : '',
	]
		.filter((part) => part.length > 0)
		.join(' ');
}

function AvatarCutoutMaskDefs({id, mask}: {readonly id: string; readonly mask: AvatarCutoutMask}): React.ReactElement {
	return (
		<svg className={styles.cutoutDefs} aria-hidden="true" data-flx="auth.accounts.account-switcher.tile.cutout-defs">
			<defs data-flx="auth.accounts.account-switcher.tile.cutout-defs.defs">
				<mask
					id={id}
					maskUnits="userSpaceOnUse"
					maskContentUnits="userSpaceOnUse"
					x={0}
					y={0}
					width={mask.width}
					height={mask.height}
					data-flx="auth.accounts.account-switcher.tile.cutout-mask"
				>
					<rect
						fill="white"
						x={0}
						y={0}
						width={mask.width}
						height={mask.height}
						data-flx="auth.accounts.account-switcher.tile.cutout-mask.base"
					/>
					{mask.cutouts.map((cutout) => (
						<rect
							key={`${cutout.x}:${cutout.y}`}
							fill="black"
							x={cutout.x}
							y={cutout.y}
							width={cutout.width}
							height={cutout.height}
							rx={cutout.radius}
							ry={cutout.radius}
							data-flx="auth.accounts.account-switcher.tile.cutout-mask.cutout"
						/>
					))}
				</mask>
			</defs>
		</svg>
	);
}

function BusyRing(): React.ReactElement {
	return (
		<svg
			className={styles.busyRing}
			viewBox="0 0 100 100"
			aria-hidden="true"
			data-flx="auth.accounts.account-switcher.tile.busy-ring"
		>
			<rect
				x="1"
				y="1"
				width="98"
				height="98"
				rx="23"
				ry="23"
				pathLength={100}
				data-flx="auth.accounts.account-switcher.account-switcher-tile.busy-ring.rect"
			/>
		</svg>
	);
}

export function AccountSwitcherTileView({
	tile,
	drag,
	isActive,
	isManaging,
	isPending,
	isDimmed,
	isMenuOpen,
	disabled,
	onFocusTile,
	onActivate,
	onOpenMenu,
}: AccountSwitcherTileViewProps): React.ReactElement {
	const frameRef = useRef<HTMLSpanElement | null>(null);
	const maskId = useId();
	const cutoutMask = useAvatarBadgeCutouts(frameRef, tileBadgeKey(tile));
	return (
		<motion.li
			ref={drag.dropRef}
			layout={drag.animateLayout ? 'position' : false}
			transition={TILE_LAYOUT_TRANSITION}
			className={clsx(styles.slot, isDimmed && styles.slotDimmed, drag.isDragging && styles.slotDragging)}
			data-account-slot=""
			data-flx="auth.accounts.account-switcher.tile.slot"
		>
			<FocusRing offset={-8} data-flx="auth.accounts.account-switcher.tile.focus-ring">
				<button
					ref={drag.dragRef}
					type="button"
					className={clsx(
						styles.tile,
						tile.isExpired && styles.tileExpired,
						(isManaging || drag.isReordering) && styles.tileStill,
					)}
					tabIndex={isActive ? 0 : -1}
					disabled={disabled}
					aria-label={tile.ariaLabel}
					aria-current={tile.isCurrent ? 'true' : undefined}
					aria-haspopup={isManaging ? 'menu' : undefined}
					aria-expanded={isManaging ? isMenuOpen : undefined}
					aria-busy={isPending || undefined}
					aria-keyshortcuts="Shift+F10"
					data-switcher-tile={tile.accountKey}
					onFocus={() => onFocusTile(tile.accountKey)}
					onClick={(event) => onActivate(tile.accountKey, event)}
					onContextMenu={(event) => onOpenMenu(tile.accountKey, event)}
					onKeyDown={(event) => {
						if (!isContextMenuKey(event)) return;
						event.preventDefault();
						event.stopPropagation();
						onOpenMenu(tile.accountKey, event.currentTarget);
					}}
					data-flx="auth.accounts.account-switcher.tile.button"
				>
					<span
						ref={frameRef}
						className={styles.avatarFrame}
						data-tile-avatar=""
						data-flx="auth.accounts.account-switcher.tile.avatar-frame"
					>
						{cutoutMask == null ? null : (
							<AvatarCutoutMaskDefs
								id={maskId}
								mask={cutoutMask}
								data-flx="auth.accounts.account-switcher.tile.cutout-mask-defs"
							/>
						)}
						<span
							className={styles.avatarClip}
							style={cutoutMask == null ? undefined : {maskImage: `url(#${maskId})`}}
							data-flx="auth.accounts.account-switcher.tile.avatar-clip"
						>
							<img
								className={styles.avatar}
								src={tile.avatarUrl}
								alt=""
								draggable={false}
								decoding="async"
								loading="lazy"
								data-flx="auth.accounts.account-switcher.tile.avatar"
							/>
							{isManaging ? (
								<span
									className={styles.manageOverlay}
									aria-hidden="true"
									data-flx="auth.accounts.account-switcher.tile.manage-overlay"
								>
									<PencilSimpleIcon weight="bold" data-flx="auth.accounts.account-switcher.tile.manage-icon" />
								</span>
							) : null}
						</span>
						{isPending ? <BusyRing data-flx="auth.accounts.account-switcher.tile.busy" /> : null}
						{tile.isCurrent ? (
							<span
								className={clsx(styles.cornerBadge, styles.currentBadge)}
								data-badge-cutout=""
								aria-hidden="true"
								data-flx="auth.accounts.account-switcher.tile.current-badge"
							>
								<CheckIcon weight="bold" data-flx="auth.accounts.account-switcher.tile.current-icon" />
							</span>
						) : null}
						{tile.instanceBadge == null ? null : (
							<AccountInstanceBadge
								badge={tile.instanceBadge}
								data-flx="auth.accounts.account-switcher.tile.instance-badge"
							/>
						)}
						{renderActivityBadge(tile)}
					</span>
					<span
						className={clsx('user-text', styles.name)}
						dir="auto"
						aria-hidden="true"
						data-flx="auth.accounts.account-switcher.tile.name"
					>
						{tile.displayName}
					</span>
					<span
						className={clsx(
							styles.secondLine,
							tile.secondLineTone === AccountTileLineTone.WARNING && styles.secondLineWarning,
							tile.secondLineTone === AccountTileLineTone.DANGER && styles.secondLineDanger,
						)}
						dir="auto"
						aria-hidden="true"
						data-flx="auth.accounts.account-switcher.tile.second-line"
					>
						{tile.secondLine}
					</span>
				</button>
			</FocusRing>
		</motion.li>
	);
}

interface AccountSwitcherAddTileProps extends Omit<TileInteraction, 'isManaging' | 'isPending' | 'isMenuOpen'> {
	readonly label: string;
	readonly onAdd: () => void;
}

export function AccountSwitcherAddTile({
	label,
	isActive,
	isDimmed,
	disabled,
	onFocusTile,
	onAdd,
}: AccountSwitcherAddTileProps): React.ReactElement {
	return (
		<li
			className={clsx(styles.slot, isDimmed && styles.slotDimmed)}
			data-flx="auth.accounts.account-switcher.add-tile.slot"
		>
			<FocusRing offset={-8} data-flx="auth.accounts.account-switcher.add-tile.focus-ring">
				<button
					type="button"
					className={clsx(styles.tile, styles.tileAdd)}
					tabIndex={isActive ? 0 : -1}
					disabled={disabled}
					data-switcher-tile={ADD_ACCOUNT_TILE_KEY}
					onFocus={() => onFocusTile(ADD_ACCOUNT_TILE_KEY)}
					onClick={onAdd}
					data-flx="auth.accounts.account-switcher.add-tile.button"
				>
					<span
						className={clsx(styles.avatarFrame, styles.addFrame)}
						data-flx="auth.accounts.account-switcher.add-tile.frame"
					>
						<PlusIcon weight="bold" aria-hidden="true" data-flx="auth.accounts.account-switcher.add-tile.plus-icon" />
					</span>
					<span className={styles.name} data-flx="auth.accounts.account-switcher.add-tile.label">
						{label}
					</span>
				</button>
			</FocusRing>
		</li>
	);
}
