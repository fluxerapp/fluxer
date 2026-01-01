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
import {CaretDownIcon, CheckIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as UserSettingsActionCreators from '~/actions/UserSettingsActionCreators';
import {getStatusTypeLabel, type StatusType, StatusTypes} from '~/Constants';
import {CustomStatusDisplay} from '~/components/common/CustomStatusDisplay/CustomStatusDisplay';
import {CustomStatusBottomSheet} from '~/components/modals/CustomStatusBottomSheet';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import {StatusIndicator} from '~/components/uikit/StatusIndicator';
import {normalizeCustomStatus} from '~/lib/customStatus';
import PresenceStore from '~/stores/PresenceStore';
import StatusExpiryStore from '~/stores/StatusExpiryStore';
import UserStore from '~/stores/UserStore';
import styles from './StatusChangeBottomSheet.module.css';

const STATUS_ORDER = [StatusTypes.ONLINE, StatusTypes.IDLE, StatusTypes.DND, StatusTypes.INVISIBLE] as const;

const STATUS_DESCRIPTIONS: Record<(typeof STATUS_ORDER)[number], React.ReactNode | null> = {
	[StatusTypes.ONLINE]: null,
	[StatusTypes.IDLE]: null,
	[StatusTypes.DND]: <Trans>You won&apos;t receive notifications on desktop</Trans>,
	[StatusTypes.INVISIBLE]: <Trans>You&apos;ll appear offline</Trans>,
};

const EXPIRY_OPTIONS = [
	{id: 'forever', label: <Trans>Until I change it</Trans>, durationMs: null},
	{id: '15m', label: <Trans>15 minutes</Trans>, durationMs: 15 * 60 * 1000},
	{id: '1h', label: <Trans>1 hour</Trans>, durationMs: 60 * 60 * 1000},
	{id: '8h', label: <Trans>8 hours</Trans>, durationMs: 8 * 60 * 60 * 1000},
	{id: '24h', label: <Trans>24 hours</Trans>, durationMs: 24 * 60 * 60 * 1000},
	{id: '3d', label: <Trans>3 days</Trans>, durationMs: 3 * 24 * 60 * 60 * 1000},
];

const STATUS_SHEET_SNAP_POINTS: Array<number> = [0, 0.75, 1];

interface StatusChangeBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
}

interface StatusItemProps {
	status: StatusType;
	currentStatus: StatusType;
	onSelect: (status: StatusType, durationMs: number | null) => void;
}

const StatusItem = observer(({status, currentStatus, onSelect}: StatusItemProps) => {
	const {i18n} = useLingui();
	const isSelected = currentStatus === status;
	const description = STATUS_DESCRIPTIONS[status as keyof typeof STATUS_DESCRIPTIONS];
	const hasExpiryOptions = status !== StatusTypes.ONLINE;
	const [showExpiry, setShowExpiry] = React.useState(false);

	const handleSelect = () => {
		if (hasExpiryOptions) {
			setShowExpiry(!showExpiry);
		} else {
			onSelect(status, null);
		}
	};

	const handleExpirySelect = (durationMs: number | null) => {
		onSelect(status, durationMs);
		setShowExpiry(false);
	};

	return (
		<div className={styles.statusItemWrapper}>
			<button type="button" onClick={handleSelect} className={styles.statusItemButton}>
				<div className={styles.statusItemContent}>
					<StatusIndicator status={status} size={14} monochromeColor="var(--brand-primary-fill)" />
					<div className={styles.statusItemInfo}>
						<span className={styles.statusLabel}>{getStatusTypeLabel(i18n, status)}</span>
						{description && <span className={styles.statusDescription}>{description}</span>}
					</div>
				</div>
				<div className={styles.statusItemRight}>
					{isSelected && (
						<div className={styles.selectedIndicator}>
							<CheckIcon weight="bold" className={styles.checkIcon} />
						</div>
					)}
					{hasExpiryOptions && (
						<CaretDownIcon
							weight="bold"
							className={clsx(styles.chevronIcon, showExpiry && styles.chevronIconExpanded)}
						/>
					)}
				</div>
			</button>
			{showExpiry && (
				<div className={styles.expiryList}>
					{EXPIRY_OPTIONS.map((option) => (
						<button
							key={option.id}
							type="button"
							className={styles.expiryItem}
							onClick={() => handleExpirySelect(option.durationMs)}
						>
							{option.label}
						</button>
					))}
				</div>
			)}
		</div>
	);
});

interface CustomStatusSectionProps {
	onOpenEditor: () => void;
}

const CustomStatusSection = observer(({onOpenEditor}: CustomStatusSectionProps) => {
	const currentUser = UserStore.getCurrentUser();
	const currentUserId = currentUser?.id ?? null;
	const existingCustomStatus = currentUserId ? PresenceStore.getCustomStatus(currentUserId) : null;
	const normalizedExisting = normalizeCustomStatus(existingCustomStatus);
	const hasExistingStatus = Boolean(normalizedExisting);

	const [isSaving, setIsSaving] = React.useState(false);

	const handleClear = async () => {
		if (isSaving) return;

		setIsSaving(true);
		try {
			await UserSettingsActionCreators.update({customStatus: null});
		} finally {
			setIsSaving(false);
		}
	};

	return (
		<div className={styles.customStatusSection}>
			<div className={styles.customStatusHeader}>
				<span className={styles.customStatusTitle}>
					<Trans>Custom Status</Trans>
				</span>
			</div>
			<button type="button" className={styles.customStatusButton} onClick={onOpenEditor}>
				{hasExistingStatus && normalizedExisting ? (
					<CustomStatusDisplay
						customStatus={normalizedExisting}
						showText={true}
						showTooltip={false}
						animateOnParentHover
					/>
				) : (
					<span className={styles.customStatusPlaceholder}>
						<Trans>Set a custom status</Trans>
					</span>
				)}
			</button>
			{hasExistingStatus && (
				<button type="button" className={styles.clearCustomStatusButton} onClick={handleClear} disabled={isSaving}>
					<Trans>Clear Custom Status</Trans>
				</button>
			)}
		</div>
	);
});

export const StatusChangeBottomSheet = observer(({isOpen, onClose}: StatusChangeBottomSheetProps) => {
	const {t} = useLingui();
	const currentUser = UserStore.getCurrentUser();
	const currentUserId = currentUser?.id ?? null;
	const status = currentUserId ? PresenceStore.getStatus(currentUserId) : StatusTypes.ONLINE;
	const [customStatusSheetOpen, setCustomStatusSheetOpen] = React.useState(false);

	const handleStatusChange = React.useCallback(
		(statusType: StatusType, durationMs: number | null) => {
			StatusExpiryStore.setActiveStatusExpiry({
				status: statusType,
				durationMs,
			});
			onClose();
		},
		[onClose],
	);

	const handleOpenCustomStatusEditor = React.useCallback(() => {
		setCustomStatusSheetOpen(true);
	}, []);

	const handleCloseCustomStatusEditor = React.useCallback(() => {
		setCustomStatusSheetOpen(false);
	}, []);

	return (
		<>
			<BottomSheet
				isOpen={isOpen}
				onClose={onClose}
				snapPoints={STATUS_SHEET_SNAP_POINTS}
				initialSnap={STATUS_SHEET_SNAP_POINTS.length - 1}
				title={t`Set Status`}
			>
				<div className={styles.content}>
					<div className={styles.topSpacer} />

					<CustomStatusSection onOpenEditor={handleOpenCustomStatusEditor} />

					<div className={styles.statusSection}>
						<div className={styles.sectionHeader}>
							<Trans>Online Status</Trans>
						</div>
						<div className={styles.statusContainer}>
							{STATUS_ORDER.map((statusType, index, arr) => (
								<React.Fragment key={statusType}>
									<StatusItem status={statusType} currentStatus={status} onSelect={handleStatusChange} />
									{index < arr.length - 1 && <div className={styles.divider} />}
								</React.Fragment>
							))}
						</div>
					</div>
				</div>
			</BottomSheet>
			<CustomStatusBottomSheet isOpen={customStatusSheetOpen} onClose={handleCloseCustomStatusEditor} />
		</>
	);
});
