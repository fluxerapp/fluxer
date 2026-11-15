// SPDX-License-Identifier: AGPL-3.0-or-later

import {CustomStatusDisplay} from '@app/features/app/components/shared/custom_status_display/CustomStatusDisplay';
import {
	getTimeWindowPresets,
	minutesToMs,
	TIME_WINDOW_FOR_LABEL_MESSAGES,
	type TimeWindowKey,
	type TimeWindowPreset,
} from '@app/features/app/config/TimeWindowPresets';
import {getStatusTypeLabel, STATUS_UNTIL_I_CHANGE_IT_DESCRIPTOR} from '@app/features/app/constants/AppConstants';
import DeveloperMode from '@app/features/devtools/state/DeveloperMode';
import Presence from '@app/features/presence/state/Presence';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {StatusIndicator} from '@app/features/ui/components/StatusIndicator';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import {CustomStatusBottomSheet} from '@app/features/user/components/modals/CustomStatusBottomSheet';
import styles from '@app/features/user/components/modals/StatusChangeBottomSheet.module.css';
import {normalizeCustomStatus} from '@app/features/user/state/CustomStatus';
import StatusExpiry from '@app/features/user/state/StatusExpiry';
import Users from '@app/features/user/state/Users';
import type {StatusType} from '@fluxer/constants/src/StatusConstants';
import {StatusTypes} from '@fluxer/constants/src/StatusConstants';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CaretDownIcon, CheckIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useMemo, useState} from 'react';

const SET_STATUS_DESCRIPTOR = msg({
	message: 'Set status',
	comment: 'Short label in the status change bottom sheet. Keep it concise.',
});
const STATUS_ORDER = [StatusTypes.ONLINE, StatusTypes.IDLE, StatusTypes.DND, StatusTypes.INVISIBLE] as const;
const STATUS_DESCRIPTIONS: Record<(typeof STATUS_ORDER)[number], React.ReactNode | null> = {
	[StatusTypes.ONLINE]: null,
	[StatusTypes.IDLE]: null,
	[StatusTypes.DND]: <Trans>You won't receive notifications on desktop</Trans>,
	[StatusTypes.INVISIBLE]: <Trans>You'll appear offline</Trans>,
};
const STATUS_EXPIRY_LABEL_MESSAGES: Record<TimeWindowKey, MessageDescriptor> = {
	...TIME_WINDOW_FOR_LABEL_MESSAGES,
	never: STATUS_UNTIL_I_CHANGE_IT_DESCRIPTOR,
};

interface StatusExpiryOption {
	id: TimeWindowKey;
	key: TimeWindowKey;
	label: MessageDescriptor;
	durationMs: number | null;
}

const getStatusExpiryOptions = (includeDeveloperOptions: boolean): ReadonlyArray<StatusExpiryOption> =>
	getTimeWindowPresets({includeDeveloperOptions}).map((preset: TimeWindowPreset) => ({
		id: preset.key,
		key: preset.key,
		label: STATUS_EXPIRY_LABEL_MESSAGES[preset.key],
		durationMs: minutesToMs(preset.minutes),
	}));
const STATUS_SHEET_SNAP_POINTS: Array<number> = [0, 0.75, 1];

interface StatusChangeBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
}

interface StatusItemProps {
	status: StatusType;
	currentStatus: StatusType;
	expiryOptions: ReadonlyArray<StatusExpiryOption>;
	onSelect: (status: StatusType, durationMs: number | null) => void;
}

const StatusItem = observer(({status, currentStatus, expiryOptions, onSelect}: StatusItemProps) => {
	const {i18n} = useLingui();
	const isSelected = currentStatus === status;
	const description = STATUS_DESCRIPTIONS[status as keyof typeof STATUS_DESCRIPTIONS];
	const hasExpiryOptions = status !== StatusTypes.ONLINE;
	const [showExpiry, setShowExpiry] = useState(false);
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
		<div
			className={styles.statusItemWrapper}
			data-flx="user.status-change-bottom-sheet.status-item.status-item-wrapper"
		>
			<button
				type="button"
				onClick={handleSelect}
				className={styles.statusItemButton}
				aria-pressed={isSelected}
				aria-expanded={hasExpiryOptions ? showExpiry : undefined}
				data-flx="user.status-change-bottom-sheet.status-item.status-item-button.select"
			>
				<div
					className={styles.statusItemContent}
					data-flx="user.status-change-bottom-sheet.status-item.status-item-content"
				>
					<StatusIndicator
						status={status}
						size={14}
						monochromeColor="var(--brand-primary-fill)"
						data-flx="user.status-change-bottom-sheet.status-item.status-indicator"
					/>
					<div
						className={styles.statusItemInfo}
						data-flx="user.status-change-bottom-sheet.status-item.status-item-info"
					>
						<span className={styles.statusLabel} data-flx="user.status-change-bottom-sheet.status-item.status-label">
							{getStatusTypeLabel(i18n, status)}
						</span>
						{description && (
							<span
								className={styles.statusDescription}
								data-flx="user.status-change-bottom-sheet.status-item.status-description"
							>
								{description}
							</span>
						)}
					</div>
				</div>
				<div
					className={styles.statusItemRight}
					data-flx="user.status-change-bottom-sheet.status-item.status-item-right"
				>
					{isSelected && (
						<div
							className={styles.selectedIndicator}
							data-flx="user.status-change-bottom-sheet.status-item.selected-indicator"
						>
							<CheckIcon
								weight="bold"
								className={styles.checkIcon}
								data-flx="user.status-change-bottom-sheet.status-item.check-icon"
							/>
						</div>
					)}
					{hasExpiryOptions && (
						<CaretDownIcon
							weight="bold"
							className={clsx(styles.chevronIcon, showExpiry && styles.chevronIconExpanded)}
							data-flx="user.status-change-bottom-sheet.status-item.chevron-icon"
						/>
					)}
				</div>
			</button>
			{showExpiry && (
				<div className={styles.expiryList} data-flx="user.status-change-bottom-sheet.status-item.expiry-list">
					{expiryOptions.map((option: StatusExpiryOption) => (
						<button
							key={option.id}
							type="button"
							className={styles.expiryItem}
							onClick={() => handleExpirySelect(option.durationMs)}
							data-flx="user.status-change-bottom-sheet.status-item.expiry-item.expiry-select.button"
						>
							{i18n._(option.label)}
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
	const currentUser = Users.getCurrentUser();
	const currentUserId = currentUser?.id ?? null;
	const existingCustomStatus = currentUserId ? Presence.getCustomStatus(currentUserId) : null;
	const normalizedExisting = normalizeCustomStatus(existingCustomStatus);
	const hasExistingStatus = Boolean(normalizedExisting);
	const [isSaving, setIsSaving] = useState(false);
	if (!hasExistingStatus || !normalizedExisting) {
		return null;
	}
	const handleClear = async () => {
		if (isSaving) return;
		setIsSaving(true);
		try {
			await UserSettingsCommands.update({customStatus: null});
		} finally {
			setIsSaving(false);
		}
	};
	return (
		<div
			className={styles.customStatusSection}
			data-flx="user.status-change-bottom-sheet.custom-status-section.custom-status-section"
		>
			<div
				className={styles.customStatusHeader}
				data-flx="user.status-change-bottom-sheet.custom-status-section.custom-status-header"
			>
				<span
					className={styles.customStatusTitle}
					data-flx="user.status-change-bottom-sheet.custom-status-section.custom-status-title"
				>
					<Trans>Custom status</Trans>
				</span>
			</div>
			<button
				type="button"
				className={styles.customStatusButton}
				onClick={onOpenEditor}
				data-flx="user.status-change-bottom-sheet.custom-status-section.custom-status-button.open-editor"
			>
				<CustomStatusDisplay
					customStatus={normalizedExisting}
					animateOnParentHover
					data-flx="user.status-change-bottom-sheet.custom-status-section.custom-status-display"
				/>
			</button>
			<button
				type="button"
				className={styles.clearCustomStatusButton}
				onClick={handleClear}
				disabled={isSaving}
				data-flx="user.status-change-bottom-sheet.custom-status-section.clear-custom-status-button"
			>
				<Trans>Clear custom status</Trans>
			</button>
		</div>
	);
});
export const StatusChangeBottomSheet = observer(({isOpen, onClose}: StatusChangeBottomSheetProps) => {
	const {i18n} = useLingui();
	const currentUser = Users.getCurrentUser();
	const currentUserId = currentUser?.id ?? null;
	const status = currentUserId ? Presence.getStatus(currentUserId) : StatusTypes.ONLINE;
	const [customStatusSheetOpen, setCustomStatusSheetOpen] = useState(false);
	const isDeveloper = DeveloperMode.isDeveloper;
	const statusExpiryOptions = useMemo(() => getStatusExpiryOptions(isDeveloper), [isDeveloper]);
	const handleStatusChange = useCallback(
		(statusType: StatusType, durationMs: number | null) => {
			StatusExpiry.setActiveStatusExpiry({
				status: statusType,
				durationMs,
			});
			onClose();
		},
		[onClose],
	);
	const handleOpenCustomStatusEditor = useCallback(() => {
		setCustomStatusSheetOpen(true);
	}, []);
	const handleCloseCustomStatusEditor = useCallback(() => {
		setCustomStatusSheetOpen(false);
	}, []);
	return (
		<>
			<BottomSheet
				isOpen={isOpen}
				onClose={onClose}
				snapPoints={STATUS_SHEET_SNAP_POINTS}
				initialSnap={STATUS_SHEET_SNAP_POINTS.length - 1}
				title={i18n._(SET_STATUS_DESCRIPTOR)}
				data-flx="user.status-change-bottom-sheet.bottom-sheet"
			>
				<div className={styles.content} data-flx="user.status-change-bottom-sheet.content">
					<div className={styles.topSpacer} data-flx="user.status-change-bottom-sheet.top-spacer" />
					<CustomStatusSection
						onOpenEditor={handleOpenCustomStatusEditor}
						data-flx="user.status-change-bottom-sheet.custom-status-section"
					/>
					<div className={styles.statusSection} data-flx="user.status-change-bottom-sheet.status-section">
						<div className={styles.sectionHeader} data-flx="user.status-change-bottom-sheet.section-header">
							<Trans>Online status</Trans>
						</div>
						<div className={styles.statusContainer} data-flx="user.status-change-bottom-sheet.status-container">
							{STATUS_ORDER.map((statusType, index, arr) => (
								<React.Fragment key={statusType}>
									<StatusItem
										status={statusType}
										currentStatus={status}
										expiryOptions={statusExpiryOptions}
										onSelect={handleStatusChange}
										data-flx="user.status-change-bottom-sheet.status-item.status-change"
									/>
									{index < arr.length - 1 && (
										<div className={styles.divider} data-flx="user.status-change-bottom-sheet.divider" />
									)}
								</React.Fragment>
							))}
						</div>
					</div>
				</div>
			</BottomSheet>
			<CustomStatusBottomSheet
				isOpen={customStatusSheetOpen}
				onClose={handleCloseCustomStatusEditor}
				data-flx="user.status-change-bottom-sheet.custom-status-bottom-sheet"
			/>
		</>
	);
});
