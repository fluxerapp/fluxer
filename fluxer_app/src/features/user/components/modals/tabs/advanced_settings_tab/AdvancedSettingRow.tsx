// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {
	AdvancedSettingControl,
	COMPACT_SWITCH_CONTROL_ITEM_IDS,
	DIRECT_CONTROL_ITEM_IDS,
	FULL_WIDTH_CONTROL_ITEM_IDS,
} from '@app/features/user/components/modals/tabs/AdvancedSettingDirectControls';
import styles from '@app/features/user/components/modals/tabs/AdvancedSettingsTab.module.css';
import {getAdvancedSettingSourceTab} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedSettingsItemUtils';
import type {SearchableSettingItem} from '@app/features/user/components/settings_utils/SettingsSectionRegistry';
import {SettingsItemStatusBadges} from '@app/features/user/components/settings_utils/SettingsStatusBadge';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowRightIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useRef} from 'react';

const OPEN_DESCRIPTOR = msg({
	message: 'Open',
	comment: 'Button label that opens the source settings section for an advanced setting.',
});

export const AdvancedSettingRow = observer(
	({
		item,
		userCreatedAt,
		onOpen,
	}: {
		item: SearchableSettingItem;
		userCreatedAt?: Date | null;
		onOpen: (item: SearchableSettingItem) => void;
	}) => {
		const {i18n} = useLingui();
		const hasDirectControl = DIRECT_CONTROL_ITEM_IDS.has(item.id);
		const hasFullWidthControl = FULL_WIDTH_CONTROL_ITEM_IDS.has(item.id);
		const canToggleFromMain = COMPACT_SWITCH_CONTROL_ITEM_IDS.has(item.id);
		const canOpen = getAdvancedSettingSourceTab(item) !== 'advanced_settings';
		const rowRef = useRef<HTMLDivElement>(null);
		const handleMainClick = useCallback(() => {
			if (!canToggleFromMain) return;
			const switchButton = rowRef.current?.querySelector<HTMLButtonElement>('button[role="switch"]:not(:disabled)');
			switchButton?.click();
			switchButton?.focus();
		}, [canToggleFromMain]);
		const MainComponent = canToggleFromMain ? 'button' : 'div';
		return (
			<div
				ref={rowRef}
				className={clsx(styles.settingRow, hasFullWidthControl && styles.settingRowFullWidth)}
				data-flx="user.advanced-settings-tab.setting-row"
			>
				<MainComponent
					{...(canToggleFromMain ? {type: 'button', onClick: handleMainClick} : {})}
					className={clsx(styles.settingMain, canToggleFromMain && styles.settingMainInteractive)}
					data-flx="user.advanced-settings-tab.setting-main"
				>
					<div className={styles.settingTitleRow} data-flx="user.advanced-settings-tab.setting-title-row">
						<span className={styles.settingTitle} data-flx="user.advanced-settings-tab.setting-title">
							{item.label}
						</span>
						<SettingsItemStatusBadges
							item={item}
							userCreatedAt={userCreatedAt}
							data-flx="user.advanced-settings-tab.setting-badges"
						/>
					</div>
					{item.description && (
						<p className={styles.settingDescription} data-flx="user.advanced-settings-tab.setting-description">
							{item.description}
						</p>
					)}
				</MainComponent>
				<div
					className={clsx(styles.settingAction, hasFullWidthControl && styles.settingActionFullWidth)}
					data-flx="user.advanced-settings-tab.setting-action"
				>
					{hasDirectControl ? (
						<AdvancedSettingControl
							item={item}
							data-flx="user.advanced-settings-tab.advanced-setting-row.advanced-setting-control"
						/>
					) : canOpen ? (
						<Button
							variant="secondary"
							rightIcon={<ArrowRightIcon size={remFromPx(14)} data-flx="user.advanced-settings-tab.arrow-right-icon" />}
							onClick={() => onOpen(item)}
							data-flx="user.advanced-settings-tab.button.open-source"
						>
							{i18n._(OPEN_DESCRIPTOR)}
						</Button>
					) : null}
				</div>
			</div>
		);
	},
);
