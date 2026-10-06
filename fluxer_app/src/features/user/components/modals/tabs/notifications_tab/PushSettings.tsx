// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {Combobox as FormCombobox} from '@app/features/ui/components/form/FormCombobox';
import styles from '@app/features/user/components/modals/tabs/notifications_tab/PushSettings.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useId, useMemo} from 'react';

const PUSH_NOTIFICATION_INACTIVE_TIMEOUT_DESCRIPTOR = msg({
	message: 'Push notification inactive timeout',
	comment: 'Label in the push settings.',
});
const AVOIDS_SENDING_PUSH_NOTIFICATIONS_TO_YOUR_MOBILE_DEVICES_DESCRIPTOR = msg({
	message:
		'{productName} avoids sending push notifications to your mobile devices when you are at your computer. Choose how long you need to be inactive on desktop before you receive push notifications.',
	comment: 'Label in the push settings.',
});
const ONE_MINUTE_DESCRIPTOR = msg({
	message: '{oneMinute} minute',
	comment: 'Push notification inactive timeout option. The value is a duration in minutes.',
});
const MINUTES_DESCRIPTOR = msg({
	message: '{minutes, plural, one {# minute} other {# minutes}}',
	comment: 'Push notification inactive timeout option. The value is a duration in minutes.',
});

interface PushSettingsProps {
	afkTimeout: number;
	onAfkTimeoutChange: (value: number) => void;
	showHeader?: boolean;
}

export const PushSettings: React.FC<PushSettingsProps> = observer(
	({afkTimeout, onAfkTimeoutChange, showHeader = true}) => {
		const {i18n} = useLingui();
		const selectId = useId();
		const timeoutOptions = useMemo<Array<{value: number; label: string}>>(() => {
			const oneMinute = 1;
			return Array.from({length: 10}, (_, index) => {
				const minutes = index + 1;
				return {
					value: minutes,
					label:
						minutes === oneMinute ? i18n._(ONE_MINUTE_DESCRIPTOR, {oneMinute}) : i18n._(MINUTES_DESCRIPTOR, {minutes}),
				};
			});
		}, [i18n.locale]);
		const timeoutMinutes = Math.max(1, Math.min(10, Math.round(afkTimeout / 60)));
		return (
			<div
				className={showHeader ? styles.container : styles.selectOnlyContainer}
				data-flx="user.notifications-tab.push-settings.container"
			>
				{showHeader && (
					<div className={styles.text} data-flx="user.notifications-tab.push-settings.text">
						<label htmlFor={selectId} className={styles.title} data-flx="user.notifications-tab.push-settings.title">
							{i18n._(PUSH_NOTIFICATION_INACTIVE_TIMEOUT_DESCRIPTOR)}
						</label>
						<p className={styles.description} data-flx="user.notifications-tab.push-settings.description">
							{i18n._(AVOIDS_SENDING_PUSH_NOTIFICATIONS_TO_YOUR_MOBILE_DEVICES_DESCRIPTOR, {productName: PRODUCT_NAME})}
						</p>
					</div>
				)}
				<div className={styles.comboboxWrap} data-flx="user.notifications-tab.push-settings.select-wrap">
					<FormCombobox<number>
						id={selectId}
						value={timeoutMinutes}
						options={timeoutOptions}
						onChange={onAfkTimeoutChange}
						className={styles.combobox}
						density="compact"
						isSearchable={false}
						menuMinWidth={128}
						data-flx="user.notifications-tab.push-settings.form-select"
					/>
				</div>
			</div>
		);
	},
);
