// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {SettingsTabSection} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {
	HOURS_AND_MINUTES_DURATION_DESCRIPTOR,
	HOURS_DURATION_PLURAL_DESCRIPTOR,
	MINUTES_DURATION_PLURAL_DESCRIPTOR,
	SECONDS_DURATION_PLURAL_DESCRIPTOR,
	TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import styles from '@app/features/user/components/modals/tabs/privacy_safety_tab/ActiveNowTab.module.css';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import {showUserErrorModalAfterAutoDismiss} from '@app/features/user/utils/UserErrorModalUtils';
import {VOICE_ACTIVITY_SHARING_COOLDOWN_MS} from '@fluxer/constants/src/UserConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useState} from 'react';

const TITLE_DESCRIPTOR = msg({
	message: 'Share your voice activity with friends',
	comment: 'Privacy > Active now: label for the switch controlling whether friends see voice activity.',
});
const CONFIRM_TITLE_ENABLE_DESCRIPTOR = msg({
	message: 'Share voice activity with all friends?',
	comment: 'Privacy > Active now: confirmation modal title when turning sharing on.',
});
const CONFIRM_TITLE_DISABLE_DESCRIPTOR = msg({
	message: 'Stop sharing voice activity with all friends?',
	comment: 'Privacy > Active now: confirmation modal title when turning sharing off.',
});
const CONFIRM_DESCRIPTION_ENABLE_DESCRIPTOR = msg({
	message:
		"You're about to start sharing your voice activity with every friend you have, including future ones. This sends an update to all of them and can only be changed again in 24 hours.",
	comment: 'Privacy > Active now: confirmation modal body when turning sharing on.',
});
const CONFIRM_DESCRIPTION_DISABLE_DESCRIPTOR = msg({
	message:
		"You're about to stop sharing your voice activity with every friend you have, including future ones. This sends an update to all of them and can only be changed again in 24 hours.",
	comment: 'Privacy > Active now: confirmation modal body when turning sharing off.',
});
const CONFIRM_PRIMARY_ENABLE_DESCRIPTOR = msg({
	message: 'Yes, share with all friends',
	comment: 'Privacy > Active now: confirm button label when turning sharing on.',
});
const CONFIRM_PRIMARY_DISABLE_DESCRIPTOR = msg({
	message: 'Yes, stop sharing',
	comment: 'Privacy > Active now: confirm button label when turning sharing off.',
});
const COOLDOWN_HELPER_DESCRIPTOR = msg({
	message: 'Available again in {time}',
	comment:
		'Privacy > Active now: helper text under the disabled switch. {time} is a localized duration such as "23 hours and 45 minutes" or "12 minutes".',
});
const TOAST_SUCCESS_DESCRIPTOR = msg({
	message: 'Voice activity sharing updated',
	comment: 'Privacy > Active now: toast shown after a successful mass-update.',
});
const TOAST_ERROR_DESCRIPTOR = msg({
	message: "Couldn't update voice activity sharing right now",
	comment: 'Privacy > Active now: error modal title shown when the mass-update request fails.',
});
const SHARE_DESCRIPTION_DESCRIPTOR = msg({
	message:
		"Friends see which voice channel you're in on their Active now list. You can change this once every 24 hours.",
	comment: 'Privacy > Active now: helper text under the switch controlling whether friends see voice activity.',
});

function formatRemaining(i18n: I18n, ms: number): string {
	const totalSeconds = Math.max(0, Math.ceil(ms / 1000));
	const hours = Math.floor(totalSeconds / 3600);
	const minutes = Math.floor((totalSeconds % 3600) / 60);
	if (hours > 0) {
		if (minutes > 0) {
			return i18n._(HOURS_AND_MINUTES_DURATION_DESCRIPTOR, {hours, minutes});
		}
		return i18n._(HOURS_DURATION_PLURAL_DESCRIPTOR, {hours});
	}
	if (minutes > 0) {
		return i18n._(MINUTES_DURATION_PLURAL_DESCRIPTOR, {minutes});
	}
	return i18n._(SECONDS_DURATION_PLURAL_DESCRIPTOR, {seconds: totalSeconds});
}

function useCooldownRemainingMs(endsAt: Date | null): number {
	const [now, setNow] = useState(() => Date.now());
	useEffect(() => {
		if (endsAt == null) return;
		const interval = window.setInterval(() => setNow(Date.now()), 30000);
		return () => window.clearInterval(interval);
	}, [endsAt]);
	if (endsAt == null) return 0;
	return Math.max(0, endsAt.getTime() - now);
}

export const ActiveNowTabContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const [submitting, setSubmitting] = useState(false);
	const sharing = UserSettings.getDefaultShareVoiceActivity();
	const lastChange = Users.getCurrentUser()?.lastVoiceActivitySharingChangeAt ?? null;
	const cooldownEndsAt =
		lastChange != null ? new Date(lastChange.getTime() + VOICE_ACTIVITY_SHARING_COOLDOWN_MS) : null;
	const remainingMs = useCooldownRemainingMs(cooldownEndsAt);
	const onCooldown = remainingMs > 0;
	const openConfirm = (nextValue: boolean) => {
		const titleDescriptor = nextValue ? CONFIRM_TITLE_ENABLE_DESCRIPTOR : CONFIRM_TITLE_DISABLE_DESCRIPTOR;
		const descriptionDescriptor = nextValue
			? CONFIRM_DESCRIPTION_ENABLE_DESCRIPTOR
			: CONFIRM_DESCRIPTION_DISABLE_DESCRIPTOR;
		const primaryDescriptor = nextValue ? CONFIRM_PRIMARY_ENABLE_DESCRIPTOR : CONFIRM_PRIMARY_DISABLE_DESCRIPTOR;
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(titleDescriptor)}
					description={i18n._(descriptionDescriptor)}
					primaryText={i18n._(primaryDescriptor)}
					primaryVariant={nextValue ? 'primary' : 'danger'}
					onPrimary={async () => {
						setSubmitting(true);
						try {
							await UserSettingsCommands.updateVoiceActivitySharingDefault(nextValue);
							ToastCommands.createToast({type: 'success', children: i18n._(TOAST_SUCCESS_DESCRIPTOR)});
						} catch {
							showUserErrorModalAfterAutoDismiss(
								i18n._(TOAST_ERROR_DESCRIPTOR),
								i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
							);
						} finally {
							setSubmitting(false);
						}
					}}
					data-flx="user.privacy-safety-tab.active-now-tab.open-confirm.confirm-modal"
				/>
			)),
		);
	};
	return (
		<SettingsTabSection data-flx="user.privacy-safety-tab.active-now-tab.settings-tab-section">
			<Switch
				label={i18n._(TITLE_DESCRIPTOR)}
				description={i18n._(SHARE_DESCRIPTION_DESCRIPTOR)}
				value={sharing}
				disabled={onCooldown || submitting}
				onChange={(value) => openConfirm(value)}
				data-flx="user.privacy-safety-tab.active-now-tab.switch.share-voice-activity"
			/>
			{onCooldown && (
				<p className={styles.cooldownHint} data-flx="user.privacy-safety-tab.active-now-tab.cooldown-hint">
					{i18n._(COOLDOWN_HELPER_DESCRIPTOR, {time: formatRemaining(i18n, remainingMs)})}
				</p>
			)}
		</SettingsTabSection>
	);
});
