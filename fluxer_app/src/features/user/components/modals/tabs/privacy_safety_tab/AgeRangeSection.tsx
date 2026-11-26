// SPDX-License-Identifier: AGPL-3.0-or-later

import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as AccountTierCommands from '@app/features/user/commands/AccountTierCommands';
import styles from '@app/features/user/components/modals/tabs/privacy_safety_tab/SensitiveContentTab.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useCallback} from 'react';

const logger = new Logger('AgeRangeSection');

const AGE_RANGE_LABEL_DESCRIPTOR = msg({
	message: 'Mature content unlocked in the Fluxer app',
	comment:
		'Label in the sensitive content settings, shown when mature content was unlocked in the Fluxer app for iPhone, iPad or Android, where the Apple or Google account confirmed that the person is an adult.',
});
const AGE_RANGE_DESCRIPTION_DESCRIPTOR = msg({
	message:
		"You unlocked mature content in the Fluxer app for iPhone, iPad or Android, where your Apple or Google account confirmed that you're an adult. This lets you see 18+ content in the UK and Brazil. It doesn't change your date of birth.",
	comment: 'Helper text under "Mature content unlocked in the Fluxer app" in the sensitive content settings.',
});
const REMOVE_AGE_RANGE_DESCRIPTOR = msg({
	message: 'Remove app unlock',
	comment:
		'Button in the sensitive content settings that opens a confirmation to remove the mature content unlock that came from the Fluxer app for iPhone, iPad or Android.',
});
const REMOVE_AGE_RANGE_TITLE_DESCRIPTOR = msg({
	message: 'Remove the app unlock?',
	comment:
		'Title of the confirmation shown before removing the mature content unlock that came from the Fluxer app for iPhone, iPad or Android.',
});
const REMOVE_AGE_RANGE_BODY_DESCRIPTOR = msg({
	message:
		'We delete what your Apple or Google account shared, the time we got it and the App Attest keys for your Apple devices. If this is how you unlocked mature content, 18+ content in the UK and Brazil stays locked until you unlock it again in the app. The app may unlock it again later if your Apple or Google account still shares this with Fluxer.',
	comment:
		'Body of the confirmation shown before removing the mature content unlock from the Fluxer app for iPhone, iPad or Android. App Attest is an Apple product name and stays in English.',
});
const REMOVE_DESCRIPTOR = msg({
	message: 'Remove',
	comment: 'Confirm button in the dialog that removes the mature content unlock from the Fluxer app.',
});
const REMOVE_AGE_RANGE_FAILED_DESCRIPTOR = msg({
	message: "We couldn't remove the app unlock. Please try again.",
	comment: 'Error shown when removing the mature content unlock from the Fluxer app fails.',
});

export const AgeRangeSection: React.FC = () => {
	const {i18n} = useLingui();
	const handleRemove = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(REMOVE_AGE_RANGE_TITLE_DESCRIPTOR)}
					description={i18n._(REMOVE_AGE_RANGE_BODY_DESCRIPTOR)}
					primaryText={i18n._(REMOVE_DESCRIPTOR)}
					primaryVariant="danger"
					onPrimary={async () => {
						try {
							await AccountTierCommands.removeAgeRange();
						} catch (error) {
							logger.error('Failed to remove age range', error);
							showGenericErrorModal({
								message: () => i18n._(REMOVE_AGE_RANGE_FAILED_DESCRIPTOR),
								dataFlx: 'user.privacy-safety-tab.age-range-section.remove-error-modal',
							});
						}
					}}
					data-flx="user.privacy-safety-tab.age-range-section.remove-confirm-modal"
				/>
			)),
		);
	}, [i18n]);
	return (
		<div className={styles.row} data-flx="user.privacy-safety-tab.age-range-section.row">
			<div className={styles.text} data-flx="user.privacy-safety-tab.age-range-section.text">
				<span className={styles.label} data-flx="user.privacy-safety-tab.age-range-section.label">
					{i18n._(AGE_RANGE_LABEL_DESCRIPTOR)}
				</span>
				<span className={styles.description} data-flx="user.privacy-safety-tab.age-range-section.description">
					{i18n._(AGE_RANGE_DESCRIPTION_DESCRIPTOR)}
				</span>
			</div>
			<Button
				variant="secondary"
				small={true}
				onClick={handleRemove}
				data-flx="user.privacy-safety-tab.age-range-section.button.remove"
			>
				{i18n._(REMOVE_AGE_RANGE_DESCRIPTOR)}
			</Button>
		</div>
	);
};
