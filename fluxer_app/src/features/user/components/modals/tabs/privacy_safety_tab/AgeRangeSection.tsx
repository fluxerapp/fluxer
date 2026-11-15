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
	message: 'Age range from your Apple or Google account',
	comment:
		'Label in the sensitive content settings, shown when the Fluxer app for iPhone, iPad or Android has sent the age range that the Apple or Google account shares with apps.',
});
const AGE_RANGE_DESCRIPTION_DESCRIPTOR = msg({
	message:
		"The Fluxer app for iPhone, iPad or Android sent us this age range. If it shows that you're 18 or older, you can see 18+ content in the UK and Brazil. It doesn't change your date of birth.",
	comment: 'Helper text under "Age range from your Apple or Google account" in the sensitive content settings.',
});
const REMOVE_AGE_RANGE_DESCRIPTOR = msg({
	message: 'Remove age range',
	comment: 'Button in the sensitive content settings that opens a confirmation to delete the stored age range.',
});
const REMOVE_AGE_RANGE_TITLE_DESCRIPTOR = msg({
	message: 'Remove your age range?',
	comment: 'Title of the confirmation shown before deleting the age range from an Apple or Google account.',
});
const REMOVE_AGE_RANGE_BODY_DESCRIPTOR = msg({
	message:
		"We delete the age range, the time we got it and the App Attest keys for your Apple devices. In the UK and Brazil, you can't see 18+ content until the app sends an age range again. The app may send it again later if your Apple or Google account still shares it with Fluxer.",
	comment:
		'Body of the confirmation shown before deleting the age range. App Attest is an Apple product name and stays in English.',
});
const REMOVE_DESCRIPTOR = msg({
	message: 'Remove',
	comment: 'Confirm button in the dialog that deletes the stored age range.',
});
const REMOVE_AGE_RANGE_FAILED_DESCRIPTOR = msg({
	message: "We couldn't remove your age range. Please try again.",
	comment: 'Error shown when deleting the stored age range fails.',
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
