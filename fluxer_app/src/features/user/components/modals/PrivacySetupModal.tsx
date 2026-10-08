// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import Guilds from '@app/features/guild/state/Guilds';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {RadioGroup, type RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import styles from '@app/features/user/components/modals/PrivacySetupModal.module.css';
import {PRIVACY_SETUP_VERSION} from '@app/features/user/constants/PrivacySetupConstants';
import UserSettings from '@app/features/user/state/UserSettings';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useId, useMemo, useRef, useState} from 'react';

type CommunityDirectMessages = 'open' | 'friends';

const PRIVACY_SETUP_TITLE_DESCRIPTOR = msg({
	message: 'Who can message you?',
	comment: 'Title of the privacy setup modal that asks who can send the user direct messages.',
});
const PRIVACY_SETUP_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Friends can always send you direct messages. Choose whether people who share a community with you can message you without sending a friend request first.',
	comment: 'Explanation at the top of the privacy setup modal.',
});
const PRIVACY_SETUP_OPEN_DESCRIPTOR = msg({
	message: 'Anyone in my communities',
	comment: 'Privacy setup option. People who share a community with the user can send them direct messages.',
});
const PRIVACY_SETUP_OPEN_DESC_DESCRIPTOR = msg({
	message: 'People who share a community with you can message you directly.',
	comment: 'Description under the "Anyone in my communities" privacy setup option.',
});
const PRIVACY_SETUP_FRIENDS_DESCRIPTOR = msg({
	message: 'Friends only',
	comment: 'Privacy setup option. Only friends can send the user direct messages.',
});
const PRIVACY_SETUP_FRIENDS_DESC_DESCRIPTOR = msg({
	message: 'People need to be your friend before they can message you. They can still send you a friend request.',
	comment: 'Description under the "Friends only" privacy setup option.',
});
const PRIVACY_SETUP_FOOTNOTE_DESCRIPTOR = msg({
	message: 'You can change this anytime in your privacy settings, including for each community.',
	comment: 'Small note at the bottom of the privacy setup modal.',
});
const PRIVACY_SETUP_SAVE_DESCRIPTOR = msg({
	message: 'Save',
	comment: 'Primary button in the privacy setup modal. Saves the chosen direct message setting.',
});
const PRIVACY_SETUP_OUTLIERS_DESCRIPTOR = msg({
	message: '{count, plural, one {# community uses a different setting} other {# communities use a different setting}}',
	comment:
		'Heading above the list of communities whose direct message setting differs from the choice in the privacy setup modal.',
});
const PRIVACY_SETUP_APPLY_TO_OUTLIERS_DESCRIPTOR = msg({
	message: 'Apply to these communities too',
	comment:
		'Checkbox in the privacy setup modal. When checked, the chosen setting also replaces the setting of the listed communities.',
});
const PRIVACY_SETUP_OPTIONS_LABEL_DESCRIPTOR = msg({
	message: 'Who can message you',
	comment: 'Accessible label for the group of options in the privacy setup modal.',
});

export const PrivacySetupModal = observer(() => {
	const {i18n} = useLingui();
	const currentDefaultRestricted = UserSettings.getDefaultGuildsRestricted();
	const currentRestrictedGuilds = UserSettings.restrictedGuilds;
	const currentChoice: CommunityDirectMessages = currentDefaultRestricted ? 'friends' : 'open';
	const [choice, setChoice] = useState<CommunityDirectMessages>(currentChoice);
	const [applyToOutliers, setApplyToOutliers] = useState(true);
	const [submitting, setSubmitting] = useState(false);
	const primaryRef = useRef<HTMLButtonElement | null>(null);
	const outliersTitleId = useId();
	const guilds = Guilds.getGuilds();
	const restrictedSet = useMemo(() => new Set(currentRestrictedGuilds), [currentRestrictedGuilds]);
	const outliers = guilds.filter((guild) => restrictedSet.has(guild.id) === (choice === 'open'));
	const options = useMemo<ReadonlyArray<RadioOption<CommunityDirectMessages>>>(
		() => [
			{
				value: 'open',
				name: i18n._(PRIVACY_SETUP_OPEN_DESCRIPTOR),
				desc: i18n._(PRIVACY_SETUP_OPEN_DESC_DESCRIPTOR),
			},
			{
				value: 'friends',
				name: i18n._(PRIVACY_SETUP_FRIENDS_DESCRIPTOR),
				desc: i18n._(PRIVACY_SETUP_FRIENDS_DESC_DESCRIPTOR),
			},
		],
		[i18n],
	);
	const handleClose = useCallback(() => {
		ModalCommands.pop();
	}, []);
	const handleSave = useCallback(async () => {
		const nextDefaultRestricted = choice === 'friends';
		const nextRestrictedGuilds =
			applyToOutliers && outliers.length > 0
				? nextDefaultRestricted
					? guilds.map((guild) => guild.id)
					: []
				: currentRestrictedGuilds;
		const restrictedChanged =
			nextRestrictedGuilds.length !== currentRestrictedGuilds.length ||
			nextRestrictedGuilds.some((id) => !restrictedSet.has(id));
		setSubmitting(true);
		try {
			if (nextDefaultRestricted === currentDefaultRestricted && !restrictedChanged) {
				await UserSettingsCommands.update({privacySetupVersion: PRIVACY_SETUP_VERSION});
			} else {
				await UserSettingsCommands.update({
					defaultGuildsRestricted: nextDefaultRestricted,
					restrictedGuilds: [...nextRestrictedGuilds],
					privacySetupVersion: PRIVACY_SETUP_VERSION,
				});
			}
			ModalCommands.pop();
		} finally {
			setSubmitting(false);
		}
	}, [
		applyToOutliers,
		choice,
		currentDefaultRestricted,
		currentRestrictedGuilds,
		guilds,
		outliers.length,
		restrictedSet,
	]);
	return (
		<Modal.Root
			size="small"
			initialFocusRef={primaryRef}
			centered
			onClose={handleClose}
			data-flx="user.privacy-setup-modal.modal-root"
		>
			<Modal.Header
				title={i18n._(PRIVACY_SETUP_TITLE_DESCRIPTOR)}
				onClose={handleClose}
				data-flx="user.privacy-setup-modal.modal-header"
			/>
			<Modal.Content data-flx="user.privacy-setup-modal.modal-content">
				<Modal.ContentLayout data-flx="user.privacy-setup-modal.modal-content-layout">
					<Modal.Description data-flx="user.privacy-setup-modal.modal-description">
						{i18n._(PRIVACY_SETUP_DESCRIPTION_DESCRIPTOR)}
					</Modal.Description>
					<RadioGroup
						options={options}
						value={choice}
						onChange={setChoice}
						aria-label={i18n._(PRIVACY_SETUP_OPTIONS_LABEL_DESCRIPTOR)}
					/>
					{outliers.length > 0 && (
						<section
							className={styles.outliers}
							aria-labelledby={outliersTitleId}
							data-flx="user.privacy-setup-modal.outliers"
						>
							<h3
								id={outliersTitleId}
								className={styles.outliersTitle}
								data-flx="user.privacy-setup-modal.outliers-title"
							>
								{i18n._(PRIVACY_SETUP_OUTLIERS_DESCRIPTOR, {count: outliers.length})}
							</h3>
							<ul className={styles.outlierList} data-flx="user.privacy-setup-modal.outlier-list">
								{outliers.map((guild) => (
									<li key={guild.id} className={styles.outlierItem} data-flx="user.privacy-setup-modal.outlier-item">
										<GuildIcon
											id={guild.id}
											name={guild.name}
											icon={guild.icon}
											sizePx={20}
											data-flx="user.privacy-setup-modal.outlier-icon"
										/>
										<span className={styles.outlierName} data-flx="user.privacy-setup-modal.outlier-name">
											{guild.name}
										</span>
									</li>
								))}
							</ul>
							<Checkbox
								checked={applyToOutliers}
								onChange={setApplyToOutliers}
								size="small"
								data-flx="user.privacy-setup-modal.checkbox.apply-to-outliers"
							>
								<span className={styles.applyLabel} data-flx="user.privacy-setup-modal.apply-label">
									{i18n._(PRIVACY_SETUP_APPLY_TO_OUTLIERS_DESCRIPTOR)}
								</span>
							</Checkbox>
						</section>
					)}
					<Modal.Description data-flx="user.privacy-setup-modal.modal-description--footnote">
						{i18n._(PRIVACY_SETUP_FOOTNOTE_DESCRIPTOR)}
					</Modal.Description>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="user.privacy-setup-modal.modal-footer">
				<Button onClick={handleClose} variant="secondary" data-flx="user.privacy-setup-modal.button.cancel">
					{i18n._(CANCEL_DESCRIPTOR)}
				</Button>
				<Button
					onClick={handleSave}
					submitting={submitting}
					variant="primary"
					ref={primaryRef}
					data-flx="user.privacy-setup-modal.button.save"
				>
					{i18n._(PRIVACY_SETUP_SAVE_DESCRIPTOR)}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
