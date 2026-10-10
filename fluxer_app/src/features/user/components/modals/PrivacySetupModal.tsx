// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import type {Guild} from '@app/features/guild/models/Guild';
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
const PRIVACY_SETUP_INTRO_DESCRIPTOR = msg({
	message: 'Take a moment to check who can message you. Nothing changes unless you choose to.',
	comment: 'Explanation at the top of the privacy setup modal.',
});
const PRIVACY_SETUP_CURRENT_HEADING_DESCRIPTOR = msg({
	message: 'Right now',
	comment: 'Heading of the box in the privacy setup modal that describes the current direct message setting.',
});
const PRIVACY_SETUP_CURRENT_OPEN_DESCRIPTOR = msg({
	message: 'People in your communities can message you without being friends.',
	comment: 'Describes the current setting in the privacy setup modal when community members can send direct messages.',
});
const PRIVACY_SETUP_CURRENT_FRIENDS_DESCRIPTOR = msg({
	message: 'People in your communities need to be your friend before they can message you.',
	comment: 'Describes the current setting in the privacy setup modal when only friends can send direct messages.',
});
const PRIVACY_SETUP_FRIENDS_ALWAYS_DESCRIPTOR = msg({
	message: 'Friends can always message you.',
	comment: 'Reassurance in the privacy setup modal that friends can always send direct messages.',
});
const PRIVACY_SETUP_OWN_SETTING_DESCRIPTOR = msg({
	message: '{count, plural, one {# community has its own setting} other {# communities have their own setting}}',
	comment:
		'Heading above the list of communities whose direct message setting differs from the default, in the privacy setup modal.',
});
const PRIVACY_SETUP_STATE_OPEN_DESCRIPTOR = msg({
	message: 'Can message you',
	comment: 'Shown next to a community in the privacy setup modal. Its members can send the user direct messages.',
});
const PRIVACY_SETUP_STATE_FRIENDS_DESCRIPTOR = msg({
	message: 'Friends only',
	comment:
		'Shown next to a community in the privacy setup modal. Only friends from it can send the user direct messages.',
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
const PRIVACY_SETUP_CURRENT_TAG_DESCRIPTOR = msg({
	message: 'Current',
	comment: 'Small tag next to the privacy setup option that matches the current setting.',
});
const PRIVACY_SETUP_OPTIONS_LABEL_DESCRIPTOR = msg({
	message: 'Who can message you',
	comment: 'Accessible label for the group of options in the privacy setup modal.',
});
const PRIVACY_SETUP_APPLY_DESCRIPTOR = msg({
	message: 'Also change the communities below to match',
	comment:
		'Unchecked checkbox in the privacy setup modal. When checked, the new setting also replaces the setting of the listed communities.',
});
const PRIVACY_SETUP_SUMMARY_NONE_DESCRIPTOR = msg({
	message: 'Nothing will change. This just confirms your settings.',
	comment: 'Summary in the privacy setup modal when the user keeps the current setting.',
});
const PRIVACY_SETUP_SUMMARY_FUTURE_DESCRIPTOR = msg({
	message: 'Communities you join from now on will use this. Your current communities keep their settings.',
	comment:
		'Summary in the privacy setup modal when the user changes the setting without changing their current communities.',
});
const PRIVACY_SETUP_SUMMARY_FUTURE_ONLY_DESCRIPTOR = msg({
	message: 'Communities you join from now on will use this.',
	comment: 'Summary in the privacy setup modal when the user changes the setting and no current community is affected.',
});
const PRIVACY_SETUP_SUMMARY_APPLY_DESCRIPTOR = msg({
	message: 'Communities you join from now on will use this, and the communities below change to match.',
	comment: 'Summary in the privacy setup modal when the new setting also applies to the listed current communities.',
});
const PRIVACY_SETUP_FOOTNOTE_DESCRIPTOR = msg({
	message: 'You can change this anytime in your privacy settings, including for each community.',
	comment: 'Small note at the bottom of the privacy setup modal.',
});
const PRIVACY_SETUP_KEEP_DESCRIPTOR = msg({
	message: 'Keep my current settings',
	comment: 'Primary button in the privacy setup modal when nothing is changed. Confirms the current settings.',
});
const PRIVACY_SETUP_SAVE_CHANGES_DESCRIPTOR = msg({
	message: 'Save changes',
	comment: 'Primary button in the privacy setup modal after the user picks a different setting.',
});

function isRestricted(restricted: ReadonlySet<string>, guild: Guild): boolean {
	return restricted.has(guild.id);
}

export const PrivacySetupModal = observer(() => {
	const {i18n} = useLingui();
	const currentDefaultRestricted = UserSettings.getDefaultGuildsRestricted();
	const currentRestrictedGuilds = UserSettings.restrictedGuilds;
	const currentChoice: CommunityDirectMessages = currentDefaultRestricted ? 'friends' : 'open';
	const [choice, setChoice] = useState<CommunityDirectMessages>(currentChoice);
	const [applyToCurrent, setApplyToCurrent] = useState(false);
	const [submitting, setSubmitting] = useState(false);
	const primaryRef = useRef<HTMLButtonElement | null>(null);
	const currentHeadingId = useId();
	const overridesHeadingId = useId();
	const guilds = Guilds.getGuilds();
	const restrictedSet = useMemo(() => new Set(currentRestrictedGuilds), [currentRestrictedGuilds]);
	const overrides = guilds.filter((guild) => isRestricted(restrictedSet, guild) !== currentDefaultRestricted);
	const changed = choice !== currentChoice;
	const nextDefaultRestricted = choice === 'friends';
	const affected = changed
		? guilds.filter((guild) => isRestricted(restrictedSet, guild) !== nextDefaultRestricted)
		: [];
	const applying = changed && applyToCurrent && affected.length > 0;
	const currentTag = (
		<span className={styles.currentTag} data-flx="user.privacy-setup-modal.current-tag">
			{i18n._(PRIVACY_SETUP_CURRENT_TAG_DESCRIPTOR)}
		</span>
	);
	const options = useMemo<ReadonlyArray<RadioOption<CommunityDirectMessages>>>(
		() => [
			{
				value: 'open',
				name: (
					<span className={styles.optionName} data-flx="user.privacy-setup-modal.option-name--open">
						{i18n._(PRIVACY_SETUP_OPEN_DESCRIPTOR)}
						{currentChoice === 'open' && currentTag}
					</span>
				),
				desc: i18n._(PRIVACY_SETUP_OPEN_DESC_DESCRIPTOR),
			},
			{
				value: 'friends',
				name: (
					<span className={styles.optionName} data-flx="user.privacy-setup-modal.option-name--friends">
						{i18n._(PRIVACY_SETUP_FRIENDS_DESCRIPTOR)}
						{currentChoice === 'friends' && currentTag}
					</span>
				),
				desc: i18n._(PRIVACY_SETUP_FRIENDS_DESC_DESCRIPTOR),
			},
		],
		[i18n, currentChoice, currentTag],
	);
	const summary = !changed
		? i18n._(PRIVACY_SETUP_SUMMARY_NONE_DESCRIPTOR)
		: applying
			? i18n._(PRIVACY_SETUP_SUMMARY_APPLY_DESCRIPTOR)
			: affected.length > 0
				? i18n._(PRIVACY_SETUP_SUMMARY_FUTURE_DESCRIPTOR)
				: i18n._(PRIVACY_SETUP_SUMMARY_FUTURE_ONLY_DESCRIPTOR);
	const handleClose = useCallback(() => {
		ModalCommands.pop();
	}, []);
	const handleSave = useCallback(async () => {
		setSubmitting(true);
		try {
			if (!changed) {
				await UserSettingsCommands.update({privacySetupVersion: PRIVACY_SETUP_VERSION});
			} else if (applying) {
				await UserSettingsCommands.update({
					defaultGuildsRestricted: nextDefaultRestricted,
					restrictedGuilds: nextDefaultRestricted ? guilds.map((guild) => guild.id) : [],
					privacySetupVersion: PRIVACY_SETUP_VERSION,
				});
			} else {
				await UserSettingsCommands.update({
					defaultGuildsRestricted: nextDefaultRestricted,
					privacySetupVersion: PRIVACY_SETUP_VERSION,
				});
			}
			ModalCommands.pop();
		} finally {
			setSubmitting(false);
		}
	}, [applying, changed, guilds, nextDefaultRestricted]);
	const renderGuildRow = (guild: Guild) => (
		<li key={guild.id} className={styles.guildItem} data-flx="user.privacy-setup-modal.guild-item">
			<GuildIcon
				id={guild.id}
				name={guild.name}
				icon={guild.icon}
				sizePx={20}
				data-flx="user.privacy-setup-modal.guild-icon"
			/>
			<span className={styles.guildName} data-flx="user.privacy-setup-modal.guild-name">
				{guild.name}
			</span>
			<span className={styles.guildState} data-flx="user.privacy-setup-modal.guild-state">
				{isRestricted(restrictedSet, guild)
					? i18n._(PRIVACY_SETUP_STATE_FRIENDS_DESCRIPTOR)
					: i18n._(PRIVACY_SETUP_STATE_OPEN_DESCRIPTOR)}
			</span>
		</li>
	);
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
						{i18n._(PRIVACY_SETUP_INTRO_DESCRIPTOR)}
					</Modal.Description>
					<section
						className={styles.current}
						aria-labelledby={currentHeadingId}
						data-flx="user.privacy-setup-modal.current"
					>
						<h3 id={currentHeadingId} className={styles.sectionTitle} data-flx="user.privacy-setup-modal.current-title">
							{i18n._(PRIVACY_SETUP_CURRENT_HEADING_DESCRIPTOR)}
						</h3>
						<p className={styles.currentText} data-flx="user.privacy-setup-modal.current-text">
							<span data-flx="user.privacy-setup-modal.current-state">
								{currentDefaultRestricted
									? i18n._(PRIVACY_SETUP_CURRENT_FRIENDS_DESCRIPTOR)
									: i18n._(PRIVACY_SETUP_CURRENT_OPEN_DESCRIPTOR)}
							</span>{' '}
							<span data-flx="user.privacy-setup-modal.friends-always">
								{i18n._(PRIVACY_SETUP_FRIENDS_ALWAYS_DESCRIPTOR)}
							</span>
						</p>
						{overrides.length > 0 && (
							<div className={styles.overrides} data-flx="user.privacy-setup-modal.overrides">
								<h4
									id={overridesHeadingId}
									className={styles.overridesTitle}
									data-flx="user.privacy-setup-modal.overrides-title"
								>
									{i18n._(PRIVACY_SETUP_OWN_SETTING_DESCRIPTOR, {count: overrides.length})}
								</h4>
								<ul
									className={styles.guildList}
									aria-labelledby={overridesHeadingId}
									data-flx="user.privacy-setup-modal.overrides-list"
								>
									{overrides.map(renderGuildRow)}
								</ul>
							</div>
						)}
					</section>
					<RadioGroup
						options={options}
						value={choice}
						onChange={setChoice}
						aria-label={i18n._(PRIVACY_SETUP_OPTIONS_LABEL_DESCRIPTOR)}
						data-flx="user.privacy-setup-modal.radio-group.set-choice"
					/>
					{changed && affected.length > 0 && (
						<section className={styles.apply} data-flx="user.privacy-setup-modal.apply">
							<Checkbox
								checked={applyToCurrent}
								onChange={setApplyToCurrent}
								size="small"
								data-flx="user.privacy-setup-modal.checkbox.apply-to-current"
							>
								<span className={styles.applyLabel} data-flx="user.privacy-setup-modal.apply-label">
									{i18n._(PRIVACY_SETUP_APPLY_DESCRIPTOR)}
								</span>
							</Checkbox>
							<ul className={styles.guildList} data-flx="user.privacy-setup-modal.affected-list">
								{affected.map(renderGuildRow)}
							</ul>
						</section>
					)}
					<p className={styles.summary} aria-live="polite" data-flx="user.privacy-setup-modal.summary">
						{summary}
					</p>
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
					{changed ? i18n._(PRIVACY_SETUP_SAVE_CHANGES_DESCRIPTOR) : i18n._(PRIVACY_SETUP_KEEP_DESCRIPTOR)}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
