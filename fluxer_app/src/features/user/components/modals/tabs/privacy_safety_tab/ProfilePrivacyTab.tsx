// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsTabSection} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {PRODUCT_LABS_COMMUNITY_NAME} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import type {User} from '@app/features/user/models/User';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import {
	PRIVACY_SETUP_STATE_FRIENDS_DESCRIPTOR as FRIENDS_ONLY_OPTION_DESCRIPTOR,
	OPERATORS_MEMBERS_SEE_YOU_DESCRIPTOR,
	PROFILE_PRIVACY_LABEL_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import {type ProfilePrivacyLevel, ProfilePrivacyLevels, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const WHO_CAN_SEE_YOUR_FULL_PROFILE_DESCRIPTOR = msg({
	message: 'Who can see your full profile',
	comment: 'Privacy > Profile privacy: section title.',
});
const ALL_COMMUNITIES_OPTION_DESCRIPTOR = msg({
	message: 'Friends and all communities',
	comment: 'Privacy > Profile privacy: radio option label allowing every community member to see the profile.',
});
const ALL_COMMUNITIES_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Your full profile is visible to friends and to anyone in your communities',
	comment: 'Privacy > Profile privacy: helper text under the Friends and all communities option.',
});
const SMALL_COMMUNITIES_OPTION_DESCRIPTOR = msg({
	message: 'Friends and small communities only',
	comment: 'Privacy > Profile privacy: radio option label allowing only friends and members of small communities.',
});
const SMALL_COMMUNITIES_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Your full profile is visible to friends and members of your communities with 200 or fewer members',
	comment: 'Privacy > Profile privacy: helper text under the Small communities option.',
});

const FRIENDS_ONLY_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Your full profile is only visible to your friends',
	comment: 'Privacy > Profile privacy: helper text under the Friends only option.',
});
const LABS_ROLE_NOTE_DESCRIPTOR = msg({
	message:
		'With any choice other than "Friends and all communities", {labsName} shows you without the Operator and Visionary roles, and you keep #operators.',
	comment:
		'Privacy > Profile privacy: note under the section title, shown to people with an Operator Pass or Visionary. "Friends and all communities" is the label of the first option, translate it the same way. {labsName} is the name of the official community, such as Fluxer Labs. Operator and Visionary are role names and #operators is a channel name, keep them as is.',
});

function hasLabsRole(user: Pick<User, 'operatorPassSince' | 'premiumType'> | null): boolean {
	return (
		!RuntimeConfig.isSelfHosted() &&
		user !== null &&
		(user.operatorPassSince != null || user.premiumType === UserPremiumTypes.LIFETIME)
	);
}

export const ProfilePrivacyTabContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const profilePrivacy = UserSettings.profilePrivacy;
	const handleChange = async (value: ProfilePrivacyLevel) => {
		await UserSettingsCommands.update({profilePrivacy: value});
	};
	return (
		<SettingsTabSection
			title={i18n._(WHO_CAN_SEE_YOUR_FULL_PROFILE_DESCRIPTOR)}
			description={
				hasLabsRole(Users.currentUser)
					? `${i18n._(LABS_ROLE_NOTE_DESCRIPTOR, {labsName: PRODUCT_LABS_COMMUNITY_NAME})} ${i18n._(OPERATORS_MEMBERS_SEE_YOU_DESCRIPTOR)}`
					: undefined
			}
			data-flx="user.privacy-safety-tab.profile-privacy-tab.settings-tab-section"
		>
			<RadioGroup
				value={profilePrivacy}
				onChange={handleChange}
				aria-label={i18n._(PROFILE_PRIVACY_LABEL_DESCRIPTOR)}
				options={[
					{
						value: ProfilePrivacyLevels.ALL_GUILDS,
						name: i18n._(ALL_COMMUNITIES_OPTION_DESCRIPTOR),
						desc: i18n._(ALL_COMMUNITIES_DESCRIPTION_DESCRIPTOR),
					},
					{
						value: ProfilePrivacyLevels.SMALL_GUILDS_ONLY,
						name: i18n._(SMALL_COMMUNITIES_OPTION_DESCRIPTOR),
						desc: i18n._(SMALL_COMMUNITIES_DESCRIPTION_DESCRIPTOR),
					},
					{
						value: ProfilePrivacyLevels.FRIENDS_ONLY,
						name: i18n._(FRIENDS_ONLY_OPTION_DESCRIPTOR),
						desc: i18n._(FRIENDS_ONLY_DESCRIPTION_DESCRIPTOR),
					},
				]}
				data-flx="user.privacy-safety-tab.profile-privacy-tab.radio-group.profile-privacy-change"
			/>
		</SettingsTabSection>
	);
});
