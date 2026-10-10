// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsTabSection} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {
	COMMUNITY_MEMBERS_DESCRIPTOR,
	FRIENDS_OF_FRIENDS_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import {CompactComboboxRow} from '@app/features/user/components/modals/tabs/components/CompactComboboxRow';
import UserSettings from '@app/features/user/state/UserSettings';
import type {UserSettingsMutationController} from '@app/features/user/UserSettingsMutationPresentation';
import {GroupDmAddPermissionFlags, IncomingCallFlags} from '@fluxer/constants/src/UserConstants';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const INCOMING_CALL_NOBODY_OPTION_DESCRIPTOR = msg({
	message: 'Block all incoming calls',
	comment: 'Privacy > Communication: select option blocking every incoming call.',
});
const INCOMING_CALL_FRIENDS_ONLY_OPTION_DESCRIPTOR = msg({
	message: 'Only friends can call you (recommended)',
	comment: 'Privacy > Communication: select option allowing only friends to call.',
});
const INCOMING_CALL_CUSTOM_OPTION_DESCRIPTOR = msg({
	message: 'Friends and selected groups can call you',
	comment: 'Privacy > Communication: select option for the custom incoming call permission tier.',
});
const INCOMING_CALL_EVERYONE_OPTION_DESCRIPTOR = msg({
	message: 'Anyone can call you, including strangers',
	comment: 'Privacy > Communication: select option allowing anyone to call.',
});
const GROUP_DM_ADD_NOBODY_OPTION_DESCRIPTOR = msg({
	message: 'No one can add you to group chats without asking',
	comment: 'Privacy > Communication: select option blocking group chat adds unless the user accepts an invite.',
});
const GROUP_DM_ADD_FRIENDS_ONLY_OPTION_DESCRIPTOR = msg({
	message: 'Only friends can add you without asking (recommended)',
	comment: 'Privacy > Communication: select option allowing only friends to add the user to group chats.',
});
const GROUP_DM_ADD_CUSTOM_OPTION_DESCRIPTOR = msg({
	message: 'Friends and selected groups can add you without asking',
	comment: 'Privacy > Communication: select option for the custom group chat add permission tier.',
});
const GROUP_DM_ADD_EVERYONE_OPTION_DESCRIPTOR = msg({
	message: 'Anyone can add you to group chats',
	comment: 'Privacy > Communication: select option allowing anyone to add the user to group chats.',
});

const CommunicationPermissionValue = Object.freeze({
	NOBODY: 'nobody',
	FRIENDS_ONLY: 'friends_only',
	EVERYONE: 'everyone',
	CUSTOM: 'custom',
} as const);

type CommunicationPermissionValue = (typeof CommunicationPermissionValue)[keyof typeof CommunicationPermissionValue];

interface CommunicationPermissionSpec {
	readonly flags: {
		readonly NOBODY: number;
		readonly FRIENDS_ONLY: number;
		readonly EVERYONE: number;
		readonly FRIENDS_OF_FRIENDS: number;
		readonly GUILD_MEMBERS: number;
	};
	readonly read: () => number;
	readonly write: (flags: number) => Promise<void>;
}

const INCOMING_CALL_SPEC: CommunicationPermissionSpec = {
	flags: IncomingCallFlags,
	read: () => UserSettings.getIncomingCallFlags(),
	write: (flags) => UserSettingsCommands.update({incomingCallFlags: flags}),
};

const GROUP_DM_ADD_SPEC: CommunicationPermissionSpec = {
	flags: GroupDmAddPermissionFlags,
	read: () => UserSettings.getGroupDmAddPermissionFlags(),
	write: (flags) => UserSettingsCommands.update({groupDmAddPermissionFlags: flags}),
};

interface CommunicationTabContentProps {
	readonly mutationController: UserSettingsMutationController;
}

interface CommunicationFlagUpdate {
	readonly enabled: boolean;
	readonly flag: number;
}

function resolveBaseValue(spec: CommunicationPermissionSpec, currentFlags: number): CommunicationPermissionValue {
	const hasFlag = (flag: number) => (currentFlags & flag) === flag;
	if (hasFlag(spec.flags.NOBODY)) return CommunicationPermissionValue.NOBODY;
	if (hasFlag(spec.flags.EVERYONE)) return CommunicationPermissionValue.EVERYONE;
	if (hasFlag(spec.flags.FRIENDS_ONLY)) return CommunicationPermissionValue.FRIENDS_ONLY;
	return CommunicationPermissionValue.CUSTOM;
}

function nextFlagsForBaseChange(spec: CommunicationPermissionSpec, value: CommunicationPermissionValue): number {
	if (value === CommunicationPermissionValue.NOBODY) return spec.flags.NOBODY;
	if (value === CommunicationPermissionValue.FRIENDS_ONLY) return spec.flags.FRIENDS_ONLY;
	if (value === CommunicationPermissionValue.EVERYONE) return spec.flags.EVERYONE;
	const customFlags = spec.read() & (spec.flags.FRIENDS_OF_FRIENDS | spec.flags.GUILD_MEMBERS);
	return customFlags === 0 ? spec.flags.FRIENDS_OF_FRIENDS : customFlags;
}

function nextFlagsForAdditiveToggle(
	spec: CommunicationPermissionSpec,
	{enabled, flag}: CommunicationFlagUpdate,
): number {
	let newFlags = spec.read() & ~(spec.flags.NOBODY | spec.flags.FRIENDS_ONLY | spec.flags.EVERYONE);
	if (enabled) {
		newFlags |= flag;
	} else {
		newFlags &= ~flag;
	}
	return newFlags === 0 ? spec.flags.FRIENDS_ONLY : newFlags;
}

function buildCommunicationOptions(
	i18n: I18n,
	descriptors: {
		readonly nobody: MessageDescriptor;
		readonly friendsOnly: MessageDescriptor;
		readonly custom: MessageDescriptor;
		readonly everyone: MessageDescriptor;
	},
): Array<ComboboxOption<CommunicationPermissionValue>> {
	return [
		{value: CommunicationPermissionValue.NOBODY, label: i18n._(descriptors.nobody)},
		{value: CommunicationPermissionValue.FRIENDS_ONLY, label: i18n._(descriptors.friendsOnly)},
		{value: CommunicationPermissionValue.CUSTOM, label: i18n._(descriptors.custom)},
		{value: CommunicationPermissionValue.EVERYONE, label: i18n._(descriptors.everyone)},
	];
}

export const CommunicationTabContent: React.FC<CommunicationTabContentProps> = observer(({mutationController}) => {
	const {i18n} = useLingui();
	const incomingCallFlags = UserSettings.getIncomingCallFlags();
	const groupDmAddPermissionFlags = UserSettings.getGroupDmAddPermissionFlags();
	const hasCallFlag = (flag: number) => (incomingCallFlags & flag) === flag;
	const hasGroupDmAddFlag = (flag: number) => (groupDmAddPermissionFlags & flag) === flag;
	const settleBaseChange = (spec: CommunicationPermissionSpec, value: CommunicationPermissionValue) => {
		mutationController.settle(() => spec.write(nextFlagsForBaseChange(spec, value)));
	};
	const settleAdditiveToggle = (spec: CommunicationPermissionSpec, update: CommunicationFlagUpdate) => {
		mutationController.settle(() => spec.write(nextFlagsForAdditiveToggle(spec, update)));
	};
	const handleIncomingCallModifierToggle = ({enabled, flag}: CommunicationFlagUpdate) => {
		mutationController.settle(() => {
			let newFlags = UserSettings.getIncomingCallFlags();
			if (enabled) {
				newFlags |= flag;
			} else {
				newFlags &= ~flag;
			}
			return UserSettingsCommands.update({incomingCallFlags: newFlags});
		});
	};
	const incomingCallOptions = buildCommunicationOptions(i18n, {
		nobody: INCOMING_CALL_NOBODY_OPTION_DESCRIPTOR,
		friendsOnly: INCOMING_CALL_FRIENDS_ONLY_OPTION_DESCRIPTOR,
		custom: INCOMING_CALL_CUSTOM_OPTION_DESCRIPTOR,
		everyone: INCOMING_CALL_EVERYONE_OPTION_DESCRIPTOR,
	});
	const groupDmAddOptions = buildCommunicationOptions(i18n, {
		nobody: GROUP_DM_ADD_NOBODY_OPTION_DESCRIPTOR,
		friendsOnly: GROUP_DM_ADD_FRIENDS_ONLY_OPTION_DESCRIPTOR,
		custom: GROUP_DM_ADD_CUSTOM_OPTION_DESCRIPTOR,
		everyone: GROUP_DM_ADD_EVERYONE_OPTION_DESCRIPTOR,
	});
	const incomingCallBaseValue = resolveBaseValue(INCOMING_CALL_SPEC, incomingCallFlags);
	const groupDmAddBaseValue = resolveBaseValue(GROUP_DM_ADD_SPEC, groupDmAddPermissionFlags);
	return (
		<>
			<SettingsTabSection
				title={<Trans>Incoming calls</Trans>}
				description={<Trans>Choose who can start a call with you.</Trans>}
				data-flx="user.privacy-safety-tab.communication-tab.communication-tab-content.settings-tab-section"
			>
				<CompactComboboxRow<CommunicationPermissionValue>
					label={<Trans>Allowed callers</Trans>}
					value={incomingCallBaseValue}
					onChange={(value) => settleBaseChange(INCOMING_CALL_SPEC, value)}
					options={incomingCallOptions}
					isSearchable={false}
					controlWidth="wide"
					dataFlx="user.privacy-safety-tab.communication-tab.communication-tab-content.select.incoming-call-base-change"
					data-flx="user.privacy-safety-tab.communication-tab.communication-tab-content.compact-combobox-row.incoming-call-base-change"
				/>
				{incomingCallBaseValue === CommunicationPermissionValue.CUSTOM && (
					<>
						<Switch
							label={i18n._(FRIENDS_OF_FRIENDS_DESCRIPTOR)}
							value={hasCallFlag(IncomingCallFlags.FRIENDS_OF_FRIENDS)}
							onChange={(value) =>
								settleAdditiveToggle(INCOMING_CALL_SPEC, {flag: IncomingCallFlags.FRIENDS_OF_FRIENDS, enabled: value})
							}
							data-flx="user.privacy-safety-tab.communication-tab.communication-tab-content.switch.incoming-call-additive-toggle"
						/>
						<Switch
							label={i18n._(COMMUNITY_MEMBERS_DESCRIPTOR)}
							value={hasCallFlag(IncomingCallFlags.GUILD_MEMBERS)}
							onChange={(value) =>
								settleAdditiveToggle(INCOMING_CALL_SPEC, {flag: IncomingCallFlags.GUILD_MEMBERS, enabled: value})
							}
							data-flx="user.privacy-safety-tab.communication-tab.communication-tab-content.switch.incoming-call-additive-toggle--2"
						/>
					</>
				)}
				{incomingCallBaseValue !== CommunicationPermissionValue.NOBODY && (
					<Switch
						label={<Trans>Receive calls silently</Trans>}
						description={<Trans>Calls you allow still come through, but they don't ring.</Trans>}
						value={hasCallFlag(IncomingCallFlags.SILENT_EVERYONE)}
						onChange={(value) =>
							handleIncomingCallModifierToggle({flag: IncomingCallFlags.SILENT_EVERYONE, enabled: value})
						}
						data-flx="user.privacy-safety-tab.communication-tab.communication-tab-content.switch.incoming-call-modifier-toggle"
					/>
				)}
			</SettingsTabSection>
			<SettingsTabSection
				title={<Trans>Group chats</Trans>}
				description={<Trans>Choose who can add you to a group chat without asking first.</Trans>}
				data-flx="user.privacy-safety-tab.communication-tab.communication-tab-content.settings-tab-section--2"
			>
				<CompactComboboxRow<CommunicationPermissionValue>
					label={<Trans>Allowed invites</Trans>}
					value={groupDmAddBaseValue}
					onChange={(value) => settleBaseChange(GROUP_DM_ADD_SPEC, value)}
					options={groupDmAddOptions}
					isSearchable={false}
					controlWidth="wide"
					dataFlx="user.privacy-safety-tab.communication-tab.communication-tab-content.select.group-dm-add-base-change"
					data-flx="user.privacy-safety-tab.communication-tab.communication-tab-content.compact-combobox-row.group-dm-add-base-change"
				/>
				{groupDmAddBaseValue === CommunicationPermissionValue.CUSTOM && (
					<>
						<Switch
							label={i18n._(FRIENDS_OF_FRIENDS_DESCRIPTOR)}
							value={hasGroupDmAddFlag(GroupDmAddPermissionFlags.FRIENDS_OF_FRIENDS)}
							onChange={(value) =>
								settleAdditiveToggle(GROUP_DM_ADD_SPEC, {
									flag: GroupDmAddPermissionFlags.FRIENDS_OF_FRIENDS,
									enabled: value,
								})
							}
							data-flx="user.privacy-safety-tab.communication-tab.communication-tab-content.switch.group-dm-add-additive-toggle"
						/>
						<Switch
							label={i18n._(COMMUNITY_MEMBERS_DESCRIPTOR)}
							value={hasGroupDmAddFlag(GroupDmAddPermissionFlags.GUILD_MEMBERS)}
							onChange={(value) =>
								settleAdditiveToggle(GROUP_DM_ADD_SPEC, {flag: GroupDmAddPermissionFlags.GUILD_MEMBERS, enabled: value})
							}
							data-flx="user.privacy-safety-tab.communication-tab.communication-tab-content.switch.group-dm-add-additive-toggle--2"
						/>
					</>
				)}
			</SettingsTabSection>
		</>
	);
});
