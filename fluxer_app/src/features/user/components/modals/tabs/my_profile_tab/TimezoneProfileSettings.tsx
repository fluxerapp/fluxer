// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {SettingsTabSection} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {COMMUNITY_MEMBERS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Combobox, type ComboboxFilterOption, type ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {ProfileFieldPrivacyFlags} from '@fluxer/constants/src/UserConstants';
import {getTimeZoneDisplayOptions} from '@fluxer/date_utils/src/TimeZoneUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useCallback, useMemo, useState} from 'react';

const TIMEZONE_IDENTIFIER_EXAMPLE = 'America/New_York';
const TIMEZONE_DESCRIPTOR = msg({
	message: 'Time zone',
	comment: 'Field label in profile settings for choosing the timezone used to show local time on the profile.',
});
const PROFILE_LOCAL_TIME_DESCRIPTOR = msg({
	message: 'Profile local time',
	comment: 'Profile settings row title for choosing whether profile viewers can see the user local time.',
});
const PROFILE_LOCAL_TIME_SUMMARY_DESCRIPTOR = msg({
	message:
		'Set your time zone once so {productName} can keep your UTC offset current when daylight saving time changes. Other people can only see your UTC offset, not your exact time zone identifier.',
	comment:
		'Description in profile settings for the button that opens timezone settings. productName is the app name. A timezone identifier means values like America/New_York; only the UTC offset can be visible to others.',
});
const EDIT_PROFILE_LOCAL_TIME_DESCRIPTOR = msg({
	message: 'Edit profile local time',
	comment: 'Button label in profile settings. Opens the profile local time modal.',
});
const SEARCH_TIMEZONES_DESCRIPTOR = msg({
	message: 'Search time zones',
	comment: 'Placeholder in the profile timezone picker.',
});
const NOT_SET_DESCRIPTOR = msg({
	message: 'Not set',
	comment: 'Option in the profile timezone picker. Means no timezone has been selected.',
});
const TIMEZONE_HELP_DESCRIPTOR = msg({
	message: 'Choose the time zone {productName} uses to calculate your UTC offset for profile local time.',
	comment: 'Helper text under the profile timezone picker. productName is the app name.',
});
const TIMEZONE_PRIVACY_NOTE_DESCRIPTOR = msg({
	message:
		'Other people can only see your current UTC offset when you choose to share profile local time. They do not see your exact time zone identifier, such as {timezoneIdentifierExample}. {productName} stores that identifier only so the offset can update automatically when daylight saving time changes.',
	comment:
		'Privacy note in profile timezone settings. Preserve {timezoneIdentifierExample}; it is inserted by code as an IANA timezone identifier example. productName is the app name. Users should understand that only the UTC offset is shown to other people.',
});
const EVERYONE_DESCRIPTOR = msg({
	message: 'Everyone',
	comment: 'Profile timezone privacy option label. Allows anyone who can view the full profile to see local time.',
});
const EVERYONE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Allow anyone who can view your full profile to see your local time',
	comment: 'Profile timezone privacy option description for the Everyone switch.',
});
const FRIENDS_DESCRIPTOR = msg({
	message: 'Friends',
	comment: 'Profile timezone privacy option label. Allows friends to see local time.',
});
const FRIENDS_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Allow your friends to see your local time',
	comment: 'Profile timezone privacy option description for the Friends switch.',
});
const COMMUNITY_MEMBERS_DESCRIPTION_DESCRIPTOR = msg({
	message: "Allow members from communities you're in to see your local time",
	comment: 'Profile timezone privacy option description for the Community members switch.',
});

interface TimeZoneSelectOption extends ComboboxOption<string> {
	readonly searchText: string;
}

interface TimezoneProfileSettingsProps {
	readonly timezone: string | null;
	readonly timezonePrivacyFlags: number;
	readonly disabled?: boolean;
	readonly onTimezoneChange: (timezone: string | null) => void;
	readonly onTimezonePrivacyFlagsChange: (privacyFlags: number) => void;
}

export function TimezoneProfileSettings({
	timezone,
	timezonePrivacyFlags,
	disabled,
	onTimezoneChange,
	onTimezonePrivacyFlagsChange,
}: TimezoneProfileSettingsProps) {
	const {i18n} = useLingui();
	const handleOpen = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<TimezoneProfileSettingsModal
					timezone={timezone}
					timezonePrivacyFlags={timezonePrivacyFlags}
					disabled={disabled}
					onTimezoneChange={onTimezoneChange}
					onTimezonePrivacyFlagsChange={onTimezonePrivacyFlagsChange}
					data-flx="user.my-profile-tab.timezone-profile-settings.handle-open.timezone-profile-settings-modal"
				/>
			)),
		);
	}, [disabled, onTimezoneChange, onTimezonePrivacyFlagsChange, timezone, timezonePrivacyFlags]);
	return (
		<SettingsTabSection
			title={i18n._(PROFILE_LOCAL_TIME_DESCRIPTOR)}
			description={i18n._(PROFILE_LOCAL_TIME_SUMMARY_DESCRIPTOR, {productName: PRODUCT_NAME})}
			data-flx="user.my-profile-tab.timezone-profile-settings.section"
		>
			<Button
				variant="secondary"
				fitContent={true}
				disabled={disabled}
				onClick={handleOpen}
				data-flx="user.my-profile-tab.timezone-profile-settings.button.open"
			>
				{i18n._(EDIT_PROFILE_LOCAL_TIME_DESCRIPTOR)}
			</Button>
		</SettingsTabSection>
	);
}

function TimezoneProfileSettingsModal({
	timezone,
	timezonePrivacyFlags,
	disabled,
	onTimezoneChange,
	onTimezonePrivacyFlagsChange,
}: TimezoneProfileSettingsProps) {
	const {i18n} = useLingui();
	const [localTimezone, setLocalTimezone] = useState(timezone);
	const [localTimezonePrivacyFlags, setLocalTimezonePrivacyFlags] = useState(timezonePrivacyFlags);
	const hasFlag = useCallback(
		(flag: number) => (localTimezonePrivacyFlags & flag) === flag,
		[localTimezonePrivacyFlags],
	);
	const everyoneEnabled = hasFlag(ProfileFieldPrivacyFlags.EVERYONE);
	const handleTimezoneChange = useCallback(
		(value: string) => {
			const nextTimezone = value || null;
			setLocalTimezone(nextTimezone);
			onTimezoneChange(nextTimezone);
			if (localTimezone === null && nextTimezone !== null) {
				setLocalTimezonePrivacyFlags(ProfileFieldPrivacyFlags.EVERYONE);
				onTimezonePrivacyFlagsChange(ProfileFieldPrivacyFlags.EVERYONE);
			}
		},
		[localTimezone, onTimezoneChange, onTimezonePrivacyFlagsChange],
	);
	const handlePrivacyToggle = useCallback(
		(flag: number, value: boolean) => {
			const nextFlags = value ? localTimezonePrivacyFlags | flag : localTimezonePrivacyFlags & ~flag;
			setLocalTimezonePrivacyFlags(nextFlags);
			onTimezonePrivacyFlagsChange(nextFlags);
		},
		[localTimezonePrivacyFlags, onTimezonePrivacyFlagsChange],
	);
	const options = useMemo<ReadonlyArray<TimeZoneSelectOption>>(
		() => [
			{
				value: '',
				label: i18n._(NOT_SET_DESCRIPTOR),
				searchText: i18n._(NOT_SET_DESCRIPTOR),
			},
			...getTimeZoneDisplayOptions().map((option) => ({
				value: option.value,
				label: option.label,
				searchText: option.searchText,
			})),
		],
		[i18n.locale],
	);
	const filterOption = (option: ComboboxFilterOption<TimeZoneSelectOption>, rawInput: string) => {
		const input = rawInput.trim().toLowerCase();
		if (!input) {
			return true;
		}
		const data = option.data;
		return data.label.toLowerCase().includes(input) || data.searchText.toLowerCase().includes(input);
	};
	return (
		<Modal.Root size="small" centered data-flx="user.my-profile-tab.timezone-profile-settings.modal-root">
			<Modal.Header
				title={i18n._(PROFILE_LOCAL_TIME_DESCRIPTOR)}
				data-flx="user.my-profile-tab.timezone-profile-settings.modal-header"
			/>
			<Modal.Content data-flx="user.my-profile-tab.timezone-profile-settings.modal-content">
				<Modal.ContentLayout data-flx="user.my-profile-tab.timezone-profile-settings.modal-content-layout">
					<Modal.Description data-flx="user.my-profile-tab.timezone-profile-settings.privacy-note">
						{i18n._(TIMEZONE_PRIVACY_NOTE_DESCRIPTOR, {
							productName: PRODUCT_NAME,
							timezoneIdentifierExample: TIMEZONE_IDENTIFIER_EXAMPLE,
						})}
					</Modal.Description>
					<Combobox<string, false, TimeZoneSelectOption>
						label={i18n._(TIMEZONE_DESCRIPTOR)}
						description={i18n._(TIMEZONE_HELP_DESCRIPTOR, {productName: PRODUCT_NAME})}
						placeholder={i18n._(SEARCH_TIMEZONES_DESCRIPTOR)}
						value={localTimezone ?? ''}
						options={options}
						onChange={handleTimezoneChange}
						disabled={disabled}
						filterOption={filterOption}
						data-flx="user.my-profile-tab.timezone-profile-settings.select.change"
					/>
					<Modal.InputGroup data-flx="user.my-profile-tab.timezone-profile-settings.input-group">
						<Switch
							label={i18n._(EVERYONE_DESCRIPTOR)}
							description={i18n._(EVERYONE_DESCRIPTION_DESCRIPTOR)}
							value={localTimezone !== null && hasFlag(ProfileFieldPrivacyFlags.EVERYONE)}
							onChange={(value) => handlePrivacyToggle(ProfileFieldPrivacyFlags.EVERYONE, value)}
							disabled={disabled || localTimezone === null}
							data-flx="user.my-profile-tab.timezone-profile-settings.switch.toggle"
						/>
						<Switch
							label={i18n._(FRIENDS_DESCRIPTOR)}
							description={i18n._(FRIENDS_DESCRIPTION_DESCRIPTOR)}
							value={localTimezone !== null && (everyoneEnabled || hasFlag(ProfileFieldPrivacyFlags.FRIENDS))}
							onChange={(value) => handlePrivacyToggle(ProfileFieldPrivacyFlags.FRIENDS, value)}
							disabled={disabled || localTimezone === null || everyoneEnabled}
							data-flx="user.my-profile-tab.timezone-profile-settings.switch.toggle--2"
						/>
						<Switch
							label={i18n._(COMMUNITY_MEMBERS_DESCRIPTOR)}
							description={i18n._(COMMUNITY_MEMBERS_DESCRIPTION_DESCRIPTOR)}
							value={localTimezone !== null && (everyoneEnabled || hasFlag(ProfileFieldPrivacyFlags.MUTUAL_GUILDS))}
							onChange={(value) => handlePrivacyToggle(ProfileFieldPrivacyFlags.MUTUAL_GUILDS, value)}
							disabled={disabled || localTimezone === null || everyoneEnabled}
							data-flx="user.my-profile-tab.timezone-profile-settings.switch.toggle--3"
						/>
					</Modal.InputGroup>
				</Modal.ContentLayout>
			</Modal.Content>
		</Modal.Root>
	);
}
