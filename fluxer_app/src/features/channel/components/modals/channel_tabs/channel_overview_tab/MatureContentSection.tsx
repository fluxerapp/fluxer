// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FormInputs} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/shared';
import type {Channel} from '@app/features/channel/models/Channel';
import type {Guild} from '@app/features/guild/models/Guild';
import {MATURE_CONTENT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getEffectiveChannelContentWarning} from '@app/features/messaging/utils/ContentWarningUtils';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Textarea} from '@app/features/ui/components/form/FormInput';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {CompactComboboxRow} from '@app/features/user/components/modals/tabs/components/CompactComboboxRow';
import {CONTENT_WARNING_TEXT_MAX_LENGTH, ContentWarningLevel} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {Controller, type UseFormReturn} from 'react-hook-form';

const INHERIT_DESCRIPTOR = msg({
	message: 'Inherit',
	comment: 'Mature-content option that uses the parent or community setting.',
});
const ON_DESCRIPTOR = msg({
	message: 'On',
	comment: 'Mature-content option that marks this channel for mature content.',
});
const OFF_2_DESCRIPTOR = msg({
	message: 'Off',
	comment: 'Mature-content option that leaves this channel ungated.',
});
const THIS_CONTAINS_SENSITIVE_CONTENT_DESCRIPTOR = msg({
	message: 'This contains sensitive content.',
	comment:
		'Channel overview settings tab label, control, or validation message (name, topic, slowmode, voice region, mature content gate).',
});
const MATURE_CONTENT_OVERRIDE_DESCRIPTOR = msg({
	message: 'Mature content override',
	comment:
		'Channel overview settings tab label, control, or validation message (name, topic, slowmode, voice region, mature content gate).',
});
const SHOW_A_CONTENT_WARNING_IN_THIS_CHANNEL_DESCRIPTOR = msg({
	message: 'Show a content warning in this channel',
	comment:
		'Channel overview settings tab label, control, or validation message (name, topic, slowmode, voice region, mature content gate).',
});
const CUSTOM_WARNING_TEXT_DESCRIPTOR = msg({
	message: 'Custom warning text',
	comment:
		'Channel overview settings tab label, control, or validation message (name, topic, slowmode, voice region, mature content gate).',
});

interface MatureContentSectionProps {
	form: UseFormReturn<FormInputs>;
	channel: Channel;
	guild: Guild | null | undefined;
}

export const MatureContentSection: React.FC<MatureContentSectionProps> = ({form, channel, guild}) => {
	const {i18n} = useLingui();
	const stubForInherit = {
		...channel,
		nsfwOverride: null,
		contentWarningLevel: ContentWarningLevel.INHERIT,
		contentWarningText: null,
	} as typeof channel;
	const inheritedWarning = getEffectiveChannelContentWarning(stubForInherit, guild);
	const matureContentOptions: ReadonlyArray<ComboboxOption<string>> = [
		{
			value: 'inherit',
			label: i18n._(INHERIT_DESCRIPTOR),
		},
		{
			value: 'on',
			label: i18n._(ON_DESCRIPTOR),
		},
		{
			value: 'off',
			label: i18n._(OFF_2_DESCRIPTOR),
		},
	];
	const overrideToKey = (v: boolean | null): string => (v === null ? 'inherit' : v ? 'on' : 'off');
	const keyToOverride = (k: string): boolean | null => (k === 'inherit' ? null : k === 'on');
	const inheritedWarningText = inheritedWarning.text ?? i18n._(THIS_CONTAINS_SENSITIVE_CONTENT_DESCRIPTOR);
	return (
		<>
			<Controller
				name="nsfw_override"
				control={form.control}
				render={({field}) => (
					<CompactComboboxRow<string>
						label={i18n._(MATURE_CONTENT_DESCRIPTOR)}
						value={overrideToKey(field.value)}
						options={matureContentOptions}
						onChange={(v) => field.onChange(keyToOverride(v))}
						isSearchable={false}
						controlWidth="small"
						dataFlx="channel.channel-tabs.channel-overview-tab.mature-content-select"
						aria-label={i18n._(MATURE_CONTENT_OVERRIDE_DESCRIPTOR)}
						data-flx="channel.channel-tabs.channel-overview-tab.mature-content-section.compact-combobox-row.change"
					/>
				)}
				data-flx="channel.channel-tabs.channel-overview-tab.controller--5"
			/>
			<Controller
				name="content_warning_level"
				control={form.control}
				render={({field}) => (
					<Switch
						label={i18n._(SHOW_A_CONTENT_WARNING_IN_THIS_CHANNEL_DESCRIPTOR)}
						value={field.value === ContentWarningLevel.CONTENT_WARNING}
						onChange={(value) =>
							field.onChange(value ? ContentWarningLevel.CONTENT_WARNING : ContentWarningLevel.INHERIT)
						}
						data-flx="channel.channel-tabs.channel-overview-tab.switch.change"
					/>
				)}
				data-flx="channel.channel-tabs.channel-overview-tab.controller--6"
			/>
			{form.watch('content_warning_level') === ContentWarningLevel.CONTENT_WARNING && (
				<Controller
					name="content_warning_text"
					control={form.control}
					render={({field}) => (
						<Textarea
							label={i18n._(CUSTOM_WARNING_TEXT_DESCRIPTOR)}
							placeholder={inheritedWarningText}
							value={field.value ?? ''}
							onChange={(e) => field.onChange(e.target.value)}
							maxLength={CONTENT_WARNING_TEXT_MAX_LENGTH}
							minRows={2}
							maxRows={4}
							showCharacterCount={true}
							data-flx="channel.channel-tabs.channel-overview-tab.textarea.change"
						/>
					)}
					data-flx="channel.channel-tabs.channel-overview-tab.controller--7"
				/>
			)}
		</>
	);
};
