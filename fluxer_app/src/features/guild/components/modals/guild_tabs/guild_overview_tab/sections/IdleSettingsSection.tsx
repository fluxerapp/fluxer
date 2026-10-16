// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/components/GuildOverviewTabSettingsSection';
import type {ChannelLike} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/GuildOverviewTypes';
import type {FormInputs} from '@app/features/guild/utils/guild_tabs/GuildOverviewTabUtils';
import {afkTimeoutOptionsRaw} from '@app/features/guild/utils/guild_tabs/GuildOverviewTabUtils';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {CompactComboboxRow} from '@app/features/user/components/modals/tabs/components/CompactComboboxRow';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useMemo} from 'react';
import type {UseFormReturn} from 'react-hook-form';
import {Controller} from 'react-hook-form';

const NO_AFK_CHANNEL_DESCRIPTOR = msg({
	message: 'No AFK channel',
	comment: 'Empty-state text in the idle settings section.',
});
const UNNAMED_CHANNEL_DESCRIPTOR = msg({
	message: 'Unnamed channel',
	comment: 'Short label in the idle settings section. Keep it concise.',
});
const AFK_IDLE_CHANNEL_DESCRIPTOR = msg({
	message: 'AFK / idle channel',
	comment: 'Label in the idle settings section.',
});
const SELECT_A_CHANNEL_DESCRIPTOR = msg({
	message: 'Select a channel',
	comment: 'Button or menu action label in the idle settings section. Keep it concise.',
});
const AFK_TIMEOUT_DESCRIPTOR = msg({
	message: 'AFK timeout',
	comment: 'Short label in the idle settings section. Keep it concise.',
});
const SECONDS_DESCRIPTOR = msg({
	message: '{seconds, plural, one {# second} other {# seconds}}',
	comment: 'AFK timeout label for a stored custom duration shown in the idle settings section.',
});
export const IdleSettingsSection: React.FC<{
	form: UseFormReturn<FormInputs>;
	canManageGuild: boolean;
	voiceChannels: Array<ChannelLike>;
}> = ({form, canManageGuild, voiceChannels}) => {
	const {i18n} = useLingui();
	const afkTimeoutOptions = useMemo<Array<{value: number; label: string}>>(
		() => afkTimeoutOptionsRaw.map(({value, label}) => ({value, label: i18n._(label)})),
		[i18n.locale],
	);
	const afkChannelOptions = useMemo<Array<ComboboxOption<string | null>>>(
		() => [
			{value: null, label: i18n._(NO_AFK_CHANNEL_DESCRIPTOR)},
			...voiceChannels.map((channel) => ({
				value: channel.id,
				label: channel.name ?? i18n._(UNNAMED_CHANNEL_DESCRIPTOR),
			})),
		],
		[voiceChannels, i18n.locale],
	);
	return (
		<SettingsSection
			title={<Trans>Idle settings</Trans>}
			data-flx="guild.guild-tabs.guild-overview-tab.idle-settings-section.settings-section"
		>
			<Controller
				name="afk_channel_id"
				control={form.control}
				render={({field}) => (
					<CompactComboboxRow<string | null>
						label={i18n._(AFK_IDLE_CHANNEL_DESCRIPTOR)}
						value={field.value ?? null}
						onChange={(v) => field.onChange(v)}
						options={afkChannelOptions}
						disabled={!canManageGuild}
						controlWidth="wide"
						menuMinWidth={280}
						dataFlx="guild.guild-tabs.guild-overview-tab.idle-settings-section.form-select.change"
						aria-label={i18n._(SELECT_A_CHANNEL_DESCRIPTOR)}
						data-flx="guild.guild-tabs.guild-overview-tab.idle-settings-section.compact-combobox-row.change"
					/>
				)}
				data-flx="guild.guild-tabs.guild-overview-tab.idle-settings-section.controller"
			/>
			<Controller
				name="afk_timeout"
				control={form.control}
				render={({field}) => {
					const currentValue = field.value ?? 300;
					const options = afkTimeoutOptions.some((option) => option.value === currentValue)
						? afkTimeoutOptions
						: [{value: currentValue, label: i18n._(SECONDS_DESCRIPTOR, {seconds: currentValue})}, ...afkTimeoutOptions];
					return (
						<CompactComboboxRow<number>
							label={i18n._(AFK_TIMEOUT_DESCRIPTOR)}
							value={currentValue}
							onChange={field.onChange}
							options={options}
							disabled={!canManageGuild}
							isSearchable={false}
							controlWidth="medium"
							dataFlx="guild.guild-tabs.guild-overview-tab.idle-settings-section.form-select.change-afk-timeout"
							data-flx="guild.guild-tabs.guild-overview-tab.idle-settings-section.compact-combobox-row.change--2"
						/>
					);
				}}
				data-flx="guild.guild-tabs.guild-overview-tab.idle-settings-section.controller--2"
			/>
		</SettingsSection>
	);
};
