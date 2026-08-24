// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelRtcRegion} from '@app/features/channel/commands/ChannelCommands';
import styles from '@app/features/channel/components/modals/channel_tabs/ChannelOverviewTab.module.css';
import type {
	FormInputs,
	RtcRegionOption,
} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/shared';
import * as EmojiUtils from '@app/features/expressions/utils/EmojiUtils';
import type {ComboboxFilterOption} from '@app/features/ui/components/form/FormCombobox';
import {CompactComboboxRow} from '@app/features/user/components/modals/tabs/components/CompactComboboxRow';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';
import {Controller, type UseFormReturn} from 'react-hook-form';

const AUTOMATIC_DESCRIPTOR = msg({
	message: 'Automatic',
	comment:
		'Channel overview settings tab label, control, or validation message (name, topic, slowmode, voice region, mature content gate).',
});
const VOICE_REGION_DESCRIPTOR = msg({
	message: 'Voice region',
	comment:
		'Channel overview settings tab label, control, or validation message (name, topic, slowmode, voice region, mature content gate).',
});

interface RtcRegionSelectProps {
	form: UseFormReturn<FormInputs>;
	rtcRegions: Array<ChannelRtcRegion>;
	isLoadingRegions: boolean;
}

export const RtcRegionSelect: React.FC<RtcRegionSelectProps> = observer(({form, rtcRegions, isLoadingRegions}) => {
	const {i18n} = useLingui();
	const automaticLabel = useMemo(() => i18n._(AUTOMATIC_DESCRIPTOR), [i18n.locale]);
	const getRegionDisplayName = useCallback((_regionId: string, regionName: string): string => {
		return regionName;
	}, []);
	const renderRegionOption = useCallback(
		(option: RtcRegionOption) => {
			const {region, label} = option;
			if (!region) {
				return <span data-flx="channel.channel-tabs.channel-overview-tab.rtc-region-option.span">{label}</span>;
			}
			const displayName = getRegionDisplayName(region.id, region.name);
			const emojiUrl = EmojiUtils.getEmojiURL(region.emoji);
			return (
				<div className={styles.regionOption} data-flx="channel.channel-tabs.channel-overview-tab.rtc-region-option">
					{emojiUrl ? (
						<img
							src={emojiUrl}
							alt={displayName}
							aria-hidden={true}
							className={styles.regionEmoji}
							data-flx="channel.channel-tabs.channel-overview-tab.rtc-region-option.region-emoji"
						/>
					) : (
						<span
							className={styles.regionEmojiText}
							data-flx="channel.channel-tabs.channel-overview-tab.rtc-region-option.region-emoji-text"
						>
							{region.emoji}
						</span>
					)}
					<span data-flx="channel.channel-tabs.channel-overview-tab.rtc-region-option.name">{displayName}</span>
				</div>
			);
		},
		[getRegionDisplayName],
	);
	return (
		<Controller
			name="rtc_region"
			control={form.control}
			render={({field}) => {
				const options: Array<RtcRegionOption> = [
					{value: null, label: automaticLabel, region: null},
					...rtcRegions
						.map((region) => ({
							value: region.id,
							label: getRegionDisplayName(region.id, region.name),
							region,
						}))
						.sort((a, b) => a.label.localeCompare(b.label)),
				];
				return (
					<CompactComboboxRow<string | null, RtcRegionOption>
						label={i18n._(VOICE_REGION_DESCRIPTOR)}
						value={field.value ?? null}
						onChange={(value) => field.onChange(value)}
						options={options}
						isSearchable={true}
						placeholder={automaticLabel}
						isClearable={false}
						isLoading={isLoadingRegions}
						controlWidth="wide"
						menuMinWidth={280}
						renderOption={renderRegionOption}
						renderValue={(option) => (option ? renderRegionOption(option) : null)}
						filterOption={(option: ComboboxFilterOption<RtcRegionOption>, inputValue: string) => {
							const searchTerm = inputValue.toLowerCase();
							if (!option.data.region) {
								return option.data.label.toLowerCase().includes(searchTerm);
							}
							const displayName = getRegionDisplayName(option.data.region.id, option.data.region.name);
							return (
								displayName.toLowerCase().includes(searchTerm) ||
								option.data.region.id.toLowerCase().includes(searchTerm)
							);
						}}
						dataFlx="channel.channel-tabs.channel-overview-tab.form-select.change"
						data-flx="channel.channel-tabs.channel-overview-tab.rtc-region-select.compact-combobox-row.change"
					/>
				);
			}}
			data-flx="channel.channel-tabs.channel-overview-tab.controller--4"
		/>
	);
});
