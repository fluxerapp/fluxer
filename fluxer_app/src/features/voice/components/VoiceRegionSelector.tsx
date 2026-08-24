// SPDX-License-Identifier: AGPL-3.0-or-later

import * as EmojiUtils from '@app/features/expressions/utils/EmojiUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Combobox as FormCombobox} from '@app/features/ui/components/form/FormCombobox';
import * as CallCommands from '@app/features/voice/commands/CallCommands';
import styles from '@app/features/voice/components/VoiceRegionSelector.module.css';
import {AUTOMATIC_VOICE_REGION_ID} from '@fluxer/constants/src/ChannelConstants';
import type {RtcRegionResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {useCallback, useEffect, useMemo, useState} from 'react';

const AUTOMATIC_DESCRIPTOR = msg({
	message: 'Automatic',
	comment: "Voice region picker option label meaning 'let Fluxer pick the best region automatically'.",
});
const logger = new Logger('VoiceRegionSelector');

interface VoiceRegionSelectorProps {
	channelId?: string | null;
	currentRegion?: string | null;
	compact?: boolean;
}

interface RtcRegionOption extends ComboboxOption<string> {
	region: RtcRegionResponse;
}

export function VoiceRegionSelector({channelId, currentRegion, compact = false}: VoiceRegionSelectorProps) {
	const {i18n} = useLingui();
	const [regions, setRegions] = useState<Array<RtcRegionResponse>>([]);
	const [isChangingRegion, setIsChangingRegion] = useState(false);
	useEffect(() => {
		if (!channelId) {
			setRegions([]);
			return undefined;
		}
		let cancelled = false;
		CallCommands.fetchCallRegions(channelId)
			.then((fetchedRegions) => {
				if (!cancelled) setRegions(fetchedRegions);
			})
			.catch(() => {
				if (!cancelled) setRegions([]);
			});
		return () => {
			cancelled = true;
		};
	}, [channelId]);
	const getRegionDisplayName = useCallback(
		(regionId: string, regionName: string): string => {
			if (regionId === AUTOMATIC_VOICE_REGION_ID) {
				return i18n._(AUTOMATIC_DESCRIPTOR);
			}
			if (regionName && regionName !== regionId) {
				return regionName;
			}
			return regionId
				.split('-')
				.map((part) => {
					const lower = part.toLowerCase();
					if (lower === 'us') return 'US';
					if (lower === 'eu') return 'EU';
					return `${lower.slice(0, 1).toUpperCase()}${lower.slice(1)}`;
				})
				.join(' ');
		},
		[i18n],
	);
	const options = useMemo<Array<RtcRegionOption>>(() => {
		const automaticRegion = regions.find((r) => r.id === AUTOMATIC_VOICE_REGION_ID);
		const otherRegions = regions.filter((r) => r.id !== AUTOMATIC_VOICE_REGION_ID);
		const automatic: RtcRegionOption = {
			value: AUTOMATIC_VOICE_REGION_ID,
			label: i18n._(AUTOMATIC_DESCRIPTOR),
			region: automaticRegion ?? {id: AUTOMATIC_VOICE_REGION_ID, name: 'Automatic', emoji: '🌐'},
		};
		const regionOptions = otherRegions
			.map((region) => ({
				value: region.id,
				label: getRegionDisplayName(region.id, region.name),
				region,
			}))
			.sort((a, b) => a.label.localeCompare(b.label));
		return [automatic, ...regionOptions];
	}, [getRegionDisplayName, regions, i18n.locale]);
	const displayName = useMemo(() => {
		const effectiveRegion = currentRegion ?? AUTOMATIC_VOICE_REGION_ID;
		if (effectiveRegion === AUTOMATIC_VOICE_REGION_ID) return i18n._(AUTOMATIC_DESCRIPTOR);
		const regionData = regions.find((region) => region.id === effectiveRegion);
		if (regionData) return getRegionDisplayName(regionData.id, regionData.name);
		return effectiveRegion;
	}, [currentRegion, getRegionDisplayName, regions, i18n.locale]);
	const selectDensity = compact ? 'compactOverlay' : 'default';
	const handleRegionSelect = useCallback(
		async (regionId: string) => {
			if (!channelId || isChangingRegion) return;
			setIsChangingRegion(true);
			try {
				await CallCommands.updateCallRegion(channelId, regionId);
			} catch (error) {
				logger.error('Failed to update region:', error);
			} finally {
				setIsChangingRegion(false);
			}
		},
		[channelId, isChangingRegion],
	);
	const selectedValue = useMemo(() => {
		const effectiveRegion = currentRegion ?? AUTOMATIC_VOICE_REGION_ID;
		return options.some((option) => option.value === effectiveRegion) ? effectiveRegion : AUTOMATIC_VOICE_REGION_ID;
	}, [currentRegion, options]);
	const renderRegionOption = useCallback(
		(option: RtcRegionOption) => {
			if (compact) {
				return (
					<span
						className={clsx(styles.regionName, styles.regionNameCompact)}
						data-flx="voice.voice-region-selector.render-region-option.region-name"
					>
						{option.label}
					</span>
				);
			}
			const emojiUrl = EmojiUtils.getEmojiURL(option.region.emoji);
			return (
				<div className={styles.regionOption} data-flx="voice.voice-region-selector.render-region-option.region-option">
					{emojiUrl ? (
						<img
							src={emojiUrl}
							alt={option.label}
							aria-hidden={true}
							className={styles.regionEmoji}
							data-flx="voice.voice-region-selector.render-region-option.region-emoji"
						/>
					) : (
						<span
							className={styles.regionEmojiText}
							data-flx="voice.voice-region-selector.render-region-option.region-emoji-text"
						>
							{option.region.emoji}
						</span>
					)}
					<span
						className={clsx(styles.regionName, compact && styles.regionNameCompact)}
						data-flx="voice.voice-region-selector.render-region-option.region-name--2"
					>
						{option.label}
					</span>
				</div>
			);
		},
		[compact],
	);
	return (
		<div
			className={clsx(styles.regionSelectorContainer, compact && styles.regionSelectorContainerCompact)}
			data-flx="voice.voice-region-selector.region-selector-container"
		>
			<FormCombobox<string, false, RtcRegionOption>
				value={selectedValue}
				options={options}
				onChange={handleRegionSelect}
				disabled={regions.length === 0 || isChangingRegion}
				isSearchable={false}
				closeMenuOnSelect={true}
				menuPlacement="bottom"
				maxMenuHeight={compact ? 140 : 220}
				placeholder={displayName}
				density={selectDensity}
				className={compact ? styles.selectCompact : styles.select}
				renderOption={(option) => renderRegionOption(option)}
				renderValue={(option) => (option ? renderRegionOption(option as RtcRegionOption) : null)}
				data-flx="voice.voice-region-selector.select-compact.region-select"
			/>
		</div>
	);
}
