// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getCachedDesktopTroubleshootingSettings,
	getDesktopTroubleshootingSettings,
} from '@app/features/devtools/utils/DesktopTroubleshootingUtils';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import type {RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import styles from '@app/features/user/components/modals/tabs/components/CodecSelectorSection.module.css';
import {getUserSettingsTabLabel} from '@app/features/user/components/settings_utils/SettingsConstants';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	type CodecPreference,
	getCodecCapabilityReport,
	getLiveKitSupportedCodecs,
	selectAutomaticScreenShareCodec,
} from '@app/features/voice/utils/CodecCapabilityDetector';
import {
	getGpuEncoderReportSync,
	type HardwareEncodeReport,
	loadGpuEncoderReport,
} from '@app/features/voice/utils/GpuEncoderCapabilities';
import {CODEC_DISPLAY_LABEL} from '@app/features/voice/utils/ScreenShareCodecPolicy';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useState} from 'react';
import {getAutomaticDescription} from './CodecSelectorDescription';

const AUTO_DESCRIPTOR = msg({
	message: 'Automatic',
	comment: 'Label for the auto codec option in the screen-share codec radio group. Keep it concise.',
});
const RECOMMENDED_DESCRIPTOR = msg({
	message: 'Recommended',
	comment: 'Badge shown next to the auto codec option in the screen-share codec radio group. Keep it concise.',
});
const SCREEN_SHARE_VIDEO_CODEC_DESCRIPTOR = msg({
	message: 'Screen share video codec',
	comment: 'Accessible label for the screen-share codec radio group.',
});

interface CodecRadioOption extends RadioOption<CodecPreference> {
	value: CodecPreference;
}

export const CodecSelectorSection = observer(() => {
	const {i18n} = useLingui();
	const isDesktopClient = isDesktop();
	const advancedSettingsLabel = getUserSettingsTabLabel(i18n, 'advanced_settings');
	const preference = VoiceSettings.getPreferredScreenShareCodec();
	const encoderMode = VoiceSettings.getScreenShareEncoderMode();
	const [gpuReport, setGpuReport] = useState<HardwareEncodeReport | null>(() => getGpuEncoderReportSync());
	const [hardwareAccelDisabled, setHardwareAccelDisabled] = useState<boolean | null>(
		() => getCachedDesktopTroubleshootingSettings()?.disableHardwareAcceleration ?? null,
	);
	useEffect(() => {
		if (gpuReport) return;
		let cancelled = false;
		void loadGpuEncoderReport().then((report) => {
			if (!cancelled) setGpuReport(report);
		});
		return () => {
			cancelled = true;
		};
	}, [gpuReport]);
	useEffect(() => {
		if (!isDesktopClient || hardwareAccelDisabled !== null) return;
		let cancelled = false;
		void getDesktopTroubleshootingSettings().then((settings) => {
			if (!cancelled && settings) setHardwareAccelDisabled(settings.disableHardwareAcceleration);
		});
		return () => {
			cancelled = true;
		};
	}, [hardwareAccelDisabled, isDesktopClient]);
	const report = useMemo(() => getCodecCapabilityReport(), [gpuReport, hardwareAccelDisabled]);
	const liveKitCodecs = useMemo(() => getLiveKitSupportedCodecs(), []);
	const automaticSelection = useMemo(
		() => selectAutomaticScreenShareCodec(encoderMode),
		[encoderMode, gpuReport, hardwareAccelDisabled],
	);
	const options = useMemo<ReadonlyArray<CodecRadioOption>>(() => {
		const autoOption: CodecRadioOption = {
			value: 'auto',
			name: (
				<div className={styles.codecLabelRow} data-flx="user.codec-selector-section.codec-label-row">
					<span className={styles.codecLabel} data-flx="user.codec-selector-section.codec-label">
						{i18n._(AUTO_DESCRIPTOR)}
					</span>
					<span
						className={clsx(styles.codecBadge, styles.codecBadgeRecommended)}
						data-flx="user.codec-selector-section.codec-badge"
					>
						{i18n._(RECOMMENDED_DESCRIPTOR)}
					</span>
				</div>
			),
			desc: getAutomaticDescription(i18n, automaticSelection, {isDesktopClient}),
		};
		const codecOptions = liveKitCodecs.map<CodecRadioOption>((codec) => ({
			value: codec,
			name: CODEC_DISPLAY_LABEL[codec],
			disabled: !report[codec].supported,
		}));
		return [autoOption, ...codecOptions];
	}, [automaticSelection, liveKitCodecs, report, i18n.locale, isDesktopClient]);
	const handleChange = useCallback((value: CodecPreference) => {
		VoiceSettingsCommands.update({preferredScreenShareCodec: value});
	}, []);
	const openAdvancedSettingsTab = useCallback((event: React.MouseEvent<HTMLButtonElement>) => {
		event.preventDefault();
		ComponentBus.dispatch('USER_SETTINGS_TAB_SELECT', {tab: 'advanced_settings'});
	}, []);
	const hardwareAccelOff = isDesktopClient && hardwareAccelDisabled === true;
	return (
		<div className={styles.codecPicker} data-flx="user.codec-selector-section.codec-picker">
			<RadioGroup<CodecPreference>
				aria-label={i18n._(SCREEN_SHARE_VIDEO_CODEC_DESCRIPTOR)}
				options={options}
				value={preference}
				onChange={handleChange}
				className={styles.codecRadioGroup}
				data-flx="user.codec-selector-section.radio-group.change"
			/>
			{hardwareAccelOff ? (
				<p className={styles.codecHint} data-flx="user.codec-selector-section.codec-hint">
					<Trans>
						Hardware acceleration is off, so codecs will encode on your CPU.{' '}
						<button
							type="button"
							className={styles.codecAdvancedLink}
							onClick={openAdvancedSettingsTab}
							data-flx="user.codec-selector-section.codec-advanced-link.open-advanced-settings-tab.button"
						>
							Turn it back on in {advancedSettingsLabel}
						</button>
						.
					</Trans>
				</p>
			) : null}
		</div>
	);
});
