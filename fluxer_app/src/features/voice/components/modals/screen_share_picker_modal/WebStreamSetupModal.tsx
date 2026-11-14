// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {type SegmentedTab, SegmentedTabs} from '@app/features/ui/segmented_tabs/SegmentedTabs';
import {supportsDesktopScreenShareAudioCapture} from '@app/features/ui/utils/NativeUtils';
import PrivacyPreferences from '@app/features/user/state/PrivacyPreferences';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import styles from '@app/features/voice/components/modals/screen_share_picker_modal/WebStreamSetupModal.module.css';
import {useVideoQualityCaps} from '@app/features/voice/components/StreamSettingsMenuContent';
import {
	type OfferedScreenShareResolution,
	type StreamSettingsQualityWrite,
	selectStreamSettingsQualityMenuState,
} from '@app/features/voice/components/StreamSettingsMenuContentStateMachine';
import VoiceSettings, {type StreamingMode} from '@app/features/voice/state/VoiceSettings';
import {
	resolveScreenShareFrameRate,
	resolveScreenShareTarget,
	type ScreenShareQualityInput,
} from '@app/features/voice/utils/ScreenShareOptions';
import {startConfiguredDisplayScreenShare} from '@app/features/voice/utils/ScreenShareStartFlow';
import {
	CUSTOM_DESCRIPTOR,
	GAMING_DESCRIPTOR,
	SCREENSHARE_DESCRIPTOR,
	SCREEN_SHARE_STATUS_SOURCE_RESOLUTION_DESCRIPTOR as SOURCE_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceSharedMessageDescriptors';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {AtomIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useState} from 'react';

const logger = new Logger('WebStreamSetupModal');

const TITLE_DESCRIPTOR = msg({
	message: 'Share your screen',
	comment:
		'Title of the modal shown on web before the browser screen picker opens. The user picks stream quality here first.',
});
const RESOLUTION_DESCRIPTOR = msg({
	message: 'Resolution',
	comment: 'Section label in the web stream setup modal for the stream resolution options.',
});
const FRAME_RATE_DESCRIPTOR = msg({
	message: 'Frame rate',
	comment: 'Section label in the web stream setup modal for the stream frame rate options, in frames per second.',
});
const PRESET_DESCRIPTOR = msg({
	message: 'Preset',
	comment:
		'Section label in the web stream setup modal. Picks the Gaming preset (motion), the Screen share preset (sharp text) or custom numbers.',
});
const SHARE_AUDIO_DESCRIPTOR = msg({
	message: 'Share audio',
	comment: 'Switch label in the web stream setup modal.',
});
const SHARE_AUDIO_HINT_DESCRIPTOR = msg({
	message: 'Your browser lets you choose whether to include audio when you pick what to share.',
	comment:
		'Description under the "Share audio" switch in the web stream setup modal. The browser picker owns the final audio choice.',
});
const PREVIEW_IMAGE_DESCRIPTOR = msg({
	message: 'Share a preview image',
	comment:
		'Switch label in the web stream setup modal. When on, Fluxer uploads a still image of the stream so people can see it before watching.',
});
const PREVIEW_IMAGE_HINT_DESCRIPTOR = msg({
	message: 'People can see a still image of what you share before they start watching.',
	comment: 'Description under the "Share a preview image" switch in the web stream setup modal.',
});
const SUMMARY_DESCRIPTOR = msg({
	message: '{resolution} at {fps} FPS',
	comment:
		'Footer summary under the preset name in the web stream setup modal. {resolution} is a technical token like 1080p. {fps} is a whole number of frames per second, and FPS is a technical token.',
});
const SUMMARY_SOURCE_DESCRIPTOR = msg({
	message: "Your screen's resolution at {fps} FPS",
	comment:
		'Footer summary under the preset name in the web stream setup modal when the Source resolution is picked. {fps} is a whole number of frames per second, and FPS is a technical token.',
});
const START_SHARING_DESCRIPTOR = msg({
	message: 'Start sharing',
	comment: "Primary button in the web stream setup modal. Opens the browser's screen picker and starts sharing.",
});

const RESOLUTION_LABELS: Record<Exclude<OfferedScreenShareResolution, 'source'>, string> = {
	low_480p: '480',
	medium: '720',
	high: '1080',
	ultra: '1440',
};

function getResolutionLabel(i18n: I18n, resolution: OfferedScreenShareResolution): string {
	return resolution === 'source' ? i18n._(SOURCE_DESCRIPTOR) : RESOLUTION_LABELS[resolution];
}

function getModeTitle(i18n: I18n, mode: StreamingMode): string {
	if (mode === 'gaming') return i18n._(GAMING_DESCRIPTOR);
	if (mode === 'screenshare') return i18n._(SCREENSHARE_DESCRIPTOR);
	return i18n._(CUSTOM_DESCRIPTOR);
}

function applyQualityWrite(write: StreamSettingsQualityWrite): void {
	if (write.kind === 'premium') {
		PremiumModalCommands.open();
		return;
	}
	if (write.kind === 'write') {
		VoiceSettingsCommands.update(write.patch);
	}
}

const PremiumIcon = () => (
	<AtomIcon
		weight="fill"
		className={styles.premiumIcon}
		aria-hidden={true}
		data-flx="voice.web-stream-setup-modal.premium-icon"
	/>
);

export const WebStreamSetupModal = observer(() => {
	const {i18n} = useLingui();
	const caps = useVideoQualityCaps();
	const showPremiumFeatures = shouldShowPremiumFeatures();
	const supportsAudio = supportsDesktopScreenShareAudioCapture();
	const [shareAudio, setShareAudio] = useState(true);
	const [starting, setStarting] = useState(false);
	const quality: ScreenShareQualityInput = {
		mode: VoiceSettings.getStreamingMode(),
		storedResolution: VoiceSettings.getScreenshareResolution(),
		storedFrameRate: resolveScreenShareFrameRate(VoiceSettings.getVideoFrameRate()),
		caps,
		context: 'display',
	};
	const target = resolveScreenShareTarget({
		...quality,
		sourceDimensions: null,
		hintSetting: VoiceSettings.getScreenShareContentHint(),
		bitrate: VoiceSettings.getScreenShareBitrateBudget(),
	});
	const menuState = selectStreamSettingsQualityMenuState({quality, target, showPremiumFeatures});
	const resolutionTabs: Array<SegmentedTab<OfferedScreenShareResolution>> = menuState.resolutions.map((option) => ({
		id: option.value,
		label: getResolutionLabel(i18n, option.value),
		icon:
			showPremiumFeatures && option.locked ? (
				<PremiumIcon data-flx="voice.screen-share-picker-modal.web-stream-setup-modal.resolution-tabs.premium-icon" />
			) : undefined,
	}));
	const frameRateTabs: Array<SegmentedTab<string>> = menuState.frameRates.map((option) => ({
		id: String(option.value),
		label: i18n.number(option.value),
		icon:
			showPremiumFeatures && option.locked ? (
				<PremiumIcon data-flx="voice.screen-share-picker-modal.web-stream-setup-modal.frame-rate-tabs.premium-icon" />
			) : undefined,
	}));
	const selectedResolution = menuState.resolutions.find((option) => option.selected);
	const selectedFrameRate = menuState.frameRates.find((option) => option.selected);
	const modeTabs: Array<SegmentedTab<StreamingMode>> = [
		{id: 'gaming', label: getModeTitle(i18n, 'gaming')},
		{id: 'screenshare', label: getModeTitle(i18n, 'screenshare')},
		{id: 'custom', label: getModeTitle(i18n, 'custom')},
	];
	const handleResolutionChange = useCallback(
		(value: OfferedScreenShareResolution) => {
			const option = menuState.resolutions.find((candidate) => candidate.value === value);
			if (option) applyQualityWrite(option.write);
		},
		[menuState.resolutions],
	);
	const handleFrameRateChange = useCallback(
		(value: string) => {
			const option = menuState.frameRates.find((candidate) => String(candidate.value) === value);
			if (option) applyQualityWrite(option.write);
		},
		[menuState.frameRates],
	);
	const handleModeChange = useCallback(
		(mode: StreamingMode) => {
			if (mode === target.mode) return;
			VoiceSettingsCommands.update({streamingMode: mode});
		},
		[target.mode],
	);
	const previewsEnabled = !PrivacyPreferences.getDisableStreamPreviews();
	const handlePreviewsToggle = useCallback((enabled: boolean) => {
		PrivacyPreferences.setDisableStreamPreviews(!enabled);
	}, []);
	const handleStream = useCallback(async () => {
		if (starting) return;
		setStarting(true);
		try {
			const started = await startConfiguredDisplayScreenShare(null, {
				includeAudio: supportsAudio && shareAudio ? undefined : false,
			});
			if (started) ModalCommands.pop();
		} catch (error) {
			logger.warn('Web screen share start failed', {error});
		} finally {
			setStarting(false);
		}
	}, [shareAudio, starting, supportsAudio]);
	const fps = i18n.number(target.frameRate);
	const summary =
		selectedResolution == null || selectedResolution.value === 'source'
			? i18n._(SUMMARY_SOURCE_DESCRIPTOR, {fps})
			: i18n._(SUMMARY_DESCRIPTOR, {resolution: `${RESOLUTION_LABELS[selectedResolution.value]}p`, fps});
	return (
		<Modal.Root
			size="large"
			centered
			onClose={ModalCommands.pop}
			className={styles.root}
			data-flx="voice.web-stream-setup-modal.root"
		>
			<Modal.Header title={i18n._(TITLE_DESCRIPTOR)} data-flx="voice.web-stream-setup-modal.header" />
			<Modal.Content data-flx="voice.web-stream-setup-modal.content">
				<div className={styles.body} data-flx="voice.web-stream-setup-modal.body">
					<div className={styles.qualityRow} data-flx="voice.web-stream-setup-modal.quality-row">
						<section
							className={clsx(styles.section, styles.resolutionSection)}
							data-flx="voice.web-stream-setup-modal.resolution"
						>
							<div className={styles.sectionLabel} data-flx="voice.web-stream-setup-modal.resolution-label">
								{i18n._(RESOLUTION_DESCRIPTOR)}
							</div>
							<SegmentedTabs
								semantics="radio"
								ariaLabel={i18n._(RESOLUTION_DESCRIPTOR)}
								tabs={resolutionTabs}
								selectedTab={selectedResolution?.value ?? resolutionTabs[0]?.id ?? 'medium'}
								onTabChange={handleResolutionChange}
								className={styles.segmented}
								data-flx="voice.screen-share-picker-modal.web-stream-setup-modal.segmented"
							/>
						</section>
						<section
							className={clsx(styles.section, styles.frameRateSection)}
							data-flx="voice.web-stream-setup-modal.frame-rate"
						>
							<div className={styles.sectionLabel} data-flx="voice.web-stream-setup-modal.frame-rate-label">
								{i18n._(FRAME_RATE_DESCRIPTOR)}
							</div>
							<SegmentedTabs
								semantics="radio"
								ariaLabel={i18n._(FRAME_RATE_DESCRIPTOR)}
								tabs={frameRateTabs}
								selectedTab={String(selectedFrameRate?.value ?? target.frameRate)}
								onTabChange={handleFrameRateChange}
								className={styles.segmented}
								data-flx="voice.screen-share-picker-modal.web-stream-setup-modal.segmented--2"
							/>
						</section>
					</div>
					<section className={styles.section} data-flx="voice.web-stream-setup-modal.mode">
						<div className={styles.sectionLabel} data-flx="voice.web-stream-setup-modal.mode-label">
							{i18n._(PRESET_DESCRIPTOR)}
						</div>
						<SegmentedTabs
							semantics="radio"
							ariaLabel={i18n._(PRESET_DESCRIPTOR)}
							tabs={modeTabs}
							selectedTab={target.mode}
							onTabChange={handleModeChange}
							className={styles.segmented}
							data-flx="voice.screen-share-picker-modal.web-stream-setup-modal.segmented--3"
						/>
					</section>
					<div className={styles.switches} data-flx="voice.web-stream-setup-modal.switches">
						{supportsAudio && (
							<Switch
								label={i18n._(SHARE_AUDIO_DESCRIPTOR)}
								description={i18n._(SHARE_AUDIO_HINT_DESCRIPTOR)}
								value={shareAudio}
								onChange={setShareAudio}
								data-flx="voice.web-stream-setup-modal.share-audio"
							/>
						)}
						<Switch
							label={i18n._(PREVIEW_IMAGE_DESCRIPTOR)}
							description={i18n._(PREVIEW_IMAGE_HINT_DESCRIPTOR)}
							value={previewsEnabled}
							onChange={handlePreviewsToggle}
							data-flx="voice.web-stream-setup-modal.show-previews"
						/>
					</div>
				</div>
			</Modal.Content>
			<Modal.Footer className={styles.footer} data-flx="voice.web-stream-setup-modal.footer">
				<div className={styles.summary} data-flx="voice.web-stream-setup-modal.summary">
					<div className={styles.summaryTitle} data-flx="voice.web-stream-setup-modal.summary-title">
						{getModeTitle(i18n, target.mode)}
					</div>
					<div
						className={styles.summaryDetails}
						aria-live="polite"
						data-flx="voice.web-stream-setup-modal.summary-details"
					>
						{summary}
					</div>
				</div>
				<Button
					submitting={starting}
					onClick={() => void handleStream()}
					data-flx="voice.web-stream-setup-modal.button.stream"
				>
					{i18n._(START_SHARING_DESCRIPTOR)}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
