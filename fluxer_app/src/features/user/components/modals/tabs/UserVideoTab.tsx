// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	MACOS_CAMERA_PERMISSION_NAME,
	MACOS_PRIVACY_AND_SECURITY_SETTINGS_NAME,
	MACOS_SYSTEM_SETTINGS_NAME,
	PREMIUM_PRODUCT_NAME,
	PRODUCT_NAME,
} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/features/app/utils/LimitUtils';
import {GET_PREMIUM_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {getAdvancedSettingsCategorySectionId} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedSettingsCategories';
import {CompactComboboxRow} from '@app/features/user/components/modals/tabs/components/CompactComboboxRow';
import {useMediaPermission} from '@app/features/user/components/modals/tabs/hooks/useMediaPermission';
import styles from '@app/features/user/components/modals/tabs/UserVideoTab.module.css';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import {CameraPreviewModalStandalone} from '@app/features/voice/components/modals/CameraPreviewModal';
import type VoiceSettings from '@app/features/voice/state/VoiceSettings';
import type {CameraResolution, ScreenshareResolution} from '@app/features/voice/state/VoiceSettings';
import {
	resolveScreenShareFrameRate,
	type SupportedScreenShareFrameRate,
} from '@app/features/voice/utils/ScreenShareOptions';
import {buildSettingsDeviceOptions} from '@app/features/voice/utils/SettingsDeviceOptions';
import {resolveEffectiveDeviceId} from '@app/features/voice/utils/VoiceDeviceManager';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CrownIcon, GearIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const LOW_480P_LABEL = '480p';
const MEDIUM_720P_LABEL = '720p';
const HIGH_1080P_LABEL = '1080p';
const PRODUCT_NEEDS_CAMERA_ACCESS_DESCRIPTOR = msg({
	message: '{productName} needs camera access to list your devices.',
	comment: 'Camera settings notice shown when camera permission has not been granted.',
});
const UNLOCK_HD_SCREEN_SHARE_DESCRIPTOR = msg({
	message: 'Unlock HD screen share with {premiumProductName}',
	comment: 'Premium upsell title in video settings.',
});
const HIGH_FRAME_RATE_REQUIRES_PREMIUM_DESCRIPTOR = msg({
	message: 'Frame rates above 30 FPS require {premiumProductName}',
	comment: 'Video settings note shown when higher frame rates require premium.',
});
const SELF_HOSTED_VIDEO_QUALITY_LIMIT_DESCRIPTOR = msg({
	message: 'This instance currently allows screen share up to 720p at 30 FPS.',
	comment: 'Neutral video settings note shown when higher screen share quality is disabled by instance limits.',
});
const VERY_LOW_240P_LABEL = '240p';
const STANDARD_720P_LABEL = '720p';
const HIGH_DEFINITION_1080P_LABEL = '1080p';
const QUAD_HD_1440P_LABEL = '1440p';
const SOURCE_DESCRIPTOR = msg({
	message: 'Source',
	comment: 'Short label in the video tab for the native source resolution option. Keep it concise.',
});
const CAMERA_2_DESCRIPTOR = msg({
	message: 'Camera',
	comment: 'Short label in the video tab. Keep it concise.',
});
const CAMERA_QUALITY_DESCRIPTOR = msg({
	message: 'Camera quality',
	comment: 'Short label in the video tab. Keep it concise.',
});
const MIRROR_CAMERA_DESCRIPTOR = msg({
	message: 'Mirror camera',
	comment: 'Switch label in the video tab for flipping the local camera preview horizontally.',
});
const SCREEN_SHARE_QUALITY_DESCRIPTOR = msg({
	message: 'Screen share quality',
	comment: 'Short label in the video tab. Keep it concise.',
});
const ADVANCED_VIDEO_SETTINGS_DESCRIPTOR = msg({
	message: 'Advanced video settings',
	comment: 'Subsection title in the video tab. Keep it concise.',
});
const OPEN_ADVANCED_MEDIA_SETTINGS_DESCRIPTOR = msg({
	message: 'Open advanced media settings',
	comment: 'Button label in the video tab that jumps to the advanced settings media section.',
});

interface VideoTabProps {
	voiceSettings: typeof VoiceSettings;
	hasPremium: boolean;
	autoRequestPermission?: boolean;
}

export const VideoTab: React.FC<VideoTabProps> = observer(
	({voiceSettings, hasPremium: _hasPremium, autoRequestPermission = true}) => {
		const {i18n} = useLingui();
		const {videoDeviceId, cameraResolution, mirrorCamera, screenshareResolution, videoFrameRate} = voiceSettings;
		const hasHigherQuality = isLimitToggleEnabled(
			{
				feature_higher_video_quality: LimitResolver.resolve({
					key: 'feature_higher_video_quality',
					fallback: 0,
				}),
			},
			'feature_higher_video_quality',
		);
		const isSelfHosted = RuntimeConfig.isSelfHosted();
		const {
			devices,
			deviceState,
			status: permissionStatus,
			requestPermission,
		} = useMediaPermission('video', {
			autoRequest: autoRequestPermission,
		});
		const videoDeviceOptions = useMemo(
			() => buildSettingsDeviceOptions(deviceState, 'videoinput', i18n),
			[deviceState, i18n.locale],
		);
		const effectiveVideoDeviceId = resolveEffectiveDeviceId(videoDeviceId, devices) ?? 'default';
		const cameraResolutionOptions: ReadonlyArray<ComboboxOption<CameraResolution>> = [
			{value: 'low', label: LOW_480P_LABEL},
			{value: 'medium', label: MEDIUM_720P_LABEL},
			...(hasHigherQuality || !isSelfHosted
				? [
						{
							value: 'high' as const,
							label: HIGH_1080P_LABEL,
							isDisabled: !hasHigherQuality,
						},
					]
				: []),
		];
		const screenshareResolutionOptions: ReadonlyArray<ComboboxOption<ScreenshareResolution>> = [
			{value: 'low_240p', label: VERY_LOW_240P_LABEL},
			{value: 'low_480p', label: LOW_480P_LABEL},
			{
				value: 'medium',
				label: STANDARD_720P_LABEL,
			},
			...(hasHigherQuality || !isSelfHosted
				? [
						{
							value: 'high' as const,
							label: HIGH_DEFINITION_1080P_LABEL,
							isDisabled: !hasHigherQuality,
						},
						{
							value: 'ultra' as const,
							label: QUAD_HD_1440P_LABEL,
							isDisabled: !hasHigherQuality,
						},
						{
							value: 'source' as const,
							label: i18n._(SOURCE_DESCRIPTOR),
							isDisabled: !hasHigherQuality,
						},
					]
				: []),
		];
		const frameRateOptions: ReadonlyArray<ComboboxOption<SupportedScreenShareFrameRate>> = hasHigherQuality
			? [
					{value: 15, label: '15 FPS'},
					{value: 30, label: '30 FPS'},
					{value: 60, label: '60 FPS'},
				]
			: [
					{value: 15, label: '15 FPS'},
					{value: 30, label: '30 FPS'},
				];
		const resolvedVideoFrameRate = resolveScreenShareFrameRate(videoFrameRate);
		const effectiveVideoFrameRate: SupportedScreenShareFrameRate =
			!hasHigherQuality && resolvedVideoFrameRate === 60 ? 30 : resolvedVideoFrameRate;
		const handleCameraPreview = async () => {
			const granted = await requestPermission();
			if (granted) {
				ModalCommands.push(
					modal(() => (
						<CameraPreviewModalStandalone
							showEnableCameraButton={false}
							data-flx="user.video-tab.handle-camera-preview.camera-preview-modal-standalone"
						/>
					)),
				);
			}
		};
		const handleScreenshareResolutionChange = useCallback((value: ScreenshareResolution) => {
			VoiceSettingsCommands.update({screenshareResolution: value, streamingMode: 'custom'});
		}, []);
		const handleVideoFrameRateChange = useCallback((value: SupportedScreenShareFrameRate) => {
			VoiceSettingsCommands.update({videoFrameRate: value, streamingMode: 'custom'});
		}, []);
		const handleOpenAdvancedVideoSettings = useCallback(() => {
			ComponentDispatch.dispatch('USER_SETTINGS_TAB_SELECT', {
				tab: 'advanced_settings',
				section: getAdvancedSettingsCategorySectionId('media'),
			});
		}, []);
		return (
			<div className={styles.content} data-flx="user.video-tab.content">
				{devices.length === 0 && permissionStatus !== 'loading' && permissionStatus !== 'granted' ? (
					<div className={styles.deviceNotice} data-flx="user.video-tab.device-notice">
						<div className={styles.deviceNoticeText} data-flx="user.video-tab.device-notice-text">
							<div className={styles.deviceNoticeTitle} data-flx="user.video-tab.device-notice-title">
								<Trans>No camera found</Trans>
							</div>
							<p className={styles.deviceNoticeDescription} data-flx="user.video-tab.device-notice-description">
								{permissionStatus === 'denied' ? (
									<Trans>
										Allow {PRODUCT_NAME} to access your camera in {MACOS_SYSTEM_SETTINGS_NAME} →{' '}
										{MACOS_PRIVACY_AND_SECURITY_SETTINGS_NAME} → {MACOS_CAMERA_PERMISSION_NAME}.
									</Trans>
								) : (
									i18n._(PRODUCT_NEEDS_CAMERA_ACCESS_DESCRIPTOR, {productName: PRODUCT_NAME})
								)}
							</p>
						</div>
						<Button
							variant="secondary"
							small={true}
							onClick={() => {
								void requestPermission();
							}}
							data-flx="user.video-tab.button"
						>
							<Trans>Allow camera</Trans>
						</Button>
					</div>
				) : null}
				<div className={styles.controlGroup} data-flx="user.video-tab.camera-settings-group">
					<CompactComboboxRow
						label={i18n._(CAMERA_2_DESCRIPTOR)}
						value={effectiveVideoDeviceId}
						options={videoDeviceOptions}
						onChange={(value) => VoiceSettingsCommands.update({videoDeviceId: value})}
						controlWidth="wide"
						menuMinWidth={280}
						dataFlx="user.video-tab.select.update"
						data-flx="user.user-video-tab.video-tab.compact-combobox-row.update"
					/>
					<div className={styles.cameraControlGroup} data-flx="user.video-tab.camera-control-group">
						<Switch
							label={i18n._(MIRROR_CAMERA_DESCRIPTOR)}
							value={mirrorCamera}
							onChange={(value) => VoiceSettingsCommands.update({mirrorCamera: value})}
							data-flx="user.user-video-tab.video-tab.switch.update"
						/>
						<Button
							variant="primary"
							fitContent
							className={styles.actionButton}
							onClick={handleCameraPreview}
							data-flx="user.video-tab.action-button.camera-preview"
						>
							<Trans>Start camera test and configure effects</Trans>
						</Button>
					</div>
					<CompactComboboxRow<CameraResolution>
						label={i18n._(CAMERA_QUALITY_DESCRIPTOR)}
						options={cameraResolutionOptions}
						value={cameraResolution}
						onChange={(value) => VoiceSettingsCommands.update({cameraResolution: value})}
						isSearchable={false}
						controlWidth="small"
						dataFlx="user.video-tab.select.camera-resolution-update"
						data-flx="user.user-video-tab.video-tab.compact-combobox-row.update--2"
					/>
				</div>
				<div className={styles.controlGroup} data-flx="user.video-tab.screen-sharing-group">
					<CompactComboboxRow<ScreenshareResolution>
						label={i18n._(SCREEN_SHARE_QUALITY_DESCRIPTOR)}
						options={screenshareResolutionOptions}
						value={screenshareResolution}
						onChange={handleScreenshareResolutionChange}
						isSearchable={false}
						controlWidth="small"
						dataFlx="user.video-tab.select.screenshare-resolution-change"
						data-flx="user.user-video-tab.video-tab.compact-combobox-row.screenshare-resolution-change"
					/>
					{!hasHigherQuality && !isSelfHosted && (
						<div className={styles.premiumCard} data-flx="user.video-tab.premium-card">
							<div className={styles.premiumHeader} data-flx="user.video-tab.premium-header">
								<CrownIcon
									weight="fill"
									size={remFromPx(18)}
									className={styles.premiumIcon}
									data-flx="user.video-tab.premium-icon"
								/>
								<span className={styles.premiumTitle} data-flx="user.video-tab.premium-title">
									{i18n._(UNLOCK_HD_SCREEN_SHARE_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
								</span>
							</div>
							<p className={styles.premiumDescription} data-flx="user.video-tab.premium-description">
								<Trans>
									Share your screen in high definition (1080p), quad HD (1440p), or native source resolution, at up to
									60 FPS.
								</Trans>
							</p>
							<Button
								variant="secondary"
								small={true}
								onClick={() => PremiumModalCommands.open()}
								data-flx="user.video-tab.button.open"
							>
								{i18n._(GET_PREMIUM_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
							</Button>
						</div>
					)}
					<CompactComboboxRow<SupportedScreenShareFrameRate>
						label={<Trans>Frame rate</Trans>}
						options={frameRateOptions}
						value={effectiveVideoFrameRate}
						onChange={handleVideoFrameRateChange}
						isSearchable={false}
						controlWidth="small"
						dataFlx="user.video-tab.select.frame-rate-change"
						data-flx="user.user-video-tab.video-tab.compact-combobox-row.video-frame-rate-change"
					/>
					{!hasHigherQuality && !isSelfHosted && (
						<div className={styles.frameRateNote} data-flx="user.video-tab.frame-rate-note">
							<CrownIcon
								weight="fill"
								size={remFromPx(14)}
								className={styles.frameRateIcon}
								data-flx="user.video-tab.frame-rate-icon"
							/>
							{i18n._(HIGH_FRAME_RATE_REQUIRES_PREMIUM_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
						</div>
					)}
					{!hasHigherQuality && isSelfHosted && (
						<div className={styles.frameRateNote} data-flx="user.video-tab.instance-limit-note">
							{i18n._(SELF_HOSTED_VIDEO_QUALITY_LIMIT_DESCRIPTOR)}
						</div>
					)}
				</div>
				<div className={styles.actionRow} data-flx="user.video-tab.advanced-video-settings-section">
					<span className={styles.actionLabel} data-flx="user.video-tab.advanced-video-settings-label">
						{i18n._(ADVANCED_VIDEO_SETTINGS_DESCRIPTOR)}
					</span>
					<Button
						variant="secondary"
						fitContent
						leftIcon={
							<GearIcon size={remFromPx(16)} weight="bold" data-flx="user.video-tab.advanced-settings.gear-icon" />
						}
						onClick={handleOpenAdvancedVideoSettings}
						data-flx="user.video-tab.button.open-advanced-video-settings"
					>
						{i18n._(OPEN_ADVANCED_MEDIA_SETTINGS_DESCRIPTOR)}
					</Button>
				</div>
			</div>
		);
	},
);
