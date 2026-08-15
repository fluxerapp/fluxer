// SPDX-License-Identifier: AGPL-3.0-or-later

import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/features/app/utils/LimitUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {CheckboxItem, MenuGroupLabel} from '@app/features/ui/action_menu/ContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItemRadio} from '@app/features/ui/action_menu/MenuItemRadio';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import {getElectronAPI, supportsDesktopScreenShareAudioCapture} from '@app/features/ui/utils/NativeUtils';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import {AudioSourcePickerLinuxSubmenu} from '@app/features/voice/components/AudioSourcePickerLinux';
import styles from '@app/features/voice/components/StreamSettingsMenuContent.module.css';
import {
	type StreamSettingsAudioControlLabelKey,
	selectStreamSettingsAudioMenuState,
} from '@app/features/voice/components/StreamSettingsMenuContentStateMachine';
import AdaptiveScreenShareEngine from '@app/features/voice/engine/AdaptiveScreenShareEngine';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import ScreenShareCodecNegotiation from '@app/features/voice/engine/ScreenShareCodecNegotiation';
import {useStoreVersion} from '@app/features/voice/engine/Store';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {useMediaDevices} from '@app/features/voice/hooks/useMediaDevices';
import VoiceSettings, {type ScreenshareResolution, type StreamingMode} from '@app/features/voice/state/VoiceSettings';
import {resolveScreenShareContentHintForContext} from '@app/features/voice/utils/CodecCapabilityDetector';
import {getNativeAudioAvailabilityCached} from '@app/features/voice/utils/NativeAudioCaptureBridge';
import {
	canRestartDisplayShareWithoutPreselectedSource,
	type DisplayShareEnvironment,
	usesNativeDisplayShareAudioSelection,
} from '@app/features/voice/utils/ScreenShareEnvironment';
import {
	buildScreenShareOptions,
	normaliseResolutionForContext,
	normaliseStreamingModeForContext,
	resolveScreenShareFrameRate,
	resolveStreamingModeSettings,
	type ScreenShareContext,
	type SupportedScreenShareFrameRate,
} from '@app/features/voice/utils/ScreenShareOptions';
import {
	reconfigureActiveLinuxScreenShareAudioLink,
	stopActiveLinuxScreenShareAudioLink,
} from '@app/features/voice/utils/ScreenShareStartFlow';
import {executeScreenShareOperation} from '@app/features/voice/utils/ScreenShareUtils';
import {
	isLinuxDesktopAudioShare,
	type StreamSettingsShareContext,
	shouldReconfigureLinuxAudioForActiveStreamSettings,
} from '@app/features/voice/utils/StreamSettingsUpdatePolicy';
import {formatVoiceAudioDeviceLabel} from '@app/features/voice/utils/VoiceMessageDescriptors';
import type {NativeAudioAvailability} from '@app/types/electron.d';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CrownSimpleIcon} from '@phosphor-icons/react';
import type {Track} from 'livekit-client';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useState} from 'react';

const GAMING_DESCRIPTOR = msg({
	message: 'Gaming',
	comment: 'Streaming preset label in the stream settings menu. Higher frame rate, optimized for video games.',
});
const FLUID_MOTION_AT_1440P_60_FPS_DESCRIPTOR = msg({
	message: 'Fluid motion at 1440p 60 FPS',
	comment:
		'Description for the high-tier Gaming streaming preset (Plutonium). Resolution and frame rate are technical tokens.',
});
const FLUID_MOTION_AT_720P_30_FPS_DESCRIPTOR = msg({
	message: 'Fluid motion at 720p 30 FPS',
	comment: 'Description for the free-tier Gaming streaming preset. Resolution and frame rate are technical tokens.',
});
const SCREENSHARE_DESCRIPTOR = msg({
	message: 'Screen share',
	comment: 'Streaming preset label in the stream settings menu. Optimized for sharp text in screen shares.',
});
const RAZOR_SHARP_TEXT_AT_NATIVE_SOURCE_15_FPS_DESCRIPTOR = msg({
	message: 'Razor-sharp text at native source, 15 FPS',
	comment:
		'Description for the high-tier Screen share streaming preset (Plutonium). Source resolution and frame rate are technical tokens.',
});
const SHARPER_TEXT_AT_720P_15_FPS_DESCRIPTOR = msg({
	message: 'Sharper text at 720p, 15 FPS',
	comment:
		'Description for the free-tier Screen share streaming preset. Resolution and frame rate are technical tokens.',
});
const CUSTOM_DESCRIPTOR = msg({
	message: 'Custom',
	comment: 'Streaming preset label in the stream settings menu. Lets user pick resolution and frame rate manually.',
});
const DIAL_IN_YOUR_OWN_NUMBERS_DESCRIPTOR = msg({
	message: 'Dial in your own numbers',
	comment: 'Description for the Custom streaming preset. Lightly playful prose; keep tone casual.',
});
const SOURCE_DESCRIPTOR = msg({
	message: 'Source',
	comment: 'Resolution option in the stream settings menu meaning the original source resolution (no downscale).',
});
const MESSAGE_15_FPS_DESCRIPTOR = msg({
	message: '15 FPS',
	comment: 'Frame rate option label in the stream settings menu. FPS is a technical token.',
});
const MESSAGE_30_FPS_DESCRIPTOR = msg({
	message: '30 FPS',
	comment: 'Frame rate option label in the stream settings menu. FPS is a technical token.',
});
const MESSAGE_60_FPS_DESCRIPTOR = msg({
	message: '60 FPS',
	comment: 'Frame rate option label in the stream settings menu. FPS is a technical token.',
});
const STREAMING_MODE_DESCRIPTOR = msg({
	message: 'Streaming mode',
	comment: 'Section header in the stream settings menu. Picks between Gaming / Screen share / Custom presets.',
});
const RESOLUTION_DESCRIPTOR = msg({
	message: 'Resolution',
	comment: 'Section header in the stream settings menu for resolution options.',
});
const FRAMERATE_DESCRIPTOR = msg({
	message: 'Framerate',
	comment: 'Section header in the stream settings menu for frame rate options.',
});
const AUDIO_DEVICE_DESCRIPTOR = msg({
	message: 'Audio device',
	comment: 'Section header in the stream settings menu. Selects the audio capture device for the share.',
});
const SYSTEM_DEFAULT_DESCRIPTOR = msg({
	message: 'System default',
	comment: 'Audio device option label meaning the OS default audio input device.',
});
const UNNAMED_INPUT_DESCRIPTOR = msg({
	message: 'Unnamed input',
	comment: 'Fallback label for an audio input device whose name is not reported by the OS.',
});
const AUDIO_SETTINGS_DESCRIPTOR = msg({
	message: 'Audio settings',
	comment: 'Section header in the stream settings menu grouping audio-related toggles.',
});
const ADAPTIVE_QUALITY_DESCRIPTOR = msg({
	message: 'Adaptive quality',
	comment:
		'Toggle label in the stream settings menu. When enabled, productName can lower screen-share resolution if the encoder is CPU or bandwidth limited.',
});
const ADAPTIVE_QUALITY_ACTIVE_DESCRIPTOR = msg({
	message: 'Adjusted to {resolution} {frameRate} FPS',
	comment:
		'Inline adaptive-quality status in the stream settings menu. Shows the current automatically lowered resolution and frame rate.',
});
const logger = new Logger('StreamSettingsMenuContent');
const SCREEN_SHARE_AUDIO_SOURCE = VoiceTrackSource.ScreenShareAudio as Track.Source;

interface PushActiveStreamSettingsOptions {
	audioSettingsChanged?: boolean;
}

interface Option<T> {
	value: T;
	label: string;
	isPremium: boolean;
	description?: string;
}

const PremiumBadge = () => (
	<span
		aria-hidden={true}
		className={styles.premiumBadge}
		data-flx="voice.stream-settings-menu-content.premium-badge.premium-badge"
	>
		<CrownSimpleIcon
			weight="fill"
			size={remFromPx(12)}
			data-flx="voice.stream-settings-menu-content.premium-badge.crown-simple-icon"
		/>
	</span>
);

function useHasHigherVideoQuality(): boolean {
	return useMemo(
		() =>
			isLimitToggleEnabled(
				{
					feature_higher_video_quality: LimitResolver.resolve({
						key: 'feature_higher_video_quality',
						fallback: 0,
					}),
				},
				'feature_higher_video_quality',
			),
		[],
	);
}

function supportsStreamAudioCapture(shareContext: StreamSettingsShareContext): boolean {
	if (shareContext === 'device') {
		return typeof navigator !== 'undefined' && Boolean(navigator.mediaDevices?.getUserMedia);
	}
	return supportsDesktopScreenShareAudioCapture();
}

function getScreenShareContext(shareContext: StreamSettingsShareContext): ScreenShareContext {
	return shareContext === 'device' ? 'device' : 'display';
}

function getPreferredDisplaySurface(shareContext: StreamSettingsShareContext): 'window' | 'monitor' | undefined {
	if (shareContext === 'app') return 'window';
	if (shareContext === 'display') return 'monitor';
	return undefined;
}

export async function pushActiveStreamSettings(
	shareContext: StreamSettingsShareContext,
	displayShareEnvironment: DisplayShareEnvironment,
	hasHigherVideoQuality: boolean,
	options: PushActiveStreamSettingsOptions = {},
): Promise<void> {
	const mode = VoiceSettings.getStreamingMode();
	const screenShareContext = getScreenShareContext(shareContext);
	const normalisedMode = normaliseStreamingModeForContext(mode, screenShareContext);
	const normalisedResolution = normaliseResolutionForContext(
		VoiceSettings.getScreenshareResolution(),
		screenShareContext,
		hasHigherVideoQuality,
	);
	const {resolution, frameRate} = resolveStreamingModeSettings(
		normalisedMode,
		normalisedResolution,
		VoiceSettings.getVideoFrameRate(),
		hasHigherVideoQuality,
	);
	const preferredDisplaySurface = getPreferredDisplaySurface(shareContext);
	const preferredScreenShareCodecPreference = VoiceSettings.getPreferredScreenShareCodec();
	const preferredVideoCodec = ScreenShareCodecNegotiation.selectScreenShareCodec(preferredScreenShareCodecPreference);
	const contentHint = resolveScreenShareContentHintForContext(
		VoiceSettings.getScreenShareContentHintOverride(),
		preferredVideoCodec,
		shareContext,
		normalisedMode,
	);
	const canControlAudio = supportsStreamAudioCapture(shareContext);
	const includeAudio =
		canControlAudio &&
		(shareContext === 'app'
			? VoiceSettings.getShareAppAudio()
			: shareContext === 'device'
				? VoiceSettings.getShareDeviceAudio()
				: VoiceSettings.getShareDesktopAudio());
	const {captureOptions, publishOptions} = buildScreenShareOptions({
		resolution,
		frameRate,
		includeAudio,
		streamingMode: normalisedMode,
		contentHint,
		maxBitrateBps: VoiceSettings.getScreenShareMaxBitrateBpsOverride(),
		preferredDisplaySurface,
	});
	if (preferredScreenShareCodecPreference !== 'auto') {
		publishOptions.videoCodec = preferredVideoCodec;
	}
	const localParticipant = MediaEngine.room?.localParticipant ?? null;
	const hasActiveScreenShareAudioPublication = localParticipant?.getTrackPublication(SCREEN_SHARE_AUDIO_SOURCE) != null;
	const linuxDesktopAudioShare = isLinuxDesktopAudioShare({
		platform: getElectronAPI()?.platform,
		shareContext,
	});
	const shouldRestartToEnableDisplayAudio =
		shareContext === 'display' &&
		!linuxDesktopAudioShare &&
		usesNativeDisplayShareAudioSelection(displayShareEnvironment) &&
		canRestartDisplayShareWithoutPreselectedSource(displayShareEnvironment) &&
		includeAudio &&
		!hasActiveScreenShareAudioPublication;
	if (shouldRestartToEnableDisplayAudio) {
		try {
			await MediaEngine.setScreenShareEnabled(
				true,
				{
					...captureOptions,
					playSound: false,
					restartIfEnabled: true,
				},
				publishOptions,
			);
		} catch (error) {
			logger.warn('Failed to restart active screen share with audio enabled', error);
		}
		return;
	}
	const activeCaptureOptions = canControlAudio
		? {...captureOptions, contentHint}
		: {contentHint, resolution: captureOptions.resolution};
	if (
		shouldReconfigureLinuxAudioForActiveStreamSettings({
			platform: getElectronAPI()?.platform,
			shareContext,
			audioSettingsChanged: options.audioSettingsChanged,
		})
	) {
		let linuxAudioLinkUpdated = true;
		try {
			if (includeAudio) {
				linuxAudioLinkUpdated = await reconfigureActiveLinuxScreenShareAudioLink();
			} else {
				linuxAudioLinkUpdated = await stopActiveLinuxScreenShareAudioLink();
			}
		} catch (error) {
			linuxAudioLinkUpdated = false;
			logger.warn('Failed to update active Linux screen share audio link', error);
		}
		if (includeAudio && !linuxAudioLinkUpdated) {
			logger.warn('Linux screen-share audio link could not be updated; keeping video-only', {
				platform: getElectronAPI()?.platform ?? null,
				sourceMode: VoiceSettings.getScreenShareAudioSourceMode(),
			});
		}
	}
	try {
		await MediaEngine.updateActiveScreenShareSettings(activeCaptureOptions, publishOptions);
		AdaptiveScreenShareEngine.start(MediaEngine.room);
	} catch (error) {
		logger.warn('Failed to push updated stream settings to the active share', error);
	}
}

interface StreamSettingsMenuContentProps {
	applyToLiveStream?: boolean;
	shareContext?: StreamSettingsShareContext;
	displayShareEnvironment: DisplayShareEnvironment;
}

export const StreamSettingsMenuContent = observer(
	({applyToLiveStream = true, shareContext = 'display', displayShareEnvironment}: StreamSettingsMenuContentProps) => {
		const {i18n} = useLingui();
		useMediaEngineVersion();
		const hasHigherVideoQuality = useHasHigherVideoQuality();
		const showPremiumFeatures = shouldShowPremiumFeatures();
		const isDeviceShare = shareContext === 'device';
		const isAppShare = shareContext === 'app';
		const {inputDevices} = useMediaDevices({autoRefresh: true, requestPermissions: false});
		const currentMode = VoiceSettings.getStreamingMode();
		const currentResolution = VoiceSettings.getScreenshareResolution();
		const currentFrameRate = resolveScreenShareFrameRate(VoiceSettings.getVideoFrameRate());
		const captureAudioEnabled = isAppShare
			? VoiceSettings.getShareAppAudio()
			: isDeviceShare
				? VoiceSettings.getShareDeviceAudio()
				: VoiceSettings.getShareDesktopAudio();
		const currentHideStreamPreview = VoiceSettings.getHideStreamPreview();
		const currentAudioDeviceId = VoiceSettings.getScreenShareAudioDeviceId();
		const effectiveAudioDeviceId = VoiceSettings.getEffectiveScreenShareAudioDeviceId();
		const supportsStreamAudio = supportsStreamAudioCapture(shareContext);
		const hasLiveScreenShareAudioPublication =
			MediaEngine.room?.localParticipant?.getTrackPublication(SCREEN_SHARE_AUDIO_SOURCE) != null;
		const [nativeAudioAvailability, setNativeAudioAvailability] = useState<NativeAudioAvailability | null>(null);
		useEffect(() => {
			let cancelled = false;
			void getNativeAudioAvailabilityCached().then((availability) => {
				if (!cancelled) setNativeAudioAvailability(availability);
			});
			return () => {
				cancelled = true;
			};
		}, []);
		const platform = getElectronAPI()?.platform;
		const audioMenuState = useMemo(
			() =>
				selectStreamSettingsAudioMenuState({
					applyToLiveStream,
					shareContext,
					displayShareEnvironment,
					supportsStreamAudio,
					captureAudioEnabled,
					hasLiveScreenShareAudioPublication,
					nativeAudioAvailability,
					platform,
				}),
			[
				applyToLiveStream,
				captureAudioEnabled,
				displayShareEnvironment,
				hasLiveScreenShareAudioPublication,
				nativeAudioAvailability,
				platform,
				shareContext,
				supportsStreamAudio,
			],
		);
		const renderAudioCaptureLabel = (labelKey: StreamSettingsAudioControlLabelKey) => {
			if (labelKey === 'captureDeviceAudio') return <Trans>Capture device audio</Trans>;
			if (labelKey === 'captureAppAudio') return <Trans>Capture app audio</Trans>;
			return <Trans>Capture desktop audio</Trans>;
		};
		const modeOptions: Array<Option<StreamingMode>> = useMemo(() => {
			const modes: Array<Option<StreamingMode>> = [
				{
					value: 'gaming',
					label: i18n._(GAMING_DESCRIPTOR),
					description: hasHigherVideoQuality
						? i18n._(FLUID_MOTION_AT_1440P_60_FPS_DESCRIPTOR)
						: i18n._(FLUID_MOTION_AT_720P_30_FPS_DESCRIPTOR),
					isPremium: false,
				},
			];
			if (!isDeviceShare) {
				modes.push({
					value: 'screenshare',
					label: i18n._(SCREENSHARE_DESCRIPTOR),
					description: hasHigherVideoQuality
						? i18n._(RAZOR_SHARP_TEXT_AT_NATIVE_SOURCE_15_FPS_DESCRIPTOR)
						: i18n._(SHARPER_TEXT_AT_720P_15_FPS_DESCRIPTOR),
					isPremium: false,
				});
			}
			modes.push({
				value: 'custom',
				label: i18n._(CUSTOM_DESCRIPTOR),
				description: i18n._(DIAL_IN_YOUR_OWN_NUMBERS_DESCRIPTOR),
				isPremium: false,
			});
			return modes;
		}, [isDeviceShare, hasHigherVideoQuality, i18n.locale]);
		const resolutionOptions: Array<Option<ScreenshareResolution>> = useMemo(() => {
			const options: Array<Option<ScreenshareResolution>> = [
				{value: 'low_240p', label: '240p', isPremium: false},
				{value: 'low_480p', label: '480p', isPremium: false},
				{value: 'medium', label: '720p', isPremium: false},
			];
			if (hasHigherVideoQuality || showPremiumFeatures) {
				options.push(
					{value: 'high', label: '1080p', isPremium: true},
					{value: 'ultra', label: '1440p', isPremium: true},
				);
			}
			if (!isDeviceShare && (hasHigherVideoQuality || showPremiumFeatures)) {
				options.push({value: 'source', label: i18n._(SOURCE_DESCRIPTOR), isPremium: true});
			}
			return options;
		}, [hasHigherVideoQuality, isDeviceShare, showPremiumFeatures, i18n.locale]);
		const frameRateOptions: Array<Option<SupportedScreenShareFrameRate>> = useMemo(
			() => [
				{value: 15, label: i18n._(MESSAGE_15_FPS_DESCRIPTOR), isPremium: false},
				{value: 30, label: i18n._(MESSAGE_30_FPS_DESCRIPTOR), isPremium: false},
				...(hasHigherVideoQuality || showPremiumFeatures
					? [{value: 60 as const, label: i18n._(MESSAGE_60_FPS_DESCRIPTOR), isPremium: true}]
					: []),
			],
			[hasHigherVideoQuality, showPremiumFeatures, i18n.locale],
		);
		const audioDeviceOptions = useMemo(() => {
			const real = inputDevices.filter((d) => d.deviceId && d.deviceId !== 'default');
			return real;
		}, [inputDevices]);
		const runApply = useCallback(
			(options?: PushActiveStreamSettingsOptions) => {
				if (!applyToLiveStream) return;
				void executeScreenShareOperation(() =>
					pushActiveStreamSettings(shareContext, displayShareEnvironment, hasHigherVideoQuality, options),
				).catch(() => undefined);
			},
			[applyToLiveStream, displayShareEnvironment, hasHigherVideoQuality, shareContext],
		);
		const reconfigureLinuxDisplayAudio = useCallback(() => {
			if (!applyToLiveStream || shareContext === 'device') return;
			void reconfigureActiveLinuxScreenShareAudioLink().catch((error) => {
				logger.warn('Failed to reconfigure active Linux screen share audio link', error);
			});
		}, [applyToLiveStream, shareContext]);
		const handleModeSelect = useCallback(
			(option: Option<StreamingMode>) => {
				if (option.isPremium && !hasHigherVideoQuality) {
					if (showPremiumFeatures) {
						PremiumModalCommands.open();
					}
					return;
				}
				if (currentMode === option.value) return;
				VoiceSettingsCommands.update({streamingMode: option.value});
				runApply();
			},
			[currentMode, hasHigherVideoQuality, runApply, showPremiumFeatures],
		);
		const handleResolutionSelect = useCallback(
			(option: Option<ScreenshareResolution>) => {
				if (option.isPremium && !hasHigherVideoQuality) {
					if (showPremiumFeatures) {
						PremiumModalCommands.open();
					}
					return;
				}
				if (currentResolution === option.value) return;
				VoiceSettingsCommands.update({screenshareResolution: option.value});
				runApply();
			},
			[currentResolution, hasHigherVideoQuality, runApply, showPremiumFeatures],
		);
		const handleFrameRateSelect = useCallback(
			(option: Option<SupportedScreenShareFrameRate>) => {
				if (option.isPremium && !hasHigherVideoQuality) {
					if (showPremiumFeatures) {
						PremiumModalCommands.open();
					}
					return;
				}
				if (currentFrameRate === option.value) return;
				VoiceSettingsCommands.update({videoFrameRate: option.value});
				runApply();
			},
			[currentFrameRate, hasHigherVideoQuality, runApply, showPremiumFeatures],
		);
		const handleCaptureAudioToggle = useCallback(
			(checked: boolean) => {
				if (isAppShare) {
					VoiceSettingsCommands.update({shareAppAudio: checked, muteStreamAudio: !checked});
				} else if (isDeviceShare) {
					VoiceSettingsCommands.update({shareDeviceAudio: checked, muteStreamAudio: !checked});
				} else {
					VoiceSettingsCommands.update({shareDesktopAudio: checked, muteStreamAudio: !checked});
				}
				runApply({audioSettingsChanged: true});
			},
			[isAppShare, isDeviceShare, runApply],
		);
		const handleHidePreviewToggle = useCallback((checked: boolean) => {
			VoiceSettingsCommands.update({hideStreamPreview: checked});
		}, []);
		const handleAudioDeviceSelect = useCallback(
			(deviceId: string) => {
				if (currentAudioDeviceId === deviceId) return;
				VoiceSettingsCommands.update({screenShareAudioDeviceId: deviceId});
				runApply({audioSettingsChanged: true});
			},
			[currentAudioDeviceId, runApply],
		);
		const selectedAudioDevice = useMemo(
			() => audioDeviceOptions.find((d) => d.deviceId === effectiveAudioDeviceId),
			[audioDeviceOptions, effectiveAudioDeviceId],
		);
		const selectedAudioDeviceLabel = selectedAudioDevice
			? formatVoiceAudioDeviceLabel(i18n, selectedAudioDevice, i18n._(UNNAMED_INPUT_DESCRIPTOR))
			: i18n._(SYSTEM_DEFAULT_DESCRIPTOR);
		return (
			<>
				<MenuGroup data-flx="voice.stream-settings-menu-content.menu-group">
					<MenuGroupLabel data-flx="voice.stream-settings-menu-content.menu-group-label.streaming-mode">
						{i18n._(STREAMING_MODE_DESCRIPTOR)}
					</MenuGroupLabel>
					{modeOptions.map((option) => {
						const premiumLocked = showPremiumFeatures && option.isPremium && !hasHigherVideoQuality;
						return (
							<MenuItemRadio
								key={option.value}
								selected={currentMode === option.value}
								onSelect={() => handleModeSelect(option)}
								data-flx="voice.stream-settings-menu-content.menu-item-radio.mode-select"
							>
								<span className={styles.modeItem} data-flx="voice.stream-settings-menu-content.mode-item">
									<span
										className={styles.modeLabelColumn}
										data-flx="voice.stream-settings-menu-content.mode-label-column"
									>
										<span className={styles.modeTitle} data-flx="voice.stream-settings-menu-content.mode-title">
											{option.label}
										</span>
										{option.description && (
											<span
												className={styles.modeDescription}
												data-flx="voice.stream-settings-menu-content.mode-description"
											>
												{option.description}
											</span>
										)}
									</span>
									{premiumLocked && <PremiumBadge data-flx="voice.stream-settings-menu-content.premium-badge" />}
								</span>
							</MenuItemRadio>
						);
					})}
				</MenuGroup>
				{currentMode === 'custom' && (
					<MenuGroup data-flx="voice.stream-settings-menu-content.menu-group--2">
						<MenuItemSubmenu
							label={i18n._(RESOLUTION_DESCRIPTOR)}
							render={() => (
								<MenuGroup data-flx="voice.stream-settings-menu-content.menu-group--3">
									{resolutionOptions.map((option) => {
										const premiumLocked = showPremiumFeatures && option.isPremium && !hasHigherVideoQuality;
										return (
											<MenuItemRadio
												key={option.value}
												selected={currentResolution === option.value}
												onSelect={() => handleResolutionSelect(option)}
												data-flx="voice.stream-settings-menu-content.menu-item-radio.resolution-select"
											>
												<span className={styles.row} data-flx="voice.stream-settings-menu-content.row">
													<span className={styles.rowLabel} data-flx="voice.stream-settings-menu-content.row-label">
														{option.label}
													</span>
													{premiumLocked && (
														<PremiumBadge data-flx="voice.stream-settings-menu-content.premium-badge--2" />
													)}
												</span>
											</MenuItemRadio>
										);
									})}
								</MenuGroup>
							)}
							data-flx="voice.stream-settings-menu-content.menu-item-submenu"
						/>
						<MenuItemSubmenu
							label={i18n._(FRAMERATE_DESCRIPTOR)}
							render={() => (
								<MenuGroup data-flx="voice.stream-settings-menu-content.menu-group--4">
									{frameRateOptions.map((option) => {
										const premiumLocked = showPremiumFeatures && option.isPremium && !hasHigherVideoQuality;
										return (
											<MenuItemRadio
												key={option.value}
												selected={currentFrameRate === option.value}
												onSelect={() => handleFrameRateSelect(option)}
												data-flx="voice.stream-settings-menu-content.menu-item-radio.frame-rate-select"
											>
												<span className={styles.row} data-flx="voice.stream-settings-menu-content.row--2">
													<span className={styles.rowLabel} data-flx="voice.stream-settings-menu-content.row-label--2">
														{option.label}
													</span>
													{premiumLocked && (
														<PremiumBadge data-flx="voice.stream-settings-menu-content.premium-badge--3" />
													)}
												</span>
											</MenuItemRadio>
										);
									})}
								</MenuGroup>
							)}
							data-flx="voice.stream-settings-menu-content.menu-item-submenu--2"
						/>
					</MenuGroup>
				)}
				<MenuGroup data-flx="voice.stream-settings-menu-content.menu-group--5">
					{audioMenuState.control.value === 'toggle' && (
						<CheckboxItem
							checked={audioMenuState.control.checked}
							onCheckedChange={handleCaptureAudioToggle}
							data-flx="voice.stream-settings-menu-content.checkbox-item"
						>
							{renderAudioCaptureLabel(audioMenuState.control.labelKey)}
						</CheckboxItem>
					)}
					{audioMenuState.showLinuxAudioControls && (
						<>
							<AudioSourcePickerLinuxSubmenu
								onSelectionChange={reconfigureLinuxDisplayAudio}
								data-flx="voice.stream-settings-menu-content.audio-source-picker-linux-submenu"
							/>
							<VenmicSettingsSubmenu
								onSettingsChange={reconfigureLinuxDisplayAudio}
								data-flx="voice.stream-settings-menu-content.venmic-settings-submenu"
							/>
						</>
					)}
					{audioMenuState.showDeviceAudioMenu && (
						<MenuItemSubmenu
							label={i18n._(AUDIO_DEVICE_DESCRIPTOR)}
							render={() => (
								<MenuGroup data-flx="voice.stream-settings-menu-content.menu-group--6">
									<MenuItemRadio
										selected={currentAudioDeviceId === 'default'}
										onSelect={() => handleAudioDeviceSelect('default')}
										data-flx="voice.stream-settings-menu-content.menu-item-radio.audio-device-select"
									>
										<span
											className={styles.audioDeviceLabel}
											data-flx="voice.stream-settings-menu-content.audio-device-label"
										>
											<span
												className={styles.audioDeviceName}
												data-flx="voice.stream-settings-menu-content.audio-device-name"
											>
												<Trans>Follow voice input</Trans>
											</span>
											<span
												className={styles.audioDeviceSubtext}
												data-flx="voice.stream-settings-menu-content.audio-device-subtext"
											>
												{selectedAudioDeviceLabel}
											</span>
										</span>
									</MenuItemRadio>
									{audioDeviceOptions.map((device) => (
										<MenuItemRadio
											key={device.deviceId}
											selected={currentAudioDeviceId === device.deviceId}
											onSelect={() => handleAudioDeviceSelect(device.deviceId)}
											data-flx="voice.stream-settings-menu-content.menu-item-radio.audio-device-select--2"
										>
											<span className={styles.row} data-flx="voice.stream-settings-menu-content.row--3">
												<span className={styles.rowLabel} data-flx="voice.stream-settings-menu-content.row-label--3">
													{formatVoiceAudioDeviceLabel(i18n, device, i18n._(UNNAMED_INPUT_DESCRIPTOR))}
												</span>
											</span>
										</MenuItemRadio>
									))}
								</MenuGroup>
							)}
							data-flx="voice.stream-settings-menu-content.menu-item-submenu--3"
						/>
					)}
					<CheckboxItem
						checked={currentHideStreamPreview}
						onCheckedChange={handleHidePreviewToggle}
						data-flx="voice.stream-settings-menu-content.checkbox-item--2"
					>
						<Trans>Hide preview thumbnail</Trans>
					</CheckboxItem>
					<AdaptiveQualityToggle data-flx="voice.stream-settings-menu-content.adaptive-quality-toggle" />
				</MenuGroup>
			</>
		);
	},
);

StreamSettingsMenuContent.displayName = 'StreamSettingsMenuContent';

const RESOLUTION_LABELS: Record<ScreenshareResolution, string> = {
	low_240p: '240p',
	low_480p: '480p',
	medium: '720p',
	high: '1080p',
	ultra: '1440p',
	source: '',
};

const AdaptiveQualityToggle = observer(() => {
	const {i18n} = useLingui();
	useStoreVersion(AdaptiveScreenShareEngine);
	const enabled = VoiceSettings.getAdaptiveScreenShareQuality();
	const snapshot = AdaptiveScreenShareEngine.qualitySnapshot;
	const label = i18n._(ADAPTIVE_QUALITY_DESCRIPTOR);
	const effectiveResolutionLabel =
		snapshot.effectiveResolution === 'source'
			? i18n._(SOURCE_DESCRIPTOR)
			: RESOLUTION_LABELS[snapshot.effectiveResolution];
	const adjustedStatus =
		enabled && snapshot.isAdapted
			? i18n._(ADAPTIVE_QUALITY_ACTIVE_DESCRIPTOR, {
					resolution: effectiveResolutionLabel,
					frameRate: snapshot.effectiveFrameRate,
				})
			: null;
	return (
		<CheckboxItem
			label={label}
			checked={enabled}
			onCheckedChange={(checked) => {
				VoiceSettingsCommands.update({adaptiveScreenShareQuality: checked});
			}}
			data-flx="voice.stream-settings-menu-content.adaptive-quality-toggle.checkbox-item"
		>
			<span className={styles.audioDeviceLabel} data-flx="voice.stream-settings-menu-content.adaptive-quality-label">
				<span className={styles.audioDeviceName} data-flx="voice.stream-settings-menu-content.adaptive-quality-name">
					{label}
				</span>
				{adjustedStatus && (
					<span
						className={styles.audioDeviceSubtext}
						data-flx="voice.stream-settings-menu-content.adaptive-quality-status"
					>
						{adjustedStatus}
					</span>
				)}
			</span>
		</CheckboxItem>
	);
});

const VenmicSettingsSubmenu = observer(({onSettingsChange}: {onSettingsChange?: () => void}) => {
	const {i18n} = useLingui();
	const workaround = VoiceSettings.getLinuxAudioCaptureWorkaround();
	const onlySpeakers = VoiceSettings.getLinuxAudioCaptureOnlySpeakers();
	const onlyDefaultSpeakers = VoiceSettings.getLinuxAudioCaptureOnlyDefaultSpeakers();
	const ignoreInputMedia = VoiceSettings.getLinuxAudioCaptureIgnoreInputMedia();
	const ignoreVirtual = VoiceSettings.getLinuxAudioCaptureIgnoreVirtual();
	const ignoreDevices = VoiceSettings.getLinuxAudioCaptureIgnoreDevices();
	const granularSelect = VoiceSettings.getLinuxAudioCaptureGranularSelect();
	const deviceSelect = VoiceSettings.getLinuxAudioCaptureDeviceSelect();
	return (
		<MenuItemSubmenu
			label={i18n._(AUDIO_SETTINGS_DESCRIPTOR)}
			render={() => (
				<MenuGroup data-flx="voice.stream-settings-menu-content.venmic-settings-submenu.menu-group">
					<CheckboxItem
						checked={workaround}
						onCheckedChange={(value) => {
							VoiceSettingsCommands.update({linuxAudioCaptureWorkaround: value});
							onSettingsChange?.();
						}}
						data-flx="voice.stream-settings-menu-content.venmic-settings-submenu.checkbox-item"
					>
						<Trans>Microphone workaround</Trans>
					</CheckboxItem>
					<CheckboxItem
						checked={onlySpeakers}
						onCheckedChange={(value) => {
							VoiceSettingsCommands.update({linuxAudioCaptureOnlySpeakers: value});
							onSettingsChange?.();
						}}
						data-flx="voice.stream-settings-menu-content.venmic-settings-submenu.checkbox-item--2"
					>
						<Trans>Only speakers</Trans>
					</CheckboxItem>
					<CheckboxItem
						checked={onlyDefaultSpeakers}
						onCheckedChange={(value) => {
							VoiceSettingsCommands.update({linuxAudioCaptureOnlyDefaultSpeakers: value});
							onSettingsChange?.();
						}}
						data-flx="voice.stream-settings-menu-content.venmic-settings-submenu.checkbox-item--3"
					>
						<Trans>Only default speakers</Trans>
					</CheckboxItem>
					{!deviceSelect && (
						<CheckboxItem
							checked={ignoreInputMedia}
							onCheckedChange={(value) => {
								VoiceSettingsCommands.update({linuxAudioCaptureIgnoreInputMedia: value});
								onSettingsChange?.();
							}}
							data-flx="voice.stream-settings-menu-content.venmic-settings-submenu.checkbox-item--4"
						>
							<Trans>Ignore input media</Trans>
						</CheckboxItem>
					)}
					<CheckboxItem
						checked={ignoreVirtual}
						onCheckedChange={(value) => {
							VoiceSettingsCommands.update({linuxAudioCaptureIgnoreVirtual: value});
							onSettingsChange?.();
						}}
						data-flx="voice.stream-settings-menu-content.venmic-settings-submenu.checkbox-item--5"
					>
						<Trans>Ignore virtual</Trans>
					</CheckboxItem>
					<CheckboxItem
						checked={ignoreDevices}
						onCheckedChange={(value) => {
							VoiceSettingsCommands.update({
								linuxAudioCaptureIgnoreDevices: value,
								linuxAudioCaptureDeviceSelect: value ? false : deviceSelect,
							});
							onSettingsChange?.();
						}}
						data-flx="voice.stream-settings-menu-content.venmic-settings-submenu.checkbox-item--6"
					>
						<Trans>Ignore hardware devices</Trans>
					</CheckboxItem>
					<CheckboxItem
						checked={granularSelect}
						onCheckedChange={(value) => {
							VoiceSettingsCommands.update({linuxAudioCaptureGranularSelect: value});
							onSettingsChange?.();
						}}
						data-flx="voice.stream-settings-menu-content.venmic-settings-submenu.checkbox-item--7"
					>
						<Trans>Granular selection</Trans>
					</CheckboxItem>
					<CheckboxItem
						checked={deviceSelect}
						onCheckedChange={(value) => {
							VoiceSettingsCommands.update({
								linuxAudioCaptureDeviceSelect: value,
								linuxAudioCaptureIgnoreDevices: value ? false : ignoreDevices,
							});
							onSettingsChange?.();
						}}
						data-flx="voice.stream-settings-menu-content.venmic-settings-submenu.checkbox-item--8"
					>
						<Trans>Device selection</Trans>
					</CheckboxItem>
				</MenuGroup>
			)}
			data-flx="voice.stream-settings-menu-content.venmic-settings-submenu.menu-item-submenu"
		/>
	);
});

VenmicSettingsSubmenu.displayName = 'VenmicSettingsSubmenu';
