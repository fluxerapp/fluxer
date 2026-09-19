// SPDX-License-Identifier: AGPL-3.0-or-later

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
	CAPTURE_DEVICES_USE_THE_GAMING_PRESET_DESCRIPTOR,
	type OfferedScreenShareResolution,
	type StreamSettingsAudioMenuViewState,
	type StreamSettingsQualityWrite,
	selectStreamSettingsAudioMenuState,
	selectStreamSettingsPresetOverriddenByContext,
	selectStreamSettingsQualityMenuState,
} from '@app/features/voice/components/StreamSettingsMenuContentStateMachine';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import ScreenShareCodecNegotiation from '@app/features/voice/engine/ScreenShareCodecNegotiation';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {resolveConfiguredScreenShareTarget} from '@app/features/voice/engine/voice_screen_share_manager/shared';
import {useMediaDevices} from '@app/features/voice/hooks/useMediaDevices';
import ActiveScreenShareSource from '@app/features/voice/state/ActiveScreenShareSource';
import VoiceSettings, {type StreamingMode} from '@app/features/voice/state/VoiceSettings';
import {filterRoutableLinuxAudioSources} from '@app/features/voice/utils/LinuxAudioSourceRules';
import {
	getNativeAudioAvailabilityCached,
	getNativeAudioAvailabilitySnapshot,
} from '@app/features/voice/utils/NativeAudioCaptureBridge';
import {
	canRestartDisplayShareWithoutPreselectedSource,
	type DisplayShareEnvironment,
	usesNativeDisplayShareAudioSelection,
} from '@app/features/voice/utils/ScreenShareEnvironment';
import {
	resolveScreenShareFrameRate,
	resolveScreenShareTarget,
	type ScreenShareQualityInput,
	type ScreenShareTarget,
} from '@app/features/voice/utils/ScreenShareOptions';
import {isScreenShareRollbackIncompleteError} from '@app/features/voice/utils/ScreenShareRollbackIncompleteError';
import {
	applyLiveScreenShareAudioSourceChange,
	buildConfiguredScreenShareOptions,
	reconfigureActiveDeviceShareAudio,
	reconfigureActiveLinuxAppShareAudio,
	reconfigureActiveLinuxScreenShareAudioLink,
	scheduleConfiguredScreenShareMutation,
	stopActiveLinuxScreenShareAudioLink,
} from '@app/features/voice/utils/ScreenShareStartFlow';
import {executeScreenShareOperation, handleScreenShareError} from '@app/features/voice/utils/ScreenShareUtils';
import {
	isLinuxDesktopAudioShare,
	type StreamSettingsShareContext,
	shouldReconfigureAudioForActiveStreamSettings,
	type WindowShareAudioScope,
} from '@app/features/voice/utils/StreamSettingsUpdatePolicy';
import {hasHigherVideoQuality as resolveHigherVideoQuality} from '@app/features/voice/utils/VideoQualityEntitlement';
import {formatVoiceAudioDeviceLabel} from '@app/features/voice/utils/VoiceMessageDescriptors';
import type {NativeAudioAvailability} from '@app/types/electron.d';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CrownSimpleIcon, MicrophoneIcon, WaveformIcon} from '@phosphor-icons/react';
import type {Track} from 'livekit-client';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useState} from 'react';

const GAMING_DESCRIPTOR = msg({
	message: 'Gaming',
	comment: 'Streaming preset label in the stream settings menu. Higher frame rate, optimized for video games.',
});
const SMOOTH_60_FPS_AT_UP_TO_1440P_DESCRIPTOR = msg({
	message: 'Smooth 60 FPS at up to 1440p',
	comment:
		'Description for the high-tier Gaming streaming preset (Plutonium). Resolution and frame rate are technical tokens.',
});
const SMOOTH_30_FPS_AT_UP_TO_720P_DESCRIPTOR = msg({
	message: 'Smooth 30 FPS at up to 720p',
	comment: 'Description for the free-tier Gaming streaming preset. Resolution and frame rate are technical tokens.',
});
const SCREENSHARE_DESCRIPTOR = msg({
	message: 'Screen share',
	comment: 'Streaming preset label in the stream settings menu. Optimized for sharp text in screen shares.',
});
const SHARP_TEXT_AT_UP_TO_4K_DESCRIPTOR = msg({
	message: 'Sharp text at up to 4K, up to 15 FPS',
	comment:
		'Description for the high-tier Screen share streaming preset (Plutonium). Resolution and frame rate are technical tokens.',
});
const SHARP_TEXT_AT_720P_DESCRIPTOR = msg({
	message: 'Sharp text at 720p, up to 30 FPS',
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
const FPS_DESCRIPTOR = msg({
	message: '{frameRate} FPS',
	comment: 'Frame rate option label in the video tab. FPS is a technical token and frameRate is a whole number.',
});
const SCREEN_SHARE_RESOLUTION_LABELS: Record<Exclude<OfferedScreenShareResolution, 'source'>, string> = {
	low_480p: '480p',
	medium: '720p',
	high: '1080p',
	ultra: '1440p',
};
const STREAMING_MODE_DESCRIPTOR = msg({
	message: 'Streaming mode',
	comment: 'Section header in the stream settings menu. Picks between Gaming / Screen share / Custom presets.',
});
const RESOLUTION_DESCRIPTOR = msg({
	message: 'Resolution',
	comment: 'Section header in the stream settings menu for resolution options.',
});
const FRAMERATE_DESCRIPTOR = msg({
	message: 'Frame rate',
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
const STREAM_QUALITY_DESCRIPTOR = msg({
	message: 'Stream quality',
	comment: 'Compact submenu label for changing the resolution and frame rate of an active stream.',
});
const SHARE_STREAM_AUDIO_DESCRIPTOR = msg({
	message: 'Share stream audio',
	comment: 'Toggle label for sharing audio with an active stream.',
});
const CAPTURE_ENTIRE_SYSTEM_AUDIO_DESCRIPTOR = msg({
	message: 'Capture entire system audio',
	comment:
		'Toggle label in the stream settings menu for a window share whose audio scope the user widened from the shared window to the whole system mix.',
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

export function useHasHigherVideoQuality(): boolean {
	return resolveHigherVideoQuality();
}

function getScreenShareResolutionLabel(i18n: I18n, resolution: OfferedScreenShareResolution): string {
	return resolution === 'source' ? i18n._(SOURCE_DESCRIPTOR) : SCREEN_SHARE_RESOLUTION_LABELS[resolution];
}

function supportsStreamAudioCapture(shareContext: StreamSettingsShareContext): boolean {
	if (shareContext === 'device') {
		return typeof navigator !== 'undefined' && Boolean(navigator.mediaDevices?.getUserMedia);
	}
	return supportsDesktopScreenShareAudioCapture();
}

function getPreferredDisplaySurface(shareContext: StreamSettingsShareContext): 'window' | 'monitor' | undefined {
	if (shareContext === 'app') return 'window';
	if (shareContext === 'display') return 'monitor';
	return undefined;
}

async function runActiveStreamSettingsPush(
	shareContext: StreamSettingsShareContext,
	displayShareEnvironment: DisplayShareEnvironment,
	options: PushActiveStreamSettingsOptions,
): Promise<boolean> {
	const sourceDimensions = ActiveScreenShareSource.getSourceDimensions();
	const target = resolveConfiguredScreenShareTarget(shareContext, sourceDimensions);
	ActiveScreenShareSource.setTarget(target);
	const preferredDisplaySurface = getPreferredDisplaySurface(shareContext);
	const canControlAudio = supportsStreamAudioCapture(shareContext);
	const includeAudio =
		canControlAudio &&
		(shareContext === 'app'
			? VoiceSettings.getShareAppAudio()
			: shareContext === 'device'
				? VoiceSettings.getShareDeviceAudio()
				: VoiceSettings.getShareDesktopAudio());
	const {captureOptions, publishOptions} = buildConfiguredScreenShareOptions({
		target,
		sourceDimensions,
		includeAudio,
		videoCodec: ScreenShareCodecNegotiation.selectScreenShareCodec(VoiceSettings.getPreferredScreenShareCodec()),
		preferredDisplaySurface,
	});
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
			if (isScreenShareRollbackIncompleteError(error)) handleScreenShareError(error);
			logger.warn('Failed to restart active screen share with audio enabled', error);
			return false;
		}
		return true;
	}
	const activeCaptureOptions = canControlAudio
		? {...captureOptions, contentHint: target.contentHint}
		: {contentHint: target.contentHint, resolution: captureOptions.resolution};
	if (
		shouldReconfigureAudioForActiveStreamSettings({
			platform: getElectronAPI()?.platform,
			shareContext,
			audioSettingsChanged: options.audioSettingsChanged,
		})
	) {
		let audioLinkUpdated = true;
		try {
			if (!includeAudio) {
				audioLinkUpdated = await stopActiveLinuxScreenShareAudioLink();
			} else if (shareContext === 'device') {
				audioLinkUpdated = await reconfigureActiveDeviceShareAudio();
			} else if (shareContext === 'app') {
				audioLinkUpdated = await reconfigureActiveLinuxAppShareAudio();
			} else {
				audioLinkUpdated = await reconfigureActiveLinuxScreenShareAudioLink();
			}
		} catch (error) {
			audioLinkUpdated = false;
			logger.warn('Failed to update the active screen share audio link', error);
		}
		if (includeAudio && !audioLinkUpdated) {
			logger.warn('Screen-share audio link could not be updated; keeping video-only', {
				platform: getElectronAPI()?.platform ?? null,
				sourceMode: VoiceSettings.getEffectiveScreenShareAudioSourceMode(),
			});
		}
	}
	try {
		return await MediaEngine.updateActiveScreenShareSettings(activeCaptureOptions, publishOptions);
	} catch (error) {
		logger.warn('Failed to push updated stream settings to the active share', error);
		return false;
	}
}

export async function pushActiveStreamSettings(
	shareContext: StreamSettingsShareContext,
	displayShareEnvironment: DisplayShareEnvironment,
	options: PushActiveStreamSettingsOptions = {},
): Promise<void> {
	await scheduleConfiguredScreenShareMutation(() =>
		runActiveStreamSettingsPush(shareContext, displayShareEnvironment, options),
	);
}

interface StreamSettingsMenuContentProps {
	applyToLiveStream?: boolean;
	shareContext?: StreamSettingsShareContext;
	shareContextResolved?: boolean;
	displayShareEnvironment: DisplayShareEnvironment;
	variant?: 'full' | 'compactLive';
}

export const StreamSettingsMenuContent = observer(
	({
		applyToLiveStream = true,
		shareContext = 'display',
		shareContextResolved = true,
		displayShareEnvironment,
		variant = 'full',
	}: StreamSettingsMenuContentProps) => {
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
		const quality: ScreenShareQualityInput = {
			mode: currentMode,
			storedResolution: currentResolution,
			storedFrameRate: currentFrameRate,
			entitled: hasHigherVideoQuality,
			context: shareContext,
		};
		const target: ScreenShareTarget = resolveScreenShareTarget({
			...quality,
			sourceDimensions: null,
			hintSetting: VoiceSettings.getScreenShareContentHint(),
		});
		const presetOverriddenByContext = selectStreamSettingsPresetOverriddenByContext(currentMode, shareContext);
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
		const [nativeAudioAvailability, setNativeAudioAvailability] = useState<NativeAudioAvailability | null>(
			getNativeAudioAvailabilitySnapshot,
		);
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
		const audioSourceMode = VoiceSettings.getScreenShareAudioSourceMode();
		const selectedAudioSourceCount = filterRoutableLinuxAudioSources(
			VoiceSettings.getScreenShareAudioIncludeSources(),
		).length;
		const windowAudioScope = applyToLiveStream
			? ActiveScreenShareSource.getWindowAudioScope()
			: ActiveScreenShareSource.getPendingWindowAudioScope();
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
					audioSourceMode,
					selectedAudioSourceCount,
					windowAudioScope,
				}),
			[
				applyToLiveStream,
				audioSourceMode,
				captureAudioEnabled,
				displayShareEnvironment,
				hasLiveScreenShareAudioPublication,
				nativeAudioAvailability,
				platform,
				selectedAudioSourceCount,
				shareContext,
				supportsStreamAudio,
				windowAudioScope,
			],
		);
		const modeOptions: Array<Option<StreamingMode>> = useMemo(() => {
			const modes: Array<Option<StreamingMode>> = [
				{
					value: 'gaming',
					label: i18n._(GAMING_DESCRIPTOR),
					description: hasHigherVideoQuality
						? i18n._(SMOOTH_60_FPS_AT_UP_TO_1440P_DESCRIPTOR)
						: i18n._(SMOOTH_30_FPS_AT_UP_TO_720P_DESCRIPTOR),
					isPremium: false,
				},
			];
			if (!isDeviceShare) {
				modes.push({
					value: 'screenshare',
					label: i18n._(SCREENSHARE_DESCRIPTOR),
					description: hasHigherVideoQuality
						? i18n._(SHARP_TEXT_AT_UP_TO_4K_DESCRIPTOR)
						: i18n._(SHARP_TEXT_AT_720P_DESCRIPTOR),
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
		const qualityMenuState = selectStreamSettingsQualityMenuState({quality, target, showPremiumFeatures});
		const audioDeviceOptions = useMemo(() => {
			const real = inputDevices.filter((d) => d.deviceId && d.deviceId !== 'default');
			return real;
		}, [inputDevices]);
		const runApply = useCallback(
			(options?: PushActiveStreamSettingsOptions) => {
				if (!applyToLiveStream) return;
				void executeScreenShareOperation(() =>
					pushActiveStreamSettings(shareContext, displayShareEnvironment, options),
				).catch(() => undefined);
			},
			[applyToLiveStream, displayShareEnvironment, shareContext],
		);
		const applyQualityWrite = useCallback(
			(write: StreamSettingsQualityWrite) => {
				if (write.kind === 'premium') {
					PremiumModalCommands.open();
					return;
				}
				if (write.kind === 'none') return;
				VoiceSettingsCommands.update(write.patch);
				runApply();
			},
			[runApply],
		);
		const applyAudioSourceChange = useCallback(
			(nextWindowAudioScope?: WindowShareAudioScope) => {
				if (!applyToLiveStream) {
					if (nextWindowAudioScope != null) {
						ActiveScreenShareSource.setPendingWindowAudioScope(nextWindowAudioScope);
					}
					return;
				}
				void applyLiveScreenShareAudioSourceChange(shareContext, nextWindowAudioScope)
					.then((applied) => {
						if (applied) return;
						logger.warn('The screen share audio source change did not take; the share is running without audio', {
							shareContext,
							nextWindowAudioScope,
						});
					})
					.catch((error) => {
						logger.warn('Failed to apply the screen share audio source change', error);
					});
			},
			[applyToLiveStream, shareContext],
		);
		const handleModeSelect = useCallback(
			(option: Option<StreamingMode>) => {
				if (option.isPremium && !hasHigherVideoQuality) {
					if (showPremiumFeatures) {
						PremiumModalCommands.open();
					}
					return;
				}
				if (target.mode === option.value) return;
				VoiceSettingsCommands.update({streamingMode: option.value});
				runApply();
			},
			[hasHigherVideoQuality, runApply, showPremiumFeatures, target.mode],
		);
		const handleCaptureAudioToggle = useCallback(
			(checked: boolean) => {
				if (isDeviceShare) {
					VoiceSettingsCommands.update({shareDeviceAudio: checked, muteStreamAudio: !checked});
				} else if (!shareContextResolved) {
					VoiceSettingsCommands.update({
						shareAppAudio: checked,
						shareDesktopAudio: checked,
						muteStreamAudio: !checked,
					});
				} else if (isAppShare) {
					VoiceSettingsCommands.update({shareAppAudio: checked, muteStreamAudio: !checked});
				} else {
					VoiceSettingsCommands.update({shareDesktopAudio: checked, muteStreamAudio: !checked});
				}
				runApply({audioSettingsChanged: true});
			},
			[isAppShare, isDeviceShare, shareContextResolved, runApply],
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
		if (variant === 'compactLive') {
			return (
				<>
					<MenuItemSubmenu
						label={i18n._(STREAM_QUALITY_DESCRIPTOR)}
						render={() => (
							<>
								<MenuGroup data-flx="voice.stream-settings-menu-content.compact-live.frame-rate-group">
									<MenuGroupLabel
										className={styles.compactLiveGroupLabel}
										data-flx="voice.stream-settings-menu-content.compact-live.frame-rate-label"
									>
										{i18n._(FRAMERATE_DESCRIPTOR)}
									</MenuGroupLabel>
									{qualityMenuState.frameRates.map((option) => {
										const isPlutoniumReserved = showPremiumFeatures && option.premium;
										return (
											<MenuItemRadio
												key={option.value}
												selected={option.selected}
												onSelect={() => applyQualityWrite(option.write)}
												data-flx="voice.stream-settings-menu-content.compact-live.frame-rate-option"
											>
												<span
													className={styles.row}
													data-flx="voice.stream-settings-menu-content.compact-live.frame-rate-row"
												>
													<span
														className={styles.rowLabel}
														data-flx="voice.stream-settings-menu-content.compact-live.frame-rate-row-label"
													>
														{i18n._(FPS_DESCRIPTOR, {frameRate: option.value})}
													</span>
													{isPlutoniumReserved && (
														<PremiumBadge data-flx="voice.stream-settings-menu-content.compact-live.frame-rate-premium-badge" />
													)}
												</span>
											</MenuItemRadio>
										);
									})}
								</MenuGroup>
								<MenuGroup data-flx="voice.stream-settings-menu-content.compact-live.resolution-group">
									<MenuGroupLabel
										className={styles.compactLiveGroupLabel}
										data-flx="voice.stream-settings-menu-content.compact-live.resolution-label"
									>
										{i18n._(RESOLUTION_DESCRIPTOR)}
									</MenuGroupLabel>
									{qualityMenuState.resolutions.map((option) => {
										const isPlutoniumReserved = showPremiumFeatures && option.premium;
										return (
											<MenuItemRadio
												key={option.value}
												selected={option.selected}
												onSelect={() => applyQualityWrite(option.write)}
												data-flx="voice.stream-settings-menu-content.compact-live.resolution-option"
											>
												<span
													className={styles.row}
													data-flx="voice.stream-settings-menu-content.compact-live.resolution-row"
												>
													<span
														className={styles.rowLabel}
														data-flx="voice.stream-settings-menu-content.compact-live.resolution-row-label"
													>
														{getScreenShareResolutionLabel(i18n, option.value)}
													</span>
													{isPlutoniumReserved && (
														<PremiumBadge data-flx="voice.stream-settings-menu-content.compact-live.resolution-premium-badge" />
													)}
												</span>
											</MenuItemRadio>
										);
									})}
								</MenuGroup>
							</>
						)}
						data-flx="voice.stream-settings-menu-content.compact-live.quality-submenu"
					/>
					<StreamSettingsAudioGroup
						audioMenuState={audioMenuState}
						shareContext={shareContext}
						displayShareEnvironment={displayShareEnvironment}
						windowAudioScope={windowAudioScope}
						compact={true}
						audioDeviceOptions={audioDeviceOptions}
						currentAudioDeviceId={currentAudioDeviceId}
						selectedAudioDeviceLabel={selectedAudioDeviceLabel}
						onCaptureAudioToggle={handleCaptureAudioToggle}
						onAudioSourceChange={applyAudioSourceChange}
						onAudioDeviceSelect={handleAudioDeviceSelect}
						data-flx="voice.stream-settings-menu-content.compact-live.audio-group"
					/>
				</>
			);
		}
		return (
			<>
				<MenuGroup data-flx="voice.stream-settings-menu-content.menu-group">
					<MenuGroupLabel data-flx="voice.stream-settings-menu-content.menu-group-label.streaming-mode">
						{i18n._(STREAMING_MODE_DESCRIPTOR)}
					</MenuGroupLabel>
					{modeOptions.map((option) => {
						const isPlutoniumReserved = showPremiumFeatures && option.isPremium;
						return (
							<MenuItemRadio
								key={option.value}
								selected={target.mode === option.value}
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
										{presetOverriddenByContext && option.value === target.mode && (
											<span
												className={styles.modeDescription}
												data-flx="voice.stream-settings-menu-content.mode-context-note"
											>
												{i18n._(CAPTURE_DEVICES_USE_THE_GAMING_PRESET_DESCRIPTOR)}
											</span>
										)}
									</span>
									{isPlutoniumReserved && <PremiumBadge data-flx="voice.stream-settings-menu-content.premium-badge" />}
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
									{qualityMenuState.resolutions.map((option) => {
										const isPlutoniumReserved = showPremiumFeatures && option.premium;
										return (
											<MenuItemRadio
												key={option.value}
												selected={option.selected}
												onSelect={() => applyQualityWrite(option.write)}
												data-flx="voice.stream-settings-menu-content.menu-item-radio.resolution-select"
											>
												<span className={styles.row} data-flx="voice.stream-settings-menu-content.row">
													<span className={styles.rowLabel} data-flx="voice.stream-settings-menu-content.row-label">
														{getScreenShareResolutionLabel(i18n, option.value)}
													</span>
													{isPlutoniumReserved && (
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
									{qualityMenuState.frameRates.map((option) => {
										const isPlutoniumReserved = showPremiumFeatures && option.premium;
										return (
											<MenuItemRadio
												key={option.value}
												selected={option.selected}
												onSelect={() => applyQualityWrite(option.write)}
												data-flx="voice.stream-settings-menu-content.menu-item-radio.frame-rate-select"
											>
												<span className={styles.row} data-flx="voice.stream-settings-menu-content.row--2">
													<span className={styles.rowLabel} data-flx="voice.stream-settings-menu-content.row-label--2">
														{i18n._(FPS_DESCRIPTOR, {frameRate: option.value})}
													</span>
													{isPlutoniumReserved && (
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
					<StreamSettingsAudioGroup
						audioMenuState={audioMenuState}
						shareContext={shareContext}
						displayShareEnvironment={displayShareEnvironment}
						windowAudioScope={windowAudioScope}
						compact={false}
						audioDeviceOptions={audioDeviceOptions}
						currentAudioDeviceId={currentAudioDeviceId}
						selectedAudioDeviceLabel={selectedAudioDeviceLabel}
						onCaptureAudioToggle={handleCaptureAudioToggle}
						onAudioSourceChange={applyAudioSourceChange}
						onAudioDeviceSelect={handleAudioDeviceSelect}
						data-flx="voice.stream-settings-menu-content.audio-group"
					/>
					<CheckboxItem
						checked={currentHideStreamPreview}
						onCheckedChange={handleHidePreviewToggle}
						data-flx="voice.stream-settings-menu-content.checkbox-item--2"
					>
						<Trans>Hide preview thumbnail</Trans>
					</CheckboxItem>
				</MenuGroup>
			</>
		);
	},
);

StreamSettingsMenuContent.displayName = 'StreamSettingsMenuContent';

interface StreamSettingsAudioGroupProps {
	audioMenuState: StreamSettingsAudioMenuViewState;
	shareContext: StreamSettingsShareContext;
	displayShareEnvironment: DisplayShareEnvironment;
	windowAudioScope: WindowShareAudioScope;
	compact: boolean;
	audioDeviceOptions: Array<MediaDeviceInfo>;
	currentAudioDeviceId: string;
	selectedAudioDeviceLabel: string;
	onCaptureAudioToggle: (checked: boolean) => void;
	onAudioSourceChange: (nextWindowAudioScope?: WindowShareAudioScope) => void;
	onAudioDeviceSelect: (deviceId: string) => void;
}

const StreamSettingsAudioGroup = observer((props: StreamSettingsAudioGroupProps) => {
	const {
		audioMenuState,
		shareContext,
		displayShareEnvironment,
		windowAudioScope,
		compact,
		audioDeviceOptions,
		currentAudioDeviceId,
		selectedAudioDeviceLabel,
		onCaptureAudioToggle,
		onAudioSourceChange,
		onAudioDeviceSelect,
	} = props;
	const {i18n} = useLingui();
	const renderCaptureLabel = () => {
		if (compact) return i18n._(SHARE_STREAM_AUDIO_DESCRIPTOR);
		if (audioMenuState.control.labelKey === 'captureDeviceAudio') return <Trans>Capture device audio</Trans>;
		if (audioMenuState.control.labelKey === 'captureAppAudio') return <Trans>Capture app audio</Trans>;
		if (audioMenuState.control.labelKey === 'captureSystemAudio') return i18n._(CAPTURE_ENTIRE_SYSTEM_AUDIO_DESCRIPTOR);
		return <Trans>Capture desktop audio</Trans>;
	};
	return (
		<>
			{audioMenuState.control.value === 'toggle' && (
				<CheckboxItem
					checked={audioMenuState.control.checked}
					onCheckedChange={onCaptureAudioToggle}
					data-flx="voice.stream-settings-menu-content.audio-group.capture-audio"
				>
					{renderCaptureLabel()}
				</CheckboxItem>
			)}
			{audioMenuState.showManualAudioSources && (
				<>
					<AudioSourcePickerLinuxSubmenu
						onSelectionChange={onAudioSourceChange}
						shareContext={shareContext}
						displayShareEnvironment={displayShareEnvironment}
						windowAudioScope={windowAudioScope}
						microphoneLabel={selectedAudioDeviceLabel}
						data-flx="voice.stream-settings-menu-content.audio-group.audio-source-picker-submenu"
					/>
					<VenmicSettingsSubmenu
						onSettingsChange={onAudioSourceChange}
						data-flx="voice.stream-settings-menu-content.audio-group.venmic-settings-submenu"
					/>
				</>
			)}
			{audioMenuState.showDeviceAudioMenu && (
				<MenuItemSubmenu
					label={i18n._(AUDIO_DEVICE_DESCRIPTOR)}
					render={() => (
						<MenuGroup data-flx="voice.stream-settings-menu-content.audio-group.audio-device-group">
							<MenuItemRadio
								selected={currentAudioDeviceId === 'default'}
								onSelect={() => onAudioDeviceSelect('default')}
								data-flx="voice.stream-settings-menu-content.audio-group.audio-device-default"
							>
								<span className={styles.row} data-flx="voice.stream-settings-menu-content.audio-group.audio-device-row">
									<WaveformIcon
										className={styles.audioDeviceIcon}
										weight="fill"
										aria-hidden={true}
										data-flx="voice.stream-settings-menu-content.audio-group.audio-device-icon"
									/>
									<span
										className={styles.audioDeviceLabel}
										data-flx="voice.stream-settings-menu-content.audio-group.audio-device-label"
									>
										<span
											className={styles.audioDeviceName}
											data-flx="voice.stream-settings-menu-content.audio-group.audio-device-name"
										>
											<Trans>Device audio only</Trans>
										</span>
										<span
											className={styles.audioDeviceSubtext}
											data-flx="voice.stream-settings-menu-content.audio-group.audio-device-subtext"
										>
											<Trans>Silent when the capture device has no audio input of its own</Trans>
										</span>
									</span>
								</span>
							</MenuItemRadio>
							{audioDeviceOptions.map((device) => (
								<MenuItemRadio
									key={device.deviceId}
									selected={currentAudioDeviceId === device.deviceId}
									onSelect={() => onAudioDeviceSelect(device.deviceId)}
									data-flx="voice.stream-settings-menu-content.audio-group.audio-device-option"
								>
									<span
										className={styles.row}
										data-flx="voice.stream-settings-menu-content.audio-group.audio-device-option-row"
									>
										<MicrophoneIcon
											className={styles.audioDeviceIcon}
											weight="fill"
											aria-hidden={true}
											data-flx="voice.stream-settings-menu-content.audio-group.audio-device-option-icon"
										/>
										<span
											className={styles.rowLabel}
											data-flx="voice.stream-settings-menu-content.audio-group.audio-device-option-label"
										>
											{formatVoiceAudioDeviceLabel(i18n, device, i18n._(UNNAMED_INPUT_DESCRIPTOR))}
										</span>
									</span>
								</MenuItemRadio>
							))}
						</MenuGroup>
					)}
					data-flx="voice.stream-settings-menu-content.audio-group.audio-device-submenu"
				/>
			)}
		</>
	);
});

StreamSettingsAudioGroup.displayName = 'StreamSettingsAudioGroup';

const VenmicSettingsSubmenu = observer(({onSettingsChange}: {onSettingsChange?: () => void}) => {
	const {i18n} = useLingui();
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
