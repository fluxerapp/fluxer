// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {getElectronAPI, isDesktop} from '@app/features/ui/utils/NativeUtils';
import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {getNativeAudioAvailabilitySnapshot} from '@app/features/voice/utils/NativeAudioCaptureBridge';
import {maySupportManualScreenShareAudioSourceSelection} from '@app/features/voice/utils/StreamSettingsUpdatePolicy';
import {msg} from '@lingui/core/macro';

const SHOW_NEW_DEVICE_ALERTS_DESCRIPTOR = msg({
	message: 'Show new device alerts',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const DEVICE_ALERTS_DESCRIPTOR = msg({
	message: 'Device alerts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUDIO_DEVICE_ALERTS_DESCRIPTOR = msg({
	message: 'Audio device alerts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const NEW_MICROPHONE_DESCRIPTOR = msg({
	message: 'New microphone',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PROMPT_WHEN_A_NEW_AUDIO_DEVICE_CONNECTS_DESCRIPTOR = msg({
	message: 'Prompt for new audio devices',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const CONNECTION_VOLUME_CONTROLS_DESCRIPTOR = msg({
	message: 'Connection volume controls',
	comment: 'Settings search entry label. Names an advanced voice setting in the settings UI.',
});
const PER_DEVICE_VOLUME_DESCRIPTOR = msg({
	message: 'Per-device volume',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DEVICE_VOLUME_DESCRIPTOR = msg({
	message: 'Device volume',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VOLUME_SLIDERS_DESCRIPTOR = msg({
	message: 'Volume sliders',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SHOW_PER_DEVICE_PARTICIPANT_VOLUME_SLIDERS_DESCRIPTOR = msg({
	message: 'Show per-device participant volume sliders in voice menus',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const AUTOMATIC_GAIN_CONTROL_DESCRIPTOR = msg({
	message: 'Automatic gain control',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const GAIN_CONTROL_DESCRIPTOR = msg({
	message: 'Gain control',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MICROPHONE_GAIN_DESCRIPTOR = msg({
	message: 'Microphone gain',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CUSTOM_VOICE_PROCESSING_DESCRIPTOR = msg({
	message: 'Custom voice processing',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONTROL_BROWSER_MICROPHONE_GAIN_FOR_CUSTOM_VOICE_PROCESSING_DESCRIPTOR = msg({
	message: 'Browser mic gain for custom processing',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const SCREEN_SHARE_CODEC_DESCRIPTOR = msg({
	message: 'Screen share codec',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const SCREEN_SHARING_CODEC_DESCRIPTOR = msg({
	message: 'Screen sharing codec',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CODEC_DESCRIPTOR = msg({
	message: 'Codec',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VIDEO_CODEC_DESCRIPTOR = msg({
	message: 'Video codec',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHOOSE_THE_VIDEO_CODEC_FOR_SCREEN_SHARING_DESCRIPTOR = msg({
	message: 'Video codec for screen sharing',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const ALLOW_AV1_SCREEN_SHARE_DESCRIPTOR = msg({
	message: 'Allow AV1 for screen sharing',
	comment: 'Settings search entry label for the AV1 screen-share opt-in. AV1 is a codec name and should stay literal.',
});
const ALLOW_HEVC_SCREEN_SHARE_DESCRIPTOR = msg({
	message: 'Allow H.265 (HEVC) for screen sharing',
	comment:
		'Settings search entry label for the H.265/HEVC screen-share opt-in. H.265 and HEVC are codec names and should stay literal.',
});
const ADVANCED_CODEC_COMPATIBILITY_NOTE_DESCRIPTOR = msg({
	message: 'May cause compatibility issues for viewers. We’re working on improving this.',
	comment: 'Settings search entry description for the AV1 and H.265 screen-share opt-in toggles.',
});
const AV1_KEYWORD_DESCRIPTOR = msg({
	message: 'AV1',
	comment: 'Settings search synonym. Codec name; keep literal. Used to match this term in the settings search bar.',
});
const HEVC_KEYWORD_DESCRIPTOR = msg({
	message: 'HEVC',
	comment: 'Settings search synonym. Codec name; keep literal. Used to match this term in the settings search bar.',
});
const OPENH264_VIDEO_CODEC_DESCRIPTOR = msg({
	message: 'OpenH264 video codec',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const H264_DESCRIPTOR = msg({
	message: 'H.264',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CISCO_DESCRIPTOR = msg({
	message: 'Cisco',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ENABLE_OPENH264_SOFTWARE_ENCODING_AND_DECODING_ON_LINUX_DESCRIPTOR = msg({
	message: 'OpenH264 software codec on Linux',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const PICK_THE_APPS_TO_CAPTURE_AUDIO_FROM_DESCRIPTOR = msg({
	message: 'Pick the apps to capture audio from',
	comment: 'Settings search entry label. Names an advanced screen-share audio setting in the settings UI.',
});
const APPLICATION_AUDIO_DESCRIPTOR = msg({
	message: 'Application audio',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUDIO_SOURCES_DESCRIPTOR = msg({
	message: 'Audio sources',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CAPTURE_CARD_DESCRIPTOR = msg({
	message: 'Capture card',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VIRTUAL_MICROPHONE_DESCRIPTOR = msg({
	message: 'Virtual microphone',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MANUAL_AUDIO_SOURCE_PICKER_NOTE_DESCRIPTOR = msg({
	message:
		'Adds a manual audio source picker to the stream settings menu. Without it, a shared window already captures that app and a shared screen captures your desktop.',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const SCREEN_SHARE_PREVIEW_BEHAVIOR_DESCRIPTOR = msg({
	message: 'Screen share preview behavior',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const SCREEN_SHARE_PREVIEW_DESCRIPTOR = msg({
	message: 'Screen share preview',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PICTURE_IN_PICTURE_DESCRIPTOR = msg({
	message: 'Picture in picture',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PIP_DESCRIPTOR = msg({
	message: 'PiP',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const STREAM_PREVIEW_DESCRIPTOR = msg({
	message: 'Stream preview',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONTROL_BACKGROUND_PREVIEW_POPOUT_AND_STREAM_THUMBNAIL_BEHAVIOR_DESCRIPTOR = msg({
	message: 'Preview, popout, and stream thumbnail behavior',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const SCREEN_SHARE_ENCODER_CONTROLS_DESCRIPTOR = msg({
	message: 'Screen share encoder controls',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const ENCODER_PATH_DESCRIPTOR = msg({
	message: 'Encoder path',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SOFTWARE_ENCODER_DESCRIPTOR = msg({
	message: 'Software encoder',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HARDWARE_ENCODER_DESCRIPTOR = msg({
	message: 'Hardware encoder',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SVC_DESCRIPTOR = msg({
	message: 'SVC',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BITRATE_DESCRIPTOR = msg({
	message: 'Bitrate',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TUNE_LOW_LEVEL_SCREEN_SHARE_ENCODER_BEHAVIOR_FOR_THE_NEXT_STREAM_DESCRIPTOR = msg({
	message: 'Encoder, SVC, backup stream, and bitrate presets',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});

export const voiceVideoIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'voice-video-new-device-alerts',
		tabType: 'voice_video',
		sectionId: 'audio',
		label: SHOW_NEW_DEVICE_ALERTS_DESCRIPTOR,
		keywords: [DEVICE_ALERTS_DESCRIPTOR, AUDIO_DEVICE_ALERTS_DESCRIPTOR, NEW_MICROPHONE_DESCRIPTOR],
		description: PROMPT_WHEN_A_NEW_AUDIO_DEVICE_CONNECTS_DESCRIPTOR,
		audience: 'advanced',
		tags: ['voice'],
	},
	{
		id: 'voice-video-connection-volume-controls',
		tabType: 'voice_video',
		sectionId: 'audio',
		label: CONNECTION_VOLUME_CONTROLS_DESCRIPTOR,
		keywords: [PER_DEVICE_VOLUME_DESCRIPTOR, DEVICE_VOLUME_DESCRIPTOR, VOLUME_SLIDERS_DESCRIPTOR],
		description: SHOW_PER_DEVICE_PARTICIPANT_VOLUME_SLIDERS_DESCRIPTOR,
		audience: 'advanced',
		tags: ['voice'],
	},
	{
		id: 'voice-video-automatic-gain-control',
		tabType: 'voice_video',
		sectionId: 'audio',
		label: AUTOMATIC_GAIN_CONTROL_DESCRIPTOR,
		keywords: [GAIN_CONTROL_DESCRIPTOR, MICROPHONE_GAIN_DESCRIPTOR, CUSTOM_VOICE_PROCESSING_DESCRIPTOR],
		description: CONTROL_BROWSER_MICROPHONE_GAIN_FOR_CUSTOM_VOICE_PROCESSING_DESCRIPTOR,
		audience: 'primary',
		tags: ['voice'],
	},
	{
		id: 'voice-video-screen-share-codec',
		tabType: 'voice_video',
		sectionId: 'video',
		label: SCREEN_SHARE_CODEC_DESCRIPTOR,
		keywords: [SCREEN_SHARING_CODEC_DESCRIPTOR, CODEC_DESCRIPTOR, VIDEO_CODEC_DESCRIPTOR],
		description: CHOOSE_THE_VIDEO_CODEC_FOR_SCREEN_SHARING_DESCRIPTOR,
		audience: 'advanced',
		tags: ['media', 'voice'],
	},
	{
		id: 'voice-video-screen-share-av1-opt-in',
		tabType: 'voice_video',
		sectionId: 'video',
		label: ALLOW_AV1_SCREEN_SHARE_DESCRIPTOR,
		keywords: [AV1_KEYWORD_DESCRIPTOR, CODEC_DESCRIPTOR, VIDEO_CODEC_DESCRIPTOR],
		description: ADVANCED_CODEC_COMPATIBILITY_NOTE_DESCRIPTOR,
		audience: 'advanced',
		tags: ['media', 'voice'],
	},
	{
		id: 'voice-video-screen-share-hevc-opt-in',
		tabType: 'voice_video',
		sectionId: 'video',
		label: ALLOW_HEVC_SCREEN_SHARE_DESCRIPTOR,
		keywords: [HEVC_KEYWORD_DESCRIPTOR, CODEC_DESCRIPTOR, VIDEO_CODEC_DESCRIPTOR],
		description: ADVANCED_CODEC_COMPATIBILITY_NOTE_DESCRIPTOR,
		audience: 'advanced',
		tags: ['media', 'voice'],
	},
	{
		id: 'voice-video-openh264-codec',
		tabType: 'voice_video',
		sectionId: 'video',
		label: OPENH264_VIDEO_CODEC_DESCRIPTOR,
		keywords: [H264_DESCRIPTOR, CODEC_DESCRIPTOR, CISCO_DESCRIPTOR],
		description: ENABLE_OPENH264_SOFTWARE_ENCODING_AND_DECODING_ON_LINUX_DESCRIPTOR,
		audience: 'advanced',
		tags: ['media', 'voice'],
		isVisible: () => isDesktop() && getElectronAPI()?.platform === 'linux',
	},
	{
		id: 'voice-video-manual-screen-share-audio-sources',
		tabType: 'voice_video',
		sectionId: 'video',
		label: PICK_THE_APPS_TO_CAPTURE_AUDIO_FROM_DESCRIPTOR,
		keywords: [
			APPLICATION_AUDIO_DESCRIPTOR,
			AUDIO_SOURCES_DESCRIPTOR,
			CAPTURE_CARD_DESCRIPTOR,
			VIRTUAL_MICROPHONE_DESCRIPTOR,
		],
		description: MANUAL_AUDIO_SOURCE_PICKER_NOTE_DESCRIPTOR,
		audience: 'advanced',
		tags: ['media', 'voice'],
		isVisible: () =>
			isDesktop() &&
			maySupportManualScreenShareAudioSourceSelection({
				platform: getElectronAPI()?.platform,
				nativeAudioAvailability: getNativeAudioAvailabilitySnapshot(),
			}),
	},
	{
		id: 'voice-video-screen-share-preview-behavior',
		tabType: 'voice_video',
		sectionId: 'video',
		label: SCREEN_SHARE_PREVIEW_BEHAVIOR_DESCRIPTOR,
		keywords: [
			SCREEN_SHARE_PREVIEW_DESCRIPTOR,
			PICTURE_IN_PICTURE_DESCRIPTOR,
			PIP_DESCRIPTOR,
			STREAM_PREVIEW_DESCRIPTOR,
		],
		description: CONTROL_BACKGROUND_PREVIEW_POPOUT_AND_STREAM_THUMBNAIL_BEHAVIOR_DESCRIPTOR,
		audience: 'advanced',
		tags: ['media', 'privacy'],
	},
	{
		id: 'voice-video-screen-share-encoder-controls',
		tabType: 'voice_video',
		sectionId: 'video',
		label: SCREEN_SHARE_ENCODER_CONTROLS_DESCRIPTOR,
		keywords: [
			ENCODER_PATH_DESCRIPTOR,
			SOFTWARE_ENCODER_DESCRIPTOR,
			HARDWARE_ENCODER_DESCRIPTOR,
			SVC_DESCRIPTOR,
			BITRATE_DESCRIPTOR,
			PRODUCT_NAME,
		],
		description: TUNE_LOW_LEVEL_SCREEN_SHARE_ENCODER_BEHAVIOR_FOR_THE_NEXT_STREAM_DESCRIPTOR,
		audience: 'advanced',
		tags: ['media', 'voice'],
	},
];
