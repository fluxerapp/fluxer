// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SectionDefinition} from '@app/features/user/components/settings_utils/section_registry/SectionRegistryTypes';
import {
	AUDIO_2_DESCRIPTOR,
	PLUTONIUM_DESCRIPTOR,
	VOICE_DESCRIPTOR,
	VOLUME_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {
	VOICE_DIRECT_INPUT_PROFILE_DESCRIPTOR,
	VOICE_ECHO_CANCELLATION_DESCRIPTOR,
	VOICE_FOCUSED_VOICE_PROFILE_DESCRIPTOR,
	VOICE_INPUT_DEVICE_DESCRIPTOR,
	VOICE_INPUT_VOLUME_DESCRIPTOR,
	VOICE_NOISE_SUPPRESSION_DESCRIPTOR,
	VOICE_OUTPUT_DEVICE_DESCRIPTOR,
	VOICE_OUTPUT_VOLUME_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {msg} from '@lingui/core/macro';

const CONFIGURE_YOUR_MICROPHONE_SPEAKERS_INPUT_MODE_PROCESSING_AND_DESCRIPTOR = msg({
	message: 'Configure your microphone, speakers, input mode, processing, and voice sounds.',
	comment: 'Settings section description. One-line summary of what the settings section controls.',
});
const MICROPHONE_DESCRIPTOR = msg({
	message: 'Microphone',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MIC_DESCRIPTOR = msg({
	message: 'Mic',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const INPUT_2_DESCRIPTOR = msg({
	message: 'Input',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUDIO_INPUT_DESCRIPTOR = msg({
	message: 'Audio input',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DEVICE_DESCRIPTOR = msg({
	message: 'Device',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SPEAKER_DESCRIPTOR = msg({
	message: 'Speaker',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const OUTPUT_DESCRIPTOR = msg({
	message: 'Output',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUDIO_OUTPUT_DESCRIPTOR = msg({
	message: 'Audio output',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HEADPHONES_DESCRIPTOR = msg({
	message: 'Headphones',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LOUDNESS_DESCRIPTOR = msg({
	message: 'Loudness',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUDIO_LEVEL_DESCRIPTOR = msg({
	message: 'Audio level',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SCREEN_SHARE_AUDIO_DESCRIPTOR = msg({
	message: 'Screen share audio',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SCREEN_SHARE_AUDIO_BOOST_DESCRIPTOR = msg({
	message: 'Screen share audio boost',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUDIO_BOOST_DESCRIPTOR = msg({
	message: 'Audio boost',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BOOST_DESCRIPTOR = msg({
	message: 'Boost',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PUSH_TO_TALK_DESCRIPTOR = msg({
	message: 'Push to talk',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PTT_DESCRIPTOR = msg({
	message: 'PTT',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const RELEASE_DELAY_DESCRIPTOR = msg({
	message: 'Release delay',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VOICE_ACTIVATION_DESCRIPTOR = msg({
	message: 'Voice activation',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ACTIVITY_THRESHOLD_DESCRIPTOR = msg({
	message: 'Activity threshold',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUTO_SENSITIVITY_DESCRIPTOR = msg({
	message: 'Auto sensitivity',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SHORTCUT_DESCRIPTOR = msg({
	message: 'Shortcut',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const NOISE_DESCRIPTOR = msg({
	message: 'Noise',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SUPPRESSION_DESCRIPTOR = msg({
	message: 'Suppression',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ECHO_DESCRIPTOR = msg({
	message: 'Echo',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BACKGROUND_NOISE_DESCRIPTOR = msg({
	message: 'Background noise',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CANCELLATION_DESCRIPTOR = msg({
	message: 'Cancellation',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const INPUT_PROFILE_DESCRIPTOR = msg({
	message: 'Input profile',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CUSTOM_PROFILE_DESCRIPTOR = msg({
	message: 'Custom profile',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MIC_TEST_DESCRIPTOR = msg({
	message: 'Mic test',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MICROPHONE_TEST_DESCRIPTOR = msg({
	message: 'Microphone test',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ENTRANCE_SOUND_DESCRIPTOR = msg({
	message: 'Entrance sound',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VOICE_ENTRANCE_SOUND_DESCRIPTOR = msg({
	message: 'Voice entrance sound',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONFIGURE_YOUR_CAMERA_SCREEN_SHARING_QUALITY_PREVIEWS_AND_DESCRIPTOR = msg({
	message: 'Configure your camera, screen sharing quality, previews, and frame rate.',
	comment: 'Settings section description. One-line summary of what the settings section controls.',
});
const CAMERA_DESCRIPTOR = msg({
	message: 'Camera',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const WEBCAM_DESCRIPTOR = msg({
	message: 'Webcam',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VIDEO_2_DESCRIPTOR = msg({
	message: 'Video',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VIDEO_INPUT_DESCRIPTOR = msg({
	message: 'Video input',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CAMERA_DEVICE_DESCRIPTOR = msg({
	message: 'Camera device',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VIDEO_PREVIEW_DESCRIPTOR = msg({
	message: 'Video preview',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CAMERA_PREVIEW_DESCRIPTOR = msg({
	message: 'Camera preview',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CAMERA_TEST_DESCRIPTOR = msg({
	message: 'Camera test',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CAMERA_EFFECTS_DESCRIPTOR = msg({
	message: 'Camera effects',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BACKGROUND_EFFECTS_DESCRIPTOR = msg({
	message: 'Background effects',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CAMERA_QUALITY_DESCRIPTOR = msg({
	message: 'Camera quality',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const RESOLUTION_DESCRIPTOR = msg({
	message: 'Resolution',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SCREEN_SHARING_QUALITY_DESCRIPTOR = msg({
	message: 'Screen sharing quality',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SCREEN_SHARE_QUALITY_DESCRIPTOR = msg({
	message: 'Screen share quality',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SCREEN_SHARING_DESCRIPTOR = msg({
	message: 'Screen sharing',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SCREEN_SHARE_DESCRIPTOR = msg({
	message: 'Screen share',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const STREAMING_DESCRIPTOR = msg({
	message: 'Streaming',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FRAME_RATE_DESCRIPTOR = msg({
	message: 'Frame rate',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FPS_DESCRIPTOR = msg({
	message: 'FPS',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HD_DESCRIPTOR = msg({
	message: 'HD',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_1080P_DESCRIPTOR = msg({
	message: '1080p',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_1440P_DESCRIPTOR = msg({
	message: '1440p',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SOURCE_QUALITY_DESCRIPTOR = msg({
	message: 'Source quality',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUDIO_DESCRIPTOR = msg({
	message: 'Audio',
	context: 'voice-video-settings-section',
	comment: 'Audio/video settings section for microphone and speaker controls.',
});
const VIDEO_DESCRIPTOR = msg({
	message: 'Video',
	context: 'voice-video-settings-section',
	comment: 'Audio/video settings section for camera controls.',
});
export const voiceVideoSections = [
	{
		id: 'audio',
		tabType: 'voice_video',
		label: AUDIO_DESCRIPTOR,
		description: CONFIGURE_YOUR_MICROPHONE_SPEAKERS_INPUT_MODE_PROCESSING_AND_DESCRIPTOR,
		keywords: [
			AUDIO_2_DESCRIPTOR,
			VOICE_DESCRIPTOR,
			MICROPHONE_DESCRIPTOR,
			MIC_DESCRIPTOR,
			INPUT_2_DESCRIPTOR,
			AUDIO_INPUT_DESCRIPTOR,
			VOICE_INPUT_DEVICE_DESCRIPTOR,
			DEVICE_DESCRIPTOR,
			SPEAKER_DESCRIPTOR,
			OUTPUT_DESCRIPTOR,
			AUDIO_OUTPUT_DESCRIPTOR,
			VOICE_OUTPUT_DEVICE_DESCRIPTOR,
			HEADPHONES_DESCRIPTOR,
			VOLUME_DESCRIPTOR,
			VOICE_INPUT_VOLUME_DESCRIPTOR,
			VOICE_OUTPUT_VOLUME_DESCRIPTOR,
			LOUDNESS_DESCRIPTOR,
			AUDIO_LEVEL_DESCRIPTOR,
			SCREEN_SHARE_AUDIO_DESCRIPTOR,
			SCREEN_SHARE_AUDIO_BOOST_DESCRIPTOR,
			AUDIO_BOOST_DESCRIPTOR,
			BOOST_DESCRIPTOR,
			PUSH_TO_TALK_DESCRIPTOR,
			PTT_DESCRIPTOR,
			RELEASE_DELAY_DESCRIPTOR,
			VOICE_ACTIVATION_DESCRIPTOR,
			ACTIVITY_THRESHOLD_DESCRIPTOR,
			AUTO_SENSITIVITY_DESCRIPTOR,
			SHORTCUT_DESCRIPTOR,
			NOISE_DESCRIPTOR,
			SUPPRESSION_DESCRIPTOR,
			VOICE_NOISE_SUPPRESSION_DESCRIPTOR,
			'DeepFilter',
			'DeepFilterNet',
			ECHO_DESCRIPTOR,
			VOICE_ECHO_CANCELLATION_DESCRIPTOR,
			BACKGROUND_NOISE_DESCRIPTOR,
			CANCELLATION_DESCRIPTOR,
			INPUT_PROFILE_DESCRIPTOR,
			VOICE_FOCUSED_VOICE_PROFILE_DESCRIPTOR,
			VOICE_DIRECT_INPUT_PROFILE_DESCRIPTOR,
			CUSTOM_PROFILE_DESCRIPTOR,
			MIC_TEST_DESCRIPTOR,
			MICROPHONE_TEST_DESCRIPTOR,
			ENTRANCE_SOUND_DESCRIPTOR,
			VOICE_ENTRANCE_SOUND_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'video',
		tabType: 'voice_video',
		label: VIDEO_DESCRIPTOR,
		description: CONFIGURE_YOUR_CAMERA_SCREEN_SHARING_QUALITY_PREVIEWS_AND_DESCRIPTOR,
		keywords: [
			CAMERA_DESCRIPTOR,
			WEBCAM_DESCRIPTOR,
			VIDEO_2_DESCRIPTOR,
			VIDEO_INPUT_DESCRIPTOR,
			CAMERA_DEVICE_DESCRIPTOR,
			VIDEO_PREVIEW_DESCRIPTOR,
			CAMERA_PREVIEW_DESCRIPTOR,
			CAMERA_TEST_DESCRIPTOR,
			CAMERA_EFFECTS_DESCRIPTOR,
			BACKGROUND_EFFECTS_DESCRIPTOR,
			CAMERA_QUALITY_DESCRIPTOR,
			RESOLUTION_DESCRIPTOR,
			SCREEN_SHARING_QUALITY_DESCRIPTOR,
			SCREEN_SHARE_QUALITY_DESCRIPTOR,
			SCREEN_SHARING_DESCRIPTOR,
			SCREEN_SHARE_DESCRIPTOR,
			STREAMING_DESCRIPTOR,
			FRAME_RATE_DESCRIPTOR,
			FPS_DESCRIPTOR,
			HD_DESCRIPTOR,
			MESSAGE_1080P_DESCRIPTOR,
			MESSAGE_1440P_DESCRIPTOR,
			SOURCE_QUALITY_DESCRIPTOR,
			PLUTONIUM_DESCRIPTOR,
		],
		isAdvanced: false,
	},
] as const satisfies ReadonlyArray<SectionDefinition>;
