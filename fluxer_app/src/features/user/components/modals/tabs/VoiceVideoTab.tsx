// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {SettingsTabContainer, SettingsTabContent} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {AUDIO_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import MediaPermission from '@app/features/permissions/system/state/MediaPermission';
import inlineStyles from '@app/features/user/components/modals/tabs/TabInline.module.css';
import {VideoTab} from '@app/features/user/components/modals/tabs/UserVideoTab';
import {VoiceTab} from '@app/features/user/components/modals/tabs/UserVoiceTab';
import {VIDEO_DESCRIPTOR} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {MediaDeviceRefreshType, refreshMediaDeviceLists} from '@app/features/voice/utils/MediaDeviceRefresh';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect} from 'react';

const VoiceVideoSections = observer(() => {
	const {i18n} = useLingui();
	const voiceSettings = VoiceSettings;
	return (
		<>
			<SettingsSection id="audio" title={i18n._(AUDIO_DESCRIPTOR)} data-flx="user.voice-video-tab.audio">
				<VoiceTab voiceSettings={voiceSettings} data-flx="user.voice-video-tab.voice-tab" />
			</SettingsSection>

			<SettingsSection id="video" title={i18n._(VIDEO_DESCRIPTOR)} data-flx="user.voice-video-tab.video">
				<VideoTab voiceSettings={voiceSettings} data-flx="user.voice-video-tab.video-tab" />
			</SettingsSection>
		</>
	);
});
const VoiceVideoTab: React.FC = observer(() => {
	return (
		<SettingsTabContainer data-flx="user.voice-video-tab.settings-tab-container">
			<SettingsTabContent data-flx="user.voice-video-tab.settings-tab-content">
				<VoiceVideoSections data-flx="user.voice-video-tab.voice-video-sections" />
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});

export default VoiceVideoTab;

export const VoiceVideoInlineContent = observer(() => {
	useEffect(() => {
		if (MediaPermission.microphoneExplicitlyDenied && MediaPermission.cameraExplicitlyDenied) {
			return;
		}
		void refreshMediaDeviceLists({type: MediaDeviceRefreshType.audio});
	}, []);
	return (
		<div className={inlineStyles.container} data-flx="user.voice-video-tab.voice-video-inline-content.div">
			<VoiceVideoSections data-flx="user.voice-video-tab.voice-video-inline-content.voice-video-sections" />
		</div>
	);
});
