// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {SettingsTabContainer, SettingsTabContent} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {AUDIO_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {VideoTab} from '@app/features/user/components/modals/tabs/UserVideoTab';
import {VoiceTab} from '@app/features/user/components/modals/tabs/UserVoiceTab';
import Users from '@app/features/user/state/Users';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const VIDEO_DESCRIPTOR = msg({
	message: 'Video',
	comment: 'Short label in the voice video tab. Keep it concise.',
});
const VoiceVideoTab: React.FC = observer(() => {
	const {i18n} = useLingui();
	const user = Users.currentUser;
	const voiceSettings = VoiceSettings;
	const hasPremium = user?.isPremium() ?? false;
	return (
		<SettingsTabContainer data-flx="user.voice-video-tab.settings-tab-container">
			<SettingsTabContent data-flx="user.voice-video-tab.settings-tab-content">
				<SettingsSection id="audio" title={i18n._(AUDIO_DESCRIPTOR)} data-flx="user.voice-video-tab.audio">
					<VoiceTab
						voiceSettings={voiceSettings}
						hasPremium={hasPremium}
						autoRequestPermission={false}
						data-flx="user.voice-video-tab.voice-tab"
					/>
				</SettingsSection>
				<SettingsSection id="video" title={i18n._(VIDEO_DESCRIPTOR)} data-flx="user.voice-video-tab.video">
					<VideoTab
						voiceSettings={voiceSettings}
						hasPremium={hasPremium}
						autoRequestPermission={false}
						data-flx="user.voice-video-tab.video-tab"
					/>
				</SettingsSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});

export default VoiceVideoTab;
