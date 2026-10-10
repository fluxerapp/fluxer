// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {AUDIO_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import MediaPermission from '@app/features/permissions/system/state/MediaPermission';
import styles from '@app/features/user/components/modals/tabs/TabInline.module.css';
import {VideoTab} from '@app/features/user/components/modals/tabs/UserVideoTab';
import {VoiceTab} from '@app/features/user/components/modals/tabs/UserVoiceTab';
import Users from '@app/features/user/state/Users';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {MediaDeviceRefreshType, refreshMediaDeviceLists} from '@app/features/voice/utils/MediaDeviceRefresh';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo} from 'react';

const VIDEO_DESCRIPTOR = msg({
	message: 'Video',
	comment: 'Short label in the inline. Keep it concise.',
});
export const VoiceVideoInlineContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const user = Users.currentUser;
	const voiceSettings = VoiceSettings;
	const hasPremium = useMemo(() => user?.isPremium() ?? false, [user]);
	useEffect(() => {
		if (MediaPermission.microphoneExplicitlyDenied && MediaPermission.cameraExplicitlyDenied) {
			return;
		}
		void refreshMediaDeviceLists({type: MediaDeviceRefreshType.audio});
	}, []);
	return (
		<div className={styles.container} data-flx="user.voice-video-tab.inline.voice-video-inline-content.container">
			<SettingsSection
				id="audio"
				title={i18n._(AUDIO_DESCRIPTOR)}
				data-flx="user.voice-video-tab.inline.voice-video-inline-content.audio"
			>
				<VoiceTab
					voiceSettings={voiceSettings}
					hasPremium={hasPremium}
					autoRequestPermission={false}
					data-flx="user.voice-video-tab.inline.voice-video-inline-content.voice-tab"
				/>
			</SettingsSection>
			<SettingsSection
				id="video"
				title={i18n._(VIDEO_DESCRIPTOR)}
				data-flx="user.voice-video-tab.inline.voice-video-inline-content.video"
			>
				<VideoTab
					voiceSettings={voiceSettings}
					hasPremium={hasPremium}
					autoRequestPermission={false}
					data-flx="user.voice-video-tab.inline.voice-video-inline-content.video-tab"
				/>
			</SettingsSection>
		</div>
	);
});
