// SPDX-License-Identifier: AGPL-3.0-or-later

import MobileLayout from '@app/features/ui/state/MobileLayout';
import {VoiceConnectionStatusInner} from '@app/features/voice/components/voice_connection_status/VoiceConnectionStatusInner';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import {observer} from 'mobx-react-lite';

export const VoiceConnectionStatus = observer(() => {
	useMediaEngineVersion();
	const storeConnectedChannelId = MediaEngine.channelId;
	const mobileLayout = MobileLayout;
	if (mobileLayout.enabled) {
		return null;
	}
	const connectedChannelId = storeConnectedChannelId;
	if (!connectedChannelId) return null;
	return <VoiceConnectionStatusInner data-flx="voice.voice-connection-status.voice-connection-status-inner" />;
});
