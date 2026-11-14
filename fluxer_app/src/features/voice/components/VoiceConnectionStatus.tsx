// SPDX-License-Identifier: AGPL-3.0-or-later

import MobileLayout from '@app/features/ui/state/MobileLayout';
import {LazyVoiceConnectionStatusInner} from '@app/features/voice/components/LazyVoiceComponents';
import VoiceRuntime, {useVoiceRuntimeVersion} from '@app/features/voice/engine/VoiceRuntime';
import {observer} from 'mobx-react-lite';

export const VoiceConnectionStatus = observer(() => {
	useVoiceRuntimeVersion();
	const storeConnectedChannelId = VoiceRuntime.channelId;
	const mobileLayout = MobileLayout;
	if (mobileLayout.enabled) {
		return null;
	}
	const connectedChannelId = storeConnectedChannelId;
	if (!connectedChannelId) return null;
	return <LazyVoiceConnectionStatusInner data-flx="voice.voice-connection-status.voice-connection-status-inner" />;
});
