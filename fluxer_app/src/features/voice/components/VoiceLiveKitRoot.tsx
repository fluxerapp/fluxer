// SPDX-License-Identifier: AGPL-3.0-or-later

import {VoiceRoomContext} from '@app/features/voice/components/VoiceRoomContext';
import VoiceRuntime from '@app/features/voice/engine/VoiceRuntime';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface VoiceLiveKitRootProps {
	children: React.ReactNode;
	'data-flx'?: string;
}

export const VoiceLiveKitRoot = observer(function VoiceLiveKitRoot({children}: VoiceLiveKitRootProps) {
	const runtime = VoiceRuntime.module;
	const room = VoiceRuntime.room;
	if (runtime === null || room === null) {
		return <VoiceRoomContext.Provider value={undefined}>{children}</VoiceRoomContext.Provider>;
	}
	return (
		<VoiceRoomContext.Provider value={room}>
			<runtime.VoiceRoomProviders room={room} data-flx="voice.voice-live-kit-root.runtime-voice-room-providers">
				{children}
			</runtime.VoiceRoomProviders>
		</VoiceRoomContext.Provider>
	);
});
