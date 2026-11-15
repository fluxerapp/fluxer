// SPDX-License-Identifier: AGPL-3.0-or-later

import {useElectronScreenSharePicker} from '@app/features/voice/hooks/useElectronScreenSharePicker';
import {startScreenSharePiPController} from '@app/features/voice/state/ScreenSharePiPController';
import {type RoomAudioRendererProps, RoomContext} from '@livekit/components-react';
import type {Room} from 'livekit-client';
import type React from 'react';
import {useEffect, useState} from 'react';

export {default as MediaEngine} from '@app/features/voice/engine/MediaEngineFacade';

type RoomAudioRendererComponent = React.ComponentType<RoomAudioRendererProps & {'data-flx'?: string}>;

let roomAudioRendererPromise: Promise<RoomAudioRendererComponent> | null = null;

function loadRoomAudioRenderer(): Promise<RoomAudioRendererComponent> {
	roomAudioRendererPromise ??= import('@livekit/components-react').then(({RoomAudioRenderer}) => RoomAudioRenderer);
	return roomAudioRendererPromise;
}

export function VoiceRoomProviders({room, children}: {room: Room; children: React.ReactNode}): React.ReactElement {
	const [RoomAudioRenderer, setRoomAudioRenderer] = useState<RoomAudioRendererComponent | null>(null);
	useEffect(() => {
		if (RoomAudioRenderer) return;
		let canceled = false;
		void loadRoomAudioRenderer().then((component) => {
			if (!canceled) setRoomAudioRenderer(() => component);
		});
		return () => {
			canceled = true;
		};
	}, [RoomAudioRenderer]);
	return (
		<RoomContext.Provider value={room}>
			{children}
			{RoomAudioRenderer && <RoomAudioRenderer data-flx="app.app.app-wrapper.room-audio-renderer" />}
		</RoomContext.Provider>
	);
}

export function VoiceRuntimeEffects(): null {
	useElectronScreenSharePicker();
	useEffect(() => startScreenSharePiPController(), []);
	return null;
}
