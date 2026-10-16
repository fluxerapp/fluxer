// SPDX-License-Identifier: AGPL-3.0-or-later

import {VoiceRoomContext} from '@app/features/voice/components/VoiceRoomContext';
import {type RoomAudioRendererProps, RoomContext} from '@livekit/components-react';
import type {Room} from 'livekit-client';
import type React from 'react';
import {useEffect, useState} from 'react';

type RoomAudioRendererComponent = React.ComponentType<RoomAudioRendererProps & {'data-flx'?: string}>;

interface LiveKitComponentsModule {
	RoomAudioRenderer: RoomAudioRendererComponent;
}

interface VoiceLiveKitRootProps {
	room: Room | null;
	children: React.ReactNode;
	'data-flx'?: string;
}

let liveKitComponentsPromise: Promise<LiveKitComponentsModule> | null = null;

function loadLiveKitComponents(): Promise<LiveKitComponentsModule> {
	liveKitComponentsPromise ??= import('@livekit/components-react').then(({RoomAudioRenderer}) => ({RoomAudioRenderer}));
	return liveKitComponentsPromise;
}

export function VoiceLiveKitRoot({room, children}: VoiceLiveKitRootProps): React.ReactElement {
	const [components, setComponents] = useState<LiveKitComponentsModule | null>(null);

	useEffect(() => {
		if (!room || components) {
			return;
		}
		let canceled = false;
		void loadLiveKitComponents().then((loadedComponents) => {
			if (!canceled) {
				setComponents(loadedComponents);
			}
		});
		return () => {
			canceled = true;
		};
	}, [components, room]);

	if (!room) {
		return <VoiceRoomContext.Provider value={undefined}>{children}</VoiceRoomContext.Provider>;
	}

	return (
		<VoiceRoomContext.Provider value={room}>
			<RoomContext.Provider value={room}>
				{children}
				{components && <components.RoomAudioRenderer data-flx="app.app.app-wrapper.room-audio-renderer" />}
			</RoomContext.Provider>
		</VoiceRoomContext.Provider>
	);
}
