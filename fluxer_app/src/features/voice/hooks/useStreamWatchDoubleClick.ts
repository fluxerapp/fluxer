// SPDX-License-Identifier: AGPL-3.0-or-later

import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import * as VoiceCallLayoutCommands from '@app/features/voice/commands/VoiceCallLayoutCommands';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {usePendingVoiceConnection} from '@app/features/voice/hooks/usePendingVoiceConnection';
import {useCallback, useMemo, useRef} from 'react';

interface UseStreamWatchDoubleClickOptions {
	streamParticipantIdentity: string | null;
	guildId: string | null;
	channelId: string | null;
	startWatching: () => void;
	onNavigateToWatch?: () => void;
}

interface UseStreamWatchDoubleClickResult {
	onClick: (event: React.MouseEvent) => void;
	onDoubleClick: (event: React.MouseEvent) => void;
}

export function useStreamWatchDoubleClick({
	streamParticipantIdentity,
	guildId,
	channelId,
	startWatching,
	onNavigateToWatch,
}: UseStreamWatchDoubleClickOptions): UseStreamWatchDoubleClickResult {
	useMediaEngineVersion();
	const lastClickTimeRef = useRef<number>(0);
	const isConnectedToChannel = useMemo(() => {
		if (!channelId) return false;
		return MediaEngine.channelId === channelId && MediaEngine.guildId === (guildId ?? null);
	}, [channelId, guildId, MediaEngine.channelId, MediaEngine.guildId]);
	const handleNavigateToWatch = useCallback(() => {
		onNavigateToWatch?.();
	}, [onNavigateToWatch]);
	const {markPending: markWatchNavigationPending} = usePendingVoiceConnection({
		guildId,
		channelId,
		onConnected: handleNavigateToWatch,
	});
	const onClick = useCallback(
		(event: React.MouseEvent) => {
			const now = Date.now();
			const timeSinceLastClick = now - lastClickTimeRef.current;
			lastClickTimeRef.current = now;
			if (timeSinceLastClick < 300 && streamParticipantIdentity) {
				event.preventDefault();
				event.stopPropagation();
				PopoutCommands.closeAll();
				startWatching();
				VoiceCallLayoutCommands.setLayoutMode('focus');
				VoiceCallLayoutCommands.setPinnedParticipant(streamParticipantIdentity, VoiceTrackSource.ScreenShare);
				if (isConnectedToChannel) {
					handleNavigateToWatch();
				} else if (channelId) {
					markWatchNavigationPending();
				}
			}
		},
		[
			streamParticipantIdentity,
			startWatching,
			isConnectedToChannel,
			channelId,
			handleNavigateToWatch,
			markWatchNavigationPending,
		],
	);
	const onDoubleClick = useCallback(
		(event: React.MouseEvent) => {
			if (!streamParticipantIdentity) return;
			event.preventDefault();
			event.stopPropagation();
		},
		[streamParticipantIdentity],
	);
	return {onClick, onDoubleClick};
}
