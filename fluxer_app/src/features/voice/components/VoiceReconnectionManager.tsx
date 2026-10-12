// SPDX-License-Identifier: AGPL-3.0-or-later

import GatewaySessions from '@app/features/gateway/transport/GatewaySessionPool';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {AudioPlaybackPermissionModal} from '@app/features/voice/components/modals/AudioPlaybackPermissionModal';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import {useAudioPlayback} from '@livekit/components-react';
import type {Room} from 'livekit-client';
import {observer} from 'mobx-react-lite';
import {useEffect, useRef} from 'react';

const logger = new Logger('VoiceReconnectionManager');
const AutoReconnectHandler = observer(() => {
	useMediaEngineVersion();
	const socket = GatewaySessions.foregroundConnection.socket;
	const hasAttemptedReconnection = useRef(false);
	const lastChannel = MediaEngine.getLastConnectedChannel();
	const shouldReconnect = MediaEngine.getShouldReconnect();
	const lastChannelGuildId = lastChannel?.guildId ?? null;
	const lastChannelId = lastChannel?.channelId ?? null;
	useEffect(() => {
		if (!socket || hasAttemptedReconnection.current) {
			return;
		}
		if (!lastChannelGuildId || !lastChannelId || !shouldReconnect) {
			return;
		}
		const lastChannel = {guildId: lastChannelGuildId, channelId: lastChannelId};
		logger.info('Attempting to reconnect to last voice channel', lastChannel);
		hasAttemptedReconnection.current = true;
		const timerId = setTimeout(() => {
			const stillShouldReconnect = MediaEngine.getShouldReconnect();
			if (stillShouldReconnect) {
				MediaEngine.connectToVoiceChannel(lastChannel.guildId, lastChannel.channelId);
				MediaEngine.markReconnectionAttempted();
			} else {
				logger.info('Reconnection was cancelled, skipping');
			}
		}, 1500);
		return () => clearTimeout(timerId);
	}, [lastChannelGuildId, lastChannelId, shouldReconnect, socket]);
	return null;
});
const AudioPlaybackHandler = observer(({room}: {room: Room}) => {
	useMediaEngineVersion();
	const {canPlayAudio, startAudio} = useAudioPlayback(room);
	const hasShownAudioModal = useRef(false);
	useEffect(() => {
		if (hasShownAudioModal.current) {
			return;
		}
		if (!canPlayAudio && MediaEngine.connected) {
			hasShownAudioModal.current = true;
			logger.info('Audio playback not allowed, showing permission modal');
			ModalCommands.pushWithKey(
				modal(() => (
					<AudioPlaybackPermissionModal
						onStartAudio={async () => {
							try {
								await startAudio();
								logger.info('Audio playback enabled');
							} catch (error) {
								logger.error('Failed to enable audio playback', error);
							}
						}}
						data-flx="voice.voice-reconnection-manager.audio-playback-handler.audio-playback-permission-modal"
					/>
				)),
				'audio-playback-permission',
			);
		}
	}, [canPlayAudio, startAudio]);
	return null;
});
export const VoiceReconnectionManager = observer(() => {
	useMediaEngineVersion();
	const room = MediaEngine.room;
	return (
		<>
			<AutoReconnectHandler data-flx="voice.voice-reconnection-manager.auto-reconnect-handler" />
			{room && <AudioPlaybackHandler room={room} data-flx="voice.voice-reconnection-manager.audio-playback-handler" />}
		</>
	);
});
