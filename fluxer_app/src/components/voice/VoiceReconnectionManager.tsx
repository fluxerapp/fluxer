/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useAudioPlayback} from '@livekit/components-react';
import type {Room} from 'livekit-client';
import {observer} from 'mobx-react-lite';
import {useEffect, useRef} from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {AudioPlaybackPermissionModal} from '~/components/modals/AudioPlaybackPermissionModal';
import {Logger} from '~/lib/Logger';
import ConnectionStore from '~/stores/ConnectionStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';

const logger = new Logger('VoiceReconnectionManager');

const AutoReconnectHandler = observer(() => {
	const socket = ConnectionStore.socket;
	const hasAttemptedReconnection = useRef(false);

	useEffect(() => {
		if (!socket || hasAttemptedReconnection.current) {
			return;
		}

		const lastChannel = MediaEngineStore.getLastConnectedChannel();
		const shouldReconnect = MediaEngineStore.getShouldReconnect();

		if (lastChannel && shouldReconnect) {
			logger.info('Attempting to reconnect to last voice channel', lastChannel);
			hasAttemptedReconnection.current = true;

			setTimeout(() => {
				const stillShouldReconnect = MediaEngineStore.getShouldReconnect();
				if (stillShouldReconnect) {
					MediaEngineStore.connectToVoiceChannel(lastChannel.guildId, lastChannel.channelId);
					MediaEngineStore.markReconnectionAttempted();
				} else {
					logger.info('Reconnection was cancelled, skipping');
				}
			}, 1500);
		}
	}, [socket]);

	return null;
});

const AudioPlaybackHandler = observer(({room}: {room: Room}) => {
	const {canPlayAudio, startAudio} = useAudioPlayback(room);
	const hasShownAudioModal = useRef(false);

	useEffect(() => {
		if (hasShownAudioModal.current) {
			return;
		}

		if (!canPlayAudio && MediaEngineStore.connected) {
			hasShownAudioModal.current = true;
			logger.info('Audio playback not allowed, showing permission modal');

			ModalActionCreators.pushWithKey(
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
					/>
				)),
				'audio-playback-permission',
			);
		}
	}, [canPlayAudio, startAudio]);

	return null;
});

export const VoiceReconnectionManager = observer(() => {
	const room = MediaEngineStore.room;

	return (
		<>
			<AutoReconnectHandler />
			{room && <AudioPlaybackHandler room={room} />}
		</>
	);
});
