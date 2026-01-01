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

import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import ConnectionStore from '~/stores/ConnectionStore';
import UserStore from '~/stores/UserStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';

interface VoiceConnectionState {
	guildId: string;
	channelId: string;
	connectionId: string;
}

export interface VoiceConnectionConfirmModalCallbacks {
	onSwitchDevice: () => void;
	onJustJoin: () => void;
	onCancel: () => void;
}

export interface VoiceConnectionConfirmModalProps extends VoiceConnectionConfirmModalCallbacks {
	guildId: string | null;
	channelId: string;
}

export interface VoiceConnectionConfirmModalLogicState {
	existingConnectionsCount: number;
	handleSwitchDevice: () => Promise<void>;
	handleJustJoin: () => void;
	handleCancel: () => void;
}

export const useVoiceConnectionConfirmModalLogic = ({
	onSwitchDevice,
	onJustJoin,
	onCancel,
}: VoiceConnectionConfirmModalCallbacks): VoiceConnectionConfirmModalLogicState => {
	const currentUser = UserStore.currentUser;

	const voiceStates = React.useMemo(() => {
		if (!currentUser) return [];

		const allVoiceStates = MediaEngineStore.getAllVoiceStates();
		const userVoiceStates: Array<VoiceConnectionState> = [];

		for (const [guildId, channels] of Object.entries(allVoiceStates)) {
			for (const [_channelId, connectionStates] of Object.entries(channels)) {
				for (const [_connectionId, voiceState] of Object.entries(connectionStates)) {
					if (voiceState.user_id === currentUser.id && voiceState.channel_id && voiceState.connection_id) {
						userVoiceStates.push({
							guildId,
							channelId: voiceState.channel_id,
							connectionId: voiceState.connection_id,
						});
					}
				}
			}
		}

		return userVoiceStates;
	}, [currentUser]);

	const existingConnectionsCount = voiceStates.length;

	const handleSwitchDevice = React.useCallback(async () => {
		const socket = ConnectionStore.socket;
		if (socket) {
			for (const voiceState of voiceStates) {
				socket.updateVoiceState({
					guild_id: voiceState.guildId,
					channel_id: null,
					self_mute: true,
					self_deaf: true,
					self_video: false,
					self_stream: false,
					connection_id: voiceState.connectionId,
				});
			}
		}
		onSwitchDevice();
		ModalActionCreators.pop();
	}, [onSwitchDevice, voiceStates]);

	const handleJustJoin = React.useCallback(() => {
		onJustJoin();
		ModalActionCreators.pop();
	}, [onJustJoin]);

	const handleCancel = React.useCallback(() => {
		onCancel();
		ModalActionCreators.pop();
	}, [onCancel]);

	return {
		existingConnectionsCount,
		handleSwitchDevice,
		handleJustJoin,
		handleCancel,
	};
};
