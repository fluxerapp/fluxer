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

import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {VoiceChannelFullModal} from '~/components/alerts/VoiceChannelFullModal';
import {VoiceConnectionConfirmModal} from '~/components/alerts/VoiceConnectionConfirmModal';
import {Logger} from '~/lib/Logger';
import ChannelStore from '~/stores/ChannelStore';
import ConnectionStore from '~/stores/ConnectionStore';
import LocalVoiceStateStore from '~/stores/LocalVoiceStateStore';
import UserStore from '~/stores/UserStore';
import VoiceConnectionManager from './VoiceConnectionManager';
import VoiceStateManager from './VoiceStateManager';

const logger = new Logger('VoiceChannelConnector');

export function checkChannelLimit(guildId: string | null, channelId: string): boolean {
	if (!guildId) return true;

	const channel = ChannelStore.getChannel(channelId);
	if (!channel?.userLimit || channel.userLimit <= 0) return true;

	const voiceStates = VoiceStateManager.getAllVoiceStatesInChannel(guildId, channelId);
	const count = Object.keys(voiceStates).length;
	const user = UserStore.getCurrentUser();
	const already = user && voiceStates[user.id];
	const adjusted = already ? count - 1 : count;

	if (adjusted >= channel.userLimit) {
		ModalActionCreators.push(modal(() => <VoiceChannelFullModal />));
		return false;
	}

	return true;
}

export function checkMultipleConnections(
	guildId: string | null,
	channelId: string,
	onSwitchDevice: () => Promise<void>,
	onJustJoin: () => void,
	onCancel: () => void,
): boolean {
	if (!guildId) return true;

	const user = UserStore.getCurrentUser();
	if (!user) return true;

	const socket = ConnectionStore.socket;
	if (!socket) return true;

	const voiceStates = VoiceStateManager.getAllVoiceStatesInChannel(guildId, channelId);
	const currentConnectionId = VoiceConnectionManager.connectionId;
	const userStates = Object.values(voiceStates).filter(
		(vs) => vs.user_id === user.id && vs.connection_id !== currentConnectionId,
	);

	if (userStates.length > 0) {
		ModalActionCreators.push(
			modal(() => (
				<VoiceConnectionConfirmModal
					guildId={guildId}
					channelId={channelId}
					onSwitchDevice={async () => {
						for (const vs of userStates) {
							if (vs.connection_id) {
								socket.updateVoiceState({
									guild_id: guildId,
									channel_id: null,
									self_mute: true,
									self_deaf: true,
									self_video: false,
									self_stream: false,
									connection_id: vs.connection_id,
								});
							}
						}
						await onSwitchDevice();
					}}
					onJustJoin={onJustJoin}
					onCancel={onCancel}
				/>
			)),
		);
		return false;
	}

	return true;
}

export function sendVoiceStateConnect(guildId: string | null, channelId: string): void {
	const socket = ConnectionStore.socket;
	if (!socket) {
		logger.warn('[sendVoiceStateConnect] No socket');
		return;
	}

	LocalVoiceStateStore.ensurePermissionMute();

	socket.updateVoiceState({
		guild_id: guildId,
		channel_id: channelId,
		self_mute: LocalVoiceStateStore.getSelfMute(),
		self_deaf: LocalVoiceStateStore.getSelfDeaf(),
		self_video: false,
		self_stream: false,
		viewer_stream_key: null,
		connection_id: VoiceConnectionManager.connectionId ?? null,
	});
}

export function sendVoiceStateDisconnect(guildId: string | null, connectionId: string): void {
	const socket = ConnectionStore.socket;
	if (!socket) {
		logger.warn('[sendVoiceStateDisconnect] No socket');
		return;
	}

	socket.updateVoiceState({
		guild_id: guildId,
		channel_id: null,
		self_mute: true,
		self_deaf: true,
		self_video: false,
		self_stream: false,
		viewer_stream_key: null,
		connection_id: connectionId,
	});
}

export function syncVoiceStateToServer(
	guildId: string | null,
	channelId: string,
	connectionId: string,
	partial?: {
		self_video?: boolean;
		self_stream?: boolean;
		self_mute?: boolean;
		self_deaf?: boolean;
		viewer_stream_key?: string | null;
	},
): void {
	const socket = ConnectionStore.socket;
	if (!socket) return;

	socket.updateVoiceState({
		guild_id: guildId,
		channel_id: channelId,
		self_mute: partial?.self_mute ?? LocalVoiceStateStore.getSelfMute(),
		self_deaf: partial?.self_deaf ?? LocalVoiceStateStore.getSelfDeaf(),
		self_video: partial?.self_video ?? LocalVoiceStateStore.getSelfVideo(),
		self_stream: partial?.self_stream ?? LocalVoiceStateStore.getSelfStream(),
		viewer_stream_key: partial?.viewer_stream_key ?? LocalVoiceStateStore.getViewerStreamKey(),
		connection_id: connectionId,
	});
}
