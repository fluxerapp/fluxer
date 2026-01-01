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

import type {ChannelRecord} from '~/records/ChannelRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import CallStateStore, {type Call} from '~/stores/CallStateStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';

export type CallHeaderControlsVariant = 'incoming' | 'join' | 'connecting' | 'inCall' | 'hidden';

export interface CallHeaderState {
	call: Call | null;
	callExistsAndOngoing: boolean;
	controlsVariant: CallHeaderControlsVariant;
	isDeviceInRoomForChannelCall: boolean;
	isDeviceConnectingToChannelCall: boolean;
	isRingingForCurrentUserOnThisDevice: boolean;
}

export function useCallHeaderState(channel?: ChannelRecord | null): CallHeaderState {
	const channelId = channel?.id ?? null;
	const call = channelId ? (CallStateStore.getCall(channelId) ?? null) : null;
	const hasParticipants = channelId && call ? CallStateStore.getParticipants(channelId).length > 0 : false;
	const callHasPendingRinging = Boolean(call && call.ringing.length > 0);
	const callExistsAndOngoing = Boolean(call && (call.region !== null || hasParticipants || callHasPendingRinging));

	const currentUserId = AuthenticationStore.currentUserId;
	const isRingingForCurrentUserOnThisDevice = Boolean(
		currentUserId && channelId && CallStateStore.isUserPendingRinging(channelId, currentUserId),
	);

	const normalizedGuildId = channel?.guildId ?? null;
	const matchesConnectionContext = Boolean(
		channelId && MediaEngineStore.channelId === channelId && (MediaEngineStore.guildId ?? null) === normalizedGuildId,
	);
	const isDeviceInRoomForChannelCall = Boolean(MediaEngineStore.room && matchesConnectionContext);
	const isDeviceConnectingToChannelCall =
		matchesConnectionContext && (MediaEngineStore.connecting || (MediaEngineStore.connected && !MediaEngineStore.room));

	const controlsVariant: CallHeaderControlsVariant = !callExistsAndOngoing
		? 'hidden'
		: isDeviceInRoomForChannelCall
			? 'inCall'
			: isRingingForCurrentUserOnThisDevice
				? 'incoming'
				: isDeviceConnectingToChannelCall
					? 'connecting'
					: 'join';

	return {
		call,
		callExistsAndOngoing,
		controlsVariant,
		isDeviceInRoomForChannelCall,
		isDeviceConnectingToChannelCall,
		isRingingForCurrentUserOnThisDevice,
	};
}
