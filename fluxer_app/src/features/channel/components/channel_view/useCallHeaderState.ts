// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import type {Channel} from '@app/features/channel/models/Channel';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import CallInitiator from '@app/features/voice/state/CallInitiator';
import CallState, {type Call} from '@app/features/voice/state/CallState';

type CallHeaderControlsVariant = 'hidden' | 'inCall' | 'incoming' | 'connecting' | 'join';

interface CallHeaderState {
	call: Call | null;
	callExistsAndOngoing: boolean;
	controlsVariant: CallHeaderControlsVariant;
	isDeviceInRoomForChannelCall: boolean;
	isDeviceConnectingToChannelCall: boolean;
	isRingingForCurrentUserOnThisDevice: boolean;
}

export function useCallHeaderState(channel?: Channel | null): CallHeaderState {
	const channelId = channel?.id ?? null;
	const call = channelId ? (CallState.getCall(channelId) ?? null) : null;
	const participantIds = channelId && call ? CallState.getParticipants(channelId) : [];
	const hasParticipants = participantIds.length > 0;
	const callHasPendingRinging = Boolean(call && call.ringing.length > 0);
	const callWasStartedOnThisDevice = Boolean(channelId && CallInitiator.hasInitiated(channelId));
	const callExistsAndOngoing = Boolean(
		call && (hasParticipants || callHasPendingRinging || callWasStartedOnThisDevice),
	);
	const currentUserId = Authentication.currentUserId;
	const isCurrentUserParticipantInCall = Boolean(currentUserId && channelId && participantIds.includes(currentUserId));
	const isRingingForCurrentUserOnThisDevice = Boolean(
		currentUserId &&
			channelId &&
			CallState.isUserPendingRinging(channelId, currentUserId) &&
			!CallInitiator.hasInitiated(channelId),
	);
	const normalizedGuildId = channel?.guildId ?? null;
	const matchesConnectionContext = Boolean(
		channelId && MediaEngine.channelId === channelId && (MediaEngine.guildId ?? null) === normalizedGuildId,
	);
	const isDeviceInRoomForChannelCall = Boolean(matchesConnectionContext && MediaEngine.room);
	const isDeviceConnectingToChannelCall =
		matchesConnectionContext && (MediaEngine.connecting || (MediaEngine.connected && !isDeviceInRoomForChannelCall));
	const controlsVariant: CallHeaderControlsVariant = !callExistsAndOngoing
		? 'hidden'
		: isDeviceInRoomForChannelCall
			? 'inCall'
			: isRingingForCurrentUserOnThisDevice
				? 'incoming'
				: isDeviceConnectingToChannelCall
					? 'connecting'
					: isCurrentUserParticipantInCall
						? 'inCall'
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
