// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import type {CallVoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import Users from '@app/features/user/state/Users';
import VoiceRuntime from '@app/features/voice/engine/VoiceRuntime';
import CallAvailability from '@app/features/voice/state/CallAvailability';
import CallState, {type GatewayCallData} from '@app/features/voice/state/CallState';

interface CallUpdatePayload {
	channel_id: string;
	message_id?: string;
	region?: string;
	ringing?: Array<string>;
	voice_states?: Array<CallVoiceState>;
}

export function handleCallUpdate(data: CallUpdatePayload, _context: GatewayHandlerContext): void {
	const callData: GatewayCallData = {
		channel_id: data.channel_id,
		message_id: data.message_id,
		region: data.region,
		ringing: data.ringing,
		voice_states: data.voice_states,
	};
	CallAvailability.setCallAvailable(data.channel_id);
	CallState.handleCallUpdate(callData);
	if (CallState.isUserPendingRinging(data.channel_id, Users.currentUserId)) VoiceRuntime.handleCallRing();
}
