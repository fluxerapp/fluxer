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

import {makeAutoObservable, observable} from 'mobx';
import {ME} from '~/Constants';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import VoiceStateManager from '~/stores/voice/VoiceStateManager';

export enum CallLayout {
	MINIMUM = 'MINIMUM',
	NORMAL = 'NORMAL',
	FULL_SCREEN = 'FULL_SCREEN',
}

interface VoiceState {
	user_id: string;
	channel_id?: string | null;
	session_id?: string;
	self_mute?: boolean;
	self_deaf?: boolean;
	self_video?: boolean;
	self_stream?: boolean;
}

export interface GatewayCallData {
	channel_id: string;
	message_id?: string;
	region?: string;
	ringing?: Array<string>;
	voice_states?: Array<VoiceState>;
}

export interface Call {
	channelId: string;
	messageId: string | null;
	region: string | null;
	ringing: Array<string>;
	layout: CallLayout;
	participants: Array<string>;
}

class CallStateStore {
	calls = observable.map<string, Call>();
	private pendingRinging = observable.map<string, Set<string>>();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getCall(channelId: string): Call | undefined {
		return this.calls.get(channelId);
	}

	getActiveCalls(): Array<Call> {
		return Array.from(this.calls.values()).filter((call) => this.hasActiveCall(call.channelId));
	}

	hasActiveCall(channelId: string): boolean {
		const call = this.calls.get(channelId);
		if (!call) return false;

		const participants = this.getParticipants(channelId);
		return participants.length > 0 || call.ringing.length > 0;
	}

	isCallActive(channelId: string, messageId?: string): boolean {
		const call = this.calls.get(channelId);
		if (!call) return false;
		if (messageId) return call.messageId === messageId;
		return call.region != null;
	}

	getCallLayout(channelId: string): CallLayout {
		const call = this.calls.get(channelId);
		const connectedChannelId = MediaEngineStore.channelId;
		if (call?.layout && channelId === connectedChannelId) {
			return call.layout;
		}
		return CallLayout.MINIMUM;
	}

	getMessageId(channelId: string): string | null {
		const call = this.calls.get(channelId);
		return call?.messageId ?? null;
	}

	getParticipants(channelId: string): Array<string> {
		const voiceStates = VoiceStateManager.getAllVoiceStatesInChannel(ME, channelId);
		return Object.values(voiceStates)
			.map((state) => state.user_id)
			.filter(Boolean);
	}

	clearPendingRinging(channelId: string, userIds?: Array<string>): void {
		if (!userIds || userIds.length === 0) {
			if (!this.pendingRinging.has(channelId)) return;
			this.pendingRinging.delete(channelId);
			this.syncCallRinging(channelId, new Set());
			return;
		}

		const normalized = this.normalizeUserIds(userIds);
		if (normalized.length === 0) return;

		const existing = this.pendingRinging.get(channelId);
		if (!existing) return;

		const nextSet = new Set(existing);
		let changed = false;

		for (const id of normalized) {
			if (nextSet.delete(id)) {
				changed = true;
			}
		}

		if (!changed) return;

		if (nextSet.size === 0) {
			this.pendingRinging.delete(channelId);
			this.syncCallRinging(channelId, new Set());
		} else {
			this.pendingRinging.set(channelId, nextSet);
			this.syncCallRinging(channelId, nextSet);
		}
	}

	isUserPendingRinging(channelId: string, userId?: string | null): boolean {
		if (!userId) return false;
		const set = this.pendingRinging.get(channelId);
		return Boolean(set?.has(userId));
	}

	handleCallCreate(data: {channelId: string; call?: GatewayCallData}): void {
		if (!data.call) return;

		const {ringing = [], message_id, region, voice_states = []} = data.call;
		const normalizedRinging = this.normalizeUserIds(ringing);
		const participants = this.extractParticipantsFromVoiceStates(voice_states);

		const existingCall = this.calls.get(data.channelId);
		const layout = existingCall?.layout ?? CallLayout.MINIMUM;

		const call: Call = {
			channelId: data.channelId,
			messageId: message_id ?? null,
			region: region ?? null,
			ringing: normalizedRinging,
			layout,
			participants,
		};

		this.calls.set(data.channelId, call);
		this.recordIncomingRinging(data.channelId, normalizedRinging);
	}

	handleCallUpdate(data: GatewayCallData): void {
		const {channel_id, ringing, message_id, region, voice_states} = data;

		const call = this.calls.get(channel_id);

		if (!call) {
			this.handleCallCreate({channelId: channel_id, call: data});
			return;
		}

		const normalizedRinging = this.normalizeUserIds(ringing);
		const hasRingingPayload = ringing !== undefined;
		const hasVoiceStatesPayload = voice_states !== undefined;
		const participants = hasVoiceStatesPayload
			? this.extractParticipantsFromVoiceStates(voice_states)
			: call.participants;

		const updatedCall: Call = {
			...call,
			ringing: hasRingingPayload ? normalizedRinging : call.ringing,
			messageId: message_id !== undefined ? message_id : call.messageId,
			region: region !== undefined ? region : call.region,
			participants,
		};

		this.calls.set(channel_id, updatedCall);
		if (hasRingingPayload) {
			if (normalizedRinging.length > 0) {
				this.recordIncomingRinging(channel_id, normalizedRinging);
			} else {
				this.clearPendingRinging(channel_id);
			}
		}
	}

	private normalizeUserIds(userIds?: Array<string>): Array<string> {
		if (!userIds || userIds.length === 0) return [];
		return userIds.map(String).filter(Boolean);
	}

	private extractParticipantsFromVoiceStates(voiceStates?: Array<VoiceState>): Array<string> {
		if (!voiceStates || voiceStates.length === 0) return [];
		return voiceStates.map((state) => state.user_id).filter((id): id is string => Boolean(id));
	}

	private recordIncomingRinging(channelId: string, userIds: Array<string>): void {
		if (userIds.length === 0) return;

		const existing = this.pendingRinging.get(channelId);
		const nextSet = existing ? new Set(existing) : new Set<string>();
		for (const id of userIds) {
			nextSet.add(id);
		}

		this.pendingRinging.set(channelId, nextSet);
		this.syncCallRinging(channelId, nextSet);
	}

	private syncCallRinging(channelId: string, ringSet?: Set<string>): void {
		const call = this.calls.get(channelId);
		if (!call) return;

		const nextSet = ringSet ?? this.pendingRinging.get(channelId) ?? new Set<string>();
		const nextRinging = Array.from(nextSet);
		const isSame =
			nextRinging.length === call.ringing.length && nextRinging.every((id, index) => call.ringing[index] === id);

		if (isSame) return;

		this.calls.set(channelId, {...call, ringing: nextRinging});
	}

	handleCallDelete(data: {channelId: string}): void {
		this.calls.delete(data.channelId);
		this.clearPendingRinging(data.channelId);
	}

	handleCallLayoutUpdate(channelId: string, layout: CallLayout): void {
		const call = this.calls.get(channelId);
		if (!call) return;
		call.layout = layout;
	}

	handleCallParticipants(channelId: string, participants: Array<string>): void {
		const call = this.calls.get(channelId);
		if (!call) return;

		const uniqueParticipants = Array.from(new Set(participants));
		call.participants = uniqueParticipants;
	}
}

export default new CallStateStore();
