// SPDX-License-Identifier: AGPL-3.0-or-later

import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import CallInitiator from '@app/features/voice/state/CallInitiator';
import {
	type Call,
	CallLayout,
	type CallStateEvent,
	type CallStateSnapshot,
	createCallStateSnapshot,
	type GatewayCallData,
	getActiveCallsFromSnapshot,
	getCallFromSnapshot,
	hasActiveCallInSnapshot,
	isDifferentCallInstance,
	isUserPendingRingingInSnapshot,
	transitionCallStateSnapshot,
} from '@app/features/voice/state/CallStateMachine';
import {ME} from '@fluxer/constants/src/AppConstants';
import {makeAutoObservable, observable} from 'mobx';

export enum CallMode {
	MINIMUM = 'MINIMUM',
	NORMAL = 'NORMAL',
	FULL_SCREEN = 'FULL_SCREEN',
}

export {type Call, CallLayout, type GatewayCallData};

class CallState {
	calls = observable.map<string, Call>();
	private snapshot: CallStateSnapshot = createCallStateSnapshot();

	constructor() {
		makeAutoObservable<CallState, 'snapshot' | 'transition' | 'syncCallsFromSnapshot'>(
			this,
			{
				snapshot: false,
				transition: false,
				syncCallsFromSnapshot: false,
				getCall: false,
				getActiveCalls: false,
				hasActiveCall: false,
				isCallActive: false,
				getCallLayout: false,
				getMessageId: false,
				getParticipants: false,
				isUserPendingRinging: false,
			},
			{autoBind: true},
		);
	}

	getCall(channelId: string): Call | undefined {
		return this.calls.get(channelId);
	}

	getActiveCalls(): Array<Call> {
		const participantsByChannel: Record<string, Array<string>> = {};
		for (const call of this.calls.values()) {
			participantsByChannel[call.channelId] = this.getParticipants(call.channelId);
		}
		return getActiveCallsFromSnapshot(this.snapshot, participantsByChannel);
	}

	hasActiveCall(channelId: string): boolean {
		return hasActiveCallInSnapshot(this.snapshot, channelId, this.getParticipants(channelId));
	}

	isCallActive(channelId: string, messageId?: string): boolean {
		const call = this.calls.get(channelId);
		if (!call) return false;
		if (messageId) return call.messageId === messageId;
		return call.region != null;
	}

	getCallLayout(channelId: string): CallLayout {
		const call = this.calls.get(channelId);
		const connectedChannelId = MediaEngine.channelId;
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
		const voiceStates = MediaEngine.getAllVoiceStatesInChannel(ME, channelId);
		const participantIds: Array<string> = [];
		const seenParticipantIds = new Set<string>();
		for (const connectionId in voiceStates) {
			const voiceState = voiceStates[connectionId];
			const userId = voiceState?.user_id;
			if (!userId || seenParticipantIds.has(userId)) continue;
			seenParticipantIds.add(userId);
			participantIds.push(userId);
		}
		return participantIds.sort();
	}

	clearPendingRinging(channelId: string, userIds?: Array<string>): void {
		this.transition({type: 'ringing.clear', channelId, userIds});
	}

	isUserPendingRinging(channelId: string, userId?: string | null): boolean {
		return isUserPendingRingingInSnapshot(this.snapshot, channelId, userId);
	}

	handleCallCreate(data: {channelId: string; call?: GatewayCallData}): void {
		if (isDifferentCallInstance(this.calls.get(data.channelId), data.call)) {
			CallInitiator.clearChannel(data.channelId);
		}
		this.transition({type: 'call.create', channelId: data.channelId, call: data.call});
	}

	handleCallUpdate(data: GatewayCallData): void {
		if (isDifferentCallInstance(this.calls.get(data.channel_id), data)) {
			CallInitiator.clearChannel(data.channel_id);
		}
		this.transition({type: 'call.update', call: data});
	}

	handleCallDelete(data: {channelId: string}): void {
		CallInitiator.clearChannel(data.channelId);
		this.transition({type: 'call.delete', channelId: data.channelId});
	}

	handleCallLayoutUpdate(channelId: string, layout: CallLayout): void {
		this.transition({type: 'call.layout.update', channelId, layout});
	}

	handleCallParticipants(channelId: string, participants: Array<string>): void {
		this.transition({type: 'call.participants.update', channelId, participants});
	}

	private transition(event: CallStateEvent): void {
		this.snapshot = transitionCallStateSnapshot(this.snapshot, event);
		this.syncCallsFromSnapshot();
	}

	private syncCallsFromSnapshot(): void {
		const nextCalls = this.snapshot.context.calls;
		for (const channelId of Array.from(this.calls.keys())) {
			if (!getCallFromSnapshot(this.snapshot, channelId)) {
				this.calls.delete(channelId);
			}
		}
		for (const [channelId, call] of Object.entries(nextCalls)) {
			if (this.calls.get(channelId) !== call) {
				this.calls.set(channelId, call);
			}
		}
	}
}

export default new CallState();
