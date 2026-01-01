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

import {reaction} from 'mobx';
import {Endpoints} from '~/Endpoints';
import HttpClient from '~/lib/HttpClient';
import CallInitiatorStore from '~/stores/CallInitiatorStore';
import CallStateStore from '~/stores/CallStateStore';
import ChannelStore from '~/stores/ChannelStore';
import GeoIPStore from '~/stores/GeoIPStore';
import SoundStore from '~/stores/SoundStore';
import UserStore from '~/stores/UserStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import {SoundType} from '~/utils/SoundUtils';

interface PendingRing {
	channelId: string;
	recipients: Array<string>;
	dispose: () => void;
}

let pendingRing: PendingRing | null = null;

export async function checkCallEligibility(channelId: string): Promise<{ringable: boolean}> {
	const response = await HttpClient.get<{ringable: boolean}>(Endpoints.CHANNEL_CALL(channelId));
	return response.body ?? {ringable: false};
}

async function ringCallRecipients(channelId: string, recipients?: Array<string>): Promise<void> {
	const latitude = GeoIPStore.latitude;
	const longitude = GeoIPStore.longitude;
	const body: {recipients?: Array<string>; latitude?: string; longitude?: string} = {};
	if (recipients) {
		body.recipients = recipients;
	}
	if (latitude && longitude) {
		body.latitude = latitude;
		body.longitude = longitude;
	}
	await HttpClient.post(Endpoints.CHANNEL_CALL_RING(channelId), body);
}

async function stopRingingCallRecipients(channelId: string, recipients?: Array<string>): Promise<void> {
	await HttpClient.post(Endpoints.CHANNEL_CALL_STOP_RINGING(channelId), recipients ? {recipients} : {});
}

export async function ringParticipants(channelId: string, recipients?: Array<string>): Promise<void> {
	return ringCallRecipients(channelId, recipients);
}

export async function stopRingingParticipants(channelId: string, recipients?: Array<string>): Promise<void> {
	return stopRingingCallRecipients(channelId, recipients);
}

function clearPendingRing(): void {
	if (pendingRing) {
		pendingRing.dispose();
		pendingRing = null;
	}
}

function setupPendingRing(channelId: string, recipients: Array<string>): void {
	clearPendingRing();

	const dispose = reaction(
		() => ({
			connected: MediaEngineStore.connected,
			currentChannelId: MediaEngineStore.channelId,
		}),
		({connected, currentChannelId}) => {
			if (connected && currentChannelId === channelId && pendingRing?.channelId === channelId) {
				void ringCallRecipients(channelId, pendingRing.recipients).catch((error) => {
					console.error('Failed to ring call recipients:', error);
				});
				clearPendingRing();
			}
		},
		{fireImmediately: true},
	);

	pendingRing = {channelId, recipients, dispose};
}

export function startCall(channelId: string, silent = false): void {
	const currentUser = UserStore.getCurrentUser();
	if (!currentUser) {
		return;
	}
	const channel = ChannelStore.getChannel(channelId);
	const recipients = channel ? channel.recipientIds.filter((id) => id !== currentUser.id) : [];

	CallInitiatorStore.markInitiated(channelId, recipients);

	if (!silent) {
		setupPendingRing(channelId, recipients);
	}

	void MediaEngineStore.connectToVoiceChannel(null, channelId);
}

export function joinCall(channelId: string): void {
	const currentUser = UserStore.getCurrentUser();
	if (!currentUser) {
		return;
	}
	CallStateStore.clearPendingRinging(channelId, [currentUser.id]);
	SoundStore.stopIncomingRing();
	SoundStore.playSound(SoundType.UserJoin);
	void MediaEngineStore.connectToVoiceChannel(null, channelId);
}

export async function leaveCall(channelId: string): Promise<void> {
	const currentUser = UserStore.getCurrentUser();
	if (!currentUser) {
		return;
	}

	if (pendingRing?.channelId === channelId) {
		clearPendingRing();
	}

	SoundStore.stopIncomingRing();

	const call = CallStateStore.getCall(channelId);
	const callRinging = call?.ringing ?? [];
	const initiatedRecipients = CallInitiatorStore.getInitiatedRecipients(channelId);
	const toStop =
		initiatedRecipients.length > 0 ? callRinging.filter((userId) => initiatedRecipients.includes(userId)) : callRinging;

	if (toStop.length > 0) {
		try {
			await stopRingingCallRecipients(channelId, toStop);
		} catch (error) {
			console.error('Failed to stop ringing pending recipients:', error);
		}
	}

	CallInitiatorStore.clearChannel(channelId);

	void MediaEngineStore.disconnectFromVoiceChannel('user');
}

export function rejectCall(channelId: string): void {
	const currentUser = UserStore.getCurrentUser();
	if (!currentUser) {
		return;
	}
	CallStateStore.clearPendingRinging(channelId, [currentUser.id]);
	const connectedChannelId = MediaEngineStore.channelId;
	if (connectedChannelId === channelId) {
		void MediaEngineStore.disconnectFromVoiceChannel('user');
	}
	void stopRingingCallRecipients(channelId).catch((error) => {
		console.error('Failed to stop ringing:', error);
	});
	SoundStore.stopIncomingRing();
	CallInitiatorStore.clearChannel(channelId);
}

export function ignoreCall(channelId: string): void {
	const currentUser = UserStore.getCurrentUser();
	if (!currentUser) {
		return;
	}
	CallStateStore.clearPendingRinging(channelId, [currentUser.id]);
	void stopRingingCallRecipients(channelId, [currentUser.id]).catch((error) => {
		console.error('Failed to stop ringing:', error);
	});
	SoundStore.stopIncomingRing();
}
