// SPDX-License-Identifier: AGPL-3.0-or-later

import {showDmActionErrorModal} from '@app/features/app/components/alerts/DmActionErrorModal';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import GeoIP from '@app/features/app/state/GeoIP';
import Channels from '@app/features/channel/state/Channels';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import Sound from '@app/features/ui/state/Sound';
import Users from '@app/features/user/state/Users';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import CallInitiator from '@app/features/voice/state/CallInitiator';
import CallState from '@app/features/voice/state/CallState';
import RtcRegions from '@app/features/voice/state/RtcRegions';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {AUTOMATIC_VOICE_REGION_ID} from '@fluxer/constants/src/ChannelConstants';
import type {RtcRegionResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {reaction} from 'mobx';

interface CallEligibilityResponse {
	ringable: boolean;
}

interface CallRingRequest {
	recipients?: Array<string>;
	latitude?: string;
	longitude?: string;
}

interface CallRegionPatch {
	region: string;
	latitude?: string;
	longitude?: string;
}

interface PendingRing {
	channelId: string;
	recipients: Array<string>;
	dispose: () => void;
}

const logger = new Logger('CallCommands');

let pendingRing: PendingRing | null = null;

function currentGeoCoordinates(): Pick<CallRingRequest, 'latitude' | 'longitude'> {
	const latitude = GeoIP.latitude;
	const longitude = GeoIP.longitude;
	return latitude && longitude ? {latitude, longitude} : {};
}

function callRingRequest(recipients?: Array<string>): CallRingRequest {
	return {
		...(recipients ? {recipients} : {}),
		...currentGeoCoordinates(),
	};
}

function stopRingingRequest(recipients?: Array<string>): Pick<CallRingRequest, 'recipients'> {
	return recipients ? {recipients} : {};
}

function channelRecipientIds(channelId: string, currentUserId: string): Array<string> {
	const channel = Channels.getChannel(channelId);
	return channel ? channel.recipientIds.filter((id) => id !== currentUserId) : [];
}

function initiatedRingingRecipients(channelId: string): Array<string> {
	const callRinging = CallState.getCall(channelId)?.ringing ?? [];
	const initiatedRecipients = CallInitiator.getInitiatedRecipients(channelId);
	const initiatedSet = initiatedRecipients.length > 0 ? new Set(initiatedRecipients) : null;
	return initiatedSet ? callRinging.filter((userId) => initiatedSet.has(userId)) : callRinging;
}

function callRegionPatch(region: string): CallRegionPatch {
	return {
		region,
		...(region === AUTOMATIC_VOICE_REGION_ID ? currentGeoCoordinates() : {}),
	};
}

export async function checkCallEligibility(channelId: string): Promise<CallEligibilityResponse> {
	const response = await http.get<CallEligibilityResponse>(Endpoints.CHANNEL_CALL(channelId));
	return response.body ?? {ringable: false};
}

async function ringCallRecipients(channelId: string, recipients?: Array<string>): Promise<void> {
	await http.post(Endpoints.CHANNEL_CALL_RING(channelId), {body: callRingRequest(recipients)});
}

async function stopRingingCallRecipients(channelId: string, recipients?: Array<string>): Promise<void> {
	await http.post(Endpoints.CHANNEL_CALL_STOP_RINGING(channelId), {body: stopRingingRequest(recipients)});
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
			connected: MediaEngine.connected,
			currentChannelId: MediaEngine.channelId,
		}),
		({connected, currentChannelId}) => {
			if (connected && currentChannelId === channelId && pendingRing?.channelId === channelId) {
				void ringCallRecipients(channelId, pendingRing.recipients).catch((error) => {
					if (failureCode(error) === APIErrorCodes.NEW_CONVERSATIONS_LIMITED) {
						showDmActionErrorModal(error);
						void leaveCall(channelId);
						return;
					}
					logger.error('Failed to ring call recipients:', error);
				});
				clearPendingRing();
			}
		},
		{fireImmediately: true},
	);
	pendingRing = {channelId, recipients, dispose};
}

export function startCall(channelId: string): void {
	const currentUser = Users.getCurrentUser();
	if (!currentUser) {
		return;
	}
	const recipients = channelRecipientIds(channelId, currentUser.id);
	CallInitiator.markInitiated(channelId, recipients);
	setupPendingRing(channelId, recipients);
	void MediaEngine.connectToVoiceChannel(null, channelId);
}

export function joinCall(channelId: string): void {
	const currentUser = Users.getCurrentUser();
	if (!currentUser) {
		return;
	}
	CallState.clearPendingRinging(channelId, [currentUser.id]);
	Sound.stopIncomingRing();
	void MediaEngine.connectToVoiceChannel(null, channelId);
}

export async function leaveCall(channelId: string): Promise<void> {
	const currentUser = Users.getCurrentUser();
	if (!currentUser) {
		return;
	}
	if (pendingRing?.channelId === channelId) {
		clearPendingRing();
	}
	Sound.stopIncomingRing();
	const toStop = initiatedRingingRecipients(channelId);
	if (toStop.length > 0) {
		try {
			await stopRingingCallRecipients(channelId, toStop);
		} catch (error) {
			logger.error('Failed to stop ringing pending recipients:', error);
		}
	}
	CallInitiator.clearChannel(channelId);
	void MediaEngine.disconnectFromVoiceChannel('user');
}

export function rejectCall(channelId: string): void {
	const currentUser = Users.getCurrentUser();
	if (!currentUser) {
		return;
	}
	CallState.clearPendingRinging(channelId, [currentUser.id]);
	const connectedChannelId = MediaEngine.channelId;
	if (connectedChannelId === channelId) {
		void MediaEngine.disconnectFromVoiceChannel('user');
	}
	void stopRingingCallRecipients(channelId).catch((error) => {
		logger.error('Failed to stop ringing:', error);
	});
	CallInitiator.clearChannel(channelId);
}

export function ignoreCall(channelId: string): void {
	const currentUser = Users.getCurrentUser();
	if (!currentUser) {
		return;
	}
	CallState.clearPendingRinging(channelId, [currentUser.id]);
	void stopRingingCallRecipients(channelId, [currentUser.id]).catch((error) => {
		logger.error('Failed to stop ringing:', error);
	});
}

export async function fetchCallRegions(channelId: string): Promise<Array<RtcRegionResponse>> {
	const channel = Channels.getChannel(channelId);
	if (channel?.isPrivate()) {
		return RtcRegions.getRegions();
	}
	const response = await http.get<Array<RtcRegionResponse>>(Endpoints.CHANNEL_RTC_REGIONS(channelId));
	return response.body ?? [];
}

export async function updateCallRegion(channelId: string, region: string): Promise<void> {
	await http.patch(Endpoints.CHANNEL_CALL(channelId), {body: callRegionPatch(region)});
}
