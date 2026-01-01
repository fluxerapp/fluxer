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

import type {WebhookEvent} from 'livekit-server-sdk';
import {WebhookReceiver} from 'livekit-server-sdk';
import {Logger} from '~/Logger';
import type {IUserRepository} from '~/user/IUserRepository';
import type {VoiceTopology} from '~/voice/VoiceTopology';
import type {IGatewayService} from './IGatewayService';
import type {ILiveKitService} from './ILiveKitService';
import type {IVoiceRoomStore} from './IVoiceRoomStore';
import {isDMRoom, parseParticipantIdentity, parseParticipantMetadataWithRaw, parseRoomName} from './VoiceRoomContext';

const FREE_MAX_WIDTH = 1280;
const FREE_MAX_HEIGHT = 720;

export class LiveKitWebhookService {
	private receivers: Map<string, WebhookReceiver>;
	private serverMap: Map<string, {regionId: string; serverId: string}>;

	constructor(
		private voiceRoomStore: IVoiceRoomStore,
		private gatewayService: IGatewayService,
		private userRepository: IUserRepository,
		private liveKitService: ILiveKitService,
		private voiceTopology: VoiceTopology,
	) {
		this.receivers = new Map();
		this.serverMap = new Map();
		this.rebuildReceivers();
		this.voiceTopology.registerSubscriber(() => this.rebuildReceivers());
	}

	async verifyAndParse(body: string, authHeader: string | undefined): Promise<{event: WebhookEvent; apiKey: string}> {
		if (!authHeader) {
			throw new Error('Missing authorization header');
		}

		let lastError: Error | null = null;
		for (const [apiKey, receiver] of this.receivers.entries()) {
			try {
				const event = await receiver.receive(body, authHeader);
				return {event: event as WebhookEvent, apiKey};
			} catch (error) {
				lastError = error as Error;
			}
		}

		throw lastError || new Error('No webhook receivers configured');
	}

	private rebuildReceivers(): void {
		const newReceivers = new Map<string, WebhookReceiver>();
		const newServerMap = new Map<string, {regionId: string; serverId: string}>();
		const regions = this.voiceTopology.getAllRegions();

		for (const region of regions) {
			const servers = this.voiceTopology.getServersForRegion(region.id);
			for (const server of servers) {
				newReceivers.set(server.apiKey, new WebhookReceiver(server.apiKey, server.apiSecret));
				newServerMap.set(server.apiKey, {regionId: region.id, serverId: server.serverId});
			}
		}

		this.receivers = newReceivers;
		this.serverMap = newServerMap;
	}

	async handleRoomFinished(event: WebhookEvent): Promise<void> {
		if (event.event !== 'room_finished' || !event.room) {
			return;
		}

		const roomName = event.room.name;
		const context = parseRoomName(roomName);

		if (!context) {
			Logger.warn({roomName}, 'Unknown room name format');
			return;
		}

		Logger.debug({roomName, type: context.type}, 'LiveKit room finished, clearing server pinning');

		if (isDMRoom(context)) {
			await this.voiceRoomStore.deleteRoomServer(undefined, context.channelId);
			Logger.debug({channelId: context.channelId.toString()}, 'Cleared DM voice room server pinning');
		} else {
			await this.voiceRoomStore.deleteRoomServer(context.guildId, context.channelId);
			Logger.debug(
				{guildId: context.guildId.toString(), channelId: context.channelId.toString()},
				'Cleared guild voice room server pinning',
			);

			try {
				const result = await this.gatewayService.disconnectAllVoiceUsersInChannel({
					guildId: context.guildId,
					channelId: context.channelId,
				});
				Logger.info(
					{
						guildId: context.guildId.toString(),
						channelId: context.channelId.toString(),
						disconnectedCount: result.disconnectedCount,
					},
					'Cleaned up zombie voice connections for finished room',
				);
			} catch (error) {
				Logger.error(
					{error, guildId: context.guildId.toString(), channelId: context.channelId.toString()},
					'Failed to clean up voice connections for finished room',
				);
			}
		}
	}

	async handleParticipantJoined(event: WebhookEvent): Promise<void> {
		if (event.event !== 'participant_joined') {
			return;
		}

		const {participant} = event;
		if (!participant?.metadata) {
			Logger.debug('Participant joined without metadata, skipping');
			return;
		}

		const parsed = parseParticipantMetadataWithRaw(participant.metadata);
		if (!parsed) {
			Logger.warn({metadata: participant.metadata}, 'Failed to parse participant metadata');
			return;
		}

		const {context} = parsed;

		try {
			if (context.type === 'dm') {
				Logger.info(
					{
						channelId: context.channelId.toString(),
						connectionId: context.connectionId,
						participantIdentity: participant.identity,
					},
					'LiveKit participant_joined - confirming DM call connection',
				);

				await this.gatewayService.confirmDMCallConnection({
					channelId: context.channelId,
					connectionId: context.connectionId,
				});
			} else {
				Logger.info(
					{
						guildId: context.guildId.toString(),
						connectionId: context.connectionId,
						participantIdentity: participant.identity,
					},
					'LiveKit participant_joined - confirming guild voice connection',
				);

				await this.gatewayService.confirmVoiceConnectionFromLiveKit({
					guildId: context.guildId,
					connectionId: context.connectionId,
				});
			}
		} catch (error) {
			Logger.error({error, type: context.type}, 'Error processing participant_joined');
		}
	}

	async handleParticipantLeft(event: WebhookEvent): Promise<void> {
		if (event.event !== 'participant_left') {
			return;
		}

		const {participant} = event;
		if (!participant?.metadata) {
			Logger.debug('Participant left without metadata, skipping');
			return;
		}

		const parsed = parseParticipantMetadataWithRaw(participant.metadata);
		if (!parsed) {
			Logger.warn({metadata: participant.metadata}, 'Failed to parse participant metadata');
			return;
		}

		const {context} = parsed;

		try {
			if (context.type === 'dm') {
				Logger.info(
					{
						channelId: context.channelId.toString(),
						userId: context.userId.toString(),
						connectionId: context.connectionId,
					},
					'LiveKit participant_left - disconnecting DM call user',
				);

				await this.gatewayService.disconnectDMCallUserIfInChannel({
					channelId: context.channelId,
					userId: context.userId,
					connectionId: context.connectionId,
				});
			} else {
				Logger.info(
					{
						guildId: context.guildId.toString(),
						userId: context.userId.toString(),
						channelId: context.channelId.toString(),
						connectionId: context.connectionId,
					},
					'LiveKit participant_left - disconnecting guild voice user',
				);

				await this.gatewayService.disconnectVoiceUserIfInChannel({
					guildId: context.guildId,
					userId: context.userId,
					connectionId: context.connectionId,
					expectedChannelId: context.channelId,
				});
			}
		} catch (error) {
			Logger.error({error, type: context.type}, 'Error processing participant_left');
		}
	}

	async handleTrackPublished(event: WebhookEvent, apiKey: string): Promise<void> {
		if (event.event !== 'track_published') {
			return;
		}

		const {room, participant, track} = event;
		if (!room || !participant || !track) {
			Logger.debug('Track published without required data, skipping');
			return;
		}

		if (track.type !== 1) {
			return;
		}

		try {
			const identity = parseParticipantIdentity(participant.identity);
			if (!identity) {
				Logger.warn({identity: participant.identity}, 'Unexpected participant identity format');
				return;
			}

			const {userId, connectionId} = identity;

			const user = await this.userRepository.findUnique(userId);
			if (!user) {
				Logger.warn({userId: userId.toString()}, 'User not found for track_published event');
				return;
			}

			if (user.isPremium()) {
				return;
			}

			const exceedsResolution = track.width > FREE_MAX_WIDTH || track.height > FREE_MAX_HEIGHT;
			if (!exceedsResolution) {
				return;
			}

			Logger.warn(
				{userId: userId.toString(), width: track.width, height: track.height},
				'Non-premium user attempting to publish video exceeding free tier limits - disconnecting',
			);

			const roomContext = parseRoomName(room.name);
			if (!roomContext) {
				Logger.warn({roomName: room.name}, 'Unknown room name format, cannot disconnect');
				return;
			}

			let regionId: string | undefined;
			let serverId: string | undefined;

			if (participant.metadata) {
				const parsed = parseParticipantMetadataWithRaw(participant.metadata);
				if (parsed) {
					regionId = parsed.raw.region_id;
					serverId = parsed.raw.server_id;
				}
			}

			if (!regionId || !serverId) {
				const serverInfo = this.serverMap.get(apiKey);
				if (serverInfo) {
					regionId = serverInfo.regionId;
					serverId = serverInfo.serverId;
				}
			}

			if (!regionId || !serverId) {
				const guildId = isDMRoom(roomContext) ? undefined : roomContext.guildId;
				const pinnedServer = await this.voiceRoomStore.getPinnedRoomServer(guildId, roomContext.channelId);
				if (pinnedServer) {
					regionId = pinnedServer.regionId;
					serverId = pinnedServer.serverId;
				}
			}

			if (!regionId || !serverId) {
				Logger.warn(
					{participantId: participant.identity, roomName: room.name, apiKey},
					'Missing region or server info, cannot disconnect',
				);
				return;
			}

			const guildId = isDMRoom(roomContext) ? undefined : roomContext.guildId;

			Logger.info(
				{
					userId: userId.toString(),
					type: roomContext.type,
					guildId: guildId?.toString(),
					channelId: roomContext.channelId.toString(),
					regionId,
					serverId,
					width: track.width,
					height: track.height,
				},
				'Disconnecting non-premium user for exceeding video quality limits',
			);

			await this.liveKitService.disconnectParticipant({
				userId,
				guildId,
				channelId: roomContext.channelId,
				connectionId,
				regionId,
				serverId,
			});

			if (isDMRoom(roomContext)) {
				await this.gatewayService.disconnectDMCallUserIfInChannel({
					channelId: roomContext.channelId,
					userId,
					connectionId,
				});
			} else {
				await this.gatewayService.disconnectVoiceUserIfInChannel({
					guildId: roomContext.guildId,
					userId,
					connectionId,
					expectedChannelId: roomContext.channelId,
				});
			}

			Logger.info(
				{
					userId: userId.toString(),
					type: roomContext.type,
					guildId: guildId?.toString(),
					channelId: roomContext.channelId.toString(),
					width: track.width,
					height: track.height,
				},
				'Disconnected non-premium user for exceeding video quality limits',
			);
		} catch (error) {
			Logger.error({error}, 'Error processing track_published event');
		}
	}

	async processEvent(data: {event: WebhookEvent; apiKey: string}): Promise<void> {
		const {event, apiKey} = data;
		switch (event.event) {
			case 'participant_joined':
				await this.handleParticipantJoined(event);
				break;
			case 'participant_left':
			case 'participant_connection_aborted':
				await this.handleParticipantLeft(event);
				break;
			case 'room_finished':
				await this.handleRoomFinished(event);
				break;
			case 'track_published':
				await this.handleTrackPublished(event, apiKey);
				break;
			default:
				Logger.debug({event: event.event}, 'Ignoring LiveKit webhook event');
		}
	}
}
