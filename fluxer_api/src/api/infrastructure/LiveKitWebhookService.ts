// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID} from '@app/api/BrandedTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ILiveKitService, LiveKitParticipantRef} from '@app/api/infrastructure/ILiveKitService';
import type {IVoiceRoomStore} from '@app/api/infrastructure/IVoiceRoomStore';
import {SERVER_MUTE_ATTRIBUTE} from '@app/api/infrastructure/LiveKitService';
import {isDMRoom, parseParticipantMetadataWithRaw, parseRoomName} from '@app/api/infrastructure/VoiceRoomContext';
import {Logger} from '@app/api/Logger';
import {
	voiceParticipantConfirmUnavailableTotal,
	voiceParticipantsDisconnectedTotal,
} from '@app/api/voice/VoiceParticipantMetrics';
import type {VoiceTopology} from '@app/api/voice/VoiceTopology';
import {ms} from 'itty-time';
import type {ParticipantPermission, WebhookEvent} from 'livekit-server-sdk';
import {TrackSource, WebhookReceiver} from 'livekit-server-sdk';

const WEBHOOK_CLOCK_TOLERANCE_SECONDS = 60;

interface SourceServer {
	regionId: string;
	serverId: string;
}

export const REXCTL_TEST_ROOM_PREFIX = 'rexctl-test-';

interface VoiceWebhookParticipantContext {
	readonly type: 'dm' | 'guild';
	readonly channelId: ChannelID;
	readonly guildId?: GuildID;
}

type ParticipantContext = NonNullable<ReturnType<typeof parseParticipantMetadataWithRaw>>['context'];

type ConfirmOutcome =
	| {status: 'confirmed'}
	| {status: 'rejected'; error: string | undefined}
	| {status: 'unavailable'; error: unknown};

const CONFIRM_ATTEMPTS = 2;
const DM_CONFIRM_RECHECK_DELAYS_MS = [ms('5 seconds'), ms('10 seconds')];

function sleep(durationMs: number): Promise<void> {
	return new Promise((resolve) => {
		setTimeout(resolve, durationMs);
	});
}

export class LiveKitWebhookService {
	private receivers: Map<string, WebhookReceiver>;
	private serversByKey: Map<string, Array<SourceServer>>;

	constructor(
		private voiceRoomStore: IVoiceRoomStore,
		private gatewayService: IGatewayService,
		private liveKitService: ILiveKitService,
		private voiceTopology: VoiceTopology,
	) {
		this.receivers = new Map();
		this.serversByKey = new Map();
		this.rebuildReceivers();
		this.voiceTopology.registerSubscriber(() => this.rebuildReceivers());
	}

	async verifyAndParse(
		body: string,
		authHeader: string | undefined,
	): Promise<{
		event: WebhookEvent;
		apiKey: string;
	}> {
		if (!authHeader) {
			throw new Error('Missing authorization header');
		}
		let lastError: Error | null = null;
		for (const [apiKey, receiver] of this.receivers.entries()) {
			try {
				const event = await receiver.receive(body, authHeader, false, WEBHOOK_CLOCK_TOLERANCE_SECONDS);
				return {event: event as WebhookEvent, apiKey};
			} catch (error) {
				lastError = error as Error;
			}
		}
		throw lastError || new Error('No webhook receivers configured');
	}

	async handleWebhookRequest(params: {body: string; authHeader: string | undefined}): Promise<{
		status: number;
		body: string | null;
	}> {
		const {body, authHeader} = params;
		Logger.debug(
			{
				bodySize: body.length,
				hasAuthHeader: Boolean(authHeader),
			},
			'Received LiveKit webhook request',
		);
		try {
			const data = await this.verifyAndParse(body, authHeader);
			const eventName = data.event.event;
			Logger.debug(
				{
					apiKey: data.apiKey,
					event: eventName,
					roomName: data.event.room?.name ?? null,
					participantIdentity: data.event.participant?.identity ?? null,
					trackType: data.event.track?.type ?? null,
				},
				'Parsed LiveKit webhook event',
			);
			if (data.event.numDropped != null && data.event.numDropped > 0) {
				Logger.warn(
					{
						numDropped: data.event.numDropped,
						roomName: data.event.room?.name ?? null,
						eventType: data.event.event,
					},
					'LiveKit webhook reports dropped events',
				);
			}
			await this.processEvent(data);
			return {status: 200, body: null};
		} catch (error) {
			Logger.debug({error}, 'Error processing LiveKit webhook');
			return {status: 400, body: 'Invalid webhook'};
		}
	}

	private rebuildReceivers(): void {
		const newReceivers = new Map<string, WebhookReceiver>();
		const newServersByKey = new Map<string, Array<SourceServer>>();
		const regions = this.voiceTopology.getAllRegions();
		for (const region of regions) {
			const servers = this.voiceTopology.getServersForRegion(region.id);
			for (const server of servers) {
				newReceivers.set(server.apiKey, new WebhookReceiver(server.apiKey, server.apiSecret));
				const sharing = newServersByKey.get(server.apiKey) ?? [];
				sharing.push({regionId: region.id, serverId: server.serverId});
				newServersByKey.set(server.apiKey, sharing);
			}
		}
		this.receivers = newReceivers;
		this.serversByKey = newServersByKey;
		Logger.debug(
			{
				regionCount: regions.length,
				serverCount: newReceivers.size,
			},
			'Rebuilt LiveKit webhook receivers',
		);
	}

	private resolveSource(
		apiKey: string,
		raw: {region_id?: string; server_id?: string} | undefined,
	): SourceServer | undefined {
		const servers = this.serversByKey.get(apiKey) ?? [];
		if (servers.length === 1) {
			return servers[0];
		}
		if (!raw?.region_id || !raw.server_id) {
			return undefined;
		}
		return servers.find((server) => server.regionId === raw.region_id && server.serverId === raw.server_id);
	}

	private async isFromPinnedServer(
		guildId: GuildID | undefined,
		channelId: ChannelID,
		source: SourceServer | undefined,
	): Promise<boolean> {
		if (!source) {
			return false;
		}
		const pinned = await this.voiceRoomStore.getPinnedRoomServer(guildId, channelId);
		if (pinned && (pinned.regionId !== source.regionId || pinned.serverId !== source.serverId)) {
			Logger.debug(
				{
					guildId: guildId?.toString(),
					channelId: channelId.toString(),
					sourceServer: source.serverId,
					pinnedServer: pinned.serverId,
				},
				'Ignoring LiveKit webhook from a server the room is not pinned to',
			);
			return false;
		}
		return true;
	}

	async handleRoomFinished(event: WebhookEvent, apiKey: string): Promise<void> {
		if (event.event !== 'room_finished' || !event.room) {
			return;
		}
		const roomName = event.room.name;
		const context = parseRoomName(roomName);
		if (!context) {
			Logger.warn({roomName}, 'Unknown room name format');
			return;
		}
		const guildId = isDMRoom(context) ? undefined : context.guildId;
		Logger.debug(
			{roomName, contextType: context.type, guildId: guildId?.toString(), channelId: context.channelId.toString()},
			'Processing LiveKit room_finished event',
		);
		const source = this.resolveSource(apiKey, undefined);
		if (!(await this.isFromPinnedServer(guildId, context.channelId, source))) {
			return;
		}
		await this.voiceRoomStore.deleteRoomServer(guildId, context.channelId);
		Logger.debug(
			{guildId: guildId?.toString(), channelId: context.channelId.toString()},
			'Cleared voice room server pinning',
		);
	}

	private async handleParticipantJoined(event: WebhookEvent, apiKey: string): Promise<void> {
		if (event.event !== 'participant_joined' || !event.participant) {
			return;
		}
		const {participant} = event;
		const roomName = event.room?.name ?? null;
		const metadata = participant.metadata ?? '';
		const parsed = metadata ? parseParticipantMetadataWithRaw(metadata) : null;
		if (!parsed) {
			Logger.warn(
				{participantIdentity: participant.identity, roomName, hasMetadata: Boolean(metadata)},
				'LiveKit participant_joined without usable metadata - disconnecting participant',
			);
			await this.removeJoinedParticipant(
				await this.locateJoinedParticipant({apiKey, roomName, identity: participant.identity, metadata}),
				'unidentified',
			);
			return;
		}
		const {context, raw} = parsed;
		const outcome = await this.confirmWithRetry(context, raw.token_nonce);
		const logContext = {
			type: context.type,
			guildId: context.type === 'guild' ? context.guildId.toString() : undefined,
			channelId: context.channelId.toString(),
			connectionId: context.connectionId,
			participantIdentity: participant.identity,
			roomName,
		};
		if (outcome.status === 'confirmed') {
			return;
		}
		if (outcome.status === 'unavailable') {
			voiceParticipantConfirmUnavailableTotal.inc();
			Logger.warn(
				{...logContext, error: outcome.error},
				'LiveKit participant_joined confirm unavailable - leaving participant for the sweep',
			);
			return;
		}
		const located = await this.locateJoinedParticipant({apiKey, roomName, identity: participant.identity, metadata});
		if (context.type === 'dm' && participant.permission) {
			void this.holdDmJoin(context, raw.token_nonce, located, participant.permission, logContext);
			return;
		}
		Logger.warn(
			{...logContext, error: outcome.error},
			'LiveKit participant_joined rejected - disconnecting participant',
		);
		await this.removeJoinedParticipant(located, 'rejected');
	}

	private async holdDmJoin(
		context: ParticipantContext,
		tokenNonce: string,
		located: Array<LiveKitParticipantRef>,
		permission: ParticipantPermission,
		logContext: Record<string, unknown>,
	): Promise<void> {
		const setPermission = async (next: Partial<ParticipantPermission>) => {
			for (const ref of located) {
				await this.liveKitService.setParticipantPermission({...ref, permission: next});
			}
		};
		try {
			await setPermission({...permission, canSubscribe: false});
			let outcome: ConfirmOutcome = {status: 'rejected', error: undefined};
			for (const delayMs of DM_CONFIRM_RECHECK_DELAYS_MS) {
				await sleep(delayMs);
				outcome = await this.confirmWithRetry(context, tokenNonce);
				if (outcome.status !== 'rejected') {
					await setPermission(permission);
					return;
				}
			}
			Logger.warn(
				{...logContext, error: outcome.status === 'rejected' ? outcome.error : undefined},
				'LiveKit participant_joined rejected - disconnecting participant',
			);
			await this.removeJoinedParticipant(located, 'rejected');
		} catch (error) {
			Logger.error({...logContext, error}, 'Failed to recheck LiveKit call participant');
		}
	}

	private async confirmWithRetry(context: ParticipantContext, tokenNonce: string): Promise<ConfirmOutcome> {
		let lastError: unknown = null;
		for (let attempt = 0; attempt < CONFIRM_ATTEMPTS; attempt += 1) {
			try {
				const result = await this.gatewayService.confirmVoiceConnection({
					guildId: context.type === 'guild' ? context.guildId : undefined,
					channelId: context.channelId,
					connectionId: context.connectionId,
					tokenNonce,
				});
				return result.success ? {status: 'confirmed'} : {status: 'rejected', error: result.error};
			} catch (error) {
				lastError = error;
			}
		}
		return {status: 'unavailable', error: lastError};
	}

	private async locateJoinedParticipant(params: {
		apiKey: string;
		roomName: string | null;
		identity: string;
		metadata: string;
	}): Promise<Array<LiveKitParticipantRef>> {
		const {apiKey, roomName, identity, metadata} = params;
		const servers = this.serversByKey.get(apiKey) ?? [];
		if (roomName === null || servers.length === 0) {
			Logger.warn(
				{participantIdentity: identity, roomName, serverCount: servers.length},
				'Cannot determine where to disconnect LiveKit participant',
			);
			return [];
		}
		const refs = servers.map((server) => ({
			regionId: server.regionId,
			serverId: server.serverId,
			roomName,
			identity,
		}));
		if (refs.length === 1) {
			return refs;
		}
		const located: Array<LiveKitParticipantRef> = [];
		for (const ref of refs) {
			const result = await this.liveKitService.getParticipant(ref);
			if (result.status === 'ok' && result.participant?.metadata === metadata) {
				located.push(ref);
			}
		}
		return located;
	}

	private async removeJoinedParticipant(
		located: Array<LiveKitParticipantRef>,
		reason: 'unidentified' | 'rejected',
	): Promise<void> {
		if (located.length === 0) {
			return;
		}
		voiceParticipantsDisconnectedTotal.inc(`reason="${reason}"`);
		for (const ref of located) {
			await this.liveKitService.removeParticipant(ref);
		}
	}

	private async isParticipantStillInRoom(params: {
		participantIdentity: string;
		context: VoiceWebhookParticipantContext;
		source: SourceServer;
	}): Promise<'present' | 'absent' | 'unknown'> {
		const {participantIdentity, context} = params;
		const guildId = context.type === 'guild' ? context.guildId : undefined;
		const {regionId, serverId} = params.source;
		const result = await this.liveKitService.listParticipants({
			guildId,
			channelId: context.channelId,
			regionId,
			serverId,
		});
		if (result.status === 'error') {
			Logger.warn(
				{errorCode: result.errorCode, retryable: result.retryable, participantIdentity},
				'Cannot determine participant presence due to LiveKit lookup failure',
			);
			return 'unknown';
		}
		return result.participants.some((p) => p.identity === participantIdentity) ? 'present' : 'absent';
	}

	private async handleParticipantLeft(event: WebhookEvent, apiKey: string): Promise<void> {
		if (event.event !== 'participant_left' && event.event !== 'participant_connection_aborted') {
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
		const {context, raw} = parsed;
		const source = this.resolveSource(apiKey, raw);
		Logger.debug(
			{
				type: context.type,
				participantIdentity: participant.identity,
				roomName: event.room?.name ?? null,
				channelId: context.channelId.toString(),
				guildId: context.type === 'guild' ? context.guildId.toString() : undefined,
				userId: context.userId.toString(),
				connectionId: context.connectionId,
			},
			`Processing LiveKit ${event.event} event`,
		);
		try {
			const guildIdForPin = context.type === 'guild' ? context.guildId : undefined;
			if (!source || !(await this.isFromPinnedServer(guildIdForPin, context.channelId, source))) {
				return;
			}
			const presenceStatus = await this.isParticipantStillInRoom({
				participantIdentity: participant.identity,
				context,
				source,
			});
			if (presenceStatus === 'present') {
				Logger.warn(
					{
						type: context.type,
						participantIdentity: participant.identity,
						channelId: context.channelId.toString(),
						guildId: context.type === 'guild' ? context.guildId.toString() : undefined,
						connectionId: context.connectionId,
					},
					'Ignoring stale participant_left event because participant is still present in room',
				);
				return;
			}
			if (presenceStatus === 'unknown') {
				Logger.warn(
					{
						type: context.type,
						participantIdentity: participant.identity,
						channelId: context.channelId.toString(),
						guildId: context.type === 'guild' ? context.guildId.toString() : undefined,
						connectionId: context.connectionId,
					},
					'Skipping participant_left disconnect because participant presence is uncertain',
				);
				return;
			}
			const guildId = context.type === 'guild' ? context.guildId : undefined;
			Logger.info(
				{
					type: context.type,
					guildId: guildId?.toString(),
					userId: context.userId.toString(),
					channelId: context.channelId.toString(),
					connectionId: context.connectionId,
				},
				'LiveKit participant_left - disconnecting voice user',
			);
			const result = await this.gatewayService.disconnectVoiceUserIfInChannel({
				guildId,
				channelId: context.channelId,
				userId: context.userId,
				connectionId: context.connectionId,
			});
			Logger.debug(
				{
					type: context.type,
					guildId: guildId?.toString(),
					userId: context.userId.toString(),
					channelId: context.channelId.toString(),
					connectionId: context.connectionId,
					result,
				},
				'LiveKit participant_left voice disconnect result',
			);
		} catch (error) {
			Logger.error({error, type: context.type}, 'Error processing participant_left');
		}
	}

	private async handleTrackPublished(event: WebhookEvent, apiKey: string): Promise<void> {
		if (event.event !== 'track_published') {
			return;
		}
		const {participant, track} = event;
		if (!participant?.metadata || track?.source !== TrackSource.MICROPHONE || !track.sid) {
			return;
		}
		if (participant.attributes[SERVER_MUTE_ATTRIBUTE] !== 'true') {
			return;
		}
		const parsed = parseParticipantMetadataWithRaw(participant.metadata);
		if (!parsed) {
			Logger.warn({metadata: participant.metadata}, 'Failed to parse participant metadata');
			return;
		}
		const {context, raw} = parsed;
		const source = this.resolveSource(apiKey, raw);
		const guildId = context.type === 'guild' ? context.guildId : undefined;
		if (!source || !(await this.isFromPinnedServer(guildId, context.channelId, source))) {
			return;
		}
		Logger.debug(
			{participantIdentity: participant.identity, trackSid: track.sid},
			'Muting microphone published while server muted',
		);
		await this.liveKitService.muteMicrophoneTrack({
			guildId,
			channelId: context.channelId,
			userId: context.userId,
			connectionId: context.connectionId,
			regionId: source.regionId,
			serverId: source.serverId,
			trackSid: track.sid,
		});
	}

	async processEvent(data: {event: WebhookEvent; apiKey: string}): Promise<void> {
		const {event, apiKey} = data;
		Logger.debug({event: event.event, apiKey}, 'Dispatching LiveKit webhook event');
		if (event.room?.name.startsWith(REXCTL_TEST_ROOM_PREFIX)) {
			return;
		}
		switch (event.event) {
			case 'participant_joined':
				await this.handleParticipantJoined(event, apiKey);
				break;
			case 'participant_left':
			case 'participant_connection_aborted':
				await this.handleParticipantLeft(event, apiKey);
				break;
			case 'room_finished':
				await this.handleRoomFinished(event, apiKey);
				break;
			case 'track_published':
				await this.handleTrackPublished(event, apiKey);
				break;
			default:
				Logger.debug({event: event.event}, 'Ignoring LiveKit webhook event');
		}
		Logger.debug({event: event.event, apiKey}, 'Finished LiveKit webhook event');
	}
}
