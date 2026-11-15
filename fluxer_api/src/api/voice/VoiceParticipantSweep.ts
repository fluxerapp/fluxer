// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ILiveKitService} from '@app/api/infrastructure/ILiveKitService';
import {REXCTL_TEST_ROOM_PREFIX} from '@app/api/infrastructure/LiveKitWebhookService';
import {isDMRoom, parseParticipantMetadataWithRaw, parseRoomName} from '@app/api/infrastructure/VoiceRoomContext';
import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import {
	voiceParticipantSweepCandidatesTotal,
	voiceParticipantsDisconnectedTotal,
} from '@app/api/voice/VoiceParticipantMetrics';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import {ms} from 'itty-time';

const VOICE_PARTICIPANT_SETTLE_MS = ms('2 minutes');
const MAX_VOICE_PARTICIPANT_REMOVALS_PER_SWEEP = 25;
const SERVER_CONCURRENCY = 2;
const ROOM_CONCURRENCY = 8;
const IDENTITY_PATTERN = /^user_(\d+)_(.+)$/;

interface VoiceParticipantSighting {
	joinedAt: number;
	firstSeenAt: number;
}

export type VoiceParticipantSightings = Record<string, VoiceParticipantSighting>;

export interface VoiceParticipantSweepDeps {
	liveKitService: Pick<
		ILiveKitService,
		'listServers' | 'listRooms' | 'listParticipants' | 'getParticipant' | 'removeParticipant'
	>;
	gatewayService: Pick<IGatewayService, 'getVoiceStatesForChannel'>;
	logger: LoggerInterface;
	now: () => number;
	dryRun: boolean;
}

interface VoiceParticipantSweepResult {
	sightings: VoiceParticipantSightings;
	rooms: number;
	skippedRooms: number;
	candidates: number;
	removed: number;
	deferred: number;
}

type RoomContext = NonNullable<ReturnType<typeof parseRoomName>>;

interface SweepRoom {
	regionId: string;
	serverId: string;
	roomName: string;
	context: RoomContext;
}

interface DueParticipant {
	key: string;
	room: SweepRoom;
	identity: string;
	gatewayKey: string | null;
	joinedAt: number;
	firstSeenAt: number;
}

export async function sweepVoiceParticipants(
	deps: VoiceParticipantSweepDeps,
	previous: VoiceParticipantSightings,
): Promise<VoiceParticipantSweepResult> {
	const rooms = await listSweepRooms(deps);
	const sightings: VoiceParticipantSightings = {};
	const due: Array<DueParticipant> = [];
	let skippedRooms = 0;
	await mapWithConcurrency(rooms, ROOM_CONCURRENCY, async (room) => {
		const checked = await checkRoom(deps, room, previous, sightings, due);
		if (!checked) skippedRooms += 1;
	});
	const candidates = Object.keys(sightings).length;
	voiceParticipantSweepCandidatesTotal.inc('', candidates);
	due.sort((a, b) => a.firstSeenAt - b.firstSeenAt);
	const removable = due.slice(0, MAX_VOICE_PARTICIPANT_REMOVALS_PER_SWEEP);
	let removed = 0;
	for (const participant of removable) {
		const outcome = await removeIfStillMissing(deps, participant);
		if (outcome !== 'unknown') {
			delete sightings[participant.key];
		}
		if (outcome === 'removed') {
			removed += 1;
		}
	}
	return {
		sightings,
		rooms: rooms.length,
		skippedRooms,
		candidates,
		removed,
		deferred: due.length - removable.length,
	};
}

async function listSweepRooms(deps: VoiceParticipantSweepDeps): Promise<Array<SweepRoom>> {
	const perServer = await mapWithConcurrency(deps.liveKitService.listServers(), SERVER_CONCURRENCY, async (server) => {
		const result = await deps.liveKitService.listRooms(server);
		if (result.status === 'error') {
			deps.logger.warn({...server, errorCode: result.errorCode}, 'Voice participant sweep could not list rooms');
			return [];
		}
		const rooms: Array<SweepRoom> = [];
		for (const room of result.rooms) {
			const context = parseRoomName(room.name);
			if (room.numParticipants === 0 || room.name.startsWith(REXCTL_TEST_ROOM_PREFIX) || context === null) {
				continue;
			}
			rooms.push({...server, roomName: room.name, context});
		}
		return rooms;
	});
	return perServer.flat();
}

async function checkRoom(
	deps: VoiceParticipantSweepDeps,
	room: SweepRoom,
	previous: VoiceParticipantSightings,
	sightings: VoiceParticipantSightings,
	due: Array<DueParticipant>,
): Promise<boolean> {
	const listed = await deps.liveKitService.listParticipants({
		guildId: isDMRoom(room.context) ? undefined : room.context.guildId,
		channelId: room.context.channelId,
		regionId: room.regionId,
		serverId: room.serverId,
	});
	if (listed.status === 'error' || listed.participants.length === 0) {
		return listed.status === 'ok';
	}
	const known = await gatewayParticipantKeys(deps, room);
	if (known === null) {
		return false;
	}
	const now = deps.now();
	for (const participant of listed.participants) {
		const gatewayKey = participantGatewayKey(room, participant.identity, participant.metadata);
		if (gatewayKey !== null && known.has(gatewayKey)) {
			continue;
		}
		const key = `${room.regionId}|${room.serverId}|${room.roomName}|${participant.identity}`;
		const seen = previous[key];
		const sighting =
			seen !== undefined && seen.joinedAt === participant.joinedAt
				? seen
				: {joinedAt: participant.joinedAt, firstSeenAt: now};
		sightings[key] = sighting;
		if (now - sighting.firstSeenAt >= VOICE_PARTICIPANT_SETTLE_MS) {
			due.push({key, room, identity: participant.identity, gatewayKey, ...sighting});
		}
	}
	return true;
}

async function gatewayParticipantKeys(deps: VoiceParticipantSweepDeps, room: SweepRoom): Promise<Set<string> | null> {
	try {
		const {voiceStates} = await deps.gatewayService.getVoiceStatesForChannel({
			guildId: isDMRoom(room.context) ? undefined : room.context.guildId,
			channelId: room.context.channelId,
		});
		return new Set(voiceStates.map((voiceState) => gatewayKeyFor(room, voiceState.userId, voiceState.connectionId)));
	} catch (error) {
		if (error instanceof UnknownGuildError || error instanceof UnknownChannelError) {
			return new Set();
		}
		deps.logger.warn({error, roomName: room.roomName}, 'Voice participant sweep could not read gateway voice states');
		return null;
	}
}

function gatewayKeyFor(room: SweepRoom, userId: string | bigint, connectionId: string): string {
	return isDMRoom(room.context) ? `${userId}` : `${userId}|${connectionId}`;
}

function participantGatewayKey(room: SweepRoom, identity: string, metadata: string): string | null {
	const parsed = metadata ? parseParticipantMetadataWithRaw(metadata) : null;
	if (parsed) {
		return gatewayKeyFor(room, parsed.context.userId, parsed.context.connectionId);
	}
	const match = IDENTITY_PATTERN.exec(identity);
	return match ? gatewayKeyFor(room, match[1], match[2]) : null;
}

async function removeIfStillMissing(
	deps: VoiceParticipantSweepDeps,
	participant: DueParticipant,
): Promise<'removed' | 'present' | 'unknown'> {
	const {room} = participant;
	const known = await gatewayParticipantKeys(deps, room);
	if (known === null) {
		return 'unknown';
	}
	if (participant.gatewayKey !== null && known.has(participant.gatewayKey)) {
		return 'present';
	}
	const ref = {
		regionId: room.regionId,
		serverId: room.serverId,
		roomName: room.roomName,
		identity: participant.identity,
	};
	const current = await deps.liveKitService.getParticipant(ref);
	if (current.status === 'error') {
		return 'unknown';
	}
	if (current.participant?.joinedAt !== participant.joinedAt) {
		return 'present';
	}
	const logContext = {
		regionId: room.regionId,
		serverId: room.serverId,
		roomName: room.roomName,
		participantIdentity: participant.identity,
		joinedAt: participant.joinedAt,
		missingForMs: deps.now() - participant.firstSeenAt,
	};
	if (deps.dryRun) {
		deps.logger.info(logContext, 'Voice participant sweep would disconnect a participant with no voice state');
		return 'unknown';
	}
	await deps.liveKitService.removeParticipant(ref);
	voiceParticipantsDisconnectedTotal.inc('reason="sweep"');
	deps.logger.info(logContext, 'Voice participant sweep disconnected a participant with no voice state');
	return 'removed';
}
