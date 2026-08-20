// SPDX-License-Identifier: AGPL-3.0-or-later

import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {deferUntilModulesLoaded} from '@app/features/platform/utils/DeferUntilModulesLoaded';
import * as PiPCommands from '@app/features/ui/commands/PiPCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import PiP from '@app/features/ui/state/PiP';
import Users from '@app/features/user/state/Users';
import {getStreamKey} from '@app/features/voice/components/StreamKeys';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {selectVoiceMediaGraphViewerStreamKeys} from '@app/features/voice/engine/VoiceMediaGraph';
import {voiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import {
	createScreenSharePiPSnapshot,
	type ScreenSharePiPConditions,
	type ScreenSharePiPScreenShare,
	type ScreenSharePiPScreenShareSource,
	type ScreenSharePiPSnapshot,
	selectScreenSharePiPCommands,
	selectScreenSharePiPMode,
	transitionScreenSharePiPSnapshot,
} from '@app/features/voice/state/ScreenSharePiPStateMachine';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	buildVoiceParticipantIdentity,
	parseVoiceParticipantIdentity,
} from '@app/features/voice/utils/VoiceParticipantIdentity';
import {type Participant, type Room, RoomEvent, Track} from 'livekit-client';
import {reaction} from 'mobx';

const logger = new Logger('ScreenSharePiPController');

const ROOM_TRACK_EVENTS: ReadonlyArray<RoomEvent> = [
	RoomEvent.ParticipantConnected,
	RoomEvent.ParticipantDisconnected,
	RoomEvent.ConnectionStateChanged,
	RoomEvent.LocalTrackPublished,
	RoomEvent.LocalTrackUnpublished,
	RoomEvent.TrackPublished,
	RoomEvent.TrackUnpublished,
	RoomEvent.TrackSubscriptionStatusChanged,
	RoomEvent.TrackMuted,
	RoomEvent.TrackUnmuted,
	RoomEvent.TrackSubscribed,
	RoomEvent.TrackUnsubscribed,
];

function buildScreenShare(
	screenShare: Omit<ScreenSharePiPScreenShare, 'source'>,
	source: ScreenSharePiPScreenShareSource,
): ScreenSharePiPScreenShare {
	return {...screenShare, source};
}

function pickLiveKitScreenShare(room: Room, channelId: string): ScreenSharePiPScreenShare | null {
	const participants = [room.localParticipant, ...Array.from(room.remoteParticipants.values())];
	for (const participant of participants) {
		for (const publication of participant.trackPublications.values()) {
			if (publication.source !== Track.Source.ScreenShare) continue;
			if (publication.isMuted) continue;
			const parsed = parseVoiceParticipantIdentity(participant.identity);
			if (!parsed.userId || !parsed.connectionId) continue;
			return buildScreenShare(
				{
					participantIdentity: participant.identity,
					userId: parsed.userId,
					connectionId: parsed.connectionId,
				},
				'livekit',
			);
		}
	}
	void channelId;
	return null;
}

function pickRemoteScreenShareFromSnapshots(channelId: string): ScreenSharePiPScreenShare | null {
	const snapshots = MediaEngine.participants;
	const connectionVoiceStates = MediaEngine.connectionVoiceStates;
	for (const participantIdentity in snapshots) {
		const snapshot = snapshots[participantIdentity];
		if (!snapshot) continue;
		if (!snapshot.isScreenShareEnabled) continue;
		const parsed = parseVoiceParticipantIdentity(snapshot.identity);
		if (!parsed.userId || !parsed.connectionId) continue;
		const voiceState = connectionVoiceStates[parsed.connectionId];
		if (voiceState && voiceState.channel_id !== channelId) continue;
		return buildScreenShare(
			{
				participantIdentity: snapshot.identity,
				userId: parsed.userId,
				connectionId: parsed.connectionId,
			},
			'participant-snapshot',
		);
	}
	return null;
}

function pickLocalSelfShare(channelId: string): ScreenSharePiPScreenShare | null {
	if (!LocalVoiceState.getSelfStream()) return null;
	const localConnectionId = MediaEngine.connectionId;
	if (!localConnectionId) return null;
	if (MediaEngine.channelId !== channelId) return null;
	const userId = Users.currentUser?.id;
	if (!userId) return null;
	return buildScreenShare(
		{
			participantIdentity: buildVoiceParticipantIdentity(userId, localConnectionId),
			userId,
			connectionId: localConnectionId,
		},
		'local-self-state',
	);
}

function detectActiveScreenShare(channelId: string | null): ScreenSharePiPScreenShare | null {
	if (!channelId) return null;
	const local = pickLocalSelfShare(channelId);
	if (local) return local;
	const room = MediaEngine.room;
	if (room) {
		const fromRoom = pickLiveKitScreenShare(room, channelId);
		if (fromRoom) return fromRoom;
	}
	const fromSnapshots = pickRemoteScreenShareFromSnapshots(channelId);
	if (fromSnapshots) return fromSnapshots;
	return null;
}

function pickWatchedScreenShareFromOpenPiP(
	channelId: string | null,
	guildId: string | null,
): ScreenSharePiPScreenShare | null {
	if (!channelId) return null;
	const content = PiP.getContent();
	if (content?.type !== 'stream') return null;
	if (content.channelId !== channelId || content.guildId !== guildId) return null;
	const streamKey = getStreamKey(content.guildId, content.channelId, content.connectionId);
	if (!selectVoiceMediaGraphViewerStreamKeys(voiceMediaGraphStore.getGraphSnapshot()).includes(streamKey)) return null;
	return buildScreenShare(
		{
			participantIdentity: content.participantIdentity,
			userId: content.userId,
			connectionId: content.connectionId,
		},
		'watched-stream',
	);
}

function detectScreenSharePiPContent(
	channelId: string | null,
	guildId: string | null,
): ScreenSharePiPScreenShare | null {
	return detectActiveScreenShare(channelId) ?? pickWatchedScreenShareFromOpenPiP(channelId, guildId);
}

function getClosedReason(conditions: ScreenSharePiPConditions): string | null {
	if (!conditions.connectedChannelId) return 'not-connected-to-voice';
	if (!conditions.screenShare) return 'no-active-screen-share';
	if (conditions.isMobile) return 'mobile-layout';
	if (conditions.disabledBySetting) return 'disabled-by-setting';
	if (conditions.disabledBySession) return 'disabled-for-session';
	if (conditions.selectedChannelId === conditions.connectedChannelId) return 'selected-channel-is-connected-channel';
	return null;
}

interface ScreenSharePublicationDebugInfo {
	source: Track.Source;
	trackSid: string;
	isMuted: boolean;
	track?: unknown;
}

function summarizePublication(publication: ScreenSharePublicationDebugInfo): object {
	const record = publication as unknown as Record<string, unknown>;
	return {
		source: publication.source,
		trackSid: publication.trackSid,
		isMuted: publication.isMuted,
		isSubscribed: typeof record.isSubscribed === 'boolean' ? record.isSubscribed : null,
		hasTrack: Boolean(publication.track),
	};
}

function summarizeParticipantScreenSharePublications(participant: Participant): Array<object> {
	return Array.from(participant.trackPublications.values())
		.filter((publication) => publication.source === Track.Source.ScreenShare)
		.map((publication) => summarizePublication(publication));
}

function summarizeRoom(room: Room | null): object | null {
	if (!room) return null;
	const participants = [room.localParticipant, ...Array.from(room.remoteParticipants.values())];
	return {
		localParticipantIdentity: room.localParticipant.identity,
		remoteParticipantCount: room.remoteParticipants.size,
		screenShareParticipants: participants
			.map((participant) => ({
				identity: participant.identity,
				isLocal: participant === room.localParticipant,
				publications: summarizeParticipantScreenSharePublications(participant),
			}))
			.filter((participant) => participant.publications.length > 0),
	};
}

function summarizeParticipantSnapshots(channelId: string | null): Array<object> {
	const summaries: Array<object> = [];
	for (const participantIdentity in MediaEngine.participants) {
		const snapshot = MediaEngine.participants[participantIdentity];
		if (!snapshot?.isScreenShareEnabled) continue;
		const parsed = parseVoiceParticipantIdentity(snapshot.identity);
		const voiceState = parsed.connectionId ? MediaEngine.connectionVoiceStates[parsed.connectionId] : null;
		summaries.push({
			identity: snapshot.identity,
			isScreenShareEnabled: snapshot.isScreenShareEnabled,
			voiceStateChannelId: voiceState?.channel_id ?? null,
			matchesConnectedChannel: channelId != null && voiceState?.channel_id === channelId,
		});
	}
	return summaries;
}

function summarizeSelfStreamVoiceStates(channelId: string | null): Array<object> {
	const summaries: Array<object> = [];
	for (const connectionId in MediaEngine.connectionVoiceStates) {
		const voiceState = MediaEngine.connectionVoiceStates[connectionId];
		if (!voiceState?.self_stream) continue;
		summaries.push({
			userId: voiceState.user_id ?? null,
			connectionId: voiceState.connection_id ?? null,
			channelId: voiceState.channel_id ?? null,
			isLocalConnection: voiceState.connection_id === MediaEngine.connectionId,
			matchesConnectedChannel: channelId != null && voiceState.channel_id === channelId,
		});
	}
	return summaries;
}

class ScreenSharePiPController {
	private snapshot: ScreenSharePiPSnapshot = createScreenSharePiPSnapshot();
	private currentRoom: Room | null = null;
	private currentRoomHandlers: Array<{event: RoomEvent; handler: () => void}> = [];
	private lastDecisionLogKey: string | null = null;
	private started = false;

	start(): void {
		if (this.started) return;
		this.started = true;
		logger.debug('Starting screen-share PiP controller');
		deferUntilModulesLoaded(() => this.bootstrap());
	}

	private bootstrap(): void {
		logger.debug('Bootstrapping screen-share PiP controller');
		reaction(
			() => MediaEngine.room,
			(room) => this.handleRoomChange(room),
			{fireImmediately: true, name: 'ScreenSharePiP.roomBinding'},
		);
		reaction(
			() => this.readReactiveInputs(),
			() => this.recompute('reactive-inputs'),
			{
				equals: shallowReactiveInputsEqual,
				fireImmediately: true,
				name: 'ScreenSharePiP.recompute',
			},
		);
		MediaEngine.subscribe(() => this.recompute('media-engine-store'));
		LocalVoiceState.subscribe(() => this.recompute('local-voice-state'));
	}

	private readReactiveInputs(): ReactiveInputs {
		return {
			connectedChannelId: MediaEngine.channelId,
			connectedGuildId: MediaEngine.guildId,
			selectedChannelId: SelectedChannel.currentChannelId,
			isMobile: MobileLayout.isMobileLayout(),
			disabledBySetting: VoiceSettings.disablePictureInPicturePopoutScreenShare,
			disabledBySession: PiP.getSessionDisable(),
		};
	}

	private handleRoomChange(room: Room | null): void {
		this.detachRoomListener();
		this.currentRoom = room;
		if (room) {
			for (const event of ROOM_TRACK_EVENTS) {
				const handler = () => this.recompute(`room.${event}`);
				room.on(event, handler);
				this.currentRoomHandlers.push({event, handler});
			}
		}
		this.recompute('room-changed');
	}

	private detachRoomListener(): void {
		if (!this.currentRoom) return;
		for (const {event, handler} of this.currentRoomHandlers) {
			this.currentRoom.off(event, handler);
		}
		this.currentRoomHandlers = [];
	}

	private recompute(trigger: string): void {
		const conditions = this.deriveConditions();
		const previousMode = selectScreenSharePiPMode(this.snapshot);
		const next = transitionScreenSharePiPSnapshot(this.snapshot, {type: 'conditions.changed', conditions});
		this.snapshot = next;
		const nextMode = selectScreenSharePiPMode(next);
		const commands = selectScreenSharePiPCommands(next);
		this.logDecision(
			trigger,
			conditions,
			previousMode.kind,
			nextMode.kind,
			commands.map((command) => command.type),
		);
		if (commands.length === 0) return;
		for (const command of commands) {
			switch (command.type) {
				case 'open':
					logger.debug('Opening screen-share PiP', {content: command.content});
					PiPCommands.openPiP(command.content);
					break;
				case 'close':
					logger.debug('Closing screen-share PiP');
					PiPCommands.closePiP();
					break;
			}
		}
	}

	private logDecision(
		trigger: string,
		conditions: ScreenSharePiPConditions,
		previousMode: string,
		nextMode: string,
		commandTypes: Array<string>,
	): void {
		const closedReason = nextMode === 'closed' ? getClosedReason(conditions) : null;
		const debugState = {
			previousMode,
			nextMode,
			closedReason,
			connectedChannelId: conditions.connectedChannelId,
			selectedChannelId: conditions.selectedChannelId,
			connectedGuildId: conditions.connectedGuildId,
			screenShare: conditions.screenShare,
			isMobile: conditions.isMobile,
			disabledBySetting: conditions.disabledBySetting,
			disabledBySession: conditions.disabledBySession,
			localSelfStream: LocalVoiceState.getSelfStream(),
			mediaEngineConnectionId: MediaEngine.connectionId,
			mediaEngineChannelId: MediaEngine.channelId,
			participantSnapshotCount: Object.keys(MediaEngine.participants).length,
			voiceStateCount: Object.keys(MediaEngine.connectionVoiceStates).length,
			screenShareSnapshots: summarizeParticipantSnapshots(conditions.connectedChannelId),
			selfStreamVoiceStates: summarizeSelfStreamVoiceStates(conditions.connectedChannelId),
			room: summarizeRoom(MediaEngine.room),
		};
		const key = JSON.stringify(debugState);
		if (commandTypes.length === 0 && key === this.lastDecisionLogKey) return;
		this.lastDecisionLogKey = key;
		logger.debug('Screen-share PiP decision', {
			trigger,
			commandTypes,
			...debugState,
		});
	}

	private deriveConditions(): ScreenSharePiPConditions {
		const connectedChannelId = MediaEngine.channelId;
		const connectedGuildId = MediaEngine.guildId ?? null;
		const screenShare = detectScreenSharePiPContent(connectedChannelId, connectedGuildId);
		return {
			connectedChannelId,
			connectedGuildId,
			screenShare,
			selectedChannelId: SelectedChannel.currentChannelId,
			isMobile: MobileLayout.isMobileLayout(),
			disabledBySetting: VoiceSettings.disablePictureInPicturePopoutScreenShare,
			disabledBySession: PiP.getSessionDisable(),
		};
	}
}

interface ReactiveInputs {
	connectedChannelId: string | null;
	connectedGuildId: string | null;
	selectedChannelId: string | null;
	isMobile: boolean;
	disabledBySetting: boolean;
	disabledBySession: boolean;
}

function shallowReactiveInputsEqual(a: ReactiveInputs, b: ReactiveInputs): boolean {
	return (
		a.connectedChannelId === b.connectedChannelId &&
		a.connectedGuildId === b.connectedGuildId &&
		a.selectedChannelId === b.selectedChannelId &&
		a.isMobile === b.isMobile &&
		a.disabledBySetting === b.disabledBySetting &&
		a.disabledBySession === b.disabledBySession
	);
}

const instance = new ScreenSharePiPController();

export function startScreenSharePiPController(): void {
	instance.start();
}

export default instance;
