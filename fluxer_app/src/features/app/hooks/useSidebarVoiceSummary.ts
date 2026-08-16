// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceBadgeActivity} from '@app/features/app/components/layout/sidebar_nav/VoiceBadge';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import {
	createVoiceParticipantSortSnapshot,
	sortVoiceParticipantItemsWithSnapshot,
} from '@app/features/voice/components/VoiceParticipantSortUtils';
import type {NormalizedVoiceState} from '@app/features/voice/engine/VoiceGatewayStateMachine';
import {useRef} from 'react';

export const SidebarVoiceSummaryScope = Object.freeze({
	GUILD: 'guild',
	CHANNEL: 'channel',
} as const);

export type SidebarVoiceSummaryScope = (typeof SidebarVoiceSummaryScope)[keyof typeof SidebarVoiceSummaryScope];

export type SidebarVoiceStatesByChannel = Readonly<Record<string, Readonly<Record<string, NormalizedVoiceState>>>>;
export type SidebarVoiceStatesByConnection = Readonly<Record<string, NormalizedVoiceState>>;

interface GuildSidebarVoiceSummaryInput {
	readonly scope: typeof SidebarVoiceSummaryScope.GUILD;
	readonly voiceStates: SidebarVoiceStatesByChannel | undefined;
	readonly guildId: string;
}

interface ChannelSidebarVoiceSummaryInput {
	readonly scope: typeof SidebarVoiceSummaryScope.CHANNEL;
	readonly voiceStates: SidebarVoiceStatesByConnection | undefined;
	readonly channelId: string;
}

type SidebarVoiceSummaryInput = GuildSidebarVoiceSummaryInput | ChannelSidebarVoiceSummaryInput;

export interface SidebarVoiceRow {
	readonly key: 'voice' | 'screenshare';
	readonly users: ReadonlyArray<User>;
}

export interface SidebarVoiceSummary {
	readonly voiceRows: ReadonlyArray<SidebarVoiceRow>;
	readonly hasVoiceActivity: boolean;
	readonly badgeActivity: VoiceBadgeActivity | null;
}

const EMPTY_SIDEBAR_VOICE_ROWS: ReadonlyArray<SidebarVoiceRow> = Object.freeze([]);
const EMPTY_SIDEBAR_VOICE_SUMMARY: SidebarVoiceSummary = Object.freeze({
	voiceRows: EMPTY_SIDEBAR_VOICE_ROWS,
	hasVoiceActivity: false,
	badgeActivity: null,
});

function getUserId(user: User): string {
	return user.id;
}

function hasAnySidebarVoiceState(input: SidebarVoiceSummaryInput): boolean {
	const voiceStates = input.voiceStates;
	if (voiceStates == null) return false;
	for (const key in voiceStates) {
		if (voiceStates[key] != null) return true;
	}
	return false;
}

function resolveVoiceBadgeActivity(
	hasVoiceActivity: boolean,
	hasScreenshare: boolean,
	hasVideo: boolean,
): VoiceBadgeActivity | null {
	if (!hasVoiceActivity) {
		return null;
	}
	if (hasScreenshare) {
		return 'screenshare';
	}
	if (hasVideo) {
		return 'video';
	}
	return 'voice';
}

class SidebarVoiceParticipantCollector {
	private readonly seenUserIds = new Set<string>();
	private readonly collectedVoiceUsers: Array<User> = [];
	private readonly collectedStreamingUsers: Array<User> = [];
	private screensharePresent = false;
	private videoPresent = false;

	get voiceUsers(): ReadonlyArray<User> {
		return this.collectedVoiceUsers;
	}

	get streamingUsers(): ReadonlyArray<User> {
		return this.collectedStreamingUsers;
	}

	get hasScreenshare(): boolean {
		return this.screensharePresent;
	}

	get hasVideo(): boolean {
		return this.videoPresent;
	}

	collectByConnection(voiceStates: SidebarVoiceStatesByConnection): void {
		for (const connectionId in voiceStates) {
			const voiceState = voiceStates[connectionId];
			if (!voiceState) continue;
			this.collectVoiceState(voiceState);
		}
	}

	collectByChannel(voiceStates: SidebarVoiceStatesByChannel): void {
		for (const channelId in voiceStates) {
			const channelStates = voiceStates[channelId];
			if (!channelStates) continue;
			this.collectByConnection(channelStates);
		}
	}

	private collectVoiceState(voiceState: NormalizedVoiceState): void {
		const isScreensharing = voiceState.self_stream === true;
		const isVideo = voiceState.self_video === true;
		if (isScreensharing) {
			this.screensharePresent = true;
		}
		if (isVideo) {
			this.videoPresent = true;
		}
		if (this.seenUserIds.has(voiceState.user_id)) {
			return;
		}
		const user = Users.getUser(voiceState.user_id);
		if (!user) {
			return;
		}
		if (isScreensharing) {
			this.collectedStreamingUsers.push(user);
		} else {
			this.collectedVoiceUsers.push(user);
		}
		this.seenUserIds.add(user.id);
	}
}

export function useSidebarVoiceSummary(input: SidebarVoiceSummaryInput): SidebarVoiceSummary {
	let guildId: string | null = null;
	let channelId: string | null = null;
	if (input.scope === SidebarVoiceSummaryScope.GUILD) {
		guildId = input.guildId;
	} else {
		channelId = input.channelId;
	}
	const voiceUserSortSnapshotRef = useRef(createVoiceParticipantSortSnapshot());
	const streamingUserSortSnapshotRef = useRef(createVoiceParticipantSortSnapshot());
	if (!hasAnySidebarVoiceState(input)) {
		voiceUserSortSnapshotRef.current.entries.clear();
		streamingUserSortSnapshotRef.current.entries.clear();
		return EMPTY_SIDEBAR_VOICE_SUMMARY;
	}
	const participantCollector = new SidebarVoiceParticipantCollector();
	if (input.voiceStates != null) {
		if (input.scope === SidebarVoiceSummaryScope.GUILD) {
			participantCollector.collectByChannel(input.voiceStates);
		} else {
			participantCollector.collectByConnection(input.voiceStates);
		}
	}
	const sortedVoiceUsers = sortVoiceParticipantItemsWithSnapshot(participantCollector.voiceUsers, {
		snapshot: voiceUserSortSnapshotRef.current,
		getParticipantKey: getUserId,
		getUserId,
		guildId,
		channelId,
		getTieBreaker: undefined,
	});
	const sortedStreamingUsers = sortVoiceParticipantItemsWithSnapshot(participantCollector.streamingUsers, {
		snapshot: streamingUserSortSnapshotRef.current,
		getParticipantKey: getUserId,
		getUserId,
		guildId,
		channelId,
		getTieBreaker: undefined,
	});
	const voiceRows: Array<SidebarVoiceRow> = [];
	if (sortedVoiceUsers.length > 0) {
		voiceRows.push({key: 'voice', users: sortedVoiceUsers});
	}
	if (sortedStreamingUsers.length > 0) {
		voiceRows.push({key: 'screenshare', users: sortedStreamingUsers});
	}
	const hasVoiceActivity = voiceRows.length > 0;
	const badgeActivity = resolveVoiceBadgeActivity(
		hasVoiceActivity,
		participantCollector.hasScreenshare,
		participantCollector.hasVideo,
	);
	return {voiceRows, hasVoiceActivity, badgeActivity};
}
