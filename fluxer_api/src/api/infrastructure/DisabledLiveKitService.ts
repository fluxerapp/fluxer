// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	GetParticipantResult,
	ILiveKitService,
	ListParticipantsResult,
	ListRoomsResult,
} from '@app/api/infrastructure/ILiveKitService';
import type {VoiceRegionMetadata, VoiceServerRecord} from '@app/api/voice/VoiceModel';

export class DisabledLiveKitService implements ILiveKitService {
	async createToken(): Promise<{
		token: string;
		endpoint: string;
	}> {
		throw new Error('Voice is disabled');
	}

	async updateParticipant(): Promise<void> {}

	async muteMicrophoneTrack(): Promise<void> {}

	async disconnectParticipant(): Promise<void> {}

	async removeParticipant(): Promise<void> {}

	async getParticipant(): Promise<GetParticipantResult> {
		return {status: 'ok', participant: null};
	}

	async setParticipantPermission(): Promise<void> {}

	async listRooms(): Promise<ListRoomsResult> {
		return {status: 'ok', rooms: []};
	}

	async listParticipants(): Promise<ListParticipantsResult> {
		return {status: 'ok', participants: []};
	}

	listServers(): Array<{regionId: string; serverId: string}> {
		return [];
	}

	getDefaultRegionId(): string | null {
		return null;
	}

	getRegionMetadata(): Array<VoiceRegionMetadata> {
		return [];
	}

	getServer(): VoiceServerRecord | null {
		return null;
	}
}
