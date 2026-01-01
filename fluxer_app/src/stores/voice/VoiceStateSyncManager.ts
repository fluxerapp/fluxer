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

import {Logger} from '~/lib/Logger';
import {syncVoiceStateToServer} from './VoiceChannelConnector';

export interface VoiceStateSyncPayload {
	guild_id: string | null;
	channel_id: string;
	connection_id: string;
	self_mute: boolean;
	self_deaf: boolean;
	self_video: boolean;
	self_stream: boolean;
	viewer_stream_key: string | null;
}

const logger = new Logger('VoiceStateSyncManager');

export class VoiceStateSyncManager {
	private pending: VoiceStateSyncPayload | null = null;
	private lastSent: VoiceStateSyncPayload | null = null;
	private serverState: VoiceStateSyncPayload | null = null;

	reset(): void {
		this.pending = null;
		this.lastSent = null;
		this.serverState = null;
		logger.debug('[reset] Voice state sync cache cleared');
	}

	requestState(payload: VoiceStateSyncPayload): void {
		this.pending = payload;
		this.flush();
	}

	confirmServerState(payload: VoiceStateSyncPayload | null): void {
		if (!payload) {
			this.serverState = null;
			return;
		}

		this.serverState = payload;
		if (this.pending && this.areEqual(this.pending, payload)) {
			this.pending = null;
		}
	}

	private flush(): void {
		if (!this.pending) return;

		if (this.lastSent && this.areEqual(this.lastSent, this.pending)) {
			this.pending = null;
			return;
		}

		if (this.serverState && this.areEqual(this.serverState, this.pending)) {
			this.pending = null;
			this.lastSent = this.serverState;
			return;
		}

		logger.debug('[flush] Sending voice state update to server', this.pending);
		syncVoiceStateToServer(this.pending.guild_id, this.pending.channel_id, this.pending.connection_id, {
			self_mute: this.pending.self_mute,
			self_deaf: this.pending.self_deaf,
			self_video: this.pending.self_video,
			self_stream: this.pending.self_stream,
			viewer_stream_key: this.pending.viewer_stream_key,
		});

		this.lastSent = this.pending;
		this.pending = null;
	}

	private areEqual(a: VoiceStateSyncPayload, b: VoiceStateSyncPayload): boolean {
		return (
			a.guild_id === b.guild_id &&
			a.channel_id === b.channel_id &&
			a.connection_id === b.connection_id &&
			a.self_mute === b.self_mute &&
			a.self_deaf === b.self_deaf &&
			a.self_video === b.self_video &&
			a.self_stream === b.self_stream &&
			a.viewer_stream_key === b.viewer_stream_key
		);
	}
}
