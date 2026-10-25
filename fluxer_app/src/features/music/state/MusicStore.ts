// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {musicErrorText} from '@app/features/music/MusicErrors';
import {http} from '@app/features/platform/transport/RestTransport';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import Users from '@app/features/user/state/Users';
import {Store} from '@app/features/voice/engine/Store';
import {getEffectiveAudioState} from '@app/features/voice/engine/VoiceEffectiveAudioState';
import {isTrustedMusicSender} from '@fluxer/music_protocol/src/Identity';
import {
	decodeRpcResult,
	type RpcMethod,
	type RpcPayloads,
	type RpcResponses,
	type RpcResult,
} from '@fluxer/music_protocol/src/Rpc';
import {decodeState, livePositionMs} from '@fluxer/music_protocol/src/State';
import {
	type LoopMode,
	MUSIC_STATE_TOPIC,
	type MusicErrorCode,
	type MusicState,
	type SearchResult,
	type Track,
} from '@fluxer/music_protocol/src/Types';
import {i18n} from '@lingui/core';
import type {Room} from 'livekit-client';
import {type Participant, RoomEvent, RpcError} from 'livekit-client';

const SLOW_RPC_TIMEOUT_MS = 30000;
const DEFAULT_RPC_TIMEOUT_MS = 10000;
const QUEUE_PAGE_SIZE = 20;

function isTimeoutError(error: unknown): boolean {
	if (!(error instanceof RpcError)) return false;
	return (
		error.code === RpcError.ErrorCode.RESPONSE_TIMEOUT ||
		error.code === RpcError.ErrorCode.CONNECTION_TIMEOUT ||
		/timeout/i.test(error.message)
	);
}

class MusicStore extends Store {
	state: MusicState | null = null;
	botIdentity: string | null = null;
	channelId: string | null = null;
	panelOpen = false;
	searchResults: Array<SearchResult> = [];
	searching = false;
	private room: Room | null = null;
	private bindDisposer: (() => void) | null = null;

	bind(room: Room, opts: {guildId: string | null; channelId: string}): () => void {
		this.dispose();
		this.room = room;
		this.update(() => {
			this.channelId = opts.channelId;
		});
		const onDataReceived = (
			payload: Uint8Array,
			participant: Participant | undefined,
			_kind: unknown,
			topic?: string,
		): void => {
			if (topic !== MUSIC_STATE_TOPIC || !participant) return;
			const identity = participant.identity;
			if (this.botIdentity !== null && this.botIdentity !== identity) return;
			if (!isTrustedMusicSender(identity, (id) => Users.getUser(id)?.bot === true)) return;
			const state = decodeState(payload);
			if (!state) return;
			this.update(() => {
				this.state = state;
				this.botIdentity = identity;
			});
		};
		const onParticipantDisconnected = (participant: Participant): void => {
			if (participant.identity !== this.botIdentity) return;
			this.update(() => {
				this.state = null;
				this.botIdentity = null;
			});
		};
		room.on(RoomEvent.DataReceived, onDataReceived);
		room.on(RoomEvent.ParticipantDisconnected, onParticipantDisconnected);
		this.bindDisposer = () => {
			room.off(RoomEvent.DataReceived, onDataReceived);
			room.off(RoomEvent.ParticipantDisconnected, onParticipantDisconnected);
		};
		return () => {
			if (this.room === room) this.dispose();
		};
	}

	dispose(): void {
		this.bindDisposer?.();
		this.bindDisposer = null;
		this.room = null;
		this.update(() => {
			this.state = null;
			this.botIdentity = null;
			this.channelId = null;
			this.searchResults = [];
			this.searching = false;
		});
	}

	get isDeafened(): boolean {
		return getEffectiveAudioState().effectiveDeaf;
	}

	livePositionMs(now: number): number {
		return this.state ? livePositionMs(this.state, now) : 0;
	}

	togglePanel(): void {
		this.update(() => {
			this.panelOpen = !this.panelOpen;
		});
	}

	async search(q: string): Promise<void> {
		this.update(() => {
			this.searching = true;
		});
		try {
			const result = await this.rpc('music.search', {q});
			this.update(() => {
				this.searchResults = result?.tracks ?? [];
			});
		} finally {
			this.update(() => {
				this.searching = false;
			});
		}
	}

	async add(input: string, next?: boolean): Promise<void> {
		if (this.botIdentity === null) {
			await this.startWithChat(input);
			return;
		}
		await this.rpc('music.add', next === undefined ? {input} : {input, next});
	}

	async startWithChat(input: string): Promise<void> {
		const channelId = this.channelId;
		if (channelId === null) return;
		try {
			await http.post(Endpoints.CHANNEL_MESSAGES(channelId), {body: {content: `!play ${input}`}});
		} catch {
			ToastCommands.error(i18n._(musicErrorText('SOURCE_FAILED')));
		}
	}

	async queuePage(offset: number): Promise<{tracks: Array<Track>; total: number}> {
		const result = await this.rpc('music.queue', {offset, limit: QUEUE_PAGE_SIZE});
		return result ?? {tracks: [], total: 0};
	}

	async pause(): Promise<void> {
		await this.rpc('music.pause', {});
	}

	async resume(): Promise<void> {
		await this.rpc('music.resume', {});
	}

	async skip(): Promise<void> {
		await this.rpc('music.skip', {});
	}

	async seek(ms: number): Promise<void> {
		await this.rpc('music.seek', {ms});
	}

	async remove(id: string): Promise<void> {
		await this.rpc('music.remove', {id});
	}

	async move(id: string, to: number): Promise<void> {
		await this.rpc('music.move', {id, to});
	}

	async clear(): Promise<void> {
		await this.rpc('music.clear', {});
	}

	async shuffle(): Promise<void> {
		await this.rpc('music.shuffle', {});
	}

	async setLoop(mode: LoopMode): Promise<void> {
		await this.rpc('music.loop', {mode});
	}

	async stop(): Promise<void> {
		await this.rpc('music.stop', {});
	}

	private async rpc<M extends RpcMethod>(method: M, payload: RpcPayloads[M]): Promise<RpcResponses[M] | null> {
		const room = this.room;
		const destinationIdentity = this.botIdentity;
		if (!room || destinationIdentity === null) return null;
		let result: RpcResult<RpcResponses[M]>;
		try {
			const response = await room.localParticipant.performRpc({
				destinationIdentity,
				method,
				payload: JSON.stringify(payload),
				responseTimeout:
					method === 'music.search' || method === 'music.add' ? SLOW_RPC_TIMEOUT_MS : DEFAULT_RPC_TIMEOUT_MS,
			});
			result = decodeRpcResult<RpcResponses[M]>(response);
		} catch (error) {
			const code: MusicErrorCode | 'TIMEOUT' = isTimeoutError(error) ? 'TIMEOUT' : 'SOURCE_FAILED';
			ToastCommands.error(i18n._(musicErrorText(code)));
			return null;
		}
		if (!result.ok) {
			ToastCommands.error(i18n._(musicErrorText(result.error)));
			return null;
		}
		return result.value;
	}
}

const instance = new MusicStore();

export default instance;
