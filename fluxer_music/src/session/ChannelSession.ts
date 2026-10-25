// SPDX-License-Identifier: AGPL-3.0-or-later

import {EventEmitter} from 'node:events';
import type {ActionKind, LastAction, LoopMode, MusicState, SearchResult, Track} from '@fluxer/music_protocol/src/Types';
import type {AudioSink, Player} from '../audio/Player';
import type {RoomLink, RpcHandlers} from '../audio/RoomLink';
import {MusicError} from '../Errors';
import type {VoiceGrant} from '../fluxer/GatewayClient';
import type {Resolver} from '../resolve/Resolver';
import type {ResolvedTrack} from '../Types';
import {Queue} from './Queue';

export interface SessionDeps {
	resolver: Pick<Resolver, 'resolve' | 'search' | 'matchSpotify' | 'streamUrl'>;
	createPlayer(
		sink: AudioSink,
	): Pick<Player, 'play' | 'pause' | 'resume' | 'seek' | 'stop' | 'positionMs' | 'state'> & EventEmitter;
	room: Pick<RoomLink, 'connect' | 'reconnect' | 'broadcast' | 'disconnect' | 'sink'> & EventEmitter;
	voice: {
		join(): Promise<VoiceGrant>;
		refresh(connectionId: string): Promise<VoiceGrant>;
		leave(connectionId: string): void;
	};
	card: {
		start(state: MusicState): Promise<void>;
		update(state: MusicState): void;
		repost(state: MusicState): Promise<void>;
		notice(text: string): Promise<void>;
		finish(): Promise<void>;
		readonly messageId: string | null;
	};
	now(): number;
}

export type EndReason = 'stop' | 'idle' | 'empty' | 'dropped' | 'join-failed' | 'room-lost';

export interface SessionOptions {
	idleMs?: number;
	prefetchMs?: number;
	maxFailures?: number;
	heartbeatMs?: number;
}

const MAX_STATE_UPCOMING = 25;
const LOOKAHEAD = 3;
const REJOIN_MAX_ATTEMPTS = 5;
const REJOIN_WINDOW_MS = 120_000;
const REJOIN_DELAY_MIN_MS = 1_000;
const REJOIN_DELAY_MAX_MS = 15_000;
const BLOCKED_NOTICE = 'YouTube may be blocking us — see FRIENDS.md.';
const ROOM_LOST_NOTICE = 'Lost the voice connection.';

type Playback = 'loading' | 'playing' | 'paused';

function describeError(err: unknown): string {
	return err instanceof Error ? err.message : String(err);
}

/** One voice channel's queue, player, LiveKit room and live card. Emits 'ended' (reason: EndReason). */
export class ChannelSession extends EventEmitter {
	private readonly queue = new Queue();
	private readonly player: ReturnType<SessionDeps['createPlayer']>;
	private readonly idleMs: number;
	private readonly prefetchMs: number;
	private readonly maxFailures: number;
	private readonly heartbeatMs: number;

	private conn: string | null = null;
	private playback: Playback = 'loading';
	private lastAction: LastAction | null = null;
	/** Bumped whenever the track being loaded or played changes; async steps compare against it. */
	private gen = 0;
	/** The generation `player.play` was last called for; player events from other generations are ignored. */
	private playerGen = -1;
	private failures = 0;
	private matches = new Map<string, Promise<ResolvedTrack | null>>();
	private prefetched: {id: string; url: string} | null = null;

	private idleTimer: ReturnType<typeof setTimeout> | null = null;
	private heartbeatTimer: ReturnType<typeof setInterval> | null = null;
	private prefetchTimer: ReturnType<typeof setTimeout> | null = null;
	private rejoinWait: {timer: ReturnType<typeof setTimeout>; resolve: () => void} | null = null;

	private voiceChain: Promise<void> = Promise.resolve();
	private roomChain: Promise<void> = Promise.resolve();
	private rejoining = false;
	private disconnectedAgain = false;
	private pausedForRejoin = false;
	private rejoinAttempts: Array<number> = [];
	private isEnded = false;
	private endPromise: Promise<void> | null = null;

	constructor(
		readonly guildId: string,
		readonly channelId: string,
		private readonly deps: SessionDeps,
		opts: SessionOptions = {},
	) {
		super();
		this.idleMs = opts.idleMs ?? 300_000;
		this.prefetchMs = opts.prefetchMs ?? 15_000;
		this.maxFailures = opts.maxFailures ?? 3;
		this.heartbeatMs = opts.heartbeatMs ?? 5_000;

		this.player = deps.createPlayer(deps.room.sink);
		this.player.on('started', () => this.onStarted());
		this.player.on('ended', () => this.onEnded());
		this.player.on('error', (err: unknown) => this.onPlayerError(err));
		deps.room.on('disconnected', () => this.onRoomDisconnected());
	}

	get connectionId(): string | null {
		return this.conn;
	}

	get cardMessageId(): string | null {
		return this.deps.card.messageId;
	}

	get ended(): boolean {
		return this.isEnded;
	}

	async start(): Promise<void> {
		this.ensureActive();
		try {
			const grant = await this.voiceOp(() => this.deps.voice.join());
			this.conn = grant.connectionId;
			if (this.ended) {
				// end() ran before the grant arrived, so it could not leave this connection.
				this.deps.voice.leave(grant.connectionId);
				throw new MusicError('NOT_FOUND');
			}
			await this.roomOp(() => this.deps.room.connect(grant, this.handlers()));
			this.ensureActive();
			this.heartbeatTimer = setInterval(() => this.broadcast(), this.heartbeatMs);
			await this.deps.card.start(this.state());
			this.ensureActive();
			if (!this.queue.current) this.startIdleTimer();
		} catch (err) {
			await this.end('join-failed');
			throw err;
		}
	}

	repostCard(): Promise<void> {
		this.ensureActive();
		return this.deps.card.repost(this.state());
	}

	async add(by: string, input: string, next = false): Promise<number> {
		this.ensureActive();
		const tracks = await this.deps.resolver.resolve(input);
		this.ensureActive();
		const added = this.queue.add(tracks, by, {next});
		this.clearIdleTimer();
		this.record('add', by, tracks[0]?.title ?? null);
		if (!this.queue.current) {
			this.advanceAndPlay('skip');
		} else {
			this.changed();
		}
		return added;
	}

	async search(query: string): Promise<Array<SearchResult>> {
		this.ensureActive();
		return this.deps.resolver.search(query);
	}

	queuePage(offset: number, limit: number): {tracks: Array<Track>; total: number} {
		this.ensureActive();
		return this.queue.page(offset, limit);
	}

	pause(by: string): void {
		this.ensureActive();
		const current = this.queue.current;
		if (!current || this.playback !== 'playing') return;
		this.pausedForRejoin = false;
		this.player.pause();
		this.playback = 'paused';
		this.record('pause', by, current.title);
		this.changed();
	}

	resume(by: string): void {
		this.ensureActive();
		const current = this.queue.current;
		if (!current || this.playback !== 'paused') return;
		this.record('resume', by, current.title);
		if (this.playerGen === this.gen) {
			this.player.resume();
			this.playback = 'playing';
			this.changed();
		} else {
			// Paused after repeated failures: the current track was never loaded.
			this.failures = 0;
			void this.startTrack(current);
		}
	}

	skip(by: string): void {
		this.ensureActive();
		this.record('skip', by, this.queue.current?.title ?? null);
		this.advanceAndPlay('skip');
	}

	seek(by: string, ms: number): void {
		this.ensureActive();
		const current = this.queue.current;
		if (!current || ms > current.durationMs || this.playerGen !== this.gen) throw new MusicError('BAD_REQUEST');
		this.player.seek(ms);
		this.record('seek', by, current.title);
		this.schedulePrefetch();
		this.changed();
	}

	remove(by: string, id: string): void {
		this.ensureActive();
		const track = this.queue.upcoming.find((t) => t.id === id);
		if (!track || !this.queue.remove(id)) throw new MusicError('NOT_FOUND');
		this.record('remove', by, track.title);
		this.changed();
	}

	move(by: string, id: string, to: number): void {
		this.ensureActive();
		const track = this.queue.upcoming.find((t) => t.id === id);
		if (!track || !this.queue.move(id, to)) throw new MusicError('NOT_FOUND');
		this.record('move', by, track.title);
		this.changed();
	}

	clear(by: string): void {
		this.ensureActive();
		this.queue.clear();
		this.record('clear', by, null);
		this.changed();
	}

	shuffle(by: string): void {
		this.ensureActive();
		this.queue.shuffle();
		this.record('shuffle', by, null);
		this.changed();
	}

	setLoop(by: string, mode: LoopMode): void {
		this.ensureActive();
		this.queue.loop = mode;
		this.record('loop', by, null);
		this.changed();
	}

	async stop(by: string): Promise<void> {
		this.ensureActive();
		this.record('stop', by, null);
		this.changed();
		await this.end('stop');
	}

	state(): MusicState {
		const current = this.queue.current;
		const upcoming = this.queue.upcoming;
		const loaded = current !== null && this.playback !== 'loading' && this.playerGen === this.gen;
		return {
			v: 1,
			status: current ? this.playback : 'idle',
			current: current ? {...current} : null,
			positionMs: loaded ? this.player.positionMs : 0,
			positionAt: this.deps.now(),
			loop: this.queue.loop,
			upcoming: upcoming.slice(0, MAX_STATE_UPCOMING).map((t) => ({...t})),
			queueLength: upcoming.length,
			lastAction: this.lastAction ? {...this.lastAction} : null,
		};
	}

	end(reason: EndReason): Promise<void> {
		if (!this.endPromise) {
			this.isEnded = true;
			this.endPromise = this.teardown(reason);
		}
		return this.endPromise;
	}

	handlers(): RpcHandlers {
		return {
			'music.search': async (_by, {q}) => ({tracks: await this.search(q)}),
			'music.add': async (by, {input, next}) => ({added: await this.add(by, input, next)}),
			'music.queue': async (_by, {offset, limit}) => this.queuePage(offset, limit),
			'music.pause': async (by) => {
				this.pause(by);
				return {};
			},
			'music.resume': async (by) => {
				this.resume(by);
				return {};
			},
			'music.skip': async (by) => {
				this.skip(by);
				return {};
			},
			'music.seek': async (by, {ms}) => {
				this.seek(by, ms);
				return {};
			},
			'music.remove': async (by, {id}) => {
				this.remove(by, id);
				return {};
			},
			'music.move': async (by, {id, to}) => {
				this.move(by, id, to);
				return {};
			},
			'music.clear': async (by) => {
				this.clear(by);
				return {};
			},
			'music.shuffle': async (by) => {
				this.shuffle(by);
				return {};
			},
			'music.loop': async (by, {mode}) => {
				this.setLoop(by, mode);
				return {};
			},
			'music.stop': async (by) => {
				await this.stop(by);
				return {};
			},
		};
	}

	// Playback

	/** Moves the queue on ('advance' honours loop track, 'skip' doesn't) and loads the new current track. */
	private advanceAndPlay(how: 'advance' | 'skip'): void {
		if (how === 'advance') this.queue.advance();
		else this.queue.skip();
		this.playCurrent();
	}

	/** Drops the current track without re-queueing it, whatever the loop mode, and loads the next one. */
	private dropCurrent(): void {
		const loop = this.queue.loop;
		this.queue.loop = 'off';
		this.queue.skip();
		this.queue.loop = loop;
	}

	private playCurrent(): void {
		const current = this.queue.current;
		if (current) {
			void this.startTrack(current);
			return;
		}
		this.gen++;
		this.player.stop();
		this.clearPrefetchTimer();
		this.startIdleTimer();
		this.changed();
	}

	private async startTrack(track: Track): Promise<void> {
		const gen = ++this.gen;
		this.player.stop();
		this.clearPrefetchTimer();
		this.playback = 'loading';
		this.changed();
		try {
			if (track.source === 'spotify') {
				const match = await this.matchFor(track);
				if (gen !== this.gen) return;
				if (!match) {
					this.record('unmatched', null, track.title);
					this.dropCurrent();
					this.playCurrent();
					return;
				}
			}
			const url = this.takePrefetched(track) ?? (await this.deps.resolver.streamUrl(track));
			if (gen !== this.gen) return;
			this.playerGen = gen;
			this.player.play(url);
		} catch (err) {
			this.onTrackFailed(gen, track, err);
		}
	}

	private onStarted(): void {
		if (this.ended || this.playerGen !== this.gen) return;
		this.playback = 'playing';
		this.failures = 0;
		this.schedulePrefetch();
		this.changed();
	}

	private onEnded(): void {
		if (this.ended || this.playerGen !== this.gen) return;
		this.advanceAndPlay('advance');
	}

	private onPlayerError(err: unknown): void {
		if (this.ended || this.playerGen !== this.gen) return;
		const current = this.queue.current;
		if (current) this.onTrackFailed(this.gen, current, err);
	}

	private onTrackFailed(gen: number, track: Track, err: unknown): void {
		if (this.ended || gen !== this.gen) return;
		console.error(`track ${track.url} failed: ${describeError(err)}`);
		this.failures++;
		this.record('failed', null, track.title);
		this.dropCurrent();
		if (this.failures < this.maxFailures) {
			this.playCurrent();
			return;
		}
		this.gen++;
		this.player.stop();
		this.clearPrefetchTimer();
		this.playback = 'paused';
		this.record('blocked', null, null);
		this.notice(BLOCKED_NOTICE);
		if (!this.queue.current) this.startIdleTimer();
		this.changed();
	}

	/** Matches a Spotify track to YouTube once; a match rewrites the track's url and source in place. */
	private matchFor(track: Track): Promise<ResolvedTrack | null> {
		let pending = this.matches.get(track.id);
		if (!pending) {
			pending = this.deps.resolver.matchSpotify(track).then((match) => {
				if (match) {
					Object.assign(track, {
						url: match.url,
						source: match.source,
						durationMs: match.durationMs,
						thumbnailUrl: track.thumbnailUrl ?? match.thumbnailUrl,
					});
				}
				return match;
			});
			// A failed lookup is retried when the track comes up again.
			pending.catch(() => this.matches.delete(track.id));
			this.matches.set(track.id, pending);
		}
		return pending;
	}

	private matchAhead(): void {
		for (const track of this.queue.upcoming.slice(0, LOOKAHEAD)) {
			if (track.source === 'spotify' && !this.matches.has(track.id)) {
				this.matchFor(track).catch((err) => console.error(`spotify match failed: ${describeError(err)}`));
			}
		}
	}

	private schedulePrefetch(): void {
		this.clearPrefetchTimer();
		const current = this.queue.current;
		if (!current) return;
		const delay = Math.max(0, current.durationMs - this.prefetchMs - this.player.positionMs);
		this.prefetchTimer = setTimeout(() => {
			this.prefetchTimer = null;
			void this.prefetchNext();
		}, delay);
	}

	private async prefetchNext(): Promise<void> {
		const next = this.queue.upcoming[0];
		if (!next || this.prefetched?.id === next.id) return;
		try {
			if (next.source === 'spotify' && !(await this.matchFor(next))) return;
			const url = await this.deps.resolver.streamUrl(next);
			if (!this.ended && this.queue.upcoming[0]?.id === next.id) this.prefetched = {id: next.id, url};
		} catch (err) {
			console.error(`prefetch of ${next.url} failed: ${describeError(err)}`);
		}
	}

	private takePrefetched(track: Track): string | null {
		const prefetched = this.prefetched;
		this.prefetched = null;
		return prefetched?.id === track.id ? prefetched.url : null;
	}

	// Room link

	private onRoomDisconnected(): void {
		if (this.ended) return;
		if (this.rejoining) {
			this.disconnectedAgain = true;
			return;
		}
		if (this.playback === 'playing' && this.playerGen === this.gen) {
			this.player.pause();
			this.pausedForRejoin = true;
		}
		void this.rejoin();
	}

	/**
	 * Rejoins until the room is stable. Every attempt, successful or not, counts toward the cap:
	 * once the 2-minute window holds REJOIN_MAX_ATTEMPTS and the room is lost again, the session ends.
	 */
	private async rejoin(): Promise<void> {
		this.rejoining = true;
		try {
			while (!this.ended) {
				this.pruneRejoinAttempts();
				const recent = this.rejoinAttempts.length;
				if (recent >= REJOIN_MAX_ATTEMPTS) {
					this.notice(ROOM_LOST_NOTICE);
					void this.end('room-lost');
					return;
				}
				if (recent > 0) {
					await this.wait(Math.min(REJOIN_DELAY_MIN_MS * 2 ** (recent - 1), REJOIN_DELAY_MAX_MS));
					if (this.ended) return;
				}
				this.rejoinAttempts.push(this.deps.now());
				this.disconnectedAgain = false;
				try {
					const connectionId = this.conn;
					if (connectionId === null) throw new Error('no voice connection to refresh');
					const grant = await this.voiceOp(() => this.deps.voice.refresh(connectionId));
					if (this.ended) return;
					this.conn = grant.connectionId;
					await this.roomOp(() => this.deps.room.reconnect(grant));
					if (this.ended) return;
					if (this.disconnectedAgain) continue;
					break;
				} catch (err) {
					if (this.ended) return;
					console.error(`voice rejoin failed: ${describeError(err)}`);
				}
			}
			if (this.ended) return;
			if (this.pausedForRejoin) {
				this.pausedForRejoin = false;
				if (this.playback === 'playing' && this.playerGen === this.gen) this.player.resume();
			}
			this.changed();
		} finally {
			this.rejoining = false;
		}
	}

	private pruneRejoinAttempts(): void {
		const now = this.deps.now();
		this.rejoinAttempts = this.rejoinAttempts.filter((t) => now - t < REJOIN_WINDOW_MS);
	}

	private wait(ms: number): Promise<void> {
		return new Promise((resolve) => {
			this.rejoinWait = {
				timer: setTimeout(() => {
					this.rejoinWait = null;
					resolve();
				}, ms),
				resolve,
			};
		});
	}

	/** Runs voice join/refresh one at a time: the gateway matches pending grants by channel only. */
	private voiceOp<T>(fn: () => Promise<T>): Promise<T> {
		const run = this.voiceChain.then(() => {
			if (this.ended) throw new MusicError('NOT_FOUND');
			return fn();
		});
		this.voiceChain = run.then(
			() => {},
			() => {},
		);
		return run;
	}

	/** Runs room connect/reconnect/disconnect one at a time, in call order. */
	private roomOp<T>(fn: () => Promise<T>): Promise<T> {
		const run = this.roomChain.then(fn);
		this.roomChain = run.then(
			() => {},
			() => {},
		);
		return run;
	}

	// State and side effects

	private record(kind: ActionKind, by: string | null, trackTitle: string | null): void {
		this.lastAction = {kind, by, at: this.deps.now(), trackTitle};
	}

	private changed(): void {
		if (this.ended) return;
		this.matchAhead();
		const state = this.state();
		this.broadcast(state);
		try {
			this.deps.card.update(state);
		} catch (err) {
			console.error(`card update failed: ${describeError(err)}`);
		}
	}

	private broadcast(state: MusicState = this.state()): void {
		this.deps.room.broadcast(state).catch((err) => console.error(`state broadcast failed: ${describeError(err)}`));
	}

	private notice(text: string): void {
		this.deps.card.notice(text).catch((err) => console.error(`card notice failed: ${describeError(err)}`));
	}

	private startIdleTimer(): void {
		this.clearIdleTimer();
		this.idleTimer = setTimeout(() => {
			this.idleTimer = null;
			void this.end('idle');
		}, this.idleMs);
	}

	private clearIdleTimer(): void {
		if (this.idleTimer) clearTimeout(this.idleTimer);
		this.idleTimer = null;
	}

	private clearPrefetchTimer(): void {
		if (this.prefetchTimer) clearTimeout(this.prefetchTimer);
		this.prefetchTimer = null;
	}

	private ensureActive(): void {
		if (this.ended) throw new MusicError('NOT_FOUND');
	}

	private async teardown(reason: EndReason): Promise<void> {
		this.gen++;
		this.clearIdleTimer();
		this.clearPrefetchTimer();
		if (this.heartbeatTimer) clearInterval(this.heartbeatTimer);
		this.heartbeatTimer = null;
		if (this.rejoinWait) {
			clearTimeout(this.rejoinWait.timer);
			this.rejoinWait.resolve();
			this.rejoinWait = null;
		}
		this.player.stop();
		this.queue.reset();
		this.matches.clear();
		this.prefetched = null;
		await this.roomOp(() => this.deps.room.disconnect()).catch((err) =>
			console.error(`room disconnect failed: ${describeError(err)}`),
		);
		if (this.conn !== null) {
			try {
				this.deps.voice.leave(this.conn);
			} catch (err) {
				console.error(`voice leave failed: ${describeError(err)}`);
			}
		}
		await this.deps.card.finish().catch((err) => console.error(`card finish failed: ${describeError(err)}`));
		this.emit('ended', reason);
	}
}
