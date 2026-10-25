// SPDX-License-Identifier: AGPL-3.0-or-later

import {livePositionMs} from '@fluxer/music_protocol/src/State';
import type {LastAction, MusicState} from '@fluxer/music_protocol/src/Types';
import type {FluxerRest} from '../fluxer/FluxerRest';
import type {Embed} from '../fluxer/Wire';

export const CARD_REACTIONS = ['⏯', '⏭', '🔀', '🔁', '⏹'] as const;

const BAR_CELLS = 20;
const UP_NEXT = 5;
const STOPPED_TEXT = 'Music stopped.';

export function formatDuration(ms: number): string {
	const total = Math.max(0, Math.floor(ms / 1000));
	const h = Math.floor(total / 3600);
	const m = Math.floor((total % 3600) / 60);
	const s = String(total % 60).padStart(2, '0');
	return h > 0 ? `${h}:${String(m).padStart(2, '0')}:${s}` : `${m}:${s}`;
}

function progressLine(positionMs: number, durationMs: number): string {
	const k =
		durationMs > 0 ? Math.min(BAR_CELLS - 1, Math.max(0, Math.floor(((BAR_CELLS - 1) * positionMs) / durationMs))) : 0;
	const bar = '▬'.repeat(k) + '🔘' + '▬'.repeat(BAR_CELLS - 1 - k);
	return `${bar} ${formatDuration(positionMs)} / ${formatDuration(durationMs)}`;
}

function actionText(action: LastAction, state: MusicState): string {
	const who = action.by ? `<@${action.by}> ` : '';
	const title = action.trackTitle ? ` ${action.trackTitle}` : '';
	switch (action.kind) {
		case 'add':
			return `➕ ${who}added${title}`;
		case 'pause':
			return `⏸ ${who}paused`;
		case 'resume':
			return `▶ ${who}resumed`;
		case 'skip':
			return `⏭ ${who}skipped${title}`;
		case 'seek':
			return `⏩ ${who}seeked`;
		case 'remove':
			return `🗑 ${who}removed${title}`;
		case 'move':
			return `↕ ${who}moved${title}`;
		case 'clear':
			return `🧹 ${who}cleared the queue`;
		case 'shuffle':
			return `🔀 ${who}shuffled the queue`;
		case 'loop':
			return `🔁 ${who}set loop to ${state.loop}`;
		case 'stop':
			return `⏹ ${who}stopped`;
		case 'failed':
			return `⚠ couldn't play${title}`;
		case 'unmatched':
			return `⚠ no YouTube match for${title}`;
		case 'blocked':
			return '⚠ YouTube may be blocking us';
	}
}

export function renderCard(state: MusicState, now: number): Embed {
	const current = state.current;
	const lines: Array<string> = [];
	if (current) {
		lines.push(progressLine(livePositionMs(state, now), current.durationMs));
		lines.push(`Added by <@${current.addedBy}>`);
	}
	if (state.upcoming.length > 0) {
		if (lines.length > 0) lines.push('');
		lines.push('Up next:');
		state.upcoming.slice(0, UP_NEXT).forEach((t, i) => {
			lines.push(`${i + 1}. ${t.title}`);
		});
	}
	// Footers are plain text, so the action line (which may mention someone) goes in the description.
	if (state.lastAction) {
		if (lines.length > 0) lines.push('');
		lines.push(actionText(state.lastAction, state));
	}

	const embed: Embed = {title: current?.title ?? 'Nothing playing'};
	if (current) embed.url = current.url;
	if (current?.thumbnailUrl) embed.thumbnail = {url: current.thumbnailUrl};
	if (lines.length > 0) embed.description = lines.join('\n');
	embed.footer = {text: `${state.queueLength} in queue · loop ${state.loop}`};
	return embed;
}

function describeError(err: unknown): string {
	return err instanceof Error ? err.message : String(err);
}

/**
 * The "now playing" message: edited on change (at most once per minEditMs) and every refreshMs while playing.
 * Every HTTP call for the card runs on one queue, so "Music stopped." is always the last write.
 */
export class LiveCard {
	private readonly minEditMs: number;
	private readonly refreshMs: number;
	private readonly now: () => number;

	private id: string | null = null;
	private latest: MusicState | null = null;
	private lastEditAt = Number.NEGATIVE_INFINITY;
	private editTimer: ReturnType<typeof setTimeout> | null = null;
	private refreshTimer: ReturnType<typeof setInterval> | null = null;
	private finished = false;
	private queue: Promise<unknown> = Promise.resolve();

	constructor(
		private readonly channelId: string,
		private readonly rest: FluxerRest,
		opts: {minEditMs?: number; refreshMs?: number; now?: () => number} = {},
	) {
		this.minEditMs = opts.minEditMs ?? 3_000;
		this.refreshMs = opts.refreshMs ?? 15_000;
		this.now = opts.now ?? Date.now;
	}

	get messageId(): string | null {
		return this.id;
	}

	async start(state: MusicState): Promise<void> {
		if (await this.post(state)) this.track(state);
	}

	update(state: MusicState): void {
		if (this.finished || this.id === null) return;
		this.track(state);
		this.scheduleEdit();
	}

	async repost(state: MusicState): Promise<void> {
		const old = this.id;
		this.id = null;
		this.clearRefreshTimer();
		if (old !== null) {
			await this.enqueue(() =>
				this.rest
					.deleteMessage(this.channelId, old)
					.catch((err) => console.error(`card delete failed: ${describeError(err)}`)),
			);
		}
		this.clearEditTimer();
		if (await this.post(state)) this.track(state);
	}

	async notice(text: string): Promise<void> {
		if (this.id === null) return;
		await this.enqueue(() => this.rest.sendMessage(this.channelId, {content: text}));
	}

	async finish(): Promise<void> {
		this.finished = true;
		this.clearEditTimer();
		this.clearRefreshTimer();
		await this.enqueue(async () => {
			if (this.id === null) return;
			await this.rest.editMessage(this.channelId, this.id, {content: STOPPED_TEXT, embeds: []});
		});
	}

	/** Runs `fn` after every card call queued before it, whether those succeeded or not. */
	private enqueue<T>(fn: () => Promise<T>): Promise<T> {
		const result = this.queue.then(fn);
		this.queue = result.catch(() => {});
		return result;
	}

	/** Sends the card; on failure logs and stays card-less. Returns whether a card exists. */
	private post(state: MusicState): Promise<boolean> {
		return this.enqueue(() => this.send(state));
	}

	private async send(state: MusicState): Promise<boolean> {
		if (this.finished) return false;
		let id: string;
		try {
			({id} = await this.rest.sendMessage(this.channelId, {embeds: [renderCard(state, this.now())]}));
		} catch (err) {
			console.error(`card post failed: ${describeError(err)}`);
			return false;
		}
		this.id = id;
		this.lastEditAt = this.now();
		for (const emoji of CARD_REACTIONS) {
			try {
				await this.rest.addReaction(this.channelId, id, emoji);
			} catch (err) {
				console.error(`card reaction failed: ${describeError(err)}`);
			}
		}
		return true;
	}

	private track(state: MusicState): void {
		this.latest = state;
		if (state.status === 'playing') {
			this.refreshTimer ??= setInterval(() => this.scheduleEdit(), this.refreshMs);
		} else {
			this.clearRefreshTimer();
		}
	}

	/** Edits now if the last edit is old enough, otherwise once the minimum gap has passed. */
	private scheduleEdit(): void {
		if (this.editTimer) return;
		const wait = this.lastEditAt + this.minEditMs - this.now();
		if (wait <= 0) {
			this.edit();
			return;
		}
		this.editTimer = setTimeout(() => {
			this.editTimer = null;
			this.edit();
		}, wait);
	}

	private edit(): void {
		if (this.finished || this.id === null || this.latest === null) return;
		this.lastEditAt = this.now();
		void this.enqueue(async () => {
			// Re-checked here: finish() or repost() may have run while this edit waited its turn.
			if (this.finished || this.id === null || this.latest === null) return;
			const id = this.id;
			const embed = renderCard(this.latest, this.now());
			await this.rest
				.editMessage(this.channelId, id, {embeds: [embed]})
				.catch((err) => console.error(`card edit failed: ${describeError(err)}`));
		});
	}

	private clearEditTimer(): void {
		if (this.editTimer) clearTimeout(this.editTimer);
		this.editTimer = null;
	}

	private clearRefreshTimer(): void {
		if (this.refreshTimer) clearInterval(this.refreshTimer);
		this.refreshTimer = null;
	}
}
