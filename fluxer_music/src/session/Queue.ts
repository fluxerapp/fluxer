// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {LoopMode, Track} from '@fluxer/music_protocol/src/Types';
import {MusicError} from '../Errors';
import type {ResolvedTrack} from '../Types';

const DEFAULT_MAX_LENGTH = 500;

export interface QueueOptions {
	maxLength?: number;
	random?: () => number;
	newId?: () => string;
}

export class Queue {
	private currentTrack: Track | null = null;
	private items: Array<Track> = [];
	private readonly maxLength: number;
	private readonly random: () => number;
	private readonly newId: () => string;
	loop: LoopMode = 'off';

	constructor(opts: QueueOptions = {}) {
		this.maxLength = opts.maxLength ?? DEFAULT_MAX_LENGTH;
		this.random = opts.random ?? Math.random;
		this.newId = opts.newId ?? (() => randomUUID());
	}

	get current(): Track | null {
		return this.currentTrack;
	}

	get upcoming(): ReadonlyArray<Track> {
		return this.items;
	}

	add(tracks: Array<ResolvedTrack>, addedBy: string, opts: {next?: boolean} = {}): number {
		const used = this.items.length + (this.currentTrack ? 1 : 0);
		const capacity = Math.max(0, this.maxLength - used);
		if (capacity === 0) {
			throw new MusicError('QUEUE_FULL');
		}
		const added: Array<Track> = tracks.slice(0, capacity).map((t) => ({...t, id: this.newId(), addedBy}));
		if (opts.next) {
			this.items.unshift(...added);
		} else {
			this.items.push(...added);
		}
		return added.length;
	}

	remove(id: string): boolean {
		const index = this.items.findIndex((t) => t.id === id);
		if (index < 0) return false;
		this.items.splice(index, 1);
		return true;
	}

	move(id: string, to: number): boolean {
		const from = this.items.findIndex((t) => t.id === id);
		if (from < 0) return false;
		const [track] = this.items.splice(from, 1);
		const target = Math.min(Math.max(to, 0), this.items.length);
		this.items.splice(target, 0, track!);
		return true;
	}

	clear(): void {
		this.items = [];
	}

	shuffle(): void {
		for (let i = this.items.length - 1; i > 0; i--) {
			const j = Math.floor(this.random() * (i + 1));
			[this.items[i], this.items[j]] = [this.items[j]!, this.items[i]!];
		}
	}

	advance(): Track | null {
		if (this.loop === 'track' && this.currentTrack) {
			return this.currentTrack;
		}
		return this.skip();
	}

	skip(): Track | null {
		if (this.loop === 'queue' && this.currentTrack) {
			this.items.push(this.currentTrack);
		}
		this.currentTrack = this.items.shift() ?? null;
		return this.currentTrack;
	}

	page(offset: number, limit: number): {tracks: Array<Track>; total: number} {
		return {tracks: this.items.slice(offset, offset + limit), total: this.items.length};
	}

	reset(): void {
		this.currentTrack = null;
		this.items = [];
	}
}
