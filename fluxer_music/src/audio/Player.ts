// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChildProcess, spawn} from 'node:child_process';
import {EventEmitter} from 'node:events';

export const FRAME_SAMPLES_PER_CHANNEL = 480;
const CHANNELS = 2;
const FRAME_BYTES = FRAME_SAMPLES_PER_CHANNEL * CHANNELS * 2;
const FRAME_MS = 10;
const HIGH_WATER_BYTES = FRAME_BYTES * 50;
const MAX_STDERR_CHARS = 2000;

export interface AudioSink {
	captureFrame(samples: Int16Array): Promise<void>;
}

export type PlayerState = 'stopped' | 'playing' | 'paused';

export interface PlayerOptions {
	ffmpegBin?: string;
	idleKillMs?: number;
}

export class Player extends EventEmitter {
	private readonly sink: AudioSink;
	private readonly ffmpegBin: string;
	private readonly idleKillMs: number;

	private generation = 0;
	private child: ChildProcess | null = null;
	private input: string | null = null;
	private startMs = 0;
	private framesSent = 0;
	private currentState: PlayerState = 'stopped';
	private buffer: Buffer = Buffer.alloc(0);
	private closed = false;
	private exitCode: number | null = null;
	private stderr = '';
	private pumping = false;
	private startedEmitted = false;
	private idleTimer: NodeJS.Timeout | null = null;

	constructor(sink: AudioSink, opts: PlayerOptions = {}) {
		super();
		this.sink = sink;
		this.ffmpegBin = opts.ffmpegBin ?? 'ffmpeg';
		this.idleKillMs = opts.idleKillMs ?? 600_000;
	}

	get positionMs(): number {
		return this.startMs + this.framesSent * FRAME_MS;
	}

	get state(): PlayerState {
		return this.currentState;
	}

	get processAlive(): boolean {
		return this.child !== null && !this.closed;
	}

	play(input: string, startMs = 0): void {
		this.killProcess();
		const gen = this.generation;
		this.input = input;
		this.startMs = startMs;
		this.framesSent = 0;
		this.buffer = Buffer.alloc(0);
		this.closed = false;
		this.exitCode = null;
		this.stderr = '';
		this.startedEmitted = false;
		this.currentState = 'playing';

		const isHttp = /^https?:\/\//i.test(input);
		const args = [
			'-hide_banner',
			'-loglevel',
			'error',
			...(isHttp ? ['-reconnect', '1', '-reconnect_streamed', '1', '-reconnect_delay_max', '5'] : []),
			...(startMs > 0 ? ['-ss', (startMs / 1000).toFixed(3)] : []),
			'-i',
			input,
			'-af',
			'loudnorm',
			'-f',
			's16le',
			'-ar',
			'48000',
			'-ac',
			'2',
			'pipe:1',
		];
		const child = spawn(this.ffmpegBin, args, {stdio: ['ignore', 'pipe', 'pipe'], shell: false});
		this.child = child;

		child.stdout?.on('data', (chunk: Buffer) => {
			if (gen !== this.generation) return;
			this.buffer = this.buffer.length === 0 ? chunk : Buffer.concat([this.buffer, chunk]);
			if (this.buffer.length >= HIGH_WATER_BYTES) child.stdout?.pause();
			void this.pump(gen);
		});
		child.stderr?.on('data', (chunk: Buffer) => {
			if (gen !== this.generation) return;
			if (this.stderr.length < MAX_STDERR_CHARS) this.stderr += chunk.toString('utf8');
		});
		child.on('error', (err) => {
			if (gen !== this.generation) return;
			this.fail(err);
		});
		child.on('close', (code, signal) => {
			if (gen !== this.generation) return;
			this.closed = true;
			this.exitCode = code ?? (signal ? -1 : 0);
			void this.pump(gen);
		});
	}

	pause(): void {
		if (this.currentState !== 'playing') return;
		this.currentState = 'paused';
		this.clearIdleTimer();
		const gen = this.generation;
		this.idleTimer = setTimeout(() => {
			this.idleTimer = null;
			if (gen !== this.generation || this.currentState !== 'paused') return;
			this.killProcess();
		}, this.idleKillMs);
		this.idleTimer.unref();
	}

	resume(): void {
		if (this.currentState !== 'paused' || this.input === null) return;
		this.clearIdleTimer();
		if (!this.processAlive) {
			this.play(this.input, this.positionMs);
			return;
		}
		this.currentState = 'playing';
		void this.pump(this.generation);
	}

	seek(ms: number): void {
		if (this.input === null) return;
		if (this.currentState === 'paused') {
			this.killProcess();
			this.startMs = ms;
			this.framesSent = 0;
			this.buffer = Buffer.alloc(0);
			return;
		}
		this.play(this.input, ms);
	}

	stop(): void {
		this.killProcess();
		this.currentState = 'stopped';
		this.buffer = Buffer.alloc(0);
	}

	private clearIdleTimer(): void {
		if (this.idleTimer) {
			clearTimeout(this.idleTimer);
			this.idleTimer = null;
		}
	}

	/** Bumps the generation so events from the old process are ignored, then kills it. */
	private killProcess(): void {
		this.generation++;
		this.clearIdleTimer();
		this.pumping = false;
		const child = this.child;
		this.child = null;
		this.closed = true;
		if (child) {
			child.stdout?.removeAllListeners('data');
			child.kill('SIGKILL');
		}
	}

	private fail(err: Error): void {
		this.killProcess();
		this.currentState = 'stopped';
		this.buffer = Buffer.alloc(0);
		this.emit('error', err);
	}

	private async pump(gen: number): Promise<void> {
		if (this.pumping) return;
		this.pumping = true;
		try {
			while (gen === this.generation && this.currentState === 'playing' && this.buffer.length >= FRAME_BYTES) {
				const frame = this.takeFrame(FRAME_BYTES);
				if (!this.startedEmitted) {
					this.startedEmitted = true;
					this.emit('started');
					if (gen !== this.generation) return;
				}
				await this.sink.captureFrame(frame);
				if (gen !== this.generation) return;
				this.framesSent++;
				if (this.buffer.length < HIGH_WATER_BYTES) this.child?.stdout?.resume();
			}
			if (gen !== this.generation || this.currentState !== 'playing') return;
			if (this.closed && this.child) await this.finish(gen);
		} finally {
			if (gen === this.generation) this.pumping = false;
		}
	}

	private async finish(gen: number): Promise<void> {
		if (this.exitCode !== 0) {
			const detail = this.stderr.trim();
			this.fail(new Error(`ffmpeg exited with code ${this.exitCode}${detail ? `: ${detail}` : ''}`));
			return;
		}
		if (this.buffer.length > 0) {
			const frame = this.takeFrame(this.buffer.length);
			await this.sink.captureFrame(frame);
			if (gen !== this.generation) return;
			this.framesSent++;
		}
		this.child = null;
		this.currentState = 'stopped';
		this.emit('ended');
	}

	/** Removes `bytes` from the buffer and returns them as a zero-padded one-frame Int16Array. */
	private takeFrame(bytes: number): Int16Array {
		const frame = new Int16Array(FRAME_BYTES / 2);
		Buffer.from(frame.buffer).set(this.buffer.subarray(0, bytes));
		this.buffer = this.buffer.subarray(bytes);
		return frame;
	}
}
