// SPDX-License-Identifier: AGPL-3.0-or-later

import {execFileSync} from 'node:child_process';
import {mkdtempSync, rmSync} from 'node:fs';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {afterAll, afterEach, beforeAll, describe, expect, test} from 'vitest';
import {type AudioSink, FRAME_SAMPLES_PER_CHANNEL, Player} from './Player';

let dir: string;
let tone: string;

beforeAll(() => {
	dir = mkdtempSync(join(tmpdir(), 'player-test-'));
	tone = join(dir, 'tone.wav');
	execFileSync('ffmpeg', [
		'-hide_banner',
		'-loglevel',
		'error',
		'-f',
		'lavfi',
		'-i',
		'sine=frequency=440:duration=1.5',
		'-ar',
		'48000',
		'-ac',
		'2',
		tone,
	]);
});

afterAll(() => {
	rmSync(dir, {recursive: true, force: true});
});

class FakeSink implements AudioSink {
	frames = 0;
	badLength = 0;
	onFrame: ((count: number) => void) | null = null;
	async captureFrame(samples: Int16Array): Promise<void> {
		if (samples.length !== FRAME_SAMPLES_PER_CHANNEL * 2) this.badLength++;
		this.frames++;
		this.onFrame?.(this.frames);
		await new Promise<void>((resolve) => setImmediate(resolve));
	}
}

const once = (player: Player, event: 'ended' | 'error' | 'started') =>
	new Promise<unknown>((resolve) => player.once(event, resolve));

const sleep = (ms: number) => new Promise<void>((resolve) => setTimeout(resolve, ms));

const waitFor = async (cond: () => boolean, timeoutMs = 5000) => {
	const start = Date.now();
	while (!cond()) {
		if (Date.now() - start > timeoutMs) throw new Error('waitFor timed out');
		await sleep(5);
	}
};

let players: Array<Player> = [];
const make = (opts?: ConstructorParameters<typeof Player>[1]) => {
	const sink = new FakeSink();
	const player = new Player(sink, opts);
	players.push(player);
	return {sink, player};
};

afterEach(() => {
	for (const p of players) p.stop();
	players = [];
});

describe('Player', () => {
	test('plays a file to the end', async () => {
		const {sink, player} = make();
		const ended = once(player, 'ended');
		player.play(tone);
		await ended;
		expect(sink.badLength).toBe(0);
		expect(sink.frames).toBeGreaterThanOrEqual(147);
		expect(sink.frames).toBeLessThanOrEqual(153);
		expect(Math.abs(player.positionMs - 1500)).toBeLessThanOrEqual(30);
		expect(player.state).toBe('stopped');
	});

	test('pause stops frames, resume continues', async () => {
		const {sink, player} = make();
		const paused = new Promise<void>((resolve) => {
			sink.onFrame = (n) => {
				if (n === 50) {
					player.pause();
					resolve();
				}
			};
		});
		player.play(tone);
		await paused;
		await sleep(50);
		const count = sink.frames;
		expect(player.state).toBe('paused');
		await sleep(200);
		expect(sink.frames).toBe(count);
		const ended = once(player, 'ended');
		player.resume();
		await ended;
		expect(Math.abs(sink.frames - 150)).toBeLessThanOrEqual(3);
	});

	test('seek restarts from the position', async () => {
		const {sink, player} = make();
		const started = once(player, 'started');
		player.play(tone);
		await started;
		const before = sink.frames;
		const ended = once(player, 'ended');
		player.seek(1000);
		await ended;
		const after = sink.frames - before;
		expect(Math.abs(after - 50)).toBeLessThanOrEqual(3 + 1);
		expect(Math.abs(player.positionMs - 1500)).toBeLessThanOrEqual(30);
	});

	test('seek while paused stays paused', async () => {
		const {sink, player} = make();
		const paused = new Promise<void>((resolve) => {
			sink.onFrame = (n) => {
				if (n === 50) {
					player.pause();
					resolve();
				}
			};
		});
		player.play(tone);
		await paused;
		await sleep(50);
		player.seek(1000);
		const count = sink.frames;
		expect(player.state).toBe('paused');
		expect(Math.abs(player.positionMs - 1000)).toBeLessThanOrEqual(10);
		expect(player.processAlive).toBe(false);
		await sleep(200);
		expect(sink.frames).toBe(count);
		expect(player.state).toBe('paused');
		const ended = once(player, 'ended');
		player.resume();
		await ended;
		expect(Math.abs(sink.frames - count - 50)).toBeLessThanOrEqual(3);
	});

	test('stop() suppresses ended', async () => {
		const {sink, player} = make();
		let ended = false;
		player.on('ended', () => {
			ended = true;
		});
		sink.onFrame = (n) => {
			if (n === 10) player.stop();
		};
		player.play(tone);
		await waitFor(() => sink.frames >= 10);
		expect(player.state).toBe('stopped');
		expect(player.processAlive).toBe(false);
		await sleep(300);
		expect(ended).toBe(false);
		expect(player.processAlive).toBe(false);
		expect(player.state).toBe('stopped');
	});

	test('idle pause kills ffmpeg and resume restarts at position', async () => {
		const {sink, player} = make({idleKillMs: 100});
		const paused = new Promise<void>((resolve) => {
			sink.onFrame = (n) => {
				if (n === 50) {
					player.pause();
					resolve();
				}
			};
		});
		player.play(tone);
		await paused;
		await waitFor(() => !player.processAlive, 2000);
		expect(player.state).toBe('paused');
		const ended = once(player, 'ended');
		player.resume();
		await ended;
		expect(Math.abs(sink.frames - 150)).toBeLessThanOrEqual(3);
	});

	test('bad input emits error', async () => {
		const {player} = make();
		let ended = false;
		player.on('ended', () => {
			ended = true;
		});
		const err = once(player, 'error');
		player.play('/nonexistent.wav');
		expect(await err).toBeInstanceOf(Error);
		await sleep(50);
		expect(ended).toBe(false);
		expect(player.state).toBe('stopped');
	});
});
