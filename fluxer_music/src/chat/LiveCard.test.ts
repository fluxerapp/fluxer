// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MusicState, Track} from '@fluxer/music_protocol/src/Types';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';
import type {FluxerRest} from '../fluxer/FluxerRest';
import {CARD_REACTIONS, LiveCard, renderCard} from './LiveCard';

function track(n: number, durationMs = 200_000): Track {
	return {
		id: `t${n}`,
		title: `Song ${n}`,
		author: `Artist ${n}`,
		durationMs,
		thumbnailUrl: `https://img/${n}.jpg`,
		url: `https://youtu.be/${n}`,
		source: 'youtube',
		addedBy: '42',
	};
}

function state(over: Partial<MusicState> = {}): MusicState {
	return {
		v: 1,
		status: 'playing',
		current: track(1),
		positionMs: 100_000,
		positionAt: 1_000,
		loop: 'off',
		upcoming: [track(2), track(3)],
		queueLength: 2,
		lastAction: null,
		...over,
	};
}

describe('renderCard', () => {
	it('renders progress, up next and footer', () => {
		const embed = renderCard(
			state({
				upcoming: [2, 3, 4, 5, 6, 7].map((n) => track(n)),
				queueLength: 6,
				loop: 'queue',
				lastAction: {kind: 'skip', by: '42', at: 0, trackTitle: 'Old'},
			}),
			11_000,
		);
		expect(embed).toMatchInlineSnapshot(`
			{
			  "description": "▬▬▬▬▬▬▬▬▬▬🔘▬▬▬▬▬▬▬▬▬ 1:50 / 3:20
			Added by <@42>

			Up next:
			1. Song 2
			2. Song 3
			3. Song 4
			4. Song 5
			5. Song 6

			⏭ <@42> skipped Old",
			  "footer": {
			    "text": "6 in queue · loop queue",
			  },
			  "thumbnail": {
			    "url": "https://img/1.jpg",
			  },
			  "title": "Song 1",
			  "url": "https://youtu.be/1",
			}
		`);
	});

	it('renders the idle card', () => {
		const embed = renderCard(state({status: 'idle', current: null, upcoming: [], queueLength: 0}), 0);
		expect(embed.title).toBe('Nothing playing');
		expect(embed.footer?.text).toBe('0 in queue · loop off');
	});

	it('describes failures', () => {
		const at = (kind: 'failed' | 'unmatched') =>
			renderCard(state({lastAction: {kind, by: null, at: 0, trackTitle: 'Song'}}), 0)
				.description?.split('\n')
				.at(-1);
		expect(at('failed')).toBe("⚠ couldn't play Song");
		expect(at('unmatched')).toBe('⚠ no YouTube match for Song');
	});

	it('puts the last action in the description when idle', () => {
		const embed = renderCard(
			state({
				status: 'idle',
				current: null,
				upcoming: [],
				queueLength: 0,
				lastAction: {kind: 'clear', by: '7', at: 0, trackTitle: null},
			}),
			0,
		);
		expect(embed.description).toBe('🧹 <@7> cleared the queue');
		expect(embed.footer?.text).toBe('0 in queue · loop off');
	});
});

describe('LiveCard', () => {
	let nowMs = 0;
	let rest: {
		sendMessage: ReturnType<typeof vi.fn>;
		editMessage: ReturnType<typeof vi.fn>;
		deleteMessage: ReturnType<typeof vi.fn>;
		addReaction: ReturnType<typeof vi.fn>;
	};
	let ids: number;

	beforeEach(() => {
		vi.useFakeTimers();
		nowMs = 0;
		ids = 0;
		rest = {
			sendMessage: vi.fn(async () => ({id: `m${++ids}`})),
			editMessage: vi.fn(async () => {}),
			deleteMessage: vi.fn(async () => {}),
			addReaction: vi.fn(async () => {}),
		};
	});
	afterEach(() => vi.useRealTimers());

	const make = () => new LiveCard('c1', rest as unknown as FluxerRest, {now: () => nowMs});
	const advance = async (ms: number) => {
		nowMs += ms;
		await vi.advanceTimersByTimeAsync(ms);
	};

	it('start sends the card and adds the reactions', async () => {
		const card = make();
		await card.start(state());
		expect(card.messageId).toBe('m1');
		expect(rest.sendMessage).toHaveBeenCalledWith('c1', {embeds: [expect.objectContaining({title: 'Song 1'})]});
		expect(rest.addReaction.mock.calls.map((c) => c[2])).toEqual([...CARD_REACTIONS]);
		await card.finish();
	});

	it('coalesces edits to one per 3s', async () => {
		const card = make();
		await card.start(state({status: 'paused'}));
		await advance(5_000);
		card.update(state({status: 'paused', loop: 'track'}));
		await advance(0);
		expect(rest.editMessage).toHaveBeenCalledTimes(1);
		card.update(state({status: 'paused', loop: 'queue'}));
		card.update(state({status: 'paused', loop: 'off', queueLength: 9}));
		await advance(2_900);
		expect(rest.editMessage).toHaveBeenCalledTimes(1);
		await advance(200);
		expect(rest.editMessage).toHaveBeenCalledTimes(2);
		const body = rest.editMessage.mock.calls[1]?.[2] as {embeds: Array<{footer: {text: string}}>};
		expect(body.embeds[0]?.footer.text).toBe('9 in queue · loop off');
		await card.finish();
	});

	it('refreshes every 15s while playing', async () => {
		const card = make();
		await card.start(state());
		await advance(14_900);
		expect(rest.editMessage).not.toHaveBeenCalled();
		await advance(200);
		expect(rest.editMessage).toHaveBeenCalledTimes(1);
		await advance(15_000);
		expect(rest.editMessage).toHaveBeenCalledTimes(2);
		card.update(state({status: 'paused'}));
		await advance(3_000);
		const edits = rest.editMessage.mock.calls.length;
		await advance(60_000);
		expect(rest.editMessage).toHaveBeenCalledTimes(edits);
		await card.finish();
	});

	it('repost deletes the old card and sends a new one', async () => {
		const card = make();
		await card.start(state());
		await card.repost(state());
		expect(rest.deleteMessage).toHaveBeenCalledWith('c1', 'm1');
		expect(card.messageId).toBe('m2');
		expect(rest.addReaction).toHaveBeenCalledTimes(10);
		await card.finish();
	});

	it('notice sends a plain message', async () => {
		const card = make();
		await card.start(state());
		await card.notice('hello');
		expect(rest.sendMessage).toHaveBeenCalledWith('c1', {content: 'hello'});
	});

	it('start without a card is a no-op afterwards', async () => {
		const log = vi.spyOn(console, 'error').mockImplementation(() => {});
		rest.sendMessage.mockRejectedValueOnce(new Error('403'));
		const card = make();
		await card.start(state());
		expect(card.messageId).toBeNull();
		rest.sendMessage.mockClear();
		card.update(state());
		await card.notice('hi');
		await advance(60_000);
		await card.finish();
		expect(rest.sendMessage).not.toHaveBeenCalled();
		expect(rest.editMessage).not.toHaveBeenCalled();
		log.mockRestore();
	});

	it('repost whose send fails leaves no card', async () => {
		const log = vi.spyOn(console, 'error').mockImplementation(() => {});
		const card = make();
		await card.start(state());
		rest.sendMessage.mockRejectedValueOnce(new Error('403'));
		await card.repost(state());
		expect(card.messageId).toBeNull();
		card.update(state());
		await advance(60_000);
		await card.finish();
		expect(rest.editMessage).not.toHaveBeenCalled();
		log.mockRestore();
	});

	it('finish is the last write even while an edit is in flight', async () => {
		const card = make();
		await card.start(state({status: 'paused'}));
		let release = () => {};
		rest.editMessage.mockImplementationOnce(
			() =>
				new Promise<void>((resolve) => {
					release = resolve;
				}),
		);
		await advance(5_000);
		card.update(state({status: 'paused', loop: 'track'}));
		await advance(0);
		expect(rest.editMessage).toHaveBeenCalledTimes(1);
		const finished = card.finish();
		await advance(0);
		expect(rest.editMessage).toHaveBeenCalledTimes(1);
		release();
		await finished;
		expect(rest.editMessage).toHaveBeenCalledTimes(2);
		expect(rest.editMessage.mock.calls.at(-1)).toEqual(['c1', 'm1', {content: 'Music stopped.', embeds: []}]);
	});

	it('finish waits for a card that is still being posted', async () => {
		let release = (_v: {id: string}) => {};
		rest.sendMessage.mockImplementationOnce(
			() =>
				new Promise<{id: string}>((resolve) => {
					release = resolve;
				}),
		);
		const card = make();
		const started = card.start(state());
		await advance(0);
		expect(rest.sendMessage).toHaveBeenCalledTimes(1);
		const finished = card.finish();
		release({id: 'm1'});
		await started;
		await finished;
		expect(rest.editMessage.mock.calls).toEqual([['c1', 'm1', {content: 'Music stopped.', embeds: []}]]);
	});

	it('a card not yet sent when finish is called is never sent', async () => {
		const card = make();
		const started = card.start(state());
		const finished = card.finish();
		await started;
		await finished;
		expect(rest.sendMessage).not.toHaveBeenCalled();
		expect(rest.editMessage).not.toHaveBeenCalled();
	});

	it('finish edits to Music stopped and stops timers', async () => {
		const card = make();
		await card.start(state());
		await card.finish();
		expect(rest.editMessage).toHaveBeenCalledWith('c1', 'm1', {content: 'Music stopped.', embeds: []});
		card.update(state());
		await advance(60_000);
		expect(rest.editMessage).toHaveBeenCalledTimes(1);
	});
});
