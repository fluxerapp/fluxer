// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LoopMode} from '@fluxer/music_protocol/src/Types';

export type Command =
	| {name: 'play' | 'playnext'; arg: string}
	| {name: 'skip' | 'pause' | 'resume' | 'np' | 'clear' | 'shuffle' | 'stop' | 'help'}
	| {name: 'queue'; page: number}
	| {name: 'remove'; index: number}
	| {name: 'move'; from: number; to: number}
	| {name: 'loop'; mode: LoopMode}
	| {name: 'seek'; ms: number}
	| {name: 'invalid'; usage: string};

const PREFIX = '!';
const LOOP_MODES: ReadonlyArray<string> = ['off', 'track', 'queue'];

const USAGE = {
	play: '!play <link or search>',
	playnext: '!playnext <link or search>',
	queue: '!queue [page]',
	remove: '!remove <position>',
	move: '!move <from> <to>',
	loop: '!loop <off|track|queue>',
	seek: '!seek <m:ss | h:mm:ss>',
} as const;

const NO_ARGS = new Set(['skip', 'pause', 'resume', 'np', 'clear', 'shuffle', 'stop', 'help']);

function positiveInt(text: string | undefined): number | null {
	if (text === undefined || !/^\d+$/.test(text)) return null;
	const n = Number(text);
	return Number.isSafeInteger(n) && n >= 1 ? n : null;
}

function parseTimestamp(text: string): number | null {
	const parts = text.split(':');
	if (parts.length < 2 || parts.length > 3 || !parts.every((p) => /^\d{1,6}$/.test(p))) return null;
	const nums = parts.map(Number);
	// Every field after the first is minutes or seconds and must be below 60.
	if (nums.slice(1).some((n) => n >= 60)) return null;
	const [h, m, s] = nums.length === 3 ? nums : [0, nums[0], nums[1]];
	return (((h as number) * 60 + (m as number)) * 60 + (s as number)) * 1000;
}

/** Returns null for anything that isn't one of our commands, so other bots' commands are left alone. */
export function parseCommand(content: string): Command | null {
	if (!content.startsWith(PREFIX)) return null;
	const words = content.slice(PREFIX.length).trim().split(/\s+/);
	const name = (words[0] ?? '').toLowerCase();
	const args = words.slice(1);
	const rest = args.join(' ');
	const invalid = (usage: string): Command => ({name: 'invalid', usage});

	switch (name) {
		case 'play':
		case 'playnext':
			return rest ? {name, arg: rest} : invalid(USAGE[name]);
		case 'queue': {
			if (args.length === 0) return {name: 'queue', page: 1};
			const page = positiveInt(args[0]);
			return page !== null && args.length === 1 ? {name: 'queue', page} : invalid(USAGE.queue);
		}
		case 'remove': {
			const index = positiveInt(args[0]);
			return index !== null && args.length === 1 ? {name: 'remove', index} : invalid(USAGE.remove);
		}
		case 'move': {
			const from = positiveInt(args[0]);
			const to = positiveInt(args[1]);
			return from !== null && to !== null && args.length === 2 ? {name: 'move', from, to} : invalid(USAGE.move);
		}
		case 'loop': {
			const mode = (args[0] ?? '').toLowerCase();
			return args.length === 1 && LOOP_MODES.includes(mode)
				? {name: 'loop', mode: mode as LoopMode}
				: invalid(USAGE.loop);
		}
		case 'seek': {
			const ms = args.length === 1 ? parseTimestamp(args[0] as string) : null;
			return ms !== null ? {name: 'seek', ms} : invalid(USAGE.seek);
		}
		default:
			if (NO_ARGS.has(name)) return {name} as Command;
			return null;
	}
}
