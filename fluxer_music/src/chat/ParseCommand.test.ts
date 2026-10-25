// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {parseCommand} from './ParseCommand';

describe('parseCommand', () => {
	it.each([
		['!play never gonna', {name: 'play', arg: 'never gonna'}],
		['!PLAY x', {name: 'play', arg: 'x'}],
		['!playnext  some song ', {name: 'playnext', arg: 'some song'}],
		['!seek 1:23', {name: 'seek', ms: 83000}],
		['!seek 1:02:03', {name: 'seek', ms: 3723000}],
		['!seek abc', {name: 'invalid', usage: '!seek <m:ss | h:mm:ss>'}],
		['!seek -1:00', {name: 'invalid', usage: '!seek <m:ss | h:mm:ss>'}],
		['!seek 1:75', {name: 'invalid', usage: '!seek <m:ss | h:mm:ss>'}],
		['!play', {name: 'invalid', usage: '!play <link or search>'}],
		['!remove 0', {name: 'invalid', usage: '!remove <position>'}],
		['!remove 3', {name: 'remove', index: 3}],
		['!move 3 1', {name: 'move', from: 3, to: 1}],
		['!move 3', {name: 'invalid', usage: '!move <from> <to>'}],
		['!loop forever', {name: 'invalid', usage: '!loop <off|track|queue>'}],
		['!loop Queue', {name: 'loop', mode: 'queue'}],
		['!queue', {name: 'queue', page: 1}],
		['!queue 2', {name: 'queue', page: 2}],
		['!queue 0', {name: 'invalid', usage: '!queue [page]'}],
		['!skip', {name: 'skip'}],
		['!np', {name: 'np'}],
		['!help', {name: 'help'}],
		['hello', null],
		['!ban x', null],
		['!', null],
		['', null],
	])('%s', (input, expected) => {
		expect(parseCommand(input)).toEqual(expected);
	});
});
