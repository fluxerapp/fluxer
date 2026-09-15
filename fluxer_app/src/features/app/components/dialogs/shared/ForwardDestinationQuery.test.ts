// SPDX-License-Identifier: AGPL-3.0-or-later

import {parseForwardDestinationQuery} from '@app/features/app/components/dialogs/shared/ForwardDestinationQuery';
import {describe, expect, it} from 'vitest';

const ALL_TYPES = ['user', 'text_channel', 'voice_channel', 'group_dm'];

describe('parseForwardDestinationQuery', () => {
	it('searches every destination type without a sigil', () => {
		expect(parseForwardDestinationQuery('general')).toEqual({mode: null, query: 'general', resultTypes: ALL_TYPES});
		expect(parseForwardDestinationQuery('')).toEqual({mode: null, query: '', resultTypes: ALL_TYPES});
	});

	it('narrows to one type for the @, # and ! sigils', () => {
		expect(parseForwardDestinationQuery('@bob')).toEqual({mode: 'user', query: 'bob', resultTypes: ['user']});
		expect(parseForwardDestinationQuery('#gen')).toEqual({
			mode: 'text_channel',
			query: 'gen',
			resultTypes: ['text_channel'],
		});
		expect(parseForwardDestinationQuery('!lounge')).toEqual({
			mode: 'voice_channel',
			query: 'lounge',
			resultTypes: ['voice_channel'],
		});
	});

	it('keeps a bare sigil as a mode with an empty query', () => {
		expect(parseForwardDestinationQuery('#')).toEqual({mode: 'text_channel', query: '', resultTypes: ['text_channel']});
	});

	it('strips the guild, game and global user sigils without narrowing', () => {
		expect(parseForwardDestinationQuery('*guild')).toEqual({mode: null, query: 'guild', resultTypes: ALL_TYPES});
		expect(parseForwardDestinationQuery('$game')).toEqual({mode: null, query: 'game', resultTypes: ALL_TYPES});
		expect(parseForwardDestinationQuery('@@bob')).toEqual({mode: null, query: 'bob', resultTypes: ALL_TYPES});
	});

	it('strips only a leading @ but the first other sigil anywhere', () => {
		expect(parseForwardDestinationQuery('a@b')).toEqual({mode: null, query: 'a@b', resultTypes: ALL_TYPES});
		expect(parseForwardDestinationQuery('foo#bar#baz')).toEqual({
			mode: null,
			query: 'foobar#baz',
			resultTypes: ALL_TYPES,
		});
		expect(parseForwardDestinationQuery('@#x')).toEqual({mode: 'user', query: '#x', resultTypes: ['user']});
	});
});
