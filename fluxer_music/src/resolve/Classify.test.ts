// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, test} from 'vitest';
import {type Classified, classifyInput} from './Classify';

const cases: Array<[string, Classified]> = [
	['https://www.youtube.com/watch?v=abc', {kind: 'youtube-video', url: 'https://www.youtube.com/watch?v=abc'}],
	['https://youtube.com/playlist?list=PL1', {kind: 'youtube-playlist', url: 'https://youtube.com/playlist?list=PL1'}],
	[
		'https://www.youtube.com/watch?v=abc&list=PL1',
		{kind: 'youtube-video', url: 'https://www.youtube.com/watch?v=abc&list=PL1'},
	],
	['https://youtu.be/abc', {kind: 'youtube-video', url: 'https://youtu.be/abc'}],
	['https://music.youtube.com/watch?v=abc', {kind: 'youtube-video', url: 'https://music.youtube.com/watch?v=abc'}],
	['https://open.spotify.com/playlist/37i9', {kind: 'spotify', type: 'playlist', id: '37i9'}],
	['https://open.spotify.com/intl-de/track/xyz', {kind: 'spotify', type: 'track', id: 'xyz'}],
	['https://evil.example/watch?v=abc', {kind: 'search', query: 'https://evil.example/watch?v=abc'}],
	['https://youtube.com.evil.example/watch?v=a', {kind: 'search', query: 'https://youtube.com.evil.example/watch?v=a'}],
	['file:///etc/passwd', {kind: 'search', query: 'file:///etc/passwd'}],
	['  never gonna give you up ', {kind: 'search', query: 'never gonna give you up'}],
];

describe('classifyInput', () => {
	test.each(cases)('%s', (input, expected) => {
		expect(classifyInput(input)).toEqual(expected);
	});
});
