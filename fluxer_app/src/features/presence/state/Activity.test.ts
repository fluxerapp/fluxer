// SPDX-License-Identifier: AGPL-3.0-or-later

import {ActivityTypes, activitiesKey, normalizeActivities} from '@app/features/presence/state/Activity';
import {describe, expect, it} from 'vitest';

describe('normalizeActivities', () => {
	it('keeps supported activity data and clamps unknown types to playing', () => {
		expect(
			normalizeActivities([
				{
					name: '  Spotify ',
					type: ActivityTypes.LISTENING,
					details: 'Track',
					state: 'Artist',
					timestamps: {start: 1_700_000_000_500, end: 1_700_000_120_900},
				},
				{name: 'Game', type: 99},
			]),
		).toEqual([
			{
				name: 'Spotify',
				type: ActivityTypes.LISTENING,
				details: 'Track',
				state: 'Artist',
				timestamps: {start: 1_700_000_000_500, end: 1_700_000_120_900},
			},
			{name: 'Game', type: ActivityTypes.PLAYING},
		]);
	});

	it('drops malformed values and limits the list to five activities', () => {
		const activities = normalizeActivities([
			null,
			{name: ''},
			{name: 'A'.repeat(129)},
			...Array.from({length: 7}, (_value, index) => ({name: `Game ${index}`, type: ActivityTypes.PLAYING})),
		]);
		expect(activities.map((activity) => activity.name)).toEqual(['Game 0', 'Game 1', 'Game 2', 'Game 3', 'Game 4']);
	});

	it('omits invalid timestamps without dropping the activity', () => {
		expect(
			normalizeActivities([
				{name: 'VLC', type: ActivityTypes.WATCHING, timestamps: {start: -1, end: Number.POSITIVE_INFINITY}},
			]),
		).toEqual([{name: 'VLC', type: ActivityTypes.WATCHING}]);
	});
});

describe('activitiesKey', () => {
	it('changes when a timestamp changes', () => {
		const first = [{name: 'Game', type: ActivityTypes.PLAYING, timestamps: {start: 100}}];
		const second = [{name: 'Game', type: ActivityTypes.PLAYING, timestamps: {start: 101}}];
		expect(activitiesKey(first)).not.toBe(activitiesKey(second));
	});
});
