// SPDX-License-Identifier: AGPL-3.0-or-later

import {emptyMap, mapToReactions, transitionReactionMap} from '@app/features/messaging/state/ReactionStateMachine';
import type {ReactionEmoji} from '@app/features/messaging/utils/ReactionUtils';
import {test} from 'vitest';

const EMOJIS: Array<ReactionEmoji> = Array.from({length: 32}, (_value, index) => ({
	id: index % 3 === 0 ? `emoji-${index}` : undefined,
	name: index % 3 === 0 ? `custom_${index}` : ['🔥', '❤️', '👍', '🎉'][index % 4],
}));

test('ReactionStateMachine benchmarks', async ({bench}) => {
	await bench('apply 1k reaction add/remove transitions for visible messages', () => {
		let map = emptyMap();
		for (let index = 0; index < 1_000; index += 1) {
			const emoji = EMOJIS[index % EMOJIS.length];
			const userId = `user-${index % 250}`;
			map = transitionReactionMap(
				map,
				{
					type: 'reaction.add',
					emoji,
					userId,
					isCurrentUser: userId === 'me',
				},
				'me',
			);
			if (index % 4 === 0) {
				map = transitionReactionMap(
					map,
					{
						type: 'reaction.remove',
						emoji,
						userId,
						isCurrentUser: false,
					},
					'me',
				);
			}
		}
		mapToReactions(map);
	}).run();

	await bench('hydrate 500-message reaction payload shape', () => {
		let map = emptyMap();
		for (let index = 0; index < 500; index += 1) {
			map = transitionReactionMap(
				map,
				{
					type: 'reaction.hydrate',
					currentUserId: 'me',
					reactions: EMOJIS.slice(0, 8).map((emoji, reactionIndex) => ({
						emoji,
						count: 1 + ((index + reactionIndex) % 50),
						me: reactionIndex === index % 8 ? true : undefined,
					})),
				},
				'me',
			);
		}
		mapToReactions(map);
	}).run();
});
