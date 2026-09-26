// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createReactionMachineSnapshot,
	emptyMap,
	mapToReactions,
	transitionReactionSnapshot,
} from '@app/features/messaging/state/ReactionStateMachine';
import type {ReactionEmoji} from '@app/features/messaging/utils/ReactionUtils';
import {test} from 'vitest';

const EMOJIS: Array<ReactionEmoji> = Array.from({length: 32}, (_value, index) => ({
	id: index % 3 === 0 ? `emoji-${index}` : undefined,
	name: index % 3 === 0 ? `custom_${index}` : ['🔥', '❤️', '👍', '🎉'][index % 4],
}));

test('ReactionStateMachine benchmarks', async ({bench}) => {
	await bench('apply 1k reaction add/remove transitions for visible messages', () => {
		let snapshot = createReactionMachineSnapshot(emptyMap(), 'me');
		for (let index = 0; index < 1_000; index += 1) {
			const emoji = EMOJIS[index % EMOJIS.length];
			const userId = `user-${index % 250}`;
			snapshot = transitionReactionSnapshot(snapshot, {
				type: 'reaction.add',
				emoji,
				userId,
				isCurrentUser: userId === 'me',
			});
			if (index % 4 === 0) {
				snapshot = transitionReactionSnapshot(snapshot, {
					type: 'reaction.remove',
					emoji,
					userId,
					isCurrentUser: false,
				});
			}
		}
		mapToReactions(snapshot.context.map);
	}).run();

	await bench('hydrate 500-message reaction payload shape', () => {
		let snapshot = createReactionMachineSnapshot(emptyMap(), 'me');
		for (let index = 0; index < 500; index += 1) {
			snapshot = transitionReactionSnapshot(snapshot, {
				type: 'reaction.hydrate',
				currentUserId: 'me',
				reactions: EMOJIS.slice(0, 8).map((emoji, reactionIndex) => ({
					emoji,
					count: 1 + ((index + reactionIndex) % 50),
					me: reactionIndex === index % 8 ? true : undefined,
				})),
			});
		}
		mapToReactions(snapshot.context.map);
	}).run();
});
