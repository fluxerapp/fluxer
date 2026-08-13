// SPDX-License-Identifier: AGPL-3.0-or-later

export type TriggerType =
	| 'mention'
	| 'channel'
	| 'emoji'
	| 'emojiReaction'
	| 'command'
	| 'meme'
	| 'gif'
	| 'sticker'
	| 'commandArgMention'
	| 'commandArg';

export function isAutocompleteTriggerAllowed(
	triggerType: TriggerType,
	allowedTriggers: ReadonlyArray<TriggerType> | undefined,
): boolean {
	return allowedTriggers == null || allowedTriggers.includes(triggerType);
}
