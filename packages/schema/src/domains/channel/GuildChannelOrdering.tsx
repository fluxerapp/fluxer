/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';

export interface ChannelOrderingChannel<Id extends string | bigint = string> {
	id: Id;
	parentId?: Id | null | undefined;
	type: number;
	position?: number | null | undefined;
}

export type GuildChannelReorderErrorCode =
	| 'TARGET_CHANNEL_NOT_FOUND'
	| 'PRECEDING_CHANNEL_NOT_FOUND'
	| 'CANNOT_POSITION_RELATIVE_TO_SELF_BLOCK'
	| 'PRECEDING_PARENT_MISMATCH'
	| 'PARENT_NOT_FOUND'
	| 'PARENT_NOT_CATEGORY'
	| 'CATEGORIES_CANNOT_HAVE_PARENTS'
	| 'PARENT_NOT_IN_GUILD_LIST'
	| 'PRECEDING_NOT_IN_GUILD_LIST';

export interface GuildChannelReorderOperation<Id extends string | bigint> {
	channelId: Id;
	parentId: Id | null | undefined;
	precedingSiblingId: Id | null | undefined;
}

export interface GuildChannelReorderPlan<Id extends string | bigint, Channel extends ChannelOrderingChannel<Id>> {
	orderedChannels: Array<Channel>;
	finalChannels: Array<Channel>;
	desiredParentById: Map<Id, Id | null>;
	orderUnchanged: boolean;
}

function idToString<Id extends string | bigint>(id: Id): string {
	return String(id);
}

export function compareChannelOrdering<Id extends string | bigint>(
	a: ChannelOrderingChannel<Id>,
	b: ChannelOrderingChannel<Id>,
): number {
	const aPos = a.position ?? 0;
	const bPos = b.position ?? 0;
	if (aPos !== bPos) return aPos - bPos;
	return idToString(a.id).localeCompare(idToString(b.id));
}

export function sortChannelsForOrdering<Id extends string | bigint, Channel extends ChannelOrderingChannel<Id>>(
	channels: ReadonlyArray<Channel>,
): Array<Channel> {
	const channelById = new Map<Id, Channel>(channels.map((channel) => [channel.id, channel]));
	const childrenByParent = new Map<Id, Array<Channel>>();
	const rootChannels: Array<Channel> = [];

	for (const channel of channels) {
		const parentId = channel.parentId ?? null;
		if (parentId === null || !channelById.has(parentId)) {
			rootChannels.push(channel);
			continue;
		}

		const existingChildren = childrenByParent.get(parentId);
		if (existingChildren) {
			existingChildren.push(channel);
		} else {
			childrenByParent.set(parentId, [channel]);
		}
	}

	const orderedChannels: Array<Channel> = [];
	const seen = new Set<Id>();

	const sortedRoots = [...rootChannels].sort(compareChannelOrdering);
	for (const root of sortedRoots) {
		orderedChannels.push(root);
		seen.add(root.id);

		if (root.type !== ChannelTypes.GUILD_CATEGORY) {
			continue;
		}

		const children = childrenByParent.get(root.id);
		if (!children) {
			continue;
		}

		for (const child of [...children].sort(compareChannelOrdering)) {
			orderedChannels.push(child);
			seen.add(child.id);
		}
	}

	const remaining = channels.filter((channel) => !seen.has(channel.id)).sort(compareChannelOrdering);
	orderedChannels.push(...remaining);

	return orderedChannels;
}

export function computeChannelMoveBlockIds<Id extends string | bigint, Channel extends ChannelOrderingChannel<Id>>({
	channels,
	targetId,
}: {
	channels: ReadonlyArray<Channel>;
	targetId: Id;
}): Set<Id> {
	const channelById = new Map<Id, Channel>(channels.map((ch) => [ch.id, ch]));
	const target = channelById.get(targetId);

	const blockIds = new Set<Id>();
	blockIds.add(targetId);

	if (target?.type === ChannelTypes.GUILD_CATEGORY) {
		for (const channel of channels) {
			if (channel.parentId === targetId) {
				blockIds.add(channel.id);
			}
		}
	}

	return blockIds;
}

export function findCategorySpanInOrderedList<Id extends string | bigint, Channel extends ChannelOrderingChannel<Id>>(
	orderedChannels: ReadonlyArray<Channel>,
	categoryId: Id,
): {start: number; end: number} {
	const start = orderedChannels.findIndex((ch) => ch.id === categoryId);
	if (start === -1) return {start: -1, end: -1};
	let end = start + 1;
	while (end < orderedChannels.length && orderedChannels[end].parentId === categoryId) {
		end++;
	}
	return {start, end};
}

export function computePrecedingSiblingIdFromPosition<
	Id extends string | bigint,
	Channel extends ChannelOrderingChannel<Id>,
>({
	channels,
	targetId,
	desiredParentId,
	position,
}: {
	channels: ReadonlyArray<Channel>;
	targetId: Id;
	desiredParentId: Id | null;
	position: number;
}): Id | null {
	const siblings = sortChannelsForOrdering(channels).filter((ch) => (ch.parentId ?? null) === desiredParentId);
	const blockIds = computeChannelMoveBlockIds({channels, targetId});
	const siblingsWithoutBlock = siblings.filter((ch) => !blockIds.has(ch.id));

	const clamped = Math.min(Math.max(position, 0), siblingsWithoutBlock.length);
	return clamped === 0 ? null : siblingsWithoutBlock[clamped - 1]!.id;
}

export function computePositionFromPrecedingSiblingId<
	Id extends string | bigint,
	Channel extends ChannelOrderingChannel<Id>,
>({
	channels,
	targetId,
	desiredParentId,
	precedingSiblingId,
}: {
	channels: ReadonlyArray<Channel>;
	targetId: Id;
	desiredParentId: Id | null;
	precedingSiblingId: Id | null;
}): number | null {
	const siblings = sortChannelsForOrdering(channels).filter((ch) => (ch.parentId ?? null) === desiredParentId);
	const blockIds = computeChannelMoveBlockIds({channels, targetId});
	const siblingsWithoutBlock = siblings.filter((ch) => !blockIds.has(ch.id));

	if (!precedingSiblingId) return 0;
	const index = siblingsWithoutBlock.findIndex((ch) => ch.id === precedingSiblingId);
	if (index === -1) return null;
	return index + 1;
}

export function computeGuildChannelReorderPlan<Id extends string | bigint, Channel extends ChannelOrderingChannel<Id>>({
	channels,
	operation,
}: {
	channels: ReadonlyArray<Channel>;
	operation: GuildChannelReorderOperation<Id>;
}): {ok: true; plan: GuildChannelReorderPlan<Id, Channel>} | {ok: false; code: GuildChannelReorderErrorCode} {
	const orderedChannels = sortChannelsForOrdering(channels);
	const channelById = new Map<Id, Channel>(orderedChannels.map((ch) => [ch.id, ch]));

	const targetChannel = channelById.get(operation.channelId);
	if (!targetChannel) {
		return {ok: false, code: 'TARGET_CHANNEL_NOT_FOUND'};
	}

	const requestedParentId = operation.parentId;
	const desiredParentId =
		targetChannel.type === ChannelTypes.GUILD_CATEGORY
			? null
			: requestedParentId !== undefined
				? requestedParentId
				: (targetChannel.parentId ?? null);

	if (targetChannel.type === ChannelTypes.GUILD_CATEGORY && operation.parentId) {
		return {ok: false, code: 'CATEGORIES_CANNOT_HAVE_PARENTS'};
	}

	if (desiredParentId) {
		const parentChannel = channelById.get(desiredParentId);
		if (!parentChannel) {
			return {ok: false, code: 'PARENT_NOT_FOUND'};
		}
		if (parentChannel.type !== ChannelTypes.GUILD_CATEGORY) {
			return {ok: false, code: 'PARENT_NOT_CATEGORY'};
		}
	}

	const precedingId = operation.precedingSiblingId ?? null;
	if (precedingId && !channelById.has(precedingId)) {
		return {ok: false, code: 'PRECEDING_CHANNEL_NOT_FOUND'};
	}

	const blockIds = computeChannelMoveBlockIds({channels: orderedChannels, targetId: targetChannel.id});
	if (precedingId && blockIds.has(precedingId)) {
		return {ok: false, code: 'CANNOT_POSITION_RELATIVE_TO_SELF_BLOCK'};
	}

	const remainingChannels = orderedChannels.filter((ch) => !blockIds.has(ch.id));
	const blockChannels = orderedChannels.filter((ch) => blockIds.has(ch.id));

	const expectedParent = desiredParentId ?? null;
	if (precedingId) {
		const precedingChannel = channelById.get(precedingId)!;
		const precedingParent = precedingChannel.parentId ?? null;
		if (precedingParent !== expectedParent) {
			return {ok: false, code: 'PRECEDING_PARENT_MISMATCH'};
		}
	}

	let insertIndex = 0;
	if (precedingId) {
		const precedingIndex = remainingChannels.findIndex((ch) => ch.id === precedingId);
		if (precedingIndex === -1) {
			return {ok: false, code: 'PRECEDING_NOT_IN_GUILD_LIST'};
		}
		const precedingChannel = channelById.get(precedingId)!;
		if (precedingChannel.type === ChannelTypes.GUILD_CATEGORY) {
			const span = findCategorySpanInOrderedList(remainingChannels, precedingChannel.id);
			insertIndex = span.end;
		} else {
			insertIndex = precedingIndex + 1;
		}
	} else if (desiredParentId) {
		const parentIndex = remainingChannels.findIndex((ch) => ch.id === desiredParentId);
		if (parentIndex === -1) {
			return {ok: false, code: 'PARENT_NOT_IN_GUILD_LIST'};
		}
		insertIndex = parentIndex + 1;
	} else {
		insertIndex = 0;
	}

	const finalChannels = [...remainingChannels];
	finalChannels.splice(insertIndex, 0, ...blockChannels);

	const desiredParentById = new Map<Id, Id | null>();
	for (const channel of finalChannels) {
		if (channel.id === targetChannel.id) {
			desiredParentById.set(channel.id, desiredParentId ?? null);
		} else {
			desiredParentById.set(channel.id, channel.parentId ?? null);
		}
	}

	const orderUnchanged =
		finalChannels.length === orderedChannels.length &&
		finalChannels.every((channel, index) => channel.id === orderedChannels[index]!.id) &&
		(targetChannel.parentId ?? null) === (desiredParentById.get(targetChannel.id) ?? null);

	return {
		ok: true,
		plan: {
			orderedChannels,
			finalChannels,
			desiredParentById,
			orderUnchanged,
		},
	};
}
