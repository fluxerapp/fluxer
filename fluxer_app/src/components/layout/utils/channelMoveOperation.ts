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

import {ChannelTypes} from '~/Constants';
import type {ChannelRecord} from '~/records/ChannelRecord';
import * as ChannelUtils from '~/utils/ChannelUtils';
import type {DragItem, DropResult} from '../types/dnd';

const isTextChannel = (channel: ChannelRecord) =>
	channel.type === ChannelTypes.GUILD_TEXT || channel.type === ChannelTypes.GUILD_LINK;
const isCategoryChannel = (channel: ChannelRecord) => channel.type === ChannelTypes.GUILD_CATEGORY;

const gatherCategoryBlock = (channels: ReadonlyArray<ChannelRecord>, categoryId: string) => {
	return channels.filter((ch) => ch.id === categoryId || ch.parentId === categoryId);
};

const filterOutCategoryBlock = (channels: ReadonlyArray<ChannelRecord>, categoryId: string) => {
	return channels.filter((ch) => ch.id !== categoryId && ch.parentId !== categoryId);
};

const findCategorySpan = (channels: ReadonlyArray<ChannelRecord>, categoryId: string) => {
	const startIndex = channels.findIndex((ch) => ch.id === categoryId);
	if (startIndex === -1) return {start: -1, end: -1};
	let endIndex = startIndex + 1;
	while (endIndex < channels.length && channels[endIndex].parentId === categoryId) {
		endIndex++;
	}
	return {start: startIndex, end: endIndex};
};

const findCurrentPreceding = (channels: ReadonlyArray<ChannelRecord>, channel: ChannelRecord): string | null => {
	const index = channels.findIndex((ch) => ch.id === channel.id);
	if (index <= 0) return null;

	for (let i = index - 1; i >= 0; i--) {
		const candidate = channels[i];
		const candidateParent = candidate.parentId ?? null;
		const channelParent = channel.parentId ?? null;
		if (candidateParent === channelParent) {
			return candidate.id;
		}
	}

	return null;
};

export interface ChannelMoveOperation {
	channelId: string;
	newParentId: string | null;
	precedingSiblingId: string | null;
	position: number;
}

export const createChannelMoveOperation = ({
	channels,
	dragItem,
	dropResult,
}: {
	channels: ReadonlyArray<ChannelRecord>;
	dragItem: DragItem;
	dropResult: DropResult;
}): ChannelMoveOperation | null => {
	const draggedChannel = channels.find((ch) => ch.id === dragItem.id);
	if (!draggedChannel) return null;

	const orderedChannels = [...channels].sort(ChannelUtils.compareChannels);
	const isCategory = isCategoryChannel(draggedChannel);

	const baseList = isCategory
		? filterOutCategoryBlock(orderedChannels, draggedChannel.id)
		: orderedChannels.filter((ch) => ch.id !== draggedChannel.id);

	const block = isCategory ? gatherCategoryBlock(orderedChannels, draggedChannel.id) : [draggedChannel];
	if (block.length === 0) return null;

	const targetId = dropResult.targetId;
	const requestedParentId =
		targetId === 'null-space'
			? null
			: dropResult.targetParentId !== undefined
				? dropResult.targetParentId
				: isCategory
					? null
					: (draggedChannel.parentId ?? null);

	let newParentId = isCategory ? null : requestedParentId;
	if (!isCategory && newParentId === undefined) {
		newParentId = draggedChannel.parentId ?? null;
	}

	if (isCategory) {
		newParentId = null;
	}

	let insertIndex = 0;
	if (targetId === 'null-space') {
		insertIndex = 0;
		newParentId = null;
	} else if (targetId === 'trailing-space') {
		insertIndex = baseList.length;
		newParentId = null;
	} else {
		const targetIndex = baseList.findIndex((ch) => ch.id === targetId);
		if (targetIndex === -1) return null;
		const targetChannel = baseList[targetIndex];

		if (dropResult.position === 'before') {
			insertIndex = targetIndex;
		} else if (dropResult.position === 'after') {
			if (isCategoryChannel(targetChannel)) {
				const span = findCategorySpan(baseList, targetChannel.id);
				insertIndex = span.end;
			} else {
				insertIndex = targetIndex + 1;
			}
		} else if (dropResult.position === 'inside') {
			if (!isCategoryChannel(targetChannel)) {
				return null;
			}
			const span = findCategorySpan(baseList, targetChannel.id);
			insertIndex = span.end;
			newParentId = targetChannel.id;
		}
	}

	if (draggedChannel.type === ChannelTypes.GUILD_VOICE && newParentId) {
		const siblingIndices = baseList.reduce<Array<{index: number; channel: ChannelRecord}>>((acc, ch, index) => {
			if (ch.parentId === newParentId) {
				acc.push({index, channel: ch});
			}
			return acc;
		}, []);

		const lastTextSibling = siblingIndices
			.filter(({channel}) => isTextChannel(channel))
			.reduce<number>((max, {index}) => Math.max(max, index), -1);

		const categoryIndex = baseList.findIndex((ch) => ch.id === newParentId);
		const minimumIndex = lastTextSibling >= 0 ? lastTextSibling + 1 : categoryIndex + 1;
		if (minimumIndex > insertIndex) {
			insertIndex = minimumIndex;
		}
	}

	const finalList = [...baseList];
	finalList.splice(insertIndex, 0, ...block);

	const insertedIndex = finalList.findIndex((ch) => ch.id === draggedChannel.id);
	if (insertedIndex === -1) return null;

	let localPosition = 0;
	for (let i = 0; i < insertedIndex; i++) {
		const candidate = finalList[i];
		const candidateParent = candidate.parentId ?? null;
		if (candidateParent === (newParentId ?? null)) {
			localPosition++;
		}
	}

	let precedingSiblingId: string | null = null;
	for (let i = insertedIndex - 1; i >= 0; i--) {
		const candidate = finalList[i];
		const candidateParent = candidate.parentId ?? null;
		if (isCategory) {
			if (candidateParent === null) {
				precedingSiblingId = candidate.id;
				break;
			}
		} else if (candidateParent === (newParentId ?? null)) {
			precedingSiblingId = candidate.id;
			break;
		}
	}

	const currentPreceding = findCurrentPreceding(orderedChannels, draggedChannel);
	const currentParentId = draggedChannel.parentId ?? null;

	if (currentParentId === (newParentId ?? null) && currentPreceding === precedingSiblingId) {
		return null;
	}

	return {
		channelId: draggedChannel.id,
		newParentId: newParentId ?? null,
		precedingSiblingId,
		position: localPosition,
	};
};
