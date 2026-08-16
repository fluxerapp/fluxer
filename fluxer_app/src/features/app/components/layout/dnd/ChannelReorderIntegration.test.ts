// SPDX-License-Identifier: AGPL-3.0-or-later

import {type DragItem, DragItemType, type DropResult} from '@app/features/app/components/layout/types/DndTypes';
import {
	type ChannelMoveOperation,
	createChannelMoveOperation,
} from '@app/features/app/components/layout/utils/ChannelMoveOperation';
import type {Channel} from '@app/features/channel/models/Channel';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {computeGuildChannelReorderPlan} from '@fluxer/schema/src/domains/channel/GuildChannelOrdering';
import {describe, expect, it} from 'vitest';
import {
	type ChannelReorderRect,
	type ChannelReorderTarget,
	selectChannelReorderIntent,
} from './ChannelReorderStateMachine';

const rect: ChannelReorderRect = {
	top: 100,
	bottom: 132,
	left: 0,
	right: 240,
};

function point(y: number) {
	return {
		x: 120,
		y,
	};
}

function channel(id: string, type: number, position: number, parentId: string | null = null): Channel {
	return {
		id,
		type,
		position,
		parentId,
	} as Channel;
}

function channelsFixture(): Array<Channel> {
	return [
		channel('root-text', ChannelTypes.GUILD_TEXT, 10),
		channel('category-a', ChannelTypes.GUILD_CATEGORY, 20),
		channel('a-text-1', ChannelTypes.GUILD_TEXT, 30, 'category-a'),
		channel('a-text-2', ChannelTypes.GUILD_TEXT, 40, 'category-a'),
		channel('a-voice-1', ChannelTypes.GUILD_VOICE, 50, 'category-a'),
		channel('a-voice-2', ChannelTypes.GUILD_VOICE, 60, 'category-a'),
		channel('category-b', ChannelTypes.GUILD_CATEGORY, 70),
		channel('b-text-1', ChannelTypes.GUILD_TEXT, 80, 'category-b'),
		channel('b-link-1', ChannelTypes.GUILD_LINK, 90, 'category-b'),
		channel('b-voice-1', ChannelTypes.GUILD_VOICE, 100, 'category-b'),
		channel('root-voice', ChannelTypes.GUILD_VOICE, 110),
	];
}

function requireChannel(channels: ReadonlyArray<Channel>, id: string): Channel {
	const found = channels.find((candidate) => candidate.id === id);
	if (!found) throw new Error(`Missing test channel ${id}`);
	return found;
}

function dragItemFrom(channel: Channel): DragItem {
	return {
		type: channel.type === ChannelTypes.GUILD_CATEGORY ? DragItemType.CATEGORY : DragItemType.CHANNEL,
		id: channel.id,
		channelType: channel.type,
		parentId: channel.parentId,
		guildId: 'guild-1',
	};
}

function targetFrom(channel: Channel): ChannelReorderTarget {
	return {
		id: channel.id,
		channelType: channel.type,
		parentId: channel.parentId,
		guildId: 'guild-1',
	};
}

function requireMachineDropResult(
	channels: ReadonlyArray<Channel>,
	sourceId: string,
	targetId: string,
	y: number,
): DropResult {
	const source = requireChannel(channels, sourceId);
	const target = requireChannel(channels, targetId);
	const intent = selectChannelReorderIntent(dragItemFrom(source), targetFrom(target), point(y), rect);
	if (!intent) throw new Error(`Expected test drop intent from ${sourceId} to ${targetId} at y=${y}`);
	return intent.result;
}

function createOperation(
	channels: ReadonlyArray<Channel>,
	sourceId: string,
	dropResult: DropResult,
): ChannelMoveOperation | null {
	return createChannelMoveOperation({
		channels,
		dragItem: dragItemFrom(requireChannel(channels, sourceId)),
		dropResult,
	});
}

function applyOperation(channels: ReadonlyArray<Channel>, operation: ChannelMoveOperation) {
	const plan = computeGuildChannelReorderPlan({
		channels,
		operation: {
			channelId: operation.channelId,
			parentId: operation.newParentId,
			precedingSiblingId: operation.precedingSiblingId,
		},
	});
	if (!plan.ok) throw new Error(`Unexpected invalid channel reorder plan: ${plan.code}`);
	return plan.plan;
}

function finalIds(channels: ReadonlyArray<Channel>, operation: ChannelMoveOperation): Array<string> {
	return applyOperation(channels, operation).finalChannels.map((ch) => ch.id);
}

function finalParent(
	channels: ReadonlyArray<Channel>,
	operation: ChannelMoveOperation,
	channelId: string,
): string | null {
	return applyOperation(channels, operation).desiredParentById.get(channelId) ?? null;
}

describe('channel reorder decision integration', () => {
	it('turns a bottom-half category drop into a text channel move inside the category before voice siblings', () => {
		const channels = channelsFixture();
		const dropResult = requireMachineDropResult(channels, 'root-text', 'category-a', rect.bottom - 1);
		expect(dropResult).toEqual({
			targetId: 'category-a',
			position: 'inside',
			targetParentId: 'category-a',
		});
		const operation = createOperation(channels, 'root-text', dropResult);
		expect(operation).toEqual({
			channelId: 'root-text',
			newParentId: 'category-a',
			precedingSiblingId: 'a-text-2',
			position: 2,
		});
		expect(finalParent(channels, operation!, 'root-text')).toBe('category-a');
		expect(finalIds(channels, operation!)).toEqual([
			'category-a',
			'a-text-1',
			'a-text-2',
			'root-text',
			'a-voice-1',
			'a-voice-2',
			'category-b',
			'b-text-1',
			'b-link-1',
			'b-voice-1',
			'root-voice',
		]);
	});

	it('turns a bottom-half category drop into a voice channel move after existing voice siblings', () => {
		const channels = channelsFixture();
		const dropResult = requireMachineDropResult(channels, 'root-voice', 'category-a', rect.bottom - 1);
		const operation = createOperation(channels, 'root-voice', dropResult);
		expect(operation).toEqual({
			channelId: 'root-voice',
			newParentId: 'category-a',
			precedingSiblingId: 'a-voice-2',
			position: 4,
		});
		expect(finalParent(channels, operation!, 'root-voice')).toBe('category-a');
		expect(finalIds(channels, operation!)).toEqual([
			'root-text',
			'category-a',
			'a-text-1',
			'a-text-2',
			'a-voice-1',
			'a-voice-2',
			'root-voice',
			'category-b',
			'b-text-1',
			'b-link-1',
			'b-voice-1',
		]);
	});

	it('preserves text-before-voice ordering when a voice channel is dropped before text siblings', () => {
		const channels = channelsFixture();
		const dropResult = requireMachineDropResult(channels, 'a-voice-1', 'a-text-1', rect.top + 1);
		expect(dropResult).toEqual({
			targetId: 'a-text-1',
			position: 'before',
			targetParentId: 'category-a',
		});
		expect(createOperation(channels, 'a-voice-1', dropResult)).toBeNull();
	});

	it('moves a voice channel after another voice sibling inside the same category', () => {
		const channels = channelsFixture();
		const dropResult = requireMachineDropResult(channels, 'a-voice-1', 'a-voice-2', rect.bottom - 1);
		const operation = createOperation(channels, 'a-voice-1', dropResult);
		expect(operation).toEqual({
			channelId: 'a-voice-1',
			newParentId: 'category-a',
			precedingSiblingId: 'a-voice-2',
			position: 3,
		});
		expect(finalIds(channels, operation!)).toEqual([
			'root-text',
			'category-a',
			'a-text-1',
			'a-text-2',
			'a-voice-2',
			'a-voice-1',
			'category-b',
			'b-text-1',
			'b-link-1',
			'b-voice-1',
			'root-voice',
		]);
	});

	it('blocks text and link channel drops onto voice rows before they can produce move operations', () => {
		const channels = channelsFixture();
		const textIntent = selectChannelReorderIntent(
			dragItemFrom(requireChannel(channels, 'a-text-1')),
			targetFrom(requireChannel(channels, 'a-voice-1')),
			point(rect.top + 1),
			rect,
		);
		const linkIntent = selectChannelReorderIntent(
			dragItemFrom(requireChannel(channels, 'b-link-1')),
			targetFrom(requireChannel(channels, 'b-voice-1')),
			point(rect.bottom - 1),
			rect,
		);
		expect(textIntent).toBeNull();
		expect(linkIntent).toBeNull();
	});

	it('moves an entire category block before another category', () => {
		const channels = channelsFixture();
		const dropResult = requireMachineDropResult(channels, 'category-b', 'category-a', rect.top + 1);
		const operation = createOperation(channels, 'category-b', dropResult);
		expect(operation).toEqual({
			channelId: 'category-b',
			newParentId: null,
			precedingSiblingId: 'root-text',
			position: 1,
		});
		expect(finalIds(channels, operation!)).toEqual([
			'root-text',
			'category-b',
			'b-text-1',
			'b-link-1',
			'b-voice-1',
			'category-a',
			'a-text-1',
			'a-text-2',
			'a-voice-1',
			'a-voice-2',
			'root-voice',
		]);
	});

	it('moves an entire category block after another category block from the bottom half', () => {
		const channels = channelsFixture();
		const dropResult = requireMachineDropResult(channels, 'category-a', 'category-b', rect.bottom - 1);
		expect(dropResult).toEqual({
			targetId: 'category-b',
			position: 'after',
			targetParentId: null,
		});
		const operation = createOperation(channels, 'category-a', dropResult);
		expect(operation).toEqual({
			channelId: 'category-a',
			newParentId: null,
			precedingSiblingId: 'category-b',
			position: 2,
		});
		expect(finalIds(channels, operation!)).toEqual([
			'root-text',
			'category-b',
			'b-text-1',
			'b-link-1',
			'b-voice-1',
			'category-a',
			'a-text-1',
			'a-text-2',
			'a-voice-1',
			'a-voice-2',
			'root-voice',
		]);
	});

	it('blocks category drops onto child channels before they can produce move operations', () => {
		const channels = channelsFixture();
		const intent = selectChannelReorderIntent(
			dragItemFrom(requireChannel(channels, 'category-b')),
			targetFrom(requireChannel(channels, 'a-text-1')),
			point(rect.top + 1),
			rect,
		);
		expect(intent).toBeNull();
	});

	it('moves nested channels to the top null-space as root channels', () => {
		const channels = channelsFixture();
		const operation = createOperation(channels, 'b-text-1', {
			targetId: 'null-space',
			position: 'before',
			targetParentId: null,
		});
		expect(operation).toEqual({
			channelId: 'b-text-1',
			newParentId: null,
			precedingSiblingId: null,
			position: 0,
		});
		expect(finalParent(channels, operation!, 'b-text-1')).toBeNull();
		expect(finalIds(channels, operation!)).toEqual([
			'b-text-1',
			'root-text',
			'category-a',
			'a-text-1',
			'a-text-2',
			'a-voice-1',
			'a-voice-2',
			'category-b',
			'b-link-1',
			'b-voice-1',
			'root-voice',
		]);
	});

	it('moves nested channels to trailing space as root channels after the final root sibling', () => {
		const channels = channelsFixture();
		const operation = createOperation(channels, 'a-text-1', {
			targetId: 'trailing-space',
			position: 'after',
			targetParentId: null,
		});
		expect(operation).toEqual({
			channelId: 'a-text-1',
			newParentId: null,
			precedingSiblingId: 'root-voice',
			position: 4,
		});
		expect(finalParent(channels, operation!, 'a-text-1')).toBeNull();
		expect(finalIds(channels, operation!)).toEqual([
			'root-text',
			'category-a',
			'a-text-2',
			'a-voice-1',
			'a-voice-2',
			'category-b',
			'b-text-1',
			'b-link-1',
			'b-voice-1',
			'root-voice',
			'a-text-1',
		]);
	});

	it('does not produce operations for current-position drops', () => {
		const channels = channelsFixture();
		expect(
			createOperation(
				channels,
				'a-text-2',
				requireMachineDropResult(channels, 'a-text-2', 'a-text-1', rect.bottom - 1),
			),
		).toBeNull();
		expect(
			createOperation(
				channels,
				'category-b',
				requireMachineDropResult(channels, 'category-b', 'category-a', rect.bottom - 1),
			),
		).toBeNull();
	});

	it('every machine-produced reorder result is accepted by the move-operation planner or resolves to a no-op', () => {
		const channels = channelsFixture();
		const sources = channels;
		for (const source of sources) {
			for (const target of channels) {
				for (const y of [rect.top + 1, rect.bottom - 1]) {
					const intent = selectChannelReorderIntent(dragItemFrom(source), targetFrom(target), point(y), rect);
					if (!intent) continue;
					const operation = createOperation(channels, source.id, intent.result);
					if (!operation) continue;
					const plan = computeGuildChannelReorderPlan({
						channels,
						operation: {
							channelId: operation.channelId,
							parentId: operation.newParentId,
							precedingSiblingId: operation.precedingSiblingId,
						},
					});
					expect(plan, `${source.id} -> ${target.id} @ ${y}`).toMatchObject({ok: true});
				}
			}
		}
	});
});
