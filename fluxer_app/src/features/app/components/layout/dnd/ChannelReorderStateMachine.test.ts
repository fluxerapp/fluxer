// SPDX-License-Identifier: AGPL-3.0-or-later

import {type DragItem, DragItemType} from '@app/features/app/components/layout/types/DndTypes';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it} from 'vitest';
import {
	type ChannelReorderRect,
	type ChannelReorderTarget,
	canChannelDropOnTarget,
	createChannelReorderSnapshot,
	getChannelReorderStateValue,
	resolveChannelReorderHover,
	selectChannelReorderIntent,
	selectChannelReorderResolution,
	transitionChannelReorderSnapshot,
} from './ChannelReorderStateMachine';

const rect: ChannelReorderRect = {
	top: 40,
	bottom: 72,
	left: 0,
	right: 240,
};

function point(y: number) {
	return {
		x: 80,
		y,
	};
}

function channelItem(
	id: string,
	channelType: number = ChannelTypes.GUILD_TEXT,
	parentId: string | null = null,
): DragItem {
	return {
		type: DragItemType.CHANNEL,
		id,
		channelType,
		parentId,
		guildId: 'guild-1',
	};
}

function categoryItem(id = 'category-source'): DragItem {
	return {
		type: DragItemType.CATEGORY,
		id,
		channelType: ChannelTypes.GUILD_CATEGORY,
		parentId: null,
		guildId: 'guild-1',
	};
}

function voiceParticipantItem(id = 'user-1', currentChannelId = 'voice-source'): DragItem {
	return {
		type: DragItemType.VOICE_PARTICIPANT,
		id,
		channelType: ChannelTypes.GUILD_VOICE,
		parentId: null,
		guildId: 'guild-1',
		userId: id,
		currentChannelId,
	};
}

function target(
	id: string,
	channelType: number = ChannelTypes.GUILD_TEXT,
	parentId: string | null = null,
): ChannelReorderTarget {
	return {
		id,
		channelType,
		parentId,
		guildId: 'guild-1',
	};
}

describe('ChannelReorderStateMachine', () => {
	it('starts idle and targets a valid channel hover', () => {
		let snapshot = createChannelReorderSnapshot();
		expect(getChannelReorderStateValue(snapshot)).toBe('idle');
		snapshot = transitionChannelReorderSnapshot(snapshot, {
			type: 'drag.hover',
			item: channelItem('source'),
			target: target('target'),
			clientOffset: point(42),
			targetRect: rect,
		});
		expect(getChannelReorderStateValue(snapshot)).toBe('targeting');
		expect(snapshot.context.intent?.indicator).toEqual({position: 'top', isValid: true});
		expect(snapshot.context.intent?.result).toEqual({
			targetId: 'target',
			position: 'before',
			targetParentId: null,
		});
	});

	it('clears targeting state on drop', () => {
		let snapshot = createChannelReorderSnapshot();
		snapshot = transitionChannelReorderSnapshot(snapshot, {
			type: 'drag.hover',
			item: channelItem('source'),
			target: target('target'),
			clientOffset: point(42),
			targetRect: rect,
		});
		snapshot = transitionChannelReorderSnapshot(snapshot, {type: 'drag.drop'});
		expect(getChannelReorderStateValue(snapshot)).toBe('idle');
		expect(snapshot.context.intent).toBeNull();
		expect(snapshot.context.indicator).toBeNull();
	});

	it('blocks dragging an item onto itself and clears on leave', () => {
		let snapshot = createChannelReorderSnapshot();
		snapshot = transitionChannelReorderSnapshot(snapshot, {
			type: 'drag.hover',
			item: channelItem('same'),
			target: target('same'),
			clientOffset: point(42),
			targetRect: rect,
		});
		expect(getChannelReorderStateValue(snapshot)).toBe('blocked');
		expect(snapshot.context.blockedReason).toBe('same-source-and-target');
		expect(snapshot.context.indicator).toEqual({position: 'top', isValid: false});
		snapshot = transitionChannelReorderSnapshot(snapshot, {type: 'drag.leave'});
		expect(getChannelReorderStateValue(snapshot)).toBe('idle');
		expect(snapshot.context.blockedReason).toBeNull();
	});

	it('covers every pixel of a valid regular channel target with a concrete reorder result', () => {
		const source = channelItem('source');
		const destination = target('target', ChannelTypes.GUILD_TEXT, 'category-1');
		for (let y = rect.top; y <= rect.bottom; y++) {
			const intent = selectChannelReorderIntent(source, destination, point(y), rect);
			expect(intent, `expected intent for y=${y}`).not.toBeNull();
			expect(['before', 'after']).toContain(intent?.result.position);
			expect(intent?.result.targetParentId).toBe('category-1');
		}
	});

	it('maps regular channel top and bottom halves to before and after', () => {
		const source = channelItem('source');
		const destination = target('target', ChannelTypes.GUILD_TEXT, 'category-1');
		expect(selectChannelReorderIntent(source, destination, point(41), rect)).toMatchObject({
			indicator: {position: 'top', isValid: true},
			result: {
				targetId: 'target',
				position: 'before',
				targetParentId: 'category-1',
			},
		});
		expect(selectChannelReorderIntent(source, destination, point(71), rect)).toMatchObject({
			indicator: {position: 'bottom', isValid: true},
			result: {
				targetId: 'target',
				position: 'after',
				targetParentId: 'category-1',
			},
		});
	});

	it('maps category top half to before-category and bottom half to inside-category', () => {
		const source = channelItem('source');
		const categoryTarget = target('category-1', ChannelTypes.GUILD_CATEGORY, null);
		expect(selectChannelReorderIntent(source, categoryTarget, point(41), rect)).toMatchObject({
			indicator: {position: 'top', isValid: true},
			result: {
				targetId: 'category-1',
				position: 'before',
				targetParentId: null,
			},
		});
		expect(selectChannelReorderIntent(source, categoryTarget, point(71), rect)).toMatchObject({
			indicator: {position: 'bottom', isValid: true},
			result: {
				targetId: 'category-1',
				position: 'inside',
				targetParentId: 'category-1',
			},
		});
	});

	it('allows voice channels to target text, voice, and category rows like the current move operation expects', () => {
		const source = channelItem('voice-source', ChannelTypes.GUILD_VOICE, 'category-1');
		expect(canChannelDropOnTarget(source, target('text-target', ChannelTypes.GUILD_TEXT, 'category-1'))).toBe(true);
		expect(canChannelDropOnTarget(source, target('voice-target', ChannelTypes.GUILD_VOICE, 'category-1'))).toBe(true);
		expect(canChannelDropOnTarget(source, target('category-1', ChannelTypes.GUILD_CATEGORY, null))).toBe(true);
	});

	it('blocks text channels from targeting voice rows and returns an invalid indicator', () => {
		const source = channelItem('text-source', ChannelTypes.GUILD_TEXT, 'category-1');
		const destination = target('voice-target', ChannelTypes.GUILD_VOICE, 'category-1');
		expect(canChannelDropOnTarget(source, destination)).toBe(false);
		expect(resolveChannelReorderHover(source, destination, point(71), rect)).toMatchObject({
			intent: null,
			indicator: {position: 'bottom', isValid: false},
			blockedReason: 'incompatible-channel-kind',
			isVoiceParticipantTransfer: false,
		});
	});

	it('blocks dragging a category onto a child channel', () => {
		const source = categoryItem();
		const childTarget = target('child', ChannelTypes.GUILD_TEXT, 'category-1');
		expect(canChannelDropOnTarget(source, childTarget)).toBe(false);
		expect(resolveChannelReorderHover(source, childTarget, point(41), rect)).toMatchObject({
			intent: null,
			indicator: {position: 'top', isValid: false},
			blockedReason: 'category-into-child-channel',
		});
	});

	it('allows dragging a category before or after a top-level channel target', () => {
		const source = categoryItem();
		const topLevelTarget = target('root-channel', ChannelTypes.GUILD_TEXT, null);
		expect(selectChannelReorderIntent(source, topLevelTarget, point(41), rect)).toMatchObject({
			result: {
				targetId: 'root-channel',
				position: 'before',
				targetParentId: null,
			},
		});
		expect(selectChannelReorderIntent(source, topLevelTarget, point(71), rect)).toMatchObject({
			result: {
				targetId: 'root-channel',
				position: 'after',
				targetParentId: null,
			},
		});
	});

	it('maps category-to-category bottom drops to after instead of inside', () => {
		const source = categoryItem('category-source');
		const categoryTarget = target('category-target', ChannelTypes.GUILD_CATEGORY, null);
		expect(selectChannelReorderIntent(source, categoryTarget, point(71), rect)).toMatchObject({
			indicator: {position: 'bottom', isValid: true},
			result: {
				targetId: 'category-target',
				position: 'after',
				targetParentId: null,
			},
		});
	});

	it('treats voice participant drops onto voice channels as transfers, not reorder intents', () => {
		const resolution = selectChannelReorderResolution(
			voiceParticipantItem(),
			target('voice-target', ChannelTypes.GUILD_VOICE, 'category-1'),
			point(41),
			rect,
		);
		expect(resolution).toEqual({
			intent: null,
			indicator: null,
			blockedReason: null,
			isVoiceParticipantTransfer: true,
		});
		let snapshot = createChannelReorderSnapshot();
		snapshot = transitionChannelReorderSnapshot(snapshot, {
			type: 'drag.hover',
			item: voiceParticipantItem(),
			target: target('voice-target', ChannelTypes.GUILD_VOICE, 'category-1'),
			clientOffset: point(41),
			targetRect: rect,
		});
		expect(getChannelReorderStateValue(snapshot)).toBe('voiceParticipantTransfer');
	});

	it('blocks voice participant drops onto non-voice channels with an invalid indicator', () => {
		const resolution = selectChannelReorderResolution(
			voiceParticipantItem(),
			target('text-target', ChannelTypes.GUILD_TEXT, 'category-1'),
			point(71),
			rect,
		);
		expect(resolution).toMatchObject({
			intent: null,
			indicator: {position: 'bottom', isValid: false},
			blockedReason: 'incompatible-channel-kind',
			isVoiceParticipantTransfer: false,
		});
	});

	it('blocks zero-height target rects without manufacturing a drop result', () => {
		const emptyRect = {...rect, bottom: rect.top};
		expect(resolveChannelReorderHover(channelItem('source'), target('target'), point(rect.top), emptyRect)).toEqual({
			intent: null,
			indicator: null,
			blockedReason: 'empty-target-rect',
			isVoiceParticipantTransfer: false,
		});
	});
});
