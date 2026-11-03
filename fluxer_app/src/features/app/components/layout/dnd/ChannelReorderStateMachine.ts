// SPDX-License-Identifier: AGPL-3.0-or-later

import {type DragItem, DragItemType, type DropResult} from '@app/features/app/components/layout/types/DndTypes';
import {getVerticalDropTargetHeight, resolveVerticalDropEdge} from '@app/features/ui/dnd/DropGeometry';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';

export interface ChannelReorderPoint {
	x: number;
	y: number;
}

export interface ChannelReorderRect {
	top: number;
	bottom: number;
	left: number;
	right: number;
}

export interface ChannelReorderTarget {
	id: string;
	channelType: number;
	parentId: string | null;
	guildId: string;
}

interface ChannelReorderIndicator {
	position: 'top' | 'bottom';
	isValid: boolean;
}

interface ChannelReorderIntent {
	indicator: ChannelReorderIndicator;
	result: DropResult;
}

type ChannelReorderBlockedReason =
	| 'same-source-and-target'
	| 'incompatible-channel-kind'
	| 'category-into-child-channel'
	| 'empty-target-rect'
	| 'unsupported-drag-item';

export interface ChannelReorderResolution {
	intent: ChannelReorderIntent | null;
	indicator: ChannelReorderIndicator | null;
	blockedReason: ChannelReorderBlockedReason | null;
	isVoiceParticipantTransfer: boolean;
}

function isCategoryType(channelType: number): boolean {
	return channelType === ChannelTypes.GUILD_CATEGORY;
}

function isVoiceType(channelType: number): boolean {
	return channelType === ChannelTypes.GUILD_VOICE;
}

function isTextType(channelType: number): boolean {
	return (
		channelType === ChannelTypes.GUILD_TEXT ||
		channelType === ChannelTypes.GUILD_ANNOUNCEMENT ||
		channelType === ChannelTypes.GUILD_LINK ||
		channelType === ChannelTypes.GUILD_FORUM ||
		channelType === ChannelTypes.GUILD_MEDIA
	);
}

function createIndicator(
	clientOffset: ChannelReorderPoint,
	rect: ChannelReorderRect,
	isValid: boolean,
): ChannelReorderIndicator {
	return {
		position: resolveVerticalDropEdge(clientOffset, rect) === 'before' ? 'top' : 'bottom',
		isValid,
	};
}

function isReorderDragItem(item: DragItem): boolean {
	return item.type === DragItemType.CHANNEL || item.type === DragItemType.CATEGORY;
}

function getChannelDropBlockedReason(item: DragItem, target: ChannelReorderTarget): ChannelReorderBlockedReason | null {
	if (item.id === target.id) return 'same-source-and-target';
	if (item.type === DragItemType.VOICE_PARTICIPANT) {
		return isVoiceType(target.channelType) ? null : 'incompatible-channel-kind';
	}
	if (!isReorderDragItem(item)) return 'unsupported-drag-item';
	const targetIsCategory = isCategoryType(target.channelType);
	const targetIsVoice = isVoiceType(target.channelType);
	const targetIsText = isTextType(target.channelType);
	if (item.type === DragItemType.CHANNEL) {
		if (item.channelType === ChannelTypes.GUILD_VOICE) {
			if (!targetIsCategory && !targetIsVoice && !targetIsText) return 'incompatible-channel-kind';
		} else if (targetIsVoice) {
			return 'incompatible-channel-kind';
		}
	}
	if (item.type === DragItemType.CATEGORY && target.parentId !== null && !targetIsCategory) {
		return 'category-into-child-channel';
	}
	return null;
}

export function canChannelDropOnTarget(item: DragItem, target: ChannelReorderTarget): boolean {
	return getChannelDropBlockedReason(item, target) === null;
}

export function selectChannelReorderResolution(
	item: DragItem,
	target: ChannelReorderTarget,
	clientOffset: ChannelReorderPoint,
	targetRect: ChannelReorderRect,
): ChannelReorderResolution {
	if (getVerticalDropTargetHeight(targetRect) <= 0) {
		return {
			intent: null,
			indicator: null,
			blockedReason: 'empty-target-rect',
			isVoiceParticipantTransfer: false,
		};
	}
	const blockedReason = getChannelDropBlockedReason(item, target);
	if (blockedReason) {
		return {
			intent: null,
			indicator: createIndicator(clientOffset, targetRect, false),
			blockedReason,
			isVoiceParticipantTransfer: false,
		};
	}
	if (item.type === DragItemType.VOICE_PARTICIPANT) {
		return {
			intent: null,
			indicator: null,
			blockedReason: null,
			isVoiceParticipantTransfer: true,
		};
	}
	const zone = resolveVerticalDropEdge(clientOffset, targetRect);
	const indicator = createIndicator(clientOffset, targetRect, true);
	const targetIsCategory = isCategoryType(target.channelType);
	const result: DropResult =
		targetIsCategory && item.type !== DragItemType.CATEGORY
			? {
					targetId: target.id,
					position: zone === 'before' ? 'before' : 'inside',
					targetParentId: zone === 'before' ? target.parentId : target.id,
				}
			: {
					targetId: target.id,
					position: zone === 'before' ? 'before' : 'after',
					targetParentId: target.parentId,
				};
	return {
		intent: {
			indicator,
			result,
		},
		indicator,
		blockedReason: null,
		isVoiceParticipantTransfer: false,
	};
}
