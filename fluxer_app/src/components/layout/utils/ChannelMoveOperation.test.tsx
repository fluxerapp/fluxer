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

import {DND_TYPES, type DragItem, type DropResult} from '@app/components/layout/types/DndTypes';
import {createChannelMoveOperation} from '@app/components/layout/utils/ChannelMoveOperation';
import {ChannelRecord} from '@app/records/ChannelRecord';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it} from 'vitest';

const TEST_GUILD_ID = 'guild';

function createChannel(params: {id: string; type: number; position: number; parentId?: string | null}): ChannelRecord {
	return new ChannelRecord({
		id: params.id,
		guild_id: TEST_GUILD_ID,
		name: params.id,
		type: params.type,
		position: params.position,
		parent_id: params.parentId ?? null,
	});
}

function createDragItem(channel: ChannelRecord): DragItem {
	return {
		type: DND_TYPES.CHANNEL,
		id: channel.id,
		channelType: channel.type,
		parentId: channel.parentId,
		guildId: TEST_GUILD_ID,
	};
}

describe('ChannelMoveOperation', () => {
	it('moves a text channel into a voice-only category at the top', () => {
		const textCategory = createChannel({id: 'text-category', type: ChannelTypes.GUILD_CATEGORY, position: 0});
		const voiceCategory = createChannel({id: 'voice-category', type: ChannelTypes.GUILD_CATEGORY, position: 1});
		const generalText = createChannel({
			id: 'general',
			type: ChannelTypes.GUILD_TEXT,
			position: 2,
			parentId: textCategory.id,
		});
		const generalVoice = createChannel({
			id: 'General',
			type: ChannelTypes.GUILD_VOICE,
			position: 3,
			parentId: voiceCategory.id,
		});

		const operation = createChannelMoveOperation({
			channels: [textCategory, voiceCategory, generalText, generalVoice],
			dragItem: createDragItem(generalText),
			dropResult: {
				targetId: voiceCategory.id,
				position: 'inside',
				targetParentId: voiceCategory.id,
			},
		});

		expect(operation).toEqual({
			channelId: generalText.id,
			newParentId: voiceCategory.id,
			precedingSiblingId: null,
			position: 0,
		});
	});

	it('keeps text channels above voice channels even when dropped after a voice channel', () => {
		const voiceCategory = createChannel({id: 'voice-category', type: ChannelTypes.GUILD_CATEGORY, position: 0});
		const generalText = createChannel({id: 'general', type: ChannelTypes.GUILD_TEXT, position: 1});
		const generalVoice = createChannel({
			id: 'General',
			type: ChannelTypes.GUILD_VOICE,
			position: 2,
			parentId: voiceCategory.id,
		});

		const dropResult: DropResult = {
			targetId: generalVoice.id,
			position: 'after',
			targetParentId: voiceCategory.id,
		};
		const operation = createChannelMoveOperation({
			channels: [voiceCategory, generalText, generalVoice],
			dragItem: createDragItem(generalText),
			dropResult,
		});

		expect(operation).toEqual({
			channelId: generalText.id,
			newParentId: voiceCategory.id,
			precedingSiblingId: null,
			position: 0,
		});
	});

	it('keeps moved voice channels below all text siblings', () => {
		const textCategory = createChannel({id: 'text-category', type: ChannelTypes.GUILD_CATEGORY, position: 0});
		const voiceCategory = createChannel({id: 'voice-category', type: ChannelTypes.GUILD_CATEGORY, position: 1});
		const textOne = createChannel({
			id: 'text-one',
			type: ChannelTypes.GUILD_TEXT,
			position: 2,
			parentId: textCategory.id,
		});
		const textTwo = createChannel({
			id: 'text-two',
			type: ChannelTypes.GUILD_TEXT,
			position: 3,
			parentId: textCategory.id,
		});
		const lounge = createChannel({
			id: 'lounge',
			type: ChannelTypes.GUILD_VOICE,
			position: 4,
			parentId: voiceCategory.id,
		});

		const operation = createChannelMoveOperation({
			channels: [textCategory, voiceCategory, textOne, textTwo, lounge],
			dragItem: createDragItem(lounge),
			dropResult: {
				targetId: textOne.id,
				position: 'before',
				targetParentId: textCategory.id,
			},
		});

		expect(operation).toEqual({
			channelId: lounge.id,
			newParentId: textCategory.id,
			precedingSiblingId: textTwo.id,
			position: 2,
		});
	});

	it('moves a top-level category above another category with root-level preceding sibling', () => {
		const milsims = createChannel({id: 'milsims', type: ChannelTypes.GUILD_CATEGORY, position: 3});
		const coopGames = createChannel({id: 'coop-games', type: ChannelTypes.GUILD_CATEGORY, position: 11});
		const frontDoor = createChannel({id: 'front-door', type: ChannelTypes.GUILD_CATEGORY, position: 30});
		const coopVoice = createChannel({
			id: 'coop-voice',
			type: ChannelTypes.GUILD_VOICE,
			position: 12,
			parentId: coopGames.id,
		});
		const coopText = createChannel({
			id: 'coop-text',
			type: ChannelTypes.GUILD_TEXT,
			position: 13,
			parentId: coopGames.id,
		});

		const operation = createChannelMoveOperation({
			channels: [milsims, coopGames, frontDoor, coopVoice, coopText],
			dragItem: createDragItem(frontDoor),
			dropResult: {
				targetId: coopGames.id,
				position: 'before',
				targetParentId: null,
			},
		});

		expect(operation).toEqual({
			channelId: frontDoor.id,
			newParentId: null,
			precedingSiblingId: milsims.id,
			position: 1,
		});
	});

	it('returns null when dropping to the same effective placement', () => {
		const category = createChannel({id: 'category', type: ChannelTypes.GUILD_CATEGORY, position: 0});
		const textOne = createChannel({
			id: 'text-one',
			type: ChannelTypes.GUILD_TEXT,
			position: 1,
			parentId: category.id,
		});
		const textTwo = createChannel({
			id: 'text-two',
			type: ChannelTypes.GUILD_TEXT,
			position: 2,
			parentId: category.id,
		});

		const operation = createChannelMoveOperation({
			channels: [category, textOne, textTwo],
			dragItem: createDragItem(textOne),
			dropResult: {
				targetId: textTwo.id,
				position: 'before',
				targetParentId: category.id,
			},
		});

		expect(operation).toBeNull();
	});
});
