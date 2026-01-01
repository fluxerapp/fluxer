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

import type {ChannelID} from '~/BrandedTypes';
import {ChannelTypes} from '~/Constants';
import {InputValidationError} from '~/Errors';
import type {Channel} from '~/Models';
import {serializeChannelForAudit as serializeChannelForAuditUtil} from '~/utils/AuditSerializationUtils';
import {toIdString} from '~/utils/IdUtils';

export interface ChannelReorderOperation {
	channelId: ChannelID;
	parentId: ChannelID | null | undefined;
	precedingSiblingId: ChannelID | null;
}

// biome-ignore lint/complexity/noStaticOnlyClass: Using class for namespace organization
export class ChannelHelpers {
	static getNextGlobalChannelPosition(
		channelType: number,
		parentId: ChannelID | null | undefined,
		existingChannels: Array<Channel>,
	): number {
		if (channelType === ChannelTypes.GUILD_CATEGORY) {
			return existingChannels.reduce((max, c) => Math.max(max, c.position || 0), 0) + 1;
		}

		if (parentId == null || parentId === undefined) {
			return existingChannels.reduce((max, c) => Math.max(max, c.position || 0), 0) + 1;
		}

		const parentCategory = existingChannels.find((c) => c.id === parentId);
		if (!parentCategory) {
			return existingChannels.reduce((max, c) => Math.max(max, c.position || 0), 0) + 1;
		}

		const channelsInCategory = existingChannels.filter((c) => c.parentId === parentId);
		const textChannels = channelsInCategory.filter(
			(c) => c.type === ChannelTypes.GUILD_TEXT || c.type === ChannelTypes.GUILD_LINK,
		);
		const voiceChannels = channelsInCategory.filter((c) => c.type === ChannelTypes.GUILD_VOICE);

		if (channelType === ChannelTypes.GUILD_VOICE) {
			if (voiceChannels.length > 0) {
				return Math.max(...voiceChannels.map((c) => c.position || 0)) + 1;
			} else if (textChannels.length > 0) {
				return Math.max(...textChannels.map((c) => c.position || 0)) + 1;
			} else {
				return parentCategory.position + 1;
			}
		} else {
			if (textChannels.length > 0) {
				const maxTextPosition = Math.max(...textChannels.map((c) => c.position || 0));
				if (voiceChannels.length > 0) {
					return maxTextPosition + 1;
				} else {
					return maxTextPosition + 1;
				}
			} else {
				return parentCategory.position + 1;
			}
		}
	}

	static validateChannelVoicePlacement(
		finalChannels: Array<Channel>,
		parentMap: Map<ChannelID, ChannelID | null>,
	): void {
		const orderedByParent = new Map<ChannelID | null, Array<Channel>>();

		for (const channel of finalChannels) {
			const parentId = parentMap.get(channel.id) ?? null;
			if (!orderedByParent.has(parentId)) {
				orderedByParent.set(parentId, []);
			}
			orderedByParent.get(parentId)!.push(channel);
		}

		for (const [parentId, siblings] of orderedByParent.entries()) {
			if (parentId === null) continue;
			let encounteredVoice = false;
			for (const sibling of siblings) {
				if (sibling.type === ChannelTypes.GUILD_VOICE) {
					encounteredVoice = true;
					continue;
				}

				const isText = sibling.type === ChannelTypes.GUILD_TEXT || sibling.type === ChannelTypes.GUILD_LINK;
				if (encounteredVoice && isText) {
					throw InputValidationError.create(
						'preceding_sibling_id',
						'Voice channels cannot be positioned above text channels within the same category',
					);
				}
			}
		}
	}

	static serializeChannelForAudit(channel: Channel): Record<string, unknown> {
		return serializeChannelForAuditUtil(channel);
	}

	static serializeChannelOrdering(
		channels: Array<Channel>,
	): Array<{channel_id: string; parent_id: string | null; position: number}> {
		return [...channels]
			.sort((a, b) => {
				if (a.position !== b.position) {
					return a.position - b.position;
				}
				return a.id.toString().localeCompare(b.id.toString());
			})
			.map((channel) => ({
				channel_id: channel.id.toString(),
				parent_id: toIdString(channel.parentId),
				position: channel.position,
			}));
	}
}
