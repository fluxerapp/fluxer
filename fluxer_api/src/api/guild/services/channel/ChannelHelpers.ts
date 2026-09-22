// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID} from '@app/api/BrandedTypes';
import type {Channel} from '@app/api/models/Channel';
import {serializeChannelForAudit as serializeChannelForAuditUtil} from '@app/api/utils/AuditSerializationUtils';
import {toIdString} from '@app/api/utils/IdUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';

export interface ChannelReorderOperation {
	channelId: ChannelID;
	parentId: ChannelID | null | undefined;
	precedingSiblingId: ChannelID | null;
}

// biome-ignore lint/complexity/noStaticOnlyClass: Existing callers use this as a namespaced helper API.
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
				return voiceChannels.reduce((max, c) => Math.max(max, c.position || 0), 0) + 1;
			} else if (textChannels.length > 0) {
				return textChannels.reduce((max, c) => Math.max(max, c.position || 0), 0) + 1;
			} else {
				return parentCategory.position + 1;
			}
		} else {
			if (textChannels.length > 0) {
				const maxTextPosition = textChannels.reduce((max, c) => Math.max(max, c.position || 0), 0);
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
		parentIdsToValidate?: ReadonlySet<ChannelID>,
		movedChannelId?: ChannelID,
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
			if (parentIdsToValidate && !parentIdsToValidate.has(parentId)) {
				continue;
			}
			if (movedChannelId) {
				const movedIndex = siblings.findIndex((s) => s.id === movedChannelId);
				if (movedIndex < 0) continue;
				const movedChannel = siblings[movedIndex];
				const isMovedVoice = movedChannel.type === ChannelTypes.GUILD_VOICE;
				const isMovedText =
					movedChannel.type === ChannelTypes.GUILD_TEXT || movedChannel.type === ChannelTypes.GUILD_LINK;
				if (isMovedVoice) {
					const next = siblings[movedIndex + 1];
					if (next && (next.type === ChannelTypes.GUILD_TEXT || next.type === ChannelTypes.GUILD_LINK)) {
						throw InputValidationError.fromCode(
							'preceding_sibling_id',
							ValidationErrorCodes.VOICE_CHANNELS_CANNOT_BE_ABOVE_TEXT_CHANNELS,
						);
					}
				} else if (isMovedText) {
					const prev = siblings[movedIndex - 1];
					if (prev && prev.type === ChannelTypes.GUILD_VOICE) {
						throw InputValidationError.fromCode(
							'preceding_sibling_id',
							ValidationErrorCodes.VOICE_CHANNELS_CANNOT_BE_ABOVE_TEXT_CHANNELS,
						);
					}
				}
			} else {
				let encounteredVoice = false;
				for (const sibling of siblings) {
					if (sibling.type === ChannelTypes.GUILD_VOICE) {
						encounteredVoice = true;
						continue;
					}
					const isText = sibling.type === ChannelTypes.GUILD_TEXT || sibling.type === ChannelTypes.GUILD_LINK;
					if (encounteredVoice && isText) {
						throw InputValidationError.fromCode(
							'preceding_sibling_id',
							ValidationErrorCodes.VOICE_CHANNELS_CANNOT_BE_ABOVE_TEXT_CHANNELS,
						);
					}
				}
			}
		}
	}

	static serializeChannelForAudit(channel: Channel): Record<string, unknown> {
		return serializeChannelForAuditUtil(channel);
	}

	static serializeChannelOrdering(channels: Array<Channel>): Array<{
		channel_id: string;
		parent_id: string | null;
		position: number;
	}> {
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
