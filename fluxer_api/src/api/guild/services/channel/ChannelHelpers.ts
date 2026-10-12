// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID} from '@app/api/BrandedTypes';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import type {Channel} from '@app/api/models/Channel';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {MAX_CHANNELS_PER_CATEGORY} from '@fluxer/constants/src/LimitConstants';
import {THREAD_ONLY_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {MaxCategoryChannelsError} from '@fluxer/errors/src/domains/channel/MaxCategoryChannelsError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';

export interface ChannelReorderOperation {
	channelId: ChannelID;
	parentId: ChannelID | null | undefined;
	precedingSiblingId: ChannelID | null;
}

function isTextLikeChannelType(type: number): boolean {
	return (
		type === ChannelTypes.GUILD_TEXT ||
		type === ChannelTypes.GUILD_ANNOUNCEMENT ||
		type === ChannelTypes.GUILD_LINK ||
		THREAD_ONLY_CHANNEL_TYPES.has(type)
	);
}

// biome-ignore lint/complexity/noStaticOnlyClass: Existing callers use this as a namespaced helper API.
export class ChannelHelpers {
	static assertValidParentCategory(
		parentId: ChannelID | null,
		channelType: number,
		channels: Array<Channel>,
	): Channel | null {
		if (parentId === null) return null;
		if (channelType === ChannelTypes.GUILD_CATEGORY)
			throw InputValidationError.fromCode('parent_id', ValidationErrorCodes.CATEGORIES_CANNOT_HAVE_PARENTS);
		const parent = channels.find((channel) => channel.id === parentId);
		if (!parent) throw InputValidationError.fromCode('parent_id', ValidationErrorCodes.INVALID_PARENT_CHANNEL);
		if (parent.type !== ChannelTypes.GUILD_CATEGORY)
			throw InputValidationError.fromCode('parent_id', ValidationErrorCodes.PARENT_MUST_BE_CATEGORY);
		return parent;
	}

	static async assertCategoryCapacity(params: {
		gatewayService: IGatewayService;
		guildRepository: GuildRepository;
		limitConfigService: LimitConfigService;
		guildId: GuildID;
		categoryId: ChannelID;
	}): Promise<void> {
		const count = await params.gatewayService.getCategoryChannelCount({
			guildId: params.guildId,
			categoryId: params.categoryId,
		});
		const guild = await params.guildRepository.findUnique(params.guildId);
		const context = createLimitMatchContext({user: null, guildFeatures: guild?.features ?? null});
		const maxChannels = Math.floor(
			resolveLimitSafe(
				params.limitConfigService.getConfigSnapshot(),
				context,
				'max_channels_per_category',
				MAX_CHANNELS_PER_CATEGORY,
				'guild',
			),
		);
		if (count >= maxChannels) throw new MaxCategoryChannelsError(maxChannels);
	}

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
		const textChannels = channelsInCategory.filter((c) => isTextLikeChannelType(c.type));
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
				const isMovedText = isTextLikeChannelType(movedChannel.type);
				if (isMovedVoice) {
					const next = siblings[movedIndex + 1];
					if (next && isTextLikeChannelType(next.type)) {
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
					const isText = isTextLikeChannelType(sibling.type);
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
}
