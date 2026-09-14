// SPDX-License-Identifier: AGPL-3.0-or-later

import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/features/app/utils/LimitUtils';
import type {Channel} from '@app/features/channel/models/Channel';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import Permission from '@app/features/permissions/state/Permission';
import {formatPermissionLabel} from '@app/features/permissions/utils/PermissionUtils';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const UNLOCK_CUSTOM_EMOJIS_IN_DMS_WITH_DESCRIPTOR = msg({
	message: 'Unlock custom emojis in DMs with {premiumProductName}',
	comment: 'Premium upsell shown when sending custom emoji in a DM without a subscription.',
});
const YOU_NEED_THE_PERMISSION_TO_USE_EXTERNAL_EMOJI_DESCRIPTOR = msg({
	message: 'You need the "{permissionLabel}" permission to use emoji from other communities here.',
	comment: 'Permission error shown when sending external emoji without the required permission.',
});
const UNLOCK_EXTERNAL_CUSTOM_EMOJIS_WITH_DESCRIPTOR = msg({
	message: 'Use emoji from other communities with {premiumProductName}',
	comment: 'Premium upsell shown when sending external emoji without a subscription.',
});
const UNLOCK_STICKERS_IN_DMS_WITH_DESCRIPTOR = msg({
	message: 'Unlock stickers in DMs with {premiumProductName}',
	comment: 'Premium upsell shown when sending custom stickers in a DM without a subscription.',
});
const YOU_NEED_THE_PERMISSION_TO_USE_EXTERNAL_STICKERS_DESCRIPTOR = msg({
	message: 'You need the "{permissionLabel}" permission to use stickers from other communities here.',
	comment: 'Permission error shown when sending external stickers without the required permission.',
});
const UNLOCK_EXTERNAL_STICKERS_WITH_DESCRIPTOR = msg({
	message: 'Use stickers from other communities with {premiumProductName}',
	comment: 'Premium upsell shown when sending external stickers without a subscription.',
});

export interface AvailabilityCheck {
	canUse: boolean;
	isLockedByPremium: boolean;
	isLockedByPermission: boolean;
	lockReason?: string;
}

export function hasGlobalExpressionsEnabled(): boolean {
	return isLimitToggleEnabled(
		{
			feature_global_expressions: LimitResolver.resolve({key: 'feature_global_expressions', fallback: 0}),
		},
		'feature_global_expressions',
	);
}

export function checkEmojiAvailability(i18n: I18n, emoji: FlatEmoji, channel: Channel | null): AvailabilityCheck {
	return checkEmojiAvailabilityWithGuildFallback(i18n, emoji, channel, null);
}

export function checkEmojiAvailabilityWithGuildFallback(
	i18n: I18n,
	emoji: FlatEmoji,
	channel: Channel | null,
	guildIdFallback: string | null,
): AvailabilityCheck {
	if (!emoji.guildId) {
		return {
			canUse: true,
			isLockedByPremium: false,
			isLockedByPermission: false,
		};
	}
	const hasGlobalExpressions = hasGlobalExpressionsEnabled();
	const channelGuildId = channel?.guildId ?? guildIdFallback;
	if (!channelGuildId) {
		if (!hasGlobalExpressions) {
			if (!RuntimeConfig.isSelfHosted()) {
				return {
					canUse: false,
					isLockedByPremium: true,
					isLockedByPermission: false,
					lockReason: i18n._(UNLOCK_CUSTOM_EMOJIS_IN_DMS_WITH_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME}),
				};
			}
			return {
				canUse: false,
				isLockedByPremium: false,
				isLockedByPermission: false,
			};
		}
		return {
			canUse: true,
			isLockedByPremium: false,
			isLockedByPermission: false,
		};
	}
	const isExternalEmoji = emoji.guildId !== channelGuildId;
	if (!isExternalEmoji) {
		return {
			canUse: true,
			isLockedByPremium: false,
			isLockedByPermission: false,
		};
	}
	const hasPermission = Permission.can(Permissions.USE_EXTERNAL_EMOJIS, {
		guildId: channelGuildId,
		channelId: channel?.id,
	});
	if (!hasPermission) {
		const permissionLabel = formatPermissionLabel(i18n, Permissions.USE_EXTERNAL_EMOJIS);
		return {
			canUse: false,
			isLockedByPremium: false,
			isLockedByPermission: true,
			lockReason: i18n._(YOU_NEED_THE_PERMISSION_TO_USE_EXTERNAL_EMOJI_DESCRIPTOR, {permissionLabel}),
		};
	}
	if (!hasGlobalExpressions) {
		if (!RuntimeConfig.isSelfHosted()) {
			return {
				canUse: false,
				isLockedByPremium: true,
				isLockedByPermission: false,
				lockReason: i18n._(UNLOCK_EXTERNAL_CUSTOM_EMOJIS_WITH_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME}),
			};
		}
		return {
			canUse: false,
			isLockedByPremium: false,
			isLockedByPermission: false,
		};
	}
	return {
		canUse: true,
		isLockedByPremium: false,
		isLockedByPermission: false,
	};
}

export function checkStickerAvailability(
	i18n: I18n,
	sticker: GuildSticker,
	channel: Channel | null,
): AvailabilityCheck {
	if (!sticker.guildId) {
		return {
			canUse: true,
			isLockedByPremium: false,
			isLockedByPermission: false,
		};
	}
	const hasGlobalExpressions = isLimitToggleEnabled(
		{
			feature_global_expressions: LimitResolver.resolve({key: 'feature_global_expressions', fallback: 0}),
		},
		'feature_global_expressions',
	);
	if (!channel?.guildId) {
		if (!hasGlobalExpressions) {
			if (!RuntimeConfig.isSelfHosted()) {
				return {
					canUse: false,
					isLockedByPremium: true,
					isLockedByPermission: false,
					lockReason: i18n._(UNLOCK_STICKERS_IN_DMS_WITH_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME}),
				};
			}
			return {
				canUse: false,
				isLockedByPremium: false,
				isLockedByPermission: false,
			};
		}
		return {
			canUse: true,
			isLockedByPremium: false,
			isLockedByPermission: false,
		};
	}
	const isExternalSticker = sticker.guildId !== channel.guildId;
	if (!isExternalSticker) {
		return {
			canUse: true,
			isLockedByPremium: false,
			isLockedByPermission: false,
		};
	}
	const hasPermission = Permission.can(Permissions.USE_EXTERNAL_STICKERS, {
		guildId: channel.guildId,
		channelId: channel.id,
	});
	if (!hasPermission) {
		const permissionLabel = formatPermissionLabel(i18n, Permissions.USE_EXTERNAL_STICKERS);
		if (!hasGlobalExpressions) {
			return {
				canUse: false,
				isLockedByPremium: false,
				isLockedByPermission: true,
				lockReason: i18n._(YOU_NEED_THE_PERMISSION_TO_USE_EXTERNAL_STICKERS_DESCRIPTOR, {permissionLabel}),
			};
		}
		return {
			canUse: false,
			isLockedByPremium: false,
			isLockedByPermission: true,
			lockReason: i18n._(YOU_NEED_THE_PERMISSION_TO_USE_EXTERNAL_STICKERS_DESCRIPTOR, {permissionLabel}),
		};
	}
	if (!hasGlobalExpressions) {
		if (!RuntimeConfig.isSelfHosted()) {
			return {
				canUse: false,
				isLockedByPremium: true,
				isLockedByPermission: false,
				lockReason: i18n._(UNLOCK_EXTERNAL_STICKERS_WITH_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME}),
			};
		}
		return {
			canUse: false,
			isLockedByPremium: false,
			isLockedByPermission: false,
		};
	}
	return {
		canUse: true,
		isLockedByPremium: false,
		isLockedByPermission: false,
	};
}

export function filterEmojisForAutocomplete(
	i18n: I18n,
	emojis: ReadonlyArray<FlatEmoji>,
	channel: Channel | null,
): ReadonlyArray<FlatEmoji> {
	return emojis.filter((emoji) => {
		const check = checkEmojiAvailability(i18n, emoji, channel);
		return check.canUse;
	});
}

export function filterStickersForAutocomplete(
	i18n: I18n,
	stickers: ReadonlyArray<GuildSticker>,
	channel: Channel | null,
): ReadonlyArray<GuildSticker> {
	return stickers.filter((sticker) => {
		const check = checkStickerAvailability(i18n, sticker, channel);
		return check.canUse;
	});
}

export function shouldShowEmojiPremiumUpsell(channel: Channel | null): boolean {
	if (RuntimeConfig.isSelfHosted()) {
		return false;
	}
	const hasGlobalExpressions = hasGlobalExpressionsEnabled();
	if (hasGlobalExpressions) {
		return false;
	}
	if (!channel?.guildId) {
		return true;
	}
	const hasPermission = Permission.can(Permissions.USE_EXTERNAL_EMOJIS, {
		guildId: channel.guildId,
		channelId: channel.id,
	});
	return hasPermission;
}

export function shouldShowStickerPremiumUpsell(channel: Channel | null): boolean {
	if (RuntimeConfig.isSelfHosted()) {
		return false;
	}
	const hasGlobalExpressions = isLimitToggleEnabled(
		{
			feature_global_expressions: LimitResolver.resolve({key: 'feature_global_expressions', fallback: 0}),
		},
		'feature_global_expressions',
	);
	if (hasGlobalExpressions) {
		return false;
	}
	if (!channel?.guildId) {
		return true;
	}
	const hasPermission = Permission.can(Permissions.USE_EXTERNAL_STICKERS, {
		guildId: channel.guildId,
		channelId: channel.id,
	});
	return hasPermission;
}
