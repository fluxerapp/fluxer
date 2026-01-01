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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Permissions} from '~/Constants';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildStickerRecord} from '~/records/GuildStickerRecord';
import type {Emoji} from '~/stores/EmojiStore';
import PermissionStore from '~/stores/PermissionStore';
import UserStore from '~/stores/UserStore';

export interface AvailabilityCheck {
	canUse: boolean;
	isLockedByPremium: boolean;
	isLockedByPermission: boolean;
	lockReason?: string;
}

export function checkEmojiAvailability(i18n: I18n, emoji: Emoji, channel: ChannelRecord | null): AvailabilityCheck {
	if (!emoji.guildId) {
		return {
			canUse: true,
			isLockedByPremium: false,
			isLockedByPermission: false,
		};
	}

	const currentUser = UserStore.getCurrentUser();
	const hasPremium = currentUser?.isPremium() ?? false;

	if (!channel?.guildId) {
		if (!hasPremium) {
			return {
				canUse: false,
				isLockedByPremium: true,
				isLockedByPermission: false,
				lockReason: i18n._(msg`Unlock custom emojis in DMs with Plutonium`),
			};
		}
		return {
			canUse: true,
			isLockedByPremium: false,
			isLockedByPermission: false,
		};
	}

	const isExternalEmoji = emoji.guildId !== channel.guildId;

	if (!isExternalEmoji) {
		return {
			canUse: true,
			isLockedByPremium: false,
			isLockedByPermission: false,
		};
	}

	const hasPermission = PermissionStore.can(Permissions.USE_EXTERNAL_EMOJIS, {
		guildId: channel.guildId,
		channelId: channel.id,
	});

	if (!hasPermission) {
		if (!hasPremium) {
			return {
				canUse: false,
				isLockedByPremium: false,
				isLockedByPermission: true,
				lockReason: i18n._(msg`You lack permission to use external emojis in this channel`),
			};
		}
		return {
			canUse: false,
			isLockedByPremium: false,
			isLockedByPermission: true,
			lockReason: i18n._(msg`You lack permission to use external emojis in this channel`),
		};
	}

	if (!hasPremium) {
		return {
			canUse: false,
			isLockedByPremium: true,
			isLockedByPermission: false,
			lockReason: i18n._(msg`Unlock external custom emojis with Plutonium`),
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
	sticker: GuildStickerRecord,
	channel: ChannelRecord | null,
): AvailabilityCheck {
	if (!sticker.guildId) {
		return {
			canUse: true,
			isLockedByPremium: false,
			isLockedByPermission: false,
		};
	}

	const currentUser = UserStore.getCurrentUser();
	const hasPremium = currentUser?.isPremium() ?? false;

	if (!channel?.guildId) {
		if (!hasPremium) {
			return {
				canUse: false,
				isLockedByPremium: true,
				isLockedByPermission: false,
				lockReason: i18n._(msg`Unlock stickers in DMs with Plutonium`),
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

	const hasPermission = PermissionStore.can(Permissions.USE_EXTERNAL_STICKERS, {
		guildId: channel.guildId,
		channelId: channel.id,
	});

	if (!hasPermission) {
		if (!hasPremium) {
			return {
				canUse: false,
				isLockedByPremium: false,
				isLockedByPermission: true,
				lockReason: i18n._(msg`You lack permission to use external stickers in this channel`),
			};
		}
		return {
			canUse: false,
			isLockedByPremium: false,
			isLockedByPermission: true,
			lockReason: i18n._(msg`You lack permission to use external stickers in this channel`),
		};
	}

	if (!hasPremium) {
		return {
			canUse: false,
			isLockedByPremium: true,
			isLockedByPermission: false,
			lockReason: i18n._(msg`Unlock external stickers with Plutonium`),
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
	emojis: ReadonlyArray<Emoji>,
	channel: ChannelRecord | null,
): ReadonlyArray<Emoji> {
	return emojis.filter((emoji) => {
		const check = checkEmojiAvailability(i18n, emoji, channel);
		return check.canUse;
	});
}

export function filterStickersForAutocomplete(
	i18n: I18n,
	stickers: ReadonlyArray<GuildStickerRecord>,
	channel: ChannelRecord | null,
): ReadonlyArray<GuildStickerRecord> {
	return stickers.filter((sticker) => {
		const check = checkStickerAvailability(i18n, sticker, channel);
		return check.canUse;
	});
}

export function shouldShowEmojiPremiumUpsell(channel: ChannelRecord | null): boolean {
	const currentUser = UserStore.getCurrentUser();
	const hasPremium = currentUser?.isPremium() ?? false;

	if (hasPremium) {
		return false;
	}

	if (!channel?.guildId) {
		return true;
	}

	const hasPermission = PermissionStore.can(Permissions.USE_EXTERNAL_EMOJIS, {
		guildId: channel.guildId,
		channelId: channel.id,
	});

	return hasPermission;
}

export function shouldShowStickerPremiumUpsell(channel: ChannelRecord | null): boolean {
	const currentUser = UserStore.getCurrentUser();
	const hasPremium = currentUser?.isPremium() ?? false;

	if (hasPremium) {
		return false;
	}

	if (!channel?.guildId) {
		return true;
	}

	const hasPermission = PermissionStore.can(Permissions.USE_EXTERNAL_STICKERS, {
		guildId: channel.guildId,
		channelId: channel.id,
	});

	return hasPermission;
}
