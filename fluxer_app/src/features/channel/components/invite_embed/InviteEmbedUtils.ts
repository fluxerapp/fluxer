// SPDX-License-Identifier: AGPL-3.0-or-later

import {clampWideAssetAspectRatio} from '@app/features/expressions/utils/AssetImageGeometry';
import type {Guild} from '@app/features/guild/models/Guild';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import type {Channel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {Guild as WireGuild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';

export function getGroupDMTitle(channel: Channel, unnamedGroupLabel: string): string {
	return getChannelDisplayName(channel, unnamedGroupLabel);
}

function getChannelDisplayName(channel: Channel, unnamedGroupLabel: string): string {
	const channelName = channel.name?.trim();
	if (channelName && channelName.length > 0) {
		return channelName;
	}
	const recipients = channel.recipients ?? [];
	const names = recipients
		.map((recipient) => NicknameUtils.getDisplayName(recipient))
		.filter((name): name is string => name !== undefined && name.length > 0);
	if (names.length === 0) {
		return unnamedGroupLabel;
	}
	return names.join(', ');
}

type InviteGuild = Guild | WireGuild;
export const getGuildEmbedSplashAspectRatio = (guild: InviteGuild): number | undefined => {
	const width = 'embedSplashWidth' in guild ? guild.embedSplashWidth : guild.embed_splash_width;
	const height = 'embedSplashHeight' in guild ? guild.embedSplashHeight : guild.embed_splash_height;
	if (width != null && height != null && width > 0 && height > 0) {
		return clampWideAssetAspectRatio(width / height);
	}
	return undefined;
};
export const getImageAspectRatioFromBase64 = (base64Url: string): Promise<number> => {
	if (typeof Image === 'undefined') {
		return Promise.resolve(16 / 9);
	}
	return new Promise((resolve, reject) => {
		const img = new Image();
		img.onload = () => {
			if (img.naturalWidth > 0 && img.naturalHeight > 0) {
				resolve(img.naturalWidth / img.naturalHeight);
			} else {
				reject(new Error('Invalid image dimensions'));
			}
			img.onload = null;
			img.onerror = null;
		};
		img.onerror = () => {
			reject(new Error('Failed to load image'));
			img.onload = null;
			img.onerror = null;
		};
		img.src = base64Url;
	});
};
