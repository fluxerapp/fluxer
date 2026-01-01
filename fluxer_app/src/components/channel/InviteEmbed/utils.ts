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

import {useLingui} from '@lingui/react/macro';
import type {Channel} from '~/records/ChannelRecord';
import type {Guild, GuildRecord} from '~/records/GuildRecord';

export const getGroupDMTitle = (channel: Channel): string => {
	const {t} = useLingui();
	const channelName = channel.name?.trim();
	if (channelName && channelName.length > 0) {
		return channelName;
	}

	const recipients = channel.recipients ?? [];
	const names = recipients
		.map((recipient) => recipient.username)
		.filter((name): name is string => name !== undefined && name.length > 0);

	if (names.length === 0) {
		return t`Unnamed Group`;
	}

	return names.join(', ');
};

type InviteGuild = Guild | GuildRecord;

export const getGuildSplashAspectRatio = (guild: InviteGuild): number | undefined => {
	const width = 'splashWidth' in guild ? guild.splashWidth : guild.splash_width;
	const height = 'splashHeight' in guild ? guild.splashHeight : guild.splash_height;
	if (width != null && height != null && width > 0 && height > 0) {
		return width / height;
	}
	return undefined;
};

export const getGuildEmbedSplashAspectRatio = (guild: InviteGuild): number | undefined => {
	const width = 'embedSplashWidth' in guild ? guild.embedSplashWidth : guild.embed_splash_width;
	const height = 'embedSplashHeight' in guild ? guild.embedSplashHeight : guild.embed_splash_height;
	if (width != null && height != null && width > 0 && height > 0) {
		return width / height;
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
