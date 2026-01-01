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
import i18n from '~/i18n';
import type {MessageRecord} from '~/records/MessageRecord';
import * as FavoriteMemeUtils from '~/utils/FavoriteMemeUtils';

export const deriveDefaultNameFromMessage = ({
	message,
	attachmentId,
	embedIndex,
	url,
	proxyUrl,
	i18nInstance = i18n,
}: {
	message: MessageRecord | undefined;
	attachmentId: string | undefined;
	embedIndex?: number | undefined;
	url: string;
	proxyUrl: string;
	i18nInstance?: I18n;
}): string => {
	if (message && attachmentId) {
		const attachment = message.attachments.find((a) => a.id === attachmentId);
		if (attachment) {
			return FavoriteMemeUtils.deriveDefaultNameFromAttachment(i18nInstance, attachment);
		}
	}

	if (message && embedIndex !== undefined) {
		const embed = message.embeds[embedIndex];
		if (embed) {
			return FavoriteMemeUtils.deriveDefaultNameFromEmbedMedia(
				i18nInstance,
				{url, proxy_url: proxyUrl, flags: 0},
				embed,
			);
		}
	}

	return FavoriteMemeUtils.deriveDefaultNameFromEmbedMedia(i18nInstance, {url, proxy_url: proxyUrl, flags: 0});
};

export const splitFilename = (filename: string): {name: string; extension: string} => {
	const lastDotIndex = filename.lastIndexOf('.');
	if (lastDotIndex === -1) {
		return {name: filename, extension: ''};
	}
	return {
		name: filename.substring(0, lastDotIndex),
		extension: filename.substring(lastDotIndex),
	};
};
