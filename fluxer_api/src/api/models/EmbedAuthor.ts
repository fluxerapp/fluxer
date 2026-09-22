// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageEmbedAuthor} from '@app/api/database/types/MessageTypes';
import {sanitizeOptionalAbsoluteUrlOrNull} from '@app/api/utils/UrlSanitizer';

export class EmbedAuthor {
	readonly name: string | null;
	readonly url: string | null;
	readonly iconUrl: string | null;

	constructor(author: MessageEmbedAuthor) {
		this.name = author.name ?? null;
		this.url = sanitizeOptionalAbsoluteUrlOrNull(author.url);
		this.iconUrl = sanitizeOptionalAbsoluteUrlOrNull(author.icon_url);
	}

	toMessageEmbedAuthor(): MessageEmbedAuthor {
		return {
			name: this.name,
			url: this.url,
			icon_url: this.iconUrl,
		};
	}
}
