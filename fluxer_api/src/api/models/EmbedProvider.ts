// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageEmbedProvider} from '@app/api/database/types/MessageTypes';
import {sanitizeOptionalAbsoluteUrlOrNull} from '@app/api/utils/UrlSanitizer';

export class EmbedProvider {
	readonly name: string | null;
	readonly url: string | null;

	constructor(provider: MessageEmbedProvider) {
		this.name = provider.name ?? null;
		this.url = sanitizeOptionalAbsoluteUrlOrNull(provider.url);
	}

	toMessageEmbedProvider(): MessageEmbedProvider {
		return {
			name: this.name,
			url: this.url,
		};
	}
}
