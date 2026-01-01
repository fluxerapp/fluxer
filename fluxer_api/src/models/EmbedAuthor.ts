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

import type {MessageEmbedAuthor} from '~/database/CassandraTypes';

export class EmbedAuthor {
	readonly name: string | null;
	readonly url: string | null;
	readonly iconUrl: string | null;

	constructor(author: MessageEmbedAuthor) {
		this.name = author.name ?? null;
		this.url = author.url ?? null;
		this.iconUrl = author.icon_url ?? null;
	}

	toMessageEmbedAuthor(): MessageEmbedAuthor {
		return {
			name: this.name,
			url: this.url,
			icon_url: this.iconUrl,
		};
	}
}
