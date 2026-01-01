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

import type {MessageEmbedProvider} from '~/database/CassandraTypes';

export class EmbedProvider {
	readonly name: string | null;
	readonly url: string | null;

	constructor(provider: MessageEmbedProvider) {
		this.name = provider.name ?? null;
		this.url = provider.url ?? null;
	}

	toMessageEmbedProvider(): MessageEmbedProvider {
		return {
			name: this.name,
			url: this.url,
		};
	}
}
