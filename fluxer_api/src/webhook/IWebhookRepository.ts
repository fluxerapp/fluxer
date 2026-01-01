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

import type {Webhook} from '~/Models';

export abstract class IWebhookRepository {
	abstract findUnique(webhookId: bigint): Promise<Webhook | null>;
	abstract findByToken(webhookId: bigint, token: string): Promise<Webhook | null>;
	abstract create(data: {
		webhookId: bigint;
		token: string;
		type: number;
		guildId: bigint | null;
		channelId: bigint | null;
		creatorId: bigint | null;
		name: string;
		avatarHash: string | null;
	}): Promise<Webhook>;
	abstract update(
		webhookId: bigint,
		data: Partial<{
			token: string;
			type: number;
			guildId: bigint | null;
			channelId: bigint | null;
			creatorId: bigint | null;
			name: string;
			avatarHash: string | null;
		}>,
	): Promise<Webhook | null>;
	abstract delete(webhookId: bigint): Promise<void>;
	abstract listByGuild(guildId: bigint): Promise<Array<Webhook>>;
	abstract listByChannel(channelId: bigint): Promise<Array<Webhook>>;
	abstract countByGuild(guildId: bigint): Promise<number>;
	abstract countByChannel(channelId: bigint): Promise<number>;
}
