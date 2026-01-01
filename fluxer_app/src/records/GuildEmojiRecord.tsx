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

import type {UserPartial} from '~/records/UserRecord';
import * as AvatarUtils from '~/utils/AvatarUtils';

export type GuildEmoji = Readonly<{
	id: string;
	name: string;
	animated: boolean;
	user?: UserPartial;
}>;

export interface GuildEmojiWithUser extends GuildEmoji {
	user?: UserPartial;
}

export class GuildEmojiRecord {
	readonly id: string;
	readonly guildId: string;
	readonly name: string;
	readonly uniqueName: string;
	readonly allNamesString: string;
	readonly url: string;
	readonly animated: boolean;
	readonly user?: UserPartial;

	constructor(guildId: string, data: GuildEmoji) {
		this.id = data.id;
		this.guildId = guildId;
		this.name = data.name;
		this.uniqueName = data.name;
		this.allNamesString = `:${data.name}:`;
		this.url = AvatarUtils.getEmojiURL({
			id: data.id,
			animated: data.animated,
		});
		this.animated = data.animated;
		this.user = data.user;
	}

	withUpdates(updates: Partial<GuildEmoji>): GuildEmojiRecord {
		return new GuildEmojiRecord(this.guildId, {
			id: updates.id ?? this.id,
			name: updates.name ?? this.name,
			animated: updates.animated ?? this.animated,
			user: updates.user ?? this.user,
		});
	}

	equals(other: GuildEmojiRecord): boolean {
		return (
			this.id === other.id &&
			this.guildId === other.guildId &&
			this.name === other.name &&
			this.animated === other.animated &&
			this.user?.id === other.user?.id
		);
	}

	toJSON(): GuildEmoji {
		return {
			id: this.id,
			name: this.name,
			animated: this.animated,
			user: this.user,
		};
	}

	static create(guildId: string, data: GuildEmoji): GuildEmojiRecord {
		return new GuildEmojiRecord(guildId, data);
	}
}
