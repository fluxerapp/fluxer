// SPDX-License-Identifier: AGPL-3.0-or-later

import {buildCustomEmojiURL} from '@app/features/expressions/utils/CustomEmojiImageUrl';
import type {GuildEmoji as WireGuildEmoji} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export class GuildEmoji {
	readonly id: string;
	readonly guildId: string;
	readonly name: string;
	readonly uniqueName: string;
	readonly allNamesString: string;
	readonly url: string;
	readonly animated: boolean;
	readonly nsfw: boolean;
	readonly user?: UserPartial;

	constructor(guildId: string, data: WireGuildEmoji) {
		this.id = data.id;
		this.guildId = guildId;
		this.name = data.name;
		this.uniqueName = data.name;
		this.allNamesString = `:${data.name}:`;
		this.url = buildCustomEmojiURL({id: data.id, animated: data.animated});
		this.animated = data.animated;
		this.nsfw = data.nsfw;
		this.user = data.user;
	}

	withUpdates(updates: Partial<WireGuildEmoji>): GuildEmoji {
		return new GuildEmoji(this.guildId, {
			id: updates.id ?? this.id,
			name: updates.name ?? this.name,
			animated: updates.animated ?? this.animated,
			nsfw: updates.nsfw ?? this.nsfw,
			user: updates.user ?? this.user,
		});
	}

	equals(other: GuildEmoji): boolean {
		return (
			this.id === other.id &&
			this.guildId === other.guildId &&
			this.name === other.name &&
			this.animated === other.animated &&
			this.nsfw === other.nsfw &&
			this.user?.id === other.user?.id
		);
	}

	toJSON(): WireGuildEmoji {
		return {
			id: this.id,
			name: this.name,
			animated: this.animated,
			nsfw: this.nsfw,
			user: this.user,
		};
	}

	static create(guildId: string, data: WireGuildEmoji): GuildEmoji {
		return new GuildEmoji(guildId, data);
	}
}
