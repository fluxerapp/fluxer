// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import type {GuildSticker as WireGuildSticker} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export class GuildSticker {
	readonly id: string;
	readonly guildId: string;
	readonly name: string;
	readonly description: string;
	readonly tags: ReadonlyArray<string>;
	readonly url: string;
	readonly animated: boolean;
	readonly user?: UserPartial;

	constructor(guildId: string, data: WireGuildSticker) {
		this.id = data.id;
		this.guildId = guildId;
		this.name = data.name;
		this.description = data.description;
		this.tags = Object.freeze([...data.tags]);
		this.url = AvatarUtils.getStickerURL({
			id: data.id,
			animated: data.animated,
			size: 320,
		});
		this.animated = data.animated;
		this.user = data.user;
	}

	withUpdates(updates: Partial<WireGuildSticker>): GuildSticker {
		return new GuildSticker(this.guildId, {
			id: updates.id ?? this.id,
			name: updates.name ?? this.name,
			description: updates.description ?? this.description,
			tags: updates.tags ?? [...this.tags],
			animated: updates.animated ?? this.animated,
			user: updates.user ?? this.user,
		});
	}

	equals(other: GuildSticker): boolean {
		return (
			this.id === other.id &&
			this.guildId === other.guildId &&
			this.name === other.name &&
			this.description === other.description &&
			JSON.stringify(this.tags) === JSON.stringify(other.tags) &&
			this.animated === other.animated &&
			this.user?.id === other.user?.id
		);
	}

	toJSON(): WireGuildSticker {
		return {
			id: this.id,
			name: this.name,
			description: this.description,
			tags: [...this.tags],
			animated: this.animated,
			user: this.user,
		};
	}

	static create(guildId: string, data: WireGuildSticker): GuildSticker {
		return new GuildSticker(guildId, data);
	}
}
