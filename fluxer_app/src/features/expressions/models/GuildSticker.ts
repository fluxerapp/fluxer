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
}
