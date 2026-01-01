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

import {StickerFormatTypes} from '~/Constants';
import type {UserPartial} from '~/records/UserRecord';
import * as AvatarUtils from '~/utils/AvatarUtils';

export type GuildSticker = Readonly<{
	id: string;
	name: string;
	description: string;
	tags: Array<string>;
	format_type: number;
	user?: UserPartial;
}>;

export interface GuildStickerWithUser extends GuildSticker {
	user?: UserPartial;
}

export function isStickerAnimated(sticker: GuildSticker) {
	return sticker.format_type === StickerFormatTypes.GIF;
}

export class GuildStickerRecord {
	readonly id: string;
	readonly guildId: string;
	readonly name: string;
	readonly description: string;
	readonly tags: ReadonlyArray<string>;
	readonly url: string;
	readonly formatType: number;
	readonly user?: UserPartial;

	constructor(guildId: string, data: GuildSticker) {
		this.id = data.id;
		this.guildId = guildId;
		this.name = data.name;
		this.description = data.description;
		this.tags = Object.freeze([...data.tags]);
		this.url = AvatarUtils.getStickerURL({
			id: data.id,
			animated: isStickerAnimated(data),
			size: 320,
		});
		this.formatType = data.format_type;
		this.user = data.user;
	}

	isAnimated() {
		return isStickerAnimated(this.toJSON());
	}

	withUpdates(updates: Partial<GuildSticker>): GuildStickerRecord {
		return new GuildStickerRecord(this.guildId, {
			id: updates.id ?? this.id,
			name: updates.name ?? this.name,
			description: updates.description ?? this.description,
			tags: updates.tags ?? [...this.tags],
			format_type: updates.format_type ?? this.formatType,
			user: updates.user ?? this.user,
		});
	}

	equals(other: GuildStickerRecord): boolean {
		return (
			this.id === other.id &&
			this.guildId === other.guildId &&
			this.name === other.name &&
			this.description === other.description &&
			JSON.stringify(this.tags) === JSON.stringify(other.tags) &&
			this.formatType === other.formatType &&
			this.user?.id === other.user?.id
		);
	}

	toJSON(): GuildSticker {
		return {
			id: this.id,
			name: this.name,
			description: this.description,
			tags: [...this.tags],
			format_type: this.formatType,
			user: this.user,
		};
	}

	static create(guildId: string, data: GuildSticker): GuildStickerRecord {
		return new GuildStickerRecord(guildId, data);
	}
}
