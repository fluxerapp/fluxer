// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, EmojiID, GuildID, RoleID, StickerID, UserID} from '@app/api/BrandedTypes';

export function toIdString(
	value: GuildID | ChannelID | RoleID | UserID | EmojiID | StickerID | bigint | string | null | undefined,
): string | null {
	if (value === null || value === undefined) {
		return null;
	}
	return value.toString();
}

function toIdArray<
	T extends {
		toString(): string;
	},
>(values: Array<T> | Set<T> | null | undefined): Array<string> {
	if (!values) return [];
	return Array.from(values).map((v) => v.toString());
}

export function toSortedIdArray<
	T extends {
		toString(): string;
	},
>(values: Array<T> | Set<T> | null | undefined): Array<string> {
	return toIdArray(values).sort();
}
