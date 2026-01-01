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

import type {ChannelID, EmojiID, GuildID, RoleID, StickerID, UserID} from '~/BrandedTypes';

export function toIdString(
	value: GuildID | ChannelID | RoleID | UserID | EmojiID | StickerID | bigint | string | null | undefined,
): string | null {
	if (value === null || value === undefined) {
		return null;
	}

	return value.toString();
}

function toIdArray<T extends {toString(): string}>(values: Array<T> | Set<T> | null | undefined): Array<string> {
	if (!values) return [];
	return Array.from(values).map((v) => v.toString());
}

export function toSortedIdArray<T extends {toString(): string}>(
	values: Array<T> | Set<T> | null | undefined,
): Array<string> {
	return toIdArray(values).sort();
}
