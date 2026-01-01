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

const GROUP_DM_COLORS = ['#dc2626', '#ea580c', '#65a30d', '#2563eb', '#9333ea', '#db2777'];

const hashString = (value: string): number => {
	let hash = 0;
	for (let i = 0; i < value.length; i += 1) {
		hash = (hash * 31 + value.charCodeAt(i)) & 0xffffffff;
	}
	return hash;
};

export const getGroupDMAccentColor = (channelId: string): string => {
	if (!channelId) {
		return GROUP_DM_COLORS[0]!;
	}
	const hash = Math.abs(hashString(channelId));
	return GROUP_DM_COLORS[hash % GROUP_DM_COLORS.length]!;
};
