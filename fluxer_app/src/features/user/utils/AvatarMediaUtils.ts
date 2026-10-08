// SPDX-License-Identifier: AGPL-3.0-or-later
const DEFAULT_AVATAR_PRIMARY_COLORS = [0x4641d9, 0xf0b100, 0x00bba7, 0x2b7fff, 0xad46ff, 0x6a7282];
const DEFAULT_AVATAR_COUNT = BigInt(DEFAULT_AVATAR_PRIMARY_COLORS.length);
export const normalizeEndpoint = (endpoint: string): string => endpoint.replace(/\/$/, '');
export const parseAvatarHash = (value: string) => {
	const animated = value.startsWith('a_');
	const hash = animated ? value.slice(2) : value;
	return {animated, hash};
};
export const getDefaultAvatarIndex = (id: string): number => Number(BigInt(id) % DEFAULT_AVATAR_COUNT);
export const getDefaultAvatarPrimaryColor = (id: string): number =>
	DEFAULT_AVATAR_PRIMARY_COLORS[getDefaultAvatarIndex(id)];
