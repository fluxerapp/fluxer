// SPDX-License-Identifier: AGPL-3.0-or-later

const GUILD_ICON_INITIALS_MAX_LENGTH = 4;

export const GuildInitialsLength = Object.freeze({
	SHORT: 'short',
	MEDIUM: 'medium',
	LONG: 'long',
} as const);

export type GuildInitialsLength = (typeof GuildInitialsLength)[keyof typeof GuildInitialsLength];

export function getInitialsFromName(name: string): string {
	const words = name.split(/\s+/u).filter(Boolean);
	return words
		.map((word) => Array.from(word)[0])
		.filter(Boolean)
		.join('');
}

export function truncateInitials(initials: string, maxLength: number): string {
	if (maxLength <= 0) return '';
	return Array.from(initials).slice(0, maxLength).join('');
}

export function getGuildIconDisplayInitials(initials: string): string {
	return truncateInitials(initials, GUILD_ICON_INITIALS_MAX_LENGTH);
}

export function getInitialsLength(initials: string): GuildInitialsLength {
	const length = Array.from(initials).length;
	if (length <= 2) return GuildInitialsLength.SHORT;
	if (length <= 4) return GuildInitialsLength.MEDIUM;
	return GuildInitialsLength.LONG;
}
