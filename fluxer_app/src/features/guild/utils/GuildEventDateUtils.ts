// SPDX-License-Identifier: AGPL-3.0-or-later

export interface GuildEventDateSnapshot {
	input: string;
	iso: string;
}

/** Keep the original instant alongside the minute-precision local editor value. */
export function snapshotGuildEventDate(iso: string): GuildEventDateSnapshot {
	const date = new Date(iso);
	const offset = date.getTimezoneOffset() * 60_000;
	return {
		input: new Date(date.getTime() - offset).toISOString().slice(0, 16),
		iso,
	};
}

export function guildEventDateToIso(value: string, original?: GuildEventDateSnapshot | null): string {
	// Re-parsing an unchanged local value loses seconds and can choose a different
	// instant in a repeated DST hour. Compare the captured input, not a recomputed
	// one, so changing the device timezone while editing also preserves the date.
	if (original && value === original.input) return original.iso;
	return new Date(value).toISOString();
}
