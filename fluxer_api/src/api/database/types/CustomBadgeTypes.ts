// SPDX-License-Identifier: AGPL-3.0-or-later

export interface CustomBadgeRow {
	badge_id: bigint;
	type: string;
	position: bigint;
	name: string;
	description: string | null;
	asset: string;
	href: string | null;
	updated_at: Date;
}

export const CUSTOM_BADGE_COLUMNS = [
	'badge_id',
	'type',
	'position',
	'name',
	'description',
	'asset',
	'href',
	'updated_at',
] as const satisfies ReadonlyArray<keyof CustomBadgeRow>;
