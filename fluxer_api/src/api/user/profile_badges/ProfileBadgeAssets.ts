// SPDX-License-Identifier: AGPL-3.0-or-later

export const PROFILE_BADGE_ASSETS = {
	staff: '42cd5fbc480b7847',
	partner: 'b58f25b2d2725ad4',
	bug_hunter: '5d3b4eacacb0ca67',
	operator_pass: '8120d774496aa5f7',
	visionary: '90fd8b9b5421bb6a',
	plutonium: '80bb0940d19fa614',
	premium: 'c61a26d1f983369c',
} as const;

const OPERATOR_PASS_COUNT_BADGE_ASSETS = {
	'2': '0b2c8e24de04c3f1',
	'3': '124e3a5e2027dea1',
	'4': '7377e2a1c9d4d2b6',
	'5': '4e621949f056b1a8',
	'6': 'b3e72421da3c396b',
	'7': 'c78ee3cafcb561fb',
	'8': '2ffe4ecffcdba3cf',
	'9': 'a86a8f878dd99a5e',
	'9+': 'bd36eed73a899545',
} as const;

const OPERATOR_PASS_SMALL_BADGE_ASSETS = {
	'9+': '88fe85ac14a20fb5',
} as const;

export function operatorPassSmallBadgeAsset(count: number | null): string | null {
	return count != null && count > 9 ? OPERATOR_PASS_SMALL_BADGE_ASSETS['9+'] : null;
}

export function operatorPassBadgeAsset(count: number | null): string {
	if (count == null || count < 2) return PROFILE_BADGE_ASSETS.operator_pass;
	return count > 9 ? OPERATOR_PASS_COUNT_BADGE_ASSETS['9+'] : OPERATOR_PASS_COUNT_BADGE_ASSETS[String(count) as '2'];
}
