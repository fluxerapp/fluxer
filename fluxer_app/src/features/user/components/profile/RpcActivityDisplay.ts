// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserActivity} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

const ACTIVITY_VERBS = new Map<number, string>([
	[0, 'Playing'],
	[2, 'Listening to'],
	[3, 'Watching'],
	[5, 'Competing in'],
]);

export function getRpcActivityVerb(activity: UserActivity): string {
	return ACTIVITY_VERBS.get(activity.type) ?? 'Active in';
}

export function getRpcActivitySubtitle(activity: UserActivity): string | null {
	const lines = [activity.details, activity.state, activity.assets?.large_text].filter((value): value is string =>
		Boolean(value?.trim()),
	);
	if (!lines.length) return null;
	return Array.from(new Set(lines)).join(' · ');
}
