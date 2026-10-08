// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

interface MutualItemsDescriptorOptions {
	mutualCommunitiesCount: number;
	mutualGroupsCount: number;
	includeCount: boolean;
}

const MUTUAL_COMMUNITIES_GROUPS_DESCRIPTOR = msg({
	message: 'Mutual places',
	comment:
		'Short label for a combined list of mutual group DMs and mutual communities. Translate "places" as the most accurate local word for shared communities and groups.',
});
const MUTUAL_COMMUNITIES_GROUPS_COUNT_DESCRIPTOR = msg({
	message: 'Mutual places ({count})',
	comment:
		'Short tab or button label for a combined list of mutual group DMs and mutual communities. Preserve {count}; it is inserted by code. Translate "places" as the most accurate local word for shared communities and groups.',
});
const MUTUAL_COMMUNITIES_DESCRIPTOR = msg({
	message: 'Mutual communities',
	comment: 'Short label for a list of mutual communities in the user profile modal.',
});
const MUTUAL_COMMUNITIES_COUNT_DESCRIPTOR = msg({
	message: 'Mutual communities ({count})',
	comment:
		'Short tab or button label for a list of mutual communities in the user profile modal. Preserve {count}; it is inserted by code.',
});
const MUTUAL_GROUPS_DESCRIPTOR = msg({
	message: 'Mutual groups',
	comment: 'Short label for a list of mutual group DMs in the user profile modal.',
});
const MUTUAL_GROUPS_COUNT_DESCRIPTOR = msg({
	message: 'Mutual groups ({count})',
	comment:
		'Short tab or button label for a list of mutual group DMs in the user profile modal. Preserve {count}; it is inserted by code.',
});
export const NO_MUTUAL_COMMUNITIES_FOUND_DESCRIPTOR = msg({
	message: 'No mutual communities found.',
	comment: 'Empty state in the user profile modal when no shared communities are available.',
});

export function getMutualItemsDescriptor({
	mutualCommunitiesCount,
	mutualGroupsCount,
	includeCount,
}: MutualItemsDescriptorOptions): MessageDescriptor {
	if (mutualCommunitiesCount > 0 && mutualGroupsCount > 0) {
		return includeCount ? MUTUAL_COMMUNITIES_GROUPS_COUNT_DESCRIPTOR : MUTUAL_COMMUNITIES_GROUPS_DESCRIPTOR;
	}
	if (mutualGroupsCount > 0) {
		return includeCount ? MUTUAL_GROUPS_COUNT_DESCRIPTOR : MUTUAL_GROUPS_DESCRIPTOR;
	}
	return includeCount ? MUTUAL_COMMUNITIES_COUNT_DESCRIPTOR : MUTUAL_COMMUNITIES_DESCRIPTOR;
}
