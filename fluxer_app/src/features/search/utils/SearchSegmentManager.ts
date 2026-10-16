// SPDX-License-Identifier: AGPL-3.0-or-later

type SearchSegmentType = 'user' | 'channel';

export interface SearchSegment {
	type: SearchSegmentType;
	filterKey: string;
	id: string;
	displayText: string;
	start: number;
	end: number;
}
