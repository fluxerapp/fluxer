// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildMemberListLayout,
	getGroupLayoutForRow,
	getTotalRowsFromLayout,
	type MemberListGroupLayout,
	type MemberListGroupSnapshot,
} from '@app/features/member/utils/MemberListLayout';
import {
	areNormalizedMemberListRangesCovered,
	type MemberListRange,
	type MemberListRanges,
	type NormalizedMemberListRanges,
	normalizeMemberListRanges,
} from '@app/features/member/utils/MemberListRangeUtils';

interface MemberListHydrationItemIndexes {
	has(index: number): boolean;
}

export interface MemberListHydrationInput {
	memberCount: number;
	groups: ReadonlyArray<MemberListGroupSnapshot>;
	itemIndexes: MemberListHydrationItemIndexes;
}

interface AppendHydratedMemberListRangeRequest {
	target: MemberListRanges;
	input: MemberListHydrationInput;
	layouts: ReadonlyArray<MemberListGroupLayout>;
	totalRows: number;
	range: MemberListRange;
}

type HydratedMemberListRowContext = Pick<AppendHydratedMemberListRangeRequest, 'input' | 'layouts' | 'totalRows'>;

function sanitizeMemberCount(memberCount: number): number {
	if (!Number.isFinite(memberCount)) {
		return 0;
	}
	return Math.max(0, Math.floor(memberCount));
}

function getHydrationTotalRows(input: MemberListHydrationInput, layouts: ReadonlyArray<MemberListGroupLayout>): number {
	if (layouts.length > 0) {
		return getTotalRowsFromLayout(layouts);
	}
	return sanitizeMemberCount(input.memberCount);
}

function isHydratedMemberListRow(context: HydratedMemberListRowContext, rowIndex: number): boolean {
	const {input, layouts, totalRows} = context;
	if (rowIndex < 0 || rowIndex >= totalRows) {
		return true;
	}
	const layout = layouts.length > 0 ? getGroupLayoutForRow(layouts, rowIndex) : null;
	if (layout != null && rowIndex === layout.headerRowIndex) {
		return true;
	}
	return input.itemIndexes.has(rowIndex);
}

function appendHydratedMemberListRange(request: AppendHydratedMemberListRangeRequest): void {
	const {target, range, totalRows} = request;
	const [rangeStart, rangeEnd] = range;
	let hydratedRangeStart: number | null = null;
	const inspectEnd = Math.min(rangeEnd, totalRows - 1);
	for (let rowIndex = rangeStart; rowIndex <= inspectEnd; rowIndex += 1) {
		if (isHydratedMemberListRow(request, rowIndex)) {
			if (hydratedRangeStart == null) {
				hydratedRangeStart = rowIndex;
			}
			continue;
		}
		if (hydratedRangeStart != null) {
			target.push([hydratedRangeStart, rowIndex - 1]);
			hydratedRangeStart = null;
		}
	}
	if (hydratedRangeStart != null) {
		target.push([hydratedRangeStart, rangeEnd]);
		return;
	}
	if (inspectEnd < rangeEnd) {
		target.push([inspectEnd + 1, rangeEnd]);
	}
}

export function getHydratedMemberListRanges(
	input: MemberListHydrationInput,
	ranges: MemberListRanges,
): NormalizedMemberListRanges {
	const normalizedRanges = normalizeMemberListRanges(ranges);
	if (normalizedRanges.length === 0) {
		return normalizedRanges;
	}
	const layouts = buildMemberListLayout(input.groups);
	const totalRows = getHydrationTotalRows(input, layouts);
	if (totalRows === 0) {
		return normalizedRanges;
	}
	const hydratedRanges: MemberListRanges = [];
	for (const range of normalizedRanges) {
		if (range[0] >= totalRows) {
			hydratedRanges.push(range);
			continue;
		}
		appendHydratedMemberListRange({target: hydratedRanges, input, layouts, totalRows, range});
	}
	return normalizeMemberListRanges(hydratedRanges);
}

export function isMemberListRangeHydrated(input: MemberListHydrationInput, ranges: MemberListRanges): boolean {
	const normalizedRanges = normalizeMemberListRanges(ranges);
	return areNormalizedMemberListRangesCovered(normalizedRanges, getHydratedMemberListRanges(input, normalizedRanges));
}
