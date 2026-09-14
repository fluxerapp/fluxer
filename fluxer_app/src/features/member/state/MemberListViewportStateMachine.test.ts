// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createMemberListViewportSnapshot,
	getMemberListViewportStateValue,
	resolveMemberListViewportModel,
	selectMemberListViewportModel,
	transitionMemberListViewportSnapshot,
} from '@app/features/member/state/MemberListViewportStateMachine';
import {MEMBER_LIST_RANGE_MAX_END} from '@fluxer/constants/src/GatewayConstants';
import {describe, expect, it} from 'vitest';

const INITIAL_RANGE: Array<[number, number]> = [[0, 99]];

describe('MemberListViewportStateMachine', () => {
	it('starts in initial loading before the first gateway payload', () => {
		const snapshot = createMemberListViewportSnapshot({requestedRanges: INITIAL_RANGE});
		const model = selectMemberListViewportModel(snapshot);
		expect(getMemberListViewportStateValue(snapshot)).toBe('initialLoading');
		expect(model.isInitialLoading).toBe(true);
		expect(model.hasStableVirtualHeight).toBe(false);
		expect(model.requestedRanges).toEqual(INITIAL_RANGE);
		expect(model.renderRanges).toEqual([]);
	});

	it('enters virtualized mode after the first gateway payload and renders only the requested render ranges', () => {
		let snapshot = createMemberListViewportSnapshot({requestedRanges: INITIAL_RANGE});
		snapshot = transitionMemberListViewportSnapshot(snapshot, {
			type: 'memberList.storeUpdated',
			hasReceivedInitialPayload: true,
			subscribedRanges: [[0, 99]],
			totalRows: 500,
		});
		snapshot = transitionMemberListViewportSnapshot(snapshot, {
			type: 'memberList.rangesRequested',
			requestedRanges: [
				[300, 399],
				[200, 299],
			],
		});
		const model = selectMemberListViewportModel(snapshot);
		expect(getMemberListViewportStateValue(snapshot)).toBe('virtualized');
		expect(model.isInitialLoading).toBe(false);
		expect(model.hasStableVirtualHeight).toBe(true);
		expect(model.totalRows).toBe(500);
		expect(model.renderRanges).toEqual([
			[200, 299],
			[300, 399],
		]);
	});

	it('keeps virtualized mode when a scrollbar drag requests an unloaded window', () => {
		let snapshot = createMemberListViewportSnapshot({requestedRanges: INITIAL_RANGE});
		snapshot = transitionMemberListViewportSnapshot(snapshot, {
			type: 'memberList.storeUpdated',
			hasReceivedInitialPayload: true,
			subscribedRanges: [[0, 99]],
			totalRows: 500,
		});
		snapshot = transitionMemberListViewportSnapshot(snapshot, {
			type: 'memberList.rangesRequested',
			requestedRanges: [[400, 499]],
		});
		const model = selectMemberListViewportModel(snapshot);
		expect(getMemberListViewportStateValue(snapshot)).toBe('virtualized');
		expect(model.isInitialLoading).toBe(false);
		expect(model.renderRanges).toEqual([[400, 499]]);
	});

	it('remains virtualized after the store prunes the previous rows for the requested window', () => {
		let snapshot = createMemberListViewportSnapshot({requestedRanges: INITIAL_RANGE});
		snapshot = transitionMemberListViewportSnapshot(snapshot, {
			type: 'memberList.storeUpdated',
			hasReceivedInitialPayload: true,
			subscribedRanges: [[0, 99]],
			totalRows: 500,
		});
		snapshot = transitionMemberListViewportSnapshot(snapshot, {
			type: 'memberList.rangesRequested',
			requestedRanges: [[400, 499]],
		});
		snapshot = transitionMemberListViewportSnapshot(snapshot, {
			type: 'memberList.storeUpdated',
			hasReceivedInitialPayload: true,
			subscribedRanges: [[400, 499]],
			totalRows: 500,
		});
		const model = selectMemberListViewportModel(snapshot);
		expect(getMemberListViewportStateValue(snapshot)).toBe('virtualized');
		expect(model.isInitialLoading).toBe(false);
		expect(model.requestedRanges).toEqual([[400, 499]]);
		expect(model.renderRanges).toEqual([[400, 499]]);
	});

	it('does not treat an empty current store window as initial loading once payload metadata exists', () => {
		const model = resolveMemberListViewportModel({
			hasReceivedInitialPayload: true,
			requestedRanges: [[400, 499]],
			subscribedRanges: [],
			totalRows: 500,
		});
		expect(model.isInitialLoading).toBe(false);
		expect(model.hasStableVirtualHeight).toBe(true);
		expect(model.renderRanges).toEqual([[400, 499]]);
	});

	it('returns to initial loading when the store reports that no initial payload is present', () => {
		let snapshot = createMemberListViewportSnapshot({
			hasReceivedInitialPayload: true,
			requestedRanges: [[400, 499]],
			subscribedRanges: [[400, 499]],
			totalRows: 500,
		});
		expect(getMemberListViewportStateValue(snapshot)).toBe('virtualized');
		snapshot = transitionMemberListViewportSnapshot(snapshot, {
			type: 'memberList.storeUpdated',
			hasReceivedInitialPayload: false,
			subscribedRanges: [],
			totalRows: 0,
		});
		const model = selectMemberListViewportModel(snapshot);
		expect(getMemberListViewportStateValue(snapshot)).toBe('initialLoading');
		expect(model.renderRanges).toEqual([]);
		expect(model.totalRows).toBe(0);
	});

	it('reset clears payload state and subscribed ranges while preserving the caller-provided initial request', () => {
		let snapshot = createMemberListViewportSnapshot({
			hasReceivedInitialPayload: true,
			requestedRanges: [[400, 499]],
			subscribedRanges: [[400, 499]],
			totalRows: 500,
		});
		snapshot = transitionMemberListViewportSnapshot(snapshot, {
			type: 'memberList.reset',
			requestedRanges: [
				[100, 199],
				[0, 99],
			],
		});
		const model = selectMemberListViewportModel(snapshot);
		expect(getMemberListViewportStateValue(snapshot)).toBe('initialLoading');
		expect(model.requestedRanges).toEqual([
			[0, 99],
			[100, 199],
		]);
		expect(model.renderRanges).toEqual([]);
		expect(model.totalRows).toBe(0);
	});

	it('normalizes ranges and sanitizes total row counts at the state boundary', () => {
		const snapshot = createMemberListViewportSnapshot({
			hasReceivedInitialPayload: true,
			requestedRanges: [
				[80, 160],
				[-10, 20],
				[MEMBER_LIST_RANGE_MAX_END + 1, MEMBER_LIST_RANGE_MAX_END + 99],
			],
			subscribedRanges: [[220.9, 250.2]],
			totalRows: 12.9,
		});
		const model = selectMemberListViewportModel(snapshot);
		expect(model.totalRows).toBe(12);
		expect(model.requestedRanges).toEqual([
			[0, 20],
			[80, 160],
		]);
		expect(model.renderRanges).toEqual([
			[0, 20],
			[80, 160],
		]);
	});

	it('stress: repeated scrollbar window requests never collapse back to initial loading after payload', () => {
		let snapshot = createMemberListViewportSnapshot({requestedRanges: INITIAL_RANGE});
		snapshot = transitionMemberListViewportSnapshot(snapshot, {
			type: 'memberList.storeUpdated',
			hasReceivedInitialPayload: true,
			subscribedRanges: [[0, 99]],
			totalRows: 10_000,
		});
		for (let i = 0; i < 1_000; i += 1) {
			const page = (i * 37) % 100;
			const start = page * 100;
			const end = start + 99;
			snapshot = transitionMemberListViewportSnapshot(snapshot, {
				type: 'memberList.rangesRequested',
				requestedRanges: [[start, end]],
			});
			const model = selectMemberListViewportModel(snapshot);
			expect(getMemberListViewportStateValue(snapshot)).toBe('virtualized');
			expect(model.isInitialLoading).toBe(false);
			expect(model.renderRanges.some(([rangeStart, rangeEnd]) => rangeStart <= start && rangeEnd >= end)).toBe(true);
		}
	});
});
