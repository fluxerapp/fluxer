// SPDX-License-Identifier: AGPL-3.0-or-later

import {hasSameResultIdentity, resolveRecomputedSelectedIndex} from '@app/features/search/state/QuickSwitcherSelection';
import type {QuickSwitcherResult} from '@app/features/search/state/QuickSwitcherTypes';
import {QuickSwitcherResultTypes} from '@fluxer/constants/src/QuickSwitcherConstants';
import {describe, expect, it} from 'vitest';

const result = (type: QuickSwitcherResult['type'], id: string) => ({type, id}) as QuickSwitcherResult;

const labelled = (type: QuickSwitcherResult['type'], id: string, title: string, subtitle?: string) =>
	({type, id, title, subtitle}) as QuickSwitcherResult;

describe('resolveRecomputedSelectedIndex', () => {
	it('follows the focused result when the list is reordered', () => {
		const previous = result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c3');
		const results = [
			result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c3'),
			result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c1'),
			result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c2'),
		];
		expect(resolveRecomputedSelectedIndex(previous, results, 0)).toBe(0);
	});

	it('keeps the focused result when entries are inserted above it', () => {
		const previous = result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c2');
		const results = [
			result(QuickSwitcherResultTypes.HEADER, 'h1'),
			result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c9'),
			result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c2'),
		];
		expect(resolveRecomputedSelectedIndex(previous, results, 1)).toBe(2);
	});

	it('falls back when the focused result disappears', () => {
		const previous = result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'gone');
		const results = [result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c1')];
		expect(resolveRecomputedSelectedIndex(previous, results, 0)).toBe(0);
	});

	it('does not confuse ids that repeat across result types', () => {
		const previous = result(QuickSwitcherResultTypes.GUILD, '42');
		const results = [result(QuickSwitcherResultTypes.TEXT_CHANNEL, '42'), result(QuickSwitcherResultTypes.GUILD, '42')];
		expect(resolveRecomputedSelectedIndex(previous, results, 0)).toBe(1);
	});

	it('uses the fallback when nothing was focused', () => {
		expect(resolveRecomputedSelectedIndex(undefined, [result(QuickSwitcherResultTypes.GUILD, 'g')], 0)).toBe(0);
	});
});

describe('hasSameResultIdentity', () => {
	it('treats a rebuilt list of the same rows in the same order as unchanged', () => {
		const previous = [
			result(QuickSwitcherResultTypes.HEADER, 'people'),
			result(QuickSwitcherResultTypes.USER, 'u1'),
			result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c1'),
		];
		const next = [
			result(QuickSwitcherResultTypes.HEADER, 'people'),
			result(QuickSwitcherResultTypes.USER, 'u1'),
			result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c1'),
		];
		expect(hasSameResultIdentity(previous, next)).toBe(true);
	});

	it('detects a reorder', () => {
		const previous = [
			result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c1'),
			result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c2'),
		];
		const next = [
			result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c2'),
			result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c1'),
		];
		expect(hasSameResultIdentity(previous, next)).toBe(false);
	});

	it('detects an insertion', () => {
		const previous = [result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c1')];
		const next = [
			result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c1'),
			result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c2'),
		];
		expect(hasSameResultIdentity(previous, next)).toBe(false);
	});

	it('detects a row whose type changed under the same id', () => {
		const previous = [result(QuickSwitcherResultTypes.TEXT_CHANNEL, '42')];
		const next = [result(QuickSwitcherResultTypes.VOICE_CHANNEL, '42')];
		expect(hasSameResultIdentity(previous, next)).toBe(false);
	});

	it('treats two empty lists as unchanged', () => {
		expect(hasSameResultIdentity([], [])).toBe(true);
	});

	it('detects a renamed row so the open list repaints', () => {
		const previous = [labelled(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c1', 'general')];
		const next = [labelled(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c1', 'general-chat')];
		expect(hasSameResultIdentity(previous, next)).toBe(false);
	});

	it('detects a changed subtitle under an unchanged title', () => {
		const previous = [labelled(QuickSwitcherResultTypes.USER, 'u1', 'ada', 'ada#0001')];
		const next = [labelled(QuickSwitcherResultTypes.USER, 'u1', 'ada', 'ada#0002')];
		expect(hasSameResultIdentity(previous, next)).toBe(false);
	});

	it('detects a subtitle that appeared where there was none', () => {
		const previous = [labelled(QuickSwitcherResultTypes.USER, 'u1', 'ada')];
		const next = [labelled(QuickSwitcherResultTypes.USER, 'u1', 'ada', 'ada#0001')];
		expect(hasSameResultIdentity(previous, next)).toBe(false);
	});

	it('detects a renamed section header', () => {
		const previous = [labelled(QuickSwitcherResultTypes.HEADER, 'people', 'People')];
		const next = [labelled(QuickSwitcherResultTypes.HEADER, 'people', 'Personnes')];
		expect(hasSameResultIdentity(previous, next)).toBe(false);
	});

	it('still treats an identically labelled rebuild as unchanged', () => {
		const previous = [
			labelled(QuickSwitcherResultTypes.HEADER, 'people', 'People'),
			labelled(QuickSwitcherResultTypes.USER, 'u1', 'ada', 'ada#0001'),
		];
		const next = [
			labelled(QuickSwitcherResultTypes.HEADER, 'people', 'People'),
			labelled(QuickSwitcherResultTypes.USER, 'u1', 'ada', 'ada#0001'),
		];
		expect(hasSameResultIdentity(previous, next)).toBe(true);
	});
});
