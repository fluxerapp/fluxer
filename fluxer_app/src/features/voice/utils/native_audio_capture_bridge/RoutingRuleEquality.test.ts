// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type NativeAudioRoutingRule,
	nativeAudioRoutingRulesEqual,
} from '@app/features/voice/utils/native_audio_capture_bridge/shared';
import {describe, expect, it} from 'vitest';

const base: NativeAudioRoutingRule = {
	include: [{'application.name': 'Firefox'}],
	exclude: [{'node.name': 'fluxer'}],
	ignoreDevices: true,
};

describe('nativeAudioRoutingRulesEqual', () => {
	it('treats a rule as equal to itself and to a structural copy', () => {
		expect(nativeAudioRoutingRulesEqual(base, base)).toBe(true);
		expect(nativeAudioRoutingRulesEqual(base, {...base, include: [{'application.name': 'Firefox'}]})).toBe(true);
	});

	it('ignores the order of include and exclude patterns', () => {
		const a: NativeAudioRoutingRule = {include: [{'application.name': 'A'}, {'application.name': 'B'}]};
		const b: NativeAudioRoutingRule = {include: [{'application.name': 'B'}, {'application.name': 'A'}]};
		expect(nativeAudioRoutingRulesEqual(a, b)).toBe(true);
	});

	it('separates a missing list from an empty one only when the contents differ', () => {
		expect(nativeAudioRoutingRulesEqual({include: []}, {})).toBe(true);
		expect(nativeAudioRoutingRulesEqual({include: [{'application.name': 'A'}]}, {})).toBe(false);
	});

	it('detects a changed selection, which is the case that must restart routing', () => {
		expect(nativeAudioRoutingRulesEqual(base, {...base, include: [{'application.name': 'Chromium'}]})).toBe(false);
		expect(nativeAudioRoutingRulesEqual(base, {...base, include: []})).toBe(false);
		expect(nativeAudioRoutingRulesEqual(base, {...base, exclude: []})).toBe(false);
	});

	it('detects a changed tuning flag', () => {
		expect(nativeAudioRoutingRulesEqual(base, {...base, ignoreDevices: false})).toBe(false);
		expect(nativeAudioRoutingRulesEqual(base, {...base, onlySpeakers: true})).toBe(false);
		expect(nativeAudioRoutingRulesEqual(base, {...base, onlyDefaultSpeakers: true})).toBe(false);
		expect(nativeAudioRoutingRulesEqual(base, {...base, ignoreInputMedia: true})).toBe(false);
	});

	it('treats an undefined flag as false rather than as a difference', () => {
		expect(nativeAudioRoutingRulesEqual({include: []}, {include: [], onlySpeakers: false})).toBe(true);
	});

	it('never reports equality when a rule is missing', () => {
		expect(nativeAudioRoutingRulesEqual(undefined, base)).toBe(false);
		expect(nativeAudioRoutingRulesEqual(base, undefined)).toBe(false);
		expect(nativeAudioRoutingRulesEqual(undefined, undefined)).toBe(true);
	});
});
