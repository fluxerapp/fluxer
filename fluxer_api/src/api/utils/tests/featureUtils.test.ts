// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	applyProtectedOverwriteBits,
	applyProtectedRolePermissions,
	parseClientFeaturesHeader,
} from '@app/api/utils/featureUtils';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it} from 'vitest';

const VCM = Permissions.VIEW_CHANNEL_MEMBERS;
const FEATURE = 'view_channel_members_permission';
const OTHER_BIT = Permissions.MANAGE_ROLES;

describe('parseClientFeaturesHeader', () => {
	it('returns empty set for null/undefined/empty', () => {
		expect(parseClientFeaturesHeader(null).size).toBe(0);
		expect(parseClientFeaturesHeader(undefined).size).toBe(0);
		expect(parseClientFeaturesHeader('').size).toBe(0);
	});
	it('parses comma-separated features and lowercases', () => {
		const features = parseClientFeaturesHeader('view_channel_members_permission, FOO_BAR');
		expect(features.has('view_channel_members_permission')).toBe(true);
		expect(features.has('foo_bar')).toBe(true);
	});
	it('rejects invalid characters and over-long names', () => {
		const tooLong = 'a'.repeat(65);
		const features = parseClientFeaturesHeader(`good, bad-name, ${tooLong}, also_good`);
		expect(features.has('good')).toBe(true);
		expect(features.has('also_good')).toBe(true);
		expect(features.has('bad-name')).toBe(false);
		expect(features.has(tooLong)).toBe(false);
	});
	it('caps at 64 entries to bound work', () => {
		const tokens = Array.from({length: 200}, (_, i) => `feat_${i}`).join(',');
		const features = parseClientFeaturesHeader(tokens);
		expect(features.size).toBe(64);
	});
});

describe('applyProtectedRolePermissions', () => {
	it('preserves protected bits when client lacks the feature', () => {
		const existing = VCM | OTHER_BIT;
		const requested = OTHER_BIT;
		const result = applyProtectedRolePermissions(requested, existing, new Set());
		expect((result & VCM) === VCM).toBe(true);
		expect((result & OTHER_BIT) === OTHER_BIT).toBe(true);
	});
	it('respects client-driven clears when feature is opted in', () => {
		const existing = VCM | OTHER_BIT;
		const requested = OTHER_BIT;
		const result = applyProtectedRolePermissions(requested, existing, new Set([FEATURE]));
		expect((result & VCM) === 0n).toBe(true);
		expect((result & OTHER_BIT) === OTHER_BIT).toBe(true);
	});
	it('does not invent bits the existing value did not have', () => {
		const existing = OTHER_BIT;
		const requested = OTHER_BIT;
		const result = applyProtectedRolePermissions(requested, existing, new Set());
		expect((result & VCM) === 0n).toBe(true);
	});
	it('also blocks an old client from setting the bit (the bit is fully invisible without the feature)', () => {
		const existing = 0n;
		const requested = VCM;
		const result = applyProtectedRolePermissions(requested, existing, new Set());
		expect((result & VCM) === 0n).toBe(true);
	});
	it('lets a feature-aware client set the bit', () => {
		const existing = 0n;
		const requested = VCM;
		const result = applyProtectedRolePermissions(requested, existing, new Set([FEATURE]));
		expect((result & VCM) === VCM).toBe(true);
	});
});

describe('applyProtectedOverwriteBits', () => {
	it('preserves bit on allow when feature absent', () => {
		const existing = {allow: VCM, deny: 0n};
		const requested = {allow: 0n, deny: 0n};
		const result = applyProtectedOverwriteBits(requested, existing, new Set());
		expect((result.allow & VCM) === VCM).toBe(true);
		expect((result.deny & VCM) === 0n).toBe(true);
	});
	it('preserves bit on deny when feature absent (the security-relevant case)', () => {
		const existing = {allow: 0n, deny: VCM};
		const requested = {allow: 0n, deny: 0n};
		const result = applyProtectedOverwriteBits(requested, existing, new Set());
		expect((result.deny & VCM) === VCM).toBe(true);
	});
	it('preserves the bit on whichever side it was set', () => {
		const existing = {allow: VCM | OTHER_BIT, deny: 0n};
		const requested = {allow: OTHER_BIT, deny: 0n};
		const result = applyProtectedOverwriteBits(requested, existing, new Set());
		expect((result.allow & VCM) === VCM).toBe(true);
		expect((result.allow & OTHER_BIT) === OTHER_BIT).toBe(true);
	});
	it('respects client-driven clears when feature is opted in (allow)', () => {
		const existing = {allow: VCM, deny: 0n};
		const requested = {allow: 0n, deny: 0n};
		const result = applyProtectedOverwriteBits(requested, existing, new Set([FEATURE]));
		expect((result.allow & VCM) === 0n).toBe(true);
	});
	it('respects client-driven clears when feature is opted in (deny)', () => {
		const existing = {allow: 0n, deny: VCM};
		const requested = {allow: 0n, deny: 0n};
		const result = applyProtectedOverwriteBits(requested, existing, new Set([FEATURE]));
		expect((result.deny & VCM) === 0n).toBe(true);
	});
	it('handles missing existing overwrite (treat as 0n) and blocks set without feature', () => {
		const existing = {allow: 0n, deny: 0n};
		const requested = {allow: VCM, deny: 0n};
		const result = applyProtectedOverwriteBits(requested, existing, new Set());
		expect((result.allow & VCM) === 0n).toBe(true);
	});
	it('lets a feature-aware client set the bit on a fresh overwrite', () => {
		const existing = {allow: 0n, deny: 0n};
		const requested = {allow: VCM, deny: 0n};
		const result = applyProtectedOverwriteBits(requested, existing, new Set([FEATURE]));
		expect((result.allow & VCM) === VCM).toBe(true);
	});
	it('does not flip bits between allow and deny', () => {
		const existing = {allow: 0n, deny: VCM};
		const requested = {allow: 0n, deny: 0n};
		const result = applyProtectedOverwriteBits(requested, existing, new Set());
		expect((result.allow & VCM) === 0n).toBe(true);
		expect((result.deny & VCM) === VCM).toBe(true);
	});
});
