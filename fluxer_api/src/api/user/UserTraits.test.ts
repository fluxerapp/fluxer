// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {isDerivedTrait, isServerManagedTrait, resolveAssignedTraits} from './UserTraits';

describe('UserTraits', () => {
	it('treats premium as derived rather than assignable', () => {
		expect(isDerivedTrait('premium')).toBe(true);
		expect(isDerivedTrait('beta-tester')).toBe(false);
	});

	it('recognises every trait shape the server manages', () => {
		expect(isServerManagedTrait('sso')).toBe(true);
		expect(isServerManagedTrait('sso:acme')).toBe(true);
		expect(isServerManagedTrait('sso_provider:0123456789abcdef')).toBe(true);
		expect(isServerManagedTrait('sso_identity:0123456789abcdef')).toBe(true);
		expect(isServerManagedTrait('registration_pending_approval')).toBe(true);
		expect(isServerManagedTrait('registration_rejected')).toBe(true);
		expect(isServerManagedTrait('beta-tester')).toBe(false);
	});

	it('keeps the operator traits it was given', () => {
		expect([...resolveAssignedTraits([], ['beta-tester', 'experimental'])]).toEqual(['beta-tester', 'experimental']);
	});

	it('drops premium because nothing reads the stored value', () => {
		expect([...resolveAssignedTraits([], ['premium'])]).toEqual([]);
		expect([...resolveAssignedTraits([], ['beta-tester', 'premium'])]).toEqual(['beta-tester']);
	});

	it('keeps a premium user premium when the operator saves other traits', () => {
		expect([...resolveAssignedTraits(['premium'], ['beta-tester'])]).toEqual(['beta-tester']);
	});

	it('preserves server managed traits the operator did not send', () => {
		const resolved = resolveAssignedTraits(
			['sso', 'sso:acme', 'sso_identity:0123456789abcdef', 'beta-tester'],
			['experimental'],
		);
		expect([...resolved].sort()).toEqual(['experimental', 'sso', 'sso:acme', 'sso_identity:0123456789abcdef']);
	});

	it('keeps a pending approval user pending when their traits are cleared', () => {
		expect([...resolveAssignedTraits(['registration_pending_approval'], [])]).toEqual([
			'registration_pending_approval',
		]);
	});

	it('refuses to let an operator forge a server managed trait', () => {
		expect([...resolveAssignedTraits([], ['sso', 'sso_identity:forged'])]).toEqual([]);
	});

	it('ignores empty trait names', () => {
		expect([...resolveAssignedTraits([], ['', 'beta-tester'])]).toEqual(['beta-tester']);
	});
});
