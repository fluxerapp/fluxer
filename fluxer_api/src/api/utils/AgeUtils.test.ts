// SPDX-License-Identifier: AGPL-3.0-or-later

import {getCachedAccountsConfig, setCachedAccountsConfig} from '@app/api/instance/AccountsConfigCache';
import {calculateAge, canUserAccessNsfwContent, parseDateOfBirth} from '@app/api/utils/AgeUtils';
import {DateOfBirthPolicies, type DateOfBirthPolicy} from '@fluxer/constants/src/AccountConfigConstants';
import {afterEach, describe, expect, it, vi} from 'vitest';

function setDateOfBirthPolicy(dateOfBirth: DateOfBirthPolicy): void {
	setCachedAccountsConfig({
		...getCachedAccountsConfig(),
		date_of_birth: dateOfBirth,
	});
}

const ADULT_DATE_OF_BIRTH = '1990-01-01';
const MINOR_DATE_OF_BIRTH = '2020-01-01';

describe('canUserAccessNsfwContent', () => {
	afterEach(() => {
		setCachedAccountsConfig(null);
	});

	it('allows a bot whatever the instance collects', () => {
		setDateOfBirthPolicy(DateOfBirthPolicies.REQUIRE_NEW_ACCOUNTS);
		expect(canUserAccessNsfwContent({isBot: true, dateOfBirth: null})).toBe(true);
	});

	it('allows an adult when the instance collects a date of birth', () => {
		setDateOfBirthPolicy(DateOfBirthPolicies.REQUIRE_NEW_ACCOUNTS);
		expect(canUserAccessNsfwContent({isBot: false, dateOfBirth: ADULT_DATE_OF_BIRTH})).toBe(true);
	});

	it('blocks a minor when the instance collects a date of birth', () => {
		setDateOfBirthPolicy(DateOfBirthPolicies.REQUIRE_NEW_ACCOUNTS);
		expect(canUserAccessNsfwContent({isBot: false, dateOfBirth: MINOR_DATE_OF_BIRTH})).toBe(false);
	});

	it('blocks a missing date of birth when the instance collects one', () => {
		setDateOfBirthPolicy(DateOfBirthPolicies.REQUIRE_NEW_ACCOUNTS);
		expect(canUserAccessNsfwContent({isBot: false, dateOfBirth: null})).toBe(false);
	});

	it('allows a missing date of birth when the instance collects none', () => {
		setDateOfBirthPolicy(DateOfBirthPolicies.OFF);
		expect(canUserAccessNsfwContent({isBot: false, dateOfBirth: null})).toBe(true);
	});

	it('allows an account with a minor date of birth when the instance collects none', () => {
		setDateOfBirthPolicy(DateOfBirthPolicies.OFF);
		expect(canUserAccessNsfwContent({isBot: false, dateOfBirth: MINOR_DATE_OF_BIRTH})).toBe(true);
	});
});

describe('parseDateOfBirth', () => {
	const at = (iso: string) => new Date(`${iso}T12:00:00Z`);
	const accepts = (value: string, today: string) => expect(parseDateOfBirth(value, at(today)).toString()).toBe(value);
	const refuses = (value: string, today: string) => expect(() => parseDateOfBirth(value, at(today))).toThrow();

	it('accepts someone exactly 120 today and refuses one day older', () => {
		accepts('1906-10-10', '2026-10-10');
		refuses('1906-10-09', '2026-10-10');
		refuses('1884-01-01', '2026-10-10');
	});

	it('refuses today, the future and impossible dates', () => {
		refuses('2026-10-10', '2026-10-10');
		refuses('2026-10-11', '2026-10-10');
		refuses('2030-01-01', '2026-10-10');
		refuses('2000-02-30', '2026-10-10');
		refuses('2001-02-29', '2026-10-10');
		refuses('2000-13-01', '2026-10-10');
		refuses('2000-1-01', '2026-10-10');
		accepts('2026-10-09', '2026-10-10');
	});

	it('handles leap-day birthdays and leap-day boundaries', () => {
		accepts('2000-02-29', '2026-10-10');
		accepts('1904-02-29', '2024-02-29');
		refuses('1904-02-28', '2024-02-29');
		accepts('1905-03-01', '2025-03-01');
		refuses('1905-02-28', '2025-03-01');
	});
});

describe('calculateAge', () => {
	const at = (iso: string) => new Date(`${iso}T12:00:00Z`);

	it('turns the minimum age on the birthday, not a day before', () => {
		expect(calculateAge('2013-10-11', at('2026-10-10'))).toBe(12);
		expect(calculateAge('2013-10-10', at('2026-10-10'))).toBe(13);
	});

	it('counts a leap-day birthday as reached on March 1 in other years', () => {
		expect(calculateAge('2012-02-29', at('2025-02-28'))).toBe(12);
		expect(calculateAge('2012-02-29', at('2025-03-01'))).toBe(13);
		expect(calculateAge('2012-02-29', at('2024-02-29'))).toBe(12);
	});

	describe('on a server outside UTC', () => {
		afterEach(() => {
			vi.unstubAllEnvs();
		});

		it('reads the UTC date just after UTC midnight in a zone behind UTC', () => {
			vi.stubEnv('TZ', 'America/Los_Angeles');
			const now = new Date('2026-10-10T00:30:00Z');
			expect(now.getDate()).toBe(9);
			expect(calculateAge('2013-10-10', now)).toBe(13);
			expect(calculateAge('2013-10-11', now)).toBe(12);
		});

		it('does not let a birthday that is only local pass just before UTC midnight', () => {
			vi.stubEnv('TZ', 'Europe/Stockholm');
			const now = new Date('2026-10-09T23:30:00Z');
			expect(now.getDate()).toBe(10);
			expect(calculateAge('2013-10-10', now)).toBe(12);
			expect(calculateAge({year: 2013, month: 10, day: 9}, now)).toBe(13);
		});
	});
});
