// SPDX-License-Identifier: AGPL-3.0-or-later

import {CONTENT_I18N_LOCALE_MESSAGES} from '@app/api/content_i18n/ContentI18nLocales';
import {CONTENT_I18N_MESSAGES} from '@app/api/content_i18n/ContentI18nMessages';
import {
	buildProfileBadges,
	type ProfileBadgeInput,
	type ProfileBadgeSubject,
} from '@app/api/user/profile_badges/ProfileBadgeBuilder';
import type {ProfileBadgeEnvironment} from '@app/api/user/profile_badges/ProfileBadgeRegistry';
import type {ProfileBadgeKey} from '@fluxer/constants/src/BadgeConstants';
import {PremiumFlags, UserFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {describe, expect, test} from 'vitest';

const HOSTED: ProfileBadgeEnvironment = {
	selfHosted: false,
	premiumTieringActive: true,
	productName: 'Fluxer',
	premiumName: 'Plutonium',
	premiumInfoUrl: null,
	marketingUrl: 'https://fluxer.example',
	mediaUrl: 'https://media.fluxer.example',
	customBadges: [],
};

const OCTOBER = new Date('2026-10-15T12:00:00Z');

function subject(overrides: Partial<ProfileBadgeSubject> = {}): ProfileBadgeSubject {
	return {
		flags: 0n,
		premiumFlags: 0,
		premiumType: null,
		premiumSince: null,
		premiumLifetimeSequence: null,
		badgeIds: new Set(),
		hiddenProfileBadges: new Set(),
		...overrides,
	};
}

const visionary = (premiumFlags = 0, premiumLifetimeSequence: number | null = 42) =>
	subject({
		premiumType: UserPremiumTypes.LIFETIME,
		premiumSince: OCTOBER,
		premiumLifetimeSequence,
		premiumFlags,
	});

function build(overrides: Partial<ProfileBadgeInput> = {}) {
	return buildProfileBadges({
		user: subject(),
		operatorPass: null,
		locale: 'en-US',
		environment: HOSTED,
		...overrides,
	});
}

const kinds = (overrides: Partial<ProfileBadgeInput> = {}) => build(overrides).map((badge) => badge.kind);

const everything = subject({
	flags: UserFlags.STAFF | UserFlags.PARTNER | UserFlags.BUG_HUNTER,
	premiumType: UserPremiumTypes.SUBSCRIPTION,
	premiumSince: OCTOBER,
});

describe('buildProfileBadges', () => {
	test('a hidden key drops the badge it names, and hiding a held Visionary also drops its mask', () => {
		const hide = (...keys: Array<ProfileBadgeKey>) => new Set(keys);
		expect(kinds({user: {...everything, hiddenProfileBadges: hide('staff', 'bug_hunter')}})).toEqual([
			'partner',
			'plutonium',
		]);
		expect(kinds({user: {...visionary(), hiddenProfileBadges: hide('visionary_number')}})).toEqual(['visionary']);
		const masked = visionary(PremiumFlags.BADGE_MASKED);
		expect(kinds({user: masked})).toEqual(['plutonium']);
		expect(kinds({user: {...masked, hiddenProfileBadges: hide('visionary')}})).toEqual([]);
		expect(kinds({user: {...masked, hiddenProfileBadges: hide('plutonium')}})).toEqual(['plutonium']);
	});

	test('restricted profiles keep only the flag badges', () => {
		expect(kinds({user: everything, restricted: true})).toEqual(['staff', 'partner', 'bug_hunter']);
		expect(kinds({user: visionary(), restricted: true})).toEqual([]);
	});

	test('hidden accounts show nothing', () => {
		expect(build({user: everything, profileHidden: true})).toEqual([]);
	});

	describe('localization', () => {
		test('the date is computed in UTC', () => {
			const user = subject({
				premiumType: UserPremiumTypes.SUBSCRIPTION,
				premiumSince: new Date('2026-10-31T23:30:00Z'),
			});
			expect(build({user})[0]!.label).toBe('Fluxer Plutonium subscriber since October 31, 2026');
		});

		test('every profile badge key is translated and keeps its variables in every locale', () => {
			const keys = Object.keys(CONTENT_I18N_MESSAGES).filter((key) => key.startsWith('profile_badge.'));
			expect(keys.length).toBeGreaterThan(0);
			const variables = (template: string) => [...template.matchAll(/\{(\w+)\}/g)].map((match) => match[1]).sort();
			for (const [locale, messages] of Object.entries(CONTENT_I18N_LOCALE_MESSAGES)) {
				for (const key of keys) {
					const template = (messages as Record<string, string | undefined>)[key];
					expect(template, `${locale} ${key}`).toBeTypeOf('string');
					expect(variables(template!), `${locale} ${key}`).toEqual(
						variables(CONTENT_I18N_MESSAGES[key as keyof typeof CONTENT_I18N_MESSAGES]),
					);
				}
			}
		});

		test('every locale renders every badge without falling back to the key', () => {
			const user = {...everything, premiumType: UserPremiumTypes.LIFETIME, premiumLifetimeSequence: 7};
			for (const locale of Object.keys(CONTENT_I18N_LOCALE_MESSAGES)) {
				const undated = {
					...user,
					premiumFlags: PremiumFlags.BADGE_TIMESTAMP_HIDDEN,
				};
				for (const badge of [...build({user, locale}), ...build({user: undated, locale})]) {
					expect(badge.label.startsWith('profile_badge.'), `${locale} ${badge.kind}`).toBe(false);
				}
			}
		});
	});
});
