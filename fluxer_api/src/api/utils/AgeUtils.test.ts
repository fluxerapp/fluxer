// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, describe, expect, it} from 'vitest';
import {setCachedDateOfBirthCollection} from '../instance/DateOfBirthCollectionCache';
import {canUserAccessNsfwContent} from './AgeUtils';

const ADULT_DATE_OF_BIRTH = '1990-01-01';
const MINOR_DATE_OF_BIRTH = '2020-01-01';

describe('canUserAccessNsfwContent', () => {
	afterEach(() => {
		setCachedDateOfBirthCollection(true);
	});

	it('allows a bot whatever the instance collects', () => {
		setCachedDateOfBirthCollection(true);
		expect(canUserAccessNsfwContent({isBot: true, dateOfBirth: null})).toBe(true);
	});

	it('allows an adult when the instance collects a date of birth', () => {
		setCachedDateOfBirthCollection(true);
		expect(canUserAccessNsfwContent({isBot: false, dateOfBirth: ADULT_DATE_OF_BIRTH})).toBe(true);
	});

	it('blocks a minor when the instance collects a date of birth', () => {
		setCachedDateOfBirthCollection(true);
		expect(canUserAccessNsfwContent({isBot: false, dateOfBirth: MINOR_DATE_OF_BIRTH})).toBe(false);
	});

	it('blocks a missing date of birth when the instance collects one', () => {
		setCachedDateOfBirthCollection(true);
		expect(canUserAccessNsfwContent({isBot: false, dateOfBirth: null})).toBe(false);
	});

	it('allows a missing date of birth when the instance collects none', () => {
		setCachedDateOfBirthCollection(false);
		expect(canUserAccessNsfwContent({isBot: false, dateOfBirth: null})).toBe(true);
	});

	it('allows an account with a minor date of birth when the instance collects none', () => {
		setCachedDateOfBirthCollection(false);
		expect(canUserAccessNsfwContent({isBot: false, dateOfBirth: MINOR_DATE_OF_BIRTH})).toBe(true);
	});
});
