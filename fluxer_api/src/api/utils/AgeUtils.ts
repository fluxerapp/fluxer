// SPDX-License-Identifier: AGPL-3.0-or-later

import {LocalDate} from '@app/api/database/LocalDate';
import {instanceCollectsDateOfBirth} from '@app/api/instance/DateOfBirthCollectionCache';
import {isPlausibleDateOfBirth} from '@fluxer/constants/src/RegionalMinimumAge';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';

export function parseDateOfBirth(value: string, now: Date = new Date()): LocalDate {
	const match = /^(\d{4})-(\d{2})-(\d{2})$/.exec(value);
	const birth = match ? {year: Number(match[1]), month: Number(match[2]), day: Number(match[3])} : null;
	const today = {year: now.getUTCFullYear(), month: now.getUTCMonth() + 1, day: now.getUTCDate()};
	if (birth === null || !isPlausibleDateOfBirth(birth, today)) {
		throw InputValidationError.fromCode('date_of_birth', ValidationErrorCodes.INVALID_DATE_OF_BIRTH_FORMAT);
	}
	return new LocalDate(birth.year, birth.month, birth.day);
}

export function calculateAge(
	dateOfBirth:
		| {
				year: number;
				month: number;
				day: number;
		  }
		| string,
	today: Date = new Date(),
): number {
	let birth: {year: number; month: number; day: number};
	if (typeof dateOfBirth === 'string') {
		const [year, month, day] = dateOfBirth.split('-').map(Number);
		birth = {year, month, day};
	} else {
		birth = dateOfBirth;
	}
	const age = today.getUTCFullYear() - birth.year;
	const monthDiff = today.getUTCMonth() + 1 - birth.month;
	const dayDiff = today.getUTCDate() - birth.day;
	return monthDiff < 0 || (monthDiff === 0 && dayDiff < 0) ? age - 1 : age;
}

export function isUserAdult(
	dateOfBirth?:
		| {
				year: number;
				month: number;
				day: number;
		  }
		| string
		| null,
): boolean {
	if (!dateOfBirth) return false;
	return calculateAge(dateOfBirth) >= 18;
}

interface NsfwEligibilityUser {
	isBot: boolean;
	dateOfBirth?:
		| {
				year: number;
				month: number;
				day: number;
		  }
		| string
		| null;
}

export function canUserAccessNsfwContent(user: NsfwEligibilityUser): boolean {
	if (user.isBot) {
		return true;
	}
	if (!instanceCollectsDateOfBirth()) {
		return true;
	}
	return isUserAdult(user.dateOfBirth);
}
