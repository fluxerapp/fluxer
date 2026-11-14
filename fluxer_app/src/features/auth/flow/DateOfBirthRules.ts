// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {isPlausibleDateOfBirth, MAXIMUM_PLAUSIBLE_AGE} from '@fluxer/constants/src/RegionalMinimumAge';

function localToday(now: Date) {
	return {year: now.getFullYear(), month: now.getMonth() + 1, day: now.getDate()};
}

export function isPlausibleDateOfBirthInput(year: string, month: string, day: string, now: Date = new Date()): boolean {
	if (!/^\d{1,4}$/.test(year) || !/^\d{1,2}$/.test(month) || !/^\d{1,2}$/.test(day)) return false;
	return isPlausibleDateOfBirth({year: Number(year), month: Number(month), day: Number(day)}, localToday(now));
}

export function dateOfBirthYearRange(now: Date = new Date()): {newest: number; oldest: number} {
	const year = now.getFullYear();
	return {newest: year, oldest: year - MAXIMUM_PLAUSIBLE_AGE};
}

export function asksDateOfBirthAtSignUp(snapshot: RuntimeConfigSnapshot): boolean {
	return snapshot.appPublic.registration.collect_date_of_birth && snapshot.features.date_of_birth_step !== true;
}
