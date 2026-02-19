/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {getDateFromUnixTimestampSeconds} from '@app/lib/markdown/utils/TimestampValidation';
import {describe, expect, test} from 'vitest';

describe('getDateFromUnixTimestampSeconds', () => {
	test('returns a valid date for normal unix timestamps', () => {
		const result = getDateFromUnixTimestampSeconds(1618953630);

		expect(result).toBeInstanceOf(Date);
		expect(result?.toISOString()).toBe('2021-04-20T21:20:30.000Z');
	});

	test('returns null for infinity', () => {
		expect(getDateFromUnixTimestampSeconds(Number.POSITIVE_INFINITY)).toBeNull();
	});

	test('returns null for NaN', () => {
		expect(getDateFromUnixTimestampSeconds(Number.NaN)).toBeNull();
	});

	test('returns null when timestamp is beyond js date range', () => {
		expect(getDateFromUnixTimestampSeconds(8640000000001)).toBeNull();
	});
});
