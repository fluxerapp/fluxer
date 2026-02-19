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

import {formatTimestamp} from '@app/lib/markdown/utils/DateFormatter';
import {TimestampStyle} from '@fluxer/markdown_parser/src/types/Enums';
import {setupI18n} from '@lingui/core';
import {afterEach, describe, expect, test, vi} from 'vitest';

const i18n = setupI18n({locale: 'en-US', messages: {'en-US': {}}});

describe('formatTimestamp', () => {
	afterEach(() => {
		vi.useRealTimers();
	});

	test('returns the raw numeric value for non-finite timestamps', () => {
		const output = formatTimestamp(Number.POSITIVE_INFINITY, TimestampStyle.ShortDateTime, i18n);

		expect(output).toBe('Infinity');
	});

	test('returns the raw numeric value for out-of-range timestamps', () => {
		const output = formatTimestamp(8640000000001, TimestampStyle.ShortDateTime, i18n);

		expect(output).toBe('8640000000001');
	});

	test('still formats valid relative timestamps', () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-02-18T12:00:00.000Z'));

		const oneMinuteAgoTimestamp = Math.floor(new Date('2026-02-18T11:59:00.000Z').getTime() / 1000);
		const output = formatTimestamp(oneMinuteAgoTimestamp, TimestampStyle.RelativeTime, i18n);

		expect(output.length).toBeGreaterThan(0);
	});
});
