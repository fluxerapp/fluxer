// SPDX-License-Identifier: AGPL-3.0-or-later

function zonedParts(at: Date, timeZone: string): {year: number; month: number; day: number; midnight: boolean} {
	const parts = new Intl.DateTimeFormat('en-US', {
		timeZone,
		hourCycle: 'h23',
		year: 'numeric',
		month: 'numeric',
		day: 'numeric',
		hour: 'numeric',
		minute: 'numeric',
		second: 'numeric',
	}).formatToParts(at);
	const value = (type: Intl.DateTimeFormatPartTypes) => Number(parts.find((part) => part.type === type)?.value ?? 0);
	return {
		year: value('year'),
		month: value('month'),
		day: value('day'),
		midnight: value('hour') === 0 && value('minute') === 0 && value('second') === 0 && at.getTime() % 1000 === 0,
	};
}

function resolveTimeZone(timeZone: string | null | undefined): string {
	if (!timeZone) return 'UTC';
	try {
		new Intl.DateTimeFormat('en-US', {timeZone});
		return timeZone;
	} catch {
		return 'UTC';
	}
}

export function getOperatorPassUnlockDay(at: Date, now: Date, timeZone: string | null | undefined): string {
	const zone = resolveTimeZone(timeZone);
	const passed = at.getTime() <= now.getTime();
	const zoned = zonedParts(passed ? now : at, zone);
	const offset = passed || zoned.midnight ? 0 : 1;
	return new Date(Date.UTC(zoned.year, zoned.month - 1, zoned.day + offset)).toISOString().slice(0, 10);
}
