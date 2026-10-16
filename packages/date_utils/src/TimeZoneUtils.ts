// SPDX-License-Identifier: AGPL-3.0-or-later

import {getTimeZones, type TimeZone, timeZonesNames} from '@vvo/tzdb';

const UTC_TIME_ZONE_ID = 'UTC';

interface TimeZoneDisplayOption {
	readonly value: string;
	readonly label: string;
	readonly searchText: string;
	readonly offsetMinutes: number;
}

const supportedTimeZoneIds = new Set<string>([...timeZonesNames, UTC_TIME_ZONE_ID]);

function cleanTimeZoneId(timeZone: string | null | undefined): string | null {
	const value = timeZone?.trim();
	return value ? value : null;
}

function getDisplayTimeZones(): Array<TimeZone> {
	return getTimeZones({includeUtc: true}).map((record) =>
		record.group.includes(UTC_TIME_ZONE_ID) ? {...record, name: UTC_TIME_ZONE_ID} : record,
	);
}

function findTimeZone(timeZone: string): TimeZone | null {
	for (const record of getDisplayTimeZones()) {
		if (record.name === timeZone || record.group.includes(timeZone)) {
			return record;
		}
	}
	return null;
}

function getRuntimeOffsetMinutes(timeZone: string): number | null {
	const now = new Date();
	try {
		const parts = new Intl.DateTimeFormat('en-US', {
			timeZone,
			year: 'numeric',
			month: '2-digit',
			day: '2-digit',
			hour: '2-digit',
			minute: '2-digit',
			second: '2-digit',
			hourCycle: 'h23',
		}).formatToParts(now);
		const values = new Map(parts.map((part) => [part.type, part.value]));
		const year = Number(values.get('year'));
		const month = Number(values.get('month'));
		const day = Number(values.get('day'));
		const hour = Number(values.get('hour'));
		const minute = Number(values.get('minute'));
		const second = Number(values.get('second'));
		if ([year, month, day, hour, minute, second].some((value) => Number.isNaN(value))) {
			return null;
		}
		const zonedAsUtc = Date.UTC(year, month - 1, day, hour, minute, second);
		return Math.round((zonedAsUtc - now.getTime()) / 60_000);
	} catch {
		return null;
	}
}

export function isSupportedTimeZoneId(timeZone: string | null | undefined): boolean {
	const value = cleanTimeZoneId(timeZone);
	return value != null && supportedTimeZoneIds.has(value);
}

export function getCurrentTimeZoneOffsetMinutes(timeZone: string | null | undefined): number | null {
	const value = cleanTimeZoneId(timeZone);
	if (!value) return null;
	if (supportedTimeZoneIds.has(value)) {
		const offset = getRuntimeOffsetMinutes(value);
		if (offset !== null) return offset;
	}
	const record = findTimeZone(value);
	if (!record) {
		return null;
	}
	return getRuntimeOffsetMinutes(record.name) ?? record.currentTimeOffsetInMinutes;
}

function formatUtcOffset(offsetMinutes: number): string {
	const sign = offsetMinutes >= 0 ? '+' : '-';
	const absolute = Math.abs(offsetMinutes);
	const hours = Math.floor(absolute / 60)
		.toString()
		.padStart(2, '0');
	const minutes = (absolute % 60).toString().padStart(2, '0');
	return `UTC${sign}${hours}:${minutes}`;
}

function formatTimeZonePlace(record: TimeZone): string {
	if (record.name === UTC_TIME_ZONE_ID) {
		return UTC_TIME_ZONE_ID;
	}
	const name = record.alternativeName || record.name.replace(/_/g, ' ');
	const cities = record.mainCities.slice(0, 2).join(', ');
	return cities ? `${name} - ${cities}` : name;
}

export function getTimeZoneDisplayOptions(): Array<TimeZoneDisplayOption> {
	return getDisplayTimeZones()
		.map((record) => ({
			value: record.name,
			label: `${formatUtcOffset(record.currentTimeOffsetInMinutes)} ${formatTimeZonePlace(record)}`,
			searchText: [
				record.name,
				...record.group,
				record.alternativeName,
				record.countryName,
				record.countryCode,
				...record.mainCities,
			]
				.filter(Boolean)
				.join(' '),
			offsetMinutes: record.currentTimeOffsetInMinutes,
		}))
		.sort((left, right) => left.offsetMinutes - right.offsetMinutes || left.label.localeCompare(right.label));
}
