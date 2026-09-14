// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RegistrationTimingResult} from '@app/api/risk/RiskTypes';

const TZ_OFFSETS: Record<string, number> = {
	'Pacific/Midway': -11,
	'Pacific/Honolulu': -10,
	'America/Anchorage': -9,
	'America/Los_Angeles': -8,
	'America/Denver': -7,
	'America/Chicago': -6,
	'America/New_York': -5,
	'America/Halifax': -4,
	'America/Sao_Paulo': -3,
	'Atlantic/South_Georgia': -2,
	'Atlantic/Azores': -1,
	'Europe/London': 0,
	UTC: 0,
	'Europe/Paris': 1,
	'Europe/Berlin': 1,
	'Europe/Rome': 1,
	'Europe/Madrid': 1,
	'Europe/Amsterdam': 1,
	'Europe/Brussels': 1,
	'Europe/Vienna': 1,
	'Europe/Warsaw': 1,
	'Europe/Zurich': 1,
	'Europe/Stockholm': 1,
	'Europe/Oslo': 1,
	'Europe/Copenhagen': 1,
	'Europe/Prague': 1,
	'Africa/Lagos': 1,
	'Africa/Casablanca': 1,
	'Europe/Athens': 2,
	'Europe/Bucharest': 2,
	'Europe/Helsinki': 2,
	'Europe/Istanbul': 3,
	'Europe/Moscow': 3,
	'Asia/Riyadh': 3,
	'Asia/Tehran': 3.5,
	'Asia/Dubai': 4,
	'Asia/Kabul': 4.5,
	'Asia/Karachi': 5,
	'Asia/Kolkata': 5.5,
	'Asia/Kathmandu': 5.75,
	'Asia/Dhaka': 6,
	'Asia/Rangoon': 6.5,
	'Asia/Bangkok': 7,
	'Asia/Ho_Chi_Minh': 7,
	'Asia/Jakarta': 7,
	'Asia/Shanghai': 8,
	'Asia/Hong_Kong': 8,
	'Asia/Singapore': 8,
	'Asia/Taipei': 8,
	'Asia/Kuala_Lumpur': 8,
	'Australia/Perth': 8,
	'Asia/Tokyo': 9,
	'Asia/Seoul': 9,
	'Australia/Adelaide': 9.5,
	'Australia/Sydney': 10,
	'Australia/Melbourne': 10,
	'Australia/Brisbane': 10,
	'Pacific/Auckland': 12,
	'Europe/Bratislava': 1,
	'Europe/Simferopol': 3,
	'Europe/Kyiv': 2,
	'Europe/Vilnius': 2,
	'Europe/Riga': 2,
	'Europe/Tallinn': 2,
	'Asia/Almaty': 6,
	'Asia/Tashkent': 5,
	'Asia/Tbilisi': 4,
	'Asia/Yerevan': 4,
	'Asia/Baku': 4,
	'America/Toronto': -5,
	'America/Vancouver': -8,
	'America/Winnipeg': -6,
	'America/Edmonton': -7,
	'America/Mexico_City': -6,
	'America/Bogota': -5,
	'America/Lima': -5,
	'America/Santiago': -4,
	'America/Buenos_Aires': -3,
	'America/Argentina/Buenos_Aires': -3,
	'Africa/Algiers': 1,
	'Africa/Cairo': 2,
	'Africa/Johannesburg': 2,
	'Africa/Nairobi': 3,
};

export function analyzeRegistrationTiming(args: {timezone: string | null}): RegistrationTimingResult {
	const tz = args.timezone;
	if (!tz) {
		return {
			timezone: null,
			localHour: null,
			isSuspiciousHour: false,
			riskNote: 'no timezone available — timing analysis skipped',
		};
	}
	const offset = TZ_OFFSETS[tz];
	if (offset === undefined) {
		return {
			timezone: tz,
			localHour: null,
			isSuspiciousHour: false,
			riskNote: `timezone "${tz}" not in offset table — timing analysis skipped`,
		};
	}
	const nowUtc = new Date();
	const localHour = (nowUtc.getUTCHours() + offset + 24) % 24;
	const roundedHour = Math.floor(localHour);
	const isSuspicious = roundedHour >= 2 && roundedHour < 5;
	let riskNote: string;
	if (isSuspicious) {
		riskNote = `registration at ~${roundedHour}:00 local time (${tz}) — unusual hour for organic signups`;
	} else {
		riskNote = `registration at ~${roundedHour}:00 local time (${tz}) — normal hours`;
	}
	return {
		timezone: tz,
		localHour: roundedHour,
		isSuspiciousHour: isSuspicious,
		riskNote,
	};
}
