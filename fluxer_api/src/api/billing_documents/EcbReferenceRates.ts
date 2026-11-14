// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {ms, seconds} from 'itty-time';

const ECB_API_URL = 'https://data-api.ecb.europa.eu/service/data/EXR';
const CACHE_PREFIX = 'billing_documents:ecb_rate';
const SETTLED_TTL_SECONDS = seconds('400 days');
const RECENT_TTL_SECONDS = seconds('1 hour');
const RECENT_DAYS = 3;
const LOOKBACK_DAYS = 14;
const PUBLICATION_HOUR_STOCKHOLM = 16;
const FETCH_TIMEOUT_MS = ms('10 seconds');
const DAY_MS = ms('1 day');

export interface SekRate {
	rate: string;
	rateDate: string;
}

export type EcbCsvFetcher = (currencies: ReadonlyArray<string>, startDate: string, endDate: string) => Promise<string>;

const stockholmParts = new Intl.DateTimeFormat('en-CA', {
	timeZone: 'Europe/Stockholm',
	year: 'numeric',
	month: '2-digit',
	day: '2-digit',
	hour: '2-digit',
	hourCycle: 'h23',
});

function shiftDate(date: string, days: number): string {
	return new Date(Date.parse(`${date}T00:00:00Z`) + days * DAY_MS).toISOString().slice(0, 10);
}

export function latestPublishedEcbDate(at: Date): string {
	const parts = Object.fromEntries(stockholmParts.formatToParts(at).map((part) => [part.type, part.value]));
	const date = `${parts.year}-${parts.month}-${parts.day}`;
	return Number(parts.hour) >= PUBLICATION_HOUR_STOCKHOLM ? date : shiftDate(date, -1);
}

function parseEcbCsv(csv: string): Map<string, Map<string, number>> {
	const [header, ...rows] = csv.trim().split(/\r?\n/);
	const columns = (header ?? '').split(',');
	const currencyColumn = columns.indexOf('CURRENCY');
	const dateColumn = columns.indexOf('TIME_PERIOD');
	const valueColumn = columns.indexOf('OBS_VALUE');
	const byDate = new Map<string, Map<string, number>>();
	if (currencyColumn < 0 || dateColumn < 0 || valueColumn < 0) {
		return byDate;
	}
	for (const row of rows) {
		const cells = row.split(',');
		const value = Number(cells[valueColumn]);
		const date = cells[dateColumn];
		const currency = cells[currencyColumn];
		if (!date || !currency || !Number.isFinite(value) || value <= 0) {
			continue;
		}
		const rates = byDate.get(date) ?? new Map<string, number>([['EUR', 1]]);
		rates.set(currency, value);
		byDate.set(date, rates);
	}
	return byDate;
}

export class EcbReferenceRates {
	constructor(
		private readonly kv: IKVProvider,
		private readonly fetchCsv: EcbCsvFetcher = fetchEcbCsv,
		private readonly now: () => Date = () => new Date(),
	) {}

	async sekRate(currency: string, at: Date): Promise<SekRate> {
		const code = currency.toUpperCase();
		const asOf = latestPublishedEcbDate(at);
		const key = `${CACHE_PREFIX}:${code}:${asOf}`;
		const cached = await this.kv.get(key);
		if (cached) {
			return JSON.parse(cached) as SekRate;
		}
		const wanted = code === 'EUR' ? ['SEK'] : [code, 'SEK'];
		const byDate = parseEcbCsv(await this.fetchCsv(wanted, shiftDate(asOf, -LOOKBACK_DAYS), asOf));
		const date = [...byDate.keys()]
			.filter((day) => day <= asOf && wanted.every((wantedCode) => byDate.get(day)?.has(wantedCode)))
			.sort()
			.at(-1);
		const rates = date ? byDate.get(date) : null;
		if (!date || !rates) {
			throw new Error(`The ECB reference rates have no rate for ${code} and SEK published by ${asOf}`);
		}
		const rate: SekRate = {rate: (rates.get('SEK')! / rates.get(code)!).toFixed(6), rateDate: date};
		const settled = Date.parse(`${asOf}T00:00:00Z`) < this.now().getTime() - RECENT_DAYS * DAY_MS;
		await this.kv.setex(key, settled ? SETTLED_TTL_SECONDS : RECENT_TTL_SECONDS, JSON.stringify(rate));
		return rate;
	}
}

async function fetchEcbCsv(currencies: ReadonlyArray<string>, startDate: string, endDate: string): Promise<string> {
	const url = `${ECB_API_URL}/D.${currencies.join('+')}.EUR.SP00.A?startPeriod=${startDate}&endPeriod=${endDate}&format=csvdata`;
	const response = await fetch(url, {signal: AbortSignal.timeout(FETCH_TIMEOUT_MS)});
	if (!response.ok) {
		throw new Error(`The ECB reference rate request failed with ${response.status}`);
	}
	return response.text();
}
