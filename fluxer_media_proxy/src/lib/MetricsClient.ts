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

import process from 'node:process';

const METRICS_HOST = process.env.FLUXER_METRICS_HOST;
const MAX_RETRIES = 1;

interface CounterParams {
	name: string;
	dimensions?: Record<string, string>;
	value?: number;
}

interface HistogramParams {
	name: string;
	dimensions?: Record<string, string>;
	valueMs: number;
}

interface GaugeParams {
	name: string;
	dimensions?: Record<string, string>;
	value: number;
}

async function sendMetric(url: string, body: string, attempt: number): Promise<void> {
	try {
		const response = await fetch(url, {
			method: 'POST',
			headers: {'Content-Type': 'application/json'},
			body,
			signal: AbortSignal.timeout(5000),
		});
		if (!response.ok && attempt < MAX_RETRIES) {
			await sendMetric(url, body, attempt + 1);
		} else if (!response.ok) {
			console.error(
				`[MetricsClient] Failed to send metric after ${attempt + 1} attempts: ${response.status} ${response.statusText}`,
			);
		}
	} catch (error) {
		if (attempt < MAX_RETRIES) {
			await sendMetric(url, body, attempt + 1);
		} else {
			console.error(`[MetricsClient] Failed to send metric after ${attempt + 1} attempts:`, error);
		}
	}
}

function fireAndForget(path: string, body: unknown): void {
	if (!METRICS_HOST) return;

	const url = `http://${METRICS_HOST}${path}`;
	const jsonBody = JSON.stringify(body);
	sendMetric(url, jsonBody, 0).catch(() => {});
}

export function counter({name, dimensions = {}, value = 1}: CounterParams): void {
	fireAndForget('/metrics/counter', {name, dimensions, value});
}

export function histogram({name, dimensions = {}, valueMs}: HistogramParams): void {
	fireAndForget('/metrics/histogram', {name, dimensions, value_ms: valueMs});
}

export function gauge({name, dimensions = {}, value}: GaugeParams): void {
	fireAndForget('/metrics/gauge', {name, dimensions, value});
}

export function isEnabled(): boolean {
	return !!METRICS_HOST;
}
