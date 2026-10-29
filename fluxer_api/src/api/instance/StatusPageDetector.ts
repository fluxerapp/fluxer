// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import {EXTERNAL_RESPONSE_LIMITS} from '@app/api/utils/ExternalResponseLimits';
import * as FetchUtils from '@app/api/utils/FetchUtils';
import {isJsonRecord, parseJsonOrNull} from '@app/api/utils/JsonBoundaryUtils';
import type {RequestUrlPolicy} from '@pkgs/http_client/src/HttpClientTypes';
import {ms} from 'itty-time';

const PROBE_TIMEOUT_MS = ms('5 seconds');
const FIRST_PROBE_WAIT_MS = ms('2 seconds');
const INSTATUS_RECHECK_MS = ms('6 hours');
const NOT_INSTATUS_RECHECK_MS = ms('24 hours');
const RETRY_DELAYS_MS = [ms('1 minute'), ms('5 minutes'), ms('15 minutes'), ms('30 minutes')];

type ProbeResult = 'instatus' | 'not_instatus' | 'transient';

interface DetectorEntry {
	readonly url: string;
	verdict: boolean | null;
	failures: number;
	nextProbeAt: number;
	probe: Promise<void> | null;
}

interface StatusPageDetectorOptions {
	requestUrlPolicy?: RequestUrlPolicy;
	now?: () => number;
}

function isInstatusSummary(value: unknown): boolean {
	return (
		isJsonRecord(value) &&
		(isJsonRecord(value['page']) ||
			Array.isArray(value['activeIncidents']) ||
			Array.isArray(value['activeMaintenances']))
	);
}

function isTransientStatus(status: number): boolean {
	return status === 408 || status === 429 || status >= 500;
}

function allowsOrigin(headers: Headers, origin: string): boolean {
	const allowed = headers.get('access-control-allow-origin')?.trim();
	return allowed === '*' || allowed === origin;
}

function webAppOrigin(): string | null {
	try {
		return new URL(Config.endpoints.webApp).origin;
	} catch {
		return null;
	}
}

async function waitAtMost(promise: Promise<void>, timeoutMs: number): Promise<void> {
	let timer: ReturnType<typeof setTimeout> | undefined;
	try {
		await Promise.race([
			promise,
			new Promise<void>((resolve) => {
				timer = setTimeout(resolve, timeoutMs);
			}),
		]);
	} finally {
		clearTimeout(timer);
	}
}

export class StatusPageDetector {
	private entry: DetectorEntry | null = null;
	private readonly requestUrlPolicy: RequestUrlPolicy | undefined;
	private readonly now: () => number;

	constructor(options: StatusPageDetectorOptions = {}) {
		this.requestUrlPolicy = options.requestUrlPolicy;
		this.now = options.now ?? Date.now;
	}

	async isInstatus(statusPageUrl: string | null): Promise<boolean> {
		if (!statusPageUrl) {
			this.entry = null;
			return false;
		}
		if (this.entry?.url !== statusPageUrl) {
			this.entry = {url: statusPageUrl, verdict: null, failures: 0, nextProbeAt: 0, probe: null};
		}
		const entry = this.entry;
		if (entry.probe === null && this.now() >= entry.nextProbeAt) {
			entry.probe = this.runProbe(entry);
		}
		if (entry.verdict === null && entry.probe !== null) {
			await waitAtMost(entry.probe, FIRST_PROBE_WAIT_MS);
		}
		return entry.verdict ?? false;
	}

	private async runProbe(entry: DetectorEntry): Promise<void> {
		const result = await this.probe(entry.url);
		entry.probe = null;
		const now = this.now();
		if (result === 'transient') {
			entry.failures += 1;
			entry.nextProbeAt = now + RETRY_DELAYS_MS[Math.min(entry.failures, RETRY_DELAYS_MS.length) - 1]!;
			return;
		}
		const verdict = result === 'instatus';
		if (entry.verdict !== verdict) {
			Logger.info(
				{statusPageUrl: entry.url, instatus: verdict},
				verdict
					? 'Status page is an Instatus page, the apps will show its incidents'
					: 'Status page is not an Instatus page the apps can read, they will only link to it',
			);
		}
		entry.verdict = verdict;
		entry.failures = 0;
		entry.nextProbeAt = now + (verdict ? INSTATUS_RECHECK_MS : NOT_INSTATUS_RECHECK_MS);
	}

	private async probe(statusPageUrl: string): Promise<ProbeResult> {
		const origin = webAppOrigin();
		if (origin === null) {
			return 'not_instatus';
		}
		const summaryUrl = `${statusPageUrl}/summary.json`;
		try {
			const response = await FetchUtils.sendRequest(
				{
					url: summaryUrl,
					method: 'GET',
					headers: {Accept: 'application/json', Origin: origin},
					timeout: PROBE_TIMEOUT_MS,
					serviceName: 'status_page_probe',
				},
				{requestUrlPolicy: this.requestUrlPolicy},
			);
			if (response.status < 200 || response.status >= 300) {
				FetchUtils.discardResponseBody(response.stream, response.status);
				return isTransientStatus(response.status) ? 'transient' : 'not_instatus';
			}
			if (!allowsOrigin(response.headers, origin)) {
				FetchUtils.discardResponseBody(response.stream, response.status);
				return 'not_instatus';
			}
			const body = await FetchUtils.streamToStringWithLimit(response.stream, {
				maxBytes: EXTERNAL_RESPONSE_LIMITS.statusPageSummaryBytes,
				headers: response.headers,
				url: summaryUrl,
				description: 'Status page summary',
				signal: AbortSignal.timeout(PROBE_TIMEOUT_MS),
			});
			return isInstatusSummary(parseJsonOrNull(body)) ? 'instatus' : 'not_instatus';
		} catch (error) {
			if (error instanceof FetchUtils.ResponseBodyTooLargeError) {
				return 'not_instatus';
			}
			Logger.warn({error, statusPageUrl}, 'Status page check failed, it will be retried later');
			return 'transient';
		}
	}
}

const detector = new StatusPageDetector();

export function getStatusPageDetector(): StatusPageDetector {
	return detector;
}
