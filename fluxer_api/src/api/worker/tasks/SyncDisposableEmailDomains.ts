// SPDX-License-Identifier: AGPL-3.0-or-later

import {domainToASCII} from 'node:url';
import {JobCancelledError, type WorkerTaskHandler, type WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {isAccountPolicyContactDomainReputationExempt} from '../../risk/AccountPolicyService';
import {EXTERNAL_RESPONSE_LIMITS} from '../../utils/ExternalResponseLimits';
import * as FetchUtils from '../../utils/FetchUtils';
import {getWorkerDependencies} from '../WorkerContext';

const SOURCES = [
	'https://raw.githubusercontent.com/doodad-labs/disposable-email-domains/main/data/domains.txt',
	'https://raw.githubusercontent.com/disposable-email-domains/disposable-email-domains/main/disposable_email_blocklist.conf',
	'https://raw.githubusercontent.com/disposable/disposable-email-domains/master/domains.txt',
	'https://raw.githubusercontent.com/micke/valid_email2/main/config/disposable_email_domains.txt',
	'https://raw.githubusercontent.com/amieiro/disposable-email-domains/master/denyDomains.txt',
	'https://raw.githubusercontent.com/ivolo/disposable-email-domains/master/index.json',
	'https://raw.githubusercontent.com/7c/fakefilter/main/txt/data.txt',
	'https://raw.githubusercontent.com/FGRibreau/mailchecker/master/list.txt',
	'https://raw.githubusercontent.com/wesbos/burner-email-providers/master/emails.txt',
	'https://raw.githubusercontent.com/unkn0w/disposable-email-domain-list/main/domains.txt',
	'https://raw.githubusercontent.com/vrittech/disposable-email/main/disposable_domains.txt',
	'https://raw.githubusercontent.com/martenson/disposable-email-domains/master/disposable_email_blocklist.conf',
];
const WRITE_PROGRESS_INTERVAL = 500;
const DOMAIN_REGEX = /^[a-z0-9]([a-z0-9-]*[a-z0-9])?(\.[a-z0-9]([a-z0-9-]*[a-z0-9])?)+$/;

interface SourceFetchResult {
	contentType: string;
	responseText: string;
}

interface SourceCollectResult {
	accepted: number;
	raw: number;
}

async function fetchSource(url: string): Promise<SourceFetchResult> {
	const res = await fetch(url, {signal: AbortSignal.timeout(60000)});
	if (!res.ok) throw new Error(`HTTP ${res.status} fetching ${url}`);
	const contentType = res.headers.get('content-type') ?? '';
	const responseText = await FetchUtils.streamToStringWithLimit(res.body, {
		maxBytes: EXTERNAL_RESPONSE_LIMITS.disposableEmailBytes,
		headers: res.headers,
		url: res.url,
		description: 'Disposable email domain feed',
	});
	return {contentType, responseText};
}

function collectDomain(raw: string, freshSet: Set<string>): boolean {
	const domain = normaliseDomain(raw);
	if (!domain) return false;
	if (isAccountPolicyContactDomainReputationExempt(domain)) return false;
	if (freshSet.has(domain)) return false;
	freshSet.add(domain);
	return true;
}

function forEachTextLine(text: string, callback: (line: string) => void): number {
	let raw = 0;
	let start = 0;
	for (let i = 0; i <= text.length; i++) {
		const isEnd = i === text.length;
		if (!isEnd && text.charCodeAt(i) !== 10) continue;
		const line = text.slice(start, isEnd ? i : i).trim();
		start = i + 1;
		if (!line || line.startsWith('#')) continue;
		raw++;
		callback(line);
	}
	return raw;
}

function collectTextDomains(text: string, freshSet: Set<string>): SourceCollectResult {
	let accepted = 0;
	const raw = forEachTextLine(text, (line) => {
		if (collectDomain(line, freshSet)) accepted++;
	});
	return {accepted, raw};
}

function collectJsonDomains(text: string, freshSet: Set<string>): SourceCollectResult {
	const json: unknown = JSON.parse(text);
	if (!Array.isArray(json)) {
		return {accepted: 0, raw: 0};
	}
	let raw = 0;
	let accepted = 0;
	for (const value of json) {
		if (typeof value !== 'string') continue;
		const trimmed = value.trim();
		if (!trimmed) continue;
		raw++;
		if (collectDomain(trimmed, freshSet)) accepted++;
	}
	return {accepted, raw};
}

async function collectSourceDomains(url: string, freshSet: Set<string>): Promise<SourceCollectResult> {
	const {contentType, responseText} = await fetchSource(url);
	if (contentType.includes('application/json') || url.endsWith('.json')) {
		return collectJsonDomains(responseText, freshSet);
	}
	return collectTextDomains(responseText, freshSet);
}

function normaliseDomain(raw: string): string | null {
	let domain = raw.trim().toLowerCase();
	if (!domain) return null;
	if (domain.startsWith('*.')) domain = domain.slice(2);
	if (domain.startsWith('.')) domain = domain.slice(1);
	if (!domain || domain.length > 253) return null;
	if (/[\s@/\\]/.test(domain)) return null;
	const ascii = domainToASCII(domain);
	if (!ascii) return null;
	if (!DOMAIN_REGEX.test(ascii)) return null;
	return ascii;
}

async function loadCurrentDisposableEmailDomains(): Promise<Set<string>> {
	const {adminRepository} = getWorkerDependencies();
	return new Set(await adminRepository.listDisposableEmailDomains());
}

async function throwIfCancelled(helpers: WorkerTaskHelpers): Promise<void> {
	if (await helpers.shouldCancel()) {
		throw new JobCancelledError();
	}
}

const syncDisposableEmailDomains: WorkerTaskHandler = async (_payload, helpers) => {
	helpers.logger.info('Starting disposable email domain sync');
	await helpers.setContextLink('/suspicious-email-domains');
	const {adminRepository} = getWorkerDependencies();
	const freshSet = new Set<string>();
	const perSourceCounts: Record<string, number> = {};
	const perSourceRawCounts: Record<string, number> = {};
	let successCount = 0;
	for (const url of SOURCES) {
		try {
			const result = await collectSourceDomains(url, freshSet);
			successCount++;
			perSourceCounts[url] = result.accepted;
			perSourceRawCounts[url] = result.raw;
		} catch (error) {
			helpers.logger.warn({url, error}, 'Failed to fetch disposable email source');
			perSourceCounts[url] = 0;
			perSourceRawCounts[url] = 0;
		}
		await throwIfCancelled(helpers);
	}
	if (successCount === 0) {
		throw new Error('All disposable email sources failed; skipping sync (no delta applied)');
	}
	helpers.logger.info(
		{
			count: freshSet.size,
			successfulSources: successCount,
			perSource: perSourceCounts,
			perSourceRaw: perSourceRawCounts,
		},
		'Fetched disposable email domains from all sources',
	);
	const currentSet = await loadCurrentDisposableEmailDomains();
	let addCount = 0;
	for (const domain of freshSet) {
		if (!currentSet.has(domain)) addCount++;
	}
	let removeCount = 0;
	for (const domain of currentSet) {
		if (!freshSet.has(domain)) removeCount++;
	}
	const totalOps = addCount + removeCount;
	let added = 0;
	let removed = 0;
	await helpers.reportProgress(0, totalOps, `Applying ${addCount} adds and ${removeCount} removes`);
	for (const domain of freshSet) {
		if (currentSet.has(domain)) continue;
		await throwIfCancelled(helpers);
		await adminRepository.addDisposableEmailDomain(domain);
		added++;
		if (added % WRITE_PROGRESS_INTERVAL === 0) {
			await helpers.reportProgress(added + removed, totalOps, null);
		}
	}
	for (const domain of currentSet) {
		if (freshSet.has(domain)) continue;
		await throwIfCancelled(helpers);
		await adminRepository.removeDisposableEmailDomain(domain);
		removed++;
		if (removed % WRITE_PROGRESS_INTERVAL === 0) {
			await helpers.reportProgress(added + removed, totalOps, null);
		}
	}
	await helpers.reportProgress(totalOps, totalOps, `+${added} added, -${removed} removed`);
	helpers.logger.info({added, removed, total: freshSet.size}, 'Disposable email domain sync complete');
};

export default syncDisposableEmailDomains;
