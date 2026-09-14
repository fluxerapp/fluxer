// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IRiskHistoryRepository} from '@app/api/risk/HistoricalOutcomeRepository';
import type {HistoricalOutcomeRecord} from '@app/api/risk/RiskHistoryTypes';
import type {HistoricalOutcomeResult} from '@app/api/risk/RiskTypes';
import {getSubnet} from '@fluxer/ip_utils/src/IpAddress';

const DEFAULT_LIST_LIMIT = 200;

export interface HistoricalOutcomeAdapterContext {
	repository: IRiskHistoryRepository;
	now?: () => Date;
}

export function createHistoricalOutcomeAdapter(ctx: HistoricalOutcomeAdapterContext) {
	async function getHistoricalOutcomesByIp(args: {ip: string; windowHours: number}): Promise<HistoricalOutcomeResult> {
		const since = getSinceTime(args.windowHours, ctx.now);
		const records = await ctx.repository.listByIp(args.ip, since, DEFAULT_LIST_LIMIT);
		return summarize(args.ip, args.windowHours, records, DEFAULT_LIST_LIMIT);
	}
	async function getHistoricalOutcomesBySubnet(args: {
		ip: string;
		windowHours: number;
	}): Promise<HistoricalOutcomeResult> {
		const subnet = getSubnet(args.ip);
		if (!subnet) {
			return emptyResult(args.ip, args.windowHours, 'could not derive subnet from IP');
		}
		const since = getSinceTime(args.windowHours, ctx.now);
		const records = await ctx.repository.listBySubnet(subnet, since, DEFAULT_LIST_LIMIT);
		return summarize(subnet, args.windowHours, records, DEFAULT_LIST_LIMIT);
	}
	async function getHistoricalOutcomesByEmailDomain(args: {
		domain: string;
		windowHours: number;
	}): Promise<HistoricalOutcomeResult> {
		const domain = args.domain.toLowerCase().trim();
		const since = getSinceTime(args.windowHours, ctx.now);
		const records = await ctx.repository.listByEmailDomain(domain, since, DEFAULT_LIST_LIMIT);
		return summarize(domain, args.windowHours, records, DEFAULT_LIST_LIMIT);
	}
	async function getHistoricalOutcomesByAsn(args: {
		asn: number;
		windowHours: number;
	}): Promise<HistoricalOutcomeResult> {
		if (!Number.isFinite(args.asn) || args.asn <= 0) {
			return emptyResult(String(args.asn), args.windowHours, 'invalid ASN');
		}
		const since = getSinceTime(args.windowHours, ctx.now);
		const records = await ctx.repository.listByAsn(args.asn, since, DEFAULT_LIST_LIMIT);
		return summarize(`AS${args.asn}`, args.windowHours, records, DEFAULT_LIST_LIMIT);
	}
	return {
		getHistoricalOutcomesByIp,
		getHistoricalOutcomesBySubnet,
		getHistoricalOutcomesByEmailDomain,
		getHistoricalOutcomesByAsn,
	};
}

function getSinceTime(windowHours: number, nowProvider?: () => Date): Date {
	const now = nowProvider?.() ?? new Date();
	return new Date(now.getTime() - windowHours * 3600 * 1000);
}

function summarize(
	identifier: string,
	windowHours: number,
	records: ReadonlyArray<HistoricalOutcomeRecord>,
	limit: number,
): HistoricalOutcomeResult {
	if (records.length === 0) {
		return emptyResult(identifier, windowHours, `no recorded outcomes in the last ${windowHours}h`);
	}
	const truncated = records.length >= limit;
	const byUser = new Map<
		string,
		{
			challenged: boolean;
			spammer: boolean;
			disabled: boolean;
			disabledSuspicious: boolean;
		}
	>();
	for (const record of records) {
		const current = byUser.get(record.userId) ?? {
			challenged: false,
			spammer: false,
			disabled: false,
			disabledSuspicious: false,
		};
		switch (record.outcomeCode) {
			case 'challenged':
				current.challenged = true;
				break;
			case 'spammer':
				current.spammer = true;
				break;
			case 'disabled':
				current.disabled = true;
				break;
			case 'disabled_suspicious':
				current.disabledSuspicious = true;
				break;
		}
		byUser.set(record.userId, current);
	}
	let challengedUsers = 0;
	let enforcedUsers = 0;
	let spammerUsers = 0;
	let disabledUsers = 0;
	let disabledSuspiciousUsers = 0;
	let adverseUsers = 0;
	for (const outcome of byUser.values()) {
		const enforced = outcome.spammer || outcome.disabled || outcome.disabledSuspicious;
		const adverse = outcome.challenged || enforced;
		if (outcome.challenged) challengedUsers++;
		if (outcome.spammer) spammerUsers++;
		if (outcome.disabled) disabledUsers++;
		if (outcome.disabledSuspicious) disabledSuspiciousUsers++;
		if (enforced) enforcedUsers++;
		if (adverse) adverseUsers++;
	}
	const observedUsers = byUser.size;
	return {
		identifier,
		windowHours,
		sampledRegistrations: records.length,
		truncated,
		sampledUsers: observedUsers,
		resolvedUsers: observedUsers,
		adverseUsers,
		challengedUsers,
		enforcedUsers,
		spammerUsers,
		disabledUsers,
		disabledSuspiciousUsers,
		riskNote: describeHistoricalOutcomes({
			windowHours,
			outcomeEvents: records.length,
			observedUsers,
			challengedUsers,
			enforcedUsers,
			truncated,
		}),
	};
}

function emptyResult(identifier: string, windowHours: number, note: string): HistoricalOutcomeResult {
	return {
		identifier,
		windowHours,
		sampledRegistrations: 0,
		truncated: false,
		sampledUsers: 0,
		resolvedUsers: 0,
		adverseUsers: 0,
		challengedUsers: 0,
		enforcedUsers: 0,
		spammerUsers: 0,
		disabledUsers: 0,
		disabledSuspiciousUsers: 0,
		riskNote: `historical_outcomes: ${note}`,
	};
}

function describeHistoricalOutcomes(args: {
	windowHours: number;
	outcomeEvents: number;
	observedUsers: number;
	challengedUsers: number;
	enforcedUsers: number;
	truncated: boolean;
}): string {
	const sampleNote = args.truncated ? ' (sample truncated — true counts may be higher)' : '';
	if (args.enforcedUsers > 0) {
		return `${args.enforcedUsers} prior users hit enforcement in ${args.windowHours}h (${args.outcomeEvents} outcome events across ${args.observedUsers} users)${sampleNote}`;
	}
	if (args.challengedUsers > 0) {
		return `${args.challengedUsers} prior users later required verification in ${args.windowHours}h (${args.outcomeEvents} outcome events across ${args.observedUsers} users)${sampleNote}`;
	}
	return `historical outcomes recorded ${args.outcomeEvents} times across ${args.observedUsers} users in ${args.windowHours}h${sampleNote}`;
}
