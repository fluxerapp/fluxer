// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VelocityResult} from '@app/api/risk/RiskTypes';
import {getSubnet} from '@fluxer/ip_utils/src/IpAddress';

export interface RegistrationEventRecord {
	userId: string;
	email: string | null;
	emailDomain: string | null;
	ip: string;
	locale: string | null;
	createdAt: Date;
}

export interface IRegistrationEventsRepository {
	recordEvent(event: RegistrationEventRecord): Promise<void>;
	listByIp(ip: string, sinceTime: Date, limit: number): Promise<ReadonlyArray<RegistrationEventRecord>>;
	listBySubnet(subnet: string, sinceTime: Date, limit: number): Promise<ReadonlyArray<RegistrationEventRecord>>;
	listByPlusAddressBase(
		plusAddressBase: string,
		sinceTime: Date,
		limit: number,
	): Promise<ReadonlyArray<RegistrationEventRecord>>;
	listByEmailDomain(
		emailDomain: string,
		sinceTime: Date,
		limit: number,
	): Promise<ReadonlyArray<RegistrationEventRecord>>;
}

const DEFAULT_LIST_LIMIT = 100;

interface VelocityAdapterContext {
	repository: IRegistrationEventsRepository;
	now?: () => Date;
}

export function createVelocityAdapter(ctx: VelocityAdapterContext) {
	async function getRegistrationsByIp(args: {ip: string; windowHours: number}): Promise<VelocityResult> {
		const since = getSinceTime(args.windowHours, ctx.now);
		const records = await ctx.repository.listByIp(args.ip, since, DEFAULT_LIST_LIMIT);
		return summarize(args.ip, args.windowHours, records, 'ip', DEFAULT_LIST_LIMIT);
	}
	async function getRegistrationsBySubnet(args: {ip: string; windowHours: number}): Promise<VelocityResult> {
		const subnet = getSubnet(args.ip);
		if (!subnet) {
			return emptyResult(args.ip, args.windowHours, 'subnet', 'could not derive subnet from IP');
		}
		const since = getSinceTime(args.windowHours, ctx.now);
		const records = await ctx.repository.listBySubnet(subnet, since, DEFAULT_LIST_LIMIT);
		return summarize(subnet, args.windowHours, records, 'subnet', DEFAULT_LIST_LIMIT);
	}
	async function getRegistrationsByEmailDomain(args: {domain: string; windowHours: number}): Promise<VelocityResult> {
		const domain = args.domain.toLowerCase().trim();
		const since = getSinceTime(args.windowHours, ctx.now);
		const records = await ctx.repository.listByEmailDomain(domain, since, DEFAULT_LIST_LIMIT);
		return summarize(domain, args.windowHours, records, 'email_domain', DEFAULT_LIST_LIMIT);
	}
	async function getRegistrationsByPlusAddressBase(args: {
		plusAddressBase: string;
		windowHours: number;
	}): Promise<VelocityResult> {
		const plusAddressBase = args.plusAddressBase.toLowerCase().trim();
		const since = getSinceTime(args.windowHours, ctx.now);
		const records = await ctx.repository.listByPlusAddressBase(plusAddressBase, since, DEFAULT_LIST_LIMIT);
		return summarize(plusAddressBase, args.windowHours, records, 'plus_address_base', DEFAULT_LIST_LIMIT);
	}
	return {
		getRegistrationsByIp,
		getRegistrationsBySubnet,
		getRegistrationsByEmailDomain,
		getRegistrationsByPlusAddressBase,
	};
}

function getSinceTime(windowHours: number, nowProvider?: () => Date): Date {
	const now = nowProvider?.() ?? new Date();
	return new Date(now.getTime() - windowHours * 3600 * 1000);
}

function summarize(
	identifier: string,
	windowHours: number,
	records: ReadonlyArray<RegistrationEventRecord>,
	kind: 'ip' | 'subnet' | 'email_domain' | 'plus_address_base',
	limit: number,
): VelocityResult {
	const truncated = records.length >= limit;
	const uniqueEmails = new Set<string>();
	const uniqueLocales = new Set<string>();
	const uniqueIps = new Set<string>();
	for (const r of records) {
		if (r.email) uniqueEmails.add(r.email.toLowerCase());
		if (r.locale) uniqueLocales.add(r.locale);
		uniqueIps.add(r.ip);
	}
	return {
		identifier,
		windowHours,
		totalRegistrations: records.length,
		truncated,
		uniqueEmails: uniqueEmails.size,
		uniqueLocales: [...uniqueLocales],
		uniqueIps: uniqueIps.size,
		riskNote: describeVelocity(records.length, windowHours, kind, uniqueIps.size, truncated),
	};
}

function emptyResult(identifier: string, windowHours: number, kind: string, note: string): VelocityResult {
	return {
		identifier,
		windowHours,
		totalRegistrations: 0,
		truncated: false,
		uniqueEmails: 0,
		uniqueLocales: [],
		uniqueIps: 0,
		riskNote: `${kind}: ${note}`,
	};
}

function describeVelocity(count: number, hours: number, kind: string, uniqueIps: number, truncated: boolean): string {
	const atLeast = truncated ? 'at least ' : '';
	if (count === 0) return `${kind}: no prior registrations in the last ${hours}h — clean`;
	if (count <= 2) return `${kind}: ${count} registrations in the last ${hours}h — normal`;
	if (count <= 5) return `${kind}: ${count} registrations in the last ${hours}h — elevated`;
	if (kind === 'subnet') {
		return `${kind}: ${atLeast}${count} registrations across ${uniqueIps}+ IPs in ${hours}h — high, likely coordinated`;
	}
	return `${kind}: ${atLeast}${count} registrations in the last ${hours}h — high, likely signup farm`;
}
