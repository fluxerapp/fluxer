// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {Config} from '@app/api/Config';
import {parseIpBanEntry, tryParseSingleIp} from '@app/api/utils/IpRangeUtils';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

const PREVIEW_IPS_KEY = 'operator_pass:preview:ips';
const ENV_LABEL = 'FLUXER_OPERATOR_PASS_PREVIEW_IPS';

export interface OperatorPassPreviewEntry {
	id: string;
	value: string;
	label: string;
	expires_at: string | null;
	created_at: string;
	created_by: string;
}

export interface OperatorPassPreviewMatch {
	label: string;
	entryId: string | null;
}

function canonicalizeOperatorPassPreviewValue(value: string): string | null {
	return parseIpBanEntry(value)?.canonical ?? null;
}

function isEntryExpired(entry: Pick<OperatorPassPreviewEntry, 'expires_at'>, now: Date): boolean {
	return entry.expires_at !== null && new Date(entry.expires_at).getTime() <= now.getTime();
}

function matchesOperatorPassPreviewValue(value: string, ip: string): boolean {
	const target = tryParseSingleIp(ip);
	const entry = parseIpBanEntry(value);
	if (!target || !entry || entry.family !== target.family) {
		return false;
	}
	if (entry.type === 'single') {
		return entry.value === target.value;
	}
	return target.value >= entry.start && target.value <= entry.end;
}

function parseStoredEntry(raw: string): OperatorPassPreviewEntry | null {
	try {
		const parsed = JSON.parse(raw) as Partial<OperatorPassPreviewEntry>;
		if (typeof parsed.id !== 'string' || typeof parsed.value !== 'string' || typeof parsed.label !== 'string') {
			return null;
		}
		return {
			id: parsed.id,
			value: parsed.value,
			label: parsed.label,
			expires_at: typeof parsed.expires_at === 'string' ? parsed.expires_at : null,
			created_at: typeof parsed.created_at === 'string' ? parsed.created_at : new Date(0).toISOString(),
			created_by: typeof parsed.created_by === 'string' ? parsed.created_by : '',
		};
	} catch {
		return null;
	}
}

export async function listOperatorPassPreviewEntries(kvClient: IKVProvider): Promise<Array<OperatorPassPreviewEntry>> {
	const stored = await kvClient.hgetall(PREVIEW_IPS_KEY);
	return Object.values(stored)
		.map(parseStoredEntry)
		.filter((entry): entry is OperatorPassPreviewEntry => entry !== null)
		.sort((left, right) => left.created_at.localeCompare(right.created_at));
}

export async function addOperatorPassPreviewEntry(
	kvClient: IKVProvider,
	params: {value: string; label: string; expiresAt: Date | null; createdBy: string; now: Date},
): Promise<OperatorPassPreviewEntry | null> {
	const value = canonicalizeOperatorPassPreviewValue(params.value);
	if (!value) {
		return null;
	}
	const entry: OperatorPassPreviewEntry = {
		id: randomUUID(),
		value,
		label: params.label,
		expires_at: params.expiresAt?.toISOString() ?? null,
		created_at: params.now.toISOString(),
		created_by: params.createdBy,
	};
	await kvClient.hset(PREVIEW_IPS_KEY, entry.id, JSON.stringify(entry));
	return entry;
}

export async function removeOperatorPassPreviewEntry(kvClient: IKVProvider, entryId: string): Promise<boolean> {
	return (await kvClient.hdel(PREVIEW_IPS_KEY, entryId)) > 0;
}

export function findOperatorPassPreviewMatch(
	entries: ReadonlyArray<OperatorPassPreviewEntry>,
	envValues: ReadonlyArray<string>,
	ip: string,
	now: Date,
): OperatorPassPreviewMatch | null {
	const entry = entries.find(
		(candidate) => !isEntryExpired(candidate, now) && matchesOperatorPassPreviewValue(candidate.value, ip),
	);
	if (entry) {
		return {label: entry.label, entryId: entry.id};
	}
	return envValues.some((value) => matchesOperatorPassPreviewValue(value, ip))
		? {label: ENV_LABEL, entryId: null}
		: null;
}

export async function resolveOperatorPassPreview(
	kvClient: IKVProvider,
	ip: string | null,
	now: Date,
): Promise<OperatorPassPreviewMatch | null> {
	if (Config.instance.selfHosted || !ip) {
		return null;
	}
	return findOperatorPassPreviewMatch(
		await listOperatorPassPreviewEntries(kvClient),
		Config.operatorPass.previewIps,
		ip,
		now,
	);
}
