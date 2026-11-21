// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CaptureStatement} from '@app/api/database/capture/CaptureRecords';

const UNREPLAYABLE_CQL =
	/\b(?:now|uuid|currentTimestamp|currentTimeUUID|currentDate|currentTime)\s*\(|\bUSING\s+TIMESTAMP\b|\b(\w+)\s*=\s*\1\s*[+-]/i;
const CONDITIONAL = /\bIF\b/i;
const EXPLICIT_TTL = /\bUSING\s+TTL\s+:([A-Za-z_][A-Za-z0-9_]*)/i;
const INSERT_PATTERN = /^(\s*INSERT\s+INTO\s+\w+\s*\([^()]*\)\s*VALUES\s*\([^()]*\))\s*;?\s*$/i;
const UPDATE_PATTERN = /^(\s*UPDATE\s+\w+)(\s+SET\b[\s\S]*)$/i;
const DEFAULT_TTL_PARAM = 'replay_ttl';

export class ReplayRefusal extends Error {
	override readonly name = 'ReplayRefusal';
}

export interface ReplayStatement {
	cql: string;
	params: Record<string, unknown>;
}

export function isReplayableCql(cql: string): boolean {
	return !UNREPLAYABLE_CQL.test(cql);
}

export function remainingTtl(ttl: number, ts: number, nowMs: number): number {
	return Math.max(1, ttl - Math.ceil(nowMs / 1000 - ts / 1e6));
}

export function replayStatement(
	statement: CaptureStatement,
	params: Record<string, unknown>,
	ts: number,
	nowMs: number,
	tableDefaultTtl: number,
): ReplayStatement {
	const cql = statement.q;
	if (!isReplayableCql(cql) || CONDITIONAL.test(cql)) {
		throw new ReplayRefusal(`refusing to replay ${JSON.stringify(cql)}`);
	}
	const explicit = EXPLICIT_TTL.exec(cql);
	if (explicit) {
		const name = explicit[1] ?? '';
		const ttl = params[name];
		if (typeof ttl !== 'number') {
			throw new ReplayRefusal(`statement on ${statement.t} has no TTL value for :${name}`);
		}
		return ttl === 0 ? {cql, params} : {cql, params: {...params, [name]: remainingTtl(ttl, ts, nowMs)}};
	}
	if (/^\s*DELETE\b/i.test(cql)) return {cql, params};
	const ttl = statement.ttl ?? 0;
	if (ttl !== tableDefaultTtl) {
		throw new ReplayRefusal(
			`${statement.t} has a default TTL of ${tableDefaultTtl} on this cluster but ${ttl} where it was captured`,
		);
	}
	if (ttl === 0) return {cql, params};
	const withTtl = {...params, [DEFAULT_TTL_PARAM]: remainingTtl(ttl, ts, nowMs)};
	const insert = INSERT_PATTERN.exec(cql);
	if (insert) return {cql: `${insert[1]} USING TTL :${DEFAULT_TTL_PARAM};`, params: withTtl};
	const update = UPDATE_PATTERN.exec(cql);
	if (update) return {cql: `${update[1]} USING TTL :${DEFAULT_TTL_PARAM}${update[2]}`, params: withTtl};
	throw new ReplayRefusal(`cannot add a TTL to ${JSON.stringify(cql)}`);
}
