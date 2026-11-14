// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {codeMatches} from '@app/api/auth/AccountCodes';

const MAX_CODE_ATTEMPTS = 5;

interface StoredCode {
	code_hash: string;
}

export type EmailCodeCheck<T> =
	| {kind: 'ok'; record: T}
	| {kind: 'missing'}
	| {kind: 'exhausted'}
	| {kind: 'wrong'; record: T};

type CodeStoreContext = Pick<ApiContext, 'services'>;

function attemptsKey(key: string): string {
	return `${key}:attempts`;
}

export async function storeEmailCode<T extends StoredCode>(
	ctx: CodeStoreContext,
	params: {key: string; record: T; ttlSeconds: number},
): Promise<void> {
	await ctx.services.kv.del(attemptsKey(params.key));
	await ctx.services.cache.set<T>(params.key, params.record, params.ttlSeconds);
}

export async function checkEmailCode<T extends StoredCode>(
	ctx: CodeStoreContext,
	params: {key: string; code: string | undefined; scope: string; ttlSeconds: number; accepts?: (record: T) => boolean},
): Promise<EmailCodeCheck<T>> {
	const {kv, cache} = ctx.services;
	const counter = attemptsKey(params.key);
	const attempt = await kv.incr(counter);
	if (attempt === 1) await kv.expire(counter, params.ttlSeconds);
	const record = await cache.get<T>(params.key);
	if (!record || (params.accepts && !params.accepts(record))) return {kind: 'missing'};
	if (attempt > MAX_CODE_ATTEMPTS) {
		await cache.delete(params.key);
		return {kind: 'exhausted'};
	}
	if (!params.code || !codeMatches(record.code_hash, params.code, params.scope)) return {kind: 'wrong', record};
	const consumed = await cache.getAndDelete<T>(params.key);
	await kv.del(counter);
	return consumed ? {kind: 'ok', record: consumed} : {kind: 'missing'};
}
