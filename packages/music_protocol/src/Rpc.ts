// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LoopMode, MusicErrorCode, SearchResult, Track} from '@fluxer/music_protocol/src/Types';

export const RPC_METHODS = [
	'music.search',
	'music.add',
	'music.queue',
	'music.pause',
	'music.resume',
	'music.skip',
	'music.seek',
	'music.remove',
	'music.move',
	'music.clear',
	'music.shuffle',
	'music.loop',
	'music.stop',
] as const;

export type RpcMethod = (typeof RPC_METHODS)[number];

type Empty = Record<string, never>;

export interface RpcPayloads {
	'music.search': {q: string};
	'music.add': {input: string; next?: boolean};
	'music.queue': {offset: number; limit: number};
	'music.pause': Empty;
	'music.resume': Empty;
	'music.skip': Empty;
	'music.seek': {ms: number};
	'music.remove': {id: string};
	'music.move': {id: string; to: number};
	'music.clear': Empty;
	'music.shuffle': Empty;
	'music.loop': {mode: LoopMode};
	'music.stop': Empty;
}

export interface RpcResponses {
	'music.search': {tracks: Array<SearchResult>};
	'music.add': {added: number};
	'music.queue': {tracks: Array<Track>; total: number};
	'music.pause': Empty;
	'music.resume': Empty;
	'music.skip': Empty;
	'music.seek': Empty;
	'music.remove': Empty;
	'music.move': Empty;
	'music.clear': Empty;
	'music.shuffle': Empty;
	'music.loop': Empty;
	'music.stop': Empty;
}

export type RpcResult<T> = {ok: true; value: T} | {ok: false; error: MusicErrorCode};

const ERROR_CODES: ReadonlyArray<MusicErrorCode> = [
	'NOT_FOUND',
	'QUEUE_FULL',
	'TOO_LONG',
	'LIVE_NOT_SUPPORTED',
	'NO_RESULTS',
	'SOURCE_FAILED',
	'BAD_REQUEST',
];
const LOOP_MODES: ReadonlyArray<string> = ['off', 'track', 'queue'];
const MAX_QUEUE_PAGE = 20;

export function encodeRpcResult<T>(r: RpcResult<T>): string {
	if (r.ok) return JSON.stringify({...r.value, ok: true});
	return JSON.stringify({error: r.error});
}

export function decodeRpcResult<T>(s: string): RpcResult<T> {
	const failed: RpcResult<T> = {ok: false, error: 'SOURCE_FAILED'};
	let parsed: unknown;
	try {
		parsed = JSON.parse(s);
	} catch {
		return failed;
	}
	if (!isRecord(parsed)) return failed;
	if (parsed.ok === true) {
		const {ok: _ok, ...value} = parsed;
		return {ok: true, value: value as T};
	}
	const code = parsed.error;
	if (typeof code === 'string' && (ERROR_CODES as ReadonlyArray<string>).includes(code)) {
		return {ok: false, error: code as MusicErrorCode};
	}
	return failed;
}

function isRecord(v: unknown): v is Record<string, unknown> {
	return typeof v === 'object' && v !== null && !Array.isArray(v);
}

const isNonEmptyString = (v: unknown): v is string => typeof v === 'string' && v.length > 0;
const isNonNegInt = (v: unknown): v is number => typeof v === 'number' && Number.isInteger(v) && v >= 0;

export function decodeRpcPayload<M extends RpcMethod>(method: M, s: string): RpcPayloads[M] | null {
	let p: unknown;
	try {
		p = JSON.parse(s);
	} catch {
		return null;
	}
	if (!isRecord(p)) return null;
	const result = ((): unknown => {
		switch (method) {
			case 'music.search':
				return isNonEmptyString(p.q) ? {q: p.q} : null;
			case 'music.add':
				if (!isNonEmptyString(p.input)) return null;
				if (p.next !== undefined && typeof p.next !== 'boolean') return null;
				return p.next === undefined ? {input: p.input} : {input: p.input, next: p.next};
			case 'music.queue':
				if (!isNonNegInt(p.offset) || !isNonNegInt(p.limit)) return null;
				return {offset: p.offset, limit: Math.min(p.limit, MAX_QUEUE_PAGE)};
			case 'music.seek':
				return isNonNegInt(p.ms) ? {ms: p.ms} : null;
			case 'music.remove':
				return isNonEmptyString(p.id) ? {id: p.id} : null;
			case 'music.move':
				return isNonEmptyString(p.id) && isNonNegInt(p.to) ? {id: p.id, to: p.to} : null;
			case 'music.loop':
				return typeof p.mode === 'string' && LOOP_MODES.includes(p.mode) ? {mode: p.mode} : null;
			default:
				return {};
		}
	})();
	return result as RpcPayloads[M] | null;
}
