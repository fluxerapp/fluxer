// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomUUID} from 'node:crypto';
import {extractClientIp} from '@fluxer/ip_utils/src/ClientIp';
import {getSameIpDecisionKey, isPublicIpAddress, parseIpAddress} from '@fluxer/ip_utils/src/IpAddress';
import type {IpInfoLookupResult} from '@pkgs/geoip/src/IpInfoService';
import type {IKVProvider, IKVSubscription} from '@pkgs/kv_client/src/IKVProvider';
import {createMiddleware} from 'hono/factory';
import {AdminRepository} from '../admin/AdminRepository';
import {Config} from '../Config';
import {IP_BAN_REFRESH_CHANNEL} from '../constants/IpBan';
import {Logger} from '../Logger';
import {isIpBanExempt} from '../risk/IpBanExemptions';
import type {HonoEnv} from '../types/HonoEnv';
import {parseJsonRecord} from '../utils/JsonBoundaryUtils';
import {ipBanCache} from './IpBanMiddleware';
import {getIpInfoService} from './ServiceMiddleware';
import {getCacheService} from './ServiceSingletons';

type IpClass = 'datacenter' | 'anonymous' | 'mobile' | 'residential' | 'unknown';
type TriggerKind = 'score' | 'token_diversity' | 'score_and_token_diversity';

interface AbuseRecord {
	score: number;
	windowStartMs: number;
	autoBanFired: boolean;
	triggeringReason: string | null;
	lookupIp: string;
	distinctTokenHashes: Set<string>;
}

interface OutboundEntry {
	scoreDelta: number;
	lookupIp: string;
	newTokenHashes: Set<string>;
}

interface PersistentScoreState {
	count: number;
	lastWindowStartMs: number;
	expiresAtMs: number;
}

interface AbuseSignalIp {
	banKey: string;
	lookupIp: string;
}

interface AbuseSignalOptions {
	tokenHash?: string;
	weight?: number;
}

const WINDOW_MS = positiveNumberFromEnv('FLUXER_ABUSE_WINDOW_MS', 60_000);
const THRESHOLD_DATACENTER = positiveNumberFromEnv('FLUXER_ABUSE_THRESHOLD_DATACENTER', 20);
const THRESHOLD_ANONYMOUS = positiveNumberFromEnv('FLUXER_ABUSE_THRESHOLD_ANONYMOUS', 500);
const THRESHOLD_MOBILE = positiveNumberFromEnv('FLUXER_ABUSE_THRESHOLD_MOBILE', 1000);
const THRESHOLD_RESIDENTIAL = positiveNumberFromEnv('FLUXER_ABUSE_THRESHOLD_RESIDENTIAL', 100);
const TOKEN_DIVERSITY_DATACENTER = positiveNumberFromEnv('FLUXER_ABUSE_TOKEN_DIVERSITY_DATACENTER', 10);
const TOKEN_DIVERSITY_ANONYMOUS = positiveNumberFromEnv('FLUXER_ABUSE_TOKEN_DIVERSITY_ANONYMOUS', 50);
const TOKEN_DIVERSITY_MOBILE = positiveNumberFromEnv('FLUXER_ABUSE_TOKEN_DIVERSITY_MOBILE', 100);
const TOKEN_DIVERSITY_RESIDENTIAL = positiveNumberFromEnv('FLUXER_ABUSE_TOKEN_DIVERSITY_RESIDENTIAL', 10);
const AUTO_BAN_TTL_SECONDS = positiveNumberFromEnv('FLUXER_ABUSE_BAN_TTL_SEC', 86_400);
const BATCH_FLUSH_MS = positiveNumberFromEnv('FLUXER_ABUSE_BATCH_FLUSH_MS', 500);
const MAX_BATCH_TICKS = positiveNumberFromEnv('FLUXER_ABUSE_MAX_BATCH_TICKS', 5000);
const MAX_NEW_TOKENS_PER_TICK = positiveNumberFromEnv('FLUXER_ABUSE_MAX_NEW_TOKENS_PER_TICK', 20);
const MAX_TRACKED_IPS = positiveNumberFromEnv('FLUXER_ABUSE_MAX_TRACKED_IPS', 100_000);
const MAX_TOKEN_HASHES_PER_IP = positiveNumberFromEnv('FLUXER_ABUSE_MAX_TOKEN_HASHES_PER_IP', 100);
const MIN_SCORE_FOR_IP_LOOKUP = positiveNumberFromEnv('FLUXER_ABUSE_MIN_SCORE_FOR_LOOKUP', 20);
const MIN_TOKENS_FOR_IP_LOOKUP = positiveNumberFromEnv('FLUXER_ABUSE_MIN_TOKENS_FOR_LOOKUP', 10);
const REQUIRED_SCORE_WINDOWS_FOR_AUTO_BAN = positiveNumberFromEnv(
	'FLUXER_ABUSE_REQUIRED_SCORE_WINDOWS_FOR_AUTO_BAN',
	3,
);
const REPLICATION_CHANNEL = 'abuse_tracker:ticks';
const POD_ID = process.env.HOSTNAME ?? randomUUID();

type ReplicatedTick = [banKey: string, scoreDelta: number, tokenHashes: Array<string>, lookupIp: string];

interface ReplicationMessage {
	sender: string;
	ticks: Array<ReplicatedTick>;
	ts: number;
}

const records = new Map<string, AbuseRecord>();
const outboundDeltas = new Map<string, OutboundEntry>();
const persistentScoreWindows = new Map<string, PersistentScoreState>();
const ipClassCache = new Map<string, IpClass>();
const ipClassPending = new Set<string>();
const recordedClientErrorRequests = new WeakSet<Request>();
const pendingAutoBanTasks = new Set<Promise<void>>();
const adminRepository = new AdminRepository();

let kvPublisher: IKVProvider | null = null;
let flushTimer: NodeJS.Timeout | null = null;
let kvSubscription: IKVSubscription | null = null;
let messageHandler: ((channel: string, message: string) => void) | null = null;
let errorHandler: ((error: Error) => void) | null = null;

function positiveNumberFromEnv(name: string, fallback: number): number {
	const raw = process.env[name];
	if (!raw) return fallback;
	const parsed = Number(raw);
	return Number.isFinite(parsed) && parsed > 0 ? parsed : fallback;
}

function clientErrorWeight(status: number): number {
	if (status === 429) return 3;
	if (status === 401) return 0.75;
	if (status === 403) return 0.5;
	if (status === 404) return 0.25;
	if (status >= 400 && status < 500) return 0.25;
	return 0;
}

function normalizeSignalIp(ip: string | null): AbuseSignalIp | null {
	if (!ip) return null;
	const parsed = parseIpAddress(ip);
	if (!parsed) return null;
	if (!isPublicIpAddress(parsed.normalized)) return null;
	if (isIpBanExempt(parsed.normalized)) return null;
	const banKey = getSameIpDecisionKey(parsed.normalized) ?? parsed.normalized;
	return {banKey, lookupIp: parsed.normalized};
}

function classifyIpInfo(result: IpInfoLookupResult): IpClass {
	if (!result.available) return 'unknown';
	if (result.flags.isMobile || result.mobile.name) return 'mobile';
	if (result.anonymous.isAnonymous) return 'anonymous';
	if (result.flags.isHosting) return 'datacenter';
	return 'residential';
}

function scoreThresholdFor(ipClass: IpClass): number {
	switch (ipClass) {
		case 'datacenter':
			return THRESHOLD_DATACENTER;
		case 'anonymous':
			return THRESHOLD_ANONYMOUS;
		case 'mobile':
			return THRESHOLD_MOBILE;
		case 'residential':
		case 'unknown':
			return THRESHOLD_RESIDENTIAL;
	}
}

function tokenDiversityThresholdFor(ipClass: IpClass): number {
	switch (ipClass) {
		case 'datacenter':
			return TOKEN_DIVERSITY_DATACENTER;
		case 'anonymous':
			return TOKEN_DIVERSITY_ANONYMOUS;
		case 'mobile':
			return TOKEN_DIVERSITY_MOBILE;
		case 'residential':
		case 'unknown':
			return TOKEN_DIVERSITY_RESIDENTIAL;
	}
}

function shouldSkipAutoBanForIpClass(ipClass: IpClass): boolean {
	return ipClass === 'mobile';
}

function pruneIfNeeded(now: number): void {
	if (records.size < MAX_TRACKED_IPS) return;
	for (const [key, rec] of records) {
		if (rec.windowStartMs + WINDOW_MS < now) {
			records.delete(key);
			ipClassCache.delete(key);
			ipClassPending.delete(key);
		}
		if (records.size < MAX_TRACKED_IPS * 0.9) return;
	}
}

function getOrResetRecord(signalIp: AbuseSignalIp, now: number, reason: string): AbuseRecord {
	let rec = records.get(signalIp.banKey);
	if (!rec || rec.windowStartMs + WINDOW_MS < now) {
		pruneIfNeeded(now);
		rec = {
			score: 0,
			windowStartMs: now,
			autoBanFired: false,
			triggeringReason: reason,
			lookupIp: signalIp.lookupIp,
			distinctTokenHashes: new Set(),
		};
		records.set(signalIp.banKey, rec);
		return rec;
	}
	rec.lookupIp = signalIp.lookupIp;
	if (!rec.triggeringReason) rec.triggeringReason = reason;
	return rec;
}

function getOrInitOutbound(signalIp: AbuseSignalIp): OutboundEntry {
	let entry = outboundDeltas.get(signalIp.banKey);
	if (!entry) {
		entry = {scoreDelta: 0, lookupIp: signalIp.lookupIp, newTokenHashes: new Set()};
		outboundDeltas.set(signalIp.banKey, entry);
		return entry;
	}
	entry.lookupIp = signalIp.lookupIp;
	return entry;
}

function queueOutboundDelta(
	signalIp: AbuseSignalIp,
	weight: number,
	tokenHash: string | undefined,
	hadToken: boolean,
): void {
	if (!kvPublisher) return;
	if (!outboundDeltas.has(signalIp.banKey) && outboundDeltas.size >= MAX_TRACKED_IPS) return;
	const outbound = getOrInitOutbound(signalIp);
	outbound.scoreDelta += weight;
	if (tokenHash && !hadToken) {
		outbound.newTokenHashes.add(tokenHash);
	}
}

function shouldEnsureIpClassLookup(key: string, rec: AbuseRecord): boolean {
	if (ipClassCache.has(key) || ipClassPending.has(key)) return false;
	return rec.score >= MIN_SCORE_FOR_IP_LOOKUP || rec.distinctTokenHashes.size >= MIN_TOKENS_FOR_IP_LOOKUP;
}

function markScoreThresholdWindow(key: string, rec: AbuseRecord, now: number): number {
	const expiresAtMs = rec.windowStartMs + WINDOW_MS * (REQUIRED_SCORE_WINDOWS_FOR_AUTO_BAN + 2);
	let state = persistentScoreWindows.get(key);
	if (!state || state.expiresAtMs <= now) {
		state = {
			count: 1,
			lastWindowStartMs: rec.windowStartMs,
			expiresAtMs,
		};
		persistentScoreWindows.set(key, state);
		return state.count;
	}
	state.expiresAtMs = expiresAtMs;
	if (state.lastWindowStartMs !== rec.windowStartMs) {
		state.count += 1;
		state.lastWindowStartMs = rec.windowStartMs;
	}
	return state.count;
}

function ensureIpClassLookup(key: string, lookupIp: string): void {
	if (ipClassCache.has(key) || ipClassPending.has(key)) return;
	ipClassPending.add(key);
	void (async () => {
		try {
			const result = await getIpInfoService().lookup(lookupIp, {source: 'AbusiveIpAutoBanner', reason: 'classify'});
			ipClassCache.set(key, classifyIpInfo(result));
		} catch (err) {
			ipClassCache.set(key, 'unknown');
			Logger.warn({err, ip: lookupIp}, '[abuse-auto-ban] IP classification lookup failed');
		} finally {
			ipClassPending.delete(key);
			const rec = records.get(key);
			if (rec) maybeFireAutoBan(key, rec);
		}
	})();
}

function maybeFireAutoBan(key: string, rec: AbuseRecord): void {
	if (rec.autoBanFired) return;
	const ipClass = ipClassCache.get(key) ?? 'unknown';
	const scoreThreshold = scoreThresholdFor(ipClass);
	const tokenThreshold = tokenDiversityThresholdFor(ipClass);
	const overScore = rec.score >= scoreThreshold;
	const overTokenDiversity = rec.distinctTokenHashes.size >= tokenThreshold;
	if (!overScore && !overTokenDiversity) return;
	if (!ipClassCache.has(key) && ipClassPending.has(key)) {
		return;
	}
	if (shouldSkipAutoBanForIpClass(ipClass)) {
		rec.autoBanFired = true;
		Logger.warn(
			{ip: key, ipClass, score: rec.score, distinctTokens: rec.distinctTokenHashes.size},
			'[abuse-auto-ban] Skipping automatic IP ban because the IP class has high CGNAT blast-radius risk',
		);
		return;
	}
	if (overScore && !overTokenDiversity) {
		const persistentWindows = markScoreThresholdWindow(key, rec, Date.now());
		if (persistentWindows < REQUIRED_SCORE_WINDOWS_FOR_AUTO_BAN) {
			return;
		}
	}
	rec.autoBanFired = true;
	const trigger: TriggerKind = overScore
		? overTokenDiversity
			? 'score_and_token_diversity'
			: 'score'
		: 'token_diversity';
	const task = executeAutoBan(key, {
		lookupIp: rec.lookupIp,
		ipClass,
		score: rec.score,
		scoreThreshold,
		distinctTokens: rec.distinctTokenHashes.size,
		tokenDiversityThreshold: tokenThreshold,
		triggeringReason: rec.triggeringReason ?? 'unknown',
		trigger,
	});
	pendingAutoBanTasks.add(task);
	task.finally(() => pendingAutoBanTasks.delete(task));
}

async function executeAutoBan(
	key: string,
	ctx: {
		lookupIp: string;
		ipClass: IpClass;
		score: number;
		scoreThreshold: number;
		distinctTokens: number;
		tokenDiversityThreshold: number;
		triggeringReason: string;
		trigger: TriggerKind;
	},
): Promise<void> {
	if (!isPublicIpAddress(ctx.lookupIp)) {
		return;
	}
	if (isIpBanExempt(ctx.lookupIp)) {
		return;
	}
	try {
		await adminRepository.banIpTemp(key, AUTO_BAN_TTL_SECONDS);
		ipBanCache.banTemp(key, AUTO_BAN_TTL_SECONDS);
		await getCacheService().publish(IP_BAN_REFRESH_CHANNEL, 'refresh');
		Logger.warn(
			{
				ip: key,
				ttlSeconds: AUTO_BAN_TTL_SECONDS,
				windowMs: WINDOW_MS,
				...ctx,
			},
			'[abuse-auto-ban] Auto-banning abusive IP',
		);
	} catch (err) {
		const rec = records.get(key);
		if (rec) rec.autoBanFired = false;
		Logger.error({err, ip: key}, '[abuse-auto-ban] Auto-ban write failed; will retry on next signal');
	}
}

export function hashAuthToken(token: string): string {
	return createHash('sha256').update(token).digest('hex').slice(0, 32);
}

export function recordAbuseSignal(ip: string | null, reason: string, opts: AbuseSignalOptions = {}): void {
	const signalIp = normalizeSignalIp(ip);
	if (!signalIp) return;
	const weight = opts.weight ?? 1;
	if (!Number.isFinite(weight) || weight <= 0) return;
	const now = Date.now();
	const rec = getOrResetRecord(signalIp, now, reason);
	rec.score += weight;
	let hadToken = false;
	let queuedTokenHash: string | undefined;
	if (opts.tokenHash && rec.distinctTokenHashes.size < MAX_TOKEN_HASHES_PER_IP) {
		hadToken = rec.distinctTokenHashes.has(opts.tokenHash);
		rec.distinctTokenHashes.add(opts.tokenHash);
		queuedTokenHash = opts.tokenHash;
	}
	queueOutboundDelta(signalIp, weight, queuedTokenHash, hadToken);
	if (shouldEnsureIpClassLookup(signalIp.banKey, rec)) {
		ensureIpClassLookup(signalIp.banKey, signalIp.lookupIp);
	}
	maybeFireAutoBan(signalIp.banKey, rec);
}

export function recordHttpClientError(request: Request, status: number, reason = `http_${status}`): void {
	const weight = clientErrorWeight(status);
	if (weight <= 0) return;
	if (recordedClientErrorRequests.has(request)) return;
	recordedClientErrorRequests.add(request);
	const clientIp = extractClientIp(request, {
		trustClientIpHeader: Config.proxy.trust_client_ip_header,
		clientIpHeaderName: Config.proxy.client_ip_header,
	});
	recordAbuseSignal(clientIp, reason, {weight});
}

export const ClientErrorAbuseSignalMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	await next();
	if (ctx.get('user')) return;
	recordHttpClientError(ctx.req.raw, ctx.res.status);
});

function applyReplicatedTick(tick: ReplicatedTick): void {
	const [banKey, scoreDelta, tokenHashes, lookupIp] = tick;
	const signalIp = normalizeSignalIp(lookupIp);
	if (!signalIp || signalIp.banKey !== banKey) return;
	const now = Date.now();
	const rec = getOrResetRecord(signalIp, now, 'replicated');
	rec.score += scoreDelta;
	for (const tokenHash of tokenHashes) {
		if (rec.distinctTokenHashes.size >= MAX_TOKEN_HASHES_PER_IP) break;
		rec.distinctTokenHashes.add(tokenHash);
	}
	if (shouldEnsureIpClassLookup(banKey, rec)) {
		ensureIpClassLookup(banKey, lookupIp);
	}
	maybeFireAutoBan(banKey, rec);
}

async function flushOutbound(): Promise<void> {
	if (!kvPublisher || outboundDeltas.size === 0) return;
	const ticks: Array<ReplicatedTick> = [];
	const selectedKeys: Array<string> = [];
	for (const [key, entry] of outboundDeltas) {
		const tokenHashes: Array<string> = [];
		for (const tokenHash of entry.newTokenHashes) {
			tokenHashes.push(tokenHash);
			if (tokenHashes.length >= MAX_NEW_TOKENS_PER_TICK) break;
		}
		ticks.push([key, entry.scoreDelta, tokenHashes, entry.lookupIp]);
		selectedKeys.push(key);
		if (ticks.length >= MAX_BATCH_TICKS) break;
	}
	const message: ReplicationMessage = {sender: POD_ID, ticks, ts: Date.now()};
	try {
		await kvPublisher.publish(REPLICATION_CHANNEL, JSON.stringify(message));
		for (const key of selectedKeys) {
			outboundDeltas.delete(key);
		}
	} catch (err) {
		Logger.warn({err, tickCount: ticks.length}, '[abuse-auto-ban] Failed to publish abuse replication batch');
	}
}

function handleReplicationMessage(channel: string, message: string): void {
	if (channel !== REPLICATION_CHANNEL) return;
	const msg = parseJsonRecord(message);
	if (!msg || msg.sender === POD_ID || !Array.isArray(msg.ticks)) return;
	for (const rawTick of msg.ticks) {
		if (!Array.isArray(rawTick) || rawTick.length < 4) continue;
		const [banKey, scoreDelta, rawTokenHashes, lookupIp] = rawTick;
		if (
			typeof banKey !== 'string' ||
			typeof scoreDelta !== 'number' ||
			!Number.isFinite(scoreDelta) ||
			scoreDelta <= 0 ||
			typeof lookupIp !== 'string'
		) {
			continue;
		}
		const tokenHashes = Array.isArray(rawTokenHashes)
			? rawTokenHashes.filter((tokenHash): tokenHash is string => typeof tokenHash === 'string')
			: [];
		applyReplicatedTick([banKey, scoreDelta, tokenHashes, lookupIp]);
	}
}

export async function startAbuseReplicationSubscriber(kvClient: IKVProvider | null): Promise<void> {
	if (!kvClient || kvSubscription) return;
	kvPublisher = kvClient;
	const subscription = kvClient.duplicate();
	kvSubscription = subscription;
	messageHandler = handleReplicationMessage;
	errorHandler = (error: Error) => {
		Logger.warn({error}, '[abuse-auto-ban] Abuse replication subscription error');
	};
	try {
		await subscription.connect();
		await subscription.subscribe(REPLICATION_CHANNEL);
		subscription.on('message', messageHandler);
		subscription.on('error', errorHandler);
		flushTimer = setInterval(() => {
			void flushOutbound();
		}, BATCH_FLUSH_MS);
		if (typeof flushTimer === 'object' && flushTimer && 'unref' in flushTimer) {
			(flushTimer as {unref(): void}).unref();
		}
	} catch (err) {
		Logger.error({err}, '[abuse-auto-ban] Failed to start abuse replication subscriber');
		kvSubscription = null;
		messageHandler = null;
		errorHandler = null;
		kvPublisher = null;
		throw err;
	}
}

export async function stopAbuseReplicationSubscriber(): Promise<void> {
	if (flushTimer) {
		clearInterval(flushTimer);
		flushTimer = null;
	}
	if (kvSubscription && messageHandler) {
		kvSubscription.off('message', messageHandler);
	}
	if (kvSubscription && errorHandler) {
		kvSubscription.off('error', errorHandler);
	}
	if (kvSubscription) {
		try {
			await kvSubscription.disconnect();
		} catch {}
	}
	kvSubscription = null;
	messageHandler = null;
	errorHandler = null;
	kvPublisher = null;
}

export async function drainAbuseAutoBanTasksForTests(): Promise<void> {
	await Promise.all([...pendingAutoBanTasks]);
}

export function resetAbuseTrackingForTests(): void {
	records.clear();
	outboundDeltas.clear();
	persistentScoreWindows.clear();
	ipClassCache.clear();
	ipClassPending.clear();
	pendingAutoBanTasks.clear();
}
