// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {createRateLimitError} from '@app/api/utils/RateLimitUtils';
import {EmailCodeResendCooldownError} from '@fluxer/errors/src/domains/auth/EmailCodeResendCooldownError';
import {getSameIpDecisionKey, getSubnet} from '@fluxer/ip_utils/src/IpAddress';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';

const TEST_ENABLE_RATE_LIMITS_HEADER = 'x-fluxer-test-enable-rate-limits';

export interface BudgetEnv {
	kv: IKVProvider;
	rateLimit: IRateLimitService;
	enforce: boolean;
}

function shouldEnforceAccountBudgets(request: Request | null): boolean {
	if (!Config.dev.testModeEnabled) return !Config.dev.disableRateLimits;
	return request?.headers.get(TEST_ENABLE_RATE_LIMITS_HEADER) === 'true';
}

export function budgetEnvFor(
	services: {kv: IKVProvider; rateLimit: IRateLimitService},
	request: Request | null,
): BudgetEnv {
	return {kv: services.kv, rateLimit: services.rateLimit, enforce: shouldEnforceAccountBudgets(request)};
}

export function ipKey(clientIp: string): string {
	return getSameIpDecisionKey(clientIp) ?? clientIp;
}

interface WindowRule {
	key: string;
	windowSeconds: number;
	free: number;
	doubling: boolean;
}

function windowKey(rule: Pick<WindowRule, 'key' | 'windowSeconds'>, nowMs = Date.now()): string {
	return `${rule.key}:${Math.floor(nowMs / 1000 / rule.windowSeconds)}`;
}

function secondsLeftInWindow(rule: Pick<WindowRule, 'windowSeconds'>, nowMs = Date.now()): number {
	const elapsed = Math.floor(nowMs / 1000) % rule.windowSeconds;
	return rule.windowSeconds - elapsed;
}

async function readWindow(kv: IKVProvider, rule: Pick<WindowRule, 'key' | 'windowSeconds'>): Promise<number> {
	const raw = await kv.get(windowKey(rule));
	const value = raw === null ? 0 : Number.parseInt(raw, 10);
	return Number.isFinite(value) ? value : 0;
}

async function bumpWindow(kv: IKVProvider, rule: Pick<WindowRule, 'key' | 'windowSeconds'>): Promise<number> {
	const key = windowKey(rule);
	const count = await kv.incr(key);
	if (count === 1) await kv.expire(key, rule.windowSeconds + 60);
	return count;
}

async function captchaStepsFor(env: BudgetEnv, rules: ReadonlyArray<WindowRule>): Promise<number | null> {
	if (!env.enforce) return null;
	let steps: number | null = null;
	for (const rule of rules) {
		const count = await readWindow(env.kv, rule);
		if (count < rule.free) continue;
		const ruleSteps = rule.doubling ? count - rule.free + 1 : 0;
		steps = Math.max(steps ?? 0, ruleSteps);
	}
	return steps;
}

async function chargeRules(env: BudgetEnv, rules: ReadonlyArray<WindowRule>): Promise<void> {
	for (const rule of rules) {
		await bumpWindow(env.kv, rule);
	}
}

async function enforceHardLimit(
	env: BudgetEnv,
	identifier: string,
	maxAttempts: number,
	windowSeconds: number,
): Promise<void> {
	const result = await env.rateLimit.checkLimit({identifier, maxAttempts, windowMs: windowSeconds * 1000});
	if (!result.allowed && env.enforce) throw createRateLimitError(result);
}

export type RequesterClass = 'known' | 'unknown';

export function requesterNetworkKey(clientIp: string): string {
	return getSubnet(clientIp) ?? ipKey(clientIp);
}

const HOUR = 3600;
const TEN_MINUTES = 600;
const DAY = 86400;

function addressLookupRules(params: {clientIp: string; emailKey: string | null; requester: RequesterClass}) {
	const rules: Array<WindowRule> = [
		{key: `budget:lookup:ip:${ipKey(params.clientIp)}`, windowSeconds: TEN_MINUTES, free: 10, doubling: false},
		{key: 'budget:lookup:global', windowSeconds: HOUR, free: 20000, doubling: false},
	];
	const subnet = getSubnet(params.clientIp);
	if (subnet) rules.push({key: `budget:lookup:subnet:${subnet}`, windowSeconds: HOUR, free: 100, doubling: false});
	if (params.emailKey) {
		rules.push({
			key: `budget:lookup:address:${params.emailKey}:${params.requester}`,
			windowSeconds: HOUR,
			free: 10,
			doubling: true,
		});
	}
	return rules;
}

export function addressLookupCaptchaSteps(
	env: BudgetEnv,
	params: {clientIp: string; emailKey: string | null; requester: RequesterClass},
): Promise<number | null> {
	return captchaStepsFor(env, addressLookupRules(params));
}

export function chargeAddressLookup(
	env: BudgetEnv,
	params: {clientIp: string; emailKey: string | null; requester: RequesterClass},
): Promise<void> {
	return chargeRules(env, addressLookupRules(params));
}

export async function enforceLookupHardLimits(env: BudgetEnv, clientIp: string): Promise<void> {
	await enforceHardLimit(env, `lookup:hard:ip:${ipKey(clientIp)}`, 30, TEN_MINUTES);
	const subnet = getSubnet(clientIp);
	if (subnet) await enforceHardLimit(env, `lookup:hard:subnet:${subnet}`, 300, HOUR);
}

const EMAIL_SEND_FREE_PER_HOUR = 5;
const EMAIL_SEND_BASE_INTERVAL_SECONDS = 30;
const EMAIL_SEND_MAX_INTERVAL_SECONDS = 900;
const CODE_FAILURES_PER_STEP = 5;
export const CODE_FAILURES_DAILY_CAP = 30;
export const ACCOUNT_CODE_FAILURES_DAILY_CAP = 50;
export const ADDRESS_CODE_FAILURES_PER_STEP = 10;
export const ADDRESS_CODE_FAILURES_DAILY_CAP = 100;
export const GLOBAL_CODE_FAILURES_THRESHOLD = 500;

function emailSendRule(emailKey: string, requester: RequesterClass): WindowRule {
	return {
		key: `email_send:${emailKey}:${requester}`,
		windowSeconds: HOUR,
		free: EMAIL_SEND_FREE_PER_HOUR,
		doubling: true,
	};
}

function lastSendKey(emailKey: string, requester: RequesterClass): string {
	return `email_send_last:${emailKey}:${requester}`;
}

function codeFailuresRule(emailKey: string, network: string) {
	return {key: `auth:email_code_failures:${emailKey}:${network}`, windowSeconds: DAY};
}

function addressCodeFailuresRule(emailKey: string) {
	return {key: `auth:email_code_failures_any:${emailKey}`, windowSeconds: DAY};
}

const GLOBAL_CODE_FAILURES_RULE = {key: 'auth:email_code_failures_global', windowSeconds: HOUR};

function accountCodeFailuresRule(userId: string) {
	return {key: `auth:account_code_failures:${userId}`, windowSeconds: DAY};
}

export async function emailSendCaptchaSteps(
	env: BudgetEnv,
	params: {emailKey: string; requester: RequesterClass; clientIp: string},
): Promise<number | null> {
	if (!env.enforce) return null;
	const sendSteps = await captchaStepsFor(env, [emailSendRule(params.emailKey, params.requester)]);
	if (params.requester === 'known') return sendSteps;
	const failures = await readWindow(env.kv, codeFailuresRule(params.emailKey, requesterNetworkKey(params.clientIp)));
	const failureSteps = failures >= CODE_FAILURES_PER_STEP ? Math.floor(failures / CODE_FAILURES_PER_STEP) : null;
	if (sendSteps === null) return failureSteps;
	return Math.max(sendSteps, failureSteps ?? 0);
}

export async function assertEmailSendAllowed(
	env: BudgetEnv,
	params: {emailKey: string; requester: RequesterClass; clientIp: string; userId?: string},
): Promise<void> {
	await enforceHardLimit(env, `email_send:ip:${ipKey(params.clientIp)}`, 20, HOUR);
	const subnet = getSubnet(params.clientIp);
	if (subnet) await enforceHardLimit(env, `email_send:subnet:${subnet}`, 100, HOUR);
	if (!env.enforce) return;
	if (params.userId) {
		const accountRule = accountCodeFailuresRule(params.userId);
		if ((await readWindow(env.kv, accountRule)) >= ACCOUNT_CODE_FAILURES_DAILY_CAP) {
			throw new EmailCodeResendCooldownError(secondsLeftInWindow(accountRule));
		}
	}
	if (params.requester === 'unknown') {
		const failuresRule = codeFailuresRule(params.emailKey, requesterNetworkKey(params.clientIp));
		if ((await readWindow(env.kv, failuresRule)) >= CODE_FAILURES_DAILY_CAP) {
			throw new EmailCodeResendCooldownError(secondsLeftInWindow(failuresRule));
		}
		const addressRule = addressCodeFailuresRule(params.emailKey);
		if ((await readWindow(env.kv, addressRule)) >= ADDRESS_CODE_FAILURES_DAILY_CAP) {
			throw new EmailCodeResendCooldownError(secondsLeftInWindow(addressRule));
		}
	}
	const count = await readWindow(env.kv, emailSendRule(params.emailKey, params.requester));
	if (count < EMAIL_SEND_FREE_PER_HOUR) return;
	const interval = Math.min(
		EMAIL_SEND_BASE_INTERVAL_SECONDS * 2 ** (count - EMAIL_SEND_FREE_PER_HOUR),
		EMAIL_SEND_MAX_INTERVAL_SECONDS,
	);
	const lastRaw = await env.kv.get(lastSendKey(params.emailKey, params.requester));
	const last = lastRaw === null ? 0 : Number.parseInt(lastRaw, 10);
	const waited = Math.floor((Date.now() - last) / 1000);
	if (waited < interval) throw new EmailCodeResendCooldownError(interval - waited);
}

export async function recordEmailSend(env: BudgetEnv, params: {emailKey: string; requester: RequesterClass}) {
	await bumpWindow(env.kv, emailSendRule(params.emailKey, params.requester));
	await env.kv.setex(lastSendKey(params.emailKey, params.requester), HOUR, Date.now().toString());
}

export async function recordAddressCodeFailure(
	env: BudgetEnv,
	params: {emailKey: string; network: string},
): Promise<void> {
	await bumpWindow(env.kv, codeFailuresRule(params.emailKey, params.network));
	await bumpWindow(env.kv, addressCodeFailuresRule(params.emailKey));
	await bumpWindow(env.kv, GLOBAL_CODE_FAILURES_RULE);
}

export async function recordAccountCodeFailure(env: BudgetEnv, userId: string): Promise<void> {
	await bumpWindow(env.kv, accountCodeFailuresRule(userId));
	await bumpWindow(env.kv, GLOBAL_CODE_FAILURES_RULE);
}

export async function codeVerifyCaptchaSteps(
	env: BudgetEnv,
	params: {emailKey: string; requester: RequesterClass},
): Promise<number | null> {
	if (!env.enforce) return null;
	const global = await readWindow(env.kv, GLOBAL_CODE_FAILURES_RULE);
	let steps = global >= GLOBAL_CODE_FAILURES_THRESHOLD ? 1 : null;
	if (params.requester === 'unknown') {
		const failures = await readWindow(env.kv, addressCodeFailuresRule(params.emailKey));
		if (failures >= ADDRESS_CODE_FAILURES_PER_STEP) {
			steps = Math.max(steps ?? 0, Math.floor(failures / ADDRESS_CODE_FAILURES_PER_STEP));
		}
	}
	return steps;
}

export async function enforceInstantAccountLimits(env: BudgetEnv, clientIp: string): Promise<void> {
	const key = ipKey(clientIp);
	await enforceHardLimit(env, `registration:instant:ip:${key}:hour`, 5, HOUR);
	await enforceHardLimit(env, `registration:instant:ip:${key}:day`, 20, DAY);
	const subnet = getSubnet(clientIp);
	if (subnet) await enforceHardLimit(env, `registration:instant:subnet:${subnet}`, 40, HOUR);
}

function instantIpRule(clientIp: string): WindowRule {
	return {key: `registration:instant:created:${ipKey(clientIp)}`, windowSeconds: HOUR, free: 0, doubling: true};
}

const INSTANT_GLOBAL_RULE = {key: 'registration:instant:global', windowSeconds: HOUR};

export async function instantAccountCaptchaSteps(
	env: BudgetEnv,
	params: {clientIp: string; escalationPerHour: number},
): Promise<number> {
	const createdFromIp = await readWindow(env.kv, instantIpRule(params.clientIp));
	const createdGlobally = await readWindow(env.kv, INSTANT_GLOBAL_RULE);
	return createdFromIp + Math.floor(createdGlobally / params.escalationPerHour);
}

export async function recordInstantAccountCreated(env: BudgetEnv, clientIp: string): Promise<void> {
	await bumpWindow(env.kv, instantIpRule(clientIp));
	await bumpWindow(env.kv, INSTANT_GLOBAL_RULE);
}
