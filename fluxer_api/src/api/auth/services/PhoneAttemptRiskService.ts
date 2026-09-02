// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac, randomBytes, randomUUID} from 'node:crypto';
import {getSameIpDecisionKey, getSubnet} from '@fluxer/ip_utils/src/IpAddress';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {Logger} from '../../Logger';

const WINDOW_SECONDS = 24 * 60 * 60;
const HARD_BLOCK_TTL_SECONDS = 24 * 60 * 60;
const IP_BLOCK_TTL_SECONDS = 60 * 60;
const HMAC_ROTATE_AFTER_MS = 28 * 24 * 60 * 60 * 1000;
const HMAC_KEY_TTL_SECONDS = 31 * 24 * 60 * 60;
const HMAC_KEY_CACHE_KEY = 'auth:phone:risk:hmac-key-state';
const KEY_PREFIX = 'auth:phone:risk:';

interface PhoneAttemptRiskThresholds {
	userDistinctPrefixCaptcha: number;
	userDistinctPhonesHardBlock: number;
	userDistinctCountriesRequireInbound: number;
	ipDistinctPhonesHardBlock: number;
	ipDistinctUsersHardBlock: number;
	subnetDistinctPhonesHardBlock: number;
}

const DEFAULT_THRESHOLDS: PhoneAttemptRiskThresholds = {
	userDistinctPrefixCaptcha: 3,
	userDistinctPhonesHardBlock: 5,
	userDistinctCountriesRequireInbound: 3,
	ipDistinctPhonesHardBlock: 10,
	ipDistinctUsersHardBlock: 3,
	subnetDistinctPhonesHardBlock: 30,
};

type PhoneAttemptRiskDecision = 'allow' | 'require_captcha' | 'require_inbound' | 'hard_block';
type PhoneAttemptRiskReason =
	| 'user_prefix_sweep'
	| 'user_rejected_burst'
	| 'user_country_sweep'
	| 'ip_distinct_phones'
	| 'ip_user_pool'
	| 'subnet_distinct_phones'
	| 'user_already_hard_blocked'
	| 'ip_already_hard_blocked'
	| 'subnet_already_hard_blocked';

interface PhoneAttemptRiskEvaluation {
	decision: PhoneAttemptRiskDecision;
	reason: PhoneAttemptRiskReason | null;
	counters: {
		userAttempts: number;
		userRejected: number;
		userDistinctPrefix: number;
		userDistinctCountries: number;
		ipDistinctPhones: number;
		ipDistinctUsers: number;
		subnetDistinctPhones: number;
	};
}

const ZERO_COUNTERS: PhoneAttemptRiskEvaluation['counters'] = {
	userAttempts: 0,
	userRejected: 0,
	userDistinctPrefix: 0,
	userDistinctCountries: 0,
	ipDistinctPhones: 0,
	ipDistinctUsers: 0,
	subnetDistinctPhones: 0,
};

interface PhoneAttemptRiskInput {
	userId?: string | bigint | null;
	clientIp?: string | null;
	phone: string;
	countryCode?: string | null;
}

interface PhoneAttemptRecordInput extends PhoneAttemptRiskInput {
	rejected: boolean;
}

interface HmacKey {
	id: string;
	secret: string;
	created_at: string;
}

interface HmacKeyState {
	primary: HmacKey;
	secondary: HmacKey | null;
}

function newKey(): HmacKey {
	return {id: randomUUID(), secret: randomBytes(32).toString('base64url'), created_at: new Date().toISOString()};
}

function shouldRotate(key: HmacKey): boolean {
	return Date.now() - new Date(key.created_at).getTime() > HMAC_ROTATE_AFTER_MS;
}

function hmac(value: string, key: HmacKey): string {
	return createHmac('sha256', Buffer.from(key.secret, 'base64url')).update(value).digest('base64url');
}

function prefix6(phone: string): string {
	return phone.slice(0, 6);
}

function userKey(userId: string | bigint, suffix: string): string {
	return `${KEY_PREFIX}user:${String(userId)}:${suffix}`;
}

function ipKey(ip: string, suffix: string): string {
	return `${KEY_PREFIX}ip:${getSameIpDecisionKey(ip) ?? ip}:${suffix}`;
}

function subnetKey(subnet: string, suffix: string): string {
	return `${KEY_PREFIX}ipnet:${subnet}:${suffix}`;
}

type PhoneAttemptHardBlockHook = (params: {
	userId: string | null;
	clientIp: string | null;
	reason: PhoneAttemptRiskReason;
}) => void;

export class PhoneAttemptRiskService {
	private readonly cache: ICacheService;
	private readonly kv: IKVProvider;
	private readonly thresholds: PhoneAttemptRiskThresholds;
	private readonly hardBlockHooks: Array<PhoneAttemptHardBlockHook> = [];
	private keyState: HmacKeyState | null = null;

	constructor(cache: ICacheService, kv: IKVProvider, thresholds: Partial<PhoneAttemptRiskThresholds> = {}) {
		this.cache = cache;
		this.kv = kv;
		this.thresholds = {...DEFAULT_THRESHOLDS, ...thresholds};
	}

	onHardBlock(hook: PhoneAttemptHardBlockHook): void {
		this.hardBlockHooks.push(hook);
	}

	async evaluate(input: PhoneAttemptRiskInput): Promise<PhoneAttemptRiskEvaluation> {
		const evaluation: PhoneAttemptRiskEvaluation = {
			decision: 'allow',
			reason: null,
			counters: {...ZERO_COUNTERS},
		};
		if (input.userId !== null && input.userId !== undefined && input.userId !== '') {
			if ((await this.kv.exists(userKey(input.userId, 'hard_block'))) > 0) {
				evaluation.decision = 'hard_block';
				evaluation.reason = 'user_already_hard_blocked';
				return evaluation;
			}
		}
		if (input.clientIp) {
			if ((await this.kv.exists(ipKey(input.clientIp, 'hard_block'))) > 0) {
				evaluation.decision = 'hard_block';
				evaluation.reason = 'ip_already_hard_blocked';
				return evaluation;
			}
			const subnet = getSubnet(input.clientIp) ?? input.clientIp;
			if ((await this.kv.exists(subnetKey(subnet, 'hard_block'))) > 0) {
				evaluation.decision = 'hard_block';
				evaluation.reason = 'subnet_already_hard_blocked';
				return evaluation;
			}
		}
		if (input.userId !== null && input.userId !== undefined && input.userId !== '') {
			const userAttempts = await this.peekCounter(userKey(input.userId, 'attempts'));
			const userRejected = await this.peekCounter(userKey(input.userId, 'rejected'));
			const userDistinctPrefix = await this.kv.scard(userKey(input.userId, 'prefix6'));
			const userDistinctCountries = await this.kv.scard(userKey(input.userId, 'countries'));
			evaluation.counters.userAttempts = userAttempts;
			evaluation.counters.userRejected = userRejected;
			evaluation.counters.userDistinctPrefix = userDistinctPrefix;
			evaluation.counters.userDistinctCountries = userDistinctCountries;
			const rejectedRatio = userAttempts > 0 ? userRejected / userAttempts : 0;
			if (userAttempts >= this.thresholds.userDistinctPhonesHardBlock && rejectedRatio >= 0.5) {
				evaluation.decision = 'hard_block';
				evaluation.reason = 'user_rejected_burst';
				return evaluation;
			}
			if (userDistinctCountries >= this.thresholds.userDistinctCountriesRequireInbound) {
				evaluation.decision = 'require_inbound';
				evaluation.reason = 'user_country_sweep';
				return evaluation;
			}
			if (userDistinctPrefix >= this.thresholds.userDistinctPrefixCaptcha) {
				evaluation.decision = 'require_captcha';
				evaluation.reason = 'user_prefix_sweep';
				return evaluation;
			}
		}
		if (input.clientIp) {
			const ipDistinctPhones = await this.kv.scard(ipKey(input.clientIp, 'phones'));
			const ipDistinctUsers = await this.kv.scard(ipKey(input.clientIp, 'users'));
			evaluation.counters.ipDistinctPhones = ipDistinctPhones;
			evaluation.counters.ipDistinctUsers = ipDistinctUsers;
			if (
				ipDistinctPhones >= this.thresholds.ipDistinctPhonesHardBlock &&
				ipDistinctUsers >= this.thresholds.ipDistinctUsersHardBlock
			) {
				evaluation.decision = 'hard_block';
				evaluation.reason = 'ip_user_pool';
				return evaluation;
			}
			const subnet = getSubnet(input.clientIp) ?? input.clientIp;
			const subnetDistinctPhones = await this.kv.scard(subnetKey(subnet, 'phones'));
			evaluation.counters.subnetDistinctPhones = subnetDistinctPhones;
			if (subnetDistinctPhones >= this.thresholds.subnetDistinctPhonesHardBlock) {
				evaluation.decision = 'hard_block';
				evaluation.reason = 'subnet_distinct_phones';
				return evaluation;
			}
		}
		return evaluation;
	}

	async record(input: PhoneAttemptRecordInput): Promise<PhoneAttemptRiskEvaluation> {
		const key = await this.getCurrentKey();
		const userAttempts =
			input.userId !== null && input.userId !== undefined && input.userId !== ''
				? await this.bumpCounter(userKey(input.userId, 'attempts'))
				: 0;
		const userRejected =
			input.rejected && input.userId !== null && input.userId !== undefined && input.userId !== ''
				? await this.bumpCounter(userKey(input.userId, 'rejected'))
				: input.userId !== null && input.userId !== undefined && input.userId !== ''
					? await this.peekCounter(userKey(input.userId, 'rejected'))
					: 0;
		let userDistinctPrefix = 0;
		let userDistinctCountries = 0;
		if (input.userId !== null && input.userId !== undefined && input.userId !== '') {
			userDistinctPrefix = await this.bumpSet(userKey(input.userId, 'prefix6'), hmac(prefix6(input.phone), key));
			if (input.countryCode) {
				userDistinctCountries = await this.bumpSet(userKey(input.userId, 'countries'), input.countryCode);
			}
		}
		let ipDistinctPhones = 0;
		let ipDistinctUsers = 0;
		let subnetDistinctPhones = 0;
		if (input.clientIp) {
			ipDistinctPhones = await this.bumpSet(ipKey(input.clientIp, 'phones'), hmac(input.phone, key));
			if (input.userId !== null && input.userId !== undefined && input.userId !== '') {
				ipDistinctUsers = await this.bumpSet(ipKey(input.clientIp, 'users'), hmac(String(input.userId), key));
			}
			const subnet = getSubnet(input.clientIp) ?? input.clientIp;
			subnetDistinctPhones = await this.bumpSet(subnetKey(subnet, 'phones'), hmac(input.phone, key));
		}
		const counters = {
			userAttempts,
			userRejected,
			userDistinctPrefix,
			userDistinctCountries,
			ipDistinctPhones,
			ipDistinctUsers,
			subnetDistinctPhones,
		};
		const tripped = this.tripsAt(counters);
		if (tripped !== null) {
			await this.applyHardBlock(input, tripped);
		}
		return {
			decision: tripped !== null ? 'hard_block' : 'allow',
			reason: tripped,
			counters,
		};
	}

	private tripsAt(counters: PhoneAttemptRiskEvaluation['counters']): PhoneAttemptRiskReason | null {
		const rejectedRatio = counters.userAttempts > 0 ? counters.userRejected / counters.userAttempts : 0;
		if (counters.userAttempts >= this.thresholds.userDistinctPhonesHardBlock && rejectedRatio >= 0.5) {
			return 'user_rejected_burst';
		}
		if (
			counters.ipDistinctPhones >= this.thresholds.ipDistinctPhonesHardBlock &&
			counters.ipDistinctUsers >= this.thresholds.ipDistinctUsersHardBlock
		) {
			return 'ip_user_pool';
		}
		if (counters.subnetDistinctPhones >= this.thresholds.subnetDistinctPhonesHardBlock) {
			return 'subnet_distinct_phones';
		}
		return null;
	}

	private async applyHardBlock(input: PhoneAttemptRecordInput, reason: PhoneAttemptRiskReason): Promise<void> {
		if (
			reason === 'user_rejected_burst' &&
			input.userId !== null &&
			input.userId !== undefined &&
			input.userId !== ''
		) {
			await this.kv.setex(userKey(input.userId, 'hard_block'), HARD_BLOCK_TTL_SECONDS, '1');
		} else if (reason === 'ip_user_pool' && input.clientIp) {
			await this.kv.setex(ipKey(input.clientIp, 'hard_block'), IP_BLOCK_TTL_SECONDS, '1');
		} else if (reason === 'subnet_distinct_phones' && input.clientIp) {
			const subnet = getSubnet(input.clientIp) ?? input.clientIp;
			await this.kv.setex(subnetKey(subnet, 'hard_block'), IP_BLOCK_TTL_SECONDS, '1');
		}
		Logger.warn(
			{
				reason,
				userId: input.userId ? String(input.userId) : null,
				clientIp: input.clientIp ?? null,
			},
			'phone_attempt_risk.hard_block applied',
		);
		for (const hook of this.hardBlockHooks) {
			try {
				hook({
					userId: input.userId ? String(input.userId) : null,
					clientIp: input.clientIp ?? null,
					reason,
				});
			} catch (error) {
				Logger.warn({error, reason}, 'phone_attempt_risk.hard_block hook failed');
			}
		}
	}

	private async peekCounter(key: string): Promise<number> {
		const v = await this.kv.get(key);
		return v ? Number(v) : 0;
	}

	private async bumpCounter(key: string): Promise<number> {
		const value = await this.kv.incr(key);
		if (value === 1) {
			await this.kv.expire(key, WINDOW_SECONDS);
		}
		return value;
	}

	private async bumpSet(key: string, member: string): Promise<number> {
		await this.kv.sadd(key, member);
		await this.kv.expire(key, WINDOW_SECONDS);
		return await this.kv.scard(key);
	}

	private async getCurrentKey(): Promise<HmacKey> {
		if (this.keyState && !shouldRotate(this.keyState.primary)) {
			return this.keyState.primary;
		}
		const state = await this.cache.get<HmacKeyState>(HMAC_KEY_CACHE_KEY);
		if (state && !shouldRotate(state.primary)) {
			this.keyState = state;
			return state.primary;
		}
		const next: HmacKeyState = {primary: newKey(), secondary: state?.primary ?? null};
		await this.cache.set(HMAC_KEY_CACHE_KEY, next, HMAC_KEY_TTL_SECONDS);
		this.keyState = next;
		return next.primary;
	}
}
