// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import {lookupGeoip} from '@app/api/utils/IpUtils';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {parseIpAddress} from '@fluxer/ip_utils/src/IpAddress';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

const DIRECT_CONTACT_SPAM_TARGET_KEY_PREFIX = 'direct-contact-spam:distinct-targets:';

interface DirectMessageSpamMitigationDecision {
	shouldSuppressRecipientDelivery: boolean;
	flaggedNow: boolean;
	distinctRecipientCount: number | null;
}

type DirectMessageSpamCountryResolver = (clientIp: string) => Promise<string | null>;

interface DirectContactSpamPolicy {
	enabled: boolean;
	countryCodes: Array<string>;
	distinctTargetThreshold: number;
	targetWindowMs: number;
	action: 'flag_spammer' | 'suppress_delivery';
}

interface DirectMessageSpamUserRepository {
	patchUpsert(userId: UserID, patchData: Partial<UserRow>, oldData?: UserRow | null): Promise<User>;
}

export function createDirectMessageSpamMitigationService(
	apiContext: ApiContext,
	userRepository: DirectMessageSpamUserRepository,
): DirectMessageSpamMitigationService {
	return new DirectMessageSpamMitigationService({
		userRepository,
		kv: apiContext.services.kv,
		clientIp: apiContext.request.clientIp,
		policy: Config.abusePolicy.directContactSpam,
	});
}

interface DirectMessageSpamMitigationServiceDeps {
	userRepository: DirectMessageSpamUserRepository;
	kv: IKVProvider;
	clientIp: string | null;
	countryResolver?: DirectMessageSpamCountryResolver;
	policy?: DirectContactSpamPolicy;
	nowMs?: () => number;
}

function isDirectMessageSpammer(user: User): boolean {
	if (user.isBot) {
		return false;
	}
	return (user.flags & UserFlags.SPAMMER) === UserFlags.SPAMMER;
}

function countryMatchesDirectContactSpamPolicy(
	policy: DirectContactSpamPolicy,
	countryCode: string | null | undefined,
): boolean {
	if (!policy.enabled) {
		return false;
	}
	if (countryCode == null) {
		return false;
	}
	const configuredCountries = new Set(policy.countryCodes.map((country) => country.toUpperCase()));
	return configuredCountries.has(countryCode.toUpperCase());
}

async function resolveGeoipCountryCode(clientIp: string): Promise<string | null> {
	const geoip = await lookupGeoip(clientIp);
	return geoip.countryCode?.toUpperCase() ?? null;
}

function normalizeClientIp(clientIp: string | null): string | null {
	if (!clientIp) {
		return null;
	}
	const firstHop = clientIp.split(',')[0]?.trim();
	if (!firstHop) {
		return null;
	}
	return parseIpAddress(firstHop)?.normalized ?? null;
}

export class DirectMessageSpamMitigationService {
	private readonly countryResolver: DirectMessageSpamCountryResolver;
	private readonly nowMs: () => number;
	private readonly policy: DirectContactSpamPolicy;

	constructor(private readonly deps: DirectMessageSpamMitigationServiceDeps) {
		this.countryResolver = deps.countryResolver ?? resolveGeoipCountryCode;
		this.nowMs = deps.nowMs ?? Date.now;
		this.policy = deps.policy ?? {
			enabled: false,
			countryCodes: [],
			distinctTargetThreshold: 25,
			targetWindowMs: 2 * 60 * 60 * 1000,
			action: 'flag_spammer',
		};
	}

	shouldSuppressDirectMessageDelivery(user: User): boolean {
		return isDirectMessageSpammer(user);
	}

	async recordOneToOneDmSend(params: {
		sender: User;
		recipientId: UserID;
	}): Promise<DirectMessageSpamMitigationDecision> {
		return await this.recordDirectContactAttempt({
			actor: params.sender,
			targetId: params.recipientId,
			action: 'dm_send',
		});
	}

	async recordFriendRequestSend(params: {
		requester: User;
		targetId: UserID;
	}): Promise<DirectMessageSpamMitigationDecision> {
		return await this.recordDirectContactAttempt({
			actor: params.requester,
			targetId: params.targetId,
			action: 'friend_request_send',
		});
	}

	private async recordDirectContactAttempt(params: {
		actor: User;
		targetId: UserID;
		action: 'dm_send' | 'friend_request_send';
	}): Promise<DirectMessageSpamMitigationDecision> {
		const {actor, targetId, action} = params;
		if (actor.isBot) {
			return {
				shouldSuppressRecipientDelivery: false,
				flaggedNow: false,
				distinctRecipientCount: null,
			};
		}
		if (isDirectMessageSpammer(actor)) {
			return {
				shouldSuppressRecipientDelivery: true,
				flaggedNow: false,
				distinctRecipientCount: null,
			};
		}
		try {
			const countryCode = await this.resolveClientCountryCode();
			if (!countryMatchesDirectContactSpamPolicy(this.policy, countryCode)) {
				return {
					shouldSuppressRecipientDelivery: false,
					flaggedNow: false,
					distinctRecipientCount: null,
				};
			}
			const distinctRecipientCount = await this.recordDistinctRecipient({
				senderId: actor.id,
				recipientId: targetId,
				windowMs: Math.max(1000, this.policy.targetWindowMs),
			});
			if (distinctRecipientCount < Math.max(1, this.policy.distinctTargetThreshold)) {
				return {
					shouldSuppressRecipientDelivery: false,
					flaggedNow: false,
					distinctRecipientCount,
				};
			}
			if (this.policy.action === 'flag_spammer') {
				await this.flagSpammer(actor);
			}
			return {
				shouldSuppressRecipientDelivery: true,
				flaggedNow: this.policy.action === 'flag_spammer',
				distinctRecipientCount,
			};
		} catch (error) {
			Logger.warn(
				{
					error,
					action,
					actorId: actor.id.toString(),
					targetId: targetId.toString(),
				},
				'DM spam mitigation check failed',
			);
			return {
				shouldSuppressRecipientDelivery: false,
				flaggedNow: false,
				distinctRecipientCount: null,
			};
		}
	}

	private async resolveClientCountryCode(): Promise<string | null> {
		const clientIp = normalizeClientIp(this.deps.clientIp);
		if (!clientIp) {
			return null;
		}
		const countryCode = await this.countryResolver(clientIp);
		return countryCode?.toUpperCase() ?? null;
	}

	private async recordDistinctRecipient(params: {
		senderId: UserID;
		recipientId: UserID;
		windowMs: number;
	}): Promise<number> {
		const now = this.nowMs();
		const key = `${DIRECT_CONTACT_SPAM_TARGET_KEY_PREFIX}${params.senderId.toString()}`;
		await this.deps.kv.zadd(key, now, params.recipientId.toString());
		await this.deps.kv.expire(key, Math.ceil(params.windowMs / 1000));
		const expiredRecipientIds = await this.deps.kv.zrangebyscore(key, '-inf', now - params.windowMs);
		if (expiredRecipientIds.length > 0) {
			await this.deps.kv.zrem(key, ...expiredRecipientIds);
		}
		return await this.deps.kv.zcard(key);
	}

	private async flagSpammer(user: User): Promise<void> {
		if (isDirectMessageSpammer(user)) {
			return;
		}
		await this.deps.userRepository.patchUpsert(user.id, {
			flags: user.flags | UserFlags.SPAMMER,
		});
	}
}
