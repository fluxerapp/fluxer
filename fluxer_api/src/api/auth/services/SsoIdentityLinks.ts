// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {SsoIdentityRepository} from '@app/api/auth/services/SsoIdentityRepository';
import type {UserSsoIdentityRow} from '@app/api/database/types/AuthTypes';
import type {InstanceSsoConfig} from '@app/api/instance/InstanceConfigRepository';
import type {User} from '@app/api/models/User';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {UserTraits} from '@app/api/user/UserTraits';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';

const SSO_IDENTITY_TRAIT_PREFIXES = ['sso:', 'sso_provider:', 'sso_identity:'];

export function ssoProviderIdOf(config: Pick<InstanceSsoConfig, 'issuer' | 'authorizationUrl'>): string {
	return config.issuer || config.authorizationUrl || 'sso';
}

export function ssoIdentityTrait(providerId: string, sub: string): string {
	const digest = createHash('sha256').update(providerId).update('\0').update(sub).digest('hex');
	return `sso_identity:${digest}`;
}

function ssoProviderTrait(providerId: string): string {
	return `sso_provider:${createHash('sha256').update(providerId).digest('hex').slice(0, 32)}`;
}

export function isLinkedToSsoProvider(user: User, providerId: string): boolean {
	return user.traits.has(`sso:${providerId}`) || user.traits.has(ssoProviderTrait(providerId));
}

export function ssoIdentityTraits(providerId: string, sub: string): Array<string> {
	return [UserTraits.SSO, `sso:${providerId}`, ssoProviderTrait(providerId), ssoIdentityTrait(providerId, sub)];
}

function withoutSsoIdentityTraits(traits: Iterable<string>): Set<string> {
	return new Set(
		Array.from(traits).filter(
			(trait) => trait !== UserTraits.SSO && !SSO_IDENTITY_TRAIT_PREFIXES.some((prefix) => trait.startsWith(prefix)),
		),
	);
}

function isLiveAccount(user: User | null): user is User {
	return user !== null && (user.flags & UserFlags.DELETED) === 0n;
}

export class SsoIdentityLinkService {
	private readonly identities = new SsoIdentityRepository();

	constructor(private readonly users: UserRepository) {}

	async unlink(user: User, providerId: string): Promise<Array<UserSsoIdentityRow>> {
		await this.releaseAll(user, providerId);
		await this.users.patchUpsert(user.id, {traits: withoutSsoIdentityTraits(user.traits)}, user.toRow());
		return this.identities.listForUser(user.id);
	}

	async link(user: User, providerId: string, subject: string): Promise<Array<UserSsoIdentityRow>> {
		const holder = await this.identities.findIdentity(providerId, subject);
		if (holder && holder.user_id !== user.id) {
			if (isLiveAccount(await this.users.findUnique(holder.user_id))) {
				throw InputValidationError.fromCode('subject', ValidationErrorCodes.SSO_IDENTITY_LINKED_TO_ANOTHER_ACCOUNT);
			}
			await this.identities.releaseIdentity(providerId, subject);
		}
		await this.releaseAll(user, providerId);
		await this.identities.tryClaimIdentity({providerId, subject, userId: user.id, claimedAt: new Date()});
		const traits = withoutSsoIdentityTraits(user.traits);
		for (const trait of ssoIdentityTraits(providerId, subject)) {
			traits.add(trait);
		}
		await this.users.patchUpsert(user.id, {traits}, user.toRow());
		return this.identities.listForUser(user.id);
	}

	async migrate(fromProviderId: string, toProviderId: string): Promise<{moved: number; skipped: number}> {
		let moved = 0;
		let skipped = 0;
		for (const identity of await this.identities.listForProvider(fromProviderId)) {
			const user = await this.users.findUnique(identity.user_id);
			if (!user || !(await this.claimFor(toProviderId, identity))) {
				skipped += 1;
				continue;
			}
			await this.identities.releaseIdentity(fromProviderId, identity.subject);
			const traits = new Set(user.traits);
			for (const trait of ssoIdentityTraits(fromProviderId, identity.subject)) {
				traits.delete(trait);
			}
			for (const trait of ssoIdentityTraits(toProviderId, identity.subject)) {
				traits.add(trait);
			}
			await this.users.patchUpsert(user.id, {traits}, user.toRow());
			moved += 1;
		}
		return {moved, skipped};
	}

	private async releaseAll(user: User, providerId: string): Promise<void> {
		if ((await this.identities.releaseIdentitiesForUser(user.id)) > 0) return;
		for (const identity of await this.identities.listForProvider(providerId)) {
			if (identity.user_id === user.id) {
				await this.identities.releaseIdentity(identity.provider_id, identity.subject);
			}
		}
	}

	private async claimFor(providerId: string, identity: UserSsoIdentityRow): Promise<boolean> {
		const claimed = await this.identities.tryClaimIdentity({
			providerId,
			subject: identity.subject,
			userId: identity.user_id,
			claimedAt: identity.claimed_at,
		});
		return claimed || (await this.identities.findUserId(providerId, identity.subject)) === identity.user_id;
	}
}
