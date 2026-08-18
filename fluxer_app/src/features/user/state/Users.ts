// SPDX-License-Identifier: AGPL-3.0-or-later

import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import Authentication from '@app/features/auth/state/Authentication';
import {User} from '@app/features/user/models/User';
import type {UserPrivate, User as WireUser} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {action, makeAutoObservable, reaction, runInAction} from 'mobx';

const CURRENT_USER_PRIVATE_WIRE_KEYS = [
	'is_staff',
	'email',
	'email_bounced',
	'mfa_enabled',
	'phone',
	'authenticator_types',
	'verified',
	'premium_type',
	'premium_since',
	'premium_until',
	'premium_will_cancel',
	'premium_billing_cycle',
	'premium_lifetime_sequence',
	'premium_grace_ends_at',
	'premium_discriminator',
	'premium_badge_hidden',
	'premium_badge_masked',
	'premium_badge_timestamp_hidden',
	'premium_badge_sequence_hidden',
	'premium_purchase_disabled',
	'premium_enabled_override',
	'premium_perks_disabled',
	'password_last_changed_at',
	'last_voice_activity_sharing_change_at',
	'required_actions',
	'nsfw_allowed',
	'pending_bulk_message_deletion',
	'has_dismissed_premium_onboarding',
	'has_ever_purchased',
	'has_unread_gift_inventory',
	'unread_gift_inventory_count',
	'age_verified_adult',
	'terms_agreed_at',
	'privacy_agreed_at',
	'traits',
	'timezone',
	'timezone_privacy_flags',
] as const;

function isPublicOnlyCurrentUserPayload(user: WireUser): boolean {
	if (typeof user.mention_flags === 'number') {
		return false;
	}
	return !CURRENT_USER_PRIVATE_WIRE_KEYS.some((key) => key in user);
}

class Users {
	users: Record<string, User> = {};
	userCount = 0;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get currentUser(): User | null {
		const currentUserId = Authentication.userId;
		if (!currentUserId) {
			return null;
		}
		return this.users[currentUserId] ?? null;
	}

	get currentUserId(): string | null {
		return Authentication.userId;
	}

	get usersList(): ReadonlyArray<User> {
		return Object.values(this.users);
	}

	getUser(userId: string): User | undefined {
		return this.users[userId];
	}

	getCurrentUser(): User | undefined {
		return this.currentUser ?? undefined;
	}

	getUserByTag(tag: string): User | undefined {
		return this.usersList.find((user) => user.tag === tag);
	}

	getUsers(): ReadonlyArray<User> {
		return this.usersList;
	}

	@action
	handleConnectionOpen(currentUser: UserPrivate): void {
		const userRecord = new User(currentUser);
		this.users = {
			[currentUser.id]: userRecord,
		};
		this.userCount = 1;
		if (!userRecord.isClaimed()) {
			setTimeout(async () => {
				openClaimAccountModal();
			}, 1000);
		}
	}

	@action
	handleUserUpdate(
		user: WireUser,
		options?: {
			clearMissingOptionalFields?: boolean;
		},
	): void {
		const existingUser = this.users[user.id];
		if (
			user.id === this.currentUserId &&
			existingUser &&
			options?.clearMissingOptionalFields !== true &&
			isPublicOnlyCurrentUserPayload(user)
		) {
			return;
		}
		this.storeUser(existingUser, existingUser ? existingUser.withUpdates(user, options) : new User(user));
	}

	private storeUser(existingUser: User | undefined, nextUser: User): void {
		if (existingUser) {
			if (existingUser.equals(nextUser)) {
				return;
			}
		} else {
			this.userCount += 1;
		}
		this.users[nextUser.id] = nextUser;
	}

	cacheUsers(
		users: Array<
			WireUser & {
				globalName?: never;
			}
		>,
	): void {
		runInAction(() => {
			for (const user of users) {
				const existingUser = this.users[user.id];
				if (user.id === this.currentUserId && existingUser && isPublicOnlyCurrentUserPayload(user)) {
					continue;
				}
				this.storeUser(existingUser, existingUser ? existingUser.withUpdates(user) : new User(user));
			}
		});
	}

	subscribe(callback: () => void): () => void {
		return reaction(
			() => this.userCount,
			() => callback(),
			{fireImmediately: true},
		);
	}
}

export default new Users();
