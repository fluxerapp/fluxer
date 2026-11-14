// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {mapUserToAdminResponse} from '@app/api/admin/models/UserTypes';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import type {AdminUserSecurityService} from '@app/api/admin/services/AdminUserSecurityService';
import {sendVerificationEmail} from '@app/api/auth/AuthEmail';
import {resolveRegistrationUsername} from '@app/api/auth/AuthRegistration';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {EMPTY_USER_ROW, type UserRow} from '@app/api/database/types/UserTypes';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import type {DiscriminatorService} from '@app/api/infrastructure/DiscriminatorService';
import {withAccountIdentitySetupLock} from '@app/api/instance/AccountIdentitySetupLock';
import {instanceCollectsDateOfBirth} from '@app/api/instance/DateOfBirthCollectionCache';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {UserSettings} from '@app/api/models/UserSettings';
import {reserveUsername, type UsernameReservation} from '@app/api/user/UniqueUsernames';
import {DOB_DEFERRED_TRAIT} from '@app/api/user/UserTraits';
import {EmailDeliveryStates} from '@fluxer/constants/src/AccountConfigConstants';
import {AccountIdentityModes, TagStyles} from '@fluxer/constants/src/AccountIdentityConstants';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {ProfileFieldPrivacyFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {UsernameSignInOnlyError} from '@fluxer/errors/src/domains/auth/UsernameSignInOnlyError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {AdminUserCreateRequest, AdminUserCreateResponse} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';

interface AdminUserCreationServiceDeps {
	apiContext: ApiContext;
	auditService: AdminAuditService;
	discriminatorService: DiscriminatorService;
	securityService: AdminUserSecurityService;
}

export class AdminUserCreationService {
	constructor(private readonly deps: AdminUserCreationServiceDeps) {}

	async createUser(
		data: AdminUserCreateRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	): Promise<AdminUserCreateResponse> {
		const {config, cache} = this.deps.apiContext.services;
		const repository = getInstanceConfigRepository();
		const user =
			!config.instance.selfHosted || (await repository.isAccountIdentityLocked())
				? await this.createAccount(data)
				: await withAccountIdentitySetupLock(cache, () => this.createAccount(data));
		await this.deps.auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(user.id),
			action: 'create_user',
			auditLogReason,
			metadata: new Map([
				['username', user.username],
				['admin', String(data.admin)],
			]),
		});
		const passwordReset = await this.deps.securityService.createPasswordResetLink(
			{user_id: user.id},
			adminUserId,
			auditLogReason,
			acls,
		);
		return {
			user: await mapUserToAdminResponse(user, cache, acls),
			password_reset: passwordReset,
		};
	}

	private async createAccount(data: AdminUserCreateRequest): Promise<User> {
		const {users, snowflake, cache} = this.deps.apiContext.services;
		const repository = getInstanceConfigRepository();
		const accountIdentity = await repository.getAccountIdentity();
		const usernameMode = accountIdentity.mode === AccountIdentityModes.USERNAME;
		const uniqueUsernames = accountIdentity.tagStyle === TagStyles.NONE;
		const emailEnabled = (await repository.getEmailDeliveryState()) === EmailDeliveryStates.ON;
		const email = data.email ?? null;
		if (usernameMode && email !== null) {
			throw new UsernameSignInOnlyError();
		}
		if (!usernameMode && email === null) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.EMAIL_IS_REQUIRED);
		}
		if (email !== null && (await users.findByEmail(email))) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.EMAIL_ALREADY_IN_USE);
		}
		const {username, discriminator} = await resolveRegistrationUsername({
			users,
			discriminatorService: this.deps.discriminatorService,
			uniqueUsernames,
			requestedUsername: data.username,
			explicitlyRequested: true,
			globalName: undefined,
		});
		const userId = createUserID(await snowflake.generate());
		const now = new Date();
		const traits = new Set<string>();
		if (instanceCollectsDateOfBirth()) traits.add(DOB_DEFERRED_TRAIT);
		const userRow: UserRow = {
			...EMPTY_USER_ROW,
			user_id: userId,
			username,
			discriminator,
			global_name: null,
			bot: false,
			system: false,
			email,
			email_verified: email === null || !emailEnabled,
			email_bounced: false,
			password_hash: null,
			password_last_changed_at: null,
			authenticator_types: new Set(),
			timezone_privacy_flags: ProfileFieldPrivacyFlags.EVERYONE,
			flags: 0n,
			terms_agreed_at: null,
			privacy_agreed_at: null,
			last_active_at: now,
			acls: data.admin ? new Set([AdminACLs.WILDCARD]) : null,
			traits: traits.size > 0 ? traits : null,
			version: 1,
		};
		let reservation: UsernameReservation | null = null;
		let user: User;
		try {
			if (uniqueUsernames) {
				reservation = await reserveUsername({users, cache}, username);
			}
			user = await users.create(userRow);
		} finally {
			await reservation?.release();
		}
		await users.upsertSettings(UserSettings.getDefaultUserSettings({userId, locale: 'en-US', isAdult: true}));
		await emitActivity(
			'registration',
			user.id.toString(),
			{
				user_id: user.id.toString(),
				method: 'other',
				source: 'app',
				email,
				username,
				username_user_chosen: true,
				global_name: null,
				locale: user.locale,
				timezone: null,
				invite_code: null,
				flags: user.flags.toString(),
				date_of_birth: user.dateOfBirth,
			},
			null,
			user.id.toString(),
		);
		if (email !== null && emailEnabled) {
			await sendVerificationEmail(this.deps.apiContext, {user, email});
		}
		return user;
	}
}
