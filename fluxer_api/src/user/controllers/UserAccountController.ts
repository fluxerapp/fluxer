/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {Context} from 'hono';
import type {HonoApp, HonoEnv} from '~/App';
import {requireSudoMode, type SudoVerificationResult} from '~/auth/services/SudoVerificationService';
import {createChannelID, createGuildID, createUserID} from '~/BrandedTypes';
import {UserFlags} from '~/Constants';
import {mapMessageToResponse} from '~/channel/ChannelModel';
import {
	AccountSuspiciousActivityError,
	InputValidationError,
	MissingAccessError,
	UnauthorizedError,
	UnknownUserError,
} from '~/Errors';
import {Logger} from '~/Logger';
import type {User} from '~/Models';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {SudoModeMiddleware} from '~/middleware/SudoModeMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {
	createStringType,
	DiscriminatorType,
	Int64Type,
	QueryBooleanType,
	SudoVerificationSchema,
	URLType,
	UsernameType,
	z,
} from '~/Schema';
import {getCachedUserPartialResponse, mapUserToPartialResponseWithCache} from '~/user/UserCacheHelpers';
import {createPremiumClearPatch, shouldStripExpiredPremium} from '~/user/UserHelpers';
import {
	mapGuildMemberToProfileResponse,
	mapUserGuildSettingsToResponse,
	mapUserSettingsToResponse,
	mapUserToOAuthResponse,
	mapUserToPrivateResponse,
	mapUserToProfileResponse,
	UserGuildSettingsUpdateRequest,
	UserSettingsUpdateRequest,
	UserUpdateRequest,
} from '~/user/UserModel';
import {Validator} from '~/Validator';

const EmailTokenType = createStringType(1, 256);

const UserUpdateWithVerificationRequest = UserUpdateRequest.merge(
	z.object({
		email_token: EmailTokenType.optional(),
	}),
)
	.merge(SudoVerificationSchema)
	.superRefine((data, ctx) => {
		if (data.email !== undefined) {
			ctx.addIssue({
				code: z.ZodIssueCode.custom,
				message: 'Email must be changed via email_token',
				path: ['email'],
			});
		}
	});

type UserUpdateWithVerificationRequestData = z.infer<typeof UserUpdateWithVerificationRequest>;
type UserUpdatePayload = Omit<
	UserUpdateWithVerificationRequestData,
	'mfa_method' | 'mfa_code' | 'webauthn_response' | 'webauthn_challenge' | 'email_token'
>;

const requiresSensitiveUserVerification = (
	user: User,
	data: UserUpdateRequest,
	emailTokenProvided: boolean,
): boolean => {
	const isUnclaimed = !user.passwordHash;
	const usernameChanged = data.username !== undefined && data.username !== user.username;
	const discriminatorChanged = data.discriminator !== undefined && data.discriminator !== user.discriminator;
	const emailChanged = data.email !== undefined && data.email !== user.email;
	const newPasswordProvided = data.new_password !== undefined;

	if (isUnclaimed) {
		return usernameChanged || discriminatorChanged;
	}

	return usernameChanged || discriminatorChanged || emailTokenProvided || emailChanged || newPasswordProvided;
};

const EmailChangeTicketSchema = z.object({
	ticket: createStringType(),
});

const EmailChangeCodeSchema = EmailChangeTicketSchema.extend({
	code: createStringType(),
});

const EmailChangeRequestNewSchema = EmailChangeTicketSchema.extend({
	new_email: createStringType(),
	original_proof: createStringType(),
});

const EmailChangeVerifyNewSchema = EmailChangeCodeSchema.extend({
	original_proof: createStringType(),
});

export const UserAccountController = (app: HonoApp) => {
	const enforceUserAccess = (user: User): void => {
		if (user.suspiciousActivityFlags !== null && user.suspiciousActivityFlags !== 0) {
			throw new AccountSuspiciousActivityError(user.suspiciousActivityFlags);
		}
		if ((user.flags & UserFlags.PENDING_MANUAL_VERIFICATION) !== 0n) {
			throw new MissingAccessError();
		}
	};

	const handlePreloadMessages = async (ctx: Context<HonoEnv>, channels: ReadonlyArray<bigint>) => {
		const channelIds = channels.map(createChannelID);

		const messages = await ctx.get('userService').preloadDMMessages({
			userId: ctx.get('user').id,
			channelIds,
		});

		const mappingPromises = Object.entries(messages).map(async ([channelId, message]) => {
			const mappedMessage = message
				? await mapMessageToResponse({
						message,
						userCacheService: ctx.get('userCacheService'),
						requestCache: ctx.get('requestCache'),
						mediaService: ctx.get('mediaService'),
						currentUserId: ctx.get('user').id,
					})
				: null;
			return [channelId, mappedMessage] as const;
		});

		const mappedEntries = await Promise.all(mappingPromises);
		const mappedMessages = Object.fromEntries(mappedEntries);

		return ctx.json(mappedMessages);
	};

	app.get('/users/@me', RateLimitMiddleware(RateLimitConfigs.USER_SETTINGS_GET), async (ctx) => {
		const tokenType = ctx.get('authTokenType');

		if (tokenType === 'bearer') {
			const scopes = ctx.get('oauthBearerScopes');
			const bearerUser = ctx.get('user');
			if (!scopes || !bearerUser) {
				throw new UnauthorizedError();
			}
			enforceUserAccess(bearerUser);
			const includeEmail = scopes.has('email');
			return ctx.json(mapUserToOAuthResponse(bearerUser, {includeEmail}));
		}

		const maybeUser = ctx.get('user');
		if (maybeUser) {
			enforceUserAccess(maybeUser);
			return ctx.json(mapUserToPrivateResponse(maybeUser));
		}

		throw new UnauthorizedError();
	});

	app.patch(
		'/users/@me',
		RateLimitMiddleware(RateLimitConfigs.USER_UPDATE_SELF),
		LoginRequired,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', UserUpdateWithVerificationRequest),
		async (ctx) => {
			const user = ctx.get('user');
			const oldEmail = user.email;
			const rawBody: UserUpdateWithVerificationRequestData = ctx.req.valid('json');
			const {
				mfa_method: _mfaMethod,
				mfa_code: _mfaCode,
				webauthn_response: _webauthnResponse,
				webauthn_challenge: _webauthnChallenge,
				email_token: emailToken,
				...userUpdateDataRest
			} = rawBody;
			let userUpdateData: UserUpdatePayload = userUpdateDataRest;
			if (userUpdateData.email !== undefined) {
				throw InputValidationError.create('email', 'Email must be changed via email_token');
			}
			const emailTokenProvided = emailToken !== undefined;
			const isUnclaimed = !user.passwordHash;
			if (isUnclaimed) {
				const {username: _ignoredUsername, discriminator: _ignoredDiscriminator, ...rest} = userUpdateData;
				userUpdateData = rest;
				const allowed = new Set(['new_password']);
				const disallowedField = Object.keys(userUpdateData).find((key) => !allowed.has(key));
				if (disallowedField) {
					throw InputValidationError.create(
						disallowedField,
						'Unclaimed accounts can only set a new email via email_token and a new password',
					);
				}
			}
			let emailFromToken: string | null = null;
			let emailVerifiedViaToken = false;

			const needsVerification = requiresSensitiveUserVerification(user, userUpdateData, emailTokenProvided);
			let sudoResult: SudoVerificationResult | null = null;
			if (needsVerification) {
				sudoResult = await requireSudoMode(ctx, user, rawBody, ctx.get('authService'), ctx.get('authMfaService'));
			}

			if (emailTokenProvided && emailToken) {
				emailFromToken = await ctx.get('emailChangeService').consumeToken(user.id, emailToken);
				userUpdateData = {...userUpdateData, email: emailFromToken};
				emailVerifiedViaToken = true;
			}

			const updatedUser = await ctx.get('userService').update({
				user,
				oldAuthSession: ctx.get('authSession'),
				data: userUpdateData,
				request: ctx.req.raw,
				sudoContext: sudoResult ?? undefined,
				emailVerifiedViaToken,
			});

			if (
				emailFromToken &&
				oldEmail &&
				updatedUser.email &&
				oldEmail.toLowerCase() !== updatedUser.email.toLowerCase()
			) {
				try {
					await ctx.get('authService').issueEmailRevertToken(updatedUser, oldEmail, updatedUser.email);
				} catch (error) {
					Logger.warn({error, userId: updatedUser.id}, 'Failed to issue email revert token');
				}
			}
			return ctx.json(mapUserToPrivateResponse(updatedUser));
		},
	);

	app.post(
		'/users/@me/email-change/start',
		RateLimitMiddleware(RateLimitConfigs.USER_EMAIL_CHANGE_START),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', z.object({}).optional()),
		async (ctx) => {
			const user = ctx.get('user');
			const result = await ctx.get('emailChangeService').start(user);
			return ctx.json(result);
		},
	);

	app.post(
		'/users/@me/email-change/resend-original',
		RateLimitMiddleware(RateLimitConfigs.USER_EMAIL_CHANGE_RESEND_ORIGINAL),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', EmailChangeTicketSchema),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			await ctx.get('emailChangeService').resendOriginal(user, body.ticket);
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/users/@me/email-change/verify-original',
		RateLimitMiddleware(RateLimitConfigs.USER_EMAIL_CHANGE_VERIFY_ORIGINAL),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', EmailChangeCodeSchema),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			const result = await ctx.get('emailChangeService').verifyOriginal(user, body.ticket, body.code);
			return ctx.json(result);
		},
	);

	app.post(
		'/users/@me/email-change/request-new',
		RateLimitMiddleware(RateLimitConfigs.USER_EMAIL_CHANGE_REQUEST_NEW),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', EmailChangeRequestNewSchema),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			const result = await ctx
				.get('emailChangeService')
				.requestNewEmail(user, body.ticket, body.new_email, body.original_proof);
			return ctx.json(result);
		},
	);

	app.post(
		'/users/@me/email-change/resend-new',
		RateLimitMiddleware(RateLimitConfigs.USER_EMAIL_CHANGE_RESEND_NEW),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', EmailChangeTicketSchema),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			await ctx.get('emailChangeService').resendNew(user, body.ticket);
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/users/@me/email-change/verify-new',
		RateLimitMiddleware(RateLimitConfigs.USER_EMAIL_CHANGE_VERIFY_NEW),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', EmailChangeVerifyNewSchema),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			const emailToken = await ctx
				.get('emailChangeService')
				.verifyNew(user, body.ticket, body.code, body.original_proof);
			return ctx.json({email_token: emailToken});
		},
	);

	app.get(
		'/users/check-tag',
		RateLimitMiddleware(RateLimitConfigs.USER_CHECK_TAG),
		LoginRequired,
		Validator('query', z.object({username: UsernameType, discriminator: DiscriminatorType})),
		async (ctx) => {
			const {username, discriminator} = ctx.req.valid('query');
			const currentUser = ctx.get('user');
			if (
				username.toLowerCase() === currentUser.username.toLowerCase() &&
				discriminator === currentUser.discriminator
			) {
				return ctx.json({taken: false});
			}
			const taken = await ctx.get('userService').checkUsernameDiscriminatorAvailability({username, discriminator});
			return ctx.json({taken});
		},
	);

	app.get(
		'/users/:user_id',
		RateLimitMiddleware(RateLimitConfigs.USER_GET),
		LoginRequired,
		Validator('param', z.object({user_id: Int64Type})),
		async (ctx) => {
			const userResponse = await getCachedUserPartialResponse({
				userId: createUserID(ctx.req.valid('param').user_id),
				userCacheService: ctx.get('userCacheService'),
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(userResponse);
		},
	);

	app.get(
		'/users/:target_id/profile',
		RateLimitMiddleware(RateLimitConfigs.USER_GET_PROFILE),
		LoginRequired,
		Validator('param', z.object({target_id: Int64Type})),
		Validator(
			'query',
			z.object({
				guild_id: Int64Type.optional(),
				with_mutual_friends: QueryBooleanType,
				with_mutual_guilds: QueryBooleanType,
			}),
		),
		async (ctx) => {
			const {target_id} = ctx.req.valid('param');
			const {guild_id, with_mutual_friends, with_mutual_guilds} = ctx.req.valid('query');
			const currentUserId = ctx.get('user').id;
			const targetUserId = createUserID(target_id);
			const guildId = guild_id ? createGuildID(guild_id) : undefined;

			const profile = await ctx.get('userService').getUserProfile({
				userId: currentUserId,
				targetId: targetUserId,
				guildId,
				withMutualFriends: with_mutual_friends,
				withMutualGuilds: with_mutual_guilds,
				requestCache: ctx.get('requestCache'),
			});

			let profileUser = profile.user;
			let premiumType = profile.premiumType;
			let premiumSince = profile.premiumSince;
			let premiumLifetimeSequence = profile.premiumLifetimeSequence;

			if (shouldStripExpiredPremium(profileUser)) {
				try {
					const sanitizedUser = await ctx
						.get('userRepository')
						.patchUpsert(profileUser.id, createPremiumClearPatch(), profileUser.toRow());
					if (sanitizedUser) {
						profileUser = sanitizedUser;
						profile.user = sanitizedUser;
						premiumType = undefined;
						premiumSince = undefined;
						premiumLifetimeSequence = undefined;
					}
				} catch (error) {
					Logger.warn(
						{userId: profileUser.id.toString(), error},
						'Failed to sanitize expired premium fields before returning profile',
					);
				}
			}

			const userProfile = mapUserToProfileResponse(profileUser);
			const guildMemberProfile = mapGuildMemberToProfileResponse(profile.guildMemberDomain ?? null);

			const mutualFriends = profile.mutualFriends
				? await Promise.all(
						profile.mutualFriends.map((user) =>
							mapUserToPartialResponseWithCache({
								user,
								userCacheService: ctx.get('userCacheService'),
								requestCache: ctx.get('requestCache'),
							}),
						),
					)
				: undefined;

			return ctx.json({
				user: await mapUserToPartialResponseWithCache({
					user: profileUser,
					userCacheService: ctx.get('userCacheService'),
					requestCache: ctx.get('requestCache'),
				}),
				user_profile: userProfile,
				guild_member: profile.guildMember ?? undefined,
				guild_member_profile: guildMemberProfile ?? undefined,
				premium_type: premiumType,
				premium_since: premiumSince?.toISOString(),
				premium_lifetime_sequence: premiumLifetimeSequence,
				mutual_friends: mutualFriends,
				mutual_guilds: profile.mutualGuilds,
			});
		},
	);

	app.get(
		'/users/@me/settings',
		RateLimitMiddleware(RateLimitConfigs.USER_SETTINGS_GET),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const settings = await ctx.get('userService').findSettings(ctx.get('user').id);
			return ctx.json(mapUserSettingsToResponse({settings}));
		},
	);

	app.patch(
		'/users/@me/settings',
		RateLimitMiddleware(RateLimitConfigs.USER_SETTINGS_UPDATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', UserSettingsUpdateRequest),
		async (ctx) => {
			const updatedSettings = await ctx.get('userService').updateSettings({
				userId: ctx.get('user').id,
				data: ctx.req.valid('json'),
			});
			return ctx.json(
				mapUserSettingsToResponse({
					settings: updatedSettings,
				}),
			);
		},
	);

	app.get(
		'/users/@me/notes',
		RateLimitMiddleware(RateLimitConfigs.USER_NOTES_READ),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const notes = await ctx.get('userService').getUserNotes(ctx.get('user').id);
			return ctx.json(notes);
		},
	);

	app.get(
		'/users/@me/notes/:target_id',
		RateLimitMiddleware(RateLimitConfigs.USER_NOTES_READ),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({target_id: Int64Type})),
		async (ctx) => {
			const note = await ctx.get('userService').getUserNote({
				userId: ctx.get('user').id,
				targetId: createUserID(ctx.req.valid('param').target_id),
			});
			if (!note) {
				throw new UnknownUserError();
			}
			return ctx.json(note);
		},
	);

	app.put(
		'/users/@me/notes/:target_id',
		RateLimitMiddleware(RateLimitConfigs.USER_NOTES_WRITE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({target_id: Int64Type})),
		Validator('json', z.object({note: createStringType(1, 256).nullish()})),
		async (ctx) => {
			const {target_id} = ctx.req.valid('param');
			const {note} = ctx.req.valid('json');
			await ctx.get('userService').setUserNote({
				userId: ctx.get('user').id,
				targetId: createUserID(target_id),
				note: note ?? null,
			});
			return ctx.body(null, 204);
		},
	);

	app.patch(
		'/users/@me/guilds/@me/settings',
		RateLimitMiddleware(RateLimitConfigs.USER_GUILD_SETTINGS_UPDATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', UserGuildSettingsUpdateRequest),
		async (ctx) => {
			const settings = await ctx.get('userService').updateGuildSettings({
				userId: ctx.get('user').id,
				guildId: null,
				data: ctx.req.valid('json'),
			});
			return ctx.json(mapUserGuildSettingsToResponse(settings));
		},
	);

	app.patch(
		'/users/@me/guilds/:guild_id/settings',
		RateLimitMiddleware(RateLimitConfigs.USER_GUILD_SETTINGS_UPDATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({guild_id: Int64Type})),
		Validator('json', UserGuildSettingsUpdateRequest),
		async (ctx) => {
			const {guild_id} = ctx.req.valid('param');
			const settings = await ctx.get('userService').updateGuildSettings({
				userId: ctx.get('user').id,
				guildId: createGuildID(guild_id),
				data: ctx.req.valid('json'),
			});
			return ctx.json(mapUserGuildSettingsToResponse(settings));
		},
	);

	app.post(
		'/users/@me/disable',
		RateLimitMiddleware(RateLimitConfigs.USER_ACCOUNT_DISABLE),
		LoginRequired,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		async (ctx) => {
			const userService = ctx.get('userService');
			const user = ctx.get('user');
			const body = ctx.req.valid('json');

			await requireSudoMode(ctx, user, body, ctx.get('authService'), ctx.get('authMfaService'), {
				issueSudoToken: false,
			});
			await userService.selfDisable(user.id);
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/users/@me/delete',
		RateLimitMiddleware(RateLimitConfigs.USER_ACCOUNT_DELETE),
		LoginRequired,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		async (ctx) => {
			const userService = ctx.get('userService');
			const user = ctx.get('user');
			const body = ctx.req.valid('json');

			await requireSudoMode(ctx, user, body, ctx.get('authService'), ctx.get('authMfaService'));
			await userService.selfDelete(user.id);
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/users/@me/push/subscribe',
		RateLimitMiddleware(RateLimitConfigs.USER_PUSH_SUBSCRIBE),
		LoginRequired,
		DefaultUserOnly,
		Validator(
			'json',
			z.object({
				endpoint: URLType,
				keys: z.object({
					p256dh: createStringType(1, 1024),
					auth: createStringType(1, 1024),
				}),
				user_agent: createStringType(1, 1024).optional(),
			}),
		),
		async (ctx) => {
			const {endpoint, keys, user_agent} = ctx.req.valid('json');
			const subscription = await ctx.get('userService').registerPushSubscription({
				userId: ctx.get('user').id,
				endpoint,
				keys,
				userAgent: user_agent,
			});
			return ctx.json({subscription_id: subscription.subscriptionId});
		},
	);

	app.get(
		'/users/@me/push/subscriptions',
		RateLimitMiddleware(RateLimitConfigs.USER_PUSH_LIST),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const subscriptions = await ctx.get('userService').listPushSubscriptions(ctx.get('user').id);
			return ctx.json({
				subscriptions: subscriptions.map((sub) => ({
					subscription_id: sub.subscriptionId,
					user_agent: sub.userAgent,
				})),
			});
		},
	);

	app.delete(
		'/users/@me/push/subscriptions/:subscription_id',
		RateLimitMiddleware(RateLimitConfigs.USER_PUSH_UNSUBSCRIBE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({subscription_id: createStringType(1, 256)})),
		async (ctx) => {
			const {subscription_id} = ctx.req.valid('param');
			await ctx.get('userService').deletePushSubscription(ctx.get('user').id, subscription_id);
			return ctx.json({success: true});
		},
	);

	app.post(
		'/users/@me/preload-messages',
		RateLimitMiddleware(RateLimitConfigs.USER_PRELOAD_MESSAGES),
		LoginRequired,
		Validator('json', z.object({channels: z.array(Int64Type).max(100)})),
		async (ctx) => handlePreloadMessages(ctx, ctx.req.valid('json').channels),
	);

	app.post(
		'/users/@me/channels/messages/preload',
		RateLimitMiddleware(RateLimitConfigs.USER_PRELOAD_MESSAGES),
		LoginRequired,
		Validator('json', z.object({channels: z.array(Int64Type).max(100)})),
		async (ctx) => handlePreloadMessages(ctx, ctx.req.valid('json').channels),
	);

	app.post(
		'/users/@me/messages/delete',
		RateLimitMiddleware(RateLimitConfigs.USER_BULK_MESSAGE_DELETE),
		LoginRequired,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');

			await requireSudoMode(ctx, user, body, ctx.get('authService'), ctx.get('authMfaService'));
			await ctx.get('userService').requestBulkMessageDeletion({userId: user.id});
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/users/@me/messages/delete',
		RateLimitMiddleware(RateLimitConfigs.USER_BULK_MESSAGE_DELETE),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const user = ctx.get('user');
			await ctx.get('userService').cancelBulkMessageDeletion(user.id);
			return ctx.json({success: true});
		},
	);

	app.post(
		'/users/@me/messages/delete/test',
		RateLimitMiddleware(RateLimitConfigs.USER_BULK_MESSAGE_DELETE),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const user = ctx.get('user');

			if (!(user.flags & UserFlags.STAFF)) {
				throw new MissingAccessError();
			}

			await ctx.get('userService').requestBulkMessageDeletion({
				userId: user.id,
				delayMs: 60 * 1000,
			});
			return ctx.body(null, 204);
		},
	);
};
