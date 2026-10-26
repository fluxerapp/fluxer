// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {UserConnectionRow} from '@app/api/database/types/ConnectionTypes';
import type {
	OperatorPassOfferRow,
	OperatorPassPurchaseRow,
	OperatorPassRow,
	OperatorPassUnitEventRow,
	OperatorPassUnitRow,
	OperatorPassWaitlistRow,
	OperatorPassWallEntryRow,
	OperatorPassWallEventRow,
} from '@app/api/database/types/OperatorPassTypes';
import type {StorePurchaseRow} from '@app/api/database/types/StoreBillingTypes';
import type {Application} from '@app/api/models/Application';
import type {AuthSession} from '@app/api/models/AuthSession';
import type {Channel} from '@app/api/models/Channel';
import type {FavoriteMeme} from '@app/api/models/FavoriteMeme';
import type {GiftCode} from '@app/api/models/GiftCode';
import type {Guild} from '@app/api/models/Guild';
import type {GuildMember} from '@app/api/models/GuildMember';
import type {MfaBackupCode} from '@app/api/models/MfaBackupCode';
import type {Payment} from '@app/api/models/Payment';
import type {PushSubscription} from '@app/api/models/PushSubscription';
import type {Relationship} from '@app/api/models/Relationship';
import type {SavedMessage} from '@app/api/models/SavedMessage';
import type {ThreadMember} from '@app/api/models/ThreadMember';
import type {User} from '@app/api/models/User';
import type {UserGuildSettings} from '@app/api/models/UserGuildSettings';
import type {UserSettings} from '@app/api/models/UserSettings';
import type {WebAuthnCredential} from '@app/api/models/WebAuthnCredential';
import {mapStorePurchaseToResponse} from '@app/api/store_billing/StoreBillingMappers';
import {resolveSessionClientInfo} from '@app/api/utils/SessionClientIdentity';
import {getAnimatedAssetExtension} from '@app/api/worker/utils/AssetArchiveHelpers';
import {
	decodeSyncedPreferencesLenient,
	syncedPreferencesToJson,
} from '@fluxer/schema/src/domains/user/SyncedPreferencesCodec';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';

interface GuildMembershipEntry {
	member: GuildMember | null;
	guild: Guild | null;
	guildId: GuildID;
}

interface ArchivedUserSettings {
	locale: UserSettings['locale'];
	theme: string;
	status: string;
	custom_status: {
		text: string | null;
		emoji_id: string | null;
		emoji_name: string | null;
		emoji_animated: boolean;
		expires_at: string | null;
	} | null;
	developer_mode: boolean;
	message_display_compact: boolean;
	animate_emoji: boolean;
	animate_stickers: UserSettings['animateStickers'];
	gif_auto_play: boolean;
	render_embeds: boolean;
	render_reactions: boolean;
	render_spoilers: UserSettings['renderSpoilers'];
	inline_attachment_media: boolean;
	inline_embed_media: boolean;
	explicit_content_filter: number;
	friend_source_flags: number;
	default_guilds_restricted: boolean;
	bot_default_guilds_restricted: boolean;
	restricted_guilds: Array<string>;
	bot_restricted_guilds: Array<string>;
	guild_positions: Array<string>;
	guild_folders: Array<{
		id: number;
		name: string | null;
		color: number | null;
		flags: number;
		icon: UserSettings['guildFolders'][number]['icon'];
		guild_ids: Array<string>;
	}>;
	afk_timeout: number;
	time_format: UserSettings['timeFormat'];
	suppress_unprivileged_self_mentions: boolean;
	suppress_unprivileged_self_mentions_bypass_user_ids: Array<string>;
	staff_dm_access_user_ids: Array<string>;
	synced_preferences: unknown;
	profile_privacy: UserSettings['profilePrivacy'];
	privacy_setup_version: number;
	privacy_setup_completed_at: string | null;
}

interface UserDataJsonParams {
	user: User;
	userId: UserID;
	productName: string;
	authSessions: Array<AuthSession>;
	relationships: Array<Relationship>;
	userNotes: Map<UserID, string>;
	userSettings: UserSettings | null;
	guildMemberships: Array<GuildMembershipEntry>;
	guildSettings: Array<UserGuildSettings | null>;
	threadMemberships: Array<ThreadMember>;
	savedMessages: Array<SavedMessage>;
	privateChannels: Array<Channel>;
	favoriteMemes: Array<FavoriteMeme>;
	pushSubscriptions: Array<PushSubscription>;
	webAuthnCredentials: Array<WebAuthnCredential>;
	mfaBackupCodes: Array<MfaBackupCode>;
	recoveryKitCreatedAt: Date | null;
	createdGiftCodes: Array<GiftCode>;
	payments: Array<Payment>;
	storePurchases: Array<StorePurchaseRow>;
	operatorPass: OperatorPassRow | null;
	operatorPassPurchases: Array<OperatorPassPurchaseRow>;
	operatorPassWaitlistEntry: OperatorPassWaitlistRow | null;
	operatorPassWall: OperatorPassWallEntryRow | null;
	operatorPassWallEvents: Array<OperatorPassWallEventRow>;
	operatorPassUnits: Array<{unit: OperatorPassUnitRow; events: Array<OperatorPassUnitEventRow>}>;
	operatorPassOffers: Array<OperatorPassOfferRow>;
	oauthClients: Array<Application>;
	connections: Array<UserConnectionRow>;
	pinnedDms: Array<{
		channel_id: bigint;
		sort_order: number;
	}>;
	authorizedIps: Array<{
		ip: string;
	}>;
	activityData: {
		last_active_at: Date | null;
		last_active_ip: string | null;
	};
}

function mapOperatorPassUnit(unit: OperatorPassUnitRow, events: Array<OperatorPassUnitEventRow>, userId: UserID) {
	const bought = unit.buyer_id === userId;
	const handOff = events.findIndex((event) => event.kind === 'transferred');
	const received = events.findIndex((event) => event.kind === 'transferred' && event.recipient_id === userId);
	const passedOn = bought && handOff !== -1;
	let visible = events;
	if (passedOn) {
		visible = events.slice(0, handOff + 1);
	} else if (!bought && received !== -1) {
		visible = events.slice(received);
	} else if (!bought && unit.purchase_id !== null) {
		visible = [];
	}
	return {
		id: bought || unit.purchase_id === null ? unit.pass_id : null,
		purchase_id: bought ? (unit.purchase_id ?? null) : null,
		source: unit.source,
		status: passedOn ? 'passed_on' : unit.status,
		held_by_you: unit.holder_id === userId,
		delivered_at: (passedOn ? unit.first_delivered_at : unit.delivered_at).toISOString(),
		first_delivered_at: (bought ? unit.first_delivered_at : unit.delivered_at).toISOString(),
		ended_at: passedOn ? null : (unit.ended_at?.toISOString() ?? null),
		end_reason: passedOn ? null : (unit.end_reason ?? null),
		withdrawal_country: bought ? (unit.withdrawal_country ?? null) : null,
		withdrawal_days: bought ? (unit.withdrawal_days ?? null) : null,
		events: visible.map((event) => ({
			occurred_at: event.occurred_at.toISOString(),
			kind: event.kind,
			payment_id: bought ? (event.payment_id ?? null) : null,
			sender_id: event.sender_id?.toString() ?? null,
			recipient_id: event.recipient_id?.toString() ?? null,
			offer_id: event.offer_id ?? null,
			country: bought ? (event.country ?? null) : null,
			days: bought ? (event.days ?? null) : null,
			rule_version: bought ? (event.rule_version ?? null) : null,
			reason: event.reason ?? null,
		})),
	};
}

export function mapPayment(payment: Payment) {
	return {
		checkout_session_id: payment.checkoutSessionId,
		amount_cents: payment.amountCents,
		currency: payment.currency,
		status: payment.status,
		subscription_id: payment.subscriptionId,
		payment_intent_id: payment.paymentIntentId,
		product_type: payment.productType,
		is_gift: payment.isGift,
		gift_code: payment.giftCode,
		created_at: payment.createdAt.toISOString(),
		completed_at: payment.completedAt?.toISOString() ?? null,
	};
}

export function mapOAuthApplication(app: Application) {
	return {
		application_id: app.applicationId.toString(),
		name: app.name,
		redirect_uris: Array.from(app.oauth2RedirectUris),
	};
}

export function mapSecurityData(params: {
	authorizedIps: Array<{
		ip: string;
	}>;
	activityData: {
		last_active_at: Date | null;
		last_active_ip: string | null;
	};
}) {
	return {
		authorized_ips: params.authorizedIps,
		activity_tracking: {
			last_active_at: params.activityData.last_active_at?.toISOString() ?? null,
			last_active_ip: params.activityData.last_active_ip,
		},
	};
}

export function buildUserDataJson(params: UserDataJsonParams) {
	const {
		user,
		userId,
		productName,
		authSessions,
		relationships,
		userNotes,
		userSettings,
		guildMemberships,
		guildSettings,
		threadMemberships,
		savedMessages,
		privateChannels,
		favoriteMemes,
		pushSubscriptions,
		webAuthnCredentials,
		mfaBackupCodes,
		recoveryKitCreatedAt,
		createdGiftCodes,
		payments,
		storePurchases,
		operatorPass,
		operatorPassPurchases,
		operatorPassWaitlistEntry,
		operatorPassWall,
		operatorPassWallEvents,
		operatorPassUnits,
		operatorPassOffers,
		oauthClients,
		connections,
		pinnedDms,
		authorizedIps,
		activityData,
	} = params;
	const responseFields = {
		user: {
			id: user.id.toString(),
			username: user.username,
			discriminator: user.discriminator,
			bot: user.isBot,
			system: user.isSystem,
			email: user.email,
			email_verified: user.emailVerified,
			email_bounced: user.emailBounced,
			avatar_hash: user.avatarHash,
			avatar_url: user.avatarHash
				? `${Config.endpoints.media}/avatars/${userId}/${user.avatarHash}.${getAnimatedAssetExtension(user.avatarHash)}`
				: null,
			banner_hash: user.bannerHash,
			banner_url: user.bannerHash
				? `${Config.endpoints.media}/banners/${userId}/${user.bannerHash}.${getAnimatedAssetExtension(user.bannerHash)}`
				: null,
			bio: user.bio,
			pronouns: user.pronouns,
			accent_color: user.accentColor,
			date_of_birth: user.dateOfBirth,
			locale: user.locale,
			flags: user.flags.toString(),
			premium_type: user.premiumType,
			premium_since: user.premiumSince?.toISOString() ?? null,
			premium_until: user.premiumUntil?.toISOString() ?? null,
			premium_lifetime_sequence: user.premiumLifetimeSequence,
			stripe_customer_id: user.stripeCustomerId,
			stripe_subscription_id: user.stripeSubscriptionId,
			terms_agreed_at: user.termsAgreedAt?.toISOString() ?? null,
			privacy_agreed_at: user.privacyAgreedAt?.toISOString() ?? null,
			last_active_at: user.lastActiveAt?.toISOString() ?? null,
			created_at: snowflakeToDate(user.id).toISOString(),
			mfa_enabled: user.authenticatorTypes.size > 0,
			authenticator_types: Array.from(user.authenticatorTypes),
		},
		auth_sessions: authSessions.map((session) => {
			const clientInfo = resolveSessionClientInfo({
				userAgent: session.clientUserAgent,
				reportedOs: session.clientOs ?? null,
				productName,
			});
			return {
				created_at: session.createdAt.toISOString(),
				approx_last_used_at: session.approximateLastUsedAt?.toISOString() ?? null,
				client_ip: session.clientIp,
				client_os: clientInfo.os,
				client_user_agent: session.clientUserAgent,
				client_platform: clientInfo.platform,
				app_platform: session.appPlatform,
				app_version: session.appVersion,
				app_release_channel: session.appReleaseChannel,
				app_build_number: session.appBuildNumber,
				app_beta_number: session.appBetaNumber,
			};
		}),
		relationships: relationships.map((rel) => ({
			target_user_id: rel.targetUserId.toString(),
			type: rel.type,
			nickname: rel.nickname,
			since: rel.since?.toISOString() ?? null,
		})),
		notes: Array.from(userNotes.entries()).map(([targetUserId, note]) => ({
			target_user_id: targetUserId.toString(),
			note,
		})),
	};
	let archivedSettings: ArchivedUserSettings | null = null;
	if (userSettings) {
		const statusFields = {
			locale: userSettings.locale,
			theme: userSettings.theme,
			status: userSettings.status,
		};
		let customStatus: ArchivedUserSettings['custom_status'] = null;
		if (userSettings.customStatus) {
			customStatus = {
				text: userSettings.customStatus.text,
				emoji_id: userSettings.customStatus.emojiId?.toString() ?? null,
				emoji_name: userSettings.customStatus.emojiName,
				emoji_animated: userSettings.customStatus.emojiAnimated,
				expires_at: userSettings.customStatus.expiresAt?.toISOString() ?? null,
			};
		}
		archivedSettings = {
			...statusFields,
			custom_status: customStatus,
			developer_mode: userSettings.developerMode,
			message_display_compact: userSettings.compactMessageDisplay,
			animate_emoji: userSettings.animateEmoji,
			animate_stickers: userSettings.animateStickers,
			gif_auto_play: userSettings.gifAutoPlay,
			render_embeds: userSettings.renderEmbeds,
			render_reactions: userSettings.renderReactions,
			render_spoilers: userSettings.renderSpoilers,
			inline_attachment_media: userSettings.inlineAttachmentMedia,
			inline_embed_media: userSettings.inlineEmbedMedia,
			explicit_content_filter: userSettings.explicitContentFilter,
			friend_source_flags: userSettings.friendSourceFlags,
			default_guilds_restricted: userSettings.defaultGuildsRestricted,
			bot_default_guilds_restricted: userSettings.botDefaultGuildsRestricted,
			restricted_guilds: Array.from(userSettings.restrictedGuilds).map((id) => id.toString()),
			bot_restricted_guilds: Array.from(userSettings.botRestrictedGuilds).map((id) => id.toString()),
			guild_positions: userSettings.guildPositions.map((id) => id.toString()),
			guild_folders: userSettings.guildFolders.map((folder) => ({
				id: folder.folderId,
				name: folder.name,
				color: folder.color,
				flags: folder.flags,
				icon: folder.icon,
				guild_ids: folder.guildIds.map(String),
			})),
			afk_timeout: userSettings.afkTimeout,
			time_format: userSettings.timeFormat,
			suppress_unprivileged_self_mentions: userSettings.suppressUnprivilegedSelfMentions,
			suppress_unprivileged_self_mentions_bypass_user_ids: Array.from(
				userSettings.suppressUnprivilegedSelfMentionBypassUserIds,
			).map((id) => id.toString()),
			staff_dm_access_user_ids: Array.from(userSettings.staffDmAccessUserIds).map((id) => id.toString()),
			synced_preferences: syncedPreferencesToJson(decodeSyncedPreferencesLenient(userSettings.syncedPreferences)),
			profile_privacy: userSettings.profilePrivacy,
			privacy_setup_version: userSettings.privacySetupVersion,
			privacy_setup_completed_at: userSettings.privacySetupCompletedAt?.toISOString() ?? null,
		};
	}
	return {
		...responseFields,
		user_settings: archivedSettings,
		guild_memberships: guildMemberships
			.filter((membership): membership is GuildMembershipEntry & {member: GuildMember} => membership.member !== null)
			.map(({member, guild, guildId}) => ({
				guild_id: guildId.toString(),
				guild_name: guild?.name ?? null,
				joined_at: member.joinedAt.toISOString(),
				nick: member.nickname,
				avatar_hash: member.avatarHash,
				avatar_url: member.avatarHash
					? `${Config.endpoints.media}/guilds/${guildId}/users/${userId}/avatars/${member.avatarHash}`
					: null,
				banner_hash: member.bannerHash,
				banner_url: member.bannerHash
					? `${Config.endpoints.media}/guilds/${guildId}/users/${userId}/banners/${member.bannerHash}`
					: null,
				role_ids: Array.from(member.roleIds).map((id) => id.toString()),
			})),
		user_guild_settings: guildSettings
			.filter((settings) => settings !== null)
			.map((settings) => ({
				guild_id: settings.guildId.toString(),
				message_notifications: settings.messageNotifications,
				muted: settings.muted,
				mobile_push: settings.mobilePush,
				suppress_everyone: settings.suppressEveryone,
				suppress_roles: settings.suppressRoles,
				hide_muted_channels: settings.hideMutedChannels,
			})),
		...(threadMemberships.length > 0
			? {
					thread_memberships: threadMemberships.map((member) => ({
						thread_id: member.threadId.toString(),
						guild_id: member.guildId.toString(),
						parent_id: member.parentId.toString(),
						join_timestamp: member.joinTimestamp.toISOString(),
						flags: member.flags,
						muted: member.muted,
					})),
				}
			: {}),
		saved_messages: savedMessages.map((msg) => ({
			channel_id: msg.channelId.toString(),
			message_id: msg.messageId.toString(),
			saved_at: msg.savedAt.toISOString(),
		})),
		private_channels: privateChannels.map((channel) => ({
			channel_id: channel.id.toString(),
			type: channel.type,
			name: channel.name,
			icon_hash: channel.iconHash,
			owner_id: channel.ownerId?.toString() ?? null,
			recipient_ids: Array.from(channel.recipientIds).map((id) => id.toString()),
			last_message_id: channel.lastMessageId?.toString() ?? null,
		})),
		favorite_memes: favoriteMemes.map((meme) => ({
			meme_id: meme.id.toString(),
			name: meme.name,
			alt_text: meme.altText,
			tags: meme.tags,
			filename: meme.filename,
			content_type: meme.contentType,
			size: meme.size.toString(),
			width: meme.width,
			height: meme.height,
			duration: meme.duration,
		})),
		push_subscriptions: pushSubscriptions.map((sub) => ({
			subscription_id: sub.subscriptionId,
			platform: sub.platform,
			app_id: sub.appId,
			provider_environment: sub.providerEnvironment,
			endpoint: sub.endpoint,
			user_agent: sub.userAgent,
		})),
		webauthn_credentials: webAuthnCredentials.map((cred) => ({
			credential_id: cred.credentialId,
			name: cred.name,
			transports: cred.transports ? Array.from(cred.transports) : [],
			created_at: cred.createdAt.toISOString(),
			last_used_at: cred.lastUsedAt?.toISOString() ?? null,
		})),
		mfa_backup_codes: {
			total_count: mfaBackupCodes.length,
			consumed_count: mfaBackupCodes.filter((code) => code.consumed).length,
			remaining_count: mfaBackupCodes.filter((code) => !code.consumed).length,
		},
		...(recoveryKitCreatedAt ? {recovery_kit: {created_at: recoveryKitCreatedAt.toISOString()}} : {}),
		gift_codes_created: createdGiftCodes.map((gift) => ({
			code: gift.code,
			duration_months: gift.durationMonths,
			duration_type: gift.durationType,
			duration_quantity: gift.durationQuantity,
			created_at: gift.createdAt.toISOString(),
			redeemed_by_user_id: gift.redeemedByUserId?.toString() ?? null,
			redeemed_at: gift.redeemedAt?.toISOString() ?? null,
			stripe_payment_intent_id: gift.stripePaymentIntentId,
		})),
		payments: payments.map(mapPayment),
		store_purchases: storePurchases.map(mapStorePurchaseToResponse),
		operator_pass: operatorPass
			? {
					granted_at: operatorPass.granted_at.toISOString(),
					source: operatorPass.source,
					checkout_session_id: operatorPass.checkout_session_id ?? null,
					pass_count: operatorPass.pass_count ?? 1,
					payment_intent_id: operatorPass.payment_intent_id ?? null,
					amount_minor: operatorPass.amount_minor ?? null,
					currency: operatorPass.currency ?? null,
					paused: operatorPass.paused_at != null,
					terms_version: operatorPass.terms_version ?? null,
					terms_accepted_at: operatorPass.terms_accepted_at?.toISOString() ?? null,
					waiver_consent_at: operatorPass.waiver_consent_at?.toISOString() ?? null,
					waiver_consent_version: operatorPass.waiver_consent_version ?? null,
					waiver_consent_ip: operatorPass.waiver_consent_ip ?? null,
					waiver_consent_user_agent: operatorPass.waiver_consent_user_agent ?? null,
				}
			: null,
		operator_pass_wall: operatorPassWall
			? {
					name: operatorPassWall.name ?? null,
					opted_out: operatorPassWall.opted_out,
					keep_as_is: operatorPassWall.keep_as_is,
					release_list_opt_in: operatorPassWall.release_list_opt_in ?? false,
					hidden: operatorPassWall.hidden,
					live_name: operatorPassWall.live_name ?? null,
					business: operatorPassWall.live_business ?? false,
					pending_name: operatorPassWall.pending_name ?? null,
					declined_name: operatorPassWall.declined_name ?? null,
					decline_reason: operatorPassWall.decline_reason ?? null,
					decline_note: operatorPassWall.decline_note ?? null,
					listed_at: operatorPassWall.listed_at?.toISOString() ?? null,
					delivered_at: operatorPassWall.delivered_at?.toISOString() ?? null,
					release_published_year: operatorPassWall.release_published_year ?? null,
					release_erased_at: operatorPassWall.release_erased_at?.toISOString() ?? null,
				}
			: null,
		operator_pass_wall_history: operatorPassWallEvents.map((event) => ({
			occurred_at: event.occurred_at.toISOString(),
			kind: event.kind,
			name: event.name ?? null,
			reason: event.reason ?? null,
			cause: event.cause ?? null,
			purchase_id: operatorPassPurchases.some((purchase) => purchase.checkout_session_id === event.purchase_id)
				? (event.purchase_id ?? null)
				: null,
			release_year: event.release_year ?? null,
			release_tag: event.release_tag ?? null,
			pass_count: event.pass_count ?? null,
			first_pass_at: event.first_pass_at?.toISOString() ?? null,
		})),
		operator_pass_purchases: operatorPassPurchases.map((purchase) => ({
			checkout_session_id: purchase.checkout_session_id,
			quantity: purchase.quantity ?? 1,
			status: purchase.status,
			phase: purchase.phase,
			amount_minor: purchase.amount_minor,
			currency: purchase.currency,
			quote_honored: purchase.quote_honored,
			payment_intent_id: purchase.payment_intent_id ?? null,
			invoice_id: purchase.invoice_id ?? null,
			subscription_id: purchase.subscription_id ?? null,
			created_at: purchase.created_at.toISOString(),
			completed_at: purchase.completed_at?.toISOString() ?? null,
			refunded_at: purchase.refunded_at?.toISOString() ?? null,
			terms_version: purchase.operator_pass_terms_version ?? null,
			purchased_at: purchase.purchased_at?.toISOString() ?? null,
			delivered_at: purchase.perks_granted_at?.toISOString() ?? null,
			withdrawal_ends_at: purchase.withdrawal_ends_at?.toISOString() ?? null,
			withdrawn_at: purchase.withdrawn_at?.toISOString() ?? null,
			withdrawal_name: purchase.withdrawal_name ?? null,
			waiver_consent_at: purchase.waiver_consent_at?.toISOString() ?? null,
			waiver_consent_version: purchase.waiver_consent_version ?? null,
			waiver_consent_text: purchase.waiver_consent_text ?? null,
			waiver_consent_ip: purchase.waiver_consent_ip ?? null,
			waiver_consent_user_agent: purchase.waiver_consent_user_agent ?? null,
			wall_name: purchase.wall_name ?? null,
			wall_opt_out: purchase.wall_opt_out ?? null,
			withdrawal_country: purchase.withdrawal_country ?? null,
			withdrawal_days: purchase.withdrawal_days ?? null,
			withdrawal_rule_version: purchase.withdrawal_rule_version ?? null,
			stripe_terms_consent: purchase.stripe_terms_consent ?? null,
		})),
		operator_passes: operatorPassUnits.map(({unit, events}) => mapOperatorPassUnit(unit, events, userId)),
		operator_pass_offers: operatorPassOffers.map((offer) => ({
			id: offer.offer_id,
			pass_id: offer.sender_id === userId ? offer.pass_id : null,
			status: offer.status,
			sender_id: offer.sender_id.toString(),
			recipient_id: offer.recipient_id.toString(),
			created_at: offer.created_at.toISOString(),
			accepted_at: offer.accepted_at?.toISOString() ?? null,
			arrives_at: offer.completes_at?.toISOString() ?? null,
			ended_at: offer.ended_at?.toISOString() ?? null,
			wall_name: offer.recipient_id === userId ? (offer.wall_name ?? null) : null,
			terms_version: offer.terms_version ?? null,
		})),
		operator_pass_waitlist: operatorPassWaitlistEntry
			? {
					status: operatorPassWaitlistEntry.status,
					locale: operatorPassWaitlistEntry.locale ?? null,
					country_code: operatorPassWaitlistEntry.country_code ?? null,
					consent_ip: operatorPassWaitlistEntry.consent_ip ?? null,
					price_feedback: operatorPassWaitlistEntry.price_feedback ?? null,
					currency: operatorPassWaitlistEntry.currency ?? null,
					followup_picks: [...(operatorPassWaitlistEntry.followup_picks ?? [])].sort(),
					followup_about: [...(operatorPassWaitlistEntry.followup_about ?? [])].sort(),
					followup_other_text: operatorPassWaitlistEntry.followup_other_text ?? null,
					followup_answered_at: operatorPassWaitlistEntry.followup_answered_at?.toISOString() ?? null,
					created_at: operatorPassWaitlistEntry.created_at.toISOString(),
					updated_at: operatorPassWaitlistEntry.updated_at.toISOString(),
					confirmed_at: operatorPassWaitlistEntry.confirmed_at?.toISOString() ?? null,
					unsubscribed_at: operatorPassWaitlistEntry.unsubscribed_at?.toISOString() ?? null,
					sale_email_sent_at: operatorPassWaitlistEntry.sale_email_sent_at?.toISOString() ?? null,
				}
			: null,
		oauth_applications: oauthClients.map(mapOAuthApplication),
		connections: connections.map((connection) => ({
			id: connection.connection_id,
			type: connection.connection_type,
			identifier: connection.identifier,
			name: connection.name,
			verified: connection.verified,
			visibility_flags: connection.visibility_flags,
			sort_order: connection.sort_order,
			created_at: connection.created_at.toISOString(),
			verified_at: connection.verified_at?.toISOString() ?? null,
			last_verified_at: connection.last_verified_at?.toISOString() ?? null,
		})),
		pinned_dms: pinnedDms.map((pin) => ({
			channel_id: pin.channel_id.toString(),
			sort_order: pin.sort_order,
		})),
		...mapSecurityData({authorizedIps, activityData}),
	};
}
