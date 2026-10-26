// SPDX-License-Identifier: AGPL-3.0-or-later

import {RegistrationModes} from '@fluxer/constants/src/AccountConfigConstants';
import {
	AccountIdentityLockReasons,
	AccountIdentityModes,
	TagStyles,
} from '@fluxer/constants/src/AccountIdentityConstants';
import {MAX_CUSTOM_BADGES_PER_TYPE} from '@fluxer/constants/src/BadgeConstants';
import {
	CaptchaProviderSchema,
	EffectiveVerificationBasisSchema,
	EmailDeliveryStateSchema,
	GuildJoinGateSchema,
	InstantAccountAvailabilitySchema,
	InteractionTierModeSchema,
} from '@fluxer/schema/src/domains/account/AccountTierSchemas';
import {DomainMigrationDiscoveryResponse} from '@fluxer/schema/src/domains/admin/DomainMigrationSchemas';
import {SsoStatusResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {
	createNamedStringLiteralUnion,
	createStringType,
	SnowflakeStringType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const LimitFilterResponse = z.object({
	traits: z.array(z.string()).optional().describe('Trait filters for this limit rule'),
	guildFeatures: z.array(z.string()).optional().describe('Guild feature filters for this limit rule'),
});

const LimitRuleResponse = z.object({
	id: z.string().describe('Unique identifier for this limit rule'),
	filters: LimitFilterResponse.optional().describe('Filters that determine when this rule applies'),
	overrides: z
		.record(z.string(), z.number())
		.describe('Map of limit keys to their override values (differences from defaults)'),
});

const LimitConfigResponse = z.object({
	version: z.literal(2).describe('Wire format version'),
	traitDefinitions: z.array(z.string()).describe('Available trait definitions (e.g., "premium")'),
	rules: z.array(LimitRuleResponse).describe('Array of limit rules to evaluate'),
	defaultsHash: z.string().describe('Hash of the default limit values for cache invalidation'),
});

export const InstanceBrandingSchema = z
	.object({
		product_name: z.string().describe('Public product name shown by client applications'),
		icon_url: z.string().nullable().describe('Optional image URL for the full application icon'),
		symbol_url: z.string().nullable().describe('Optional image URL for the compact application symbol'),
		logo_url: z.string().nullable().describe('Optional image URL for the application logo'),
		wordmark_url: z.string().nullable().describe('Optional image URL for the application wordmark'),
		favicon_url: z.string().nullable().describe('Optional favicon URL for browser metadata'),
		favicon_dark_url: z
			.string()
			.nullable()
			.describe('Optional favicon URL a self-hosted instance shows when the browser uses a dark color scheme'),
		theme_color: z.string().nullable().describe('Optional browser theme color'),
		status_page_url: z.string().nullable().describe('Optional public status page URL'),
		status_page_incident_history_url: z
			.string()
			.nullable()
			.describe('Optional public status page incident history URL'),
		premium_product_name: z.string().describe('Name of the premium tier shown by client applications'),
		premium_info_url: z.string().nullable().describe('Optional absolute URL of a page describing the premium tier'),
		sign_in_backdrop_url: z
			.string()
			.nullable()
			.describe('Optional image URL drawn behind the sign-in card on a self-hosted instance'),
		custom_css: z
			.string()
			.nullable()
			.describe('Optional stylesheet a self-hosted instance applies to every page of its web and desktop app'),
		upload_limit_message: z
			.string()
			.nullable()
			.describe(
				'Optional Markdown a self-hosted instance shows when a file is over the upload limit, in place of the default note',
			),
		trusted_domains: z
			.array(z.string())
			.describe(
				'Extra domains a self-hosted instance trusts, so links to them open without a warning. Each entry is a hostname, or a hostname with a *. prefix that also covers its subdomains',
			),
	})
	.describe('Branding values safe to expose to clients');
export type InstanceBranding = z.infer<typeof InstanceBrandingSchema>;

export const AccountIdentityLockReasonSchema = createNamedStringLiteralUnion(
	[
		[
			AccountIdentityLockReasons.SETUP_CONFIGURED,
			'SETUP_CONFIGURED',
			'The instance configuration marks setup as finished',
		],
		[
			AccountIdentityLockReasons.EXISTING_INSTANCE,
			'EXISTING_INSTANCE',
			'The instance was in use before username sign-in existed, so it stays on email',
		],
		[AccountIdentityLockReasons.ADMIN_EXISTS, 'ADMIN_EXISTS', 'The instance already has an administrator'],
		[AccountIdentityLockReasons.ACCOUNTS_EXIST, 'ACCOUNTS_EXIST', 'The instance already has accounts'],
	],
	'Why the sign-in method can no longer change',
);

export const InstanceSetupSchema = z
	.object({
		configured: z.boolean().describe('Whether the instance administrator has completed initial setup'),
		admin_url: z.string().nullable().describe('Admin panel URL to continue instance setup'),
		account_identity_locked: z
			.boolean()
			.optional()
			.describe(
				'Present only while a self-hosted instance is unconfigured. True when the sign-in method can no longer change',
			),
		account_identity_lock_reason: AccountIdentityLockReasonSchema.nullable()
			.optional()
			.describe(
				'Present only while a self-hosted instance is unconfigured. Why the sign-in method can no longer change, or null while it still can',
			),
		setup_token_required: z
			.boolean()
			.optional()
			.describe(
				'Present only while a self-hosted instance is unconfigured. True when registration needs the setup token',
			),
	})
	.describe('Initial setup state for self-hosted instances');
export type InstanceSetup = z.infer<typeof InstanceSetupSchema>;

const InstanceLegalSchema = z
	.object({
		terms_url: z.string().nullable().describe('Optional public terms of service URL for account registration'),
		privacy_url: z.string().nullable().describe('Optional public privacy policy URL for account registration'),
		guidelines_url: z
			.string()
			.nullable()
			.describe('Optional public community guidelines URL linked from reporting and enforcement notices'),
	})
	.describe('Optional legal and policy document URLs shown to users');

const InstanceAppRegistrationSchema = z
	.object({
		collect_date_of_birth: z.boolean().describe('Whether public registration collects and validates date of birth'),
	})
	.describe('Public registration field collection policy');

export const InstanceAppPublicSchema = z.object({
	branding: InstanceBrandingSchema,
	setup: InstanceSetupSchema,
	legal: InstanceLegalSchema,
	registration: InstanceAppRegistrationSchema,
});
export type InstanceAppPublic = z.infer<typeof InstanceAppPublicSchema>;

export const InstanceEndpointsSchema = z
	.object({
		api: z.string().describe('Base URL for authenticated API requests'),
		api_client: z.string().describe('Base URL for client API requests'),
		api_public: z.string().describe('Base URL for public API requests'),
		gateway: z.string().describe('WebSocket URL for the gateway'),
		media: z.string().describe('Base URL for the media proxy'),
		upload_relay: z.string().optional().describe('Base URL for proxied attachment and preview uploads'),
		static_cdn: z.string().describe('Base URL for static assets (avatars, emojis, etc.)'),
		marketing: z.string().describe('Base URL for the marketing website'),
		docs: z.string().optional().describe('Base URL for the documentation website'),
		admin: z.string().describe('Base URL for the admin panel'),
		invite: z.string().describe('Base URL for invite links'),
		gift: z.string().describe('Base URL for gift links'),
		webapp: z.string().describe('Base URL for the web application'),
	})
	.describe('Endpoint URLs for various services');
export type InstanceEndpoints = z.infer<typeof InstanceEndpointsSchema>;

export const InstanceCaptchaSchema = z
	.object({
		provider: CaptchaProviderSchema.describe('Captcha provider (altcha or none)'),
	})
	.describe('Captcha configuration');
export type InstanceCaptcha = z.infer<typeof InstanceCaptchaSchema>;

export const AccountIdentityModeSchema = createNamedStringLiteralUnion(
	[
		[AccountIdentityModes.EMAIL, 'EMAIL', 'People sign in with an email address'],
		[AccountIdentityModes.USERNAME, 'USERNAME', 'People sign in with a username and no email is collected'],
	],
	'How people identify themselves when they sign in',
);

export const TagStyleSchema = createNamedStringLiteralUnion(
	[
		[TagStyles.NONE, 'NONE', 'Usernames are unique and shown without a tag'],
		[TagStyles.RANDOM, 'RANDOM', 'Every account gets a random tag'],
	],
	'How usernames are tagged',
);

export const InstanceFeaturesSchema = z
	.object({
		voice_enabled: z.boolean().describe('Whether voice/video calling is enabled'),
		stripe_enabled: z.boolean().describe('Whether premium purchases through Stripe are available'),
		premium_enabled: z
			.boolean()
			.describe('Whether this instance has a premium tier, so premium status, gifts and perks apply'),
		stripe_serviceable: z
			.boolean()
			.describe('Whether existing Stripe subscriptions can be managed, cancelled and billed on this instance'),
		self_hosted: z.boolean().describe('Whether this is a self-hosted instance'),
		presigned_attachment_uploads: z.boolean().describe('Whether clients can request presigned attachment upload URLs'),
		emails_enabled: z.boolean().describe('Whether the instance sends emails (verification, password reset, etc.)'),
		phone_verification_enabled: z.boolean().describe('Deprecated. Always false.'),
		discovery_enabled: z
			.boolean()
			.optional()
			.describe('Whether the Explore community directory is available. Missing means true'),
		desktop_modules_enabled: z.boolean().optional().describe('Whether desktop clients may load downloadable modules'),
		account_deletion_grace_period_hours: z
			.number()
			.int()
			.min(1)
			.max(8760)
			.optional()
			.describe('Hours between a deletion request and the permanent deletion of the account'),
		account_identity: AccountIdentityModeSchema.optional().describe(
			'How people sign in on this instance. Clients treat a missing value as email',
		),
		tag_style: TagStyleSchema.optional().describe('How usernames are tagged. Clients treat a missing value as random'),
		identifier_first: z
			.boolean()
			.optional()
			.describe('Whether clients show the single sign-in entry that starts with an email. Missing means false'),
		instant_accounts: z
			.boolean()
			.optional()
			.describe('Whether accounts can be created without an email, from a display name. Missing means false'),
		email_codes: z
			.boolean()
			.optional()
			.describe('Whether the instance can send sign-in and verification codes by email. Missing means false'),
		date_of_birth_step: z
			.boolean()
			.optional()
			.describe(
				'Whether accounts enter their date of birth in a step after they are created, so sign-up never asks for it. Missing means false',
			),
		instant_account_availability: InstantAccountAvailabilitySchema.optional().describe(
			'Whether accounts can be created from just a display name through public sign-up, and why not. Missing means instant_accounts decides',
		),
		interaction_tier: InteractionTierModeSchema.optional().describe(
			'Which limits apply to accounts that have not finished setting up. Missing means legacy',
		),
		guild_join_gate: GuildJoinGateSchema.optional().describe(
			'When joining a community needs a finished account while interaction_tier is enforced. Missing means always',
		),
		email_delivery_state: EmailDeliveryStateSchema.optional().describe(
			'Whether the instance sends email, and why not. Missing means emails_enabled decides',
		),
		verification_basis: EffectiveVerificationBasisSchema.optional().describe(
			'What an account needs to finish setting up right now. Missing means confirmed_email while emails_enabled, otherwise local_credential',
		),
	})
	.describe('Feature flags for this instance');
export type InstanceFeatures = z.infer<typeof InstanceFeaturesSchema>;

export const InstanceGifSchema = z
	.object({
		provider: z.string().describe('Stable machine name of the active GIF provider.'),
		display_name: z.string().describe('Human-readable provider name shown in the UI'),
		attribution_required: z
			.boolean()
			.describe('Whether the client must show a "Powered by …" watermark for this provider'),
	})
	.describe('GIF provider configuration for clients');
export type InstanceGif = z.infer<typeof InstanceGifSchema>;

export const InstanceSsoSchema = SsoStatusResponse.extend({
	available: z.boolean().optional().describe('Whether an SSO provider is configured for this instance'),
}).describe('Single sign-on configuration');
export type InstanceSso = z.infer<typeof InstanceSsoSchema>;

export const InstanceRegistrationModeSchema = createNamedStringLiteralUnion(
	[
		[RegistrationModes.OPEN, 'open', 'Anyone can register'],
		[RegistrationModes.APPROVAL, 'approval', 'Anyone can register, but admins must approve new accounts'],
		[RegistrationModes.CLOSED, 'closed', 'Public registration is closed'],
	],
	'Registration mode',
);

export const InstanceRegistrationSchema = z
	.object({
		mode: InstanceRegistrationModeSchema.describe('Public registration mode for this instance'),
		admin_registration_urls_enabled: z.boolean().describe('Whether admin-issued registration URLs are accepted'),
	})
	.describe('Registration policy for this instance');
export type InstanceRegistration = z.infer<typeof InstanceRegistrationSchema>;

export const InstanceCommunitySchema = z
	.object({
		single_community: z
			.boolean()
			.describe('Whether this instance runs as a single community that every user automatically joins'),
		single_community_guild_id: z
			.string()
			.nullable()
			.describe('The stock community guild ID when single-community mode is enabled'),
		direct_messages_disabled: z.boolean().describe('Whether direct messages are disabled instance-wide'),
		friends_disabled: z
			.boolean()
			.optional()
			.describe('Whether friend requests are disabled instance-wide. Missing means direct_messages_disabled decides'),
		personal_notes_enabled: z
			.boolean()
			.optional()
			.describe('Whether people can use personal notes. Missing means they can'),
		guild_create_access: z
			.boolean()
			.describe(
				'Whether every account can create communities. When false, only admins and accounts granted the feature_guild_create limit can',
			),
	})
	.describe('Community topology and direct-message policy for this instance');
export type InstanceCommunity = z.infer<typeof InstanceCommunitySchema>;

export const InstanceServicesSchema = z
	.object({
		gif_enabled: z.boolean().describe('Whether the GIF picker is enabled for this instance'),
		youtube_enabled: z.boolean().describe('Whether YouTube link enrichment is enabled for this instance'),
		bluesky_enabled: z.boolean().describe('Whether Bluesky profile connections are enabled for this instance'),
	})
	.describe('Optional third-party service integrations enabled for this instance');
export type InstanceServices = z.infer<typeof InstanceServicesSchema>;

export const InstancePushDeliveryModeSchema = z.enum(['direct', 'external_relay', 'hybrid']);
export type InstancePushDeliveryMode = z.infer<typeof InstancePushDeliveryModeSchema>;

export const InstancePushSchema = z
	.object({
		public_vapid_key: z.string().nullable().describe('VAPID public key for web push notifications'),
		delivery_mode: InstancePushDeliveryModeSchema.optional().describe('How push notifications reach devices'),
		event_resolution_path: z
			.string()
			.optional()
			.describe('API path clients call to resolve the payload of a push event'),
		external_relay_url: z.string().nullable().optional().describe('URL of the external push relay, if any'),
		relay_signing_key_hashes: z
			.array(z.string())
			.nullable()
			.optional()
			.describe('Hashes of the keys the external push relay signs with'),
	})
	.describe('Push notification configuration');
export type InstancePush = z.infer<typeof InstancePushSchema>;

export const InstanceAgePolicyActionSchema = z.enum(['restrict', 'block']);
export type InstanceAgePolicyAction = z.infer<typeof InstanceAgePolicyActionSchema>;

export const InstanceAgePolicyGeoSchema = z
	.object({
		country_code: z.string().describe('ISO 3166-1 alpha-2 country code'),
		region_code: z.string().nullable().describe('ISO 3166-2 subdivision code, or null for the whole country'),
		action: InstanceAgePolicyActionSchema.describe('Whether the region restricts or blocks access'),
	})
	.describe('Age policy for one region');
export type InstanceAgePolicyGeo = z.infer<typeof InstanceAgePolicyGeoSchema>;

export const InstanceAgePolicySchema = z
	.object({
		geos: z.array(InstanceAgePolicyGeoSchema).describe('Regions with an age policy'),
	})
	.describe('Regional age policy for this instance');
export type InstanceAgePolicy = z.infer<typeof InstanceAgePolicySchema>;

export const InstanceNoticeSchema = z
	.object({
		id: z.string().describe('Changes whenever the notice text changes, so clients can remember a dismissal'),
		message: z.string().describe('Notice text written by the instance administrator'),
		url: z.string().nullable().describe('Optional link shown with the notice'),
		starts_at: z.iso.datetime().nullable().describe('When clients start showing the notice, or null for now'),
		ends_at: z.iso.datetime().nullable().describe('When clients stop showing the notice, or null for no end'),
	})
	.describe('Notice from the instance administrator shown to everyone, such as planned maintenance');
export type InstanceNotice = z.infer<typeof InstanceNoticeSchema>;

export const InstanceGuildBadgeSchema = z.object({
	id: SnowflakeStringType.describe('The ID of the badge, as listed in badge_ids of a community'),
	name: z.string().describe('The name of the badge'),
	description: z.string().nullable().describe('Text shown under the name when hovering over the badge, or null'),
	asset_url: z.string().describe('URL of the badge image, served as WebP by the media proxy'),
	href: z.string().nullable().describe('URL opened when the badge is clicked, or null'),
});

export type InstanceGuildBadge = z.infer<typeof InstanceGuildBadgeSchema>;

export const WellKnownFluxerResponse = z.object({
	codename: z.string().optional().describe('Protocol generation spoken by this instance'),
	api_code_version: z.number().int().describe('Version of the API server code'),
	endpoints: InstanceEndpointsSchema,
	captcha: InstanceCaptchaSchema,
	features: InstanceFeaturesSchema,
	gif: InstanceGifSchema,
	sso: InstanceSsoSchema,
	registration: InstanceRegistrationSchema,
	community: InstanceCommunitySchema,
	services: InstanceServicesSchema,
	limits: LimitConfigResponse.describe('Limit configuration with rules and trait definitions'),
	push: InstancePushSchema,
	app_public: InstanceAppPublicSchema.describe('Public application configuration for client-side features'),
	age_policy: InstanceAgePolicySchema.optional().describe('Regional age policy for this instance'),
	notice: InstanceNoticeSchema.nullable()
		.optional()
		.describe('Notice from the instance administrator, or null when there is none'),
	domain_migration: DomainMigrationDiscoveryResponse.optional().describe(
		'Web domain migration switch and anonymous rollout, only acted on by official instance clients',
	),
	guild_badges: z
		.array(InstanceGuildBadgeSchema)
		.max(MAX_CUSTOM_BADGES_PER_TYPE)
		.optional()
		.describe('Custom badges that communities on this instance can show next to their name, in display order'),
});

export type WellKnownFluxerResponse = z.infer<typeof WellKnownFluxerResponse>;

export const InstanceAccountIdentityUpdateRequest = z.object({
	mode: AccountIdentityModeSchema.describe('Sign-in method for the new instance'),
	tag_style: TagStyleSchema.optional().describe(
		'How usernames are tagged. Defaults to none. Username sign-in accepts only none',
	),
	setup_token: createStringType(1, 256)
		.nullish()
		.describe('Setup token of a new self-hosted instance. Required until the first admin account exists'),
});

export type InstanceAccountIdentityUpdateRequest = z.infer<typeof InstanceAccountIdentityUpdateRequest>;

export const InstanceSetupTokenCheckRequest = z.object({
	setup_token: createStringType(1, 256).describe('Setup token of a new self-hosted instance'),
});

export type InstanceSetupTokenCheckRequest = z.infer<typeof InstanceSetupTokenCheckRequest>;

export const InstanceAccountIdentityResponse = z.object({
	mode: AccountIdentityModeSchema.describe('Sign-in method now in effect'),
	tag_style: TagStyleSchema.describe('How usernames are tagged'),
});

export type InstanceAccountIdentityResponse = z.infer<typeof InstanceAccountIdentityResponse>;
