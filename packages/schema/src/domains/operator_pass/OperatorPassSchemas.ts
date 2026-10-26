// SPDX-License-Identifier: AGPL-3.0-or-later

import {PurchaseBlockedReasons} from '@fluxer/constants/src/AccountTierConstants';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import {CaptchaProviderSchema} from '@fluxer/schema/src/domains/account/AccountTierSchemas';
import {
	createNamedStringLiteralUnion,
	createStringType,
	SnowflakeStringType,
	SnowflakeType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {DiscriminatorType, UsernameType} from '@fluxer/schema/src/primitives/UserValidators';
import {z} from 'zod';

export const OPERATOR_PASS_PHASES = ['waitlist', 'early_bird', 'standard'] as const;

export type OperatorPassPhase = (typeof OPERATOR_PASS_PHASES)[number];

export const OperatorPassPhaseSchema = createNamedStringLiteralUnion(
	[
		['waitlist', 'WAITLIST', 'Sales have not opened yet, the waitlist is open'],
		['early_bird', 'EARLY_BIRD', 'Sales are open at the early bird price'],
		['standard', 'STANDARD', 'Sales are open at the standard price'],
	],
	'Operator Pass sale phase',
);

export const OPERATOR_PASS_CURRENCIES = ['USD', 'EUR', 'BRL', 'DKK', 'INR', 'NOK', 'PLN', 'SEK', 'TRY'] as const;

export type OperatorPassCurrency = (typeof OPERATOR_PASS_CURRENCIES)[number];

export const OperatorPassCurrencySchema = z.enum(OPERATOR_PASS_CURRENCIES).describe('Operator Pass price currency');

export const OPERATOR_PASS_PRICE_FEEDBACK = ['too_high', 'about_right', 'could_pay_more'] as const;

export type OperatorPassPriceFeedback = (typeof OPERATOR_PASS_PRICE_FEEDBACK)[number];

export const OperatorPassPriceFeedbackSchema = createNamedStringLiteralUnion(
	[
		['too_high', 'TOO_HIGH', 'The early bird price feels too high'],
		['about_right', 'ABOUT_RIGHT', 'The early bird price feels about right'],
		['could_pay_more', 'COULD_PAY_MORE', 'The visitor would pay more than the early bird price'],
	],
	'How the visitor feels about the early bird price they were shown',
);

export const OPERATOR_PASS_SOURCES = ['purchase', 'transfer', 'admin'] as const;

export type OperatorPassSource = (typeof OPERATOR_PASS_SOURCES)[number];

export const OperatorPassHolderSourceSchema = createNamedStringLiteralUnion(
	[
		['purchase', 'PURCHASE', 'Bought the pass'],
		['transfer', 'TRANSFER', 'Received from another holder through Pass it on'],
		['admin', 'ADMIN', 'Granted by staff'],
	],
	'How the Operator Pass was obtained',
);

const OperatorPassPurchaseBlockedReasons = {
	NOT_ON_SALE: 'not_on_sale',
	COUNTRY_UNAVAILABLE: 'country_unavailable',
	...PurchaseBlockedReasons,
	PURCHASE_DISABLED: 'purchase_disabled',
	PAYMENT_DISPUTE_OPEN: 'payment_dispute_open',
	REMOVED: 'removed',
} as const;

export type OperatorPassPurchaseBlockedReason = ValueOf<typeof OperatorPassPurchaseBlockedReasons>;

export const OperatorPassPurchaseBlockedReasonSchema = createNamedStringLiteralUnion(
	[
		[OperatorPassPurchaseBlockedReasons.NOT_ON_SALE, 'NOT_ON_SALE', 'Sales have not opened yet'],
		[
			OperatorPassPurchaseBlockedReasons.COUNTRY_UNAVAILABLE,
			'COUNTRY_UNAVAILABLE',
			'The pass is not on sale in the country of the buyer right now',
		],
		[PurchaseBlockedReasons.EMAIL_UNVERIFIED, 'EMAIL_UNVERIFIED', 'The account email is not verified'],
		[PurchaseBlockedReasons.UNCLAIMED, 'UNCLAIMED', 'The account is not claimed'],
		[
			OperatorPassPurchaseBlockedReasons.PURCHASE_DISABLED,
			'PURCHASE_DISABLED',
			'Purchases are disabled for the account',
		],
		[
			OperatorPassPurchaseBlockedReasons.PAYMENT_DISPUTE_OPEN,
			'PAYMENT_DISPUTE_OPEN',
			'A payment dispute the account made is open, purchases come back when it closes',
		],
		[
			OperatorPassPurchaseBlockedReasons.REMOVED,
			'REMOVED',
			'The pass was removed from the account permanently under the Operator Pass terms',
		],
	],
	'Why the current account cannot buy passes',
);

export const OPERATOR_PASS_WALL_NAME_MAX_LENGTH = 32;

export const OPERATOR_PASS_MAX_QUANTITY = 100;

export const OPERATOR_PASS_WAIVER_CONSENT_VERSION = 'eu-2026-10-16b';

export const OPERATOR_PASS_COUNTRY_CONSENT_PREFIX = 'country-';
export const OPERATOR_PASS_COUNTRY_CONSENT_VERSION = `${OPERATOR_PASS_COUNTRY_CONSENT_PREFIX}2026-10-16b`;

export const OperatorPassWallNameType = z
	.string()
	.trim()
	.min(1)
	.max(OPERATOR_PASS_WALL_NAME_MAX_LENGTH)
	.regex(/^[^\r\n]*$/u, 'The name cannot contain line breaks')
	.describe("Name for the Operators' wall, 1 to 32 characters without line breaks. Any display name or nickname works");

export const OperatorPassListingStatusSchema = createNamedStringLiteralUnion(
	[
		['listed', 'LISTED', "The holder is on the Operators' wall"],
		['not_listed', 'NOT_LISTED', 'The holder chose not to be listed or has not chosen a name'],
	],
	"Whether the holder is on the Operators' wall",
);

export const OperatorPassWallDeclineReasonSchema = createNamedStringLiteralUnion(
	[
		['guidelines', 'GUIDELINES', 'It breaks the community guidelines'],
		['impersonation', 'IMPERSONATION', 'It pretends to be someone else'],
		['rights', 'RIGHTS', 'It uses a name or image the holder has no right to use'],
		['age_suitability', 'AGE_SUITABILITY', 'It is not suitable for the youngest people allowed to use Fluxer'],
	],
	"Why staff declined a name or avatar on the Operators' wall, under section 2.1 of the Operator Pass terms",
);

const OperatorPassCountryCode = createStringType(2, 2).describe(
	'Two-letter country code used to pick the price currency. The request GeoIP country is used when omitted.',
);

const OperatorPassEmail = z.string().trim().toLowerCase().check(z.email()).max(254);

const OperatorPassLinkToken = z.string().trim().min(1).max(256);

export const OperatorPassStateQuery = z.object({
	country_code: OperatorPassCountryCode.optional(),
});

export type OperatorPassStateQuery = z.infer<typeof OperatorPassStateQuery>;

export const OperatorPassHolderResponse = z.object({
	since: z.iso.datetime().describe('When the account got its first pass'),
	source: OperatorPassHolderSourceSchema,
	count: z.number().int().describe('How many passes the account holds now'),
	recognition_count: z
		.number()
		.int()
		.min(0)
		.optional()
		.describe('The public count of valid held passes and purchases passed on. It is zero without active benefits.'),
	wall_chosen: z
		.boolean()
		.describe(
			"Whether the account already chose to be listed or not listed on the Operators' wall. When false, checkout and accepting an offer ask for the choice",
		),
});

export const OperatorPassWithdrawalRuleResponse = z.object({
	country: z.string().describe('Two-letter code of the country whose withdrawal right applies'),
	days: z.number().int().describe('Days after payment the buyer can withdraw for a full refund'),
});

export type OperatorPassWithdrawalRuleResponse = z.infer<typeof OperatorPassWithdrawalRuleResponse>;

export type OperatorPassHolderResponse = z.infer<typeof OperatorPassHolderResponse>;

export const OperatorPassDonationOptionsResponse = z.object({
	currency: OperatorPassCurrencySchema.describe('Currency of the optional monthly donation'),
	presets_minor: z.array(z.number().int()).describe('Suggested monthly donation amounts in minor units'),
	min_minor: z.number().int().describe('Smallest monthly donation in minor units'),
	max_minor: z.number().int().describe('Largest monthly donation in minor units'),
});

export type OperatorPassDonationOptionsResponse = z.infer<typeof OperatorPassDonationOptionsResponse>;

export const OperatorPassStateResponse = z.object({
	phase: OperatorPassPhaseSchema,
	now: z.iso.datetime().describe('The server time the state was computed at'),
	sale_opens_at: z.iso.datetime().nullable().describe('When sales open, null while no sale date is set'),
	price_rises_at: z.iso
		.datetime()
		.nullable()
		.describe('When the early bird price ends, null while no sale date is set'),
	currency: OperatorPassCurrencySchema,
	price_minor: z.number().int().describe('Current price of one pass in minor units'),
	max_quantity: z
		.number()
		.int()
		.describe('Most passes one checkout can buy, FLUXER_OPERATOR_PASS_MAX_QUANTITY, 25 by default'),
	withdrawal_rule: OperatorPassWithdrawalRuleResponse.nullable().describe(
		'A withdrawal right that cannot be waived, for buyers in a country that has one, null otherwise. Checkout shows the matching box text then',
	),
	quote: z.string().describe('Signed price quote to send back with checkout'),
	holder: OperatorPassHolderResponse.nullable().describe('The passes held by the current account, null otherwise'),
	purchase_blocked_reason: OperatorPassPurchaseBlockedReasonSchema.nullable().describe(
		'Why the current account cannot buy passes, null when it can',
	),
	donation: OperatorPassDonationOptionsResponse,
});

export type OperatorPassStateResponse = z.infer<typeof OperatorPassStateResponse>;

export const OperatorPassCheckoutRequest = z.object({
	quantity: z
		.number()
		.int()
		.min(1)
		.max(OPERATOR_PASS_MAX_QUANTITY)
		.default(1)
		.describe('How many passes to buy, all of them are delivered to the account of the buyer'),
	country_code: OperatorPassCountryCode.optional(),
	quote: z.string().max(2048).optional().describe('Quote from the state response the buyer saw'),
	donation_amount_minor: z
		.number()
		.int()
		.positive()
		.nullable()
		.optional()
		.describe('Optional monthly donation in minor units of the pass currency'),
	waiver_consent: z
		.boolean()
		.optional()
		.describe(
			'Whether the buyer ticked the required box asking for delivery as soon as they pay and acknowledging that the bonus is not part of what they pay for. Outside a withdrawal_rule country the box also acknowledges losing the right of withdrawal once the passes are delivered. Must be true',
		),
	wall_name: OperatorPassWallNameType.nullish().describe(
		"Name for the Operators' wall, required unless wall_opt_out is true. Keeps the current name when the account is already listed",
	),
	wall_opt_out: z.boolean().optional().describe("Whether the buyer chose not to be listed on the Operators' wall"),
	consent_locale: z
		.string()
		.max(16)
		.optional()
		.describe('Locale the client used to show the box texts. The purchase records the box text in this locale'),
	age_confirmed: z
		.boolean()
		.optional()
		.describe(
			"Whether the buyer ticked \"I'm 18 or older, or have a parent's or guardian's permission.\" Must be true when quantity is above 1",
		),
});

export type OperatorPassCheckoutRequest = z.infer<typeof OperatorPassCheckoutRequest>;

export const OperatorPassCheckoutResponse = z.object({
	url: z.url().describe('Stripe checkout URL to redirect the buyer to'),
});

export type OperatorPassCheckoutResponse = z.infer<typeof OperatorPassCheckoutResponse>;

export const OperatorPassAltchaChallenge = z.object({
	parameters: z.looseObject({
		algorithm: z.string(),
		nonce: z.string(),
		salt: z.string(),
		cost: z.number().int(),
		keyLength: z.number().int(),
		keyPrefix: z.string(),
		keySignature: z.string(),
		expiresAt: z.number().int(),
	}),
	signature: z.string(),
});

export const OperatorPassWaitlistChallengeResponse = z.object({
	captcha_provider: CaptchaProviderSchema.describe('Captcha provider the waitlist expects'),
	altcha_challenge: OperatorPassAltchaChallenge.optional().describe(
		'ALTCHA challenge to solve when the provider is altcha',
	),
});

export type OperatorPassWaitlistChallengeResponse = z.infer<typeof OperatorPassWaitlistChallengeResponse>;

export const OperatorPassWaitlistRequest = z.strictObject({
	email: OperatorPassEmail.describe('Email address to put on the waitlist'),
	country_code: OperatorPassCountryCode.optional(),
	price_feedback: OperatorPassPriceFeedbackSchema,
	currency: OperatorPassCurrencySchema.describe('Currency of the early bird price the visitor was shown'),
});

export type OperatorPassWaitlistRequest = z.infer<typeof OperatorPassWaitlistRequest>;

export const OperatorPassWaitlistResponse = z.object({
	status: z.literal('pending').describe('A confirmation email is on its way when the address can join'),
	followup_token: z
		.string()
		.describe(
			'Signed token for the follow-up question, valid for 30 minutes. Every signup gets one of the same shape, and only one issued for a new waitlist entry stores an answer',
		),
});

export type OperatorPassWaitlistResponse = z.infer<typeof OperatorPassWaitlistResponse>;

export const OPERATOR_PASS_WAITLIST_FOLLOWUP_KEYS = [
	'wall_spot',
	'badge',
	'operators_channel',
	'early_testing',
	'yearly_list',
	'instance_mark',
	'other',
] as const;

export type OperatorPassWaitlistFollowupKey = (typeof OPERATOR_PASS_WAITLIST_FOLLOWUP_KEYS)[number];

export const OperatorPassWaitlistFollowupKeySchema = createNamedStringLiteralUnion(
	[
		['wall_spot', 'WALL_SPOT', "A spot on the Operators' wall"],
		['badge', 'BADGE', 'The Operator badge'],
		['operators_channel', 'OPERATORS_CHANNEL', 'The Operators channel'],
		['early_testing', 'EARLY_TESTING', 'Testing new features early'],
		['yearly_list', 'YEARLY_LIST', 'A name on the yearly Operators list'],
		['instance_mark', 'INSTANCE_MARK', 'An Operator mark on your instance'],
		['other', 'OTHER', "Something else, in the visitor's own words"],
	],
	'What is most important to a waitlist visitor about the Operator Pass',
);

export const OPERATOR_PASS_WAITLIST_ABOUT_KEYS = [
	'self_hosting_new',
	'self_hosting_experienced',
	'bots_new',
	'bots_experienced',
	'support_fluxer',
	'community_owner',
] as const;

export type OperatorPassWaitlistAboutKey = (typeof OPERATOR_PASS_WAITLIST_ABOUT_KEYS)[number];

export const OperatorPassWaitlistAboutKeySchema = createNamedStringLiteralUnion(
	[
		['self_hosting_new', 'SELF_HOSTING_NEW', "I'm new to self-hosting"],
		['self_hosting_experienced', 'SELF_HOSTING_EXPERIENCED', "I've self-hosted before"],
		['bots_new', 'BOTS_NEW', "I'm new to building bots"],
		['bots_experienced', 'BOTS_EXPERIENCED', "I've built bots before"],
		['support_fluxer', 'SUPPORT_FLUXER', 'I mainly want to support Fluxer'],
		['community_owner', 'COMMUNITY_OWNER', 'I run a community on Fluxer'],
	],
	'A description a waitlist visitor picked for themselves',
);

export const OPERATOR_PASS_WAITLIST_FOLLOWUP_OTHER_TEXT_MAX = 500;

export const OperatorPassWaitlistFollowupRequest = z.strictObject({
	token: OperatorPassLinkToken.describe('followup_token from the waitlist signup response'),
	picks: z
		.array(z.string().max(64))
		.max(32)
		.describe(
			`What is most important, 0 to 7 unique keys out of ${OPERATOR_PASS_WAITLIST_FOLLOWUP_KEYS.join(', ')}. An unknown or repeated key returns OPERATOR_PASS_WAITLIST_FOLLOWUP_INVALID`,
		),
	about: z
		.array(z.string().max(64))
		.max(32)
		.optional()
		.describe(
			`Which descriptions fit the visitor, 0 to 6 unique keys out of ${OPERATOR_PASS_WAITLIST_ABOUT_KEYS.join(', ')}. When omitted, the descriptions stored before stay as they are, and an empty array clears them. An unknown or repeated key returns OPERATOR_PASS_WAITLIST_FOLLOWUP_INVALID`,
		),
	other_text: z
		.string()
		.max(4000)
		.nullish()
		.describe(
			'Plain text answer for other, only with other in picks. Trimmed, then 1 to 500 characters, without control characters other than line breaks and tabs',
		),
});

export type OperatorPassWaitlistFollowupRequest = z.infer<typeof OperatorPassWaitlistFollowupRequest>;

export const OperatorPassWaitlistConfirmRequest = z.object({
	token: OperatorPassLinkToken.describe('Token from the confirmation email'),
});

export type OperatorPassWaitlistConfirmRequest = z.infer<typeof OperatorPassWaitlistConfirmRequest>;

export const OperatorPassWaitlistConfirmResponse = z.object({
	status: z.literal('confirmed'),
});

export type OperatorPassWaitlistConfirmResponse = z.infer<typeof OperatorPassWaitlistConfirmResponse>;

export const OperatorPassWaitlistUnsubscribeQuery = z.object({
	token: OperatorPassLinkToken.optional().describe('Unsubscribe token from the email'),
});

export type OperatorPassWaitlistUnsubscribeQuery = z.infer<typeof OperatorPassWaitlistUnsubscribeQuery>;

export const OperatorPassWaitlistUnsubscribeRequest = z.object({
	token: OperatorPassLinkToken.optional().describe('Unsubscribe token from the email'),
});

export type OperatorPassWaitlistUnsubscribeRequest = z.infer<typeof OperatorPassWaitlistUnsubscribeRequest>;

export const OperatorPassWaitlistUnsubscribeResponse = z.object({
	status: z.literal('unsubscribed'),
});

export type OperatorPassWaitlistUnsubscribeResponse = z.infer<typeof OperatorPassWaitlistUnsubscribeResponse>;

export const OperatorPassPriceChangedErrorData = z.object({
	phase: OperatorPassPhaseSchema,
	currency: OperatorPassCurrencySchema,
	price_minor: z.number().int(),
	quote: z.string(),
});

export type OperatorPassPriceChangedErrorData = z.infer<typeof OperatorPassPriceChangedErrorData>;

const OPERATOR_PASS_PURCHASE_STATUSES = [
	'pending',
	'completed',
	'withdrawn',
	'refunded',
	'disputed',
	'dispute_lost',
	'duplicate_refunded',
] as const;

export type OperatorPassPurchaseStatus = (typeof OPERATOR_PASS_PURCHASE_STATUSES)[number];

export const OperatorPassPurchaseStatusSchema = createNamedStringLiteralUnion(
	[
		['pending', 'PENDING', 'Checkout was started and the payment has not settled'],
		['completed', 'COMPLETED', 'Paid, and the passes were delivered'],
		['withdrawn', 'WITHDRAWN', 'The buyer withdrew and the payment was refunded'],
		['refunded', 'REFUNDED', 'The payment was refunded'],
		['disputed', 'DISPUTED', 'A payment dispute is open and the passes are paused'],
		['dispute_lost', 'DISPUTE_LOST', 'A payment dispute returned the money to the buyer'],
		['duplicate_refunded', 'DUPLICATE_REFUNDED', 'The account had lost the pass for good and the payment was refunded'],
	],
	'Operator Pass purchase status',
);

export const OperatorPassPurchaseResponse = z.object({
	id: z.string().describe('Identifier of the purchase, the Stripe checkout session ID'),
	status: OperatorPassPurchaseStatusSchema,
	quantity: z.number().int().describe('How many passes the purchase bought'),
	amount_minor: z.number().int().describe('Price paid for the passes in minor units, tax included'),
	currency: OperatorPassCurrencySchema,
	purchased_at: z.iso.datetime().nullable().describe('When the payment settled, null while pending'),
	delivered_at: z.iso.datetime().nullable().describe('When the passes were delivered, null while pending'),
	waiver_consent_at: z.iso.datetime().nullable().describe('When the buyer ticked the box at checkout'),
	withdrawal_country: z
		.string()
		.nullable()
		.describe('Country whose withdrawal right applies to the purchase, null when the waiver applies'),
	withdrawal_ends_at: z.iso
		.datetime()
		.nullable()
		.describe(
			'When the right of withdrawal ends, null when the purchase has none: delivered passes outside a withdrawal_country. Under a country rule this is the end of withdrawal_ends_on in America/Rio_Branco',
		),
	withdrawal_ends_on: z.iso
		.date()
		.nullable()
		.optional()
		.describe(
			'The last day of the cancel window under a country rule, as YYYY-MM-DD in America/Rio_Branco. Show it as is, without converting time zones',
		),
	withdrawn_at: z.iso.datetime().nullable().describe('When the buyer last withdrew'),
	has_donation: z.boolean().describe('Whether a monthly donation was added at checkout'),
	can_withdraw: z
		.boolean()
		.describe('Whether the whole purchase can be withdrawn now, only while paid passes are not delivered yet'),
});

export type OperatorPassPurchaseResponse = z.infer<typeof OperatorPassPurchaseResponse>;

export const OperatorPassPurchaseListResponse = z.array(OperatorPassPurchaseResponse);

export type OperatorPassPurchaseListResponse = z.infer<typeof OperatorPassPurchaseListResponse>;

export const OperatorPassPurchaseIdParam = z.object({
	purchase_id: z.string().trim().min(1).max(256).describe('The purchase ID'),
});

export type OperatorPassPurchaseIdParam = z.infer<typeof OperatorPassPurchaseIdParam>;

export const OperatorPassWithdrawRequest = z.object({
	name: z.string().trim().min(1).max(200).describe('Name of the buyer who withdraws'),
});

export type OperatorPassWithdrawRequest = z.infer<typeof OperatorPassWithdrawRequest>;

export const OperatorPassUnitStatusSchema = createNamedStringLiteralUnion(
	[
		['held', 'HELD', 'The account holds the pass'],
		['withdrawn', 'WITHDRAWN', 'The buyer withdrew and the payment for the pass was refunded'],
		['revoked', 'REVOKED', 'The pass ended because its payment was refunded or reversed, or staff removed it'],
	],
	'Operator Pass status',
);

export const OperatorPassOfferStatusSchema = createNamedStringLiteralUnion(
	[
		['pending', 'PENDING', 'Waiting for the recipient to accept or decline'],
		['accepted', 'ACCEPTED', 'The recipient accepted, the pass moves at arrives_at'],
		['completed', 'COMPLETED', 'The pass moved to the recipient'],
		['declined', 'DECLINED', 'The recipient declined'],
		['withdrawn', 'WITHDRAWN', 'The sender withdrew the offer'],
		['cancelled', 'CANCELLED', 'The offer ended because the pass ended or its payment changed'],
	],
	'Pass it on offer status',
);

export const OperatorPassOfferUserResponse = z.object({
	id: SnowflakeStringType.describe('ID of the user'),
	username: z.string().describe('Username of the user'),
	global_name: z.string().nullable().describe('Display name of the user'),
	avatar: z.string().nullable().describe('Avatar hash of the user'),
});

export const OperatorPassOfferResponse = z.object({
	id: z.string().describe('ID of the offer'),
	pass_id: z.string().describe('ID of the pass on offer'),
	status: OperatorPassOfferStatusSchema,
	sender: OperatorPassOfferUserResponse.nullable().describe('Who passes the pass on, null when the account is gone'),
	recipient: OperatorPassOfferUserResponse.nullable().describe('Who gets the pass, null when the account is gone'),
	created_at: z.iso.datetime().describe('When the sender made the offer'),
	accepted_at: z.iso.datetime().nullable().describe('When the recipient accepted'),
	arrives_at: z.iso
		.datetime()
		.describe('The earliest time the pass can move to the recipient once accepted, when the transfer lock ends'),
	ended_at: z.iso.datetime().nullable().describe('When the offer completed or ended'),
});

export type OperatorPassOfferResponse = z.infer<typeof OperatorPassOfferResponse>;

export const OperatorPassUnitResponse = z.object({
	id: z.string().describe('ID of the pass'),
	source: z.enum(OPERATOR_PASS_SOURCES).describe('How the account got the pass'),
	status: OperatorPassUnitStatusSchema,
	purchase_id: z.string().nullable().describe('The purchase that paid for the pass, null for a staff grant'),
	delivered_at: z.iso.datetime().describe('When the pass reached this account'),
	paused: z.boolean().describe('Whether the pass is paused while a payment dispute is open'),
	can_pass_on: z.boolean().describe('Whether the pass can be passed on. Received and staff granted passes cannot'),
	transfer_lock_ends_at: z.iso
		.datetime()
		.nullable()
		.describe('The earliest time a passed on pass can move to the recipient, null when it cannot be passed on'),
	can_withdraw: z.boolean().describe('Whether the withdraw button for this pass is available now'),
	withdrawal_ends_at: z.iso
		.datetime()
		.nullable()
		.describe(
			'When the cancel window for this pass closes, null when it has none. Under a country rule this is the end of withdrawal_ends_on in America/Rio_Branco',
		),
	withdrawal_ends_on: z.iso
		.date()
		.nullable()
		.optional()
		.describe(
			'The last day of the cancel window under a country rule, as YYYY-MM-DD in America/Rio_Branco. Show it as is, without converting time zones',
		),
	offer: OperatorPassOfferResponse.nullable().describe('The open offer for this pass, null when none'),
	received_from: OperatorPassOfferUserResponse.nullable()
		.optional()
		.describe('Who passed this pass on to the account, null when the account bought it or that account is gone'),
});

export type OperatorPassUnitResponse = z.infer<typeof OperatorPassUnitResponse>;

export const OperatorPassPassesResponse = z.object({
	count: z.number().int().describe('How many passes the account holds now'),
	recognition_count: z
		.number()
		.int()
		.min(0)
		.optional()
		.describe('The public count of valid held passes and purchases passed on. It is zero without active benefits.'),
	passes: z.array(OperatorPassUnitResponse).describe('Passes the account holds, oldest first'),
	incoming_offers: z.array(OperatorPassOfferResponse).describe('Open offers to the account'),
	outgoing_offers: z.array(OperatorPassOfferResponse).describe('Open offers the account made'),
	passed_on: z
		.array(OperatorPassOfferResponse)
		.optional()
		.describe('Completed offers the account made, the passes it passed on, oldest first'),
});

export type OperatorPassPassesResponse = z.infer<typeof OperatorPassPassesResponse>;

export const OperatorPassPassIdParam = z.object({
	pass_id: z.string().trim().min(1).max(256).describe('The pass ID'),
});

export type OperatorPassPassIdParam = z.infer<typeof OperatorPassPassIdParam>;

export const OperatorPassOfferIdParam = z.object({
	offer_id: z.string().trim().min(1).max(64).describe('The offer ID'),
});

export type OperatorPassOfferIdParam = z.infer<typeof OperatorPassOfferIdParam>;

export const OperatorPassOfferCreateRequest = z.object({
	recipient_id: SnowflakeType.optional().describe(
		'ID of the user to pass the pass on to. Send this or username and discriminator',
	),
	username: UsernameType.optional().describe(
		'Username of the user to pass the pass on to, as in a friend request by tag. Send it with discriminator',
	),
	discriminator: DiscriminatorType.optional().describe('Discriminator tag of that user'),
	confirm_last_pass: z
		.boolean()
		.optional()
		.describe(
			'Must be true when this is the last pass on the account, because passing it on removes the badge and the wall listing',
		),
});

export type OperatorPassOfferCreateRequest = z.infer<typeof OperatorPassOfferCreateRequest>;

export const OperatorPassOfferAcceptRequest = z.object({
	accept_terms: z
		.boolean()
		.describe('Whether the recipient accepted the Operator Pass terms with the box next to Accept. Must be true'),
	wall_name: OperatorPassWallNameType.nullish().describe(
		"Name for the Operators' wall, required unless wall_opt_out is true. Keeps the current name when the account is already listed",
	),
	wall_opt_out: z.boolean().optional().describe("Whether the recipient chose not to be listed on the Operators' wall"),
});

export type OperatorPassOfferAcceptRequest = z.infer<typeof OperatorPassOfferAcceptRequest>;

export const OperatorPassWallDisplayStateSchema = createNamedStringLiteralUnion(
	[
		['live', 'LIVE', 'The wall shows the approved name and avatar'],
		[
			'fallback',
			'FALLBACK',
			'The wall shows the username, or Operator, with the default avatar until a name is approved',
		],
		['hidden', 'HIDDEN', 'Staff hid the listing for now'],
		['removed', 'REMOVED', 'The listing is off the wall under section 8 of the Operator Pass terms'],
	],
	"What the Operators' wall shows for the listing",
);

export const OperatorPassWallReviewStateSchema = createNamedStringLiteralUnion(
	[
		['pending', 'PENDING', 'A new name or avatar waits for review, the wall keeps the last approved version'],
		['approved', 'APPROVED', 'The latest name and avatar are approved'],
		['declined', 'DECLINED', 'Staff declined the latest name or avatar, see decline_reason'],
	],
	'Review state of the latest name and avatar',
);

export const OperatorPassListingResponse = z.object({
	wall_name: z.string().nullable().describe('The latest name the holder submitted, null when not listed'),
	wall_opt_out: z.boolean().describe('Whether the holder chose not to be listed'),
	keep_as_is: z
		.boolean()
		.describe('Whether profile name and avatar changes stay off the wall instead of becoming new pending changes'),
	release_list_opt_in: z
		.boolean()
		.describe('Whether the holder chose to have their wall name in every yearly The Operators of list. Off by default'),
	status: OperatorPassListingStatusSchema,
	display_state: OperatorPassWallDisplayStateSchema.nullable().describe('What the wall shows, null when not listed'),
	review_state: OperatorPassWallReviewStateSchema.nullable().describe(
		'Review state, null before anything was submitted',
	),
	shown_name: z.string().nullable().describe('The name the wall shows right now, null when not listed'),
	shown_avatar_url: z.url().nullable().describe('The avatar the wall shows right now, null for the default avatar'),
	pending_name: z.string().nullable().describe('The name waiting for review, null when nothing is pending'),
	pending_since: z.iso.datetime().nullable().describe('When the pending change was submitted'),
	review_due_at: z.iso.datetime().nullable().describe('When the review is due, 72 hours after submission'),
	decline_reason: OperatorPassWallDeclineReasonSchema.nullable().describe(
		'Why staff declined the latest change, null otherwise',
	),
	decline_note: z.string().nullable().describe('The facts staff gave for the decline, null otherwise'),
	declined_name: z.string().nullable().describe('The declined name, null when none is'),
	listed_at: z.iso.datetime().nullable().describe("When the listing went on the Operators' wall, null unless listed"),
	release_year: z
		.number()
		.int()
		.nullable()
		.describe(
			'The year of the latest published The Operators of list the name is in, null when none or after an erasure',
		),
	has_wall: z.boolean().describe("Whether the account holds the pass, so it shows on the Operators' wall"),
	pass_count: z
		.number()
		.int()
		.describe(
			'Public count of valid held passes and purchases passed on, shown to the holder even when hidden publicly',
		),
});

export type OperatorPassListingResponse = z.infer<typeof OperatorPassListingResponse>;

export const OperatorPassListingUpdateRequest = z.object({
	wall_name: OperatorPassWallNameType.nullish().describe('The name to list, required unless wall_opt_out is true'),
	wall_opt_out: z.boolean().describe("Whether to stay off the Operators' wall"),
	keep_as_is: z
		.boolean()
		.optional()
		.describe('Whether profile name and avatar changes stay off the wall, unchanged when omitted'),
	release_list_opt_in: z
		.boolean()
		.optional()
		.describe('Whether to put the wall name in every yearly The Operators of list, unchanged when omitted'),
});

export type OperatorPassListingUpdateRequest = z.infer<typeof OperatorPassListingUpdateRequest>;

export const OperatorPassWallOperatorResponse = z.object({
	id: z.string().describe('Stable public ID of the entry, not the user ID'),
	name: z.string().describe('The name shown: the approved name, or the username or Operator as fallback'),
	avatar: z.string().nullable().describe('SHA-256 of the approved avatar snapshot, null for the default avatar'),
	avatar_url: z
		.url()
		.nullable()
		.describe('The approved avatar snapshot at the largest size, null for the default avatar'),
	count: z
		.number()
		.int()
		.optional()
		.describe('How many passes the Operator holds now. Left out when the Operator hides the number'),
	tier: z
		.number()
		.int()
		.describe(
			'Size tier on the wall from the number of passes held, 1 for the smallest. The thresholds come from FLUXER_OPERATOR_PASS_WALL_TIERS, by default 1, 2 to 4, 5 to 9 and 10 or more',
		),
	business: z
		.literal(true)
		.optional()
		.describe(
			'Present on a business listing, which shows a company name and logo at one fixed size after every listing of a person, ordered by first pass date',
		),
});

export const OperatorPassWallResponse = z.object({
	version: z.number().int().describe('The wall version, it rises with every published change'),
	operators: z
		.array(OperatorPassWallOperatorResponse)
		.describe(
			'Every Operator on the wall in display order: the highest tier first, then by when the Operator got their first pass, with business listings last in order of their first pass',
		),
});

export type OperatorPassWallResponse = z.infer<typeof OperatorPassWallResponse>;

export const OperatorPassReleaseResponse = z.object({
	year: z.number().int().describe('The year of the list, as in The Operators of 2026'),
	tag: z.string().describe('The tag of the Fluxer release the list shipped in'),
	released_at: z.iso.datetime().describe('When the release shipped'),
	names: z.array(z.string()).describe('Names in the list, sorted'),
});

export type OperatorPassReleaseResponse = z.infer<typeof OperatorPassReleaseResponse>;

export const OperatorPassLatestReleaseResponse = z.object({
	release: OperatorPassReleaseResponse.nullable().describe('The latest published list, null before the first one'),
});

export type OperatorPassLatestReleaseResponse = z.infer<typeof OperatorPassLatestReleaseResponse>;
