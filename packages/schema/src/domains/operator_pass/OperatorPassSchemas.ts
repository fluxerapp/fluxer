// SPDX-License-Identifier: AGPL-3.0-or-later

import {PurchaseBlockedReasons} from '@fluxer/constants/src/AccountTierConstants';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import {
	countOperatorPassCertificateNameCharacters,
	OPERATOR_PASS_CERTIFICATE_NAME_MAX_CODE_UNITS,
	OPERATOR_PASS_CERTIFICATE_NAME_MAX_LENGTH,
	OPERATOR_PASS_WALL_NAME_MAX_LENGTH,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassNames';
import {OPERATOR_PASS_WALL_QUOTE_MAX_CODE_UNITS} from '@fluxer/schema/src/domains/operator_pass/OperatorPassWallQuote';
import {CnpjSchema} from '@fluxer/schema/src/domains/premium/Cnpj';
import {
	createNamedStringLiteralUnion,
	createStringType,
	SnowflakeStringType,
	SnowflakeType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {DiscriminatorType, UsernameType} from '@fluxer/schema/src/primitives/UserValidators';
import {z} from 'zod';

export {
	OPERATOR_PASS_CERTIFICATE_NAME_MAX_CODE_UNITS,
	OPERATOR_PASS_CERTIFICATE_NAME_MAX_LENGTH,
	OPERATOR_PASS_WALL_NAME_MAX_LENGTH,
	truncateOperatorPassCertificateName,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassNames';

export const OPERATOR_PASS_PHASES = ['early_bird', 'standard'] as const;

export type OperatorPassPhase = (typeof OPERATOR_PASS_PHASES)[number];

export const OperatorPassPhaseSchema = createNamedStringLiteralUnion(
	[
		['early_bird', 'EARLY_BIRD', 'Sales are open at the early bird price'],
		['standard', 'STANDARD', 'Sales are open at the standard price'],
	],
	'Operator Pass sale phase',
);

export const OPERATOR_PASS_CURRENCIES = ['USD', 'EUR', 'BRL', 'DKK', 'INR', 'NOK', 'PLN', 'SEK', 'TRY'] as const;

export type OperatorPassCurrency = (typeof OPERATOR_PASS_CURRENCIES)[number];

export const OperatorPassCurrencySchema = z.enum(OPERATOR_PASS_CURRENCIES).describe('Operator Pass price currency');

export const OPERATOR_PASS_SOURCES = ['purchase', 'transfer', 'visionary', 'admin'] as const;

export type OperatorPassSource = (typeof OPERATOR_PASS_SOURCES)[number];

export const OperatorPassHolderSourceSchema = createNamedStringLiteralUnion(
	[
		['purchase', 'PURCHASE', 'Bought the pass'],
		['transfer', 'TRANSFER', 'Received from another holder through Pass it on'],
		['visionary', 'VISIONARY', 'Came with Visionary and was turned on by the account'],
		['admin', 'ADMIN', 'Granted by staff'],
	],
	'How the Operator Pass was obtained',
);

const OperatorPassPurchaseBlockedReasons = {
	...PurchaseBlockedReasons,
	PURCHASE_DISABLED: 'purchase_disabled',
	PAYMENT_DISPUTE_OPEN: 'payment_dispute_open',
	EMAIL_UNDELIVERABLE: 'email_undeliverable',
	REMOVED: 'removed',
} as const;

export type OperatorPassPurchaseBlockedReason = ValueOf<typeof OperatorPassPurchaseBlockedReasons>;

export const OperatorPassPurchaseBlockedReasonSchema = createNamedStringLiteralUnion(
	[
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
			OperatorPassPurchaseBlockedReasons.EMAIL_UNDELIVERABLE,
			'EMAIL_UNDELIVERABLE',
			'Emails to the account address bounced, so the purchase confirmation cannot be delivered',
		],
		[
			OperatorPassPurchaseBlockedReasons.REMOVED,
			'REMOVED',
			'The pass was removed from the account permanently under the Operator Pass terms',
		],
	],
	'Why the current account cannot buy passes',
);

export const OPERATOR_PASS_MAX_QUANTITY = 100;

export const OPERATOR_PASS_DELIVERY_WAIVER_VERSION = 'delivery-waiver-2026-10-30';

export const OPERATOR_PASS_DELIVERY_COUNTRY_VERSION = 'delivery-country-2026-10-30';

export const OPERATOR_PASS_DELIVERY_BUSINESS_VERSION = 'delivery-business-2026-10-30';

export const OPERATOR_PASS_VISIONARY_BOX_VERSION = 'visionary-terms-2026-10-31';

export const OperatorPassCertificateNameType = z
	.string()
	.normalize('NFC')
	.trim()
	.min(1)
	.max(OPERATOR_PASS_CERTIFICATE_NAME_MAX_CODE_UNITS)
	.refine(
		(name) => countOperatorPassCertificateNameCharacters(name) <= OPERATOR_PASS_CERTIFICATE_NAME_MAX_LENGTH,
		'The name can have at most 48 characters',
	)
	.regex(/^[^\r\n]*$/u, 'The name cannot contain line breaks')
	.describe(
		'Name printed on one page of the Operator certificate, 1 to 48 characters without line breaks. A character is a grapheme cluster, counted after Unicode NFC normalization',
	);

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
	"Why staff took a name or avatar off the Operators' wall or hid a listing, under section 3.1 of the Operator Pass terms",
);

const OperatorPassCountryCode = createStringType(2, 2).describe(
	'Two-letter country code used to pick the price currency. The request GeoIP country is used when omitted.',
);

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
	days: z
		.number()
		.int()
		.describe(
			'Days the buyer can withdraw for a full refund, even after delivery. The period ends on the last of these days counted from the day the certificate is delivered, and withdrawal_ends_at and withdrawal_ends_on on each purchase give the exact deadline',
		),
});

export type OperatorPassWithdrawalRuleResponse = z.infer<typeof OperatorPassWithdrawalRuleResponse>;

export type OperatorPassHolderResponse = z.infer<typeof OperatorPassHolderResponse>;

export const OperatorPassStateResponse = z.object({
	phase: OperatorPassPhaseSchema,
	now: z.iso.datetime().describe('The server time the state was computed at'),
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
		'A country rule that keeps the right of withdrawal after delivery, null when the right ends once the certificate is delivered',
	),
	standard_price_minor: z
		.number()
		.int()
		.nullable()
		.describe('Price of one pass after the early bird phase in minor units, null when no standard price is set'),
	quote: z.string().describe('Signed price quote to send back with checkout'),
	holder: OperatorPassHolderResponse.nullable().describe('The passes held by the current account, null otherwise'),
	purchase_blocked_reason: OperatorPassPurchaseBlockedReasonSchema.nullable().describe(
		'Why the current account cannot buy passes, null when it can',
	),
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
	delivery_consent: z
		.boolean()
		.optional()
		.describe(
			'Whether the buyer ticked the box asking for the certificate to be delivered as soon as they pay. Outside a withdrawal_rule country the box also acknowledges that the right of withdrawal ends once the certificate is delivered. Must be true',
		),
	certificate_names: z
		.array(OperatorPassCertificateNameType)
		.max(OPERATOR_PASS_MAX_QUANTITY)
		.optional()
		.describe(
			'The names printed on the certificate. Give one name to print it on every page, or one name per pass in page order. Left out or empty, every page gets the account display name, or the username when there is none',
		),
	wall_name: OperatorPassWallNameType.nullish().describe(
		"Name for the Operators' wall, required unless wall_opt_out is true. Keeps the current name when the account is already listed",
	),
	wall_opt_out: z.boolean().optional().describe("Whether the buyer chose not to be listed on the Operators' wall"),
	labs_greeting: z
		.boolean()
		.optional()
		.describe(
			'Whether the system user welcomes the buyer by name in #operators in Fluxer Labs once the certificate is delivered. Left out, the choice the account made before stays, and a new account is welcomed',
		),
	consent_locale: z
		.string()
		.max(16)
		.optional()
		.describe('Locale the client used to show the box texts. The purchase records the box text in this locale'),
	age_confirmed: z
		.boolean()
		.optional()
		.describe(
			"Whether the buyer ticked \"I'm 18 or older, or have a parent's or guardian's permission.\" Must be true for every purchase",
		),
	is_business: z
		.boolean()
		.optional()
		.describe(
			'Whether the pass is bought for a business. When true, the payment page requires a billing address for the invoice. When false or omitted, billing address collection is left to Stripe (auto)',
		),
	cnpj: CnpjSchema.optional(),
});

export type OperatorPassCheckoutRequest = z.infer<typeof OperatorPassCheckoutRequest>;

export const OperatorPassCheckoutResponse = z.object({
	url: z.url().describe('Stripe checkout URL to redirect the buyer to'),
	session_id: z
		.string()
		.describe('ID of the Stripe checkout session, to look the purchase up with GET /operator-pass/purchases'),
	expires_at: z.iso.datetime().describe('When the checkout session expires if nobody pays'),
});

export type OperatorPassCheckoutResponse = z.infer<typeof OperatorPassCheckoutResponse>;

export const OperatorPassWaitlistUnsubscribeQuery = z.object({
	token: z.string().trim().min(1).max(256).optional().describe('Unsubscribe token from the email'),
});

export type OperatorPassWaitlistUnsubscribeQuery = z.infer<typeof OperatorPassWaitlistUnsubscribeQuery>;

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
	order_number: z
		.string()
		.nullable()
		.describe('Order number shown on the purchase confirmation and the withdrawal receipt, null when not known'),
	quantity: z.number().int().describe('How many passes the purchase bought'),
	amount_minor: z.number().int().describe('Price paid for the passes in minor units, tax included'),
	currency: OperatorPassCurrencySchema,
	purchased_at: z.iso.datetime().nullable().describe('When the payment settled, null while pending'),
	delivered_at: z.iso.datetime().nullable().describe('When the certificate was delivered, null while pending'),
	waiver_consent_at: z.iso.datetime().nullable().describe('When the buyer ticked the boxes at checkout'),
	withdrawal_country: z
		.string()
		.nullable()
		.describe('Country whose withdrawal right applies to the purchase, null when the waiver applies'),
	withdrawal_ends_at: z.iso
		.datetime()
		.nullable()
		.describe(
			'When the right of withdrawal ends, null when the purchase has none: a delivered certificate outside a withdrawal_country. Under a country rule this is the end of withdrawal_ends_on in America/Rio_Branco',
		),
	withdrawal_ends_on: z.iso
		.date()
		.nullable()
		.optional()
		.describe(
			'The last day the buyer can withdraw, as YYYY-MM-DD in America/Rio_Branco. Show it as is, without converting time zones',
		),
	certificate_available: z
		.boolean()
		.describe('Whether the purchase has at least one certificate page that is not cancelled'),
	withdrawn_at: z.iso.datetime().nullable().describe('When the buyer last withdrew'),
	pages: z
		.array(
			z.object({
				pass_id: z.string().describe('ID of the pass the page belongs to'),
				name: z
					.string()
					.describe(
						'The name on the newest copy of this page, which is the name chosen at checkout unless the page was issued again with a new name',
					),
				certificate_url: z
					.string()
					.nullable()
					.describe(
						'The private link of this page on the marketing site, which shows the page and opens its PDF. Null when the page has no link',
					),
			}),
		)
		.describe('Certificate pages of the purchase that are not cancelled, in page order. Empty before delivery'),
	can_withdraw: z
		.boolean()
		.describe(
			'Whether the withdraw button for the purchase is available now, before delivery or while a country rule keeps the right of withdrawal',
		),
	refundable_minor: z
		.number()
		.int()
		.describe(
			'What withdrawing from the whole purchase now refunds, in minor units, tax included. It leaves out passes already refunded or withdrawn, and is 0 when can_withdraw is false',
		),
	certificate_emailed: z.boolean().describe('Whether the email with the certificate attached was sent'),
	refusal_reason: z
		.enum(['removed', 'names_missing', 'email_undeliverable'])
		.nullable()
		.describe(
			'Why the purchase was refunded before delivery, null when it was not refused. Only names_missing also sends the buyer an email that explains the refusal',
		),
});

export type OperatorPassPurchaseResponse = z.infer<typeof OperatorPassPurchaseResponse>;

export const OperatorPassCertificateFileResponse = z
	.file()
	.describe('The newest stored copy of one Operator certificate page as a PDF');

export const OperatorPassCertificateTokenParam = z.object({
	token: z
		.string()
		.regex(/^[A-Za-z0-9_-]{22}$/)
		.describe('The random code in the link of a certificate page'),
});

export type OperatorPassCertificateTokenParam = z.infer<typeof OperatorPassCertificateTokenParam>;

export const OperatorPassCertificateStatusSchema = createNamedStringLiteralUnion(
	[
		['valid', 'VALID', 'The page is a valid Operator certificate'],
		['cancelled', 'CANCELLED', 'The page was cancelled and is no longer a valid Operator certificate'],
	],
	'Status of an Operator certificate page',
);

export const OperatorPassCertificateSourceSchema = createNamedStringLiteralUnion(
	[
		['purchase', 'PURCHASE', 'The pass was bought'],
		['visionary', 'VISIONARY', 'The pass came with Visionary'],
	],
	'How the pass on an Operator certificate page was obtained',
);

export const OperatorPassCertificateResponse = z.object({
	number: z.number().int().describe('The certificate number'),
	status: OperatorPassCertificateStatusSchema,
	name: z.string().nullable().describe('The name on the newest copy of the page, null when the page is cancelled'),
	source: OperatorPassCertificateSourceSchema,
	paid_at: z.iso
		.datetime()
		.describe('When the pass was paid for, or the date the account got Visionary for a Visionary page'),
	issued_at: z.iso
		.datetime()
		.nullable()
		.describe('When the newest copy of the page, which carries this link, was issued, null when the page is cancelled'),
});

export type OperatorPassCertificateResponse = z.infer<typeof OperatorPassCertificateResponse>;

export const OperatorPassCertificateLinkResponse = z.object({
	certificate_url: z.string().describe('The new private link of the certificate page'),
});

export type OperatorPassCertificateLinkResponse = z.infer<typeof OperatorPassCertificateLinkResponse>;

export const OperatorPassPurchaseListResponse = z.array(OperatorPassPurchaseResponse);

export type OperatorPassPurchaseListResponse = z.infer<typeof OperatorPassPurchaseListResponse>;

export const OperatorPassPurchaseIdParam = z.object({
	purchase_id: z.string().trim().min(1).max(256).describe('The purchase ID'),
});

export type OperatorPassPurchaseIdParam = z.infer<typeof OperatorPassPurchaseIdParam>;

export const OperatorPassPurchaseListQuery = z.object({
	session_id: z
		.string()
		.trim()
		.min(1)
		.max(256)
		.optional()
		.describe(
			'Checkout session the buyer just returned from. If it is paid and the payment webhook has not arrived yet, the purchase is fulfilled before the list is returned.',
		),
});

export type OperatorPassPurchaseListQuery = z.infer<typeof OperatorPassPurchaseListQuery>;

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
		['accepted', 'ACCEPTED', 'The recipient accepted and the pass is about to move'],
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
	ended_at: z.iso.datetime().nullable().describe('When the offer completed or ended'),
});

export type OperatorPassOfferResponse = z.infer<typeof OperatorPassOfferResponse>;

export const OperatorPassUnitResponse = z.object({
	id: z.string().describe('ID of the pass'),
	source: z.enum(OPERATOR_PASS_SOURCES).describe('How the account got the pass'),
	status: OperatorPassUnitStatusSchema,
	purchase_id: z
		.string()
		.nullable()
		.describe('The purchase that paid for the pass, null for a staff grant or the pass that came with Visionary'),
	delivered_at: z.iso.datetime().describe('When the pass reached this account'),
	purchased_at: z.iso
		.datetime()
		.nullable()
		.optional()
		.describe('When the payment for the purchase settled, null when the account did not buy this pass itself'),
	paused: z.boolean().describe('Whether the pass is paused while a payment dispute is open'),
	can_pass_on: z.boolean().describe('Whether the pass can be passed on. Received and staff granted passes cannot'),
	can_withdraw: z.boolean().describe('Whether the withdraw button for this pass is available now'),
	withdrawal_ends_at: z.iso
		.datetime()
		.nullable()
		.describe(
			'When the withdrawal window for this pass closes, null when it has none. Under a country rule this is the end of withdrawal_ends_on in America/Rio_Branco',
		),
	withdrawal_ends_on: z.iso
		.date()
		.nullable()
		.optional()
		.describe(
			'The last day of the withdrawal window under a country rule, as YYYY-MM-DD in America/Rio_Branco. Show it as is, without converting time zones',
		),
	offer: OperatorPassOfferResponse.nullable().describe('The open offer for this pass, null when none'),
	received_from: OperatorPassOfferUserResponse.nullable()
		.optional()
		.describe('Who passed this pass on to the account, null when the account bought it or that account is gone'),
	certificate_number: z
		.number()
		.int()
		.nullable()
		.describe('Number of the certificate page of this pass, null when the pass has no page'),
});

export type OperatorPassUnitResponse = z.infer<typeof OperatorPassUnitResponse>;

export const OperatorPassVisionaryStateSchema = createNamedStringLiteralUnion(
	[
		['available', 'AVAILABLE', 'The account has Visionary and can turn on the Operator Pass that comes with it'],
		['on', 'ON', 'The account turned on the Operator Pass that comes with Visionary'],
		['ended', 'ENDED', 'The pass ended because the Visionary of the account ended'],
		['unavailable', 'UNAVAILABLE', 'The account has Visionary but cannot turn the pass on, see reason'],
	],
	'State of the Operator Pass that comes with Visionary',
);

export const OperatorPassVisionaryUnavailableReasonSchema = z
	.literal('removed')
	.describe(
		'Why the account cannot turn on the Operator Pass that comes with Visionary. removed means the pass was removed from the account permanently under section 8 of the Operator Pass terms',
	);

export const OperatorPassVisionaryResponse = z.object({
	state: OperatorPassVisionaryStateSchema,
	reason: OperatorPassVisionaryUnavailableReasonSchema.nullable().describe('Why state is unavailable, null otherwise'),
	since: z.iso
		.datetime()
		.describe('The date the account got Visionary. The certificate, the badge and the wall order use this date'),
	pass_id: z.string().nullable().describe('ID of the pass once it is turned on, null before'),
	certificate_number: z
		.number()
		.int()
		.nullable()
		.describe('Number of the certificate page once the pass is turned on, null before'),
	certificate_url: z
		.string()
		.nullable()
		.describe(
			'The private link of the certificate page on the marketing site once the pass is on, null before, after it ends or when the page has no link',
		),
	labs_member: z.boolean().describe('Whether the account is in Fluxer Labs, where the pass gives the Operator role'),
	terms_version: z
		.string()
		.describe('The Operator Pass terms version to send back when turning the pass on, or the one accepted once on'),
});

export type OperatorPassVisionaryResponse = z.infer<typeof OperatorPassVisionaryResponse>;

export const OperatorPassVisionaryTurnOnRequest = z.object({
	certificate_name: OperatorPassCertificateNameType,
	show_badge: z
		.boolean()
		.describe('Whether to show the Operator badge on the profile. Ignored when the account already has the badge'),
	wall: z
		.discriminatedUnion('opted_out', [
			z.object({opted_out: z.literal(true)}),
			z.object({opted_out: z.literal(false), name: OperatorPassWallNameType}),
		])
		.optional()
		.describe(
			"The choice for the Operators' wall. Ignored when the account already made a choice, and treated as not listed when left out",
		),
	labs_greeting: z
		.boolean()
		.optional()
		.describe(
			'Whether the system user welcomes the account by name in #operators in Fluxer Labs once the pass is on. Left out, the choice the account made before stays, and a new account is welcomed',
		),
	terms_accepted: z
		.boolean()
		.describe('Whether the account ticked the box that accepts the Operator Pass terms. Must be true'),
	age_confirmed: z
		.boolean()
		.describe(
			"Whether the account ticked \"I'm 18 or older, or have a parent's or guardian's permission.\" Must be true",
		),
	terms_version: z.string().max(64).describe('The version of the Operator Pass terms the account was shown'),
	box_text_version: z.string().max(64).describe('The version of the terms box text the account was shown'),
	consent_locale: z
		.string()
		.max(16)
		.optional()
		.describe('Locale the client used to show the box text. The activation records the box text in this locale'),
});

export type OperatorPassVisionaryTurnOnRequest = z.infer<typeof OperatorPassVisionaryTurnOnRequest>;

export const OperatorPassLabsResponse = z.object({
	greeting: z
		.boolean()
		.describe(
			'Whether the system user welcomes the account by name in #operators in Fluxer Labs when a pass is delivered. On unless the account turned it off',
		),
	show_operators: z
		.boolean()
		.describe(
			'Whether the account gets the Fluxer Labs roles that open #operators. On unless the account turned it off. When off, no role update adds them back',
		),
	greeted: z.boolean().describe('Whether the account was already welcomed in #operators. Nobody is welcomed twice'),
	left: z
		.boolean()
		.describe('Whether the account left Fluxer Labs or was removed from it, so passes no longer add it to Labs'),
	member: z.boolean().describe('Whether the account is in Fluxer Labs now'),
	guild_id: SnowflakeStringType.nullable().describe('ID of the Fluxer Labs community, null when it is not set up'),
	operators_channel_id: SnowflakeStringType.nullable().describe(
		'ID of the #operators channel in Fluxer Labs, null when it is not set up',
	),
});

export type OperatorPassLabsResponse = z.infer<typeof OperatorPassLabsResponse>;

export const OperatorPassLabsJoinResponse = z.object({
	status: z
		.enum(['joined', 'left', 'banned', 'unavailable'])
		.describe(
			'joined: the account is in Fluxer Labs with the Operator role. left: the account left Fluxer Labs or was removed from it before, so it is not added again and can join with the invite. banned: the account is banned from Fluxer Labs. unavailable: the account cannot be added right now',
		),
	guild_id: SnowflakeStringType.nullable().describe('ID of the Fluxer Labs community, null when it is not set up'),
	operators_channel_id: SnowflakeStringType.nullable().describe(
		'ID of the #operators channel in Fluxer Labs, null when it is not set up',
	),
});

export type OperatorPassLabsJoinResponse = z.infer<typeof OperatorPassLabsJoinResponse>;

export const OperatorPassLabsGreetingRequest = z.object({
	greeting: z
		.boolean()
		.describe(
			'Whether the system user welcomes the account by name in #operators in Fluxer Labs when a pass is delivered',
		),
});

export type OperatorPassLabsGreetingRequest = z.infer<typeof OperatorPassLabsGreetingRequest>;

export const OperatorPassLabsOperatorsRequest = z.object({
	show_operators: z.boolean().describe('Whether the account gets the Fluxer Labs roles that open #operators'),
});

export type OperatorPassLabsOperatorsRequest = z.infer<typeof OperatorPassLabsOperatorsRequest>;

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
	visionary: OperatorPassVisionaryResponse.optional().describe(
		'The Operator Pass that comes with Visionary. Left out when the account never had Visionary',
	),
	labs: OperatorPassLabsResponse.describe('The place in Fluxer Labs that comes with the pass'),
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
			'Must be true when this is the last pass on the account, because passing it on removes the badge, the wall listing and #operators from the account',
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
	labs_greeting: z
		.boolean()
		.optional()
		.describe(
			'Whether the system user welcomes the recipient by name in #operators in Fluxer Labs once the pass reaches them. Left out, the choice the account made before stays, and a new account is welcomed',
		),
});

export type OperatorPassOfferAcceptRequest = z.infer<typeof OperatorPassOfferAcceptRequest>;

export const OperatorPassWallDisplayStateSchema = createNamedStringLiteralUnion(
	[
		['live', 'LIVE', 'The wall shows the name and avatar the holder chose'],
		['fallback', 'FALLBACK', 'The wall shows Operator with the default avatar after staff took the name or avatar off'],
		['hidden', 'HIDDEN', 'Staff hid the listing for now'],
		['removed', 'REMOVED', 'The listing is off the wall under section 8 of the Operator Pass terms'],
	],
	"What the Operators' wall shows for the listing",
);

export const OperatorPassListingResponse = z.object({
	wall_name: z.string().nullable().describe('The name the holder chose for the wall, null when not listed'),
	wall_quote: z
		.string()
		.nullable()
		.describe('The quote the holder set for their wall listing, null when there is none or the holder is not listed'),
	wall_opt_out: z.boolean().describe('Whether the holder chose not to be listed'),
	keep_as_is: z.boolean().describe('Whether profile name and avatar changes stay off the wall'),
	release_list_opt_in: z
		.boolean()
		.describe('Whether the holder chose to have their wall name in every yearly The Operators of list. Off by default'),
	status: OperatorPassListingStatusSchema,
	display_state: OperatorPassWallDisplayStateSchema.nullable().describe('What the wall shows, null when not listed'),
	shown_name: z.string().nullable().describe('The name the wall shows right now, null when not listed'),
	shown_avatar_url: z.url().nullable().describe('The avatar the wall shows right now, null for the default avatar'),
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
	wall_quote: z
		.string()
		.max(OPERATOR_PASS_WALL_QUOTE_MAX_CODE_UNITS)
		.nullish()
		.describe(
			'A short quote for the wall listing, up to 80 characters on one line with no links or contact handles. A character is a grapheme cluster, counted after Unicode NFC normalization. Null or an empty string clears it, and so does leaving the wall with wall_opt_out true. Unchanged when omitted',
		),
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
	name: z.string().describe('The name shown: the name the holder chose, or Operator as fallback'),
	avatar: z.string().nullable().describe('SHA-256 of the avatar snapshot, null for the default avatar'),
	avatar_url: z.url().nullable().describe('The avatar snapshot at the largest size, null for the default avatar'),
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
	quote: z
		.string()
		.optional()
		.describe(
			'The quote the Operator set for their listing, plain text to show in straight double quotes and never parse as markup. Left out when there is none',
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
