// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	OperatorPassCertificateNameType,
	OperatorPassHolderResponse,
	OperatorPassPurchaseResponse,
	OperatorPassWallDeclineReasonSchema,
	OperatorPassWallDisplayStateSchema,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {base64LengthForBytes, createBase64StringType} from '@fluxer/schema/src/primitives/FileValidators';
import {createNamedStringLiteralUnion, SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const AdminOperatorPassRuleText = z.string().trim().min(1).max(500);
const AdminOperatorPassFactsText = z.string().trim().min(1).max(4000);

export const AdminOperatorPassRemovalStateSchema = createNamedStringLiteralUnion(
	[
		['notice', 'NOTICE', 'The holder was told the pass will be removed permanently and has 7 days to reply'],
		['removed', 'REMOVED', 'The perks are removed for a limited time or permanently'],
	],
	'State of an Operator Pass perk removal',
);

export const AdminOperatorPassRemovalResponse = z.object({
	state: AdminOperatorPassRemovalStateSchema,
	rule: z.string().describe('The rule the holder broke'),
	facts: z.string().describe('The facts the decision relied on'),
	automated: z.boolean().describe('Whether automated tools helped detect the breach'),
	serious: z.boolean().nullable().describe('Whether the breach was serious enough to skip the 7 day notice'),
	removed_until: z.iso.datetime().nullable().describe('When a limited removal ends, null when permanent'),
	notice_sent_at: z.iso.datetime().nullable().describe('When the 7 day notice was sent'),
	removed_at: z.iso.datetime().nullable().describe('When the perks were removed'),
	admin_user_id: SnowflakeStringType.describe('Staff member who made the latest decision'),
});

export type AdminOperatorPassRemovalResponse = z.infer<typeof AdminOperatorPassRemovalResponse>;

export const AdminOperatorPassRowResponse = z.object({
	granted_at: z.iso.datetime().describe('When the pass was granted, the date on the badge'),
	source: z.string().describe('How the pass was obtained'),
	paused: z.boolean().describe('Whether an open payment dispute pauses the pass'),
	checkout_session_id: z.string().nullable().describe('Purchase that delivered the first pass the account holds'),
	invoice_number: z
		.string()
		.nullable()
		.describe("Stripe's invoice number of that purchase, or the charge receipt number when it has no invoice"),
	pass_count: z.number().int().describe('How many passes the account holds'),
	terms_version: z.string().nullable().describe('Version of the Operator Pass terms the holder accepted'),
	waiver_consent_at: z.iso
		.datetime()
		.nullable()
		.describe('When the holder asked for delivery right away and acknowledged losing the right of withdrawal'),
	waiver_consent_version: z.string().nullable().describe('Version of the consent text the holder agreed to'),
	waiver_consent_ip: z
		.string()
		.nullable()
		.describe('IP address the consent came from, for chargeback evidence. Null unless the admin has USER_VIEW_IP'),
	waiver_user_agent: z
		.string()
		.nullable()
		.describe('Browser or app that sent the consent. Null unless the admin has USER_VIEW_IP'),
});

export const AdminOperatorPassRemovalEndReasonSchema = createNamedStringLiteralUnion(
	[
		['expired', 'EXPIRED', 'The limited removal ran its full time'],
		['restored', 'RESTORED', 'Staff ended the removal early, for example after an appeal'],
		['replaced', 'REPLACED', 'A later removal replaced it'],
	],
	'Why an Operator Pass perk removal ended',
);

export const AdminOperatorPassRemovalHistoryEntryResponse = z.object({
	removed_at: z.iso.datetime().describe('When the perks were removed'),
	removed_until: z.iso.datetime().nullable().describe('When a limited removal was set to end, null when permanent'),
	rule: z.string().describe('The rule the holder broke'),
	facts: z.string().describe('The facts the decision relied on'),
	automated: z.boolean().describe('Whether automated tools helped detect the breach'),
	serious: z.boolean().describe('Whether the breach was treated as serious'),
	notice_sent_at: z.iso.datetime().nullable().describe('When the 7 day notice before it was sent'),
	admin_user_id: SnowflakeStringType.describe('Staff member who removed the perks'),
	ended_at: z.iso.datetime().nullable().describe('When the removal ended, null while it lasts'),
	end_reason: AdminOperatorPassRemovalEndReasonSchema.nullable().describe('Why the removal ended'),
});

export const AdminOperatorPassWallEventKindSchema = createNamedStringLiteralUnion(
	[
		['delivered', 'DELIVERED', 'A pass was delivered, at payment or when a passed on pass arrived'],
		['listed', 'LISTED', "A name went on the Operators' wall"],
		['changed', 'CHANGED', 'The holder changed the name on the wall'],
		['opted_out', 'OPTED_OUT', 'The holder chose not to be listed'],
		['taken_down', 'TAKEN_DOWN', "The name came off the Operators' wall, see cause"],
		['shown_again', 'SHOWN_AGAIN', 'The name came back on the wall after a pause or a limited removal ended'],
		['reviewed', 'REVIEWED', 'Staff reviewed the listed name and kept it, from before moderation states'],
		['approved', 'APPROVED', 'A name and avatar went on the wall through a staff approval or a revert'],
		['declined', 'DECLINED', 'Staff declined the name or the avatar, see reason'],
		['fallback', 'FALLBACK', 'The wall shows Operator because no accepted name is left'],
		[
			'legal_hold_set',
			'LEGAL_HOLD_SET',
			'Staff kept the copies past the retention periods for an appeal or a legal hold',
		],
		['legal_hold_cleared', 'LEGAL_HOLD_CLEARED', 'Staff released a legal hold'],
		['display_adjusted', 'DISPLAY_ADJUSTED', 'Staff made a technical adjustment to how the name is shown'],
		['release_published', 'RELEASE_PUBLISHED', 'The name was published in a The Operators of list'],
		['erased', 'ERASED', 'Staff erased the name from a published list'],
		['count_changed', 'COUNT_CHANGED', 'The displayed wall count changed, see pass_count'],
		['quote_cleared', 'QUOTE_CLEARED', 'Staff cleared the quote on the listing, see reason'],
	],
	'What happened to the name or the pass',
);

export const AdminOperatorPassWallEventResponse = z.object({
	occurred_at: z.iso.datetime().describe('When it happened'),
	kind: AdminOperatorPassWallEventKindSchema,
	name: z.string().nullable().describe('The name involved'),
	reason: z.string().nullable().describe('The reason code of a decline'),
	cause: z
		.string()
		.nullable()
		.describe(
			'Why a name came down: withdrawal, refund, dispute, removal (benefits removed under section 8), hidden (staff hid the listing for now), deletion, holder_request (the holder left the wall), pause or passed_on',
		),
	purchase_id: z.string().nullable().describe('The purchase that was delivered'),
	release_year: z.number().int().nullable().describe('The list year involved'),
	release_tag: z.string().nullable().describe('The release tag involved'),
	actor_id: SnowflakeStringType.nullable().describe('Who did it, the holder or a staff member, null for the system'),
	pass_count: z
		.number()
		.int()
		.nullable()
		.describe('The displayed count after a change. Older events recorded held passes.'),
	first_pass_at: z.iso
		.datetime()
		.nullable()
		.optional()
		.describe('The earliest qualifying delivery for this count. Older events do not include this date.'),
});

export type AdminOperatorPassWallEventResponse = z.infer<typeof AdminOperatorPassWallEventResponse>;

export const AdminOperatorPassWallEntryResponse = z.object({
	user_id: SnowflakeStringType,
	username: z.string().nullable().describe('Username of the account, null when it no longer exists'),
	avatar_url: z.url().nullable().describe('Current profile avatar of the account'),
	wall_name: z.string().nullable().describe('The name the holder chose for the wall'),
	wall_quote: z
		.string()
		.nullable()
		.describe('The quote the holder set for their listing, shown on the wall only while the listing is shown'),
	wall_opt_out: z.boolean(),
	keep_as_is: z.boolean().describe('Whether profile changes stay off the wall'),
	release_list_opt_in: z.boolean().describe('Whether the holder chose to be in every yearly The Operators of list'),
	display_state: OperatorPassWallDisplayStateSchema.nullable(),
	shown_name: z.string().nullable().describe('The name the wall shows now'),
	shown_avatar_url: z
		.url()
		.nullable()
		.describe('The avatar the wall shows now, null for the default avatar or when the wall shows none'),
	live_revision_id: SnowflakeStringType.nullable().describe('The revision the wall shows'),
	live_avatar_private_url: z.string().nullable().describe('Admin route to the private copy of the live avatar'),
	business: z
		.boolean()
		.describe('Whether the wall shows a business listing with a company name and logo, after the listings of people'),
	decline_reason: OperatorPassWallDeclineReasonSchema.nullable(),
	decline_note: z.string().nullable().describe('The facts sent to the holder'),
	hidden: z.boolean().describe('Whether staff hid the listing for now'),
	legal_hold: z
		.boolean()
		.describe('Whether the retention job keeps the name and avatar copies, for an open appeal or a legal hold'),
	left_wall_at: z.iso
		.datetime()
		.nullable()
		.describe('When the holder left the wall, the copies are deleted 60 days later'),
	listed_at: z.iso.datetime().nullable().describe('When the listing went on the wall'),
	delivered_at: z.iso.datetime().nullable().describe('When the pass was delivered'),
	release_published_year: z.number().int().nullable().describe('Year of the latest list the name was published in'),
	release_published_at: z.iso
		.datetime()
		.nullable()
		.describe('Release date of the latest list the name was published in'),
	release_erased_at: z.iso
		.datetime()
		.nullable()
		.describe('When staff first erased the name from a list, which keeps it out of later lists'),
});

export const AdminOperatorPassWallRevisionResponse = z.object({
	id: SnowflakeStringType,
	name: z.string(),
	avatar_url: z.string().nullable().describe('Admin route to the private copy of the avatar'),
	business: z.boolean().describe('Whether the revision is a business listing with a company name and logo'),
	source: z.string().describe('checkout, profile, settings or admin'),
	status: z.string().describe('pending, approved, declined, superseded or reverted'),
	created_at: z.iso.datetime(),
	created_by: SnowflakeStringType.nullable(),
});

export const AdminOperatorPassWallDecisionResponse = z.object({
	id: SnowflakeStringType,
	revision_id: SnowflakeStringType.nullable(),
	kind: z.string().describe('approve, decline, revert, force_fallback, hide, unhide or clear_quote'),
	decided_by: SnowflakeStringType.nullable(),
	decided_at: z.iso.datetime(),
	reason: OperatorPassWallDeclineReasonSchema.nullable(),
	note: z.string().nullable(),
	statement_id: z.string().nullable().describe('ID of the statement of reasons sent to the holder'),
});

export const AdminOperatorPassWallTimelineResponse = z.object({
	entry: z
		.lazy(() => AdminOperatorPassWallEntryResponse)
		.nullable()
		.describe('The current wall entry'),
	revisions: z.array(AdminOperatorPassWallRevisionResponse).describe('Every revision, oldest first'),
	decisions: z.array(AdminOperatorPassWallDecisionResponse).describe('Every decision, oldest first'),
	events: z.array(z.lazy(() => AdminOperatorPassWallEventResponse)).describe('Wall history, oldest first'),
});

export const AdminOperatorPassWallDecisionRequest = z.object({
	reason: OperatorPassWallDeclineReasonSchema,
	note: z.string().trim().min(1).max(300).describe('The facts the decision relies on, shown to the holder'),
});

export const AdminOperatorPassWallClearQuoteRequest = AdminOperatorPassWallDecisionRequest.extend({
	quote: z
		.string()
		.max(2048)
		.describe('The quote staff reviewed, the request fails when the listing now has another one'),
});

export const AdminOperatorPassWallBusinessRequest = z.object({
	name: z.string().trim().min(1).max(32).describe('The company name as it shows on the wall'),
	logo: createBase64StringType(1, base64LengthForBytes(2 * 1024 * 1024)).describe(
		'A square PNG, JPEG or WebP logo of at least 256 pixels and under 2 MB, not animated, as base64 or a data URI',
	),
});

export const AdminOperatorPassWallRevisionIdParam = z.object({
	user_id: SnowflakeStringType,
	revision_id: SnowflakeStringType,
});

export const AdminOperatorPassWallAvatarParam = z.object({
	sha256: z
		.string()
		.regex(/^[0-9a-f]{64}$/u)
		.describe('SHA-256 of the avatar snapshot'),
});

export const AdminOperatorPassWallHideRequest = z.object({
	hidden: z.boolean(),
	reason: OperatorPassWallDeclineReasonSchema.nullish(),
	note: z.string().trim().min(1).max(300).nullish().describe('The facts, shown to the holder when hiding'),
});

export const AdminOperatorPassWallAsOfQuery = z.object({
	at: z.iso.datetime().describe('The time to render the wall at'),
});

export const AdminOperatorPassWallAsOfEntryResponse = z.object({
	user_id: SnowflakeStringType.nullable().describe('The account, null for a deleted holder'),
	name: z.string(),
	avatar_url: z
		.string()
		.nullable()
		.describe('Admin route to the private avatar copy, null for the default or a deleted holder'),
	count: z.number().int(),
	tier: z.number().int(),
	first_pass_at: z.iso.datetime(),
	deleted: z.boolean().describe('Whether the holder was deleted, shown as a neutral placeholder'),
});

export const AdminOperatorPassWallAsOfResponse = z.object({
	at: z.iso.datetime(),
	entries: z
		.array(AdminOperatorPassWallAsOfEntryResponse)
		.describe('The wall as it was at that time, in display order'),
});

export const AdminOperatorPassWallStateResponse = z.object({
	version: z.number().int().describe('The published wall version'),
	frozen: z.boolean(),
	frozen_at: z.iso.datetime().nullable(),
	frozen_by: SnowflakeStringType.nullable(),
	published_at: z.iso.datetime().nullable(),
	entry_count: z.number().int().describe('Entries in the snapshot that is being served'),
});

export const AdminOperatorPassWallLegalHoldRequest = z.object({
	held: z
		.boolean()
		.describe('Whether to keep the copies past the retention periods, for an open appeal or a legal hold'),
});

export const AdminOperatorPassWallFreezeRequest = z.object({
	frozen: z.boolean().describe('Whether to stop publishing and keep serving the current snapshot'),
});

export type AdminOperatorPassWallEntryResponse = z.infer<typeof AdminOperatorPassWallEntryResponse>;

export const AdminOperatorPassReleaseYearParam = z.object({
	year: z.coerce.number().int().min(2026).max(9999).describe('The year of the list'),
});

export const AdminOperatorPassReleaseEraseParam = z.object({
	year: z.coerce.number().int().min(2026).max(9999).describe('The year of the list'),
	user_id: SnowflakeStringType.describe('The account whose name is erased'),
});

export const AdminOperatorPassReleaseCandidateResponse = z.object({
	user_id: SnowflakeStringType,
	name: z.string(),
	delivered_at: z.iso.datetime().nullable(),
});

export const AdminOperatorPassStoredReleaseResponse = z.object({
	year: z.number().int(),
	tag: z.string(),
	released_at: z.iso.datetime(),
	published_at: z.iso.datetime(),
	published_by: SnowflakeStringType,
	cutoff_at: z.iso.datetime(),
	names: z.array(z.string()),
	erased_names: z.array(z.string()).describe('Names erased after publication'),
});

export const AdminOperatorPassReleasePreviewResponse = z.object({
	year: z.number().int(),
	cutoff_at: z.iso.datetime().describe('Passes delivered by then go into the list'),
	publish_by: z.iso.datetime().describe('The latest date to publish the list'),
	can_publish: z.boolean().describe('Whether the cut-off has passed and the list is not published yet'),
	published: AdminOperatorPassStoredReleaseResponse.nullable(),
	candidates: z.array(AdminOperatorPassReleaseCandidateResponse).describe('Names that go into the list, sorted'),
});

export type AdminOperatorPassReleasePreviewResponse = z.infer<typeof AdminOperatorPassReleasePreviewResponse>;

export const AdminOperatorPassReleasePublishRequest = z.object({
	tag: z.string().trim().min(1).max(100).describe('The tag of the Fluxer release the list ships in'),
	released_at: z.iso.datetime().describe('When the release shipped'),
});

export const AdminOperatorPassReleaseFileResponse = z.object({
	release: AdminOperatorPassStoredReleaseResponse,
	operators_file: z.string().describe('The OPERATORS file to commit'),
	tasks: z.array(z.string()).describe('Edits to make by hand, such as release notes changes after an erasure'),
});

export type AdminOperatorPassReleaseFileResponse = z.infer<typeof AdminOperatorPassReleaseFileResponse>;

export const AdminOperatorPassUnitEventResponse = z.object({
	occurred_at: z.iso.datetime().describe('When it happened'),
	kind: z
		.string()
		.describe(
			'purchased, visionary_turned_on, country_rule, delivered, offer_created, offer_accepted, offer_declined, offer_withdrawn, offer_cancelled, transferred, paused, resumed, withdrawn or revoked',
		),
	payment_id: z.string().nullable().describe('Stripe payment intent of the purchase'),
	purchase_id: z.string().nullable().describe('The purchase that paid for the pass'),
	sender_id: SnowflakeStringType.nullable().describe('Who passed the pass on, or the holder it ended for'),
	recipient_id: SnowflakeStringType.nullable().describe('Who got the pass'),
	offer_id: z.string().nullable().describe('The Pass it on offer'),
	country: z.string().nullable().describe('Country of a withdrawal rule'),
	days: z.number().int().nullable().describe('Days of a withdrawal rule'),
	rule_version: z.string().nullable().describe('Version of the withdrawal country table'),
	reason: z.string().nullable().describe('Why the pass ended or who granted it'),
});

export const AdminOperatorPassUnitResponse = z.object({
	id: z.string().describe('ID of the pass'),
	purchase_id: z.string().nullable().describe('The purchase that paid for the pass, null for a staff grant'),
	buyer_id: SnowflakeStringType.nullable().describe('Who paid for the pass'),
	holder_id: SnowflakeStringType.describe('Who holds the pass, or held it last'),
	source: z.string().describe('purchase, transfer or admin'),
	status: z.string().describe('held, withdrawn or revoked'),
	transferable: z.boolean().describe('Whether the holder can pass it on'),
	delivered_at: z.iso.datetime().describe('When the pass reached the current holder'),
	first_delivered_at: z.iso.datetime().describe('When the pass was first delivered'),
	paused_at: z.iso.datetime().nullable().describe('When an open payment dispute paused it'),
	ended_at: z.iso.datetime().nullable().describe('When the pass ended'),
	end_reason: z.string().nullable().describe('Why the pass ended'),
	withdrawal_country: z.string().nullable().describe('Country whose withdrawal right applies'),
	withdrawal_days: z.number().int().nullable().describe('Days of that withdrawal right'),
	events: z.array(AdminOperatorPassUnitEventResponse).describe('Append-only history of the pass, oldest first'),
});

export const AdminOperatorPassOfferResponse = z.object({
	id: z.string().describe('ID of the offer'),
	pass_id: z.string().describe('ID of the pass on offer'),
	status: z.string().describe('pending, accepted, completed, declined, withdrawn or cancelled'),
	sender_id: SnowflakeStringType.describe('Who passes the pass on'),
	recipient_id: SnowflakeStringType.describe('Who gets the pass'),
	created_at: z.iso.datetime().describe('When the offer was made'),
	accepted_at: z.iso.datetime().nullable().describe('When the recipient accepted'),
	ended_at: z.iso.datetime().nullable().describe('When the offer completed or ended'),
	terms_version: z.string().nullable().describe('Version of the terms the recipient accepted'),
});

export const AdminOperatorPassVisionaryActivationResponse = z.object({
	visionary_since: z.iso.datetime().nullable().describe('The date the pass shows, copied at turn-on'),
	visionary_sequence: z.number().int().nullable().describe('The Visionary number at turn-on'),
	activated_at: z.iso.datetime().nullable().describe('When the account turned the pass on, null before'),
	terms_version: z.string().nullable().describe('The Operator Pass terms version the account accepted'),
	terms_box_text: z.string().nullable().describe('The terms box text as shown, cleared on account deletion'),
	age_confirmed_at: z.iso.datetime().nullable().describe('When the account ticked the age box'),
	pass_id: z.string().nullable().describe('The pass, visionary:<user_id>:1'),
	certificate_number: z.number().int().nullable().describe('Number of the certificate page'),
	ended_at: z.iso.datetime().nullable().describe('When the pass ended'),
	end_reason: z.string().nullable().describe('visionary_ended or deletion'),
});

export type AdminOperatorPassVisionaryActivationResponse = z.infer<typeof AdminOperatorPassVisionaryActivationResponse>;

export const AdminOperatorPassVisionaryActivationsCsvResponse = z
	.file()
	.describe(
		'CSV of every Visionary turn-on with user_id, visionary_sequence, visionary_since, activated_at, terms_version, certificate_number, issued_at, ended_at and end_reason',
	);

export const AdminOperatorPassDeliveriesQuery = z.object({
	month: z
		.string()
		.regex(/^\d{4}-(0[1-9]|1[0-2])$/u)
		.describe('Calendar month of delivered_at in UTC, as YYYY-MM'),
});

export const AdminOperatorPassDeliveriesCsvResponse = z
	.file()
	.describe(
		'CSV with one row per pass of every Operator Pass purchase delivered in the month, in the operator-pass-leveranser format the books read',
	);

export const AdminOperatorPassUserResponse = z.object({
	holder: OperatorPassHolderResponse.nullable().describe('The pass the account holds now, null when it holds none'),
	pass: AdminOperatorPassRowResponse.nullable().describe('The stored pass, also while paused or removed'),
	removal: AdminOperatorPassRemovalResponse.nullable().describe('Notice or removal under section 8'),
	removal_history: z
		.array(AdminOperatorPassRemovalHistoryEntryResponse)
		.describe('Every removal under section 8, oldest first, also after it ended'),
	purchases: z.array(OperatorPassPurchaseResponse).describe('Purchases the account paid for'),
	units: z
		.array(AdminOperatorPassUnitResponse)
		.describe('Every pass the account holds or paid for, with its history, oldest first'),
	offers: z.array(AdminOperatorPassOfferResponse).describe('Pass it on offers the account made or received'),
	wall: AdminOperatorPassWallEntryResponse.nullable().describe(
		"The name on the Operators' wall, null when none was set",
	),
	wall_history: z
		.array(AdminOperatorPassWallEventResponse)
		.describe('Append-only history of deliveries, the wall name and release lists, oldest first'),
	visionary_activation: AdminOperatorPassVisionaryActivationResponse.nullable()
		.optional()
		.describe('The Visionary turn-on and email record, null when the account has none'),
});

export type AdminOperatorPassUserResponse = z.infer<typeof AdminOperatorPassUserResponse>;

export const AdminOperatorPassNoticeRequest = z.object({
	rule: AdminOperatorPassRuleText.describe('The rule the holder broke'),
	facts: AdminOperatorPassFactsText.describe('The facts the notice relies on'),
});

export type AdminOperatorPassNoticeRequest = z.infer<typeof AdminOperatorPassNoticeRequest>;

export const AdminOperatorPassRemovalRequest = z.object({
	rule: AdminOperatorPassRuleText.describe('The rule the holder broke'),
	facts: AdminOperatorPassFactsText.describe('The facts the decision relies on'),
	automated: z.boolean().describe('Whether automated tools helped detect the breach'),
	serious: z.boolean().describe('Whether the breach is serious enough to act without the 7 day notice'),
	duration_days: z
		.number()
		.int()
		.min(1)
		.max(3650)
		.nullable()
		.describe('Length of a limited removal in days, null for a permanent removal'),
	notify_user: z.boolean().default(true).describe('Whether to email the statement of reasons'),
});

export type AdminOperatorPassRemovalRequest = z.infer<typeof AdminOperatorPassRemovalRequest>;

export const AdminOperatorPassPurchaseIdParam = z.object({
	purchase_id: z.string().trim().min(1).max(256).describe('The purchase ID'),
});

export const AdminOperatorPassWithdrawalChannelSchema = createNamedStringLiteralUnion(
	[
		['form', 'FORM', 'The withdrawal form in section 7.2 of the Operator Pass terms'],
		['email', 'EMAIL', 'An email to support'],
		['support', 'SUPPORT', 'Another support channel'],
		['other', 'OTHER', 'Any other clear statement'],
	],
	'How the buyer sent a withdrawal outside the withdraw button',
);

export const AdminOperatorPassWithdrawRequest = z.object({
	name: z.string().trim().min(1).max(200).describe('Name of the buyer who withdraws'),
	received_at: z.iso.datetime().describe('When Fluxer received the withdrawal'),
	channel: AdminOperatorPassWithdrawalChannelSchema,
});

export type AdminOperatorPassWithdrawRequest = z.infer<typeof AdminOperatorPassWithdrawRequest>;

export const AdminOperatorPassCertificatePassIdParam = z.object({
	pass_id: z
		.string()
		.trim()
		.min(1)
		.max(300)
		.describe('The pass ID, the checkout session ID and the pass number joined by a colon'),
});

export const AdminOperatorPassCertificateReissueReasonSchema = createNamedStringLiteralUnion(
	[
		['name_change', 'NAME_CHANGE', 'The holder changed their name, offered under section 3 of the Operator Pass terms'],
		['correction', 'CORRECTION', 'A mistake on our side, fixed under section 6 of the Operator Pass terms'],
		[
			'objection',
			'OBJECTION',
			'The person named on the page objected and the objection succeeded, so the name on every earlier copy and the earlier PDF files are erased',
		],
	],
	'Why a new copy of an Operator certificate page is issued',
);

export const AdminOperatorPassCertificateReissueRequest = z.object({
	holder_name: OperatorPassCertificateNameType.describe('The name to print on the new copy'),
	reason: AdminOperatorPassCertificateReissueReasonSchema,
});

export type AdminOperatorPassCertificateReissueRequest = z.infer<typeof AdminOperatorPassCertificateReissueRequest>;

export const AdminOperatorPassCertificateReissueResponse = z.object({
	pass_id: z.string().describe('The pass ID'),
	number: z.number().int().positive().describe('The certificate number, the same on every copy'),
	version: z.number().int().positive().describe('The version of the new copy'),
	holder_name: z.string().describe('The name printed on the new copy'),
	issued_at: z.iso.datetime().describe('When the new copy was issued'),
	reissue_reason: AdminOperatorPassCertificateReissueReasonSchema,
});

export type AdminOperatorPassCertificateReissueResponse = z.infer<typeof AdminOperatorPassCertificateReissueResponse>;
