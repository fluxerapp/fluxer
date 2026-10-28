// SPDX-License-Identifier: AGPL-3.0-or-later

export const EMAIL_I18N_MESSAGES = {
	account_deletion_cancelled: {
		subject: "Your {product_name} account won't be deleted",
		intro: 'We canceled the scheduled deletion of your {product_name} account.',
		note: "{reply, select, yes {If you didn't do this, reply to this email.} other {If you didn't do this, contact the admins of this instance.}}",
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because the deletion of your {product_name} account was canceled.',
	},
	account_deletion_scheduled_inactivity: {
		subject: 'Your inactive {product_name} account will be deleted',
		intro: "Your {product_name} account has been inactive for a long time, so we'll delete it on {deletion_date}.",
		note: '{reply, select, yes {To keep it, sign in before then. Questions? Reply to this email.} other {To keep it, sign in before then. If you have questions, contact the admins of this instance.}}',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because your {product_name} account has been inactive for a long time.',
	},
	account_deletion_scheduled_requested: {
		subject: 'Your {product_name} account will be deleted on {deletion_date}',
		intro: "As you asked, we'll delete your {product_name} account on {deletion_date}.",
		note: "{reply, select, yes {To keep it, sign in before then. If you didn't ask for this, reply to this email.} other {To keep it, sign in before then. If you didn't ask for this, contact the admins of this instance.}}",
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because the deletion of your {product_name} account was requested.',
	},
	billing_credit_note: {
		subject: 'You have {amountFormatted} in credit with {product_name}',
		heading: 'Credit added to your account',
		preheader: '{amountFormatted} in credit added to your account',
		intro:
			"{numbered, select, yes {We've added {amountFormatted} in credit to your account for invoice {invoiceNumber}.} other {We've added {amountFormatted} in credit to your account.}} {upcoming, select, yes {It will be taken off your next {product_name} invoice, such as a subscription renewal.} other {{reply, select, yes {You don't have an upcoming invoice that it can be used on. If you'd like it refunded, reply to this email.} other {You don't have an upcoming invoice that it can be used on. If you'd like it refunded, contact the admins of this instance.}}}}",
		credit_note_link: 'View credit note',
		seller: 'Seller: Fluxer Platform AB, 559537-3993, {sellerAddress}, <mail>{sellerEmail}</mail>',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because credit was added to your {product_name} account.',
	},
	bulk_message_deletion_completed: {
		subject: 'We finished deleting your {product_name} messages',
		intro:
			'We finished deleting your messages. We removed {message_count, plural, =0 {0 messages} one {# message} other {# messages}} from {channel_count, plural, =0 {0 places} one {# place} other {# places}}.',
		note: "{reply, select, yes {If you didn't ask for this, change your password now and reply to this email.} other {If you didn't ask for this, change your password now and contact the admins of this instance.}}",
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because a message deletion was requested for your {product_name} account.',
	},
	donation_cancellation_confirmation: {
		subject: "We've received your {product_name} donation cancellation",
		heading: 'Cancellation received',
		preheader: 'Your recurring donation ends on {endsAt, date, long}',
		intro: 'We received your request to cancel your recurring donation to {product_name} on {receivedFormatted}.',
		ends_line: 'Your recurring donation ends on {endsAt, date, long}.',
		charge_line:
			"{unpaid, select, yes {One invoice for {unpaidFormatted} is still unpaid. Canceling doesn't remove it, and we'll keep trying to collect it.} other {We won't charge you again.}}",
		cta: 'Manage your donation',
		reference_line: 'Reference: {reference}',
		seller: 'Seller: Fluxer Platform AB, 559537-3993, {sellerAddress}, <mail>{sellerEmail}</mail>',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because you canceled your recurring donation to {product_name}.',
	},
	donation_card_expiring: {
		subject: 'Your card expires before your next {product_name} donation payment',
		heading: 'Update your card',
		preheader:
			'{branded, select, yes {Your {brand} ending in {last4}} other {Your card ending in {last4}}} expires soon',
		intro:
			'{branded, select, yes {Your {brand} ending in {last4}} other {Your card ending in {last4}}} expires at the end of {expiryFormatted}. Your next payment of {tax, select, yes {{amountFormatted}} other {{amountFormatted}, plus any tax that applies,}} is due on {chargeAt, date, long}.',
		lead: 'Update your card before then so your donation keeps going.',
		cta: 'Update your payment method',
		seller: 'Seller: Fluxer Platform AB, 559537-3993, {sellerAddress}, <mail>{sellerEmail}</mail>',
		signoff: 'The {product_name} team',
		footer_reason:
			'You received this email because a card you use for your donation to {product_name} is about to expire.',
	},
	donation_confirmation: {
		subject: 'Thank you for your {product_name} donation',
		intro:
			'{interval, select, month {Your monthly donation to {product_name} is set up.} year {Your yearly donation to {product_name} is set up.} other {We received your donation to {product_name}.}}',
		amount_label: 'Amount',
		currency_label: 'Currency',
		amount_value: '{interval, select, month {{amount} a month} year {{amount} a year} other {{amount}}}',
		date_label: 'Payment date',
		date_value: '{paidAt, date, long}',
		reference_label: 'Reference',
		lead: '{interval, select, month {We email you a receipt for each payment.} year {We email you a receipt for each payment.} other {This email is your receipt.}}',
		receipt_link: 'View the payment receipt',
		cta: '{interval, select, month {Manage or cancel} year {Manage or cancel} other {Manage donations}}',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because you donated to {product_name} with this email address.',
	},
	donation_magic_link: {
		subject: 'Manage your {product_name} donations',
		intro: 'Use this link to manage your donations and download your invoices.',
		cta: 'Manage donations',
		expiry: 'This link expires in {minutes, plural, one {# minute} other {# minutes}}.',
		note: "If you didn't ask for this link, you can ignore this email.",
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because a donor portal link was requested for this email address.',
	},
	donation_payment_failed: {
		subject:
			"{reason, select, action_required {Confirm your {product_name} donation payment} other {Your {product_name} donation payment didn't go through}}",
		heading:
			"{reason, select, action_required {Your bank needs you to confirm a payment} other {Your donation payment didn't go through}}",
		preheader:
			"{reason, select, action_required {Confirm {amountFormatted} for your donation} other {We couldn't charge {amountFormatted} for your donation}}",
		intro:
			"{reason, select, action_required {Your bank wants you to confirm the next payment of your recurring donation to {product_name}.} other {We couldn't charge the next payment of your recurring donation to {product_name}.}}",
		invoice_label: 'Invoice number',
		date_label: 'Payment attempted',
		date_value: '{attemptedAt, date, long}',
		amount_label: 'Amount',
		lead: '{reason, select, action_required {Open the invoice page and confirm the payment there.} other {You can pay the invoice now. To change your payment method or stop the donation, go to your donations.}}',
		cta: '{reason, select, action_required {Confirm the payment} other {Pay the invoice}}',
		manage_link: 'Manage your donations',
		note: '{reply, select, yes {If you need help, reply to this email.} other {If you need help, contact the admins of this instance.}}',
		signoff: 'The {product_name} team',
		footer_reason:
			'You received this email because a payment for your recurring donation to {product_name} needs your attention.',
		reminder_subject: 'Reminder to confirm your donation payment',
	},
	donation_receipt: {
		subject: 'Your {product_name} donation receipt',
		heading: 'Receipt for your donation',
		preheader: '{amountFormatted} donated to {product_name}',
		intro:
			'{interval, select, year {Thank you for your yearly donation. This email is your receipt.} other {Thank you for your monthly donation. This email is your receipt.}}',
		invoice_label: 'Invoice number',
		date_label: 'Payment date',
		date_value: '{paidAt, date, long}',
		period_label: 'Period',
		period_value: '{periodStart, date, long} to\n{periodEnd, date, long}',
		total_label: 'Total',
		lead: '{pdf, select, yes {The invoice is attached to this email as a PDF.} other {You can download the invoice as a PDF on the invoice page.}}',
		cta: 'View invoice',
		link_expiry:
			'{reply, select, yes {The invoice link works until {invoiceLinkUntil, date, long}. After that, reply to this email for a copy.} other {The invoice link works until {invoiceLinkUntil, date, long}. After that, ask the admins of this instance for a copy.}}',
		manage_link: 'Manage your donations',
		signoff: 'The {product_name} team',
		footer_reason:
			'{interval, select, year {You received this email because you donate to {product_name} every year.} other {You received this email because you donate to {product_name} every month.}}',
	},
	donation_refund_receipt: {
		subject: 'Your {product_name} donation refund',
		heading: 'Refund of {amountFormatted}',
		preheader: '{amountFormatted} of your donation refunded',
		intro:
			'We refunded {amountFormatted} of your donation to the card or account you paid with. It usually takes 5 to 10 business days to show up, depending on your bank.',
		reference_label: 'Reference',
		date_label: 'Refund date',
		date_value: '{refundedAt, date, long}',
		refund_label: 'Refunded',
		cta: 'View refund details',
		note: '{reply, select, yes {Questions about this refund? Reply to this email.} other {If you have questions about this refund, contact the admins of this instance.}}',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because a donation to {product_name} was refunded.',
	},
	donation_renewal_notice: {
		subject: 'Your yearly donation to {product_name} renews on {chargeAt, date, long}',
		heading: 'Your donation renews soon',
		preheader: 'Your yearly donation renews on {chargeAt, date, long}',
		intro:
			'Your yearly donation to {product_name} renews automatically on {chargeAt, date, long} unless you cancel before then.',
		charge_line:
			"{method, select, card {On {chargeAt, date, long} we'll charge {tax, select, yes {{nextFormatted}} other {{nextFormatted}, plus any tax that applies,}} to your {brand} ending in {last4}.} plain_card {On {chargeAt, date, long} we'll charge {tax, select, yes {{nextFormatted}} other {{nextFormatted}, plus any tax that applies,}} to your card ending in {last4}.} named {On {chargeAt, date, long} we'll charge {tax, select, yes {{nextFormatted}} other {{nextFormatted}, plus any tax that applies,}} using {methodName}.} other {On {chargeAt, date, long} we'll charge {tax, select, yes {{nextFormatted}} other {{nextFormatted}, plus any tax that applies,}} to your saved payment method.}}",
		after_line:
			'{differs, select, yes {After that, the price is {tax, select, yes {{regularFormatted}} other {{regularFormatted}, plus any tax that applies,}} every year.} other {After that, you pay {tax, select, yes {{regularFormatted}} other {{regularFormatted}, plus any tax that applies,}} every year until you cancel.}}',
		box: 'To stop the renewal, cancel before {cancelByFormatted}.',
		cta: 'Manage your donation',
		keep_line: "If you cancel, we won't charge you again.",
		seller: 'Seller: Fluxer Platform AB, 559537-3993, {sellerAddress}, <mail>{sellerEmail}</mail>',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because your yearly donation to {product_name} renews automatically.',
	},
	dsa_report_resolved: {
		subject: 'We made a decision on your {product_name} report',
		intro:
			"{outcome, select, removed {We removed the content you reported because it broke our rules.} restricted {The account you reported broke our rules, and we took action on it. We don't share which action we took.} action_taken {What you reported broke our rules, and we took action. We don't share which action we took.} no_violation {What you reported doesn't break our rules, so we left it as it is.} other {We closed your report.}}",
		about:
			'{target, select, message_community {This is about your report on a message in #{channelName} in "{communityName}", sent on {reportedAt, date, long}.} message_dm {This is about your report on a message in a direct message, sent on {reportedAt, date, long}.} message {This is about your report on a message, sent on {reportedAt, date, long}.} user {This is about your report on the account "{username}", sent on {reportedAt, date, long}.} community {This is about your report on the community "{communityName}", sent on {reportedAt, date, long}.} other {This is about the report you sent on {reportedAt, date, long}.}}',
		thanks:
			'{outcome, select, removed {Thanks for reporting it. Reports like yours help keep {product_name} safe for everyone.} restricted {Thanks for reporting it. Reports like yours help keep {product_name} safe for everyone.} action_taken {Thanks for reporting it. Reports like yours help keep {product_name} safe for everyone.} other {Thanks for letting us know.}}',
		report_label: 'Report ID',
		comment_label: 'Note from our team',
		note: '{reply, select, yes {If you disagree, reply to this email within 60 days.} other {{appeals_email, select, null {} other {If you disagree, email <mail>{appeals_email}</mail> within 60 days and include the report ID.}}}}{appeals_email, select, null {} other { In the EU, you can also go to a certified <dsa>out-of-court dispute settlement body</dsa> or complain to the Digital Services Coordinator in your country.}}',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because you submitted a Digital Services Act report on {product_name}.',
	},
	dsa_report_verification: {
		subject: 'Your {product_name} code for a DSA report',
		intro: 'Use this code to send your Digital Services Act report to {product_name}.',
		expiry: 'This code expires in {minutes, plural, one {# minute} other {# minutes}}.',
		note: "If you didn't ask for this, you can ignore this email.",
		signoff: 'The {product_name} safety team',
		footer_reason:
			'You received this email because this address was entered on a Digital Services Act report form on {product_name}.',
	},
	email_change_new: {
		subject: 'Confirm your new {product_name} email address',
		intro: 'Enter this code in the app to confirm your new email address.',
		expiry: 'This code expires in {minutes, plural, one {# minute} other {# minutes}}.',
		note: "If you didn't ask for this, you can ignore this email.",
		signoff: 'The {product_name} team',
		footer_reason:
			'You received this email because this address was entered as the new email for a {product_name} account.',
	},
	email_change_original: {
		subject: 'Confirm your {product_name} email change',
		intro: 'To change the email address on your {product_name} account, enter this code in the app.',
		expiry: 'This code expires in {minutes, plural, one {# minute} other {# minutes}}.',
		note: "If you didn't ask for this, change your password now.",
		signoff: 'The {product_name} team',
		footer_reason:
			'You received this email because someone asked to change the email address on your {product_name} account.',
	},
	email_change_revert: {
		subject: 'Your {product_name} email address was changed',
		intro: 'The email address on your {product_name} account was changed.',
		new_email_label: 'New email address',
		lead: "If you didn't do this, undo the change and secure your account.",
		cta: 'Undo and secure my account',
		note: 'This brings back your previous email address, signs you out everywhere, turns off two-factor authentication and asks you to set a new password.',
		signoff: 'The {product_name} safety team',
		footer_reason:
			'You received this email because it was the email address on your {product_name} account before the change.',
	},
	email_verification: {
		subject: 'Confirm your {product_name} email address',
		intro: 'Confirm this email address to finish setting up your {product_name} account.',
		cta: 'Confirm email address',
		expiry: 'This link expires in 24 hours.',
		note: "If you didn't create a {product_name} account, you can ignore this email.",
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because this address was used for a {product_name} account.',
	},
	email_verification_code: {
		subject: 'Your {product_name} email confirmation code',
		intro: 'Enter this code in the app to confirm your email address.',
		expiry: 'This code expires in {minutes, plural, one {# minute} other {# minutes}}.',
		note: "Don't share this code with anyone. {product_name} staff will never ask for it. If you didn't add this email address to an account, you can ignore this email.",
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because this address was added to a {product_name} account.',
	},
	enforcement_ended: {
		subject:
			'{outcome, select, reversed {We reversed a decision about your {product_name} account} other {A restriction on your {product_name} account has ended}}',
		intro:
			"{outcome, select, reversed {{decision_id, select, null {We looked again at a decision about your account and reversed it.} other {We looked again at decision {decision_id} and reversed it.}} We're sorry for the trouble.} other {{decision_id, select, null {We lifted the restriction on your account.} other {We lifted the restriction from decision {decision_id}.}}}}{restores_access, select, yes { You can sign in and use {product_name} again.} other {}}",
		statement: '{statement, select, null {} other {Note from our team: {statement}}}',
		content_removed: "{content_removed, select, yes {Content we removed can't be restored.} other {}}",
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because a decision about your {product_name} account changed.',
	},
	enforcement_notice: {
		subject:
			'{neutral, select, yes {An update about your {product_name} account} other {{kind_label, select, warning {A warning about your {product_name} account} suspension {Your {product_name} account is suspended} ban {Your {product_name} account is banned} closure {Your {product_name} account is closed} feature_limit {Some features are limited on your {product_name} account} content_removal {We removed messages you sent on {product_name}} profile_reset {We reset parts of your {product_name} profile} community_removal {We removed your community on {product_name}} bot {An update about your bot on {product_name}} other {An update about your {product_name} account}}}}',
		intro: '{summary}',
		effects: '{effects}',
		rule_label: 'Rule',
		decision_label: 'Decision ID',
		why_title: 'Why',
		why_body: '{facts}',
		statement: '{statement, select, null {} other {Note from our team: {statement}}}',
		how_title: 'How we decided',
		how_body: '{how_we_decided}',
		appeal_title: 'How to appeal',
		appeal_body:
			'{appeals_email, select, null {} other {{reply, select, yes {If you think we got this wrong, reply to this email by {appeal_deadline}.} other {If you think we got this wrong, email <mail>{appeals_email}</mail> by {appeal_deadline} and include the decision ID.}} Appeals are free, and a person reviews each one.}}',
		redress: '{redress, select, null {} other {{redress}}}',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because we made a decision about your {product_name} account.',
	},
	gift_chargeback_notification: {
		subject: 'We removed the perks from a {product_name} gift you redeemed',
		intro: 'The payment for a gift code you redeemed was reversed, so we removed the perks it added to your account.',
		note: '{reply, select, yes {If you think this is a mistake, reply to this email and tell us what you know about the gift code.} other {If you think this is a mistake, contact the admins of this instance and tell them what you know about the gift code.}}',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because perks were removed from your {product_name} account.',
		billing_resumed:
			'Because the gift was removed, your {plan, select, yearly {yearly} other {monthly}} {premiumName} subscription is billed again from today. You\'ll get a separate receipt for the charge. You can cancel any time under "Manage subscription".',
		manage_cta: 'Manage your subscription',
	},
	harvest_completed: {
		subject: 'Your {product_name} data export is ready',
		intro: 'Your {product_name} data export is ready to download.',
		messages_label: 'Messages',
		messages: '{totalMessages, number}',
		size_label: 'File size',
		size: '{fileSizeMB, number} MB',
		cta: 'Download your data',
		expiry: 'Download it before {expiresAt, date, long}.',
		note: "{reply, select, yes {If you didn't ask for this export, change your password now and reply to this email.} other {If you didn't ask for this export, change your password now and contact the admins of this instance.}}",
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because a data export was requested for your {product_name} account.',
	},
	inactivity_warning: {
		subject: 'Sign in to keep your {product_name} account',
		intro:
			"Your {product_name} account has been inactive since {lastActiveDate, date, long}. Sign in before {deletionDate, date, long}, or we'll delete it.",
		cta: 'Sign in to {product_name}',
		note: '{reply, select, yes {If you used {product_name} recently, reply to this email.} other {If you used {product_name} recently, contact the admins of this instance.}}',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because your {product_name} account has been inactive for a long time.',
	},
	ip_authorization: {
		subject: 'Authorize a {product_name} sign-in from a new IP address',
		intro: 'Someone tried to sign in to your {product_name} account from a new IP address.',
		ip_label: 'IP address',
		location_label: 'Location',
		lead: 'If it was you, authorize this IP address.',
		cta: 'Authorize this IP address',
		expiry: 'This link expires in 30 minutes.',
		note: "If it wasn't you, change your password now.",
		signoff: 'The {product_name} team',
		footer_reason:
			'You received this email because someone tried to sign in to your {product_name} account from a new IP address.',
	},
	mfa_backup_codes_view: {
		subject: 'Confirm access to your {product_name} backup codes',
		intro: 'To view your {product_name} backup codes, enter this code in the app.',
		expiry: 'This code expires in {minutes, plural, one {# minute} other {# minutes}}.',
		note: "If you didn't ask for this, someone may have access to your account. Change your password now.",
		signoff: 'The {product_name} team',
		footer_reason:
			'You received this email because someone asked to view the backup codes on your {product_name} account.',
	},
	operator_pass_invoice: {
		subject: 'Your {product_name} Operator Pass invoice',
		heading: 'Invoice for your Operator Pass',
		preheader: '{amountFormatted} paid for the Operator Pass',
		intro:
			'Here is the invoice for your Operator Pass purchase. Your purchase confirmation has the details of what you bought.',
		invoice_label: 'Invoice number',
		date_label: 'Payment date',
		date_value: '{paidAt, date, long}',
		total_label: 'Total',
		tax_included: 'Includes {taxFormatted} tax',
		tax_added: '{taxFormatted} tax added to the price',
		reverse_charge: 'No VAT was charged because reverse charge applies. The invoice has the details.',
		lead: '{pdf, select, yes {The invoice is attached to this email as a PDF.} other {You can download the invoice as a PDF on the invoice page.}}',
		cta: 'View invoice',
		link_expiry:
			'{reply, select, yes {The invoice link works until {invoiceLinkUntil, date, long}. After that, reply to this email for a copy.} other {The invoice link works until {invoiceLinkUntil, date, long}. After that, ask the admins of this instance for a copy.}}',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because you bought an Operator Pass on {product_name}.',
	},
	operator_pass_offer_accepted: {
		subject: '{recipientName} accepted your {product_name} Operator Pass',
		intro: "{recipientName} accepted the Operator Pass you passed on, and it's on their account.",
		note: '{remainingCount, plural, =0 {You have no Operator Passes left, so your badge and wall listing are gone.} =1 {You have # Operator Pass left.} other {You have # Operator Passes left.}} Includes valid passes you have and passes you bought and passed on. Keep at least one active pass to retain your badge and wall listing. Paused or ended passes do not count.',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because you passed on an Operator Pass on {product_name}.',
	},
	operator_pass_offer_ended: {
		subject:
			'{ended, select, withdrawn {An Operator Pass offer was taken back} declined {Your Operator Pass offer was declined} other {An Operator Pass offer was canceled}}',
		intro:
			"{ended, select, withdrawn {{otherName} took back their Operator Pass offer.} declined {{otherName} declined your Operator Pass. It's still yours, and you can offer it to someone else.} other {The Operator Pass offer with {otherName} was canceled because the pass or its payment changed.}}",
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because of an Operator Pass offer on {product_name}.',
	},
	operator_pass_offer_received: {
		subject: '{senderName} wants to pass you an Operator Pass',
		intro: '{senderName} wants to pass you one of their Operator Passes.',
		lead: 'Accept or decline it in {product_name}. If you accept, it arrives on {arrivesAt, date, long}.',
		cta: 'View the offer',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because someone offered you an Operator Pass on {product_name}.',
	},
	operator_pass_perks_removed: {
		subject: 'We removed your {product_name} Operator Pass benefits',
		intro:
			"{scope, select, permanent {We removed your Operator Pass benefits permanently.} other {We removed your Operator Pass benefits until {until, date, long}, when they come back with your original badge date.}} This includes your wall listing, badge, #operators and the Operator role. Your {product_name} account isn't suspended.",
		rule_label: 'Rule',
		facts_label: 'Facts we relied on',
		ground_label: 'Ground',
		ground: 'Section 8 of the Operator Pass terms',
		automated_text:
			'{automated, select, yes {Automated tools helped us find this. A person on our team made the decision.} other {A person on our team made the decision without automated tools.}}',
		refund:
			"{scope, select, permanent {You can't buy or receive the Operator Pass again while this decision stands. } other {}}Removed benefits aren't refunded. If you still have a right of withdrawal, this decision doesn't take it away.",
		appeal_title: 'How to appeal',
		appeal_body:
			"{reply, select, yes {Reply to this email within 60 days and tell us why you disagree.} other {Contact the admins of this instance within 60 days and tell them why you disagree.}} It's free, and someone who didn't make this decision reviews it.",
		redress:
			"You can also use a certified <dsa>out-of-court dispute settlement body</dsa> or go to court. If you're a consumer, you can take the dispute to the Swedish National Board for Consumer Disputes (<arn>ARN</arn>), and we take part.",
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because of an Operator Pass decision on your {product_name} account.',
	},
	operator_pass_purchase_confirmation: {
		subject: 'Your {product_name} Operator Pass receipt',
		intro:
			'Thank you for your order. This email is your receipt, and the Operator Pass terms you accepted (version {termsVersion}) are attached as a PDF.',
		order_number_label: 'Order number',
		order_date_label: 'Order date',
		order_date_value: '{orderDate, date, long}',
		product_label: 'Product',
		product_value: '{quantity, plural, one {{product_name} Operator Pass} other {# {product_name} Operator Passes}}',
		unit_price_label: 'Price per pass',
		price_label: 'Total',
		currency_label: 'Currency',
		price_value: '{priceFormatted}, paid once',
		payment_label: 'Payment',
		included_heading: 'What you get',
		included_perks:
			"A spot on the Operators' wall, whenever you want it, and the Operator badge, for life, as long as we run {product_name}.",
		included_pass_on: 'Each pass can be passed on once.',
		bonus_heading: 'Bonus',
		bonus_line:
			"While we run it, you also get the Operator role and #operators in Fluxer Labs. The bonus isn't part of the price and can change or end.",
		consent_heading: 'Your consent',
		consent_line: 'At checkout on {waiverConsentAt, date, long}, you checked the box "{consentText}"',
		withdrawal_heading: 'Right of withdrawal',
		withdrawal_waived:
			'{quantity, plural, =1 {Your right of withdrawal ended when we delivered your pass, as you asked.} other {Your right of withdrawal ended when we delivered your passes, as you asked.}}',
		withdrawal_cancel_until:
			'{quantity, plural, =1 {You can cancel until {withdrawalEndsOn, date, long} for a full refund.} other {You can cancel each pass until {withdrawalEndsOn, date, long} for a full refund.}} Use Cancel next to the pass in your <settings>settings</settings>.',
		withdrawal_rule_cancel_until:
			'{quantity, plural, =1 {A rule for your country replaces the box you checked, so you can still cancel until {withdrawalEndsOn, date, long} for a full refund.} other {A rule for your country replaces the box you checked, so you can still cancel each pass until {withdrawalEndsOn, date, long} for a full refund.}} Use Cancel next to the pass in your <settings>settings</settings>.',
		seller: 'Seller: Fluxer Platform AB, 559537-3993, <mail>{sellerEmail}</mail>',
		terms_link: 'Operator Pass terms',
		wall_status:
			"{wallOptOut, select, yes {You chose not to be listed. You can add your name to the wall anytime in your <settings>settings</settings>.} other {You're listed as {wallName}. You can change it or leave the wall in your <settings>settings</settings>.}}",
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because you bought an Operator Pass on {product_name}.',
	},
	operator_pass_refund_receipt: {
		subject: 'Your {product_name} Operator Pass refund',
		heading: 'Refund of {amountFormatted}',
		preheader: '{amountFormatted} refunded for your Operator Pass',
		intro:
			'We refunded {amountFormatted} for your Operator Pass purchase to the card or account you paid with. It usually takes 5 to 10 business days to show up, depending on your bank.',
		order_number_label: 'Order number',
		order_date_label: 'Order date',
		order_date_value: '{orderDate, date, long}',
		date_label: 'Refund date',
		date_value: '{refundedAt, date, long}',
		refund_label: 'Refunded',
		lead: '{full, select, yes {The passes from this purchase have ended, and so have the badge, wall listing and bonus that came with them.} other {The passes this refund covers have ended. Any other passes from the purchase stay.}}',
		note: '{reply, select, yes {Questions about this refund? Reply to this email.} other {If you have questions about this refund, contact the admins of this instance.}}',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because an Operator Pass payment on {product_name} was refunded.',
	},
	operator_pass_removal_notice: {
		subject: 'We intend to remove your {product_name} Operator Pass',
		intro:
			'We intend to remove your Operator Pass permanently. You have {replyDays, plural, one {# day} other {# days}} to reply before we decide.',
		rule_label: 'Rule',
		facts_label: 'Facts we relied on',
		ground_label: 'Ground',
		ground: 'Section 8 of the Operator Pass terms',
		reply:
			'{reply, select, yes {Reply to this email and tell us anything we should know.} other {Contact the admins of this instance and tell them anything we should know.}} A person on our team reads every reply before we decide. Until then, your wall listing, badge, #operators and the Operator role stay as they are.',
		next: "If we go ahead, we email you the decision and how to appeal it. If you're a consumer, you can also take a dispute about a removal to the Swedish National Board for Consumer Disputes (<arn>ARN</arn>), and we take part.",
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because of an Operator Pass decision on your {product_name} account.',
	},
	operator_pass_revoked: {
		subject: 'An Operator Pass on your {product_name} account has ended',
		intro:
			"{cause, select, dispute {The payment for an Operator Pass you received was reversed in a payment dispute, so the pass has ended.} withdrawal {The person who bought an Operator Pass you received withdrew from the purchase, so the pass has ended.} other {The payment for an Operator Pass you received was refunded, so the pass has ended.}} This isn't about anything you did.",
		note: '{lastPass, select, yes {It was your last pass, so your badge and wall listing are gone.} other {Your other passes stay.}}',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because an Operator Pass on your {product_name} account ended.',
	},
	operator_pass_sale_open: {
		subject: 'The {product_name} Operator Pass is on sale',
		intro: "You asked us to tell you when the Operator Pass goes on sale. It's on sale now.",
		price_label: 'Early bird price',
		price_value: '{priceFormatted}, paid once, until {priceEndsOn, date, long}',
		included_perks:
			"A spot on the Operators' wall, whenever you want it, and the Operator badge, for life, as long as we run {product_name}.",
		included_bonus:
			"While we run it, you also get the Operator role and #operators in Fluxer Labs. The bonus isn't part of the price.",
		cta: 'Get the Operator Pass',
		terms_link: 'Operator Pass terms',
		signoff: 'The {product_name} team',
		footer_reason:
			'You received this email because you joined the Operator Pass waitlist. This is the last email we send about the pass.',
		unsubscribe: 'Unsubscribe',
	},
	operator_pass_waitlist_verification: {
		subject: 'Confirm your {product_name} Operator Pass waitlist signup',
		intro:
			"Confirm this email address to join the Operator Pass waitlist. We'll email you once, when the pass goes on sale.",
		cta: 'Join the waitlist',
		expiry: 'This link expires in 48 hours.',
		note: "If you didn't ask for this, ignore this email and we won't contact you again.",
		signoff: 'The {product_name} team',
		footer_reason:
			'You received this email because this address was entered on the Operator Pass waitlist on the {product_name} website.',
	},
	operator_pass_wall_hidden: {
		subject: "We took your listing off the {product_name} Operators' wall for now",
		intro:
			"We took your listing off the Operators' wall for now. Your passes and badge stay, and nothing is withdrawn or refunded.",
		reason_label: 'Reason',
		reason_value:
			'{reason, select, impersonation {Pretends to be someone else} rights {Uses a name or image without the rights to it} age_suitability {Not suitable for younger users} other {Breaks the community guidelines}}',
		facts_label: 'Facts we relied on',
		ground_label: 'Ground',
		ground: 'Section 8 of the Operator Pass terms',
		automated_text: 'A person on our team made the decision without automated tools.',
		next: 'Your listing comes back at its size and place if we show it again or your appeal succeeds.',
		appeal_title: 'How to appeal',
		appeal_body:
			"{reply, select, yes {Reply to this email within 60 days and tell us why you disagree.} other {Contact the admins of this instance within 60 days and tell them why you disagree.}} It's free, and someone who didn't make this decision reviews it.",
		redress:
			"You can also use a certified <dsa>out-of-court dispute settlement body</dsa> or go to court. If you're a consumer, you can take the dispute to the Swedish National Board for Consumer Disputes (<arn>ARN</arn>), and we take part.",
		signoff: 'The {product_name} team',
		footer_reason:
			"You received this email because of a decision about your listing on the {product_name} Operators' wall.",
	},
	operator_pass_wall_name_replaced: {
		subject: "We declined your name or avatar for the {product_name} Operators' wall",
		intro:
			"{target, select, avatar {We declined your avatar for the Operators' wall.} both {We declined your name and avatar for the Operators' wall.} other {We declined your name for the Operators' wall.}} {shown, select, previous {The wall keeps your last approved version.} username {Until you send a new one, the wall shows your username and the default avatar.} other {Until you send a new one, the wall shows the word Operator and the default avatar.}} You keep your spot, size and badge, and nothing is withdrawn or refunded.",
		reason_label: 'Reason',
		reason_value:
			'{reason, select, impersonation {Pretends to be someone else} rights {Uses a name or image without the rights to it} age_suitability {Not suitable for younger users} other {Breaks the community guidelines}}',
		facts_label: 'Facts we relied on',
		ground_label: 'Ground',
		ground: 'Section 2.1 of the Operator Pass terms',
		automated_text: 'A person on our team made the decision without automated tools.',
		next: 'You can send a new name or avatar anytime in your <settings>settings</settings>.',
		appeal_title: 'How to appeal',
		appeal_body:
			"{reply, select, yes {Reply to this email within 60 days and tell us why you disagree.} other {Contact the admins of this instance within 60 days and tell them why you disagree.}} It's free, and someone who didn't make this decision reviews it.",
		redress:
			"You can also use a certified <dsa>out-of-court dispute settlement body</dsa> or go to court. If you're a consumer, you can take the dispute to the Swedish National Board for Consumer Disputes (<arn>ARN</arn>), and we take part.",
		signoff: 'The {product_name} team',
		footer_reason:
			"You received this email because of a decision about your listing on the {product_name} Operators' wall.",
	},
	operator_pass_welcome: {
		subject: 'Your {product_name} Operator Pass is active',
		intro: 'Your Operator Pass is active. Thank you for backing {product_name}.',
		order_number_label: 'Order number',
		delivered_label: 'Delivered',
		delivered_value: '{deliveredAt, date, long}',
		pass_count_label: 'Operator Passes',
		wall_line:
			"{wallOptOut, select, yes {You chose not to be listed on the Operators' wall. You can add your name anytime in your <settings>settings</settings>.} other {You're listed on the Operators' wall as {wallName}, and your name and avatar show once we check them. You can change it or leave the wall in your <settings>settings</settings>.}}",
		badge_line: 'Your badge is on your profile. You can hide it in your <settings>settings</settings>.',
		labs_line: 'In Fluxer Labs you get the Operator role and #operators. This bonus lasts while we run it.',
		cta: 'Open Fluxer Labs',
		terms_link: 'Operator Pass terms',
		signoff: '{founderName} and the {product_name} team',
		footer_reason: 'You received this email because an Operator Pass was added to your {product_name} account.',
	},
	operator_pass_withdrawal_receipt: {
		subject:
			'{cancellation, select, yes {Your {product_name} Operator Pass is canceled} other {We received your {product_name} Operator Pass withdrawal}}',
		intro:
			'{cancellation, select, yes {We received your cancellation on {receivedAt, date, long} at {receivedAt, time, short}.} other {We received your withdrawal on {receivedAt, date, long} at {receivedAt, time, short}.}}',
		name_label: 'Name',
		order_number_label: 'Order number',
		order_date_label: 'Order date',
		order_date_value: '{orderDate, date, long}',
		refund_label: 'Refund',
		currency_label: 'Currency',
		refund:
			'{refundViaDispute, select, yes {Your bank opened a payment dispute for this purchase first, so we accepted the dispute and your bank returns the full {amountFormatted} to you.} other {We started a full refund of {amountFormatted} to your payment method, and you get it within 14 days.}} {lastPass, select, yes {The pass has ended, and so have your badge, wall listing and the bonus.} other {The pass has ended. Your other passes stay.}}',
		donation: 'The monthly donation you added at checkout is separate and keeps running. You can cancel it anytime.',
		cta: 'Manage your donation',
		note: 'Keep this email as your receipt.',
		signoff: 'The {product_name} team',
		footer_reason:
			'{cancellation, select, yes {You received this email because you canceled an Operator Pass purchase on {product_name}.} other {You received this email because you withdrew from an Operator Pass purchase on {product_name}.}}',
	},
	password_change_verification: {
		subject: 'Confirm your {product_name} password change',
		intro: 'To change your {product_name} password, enter this code in the app.',
		expiry: 'This code expires in {minutes, plural, one {# minute} other {# minutes}}.',
		note: "If you didn't ask for this, someone may have access to your account. Change your password now and turn on two-factor authentication.",
		signoff: 'The {product_name} team',
		footer_reason:
			'You received this email because someone asked to change the password on your {product_name} account.',
	},
	password_reset: {
		subject: 'Reset your {product_name} password',
		intro: 'You asked to reset your {product_name} password.',
		ip_label: 'IP address',
		location_label: 'Location',
		cta: 'Set a new password',
		expiry: 'This link expires in 1 hour.',
		note: "If you didn't ask for this, you can ignore this email.",
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because a password reset was requested for your {product_name} account.',
	},
	payment_dispute_warning: {
		subject: 'About your payment dispute on {product_name}',
		intro:
			'You disputed your {product, select, plutonium {Plutonium payment} other {Operator Pass payment}} with your bank or card company. A person on our team checked the dispute against our terms and found it unjustified.',
		order_number_label: 'Order number',
		order_date_label: 'Order date',
		order_date_value: '{orderDate, date, long}',
		amount_label: 'Amount disputed',
		currency_label: 'Currency',
		ground_label: 'Ground',
		ground:
			'{product, select, plutonium {Section 4.4 of the terms of service} other {Section 9 of the Operator Pass terms}}',
		deadline_label: 'Withdraw the dispute before',
		deadline_value: '{warningDate, date, long}',
		deadline_body:
			"If the dispute isn't withdrawn before {warningDate, date, long} and it ends with the money returned to you, we'll permanently delete your {product_name} account. A deleted account can't be restored. If we close your account, you also can't create or use other {product_name} accounts.",
		why_title: 'Why we found it unjustified',
		why_body:
			'You or someone you allowed made the payment, we charged the right amount, {product, select, plutonium {Plutonium was provided as described} other {the Operator Pass was delivered as described}}, we had not refunded the payment, and the withdrawal period had ended when you disputed it.',
		next_title: 'What happens now',
		next_body:
			"If you made this payment, you can ask your bank or card company to withdraw the dispute. If the dispute is withdrawn or closes without the money going back to you, nothing happens to your account. Until the dispute closes, you can't buy anything on {product_name}.{privacy_email, select, null {} other { You can ask for a copy of your data anytime by writing to <mail>{privacy_email}</mail>.}}",
		appeal_title: 'If you think we got this wrong',
		appeal_body:
			'{reply, select, yes {Reply to this email and tell us why.} other {{appeals_email, select, null {Contact the admins of this instance and tell them why.} other {Write to <mail>{appeals_email}</mail> and tell us why.}}}} A person who did not make this decision looks at it again, and if the dispute was justified, we take no action against your account.',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because of a payment dispute about your {product_name} account.',
	},
	plutonium_cancellation_confirmation: {
		subject: "We've received your {product_name} {premiumName} cancellation",
		heading: 'Cancellation received',
		preheader: 'You keep {premiumName} until {endsAt, date, long}',
		intro:
			'We received your request to cancel your {plan, select, yearly {yearly} other {monthly}} {premiumName} subscription on {receivedFormatted}.',
		ends_line: "You keep {premiumName} until {endsAt, date, long}. Your subscription then ends and won't renew.",
		charge_line:
			"{unpaid, select, yes {One invoice for {unpaidFormatted} is still unpaid. Canceling doesn't remove it, and we'll keep trying to collect it.} other {We won't charge you again.}}",
		lead: 'Changed your mind? You can resume your subscription under "Manage subscription" before {endsAt, date, long}.',
		cta: 'Manage your subscription',
		reference_line: 'Reference: {reference}',
		seller: 'Seller: Fluxer Platform AB, 559537-3993, {sellerAddress}, <mail>{sellerEmail}</mail>',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because you canceled your {premiumName} subscription.',
	},
	plutonium_card_expiring: {
		subject: 'Your card expires before your next {product_name} {premiumName} payment',
		heading: 'Update your card',
		preheader:
			'{branded, select, yes {Your {brand} ending in {last4}} other {Your card ending in {last4}}} expires soon',
		intro:
			'{branded, select, yes {Your {brand} ending in {last4}} other {Your card ending in {last4}}} expires at the end of {expiryFormatted}. Your next payment of {tax, select, yes {{amountFormatted}} other {{amountFormatted}, plus any tax that applies,}} is due on {chargeAt, date, long}.',
		lead: 'Update your card before then so your subscription keeps working.',
		cta: 'Update your payment method',
		seller: 'Seller: Fluxer Platform AB, 559537-3993, {sellerAddress}, <mail>{sellerEmail}</mail>',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because a card you use for {premiumName} is about to expire.',
	},
	plutonium_ended_unpaid: {
		subject: 'Your {product_name} {premiumName} subscription has ended',
		heading: 'Your subscription has ended',
		preheader: "We couldn't collect the payment for your {premiumName} subscription",
		intro:
			"{numbered, select, yes {Your {premiumName} subscription has ended because we couldn't collect the payment for invoice {invoiceNumber}.} other {Your {premiumName} subscription has ended because we couldn't collect the renewal payment.}}",
		owed_line:
			"{owed, select, none {You don't owe anything for this invoice.} stopped {We've stopped trying to collect this invoice.} open {The invoice for {amountFormatted} is still unpaid. You can pay it on the invoice page.} other {}}",
		invoice_link: 'View the invoice',
		lead: 'You can subscribe again any time.',
		cta: 'Get {premiumName}',
		seller: 'Seller: Fluxer Platform AB, 559537-3993, {sellerAddress}, <mail>{sellerEmail}</mail>',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because your {premiumName} subscription ended.',
	},
	plutonium_gift_applied: {
		subject: 'Your gift has been added to your {product_name} {premiumName} subscription',
		heading: 'Gift added',
		preheader: "You won't be charged until {trialEnd, date, long}",
		intro:
			"Your gifted time has been added. You won't be charged for your {plan, select, yearly {yearly} other {monthly}} {premiumName} subscription until {trialEnd, date, long}.",
		charge_line:
			"{method, select, card {On {trialEnd, date, long} we'll charge {tax, select, yes {{amountFormatted}} other {{amountFormatted}, plus any tax that applies,}} to your {brand} ending in {last4}, and your subscription renews every {plan, select, yearly {year} other {month}} until you cancel.} plain_card {On {trialEnd, date, long} we'll charge {tax, select, yes {{amountFormatted}} other {{amountFormatted}, plus any tax that applies,}} to your card ending in {last4}, and your subscription renews every {plan, select, yearly {year} other {month}} until you cancel.} named {On {trialEnd, date, long} we'll charge {tax, select, yes {{amountFormatted}} other {{amountFormatted}, plus any tax that applies,}} using {methodName}, and your subscription renews every {plan, select, yearly {year} other {month}} until you cancel.} other {On {trialEnd, date, long} we'll charge {tax, select, yes {{amountFormatted}} other {{amountFormatted}, plus any tax that applies,}} to your saved payment method, and your subscription renews every {plan, select, yearly {year} other {month}} until you cancel.}}",
		lead: 'You can cancel any time before then under "Manage subscription".',
		cta: 'Manage your subscription',
		seller: 'Seller: Fluxer Platform AB, 559537-3993, {sellerAddress}, <mail>{sellerEmail}</mail>',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because you redeemed a {premiumName} gift.',
	},
	plutonium_payment_failed: {
		subject:
			"{reason, select, action_required {Confirm your {product_name} {premiumName} payment} other {Your {product_name} {premiumName} payment didn't go through}}",
		heading:
			"{reason, select, action_required {Your bank needs you to confirm a payment} other {Your {premiumName} payment didn't go through}}",
		preheader:
			"{reason, select, action_required {Confirm {amountFormatted} for {premiumName}} other {We couldn't charge {amountFormatted} for {premiumName}}}",
		intro:
			"{reason, select, action_required {Your bank wants you to confirm the renewal payment for your {premiumName} subscription.} other {We couldn't charge the renewal payment for your {premiumName} subscription.}} {access, select, yes {You keep {premiumName} until {accessEndsAt, date, long}.} other {}}",
		invoice_label: 'Invoice number',
		date_label: 'Payment attempted',
		date_value: '{attemptedAt, date, long}',
		amount_label: 'Amount',
		lead: '{reason, select, action_required {Open the invoice page and confirm the payment there.} other {{access, select, yes {To keep it after that, pay the invoice or update your payment method.} other {To get {premiumName} back, pay the invoice or update your payment method.}}}}',
		cta: '{reason, select, action_required {Confirm the payment} other {Pay the invoice}}',
		manage_link: 'Update your payment method under "Manage subscription"',
		note: '{reply, select, yes {If you need help, reply to this email.} other {If you need help, contact the admins of this instance.}}',
		signoff: 'The {product_name} team',
		footer_reason:
			'You received this email because a payment for your {premiumName} subscription on {product_name} needs your attention.',
		reminder_subject: 'Reminder to confirm your payment for {premiumName}',
		intro_change:
			"{reason, select, action_required {Your bank wants you to confirm the payment for the change to your {premiumName} subscription.} other {We couldn't charge the payment for the change to your {premiumName} subscription.}} {access, select, yes {You keep {premiumName} until {accessEndsAt, date, long}.} other {}}",
	},
	plutonium_payment_receipt: {
		subject:
			'{gift, select, yes {Your {product_name} {premiumName} gift receipt} other {Your {product_name} {premiumName} receipt}}',
		heading: '{gift, select, yes {Receipt for your {premiumName} gift} other {Receipt for {premiumName}}}',
		preheader: '{amountFormatted} paid for {premiumName}',
		intro:
			'{gift, select, yes {Thank you for buying a {premiumName} gift. This email is your receipt.} other {Thank you for your payment. This email is your receipt.}}',
		invoice_label: 'Invoice number',
		date_label: 'Payment date',
		date_value: '{paidAt, date, long}',
		product_label: 'Product',
		product_value:
			'{plan, select, monthly {{premiumName}, monthly} yearly {{premiumName}, yearly} gift_1_month {{premiumName} gift, 1 month} gift_1_year {{premiumName} gift, 1 year} other {{premiumName}}}',
		period_label: 'Period',
		period_value: '{periodStart, date, long} to\n{periodEnd, date, long}',
		total_label: 'Total',
		tax_included: 'Includes {taxFormatted} tax',
		tax_added: '{taxFormatted} tax added to the price',
		reverse_charge: 'No VAT was charged because reverse charge applies. The invoice has the details.',
		credit_label: 'Credit applied',
		paid_label: 'Amount paid',
		gift_note:
			'Your gift link is in your settings under "Gifts". Copy it there and send it to the person you want to give {premiumName} to. Anyone with the link can redeem it.',
		lead: '{pdf, select, yes {The invoice is attached to this email as a PDF.} other {On the invoice page you can see the tax details and download the invoice as a PDF.}}',
		cta: 'View invoice',
		link_expiry:
			'{reply, select, yes {The invoice link works until {invoiceLinkUntil, date, long}. After that, reply to this email for a copy.} other {The invoice link works until {invoiceLinkUntil, date, long}. After that, ask the admins of this instance for a copy.}}',
		plan_title: '{gift, select, yes {Your gift} other {Your subscription}}',
		plan_body:
			'{renewal, select, month {{premiumName} renews every month at {renewalFormatted}{renewalTax, select, included { including tax} added { plus tax} other {}} until you cancel. You can cancel at any time under "Manage subscription" on the {premiumName} page, and you keep {premiumName} until the end of the period you paid for.} year {{premiumName} renews every year at {renewalFormatted}{renewalTax, select, included { including tax} added { plus tax} other {}} until you cancel. You can cancel at any time under "Manage subscription" on the {premiumName} page, and you keep {premiumName} until the end of the period you paid for.} other {This gift is a one-time purchase. It doesn\'t renew, and nothing more is charged.}}',
		consent_title: 'Starting right away',
		consent_body:
			'On {consentAt, date, long} you asked for {premiumName} to start right away. You also confirmed that you can still withdraw for a full refund until your withdrawal period ends.',
		withdrawal_title: 'Right of withdrawal',
		withdrawal_body:
			"You can withdraw from this purchase until {withdrawalEndsAt, date, long} and get a full refund. To withdraw, reply to this email or write to <mail>{sellerEmail}</mail> and tell us that you withdraw. You can use the form below, but you don't have to.",
		withdrawal_note:
			"The refund button in the app works for 3 days after a payment. It's an extra on top of your right of withdrawal and doesn't replace it.",
		form_title: 'Withdrawal form',
		form_body:
			'To Fluxer Platform AB, {sellerAddress}, <mail>{sellerEmail}</mail>:\nI withdraw from my contract for {premiumName}, bought on {paidAt, date, long}.\nInvoice number: {invoiceNumber}\nName:\nAddress:\nDate:\nSignature (only on paper):',
		seller: 'Seller: Fluxer Platform AB, 559537-3993, {sellerAddress}, <mail>{sellerEmail}</mail>',
		manage_link: 'Manage your subscription',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because you paid for {premiumName} on {product_name}.',
	},
	plutonium_refund_receipt: {
		subject: 'Your {product_name} {premiumName} refund',
		heading: 'Refund of {amountFormatted}',
		preheader: '{amountFormatted} refunded for {premiumName}',
		intro:
			'{partial, select, yes {We refunded {amountFormatted}, part of your payment, to the card or account you paid with.} other {We refunded {amountFormatted} to the card or account you paid with.}} It usually takes 5 to 10 business days to show up, depending on your bank.',
		invoice_label: 'Invoice number',
		date_label: 'Refund date',
		date_value: '{refundedAt, date, long}',
		product_label: 'Product',
		product_value:
			'{plan, select, monthly {{premiumName}, monthly} yearly {{premiumName}, yearly} gift_1_month {{premiumName} gift, 1 month} gift_1_year {{premiumName} gift, 1 year} other {{premiumName}}}',
		original_label: 'Original payment',
		original_value: '{paidFormatted} on {paidAt, date, long}',
		refund_label: 'Refunded',
		cta: 'View refund details',
		note: '{reply, select, yes {Questions about this refund? Reply to this email.} other {If you have questions about this refund, contact the admins of this instance.}}',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because a {premiumName} payment on {product_name} was refunded.',
		ended_line:
			"We received your refund request on {requestedFormatted}. The refund also canceled your {premiumName} subscription. It has ended and won't renew. Reference: {reference}.",
	},
	plutonium_renewal_notice: {
		subject:
			'{gift, select, yes {Your gifted {product_name} {premiumName} time ends on {giftEndsAt, date, long}} other {{annual, select, yes {Your yearly reminder about your {product_name} {premiumName} subscription} other {Your {product_name} {premiumName} subscription renews on {chargeAt, date, long}}}}}',
		heading:
			'{gift, select, yes {Your gifted time ends soon} other {{annual, select, yes {Your yearly renewal reminder} other {Your subscription renews soon}}}}',
		preheader: 'Your {premiumName} subscription renews on {chargeAt, date, long}',
		annual_line:
			'This is your yearly reminder that your monthly {premiumName} subscription renews automatically every month until you cancel.',
		gift_line:
			'Your gifted time ends on {giftEndsAt, date, long}. After that, your {plan, select, yearly {yearly} other {monthly}} {premiumName} subscription renews automatically unless you cancel before then.',
		intro:
			'Your {plan, select, yearly {yearly} other {monthly}} {premiumName} subscription renews automatically on {chargeAt, date, long} unless you cancel before then.',
		charge_line:
			"{method, select, card {On {chargeAt, date, long} we'll charge {tax, select, yes {{nextFormatted}} other {{nextFormatted}, plus any tax that applies,}} to your {brand} ending in {last4}.} plain_card {On {chargeAt, date, long} we'll charge {tax, select, yes {{nextFormatted}} other {{nextFormatted}, plus any tax that applies,}} to your card ending in {last4}.} named {On {chargeAt, date, long} we'll charge {tax, select, yes {{nextFormatted}} other {{nextFormatted}, plus any tax that applies,}} using {methodName}.} other {On {chargeAt, date, long} we'll charge {tax, select, yes {{nextFormatted}} other {{nextFormatted}, plus any tax that applies,}} to your saved payment method.}}",
		year_line: 'The charge on {yearStartsAt, date, long} starts your next year of {premiumName}.',
		after_line:
			'{differs, select, yes {After that, the price is {tax, select, yes {{regularFormatted}} other {{regularFormatted}, plus any tax that applies,}} every {plan, select, yearly {year} other {month}}.} other {After that, you pay {tax, select, yes {{regularFormatted}} other {{regularFormatted}, plus any tax that applies,}} every {plan, select, yearly {year} other {month}} until you cancel.}}',
		box: 'To stop the renewal, cancel before {cancelByFormatted}.',
		cta: 'Manage your subscription',
		keep_line: "If you cancel, you keep {premiumName} until {chargeAt, date, long} and we won't charge you again.",
		seller: 'Seller: Fluxer Platform AB, 559537-3993, {sellerAddress}, <mail>{sellerEmail}</mail>',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because your {premiumName} subscription renews automatically.',
	},
	reauth_code: {
		subject: 'Your {product_name} confirmation code',
		intro: "Enter this code in the app to confirm it's you before you change your account.",
		expiry: 'This code expires in {minutes, plural, one {# minute} other {# minutes}}.',
		note: "Don't share this code with anyone. {product_name} staff will never ask for it. If you didn't ask for it, someone may be using your account. Sign out of every session and check your account security.",
		signoff: 'The {product_name} team',
		footer_reason:
			'You received this email because a change to your {product_name} account asked for a confirmation code.',
	},
	registration_approved: {
		subject: 'Your {product_name} account is approved',
		intro: 'Your sign-up on {product_name} was approved. You can sign in now.',
		cta: 'Sign in to {product_name}',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because this address was used for a {product_name} account.',
	},
	registration_pending: {
		subject: 'New sign-ups are waiting for approval on {product_name}',
		intro: 'New accounts on {product_name} are waiting for approval.',
		cta: 'Review pending registrations',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because you can approve new accounts on {product_name}.',
	},
	report_received: {
		subject: 'We received your {product_name} report',
		intro:
			'We received your Digital Services Act report about {targetKind, select, message {a message} user {an account} guild {a community} other {content}} on {product_name}.',
		report_label: 'Report ID',
		next: "We'll email you at this address once we decide.",
		signoff: 'The {product_name} safety team',
		footer_reason: 'You received this email because you submitted a Digital Services Act report on {product_name}.',
	},
	report_resolved: {
		subject: 'Your {product_name} report has been reviewed',
		intro:
			"{outcome, select, removed {We removed the content you reported because it broke our rules.} restricted {The account you reported broke our rules, and we took action on it. We don't share which action we took.} action_taken {What you reported broke our rules, and we took action. We don't share which action we took.} no_violation {What you reported doesn't break our rules, so we left it as it is.} other {We closed your report.}}",
		about:
			'{target, select, message_community {This is about your report on a message in #{channelName} in "{communityName}", sent on {reportedAt, date, long}.} message_dm {This is about your report on a message in a direct message, sent on {reportedAt, date, long}.} message {This is about your report on a message, sent on {reportedAt, date, long}.} user {This is about your report on the account "{username}", sent on {reportedAt, date, long}.} community {This is about your report on the community "{communityName}", sent on {reportedAt, date, long}.} other {This is about the report you sent on {reportedAt, date, long}.}}',
		thanks:
			'{outcome, select, removed {Thanks for reporting it. Reports like yours help keep {product_name} safe for everyone.} restricted {Thanks for reporting it. Reports like yours help keep {product_name} safe for everyone.} action_taken {Thanks for reporting it. Reports like yours help keep {product_name} safe for everyone.} other {Thanks for letting us know.}}',
		report_label: 'Report ID',
		comment_label: 'Note from our team',
		note: '{reply, select, yes {If you disagree, reply to this email within 60 days.} other {{appeals_email, select, null {} other {If you disagree, email <mail>{appeals_email}</mail> within 60 days and include the report ID.}}}}{appeals_email, select, null {} other { In the EU, you can also go to a certified <dsa>out-of-court dispute settlement body</dsa> or complain to the Digital Services Coordinator in your country.}}',
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because you submitted a report on {product_name}.',
	},
	self_deletion_scheduled: {
		subject: 'Your {product_name} account will be deleted on {deletionDate, date, long}',
		intro: "As you asked, we'll delete your {product_name} account on {deletionDate, date, long}.",
		note: "If you didn't ask for this, sign in before then to cancel it, and change your password.",
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because you asked to delete your {product_name} account.',
	},
	sign_in_code: {
		subject: 'Your {product_name} sign-in code',
		intro: 'Use this code to sign in to {product_name}.',
		location_label: 'Requested from',
		device_label: 'Device',
		expiry: 'This code expires in {minutes, plural, one {# minute} other {# minutes}}.',
		note: "Don't share this code with anyone. {product_name} staff will never ask for it. If you didn't try to sign in, you can ignore this email.",
		signoff: 'The {product_name} team',
		footer_reason: 'You received this email because someone asked to sign in to {product_name} with this address.',
	},
	sign_up_code: {
		subject: 'Your {product_name} sign-up code',
		intro: 'Use this code to finish creating your {product_name} account.',
		location_label: 'Requested from',
		device_label: 'Device',
		expiry: 'This code expires in {minutes, plural, one {# minute} other {# minutes}}.',
		note: "Don't share this code with anyone. {product_name} staff will never ask for it. If you didn't try to create an account, you can ignore this email.",
		signoff: 'The {product_name} team',
		footer_reason:
			'You received this email because someone asked to create a {product_name} account with this address.',
	},
} as const;
