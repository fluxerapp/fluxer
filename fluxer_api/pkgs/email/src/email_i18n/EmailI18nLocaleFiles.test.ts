import {extractMessageTemplatePlaceholders} from '@fluxer/i18n/src/runtime/MessageCatalogTypes';
import {getEmailTemplate} from '@pkgs/email/src/email_i18n/EmailI18n';
import {EMAIL_I18N_LOCALE_MESSAGES} from '@pkgs/email/src/email_i18n/EmailI18nLocales';
import {EMAIL_I18N_MESSAGES} from '@pkgs/email/src/email_i18n/EmailI18nMessages';
import type {EmailTemplateVariables} from '@pkgs/email/src/email_i18n/EmailI18nTypes';
import type {EmailTemplate, EmailTemplateKey} from '@pkgs/email/src/email_i18n/EmailI18nTypes.generated';
import {describe, expect, it} from 'vitest';

const LOCALES = Object.keys(EMAIL_I18N_LOCALE_MESSAGES) as Array<keyof typeof EMAIL_I18N_LOCALE_MESSAGES>;
const TEMPLATE_KEYS = Object.keys(EMAIL_I18N_MESSAGES) as Array<EmailTemplateKey>;
const DATE = new Date('2026-10-01T23:30:00Z');

const FIXTURE: {[K in EmailTemplateKey]: EmailTemplateVariables[K]} = {
	account_deletion_cancelled: {},
	account_deletion_scheduled_inactivity: {deletion_date: 'Dec 1, 2026'},
	account_deletion_scheduled_requested: {deletion_date: 'Dec 14, 2026'},
	bulk_message_deletion_completed: {message_count: 1240, channel_count: 18},
	donation_confirmation: {amount: '$5.00', interval: 'month'},
	donation_magic_link: {minutes: 15},
	dsa_report_resolved: {
		outcome: 'removed',
		appeals_email: 'appeals@fluxer.com',
		target: 'message_community',
		reportedAt: DATE,
		username: 'example',
		communityName: 'Fluxer HQ',
		channelName: 'general',
	},
	dsa_report_verification: {minutes: 10},
	email_change_new: {minutes: 10},
	email_change_original: {minutes: 10},
	email_change_revert: {},
	email_verification: {},
	email_verification_code: {minutes: 10},
	enforcement_ended: {
		outcome: 'reversed',
		decision_id: '123',
		restores_access: 'yes',
		statement: 'We checked again',
		content_removed: 'no',
	},
	enforcement_notice: {
		kind_label: 'suspension',
		neutral: 'no',
		summary: 'We suspended your account.',
		effects: 'You cannot sign in.',
		rule_title: 'Spam',
		rule_url: 'https://example.com/guidelines',
		facts: 'We found spam.',
		statement: null,
		how_we_decided: 'A person decided.',
		appeals_email: 'appeals@fluxer.com',
		appeal_deadline: 'Dec 1, 2026',
		redress: null,
		decision_id: '123',
	},
	gift_chargeback_notification: {},
	harvest_completed: {totalMessages: 1200, fileSizeMB: 3.5, expiresAt: DATE},
	inactivity_warning: {deletionDate: DATE, lastActiveDate: DATE},
	ip_authorization: {},
	mfa_backup_codes_view: {minutes: 10},
	operator_pass_offer_accepted: {recipientName: 'Grace', remainingCount: 2},
	operator_pass_offer_ended: {otherName: 'Grace', ended: 'declined'},
	operator_pass_offer_received: {senderName: 'Ada', arrivesAt: '2026-10-12'},
	operator_pass_perks_removed: {scope: 'temporary', until: DATE, automated: 'no'},
	operator_pass_purchase_confirmation: {
		termsVersion: '2026-10-18',
		orderDate: DATE,
		quantity: 2,
		priceFormatted: '$199',
		withdrawalEndsOn: '2026-11-08',
		waiverConsentAt: DATE,
		consentText: 'Deliver all 2 passes now.',
		wallName: 'Ada',
		wallOptOut: 'no',
		sellerEmail: 'support@fluxer.com',
	},
	operator_pass_removal_notice: {replyDays: 7},
	operator_pass_revoked: {cause: 'refund', lastPass: 'yes'},
	operator_pass_sale_open: {priceFormatted: '$199', priceEndsOn: '2027-01-01'},
	operator_pass_waitlist_verification: {},
	operator_pass_wall_hidden: {reason: 'guidelines'},
	operator_pass_wall_name_replaced: {target: 'both', shown: 'username', reason: 'rights'},
	operator_pass_welcome: {founderName: 'Hampus', deliveredAt: DATE, wallName: 'Ada', wallOptOut: 'no'},
	operator_pass_withdrawal_receipt: {
		cancellation: 'no',
		receivedAt: DATE,
		orderDate: DATE,
		amountFormatted: '$199',
		lastPass: 'no',
		refundViaDispute: 'no',
	},
	password_change_verification: {minutes: 10},
	password_reset: {},
	payment_dispute_warning: {
		appeals_email: 'appeals@fluxer.com',
		privacy_email: 'privacy@fluxer.com',
		product: 'plutonium',
		orderDate: DATE,
		warningDate: DATE,
	},
	reauth_code: {minutes: 10},
	registration_approved: {},
	registration_pending: {},
	report_received: {targetKind: 'message'},
	report_resolved: {
		outcome: 'restricted',
		appeals_email: 'appeals@fluxer.com',
		target: 'message_community',
		reportedAt: DATE,
		username: 'example',
		communityName: 'Fluxer HQ',
		channelName: 'general',
	},
	self_deletion_scheduled: {deletionDate: DATE},
	sign_in_code: {minutes: 10},
	sign_up_code: {minutes: 10},
};

function withReply<K extends EmailTemplateKey>(key: K): EmailTemplateVariables[K] {
	return {...FIXTURE[key], reply: 'yes'} as EmailTemplateVariables[K];
}

describe('EmailI18n locale files', () => {
	it.each(LOCALES)('%s has the same template fields as the source catalog', (locale) => {
		const messages: Partial<Record<EmailTemplateKey, EmailTemplate>> = EMAIL_I18N_LOCALE_MESSAGES[locale];
		for (const key of TEMPLATE_KEYS) {
			expect(Object.keys(messages[key] ?? {}).sort(), key).toEqual(Object.keys(EMAIL_I18N_MESSAGES[key]).sort());
		}
	});
	it.each(LOCALES)('%s keeps the source placeholders in every template field', (locale) => {
		const messages: Partial<Record<EmailTemplateKey, EmailTemplate>> = EMAIL_I18N_LOCALE_MESSAGES[locale];
		for (const key of TEMPLATE_KEYS) {
			const source: EmailTemplate = EMAIL_I18N_MESSAGES[key];
			const translated = messages[key];
			expect(translated, key).toBeDefined();
			if (!translated) continue;
			for (const field of Object.keys(source)) {
				expect(extractMessageTemplatePlaceholders(translated[field] ?? ''), `${key}.${field}`).toEqual(
					extractMessageTemplatePlaceholders(source[field]),
				);
			}
		}
	});
	it.each(LOCALES)('%s keeps every link tag of the source in every template field', (locale) => {
		const messages: Partial<Record<EmailTemplateKey, EmailTemplate>> = EMAIL_I18N_LOCALE_MESSAGES[locale];
		const tags = (text: string) => (text.match(/<\/?(mail|settings|dsa|arn)>/g) ?? []).sort();
		for (const key of TEMPLATE_KEYS) {
			const source: EmailTemplate = EMAIL_I18N_MESSAGES[key];
			for (const [field, text] of Object.entries(source)) {
				const translated = messages[key]?.[field] ?? '';
				expect(tags(translated), `${key}.${field}`).toEqual(tags(text));
				expect(translated, `${key}.${field}`).not.toMatch(/<(mail|settings|dsa|arn)>[^<]*<(mail|settings|dsa|arn)>/);
			}
		}
	});
	it.each(['en-US', ...LOCALES])('%s renders every template with UTC times', (locale) => {
		for (const key of TEMPLATE_KEYS) {
			const result = getEmailTemplate(key, locale, withReply(key), 'Fluxer');
			expect(result.ok, key).toBe(true);
			if (!result.ok) continue;
			for (const [field, text] of Object.entries(result.value)) {
				expect(text, `${key}.${field}`).not.toContain('GMT');
				expect(text, `${key}.${field}`).not.toContain('Coordinated Universal Time');
			}
		}
	});
});
