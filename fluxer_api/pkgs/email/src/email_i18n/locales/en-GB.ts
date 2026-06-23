// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_EN_GB_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Your {product_name} account has been temporarily disabled",
		"body": "Hello {username},\n\nWe temporarily disabled your {product_name} account because we detected suspicious activity.\n\n{reason, select,\n  null {}\n  other {Reason: {reason}}\n}\n\nTo regain access to your account, you'll need to reset your password:\n\n{forgotUrl}\n\nAfter you reset your password, you'll be able to log in again.\n\nIf you believe this was done in error, please contact our support team.\n\n– {product_name} Safety Team"
	},
	"account_scheduled_deletion": {
		"subject": "Your {product_name} account will be permanently deleted",
		"body": "Hello {username},\n\nYour {product_name} account has been scheduled for permanent deletion due to violations of our Terms of Service or Community Guidelines.\n\nScheduled deletion: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Reason: {reason}}\n}\n\nThis is a serious enforcement action. Your account data will be permanently deleted on the scheduled date.\n\nPlease review:\n- Terms of Service: {termsUrl}\n- Community Guidelines: {guidelinesUrl}\n\nAppeals process:\nIf you believe this enforcement decision was incorrect or unjustified, you have 60 days to submit an appeal. Email {appeals_email} from this email address.\n\nIn your appeal:\n- Clearly explain why you believe the enforcement decision was incorrect or unjustified\n- Provide any relevant evidence or context\n\nA member of the {product_name} Safety Team will review your appeal and may pause the pending deletion until a final decision has been reached.\n\n– {product_name} Safety Team"
	},
	"account_temp_banned": {
		"subject": "Your {product_name} account has been temporarily suspended",
		"body": "Hello {username},\n\nYour {product_name} account has been temporarily suspended for violating our Terms of Service or Community Guidelines.\n\nDuration: {durationHours, plural,\n  =1 {1 hour}\n  other {# hours}\n}\nSuspended until: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Reason: {reason}}\n}\n\nDuring this time, you won't be able to access your account.\n\nPlease review:\n- Terms of Service: {termsUrl}\n- Community Guidelines: {guidelinesUrl}\n\nIf you believe this enforcement decision was incorrect or unjustified, you can submit an appeal. Email {appeals_email} from this email address and clearly explain why you believe the decision was incorrect. We'll review your appeal and respond with our decision.\n\n– {product_name} Safety Team"
	},
	"donation_confirmation": {
		"subject": "Thank you for your {product_name} donation",
		"body": "Hello,\n\nThank you for your donation to {product_name}! Your {interval, select,\n  month {recurring donation}\n  year {recurring donation}\n  other {one-time donation}\n} has been {interval, select,\n  month {set up}\n  year {set up}\n  other {processed}\n} successfully.\n\nDonation details:\nAmount: {amount} {currency} {interval, select,\n  month {per month}\n  year {per year}\n  other {}\n}\n\nStripe will email you a separate receipt with your invoice PDF shortly. This includes all payment details and can be used for tax purposes.\n\nYou can view your donation history, download invoices, {interval, select,\n  month {and manage or cancel your subscription}\n  year {and manage or cancel your subscription}\n  other {and manage future donations}\n} at any time using this link:\n\n{manageUrl}\n\nYour support helps keep {product_name} running. Thank you!\n\n– {product_name} Team"
	},
	"donation_magic_link": {
		"subject": "Manage your {product_name} donations",
		"body": "Hello,\n\nClick the link below to access your donor portal:\n\n{manageUrl}\n\nIn the portal, you can manage subscriptions, download invoices, and view your donation history.\n\nThis link expires on {expiresAt, date, full} at {expiresAt, time, short}.\n\nIf you did not request this link, you can safely ignore this email.\n\n– {product_name} Team"
	},
	"dsa_report_verification": {
		"subject": "Verify your email for a DSA report",
		"body": "Hello,\n\nUse the verification code below to submit your Digital Services Act report on {product_name}:\n\n{code}\n\nThis code expires on {expiresAt, date, full} at {expiresAt, time, short}.\n\nIf you didn't request this, you can ignore this email.\n\n– {product_name} Safety Team"
	},
	"email_change_new": {
		"subject": "Verify your new {product_name} email",
		"body": "Hello {username},\n\nEnter this code in the app to verify your new {product_name} email:\n\n{code}\n\nThis code expires on {expiresAt, date, full} at {expiresAt, time, short}.\n\nIf you didn't request this, you can ignore this email.\n\n– {product_name} Team"
	},
	"email_change_original": {
		"subject": "Confirm your {product_name} email change",
		"body": "Hello {username},\n\nWe received a request to change the email address on your {product_name} account.\n\nTo confirm this change, enter this code in the app:\n\n{code}\n\nThis code expires on {expiresAt, date, full} at {expiresAt, time, short}.\n\nIf you didn't request this, please secure your account right away.\n\n– {product_name} Team"
	},
	"email_change_revert": {
		"subject": "Your {product_name} email was changed",
		"body": "Hello {username},\n\nThe email address on your {product_name} account was changed to {newEmail}.\n\nIf you made this change, no action is needed. If you didn't, you can revert the change and secure your account using this link:\n\n{revertUrl}\n\nThis will restore your previous email, sign you out everywhere, remove linked phone numbers, disable MFA, and require you to set a new password.\n\n– {product_name} Safety Team"
	},
	"email_verification": {
		"subject": "Verify your {product_name} email address",
		"body": "Hello {username},\n\nPlease verify the email address for your {product_name} account by clicking the link below:\n\n{verifyUrl}\n\nIf you didn't create a {product_name} account, you can safely ignore this email.\n\nThis link is valid for 24 hours.\n\n– {product_name} Team"
	},
	"gift_chargeback_notification": {
		"subject": "Perks from your redeemed gift have been removed",
		"body": "Hello {username},\n\nA gift code you redeemed was originally paid for by someone else. That payment has since been reversed (a chargeback).\n\nBecause of this, we've removed the perks that were added to your account when you redeemed the gift.\n\nIf you think this is a mistake, please contact our support team and include any details you have about the gift code and when you redeemed it.\n\n– {product_name} Team"
	},
	"harvest_completed": {
		"subject": "Your {product_name} data export is ready to download",
		"body": "Hello {username},\n\nYour data export is ready.\n\nDownload link:\n{downloadUrl}\n\nMessages included: {totalMessages, number}\nFile size: {fileSizeMB, number} MB\n\nThis link expires on {expiresAt, date, full} at {expiresAt, time, short}.\n\nIf you didn't request this export, please change your password immediately and contact our support team.\n\n– {product_name} Team"
	},
	"inactivity_warning": {
		"subject": "Your {product_name} account will be deleted due to inactivity",
		"body": "Hello {username},\n\nWe haven't seen any activity on your {product_name} account since {lastActiveDate, date, full}.\n\nIf you don't log in by {deletionDate, date, full} at {deletionDate, time, short}, your account will be permanently deleted due to inactivity.\n\nLog in here:\n{loginUrl}\n\nIf you've used {product_name} recently, please contact our support team right away.\n\n– {product_name} Team"
	},
	"ip_authorization": {
		"subject": "Authorize login from a new IP address",
		"body": "Hello {username},\n\nWe detected a login attempt to your {product_name} account from a new IP address:\n\nIP address: {ipAddress}\nLocation: {location}\n\nIf this was you, please authorize this IP address by clicking the link below:\n\n{authUrl}\n\nIf you didn't attempt to log in, please change your password right away.\n\nThis link is valid for 30 minutes.\n\n– {product_name} Team"
	},
	"password_change_verification": {
		"subject": "Confirm your {product_name} password change",
		"body": "Hello {username},\n\nWe received a request to change the password on your {product_name} account.\n\nTo confirm this change, enter this code in the app:\n\n{code}\n\nThis code expires at {expiresAt}.\n\nIf you didn't request this, someone may have access to your account. Change your password immediately and enable two-factor authentication.\n\n– {product_name} Team"
	},
	"password_reset": {
		"subject": "Reset your {product_name} password",
		"body": "Hello {username},\n\nYou requested a {product_name} password reset. Use the link below to set a new password:\n\n{resetUrl}\n\nIf you didn't request this, you can safely ignore this email.\n\nThis link is valid for 1 hour.\n\n– {product_name} Team"
	},
	"registration_approved": {
		"subject": "Your {product_name} registration has been approved",
		"body": "Hello {username},\n\nGood news: your {product_name} registration has been approved.\n\nYou can now log in to the {product_name} app here:\n{channelsUrl}\n\nWelcome to the {product_name} community.\n\n– {product_name} Team"
	},
	"report_resolved": {
		"subject": "Your {product_name} report has been reviewed",
		"body": "Hello {username},\n\nYour report (ID: {reportId}) has been reviewed by our Safety Team.{hasComment, select, yes {\n\nResponse from the Safety Team:\n{publicComment}} other {}}\n\nThanks for helping keep {product_name} safe for everyone. We take all reports seriously and appreciate your contribution to the community.\n\nIf you have any questions or concerns about this outcome, please contact {safety_email}.\n\n– {product_name} Safety Team"
	},
	"scheduled_deletion_notification": {
		"subject": "Your {product_name} account will be permanently deleted",
		"body": "Hello {username},\n\nYour {product_name} account has been scheduled for permanent deletion.\n\nScheduled deletion: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Reason: {reason}}\n}\n\nThis is a serious enforcement action. Your account data will be permanently deleted on the scheduled date.\n\nIf you believe this enforcement decision was incorrect, you can submit an appeal. Email {appeals_email} from this email address.\n\n– {product_name} Safety Team"
	},
	"self_deletion_scheduled": {
		"subject": "Your {product_name} account deletion is scheduled",
		"body": "Hello {username},\n\nYou requested to delete your {product_name} account. Your account is scheduled for permanent deletion on:\n\n{deletionDate, date, full} at {deletionDate, time, short}\n\nIf you didn't request this, sign in to your account to cancel the deletion. We also recommend changing your password to secure your account.\n\n– {product_name} Team"
	},
	"unban_notification": {
		"subject": "Your {product_name} account suspension has been lifted",
		"body": "Hello {username},\n\nGood news: your {product_name} account suspension has been lifted.\n\n{reason, select,\n  null {}\n  other {Reason: {reason}}\n}\n\nYou can now log back in and continue using {product_name} as normal.\n\n– {product_name} Safety Team"
	}
});

export default EMAIL_I18N_EN_GB_MESSAGES;
