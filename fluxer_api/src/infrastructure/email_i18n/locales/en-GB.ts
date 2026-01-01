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

import type {EmailTranslations} from '../types';

export const enGB: EmailTranslations = {
	passwordReset: {
		subject: 'Reset your Fluxer password',
		body: `Hello {username},

You requested to reset your Fluxer password. Please follow the link below to set a new password:

{resetUrl}

If you did not request this password reset, you can safely ignore this email.

This link will expire in 1 hour.

- Fluxer Team`,
	},
	emailVerification: {
		subject: 'Verify your Fluxer email address',
		body: `Hello {username},

Please verify your email address for your Fluxer account by clicking the link below:

{verifyUrl}

If you did not create a Fluxer account, you can safely ignore this email.

This link will expire in 24 hours.

- Fluxer Team`,
	},
	ipAuthorization: {
		subject: 'Authorise login from new IP address',
		body: `Hello {username},

We detected a login attempt to your Fluxer account from a new IP address:

IP Address: {ipAddress}
Location: {location}

If this was you, please authorise this IP address by clicking the link below:

{authUrl}

If you did not attempt to log in, please change your password immediately.

This authorisation link will expire in 30 minutes.

- Fluxer Team`,
	},
	accountDisabledSuspicious: {
		subject: 'Your Fluxer account has been temporarily disabled',
		body: `Hello {username},

Your Fluxer account has been temporarily disabled due to suspicious activity.

{reason, select,
	null {}
	other {Reason: {reason}

}}To regain access to your account, you must reset your password:

{forgotUrl}

After resetting your password, you will be able to log in again.

If you believe this action was taken in error, please contact our support team.

- Fluxer Safety Team`,
	},
	accountTempBanned: {
		subject: 'Your Fluxer account has been temporarily suspended',
		body: `Hello {username},

Your Fluxer account has been temporarily suspended for violating our Terms of Service or Community Guidelines.

Duration: {durationHours, plural,
	=1 {1 hour}
	other {# hours}
}
Suspended until: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {
Reason: {reason}}
}

During this time, you will not be able to access your account.

We urge you to review our:
- Terms of Service: {termsUrl}
- Community Guidelines: {guidelinesUrl}

If you believe this enforcement decision was incorrect or unjustified, you may submit an appeal to appeals@fluxer.app from this email address. Please clearly explain why you believe the decision was wrong. We will review your appeal and respond with our determination.

- Fluxer Safety Team`,
	},
	accountScheduledDeletion: {
		subject: 'Your Fluxer account is scheduled for deletion',
		body: `Hello {username},

Your Fluxer account has been scheduled for permanent deletion due to violations of our Terms of Service or Community Guidelines.

Scheduled deletion date: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
Reason: {reason}}
}

This is a serious enforcement action. Your account data will be permanently deleted on the scheduled date.

We urge you to review our:
- Terms of Service: {termsUrl}
- Community Guidelines: {guidelinesUrl}

APPEALS PROCESS:
If you believe this enforcement decision was incorrect or unjustified, you have 30 days to submit an appeal to appeals@fluxer.app from this email address.

In your appeal, please:
- Clearly explain why you believe the enforcement decision was incorrect or unjustified
- Provide any relevant evidence or context

A Fluxer Safety Team member will review your appeal and may cancel the pending deletion until a final verdict has been reached.

- Fluxer Safety Team`,
	},
	selfDeletionScheduled: {
		subject: 'Your Fluxer account deletion has been scheduled',
		body: `Hello {username},

We're sad to see you go! Your Fluxer account has been scheduled for deletion.

Scheduled deletion date: {deletionDate, date, full} {deletionDate, time, short}

IMPORTANT: You can cancel this deletion at any time before {deletionDate, date, full} {deletionDate, time, short} by simply logging back into your account.

BEFORE YOU GO:
Your Privacy Dashboard in User Settings allows you to:
- Delete your messages on the platform
- Extract any valuable data before departing

Please note: Once your account is deleted, there is no way to delete your messages. If you want to delete your messages, please do so through the Privacy Dashboard before your account deletion is finalised.

If you change your mind, just log back in to cancel the deletion.

- Fluxer Team`,
	},
	inactivityWarning: {
		subject: 'Your Fluxer account will be deleted due to inactivity',
		body: `Hello {username},

We noticed you haven't logged into your Fluxer account in over 2 years.

Last login: {lastActiveDate, date, full} {lastActiveDate, time, short}

As part of our data retention policy, inactive accounts are automatically scheduled for deletion. Your account will be permanently deleted on:

Scheduled deletion date: {deletionDate, date, full} {deletionDate, time, short}

HOW TO KEEP YOUR ACCOUNT:
Simply log in to your account at {loginUrl} before the deletion date to cancel this automatic deletion. No other action is required.

WHAT HAPPENS IF YOU DON'T LOG IN:
- Your account and all associated data will be permanently deleted
- Your messages will be anonymised (attributed to “Deleted User”)
- This action cannot be reversed

WANT TO DELETE YOUR MESSAGES?
If you want to delete your messages before your account is deleted, please log in and use the Privacy Dashboard in User Settings.

We hope to see you back on Fluxer!

- Fluxer Team`,
	},
	harvestCompleted: {
		subject: 'Your Fluxer Data Export is Ready',
		body: `Hello {username},

Your data export has been completed and is ready for download!

Export Summary:
- Total messages: {totalMessages, number}
- File size: {fileSizeMB} MB
- Format: ZIP archive with JSON files

Download your data: {downloadUrl}

IMPORTANT: This download link will expire on {expiresAt, date, full} {expiresAt, time, short}

What's included in your export:
- All your messages organised by channel
- Channel metadata
- Your user profile and account information
- Guild memberships and settings
- Authentication sessions and security information

The data is organised in JSON format for easy parsing and analysis.

If you have any questions about your data export, please contact support@fluxer.app

- Fluxer Team`,
	},
	unbanNotification: {
		subject: 'Your Fluxer account suspension has been lifted',
		body: `Hello {username},

Good news! Your Fluxer account suspension has been lifted.

Reason: {reason}

You can now log back into your account and continue using Fluxer.

- Fluxer Safety Team`,
	},
	scheduledDeletionNotification: {
		subject: 'Your Fluxer account is scheduled for deletion',
		body: `Hello {username},

Your Fluxer account has been scheduled for permanent deletion.

Scheduled deletion date: {deletionDate, date, full} {deletionDate, time, short}
Reason: {reason}

This is a serious enforcement action. Your account data will be permanently deleted on the scheduled date.

If you believe this enforcement decision was incorrect, you may submit an appeal to appeals@fluxer.app from this email address.

- Fluxer Safety Team`,
	},
	giftChargebackNotification: {
		subject: 'Your Fluxer Premium gift has been revoked',
		body: `Hello {username},

We're writing to inform you that the Fluxer Premium gift you redeemed has been revoked due to a payment dispute (chargeback) filed by the original purchaser.

Your premium benefits have been removed from your account. This action was taken because the payment for the gift was disputed and reversed.

If you have questions about this, please contact support@fluxer.app.

- Fluxer Team`,
	},
	reportResolved: {
		subject: 'Your Fluxer report has been reviewed',
		body: `Hello {username},

Your report (ID: {reportId}) has been reviewed by our Safety Team.

Response from Safety Team:
{publicComment}

Thank you for helping keep Fluxer safe for everyone. We take all reports seriously and appreciate your contribution to our community.

If you have any questions or concerns about this resolution, please contact safety@fluxer.app.

- Fluxer Safety Team`,
	},
	dsaReportVerification: {
		subject: 'Verify your email for a DSA report',
		body: `Hello,

Use the following verification code to submit your Digital Services Act report on Fluxer:

{code}

This code expires at {expiresAt, date, full} {expiresAt, time, short}.

If you did not request this, please ignore this email.

- Fluxer Safety Team`,
	},
	registrationApproved: {
		subject: 'Your Fluxer registration has been approved',
		body: `Hello {username},

Great news! Your Fluxer registration has been approved.

You can now log in to the Fluxer app at:
{channelsUrl}

Welcome to the Fluxer community!

- Fluxer Team`,
	},
	emailChangeRevert: {
		subject: 'Your Fluxer email was changed',
		body: `Hello {username},

Your Fluxer account email was changed to {newEmail}.

If you made this change, no action is needed. If not, you can revert and secure your account using this link:

{revertUrl}

This will restore your previous email, sign you out everywhere, remove linked phone numbers, disable MFA, and require a new password.

- Fluxer Safety Team`,
	},
};
