// SPDX-License-Identifier: AGPL-3.0-or-later

import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {openBouncedEmailChange, useVerifyEmailAction} from '@app/features/auth/hooks/useVerifyEmailAction';
import {ADD_EMAIL_DESCRIPTOR, CLAIM_ACCOUNT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Users from '@app/features/user/state/Users';
import {EmailDeliveryStates} from '@fluxer/constants/src/AccountConfigConstants';
import {
	type AccountVerificationNeed,
	AccountVerificationNeeds,
	TierGatedActions,
} from '@fluxer/constants/src/AccountTierConstants';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {EnvelopeSimpleIcon, type Icon, UserCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

export const AccountGateActions = {
	...TierGatedActions,
	PURCHASE: 'purchase',
} as const;

export type AccountGateAction = ValueOf<typeof AccountGateActions>;

const AccountGateVariants = {
	ADD_EMAIL: 'add_email',
	ADD_EMAIL_NO_DELIVERY: 'add_email_no_delivery',
	CONFIRM_EMAIL: 'confirm_email',
	SET_PASSWORD: 'set_password',
} as const;

type AccountGateVariant = ValueOf<typeof AccountGateVariants>;

export interface AccountGate {
	readonly action: AccountGateAction;
	readonly need: AccountVerificationNeed;
}

export interface AccountGatePresentation {
	readonly Icon: Icon;
	readonly title: string;
	readonly description: string;
	readonly actionLabel: string;
	readonly onAction: () => void;
}

const ADD_EMAIL_REASONS: Readonly<Record<AccountGateAction, MessageDescriptor>> = {
	[AccountGateActions.MESSAGE]: msg({
		message: 'Add an email to send messages.',
		comment: 'Reason shown when someone without an email tries to send a message.',
	}),
	[AccountGateActions.TYPING]: msg({
		message: 'Add an email to send messages.',
		comment: 'Reason shown when someone without an email starts typing a message.',
	}),
	[AccountGateActions.REACTION]: msg({
		message: 'Add an email to add reactions.',
		comment: 'Reason shown when someone without an email tries to react to a message.',
	}),
	[AccountGateActions.DIRECT_MESSAGE]: msg({
		message: 'Add an email to send direct messages.',
		comment: 'Reason shown when someone without an email tries to send a direct message.',
	}),
	[AccountGateActions.DIRECT_MESSAGE_OPEN]: msg({
		message: 'Add an email to message people directly.',
		comment: 'Reason shown when someone without an email tries to start a direct message or group chat.',
	}),
	[AccountGateActions.GROUP_DM]: msg({
		message: 'Add an email to join group chats.',
		comment: 'Reason shown when someone without an email tries to join a group chat.',
	}),
	[AccountGateActions.FRIEND_REQUEST]: msg({
		message: 'Add an email to add friends.',
		comment: 'Reason shown when someone without an email tries to send a friend request.',
	}),
	[AccountGateActions.FRIEND_ACCEPT]: msg({
		message: 'Add an email to accept friend requests.',
		comment: 'Reason shown when someone without an email tries to accept a friend request.',
	}),
	[AccountGateActions.INVITE_CREATE]: msg({
		message: 'Add an email to invite people.',
		comment: 'Reason shown when someone without an email tries to create an invite.',
	}),
	[AccountGateActions.GUILD_JOIN]: msg({
		message: 'Add an email to join this community.',
		comment: 'Reason shown when someone without an email tries to join a community.',
	}),
	[AccountGateActions.GUILD_CREATE]: msg({
		message: 'Add an email to create a community.',
		comment: 'Reason shown when someone without an email tries to create a community.',
	}),
	[AccountGateActions.VOICE]: msg({
		message: 'Add an email to join voice channels.',
		comment: 'Reason shown when someone without an email tries to join a voice channel.',
	}),
	[AccountGateActions.CALL]: msg({
		message: 'Add an email to start or join calls.',
		comment: 'Reason shown when someone without an email tries to start or join a call.',
	}),
	[AccountGateActions.GROUP_CALL]: msg({
		message: 'Add an email to start or join group calls.',
		comment: 'Reason shown when someone without an email tries to start or join a call in a group chat.',
	}),
	[AccountGateActions.THREAD_JOIN]: msg({
		message: 'Add an email to join threads.',
		comment: 'Reason shown when someone without an email tries to join a thread.',
	}),
	[AccountGateActions.CHANNEL_FOLLOW]: msg({
		message: 'Add an email to follow channels.',
		comment: 'Reason shown when someone without an email tries to follow an announcement channel.',
	}),
	[AccountGateActions.OWNERSHIP_TRANSFER]: msg({
		message: 'This person needs to add an email before they can own a community.',
		comment:
			'Reason shown when the new owner picked for a community has not finished setting up their account. Variant: they have no email.',
	}),
	[AccountGateActions.APPLICATION_CREATE]: msg({
		message: 'Add an email to create applications.',
		comment: 'Reason shown when someone without an email tries to create an application.',
	}),
	[AccountGateActions.GIFT_REDEEM]: msg({
		message: 'Add an email to redeem gifts.',
		comment: 'Reason shown when someone without an email tries to redeem a gift.',
	}),
	[AccountGateActions.PROFILE_CUSTOMIZE]: msg({
		message: 'Add an email to edit your profile.',
		comment: 'Reason shown when someone without an email tries to change their profile.',
	}),
	[AccountGateActions.REPORT]: msg({
		message: 'Add an email to submit reports.',
		comment: 'Reason shown when someone without an email tries to submit a report.',
	}),
	[AccountGateActions.MFA]: msg({
		message: 'Add an email to set up two-factor authentication and passkeys.',
		comment: 'Reason shown when someone without an email tries to turn on two-factor authentication or add a passkey.',
	}),
	[AccountGateActions.GUILD_VERIFICATION]: msg({
		message: 'Add an email to talk in this community.',
		comment: 'Reason shown when someone without an email tries to talk in a community that needs a finished account.',
	}),
	[AccountGateActions.PURCHASE]: msg({
		message: 'Add an email so we can send your receipts.',
		comment: 'Reason shown when someone without an email starts a purchase.',
	}),
};

const ADD_EMAIL_NO_DELIVERY_REASONS: Readonly<Record<AccountGateAction, MessageDescriptor>> = {
	[AccountGateActions.MESSAGE]: msg({
		message: 'Add an email and a password to send messages.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to send a message.',
	}),
	[AccountGateActions.TYPING]: msg({
		message: 'Add an email and a password to send messages.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email starts typing a message.',
	}),
	[AccountGateActions.REACTION]: msg({
		message: 'Add an email and a password to add reactions.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to react to a message.',
	}),
	[AccountGateActions.DIRECT_MESSAGE]: msg({
		message: 'Add an email and a password to send direct messages.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to send a direct message.',
	}),
	[AccountGateActions.DIRECT_MESSAGE_OPEN]: msg({
		message: 'Add an email and a password to message people directly.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to start a direct message or group chat.',
	}),
	[AccountGateActions.GROUP_DM]: msg({
		message: 'Add an email and a password to join group chats.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to join a group chat.',
	}),
	[AccountGateActions.FRIEND_REQUEST]: msg({
		message: 'Add an email and a password to add friends.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to send a friend request.',
	}),
	[AccountGateActions.FRIEND_ACCEPT]: msg({
		message: 'Add an email and a password to accept friend requests.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to accept a friend request.',
	}),
	[AccountGateActions.INVITE_CREATE]: msg({
		message: 'Add an email and a password to invite people.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to create an invite.',
	}),
	[AccountGateActions.GUILD_JOIN]: msg({
		message: 'Add an email and a password to join this community.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to join a community.',
	}),
	[AccountGateActions.GUILD_CREATE]: msg({
		message: 'Add an email and a password to create a community.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to create a community.',
	}),
	[AccountGateActions.VOICE]: msg({
		message: 'Add an email and a password to join voice channels.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to join a voice channel.',
	}),
	[AccountGateActions.CALL]: msg({
		message: 'Add an email and a password to start or join calls.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to start or join a call.',
	}),
	[AccountGateActions.GROUP_CALL]: msg({
		message: 'Add an email and a password to start or join group calls.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to start or join a call in a group chat.',
	}),
	[AccountGateActions.THREAD_JOIN]: msg({
		message: 'Add an email and a password to join threads.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to join a thread.',
	}),
	[AccountGateActions.CHANNEL_FOLLOW]: msg({
		message: 'Add an email and a password to follow channels.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to follow an announcement channel.',
	}),
	[AccountGateActions.OWNERSHIP_TRANSFER]: msg({
		message: 'This person needs to add an email and a password before they can own a community.',
		comment:
			'Reason shown when the new owner picked for a community has not finished setting up their account. Variant: they have no email and the instance cannot send email.',
	}),
	[AccountGateActions.APPLICATION_CREATE]: msg({
		message: 'Add an email and a password to create applications.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to create an application.',
	}),
	[AccountGateActions.GIFT_REDEEM]: msg({
		message: 'Add an email and a password to redeem gifts.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to redeem a gift.',
	}),
	[AccountGateActions.PROFILE_CUSTOMIZE]: msg({
		message: 'Add an email and a password to edit your profile.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to change their profile.',
	}),
	[AccountGateActions.REPORT]: msg({
		message: 'Add an email and a password to submit reports.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to submit a report.',
	}),
	[AccountGateActions.MFA]: msg({
		message: 'Add an email and a password to set up two-factor authentication and passkeys.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to turn on two-factor authentication or add a passkey.',
	}),
	[AccountGateActions.GUILD_VERIFICATION]: msg({
		message: 'Add an email and a password to talk in this community.',
		comment:
			'Reason shown on an instance that cannot send email, when someone without an email tries to talk in a community that needs a finished account.',
	}),
	[AccountGateActions.PURCHASE]: msg({
		message: 'Add an email and a password so we can send your receipts.',
		comment: 'Reason shown on an instance that cannot send email, when someone without an email starts a purchase.',
	}),
};

const CONFIRM_EMAIL_REASONS: Readonly<Record<AccountGateAction, MessageDescriptor>> = {
	[AccountGateActions.MESSAGE]: msg({
		message: 'Confirm your email to send messages.',
		comment: 'Reason shown when someone with an unconfirmed email tries to send a message.',
	}),
	[AccountGateActions.TYPING]: msg({
		message: 'Confirm your email to send messages.',
		comment: 'Reason shown when someone with an unconfirmed email starts typing a message.',
	}),
	[AccountGateActions.REACTION]: msg({
		message: 'Confirm your email to add reactions.',
		comment: 'Reason shown when someone with an unconfirmed email tries to react to a message.',
	}),
	[AccountGateActions.DIRECT_MESSAGE]: msg({
		message: 'Confirm your email to send direct messages.',
		comment: 'Reason shown when someone with an unconfirmed email tries to send a direct message.',
	}),
	[AccountGateActions.DIRECT_MESSAGE_OPEN]: msg({
		message: 'Confirm your email to message people directly.',
		comment: 'Reason shown when someone with an unconfirmed email tries to start a direct message or group chat.',
	}),
	[AccountGateActions.GROUP_DM]: msg({
		message: 'Confirm your email to join group chats.',
		comment: 'Reason shown when someone with an unconfirmed email tries to join a group chat.',
	}),
	[AccountGateActions.FRIEND_REQUEST]: msg({
		message: 'Confirm your email to add friends.',
		comment: 'Reason shown when someone with an unconfirmed email tries to send a friend request.',
	}),
	[AccountGateActions.FRIEND_ACCEPT]: msg({
		message: 'Confirm your email to accept friend requests.',
		comment: 'Reason shown when someone with an unconfirmed email tries to accept a friend request.',
	}),
	[AccountGateActions.INVITE_CREATE]: msg({
		message: 'Confirm your email to invite people.',
		comment: 'Reason shown when someone with an unconfirmed email tries to create an invite.',
	}),
	[AccountGateActions.GUILD_JOIN]: msg({
		message: 'Confirm your email to join this community.',
		comment: 'Reason shown when someone with an unconfirmed email tries to join a community.',
	}),
	[AccountGateActions.GUILD_CREATE]: msg({
		message: 'Confirm your email to create a community.',
		comment: 'Reason shown when someone with an unconfirmed email tries to create a community.',
	}),
	[AccountGateActions.VOICE]: msg({
		message: 'Confirm your email to join voice channels.',
		comment: 'Reason shown when someone with an unconfirmed email tries to join a voice channel.',
	}),
	[AccountGateActions.CALL]: msg({
		message: 'Confirm your email to start or join calls.',
		comment: 'Reason shown when someone with an unconfirmed email tries to start or join a call.',
	}),
	[AccountGateActions.GROUP_CALL]: msg({
		message: 'Confirm your email to start or join group calls.',
		comment: 'Reason shown when someone with an unconfirmed email tries to start or join a call in a group chat.',
	}),
	[AccountGateActions.THREAD_JOIN]: msg({
		message: 'Confirm your email to join threads.',
		comment: 'Reason shown when someone with an unconfirmed email tries to join a thread.',
	}),
	[AccountGateActions.CHANNEL_FOLLOW]: msg({
		message: 'Confirm your email to follow channels.',
		comment: 'Reason shown when someone with an unconfirmed email tries to follow an announcement channel.',
	}),
	[AccountGateActions.OWNERSHIP_TRANSFER]: msg({
		message: 'This person needs to confirm their email before they can own a community.',
		comment:
			'Reason shown when the new owner picked for a community has not finished setting up their account. Variant: their email is not confirmed.',
	}),
	[AccountGateActions.APPLICATION_CREATE]: msg({
		message: 'Confirm your email to create applications.',
		comment: 'Reason shown when someone with an unconfirmed email tries to create an application.',
	}),
	[AccountGateActions.GIFT_REDEEM]: msg({
		message: 'Confirm your email to redeem gifts.',
		comment: 'Reason shown when someone with an unconfirmed email tries to redeem a gift.',
	}),
	[AccountGateActions.PROFILE_CUSTOMIZE]: msg({
		message: 'Confirm your email to edit your profile.',
		comment: 'Reason shown when someone with an unconfirmed email tries to change their profile.',
	}),
	[AccountGateActions.REPORT]: msg({
		message: 'Confirm your email to submit reports.',
		comment: 'Reason shown when someone with an unconfirmed email tries to submit a report.',
	}),
	[AccountGateActions.MFA]: msg({
		message: 'Confirm your email to set up two-factor authentication and passkeys.',
		comment:
			'Reason shown when someone with an unconfirmed email tries to turn on two-factor authentication or add a passkey.',
	}),
	[AccountGateActions.GUILD_VERIFICATION]: msg({
		message: 'Confirm your email to talk in this community.',
		comment:
			'Reason shown when someone with an unconfirmed email tries to talk in a community that needs a finished account.',
	}),
	[AccountGateActions.PURCHASE]: msg({
		message: 'Confirm your email so we can send your receipts.',
		comment: 'Reason shown when someone with an unconfirmed email starts a purchase.',
	}),
};

const SET_PASSWORD_REASONS: Readonly<Record<AccountGateAction, MessageDescriptor>> = {
	[AccountGateActions.MESSAGE]: msg({
		message: 'Set a password to send messages.',
		comment: 'Reason shown when someone whose account has no password or passkey yet tries to send a message.',
	}),
	[AccountGateActions.TYPING]: msg({
		message: 'Set a password to send messages.',
		comment: 'Reason shown when someone whose account has no password or passkey yet starts typing a message.',
	}),
	[AccountGateActions.REACTION]: msg({
		message: 'Set a password to add reactions.',
		comment: 'Reason shown when someone whose account has no password or passkey yet tries to react to a message.',
	}),
	[AccountGateActions.DIRECT_MESSAGE]: msg({
		message: 'Set a password to send direct messages.',
		comment: 'Reason shown when someone whose account has no password or passkey yet tries to send a direct message.',
	}),
	[AccountGateActions.DIRECT_MESSAGE_OPEN]: msg({
		message: 'Set a password to message people directly.',
		comment:
			'Reason shown when someone whose account has no password or passkey yet tries to start a direct message or group chat.',
	}),
	[AccountGateActions.GROUP_DM]: msg({
		message: 'Set a password to join group chats.',
		comment: 'Reason shown when someone whose account has no password or passkey yet tries to join a group chat.',
	}),
	[AccountGateActions.FRIEND_REQUEST]: msg({
		message: 'Set a password to add friends.',
		comment: 'Reason shown when someone whose account has no password or passkey yet tries to send a friend request.',
	}),
	[AccountGateActions.FRIEND_ACCEPT]: msg({
		message: 'Set a password to accept friend requests.',
		comment: 'Reason shown when someone whose account has no password or passkey yet tries to accept a friend request.',
	}),
	[AccountGateActions.INVITE_CREATE]: msg({
		message: 'Set a password to invite people.',
		comment: 'Reason shown when someone whose account has no password or passkey yet tries to create an invite.',
	}),
	[AccountGateActions.GUILD_JOIN]: msg({
		message: 'Set a password to join this community.',
		comment: 'Reason shown when someone whose account has no password or passkey yet tries to join a community.',
	}),
	[AccountGateActions.GUILD_CREATE]: msg({
		message: 'Set a password to create a community.',
		comment: 'Reason shown when someone whose account has no password or passkey yet tries to create a community.',
	}),
	[AccountGateActions.VOICE]: msg({
		message: 'Set a password to join voice channels.',
		comment: 'Reason shown when someone whose account has no password or passkey yet tries to join a voice channel.',
	}),
	[AccountGateActions.CALL]: msg({
		message: 'Set a password to start or join calls.',
		comment: 'Reason shown when someone whose account has no password or passkey yet tries to start or join a call.',
	}),
	[AccountGateActions.GROUP_CALL]: msg({
		message: 'Set a password to start or join group calls.',
		comment:
			'Reason shown when someone whose account has no password or passkey yet tries to start or join a call in a group chat.',
	}),
	[AccountGateActions.THREAD_JOIN]: msg({
		message: 'Set a password to join threads.',
		comment: 'Reason shown when someone whose account has no password or passkey yet tries to join a thread.',
	}),
	[AccountGateActions.CHANNEL_FOLLOW]: msg({
		message: 'Set a password to follow channels.',
		comment:
			'Reason shown when someone whose account has no password or passkey yet tries to follow an announcement channel.',
	}),
	[AccountGateActions.OWNERSHIP_TRANSFER]: msg({
		message: 'This person needs to set a password before they can own a community.',
		comment:
			'Reason shown when the new owner picked for a community has not finished setting up their account. Variant: they have no password or passkey.',
	}),
	[AccountGateActions.APPLICATION_CREATE]: msg({
		message: 'Set a password to create applications.',
		comment: 'Reason shown when someone whose account has no password or passkey yet tries to create an application.',
	}),
	[AccountGateActions.GIFT_REDEEM]: msg({
		message: 'Set a password to redeem gifts.',
		comment: 'Reason shown when someone whose account has no password or passkey yet tries to redeem a gift.',
	}),
	[AccountGateActions.PROFILE_CUSTOMIZE]: msg({
		message: 'Set a password to edit your profile.',
		comment: 'Reason shown when someone whose account has no password or passkey yet tries to change their profile.',
	}),
	[AccountGateActions.REPORT]: msg({
		message: 'Set a password to submit reports.',
		comment: 'Reason shown when someone whose account has no password or passkey yet tries to submit a report.',
	}),
	[AccountGateActions.MFA]: msg({
		message: 'Set a password to set up two-factor authentication and passkeys.',
		comment:
			'Reason shown when someone whose account has no password or passkey yet tries to turn on two-factor authentication or add a passkey.',
	}),
	[AccountGateActions.GUILD_VERIFICATION]: msg({
		message: 'Set a password to talk in this community.',
		comment:
			'Reason shown when someone whose account has no password or passkey yet tries to talk in a community that needs a finished account.',
	}),
	[AccountGateActions.PURCHASE]: msg({
		message: 'Set a password to make purchases.',
		comment: 'Reason shown when someone whose account has no password or passkey yet starts a purchase.',
	}),
};
const REASONS: Readonly<Record<AccountGateVariant, Readonly<Record<AccountGateAction, MessageDescriptor>>>> = {
	[AccountGateVariants.ADD_EMAIL]: ADD_EMAIL_REASONS,
	[AccountGateVariants.ADD_EMAIL_NO_DELIVERY]: ADD_EMAIL_NO_DELIVERY_REASONS,
	[AccountGateVariants.CONFIRM_EMAIL]: CONFIRM_EMAIL_REASONS,
	[AccountGateVariants.SET_PASSWORD]: SET_PASSWORD_REASONS,
};

const TITLES: Readonly<Record<AccountGateVariant, MessageDescriptor>> = {
	[AccountGateVariants.ADD_EMAIL]: msg({
		message: 'Add an email',
		comment: 'Heading shown when an account needs an email address before it can do something.',
	}),
	[AccountGateVariants.ADD_EMAIL_NO_DELIVERY]: msg({
		message: 'Add an email and a password',
		comment:
			'Heading shown on an instance that cannot send email, when an account needs an email address and a password before it can do something.',
	}),
	[AccountGateVariants.CONFIRM_EMAIL]: msg({
		message: 'Confirm your email',
		comment: 'Heading shown when an account needs to confirm its email address before it can do something.',
	}),
	[AccountGateVariants.SET_PASSWORD]: msg({
		message: 'Set a password',
		comment: 'Heading shown when an account needs a password before it can do something.',
	}),
};

const ACTION_LABELS: Readonly<Record<AccountGateVariant, MessageDescriptor>> = {
	[AccountGateVariants.ADD_EMAIL]: msg({
		message: 'Add email',
		comment: 'Button that opens the dialog for adding an email address to the account.',
	}),
	[AccountGateVariants.ADD_EMAIL_NO_DELIVERY]: msg({
		message: 'Add email and password',
		comment: 'Button that opens the dialog for adding an email address and a password to the account.',
	}),
	[AccountGateVariants.CONFIRM_EMAIL]: msg({
		message: 'Confirm email',
		comment: 'Button that opens the step for confirming the email address on the account.',
	}),
	[AccountGateVariants.SET_PASSWORD]: msg({
		message: 'Set password',
		comment: 'Button that opens the dialog for setting a password on the account.',
	}),
};

const ICONS: Readonly<Record<AccountGateVariant, Icon>> = {
	[AccountGateVariants.ADD_EMAIL]: EnvelopeSimpleIcon,
	[AccountGateVariants.ADD_EMAIL_NO_DELIVERY]: EnvelopeSimpleIcon,
	[AccountGateVariants.CONFIRM_EMAIL]: EnvelopeSimpleIcon,
	[AccountGateVariants.SET_PASSWORD]: UserCircleIcon,
};

export const UnclaimedAccountSurfaces = {
	NAGBAR: 'nagbar',
	ALERT: 'alert',
	SIGN_OUT: 'sign_out',
} as const;

export type UnclaimedAccountSurface = ValueOf<typeof UnclaimedAccountSurfaces>;

export interface UnclaimedAccountPresentation {
	readonly message: string;
	readonly actionLabel: string;
}

const UNCLAIMED_ADD_EMAIL_MESSAGE = msg({
	message: 'This account is unclaimed. Add an email to sign in on other devices.',
	comment: 'Quiet, dismissible bar and settings alert for accounts made without an email. Calm, not a warning.',
});

const UNCLAIMED_ADD_EMAIL_MESSAGES: Readonly<Record<UnclaimedAccountSurface, MessageDescriptor>> = {
	[UnclaimedAccountSurfaces.NAGBAR]: UNCLAIMED_ADD_EMAIL_MESSAGE,
	[UnclaimedAccountSurfaces.ALERT]: UNCLAIMED_ADD_EMAIL_MESSAGE,
	[UnclaimedAccountSurfaces.SIGN_OUT]: msg({
		message: "This account is unclaimed, so you can't sign back in to it. Add an email first to sign in again later.",
		comment: 'Sign out dialog text for an account made without an email.',
	}),
};

const UNCLAIMED_SET_PASSWORD_MESSAGES: Readonly<Record<UnclaimedAccountSurface, MessageDescriptor>> = {
	[UnclaimedAccountSurfaces.NAGBAR]: msg({
		message: 'This account is unclaimed. Set a password to sign in on other devices.',
		comment: 'Quiet, dismissible bar for accounts that need a password before they can sign in on other devices.',
	}),
	[UnclaimedAccountSurfaces.ALERT]: msg({
		message: 'This account has no password yet. Set a password to sign in on other devices.',
		comment: 'Settings alert for an account that has an email but needs a password to sign in on other devices.',
	}),
	[UnclaimedAccountSurfaces.SIGN_OUT]: msg({
		message: "This account is unclaimed, so you can't sign back in to it. Set a password first to sign in again later.",
		comment: 'Sign out dialog text for an account that needs a password before it can sign back in.',
	}),
};

const UNCLAIMED_USERNAME_MESSAGES: Readonly<Record<UnclaimedAccountSurface, MessageDescriptor>> = {
	...UNCLAIMED_SET_PASSWORD_MESSAGES,
	[UnclaimedAccountSurfaces.ALERT]: msg({
		message:
			"Your account is not yet claimed. Without a username and password, you won't be able to sign in from other devices and you could lose access to your account. Claim your account now to secure it.",
		comment: 'Settings alert for an unclaimed account on an instance where people sign in with a username.',
	}),
};

function unclaimedAccountMessages(
	needsPassword: boolean,
): Readonly<Record<UnclaimedAccountSurface, MessageDescriptor>> {
	if (!needsPassword) return UNCLAIMED_ADD_EMAIL_MESSAGES;
	return RuntimeConfig.usesUsernameSignIn ? UNCLAIMED_USERNAME_MESSAGES : UNCLAIMED_SET_PASSWORD_MESSAGES;
}

export function getUnclaimedAccountPresentation(
	i18n: I18n,
	surface: UnclaimedAccountSurface,
	need: AccountVerificationNeed | null | undefined,
): UnclaimedAccountPresentation {
	const needsPassword = need === AccountVerificationNeeds.SET_PASSWORD;
	return {
		message: i18n._(unclaimedAccountMessages(needsPassword)[surface]),
		actionLabel: i18n._(needsPassword ? CLAIM_ACCOUNT_DESCRIPTOR : ADD_EMAIL_DESCRIPTOR),
	};
}

function accountGateVariant(need: AccountVerificationNeed): AccountGateVariant {
	switch (need) {
		case AccountVerificationNeeds.ADD_EMAIL:
			return RuntimeConfig.emailDeliveryState === EmailDeliveryStates.ON
				? AccountGateVariants.ADD_EMAIL
				: AccountGateVariants.ADD_EMAIL_NO_DELIVERY;
		case AccountVerificationNeeds.CONFIRM_EMAIL:
			return AccountGateVariants.CONFIRM_EMAIL;
		case AccountVerificationNeeds.SET_PASSWORD:
			return AccountGateVariants.SET_PASSWORD;
	}
}

export function accountGateReason(i18n: I18n, gate: AccountGate): string {
	return i18n._(REASONS[accountGateVariant(gate.need)][gate.action]);
}

export function openAccountGateFlow(
	i18n: I18n,
	gate: AccountGate,
	onComplete?: () => void,
	onDismiss?: () => void,
): void {
	if (gate.need === AccountVerificationNeeds.CONFIRM_EMAIL && Users.currentUser?.emailBounced === true) {
		openBouncedEmailChange();
		return;
	}
	const reason = accountGateReason(i18n, gate);
	void import('@app/features/auth/components/modals/ClaimAccountModal').then(({openClaimAccountModal}) =>
		openClaimAccountModal({reason, onComplete, onDismiss}),
	);
}

export function getAccountGatePresentation(
	i18n: I18n,
	gate: AccountGate,
	onComplete?: () => void,
): AccountGatePresentation {
	const variant = accountGateVariant(gate.need);
	return {
		Icon: ICONS[variant],
		title: i18n._(TITLES[variant]),
		description: i18n._(REASONS[variant][gate.action]),
		actionLabel: i18n._(ACTION_LABELS[variant]),
		onAction: () => openAccountGateFlow(i18n, gate, onComplete),
	};
}

interface AccountGateActionButton {
	readonly text: string;
	readonly onClick: () => void;
	readonly submitting: boolean;
}

export function useAccountGateAction(gate: AccountGate, onComplete?: () => void): AccountGateActionButton {
	const {i18n} = useLingui();
	const verifyEmailAction = useVerifyEmailAction();
	if (gate.need === AccountVerificationNeeds.CONFIRM_EMAIL) return verifyEmailAction;
	const presentation = getAccountGatePresentation(i18n, gate, onComplete);
	return {text: presentation.actionLabel, onClick: presentation.onAction, submitting: false};
}

interface AccountGateSlateProps {
	readonly gate: AccountGate;
	readonly onComplete?: () => void;
	readonly fullHeight?: boolean;
}

export const AccountGateSlate: React.FC<AccountGateSlateProps> = observer(({gate, onComplete, fullHeight}) => {
	const {i18n} = useLingui();
	const presentation = getAccountGatePresentation(i18n, gate, onComplete);
	const action = useAccountGateAction(gate, onComplete);
	return (
		<StatusSlate
			Icon={presentation.Icon}
			title={presentation.title}
			description={presentation.description}
			actions={[{text: action.text, onClick: action.onClick, submitting: action.submitting}]}
			fullHeight={fullHeight}
			data-flx="user.account-gate-slate.status-slate"
		/>
	);
});
