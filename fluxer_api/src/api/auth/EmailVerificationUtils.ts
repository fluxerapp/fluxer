// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DirectMessageEmailVerificationRequiredError,
	EmailVerificationRequiredError,
	FriendRequestEmailVerificationRequiredError,
	GuildCreationEmailVerificationRequiredError,
	GuildEmailVerificationRequiredError,
	MfaEmailVerificationRequiredError,
	PollVoteVerificationRequiredError,
	ProfileEmailVerificationRequiredError,
	ReactionEmailVerificationRequiredError,
	ReportEmailVerificationRequiredError,
} from '@fluxer/errors/src/domains/auth/EmailVerificationRequiredError';

export type EmailVerificationRequiredReason =
	| 'direct_message'
	| 'friend_request'
	| 'guild'
	| 'guild_creation'
	| 'mfa'
	| 'profile'
	| 'reaction'
	| 'vote'
	| 'report';

const ErrorByReason = {
	direct_message: DirectMessageEmailVerificationRequiredError,
	friend_request: FriendRequestEmailVerificationRequiredError,
	guild: GuildEmailVerificationRequiredError,
	guild_creation: GuildCreationEmailVerificationRequiredError,
	mfa: MfaEmailVerificationRequiredError,
	profile: ProfileEmailVerificationRequiredError,
	reaction: ReactionEmailVerificationRequiredError,
	vote: PollVoteVerificationRequiredError,
	report: ReportEmailVerificationRequiredError,
} satisfies Record<EmailVerificationRequiredReason, new () => EmailVerificationRequiredError>;

export function requireEmailVerified(
	user: {emailVerified: boolean; isBot?: boolean},
	reason?: EmailVerificationRequiredReason,
): void {
	if (user.isBot) {
		return;
	}
	if (!user.emailVerified) {
		const ErrorClass = reason ? ErrorByReason[reason] : EmailVerificationRequiredError;
		throw new ErrorClass();
	}
}
