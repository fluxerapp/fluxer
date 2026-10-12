// SPDX-License-Identifier: AGPL-3.0-or-later

import modalI18n from '@app/app/I18n';
import {FeatureTemporarilyDisabledModal} from '@app/features/app/components/alerts/FeatureTemporarilyDisabledModal';
import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Authentication from '@app/features/auth/state/Authentication';
import {GuildAtCapacityModal} from '@app/features/guild/components/alerts/GuildAtCapacityModal';
import {MaxGuildsModal} from '@app/features/guild/components/alerts/MaxGuildsModal';
import {TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {InvitesDisabledModal} from '@app/features/invite/components/alerts/InvitesDisabledModal';
import {InviteAcceptModal} from '@app/features/invite/components/modals/InviteAcceptModal';
import Invites from '@app/features/invite/state/Invites';
import {isGroupDmInvite, isGuildInvite} from '@app/features/invite/types/InviteTypes';
import GuildMembers from '@app/features/member/state/GuildMembers';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {type InstanceHTTPTarget, instanceRequest} from '@app/features/platform/transport/InstanceHTTP';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {isAbortError} from '@app/features/platform/utils/PlatformHelpers';
import {failureCode, failureMessage} from '@app/features/platform/utils/ResponseInspection';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import Users from '@app/features/user/state/Users';
import {blockIfAccountLimited, showAccountLimitedModal} from '@app/features/user/utils/AccountLimitUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {Invite} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const InviteAcceptFailedModal_FAILED_TO_ACCEPT_INVITE_DESCRIPTOR = msg({
	message: "Couldn't join community",
	comment: 'Error modal title shown when accepting a community invite fails.',
});

const UserIpBannedFromGuildModal_YOUR_IP_IS_BANNED_DESCRIPTOR = msg({
	message: 'Your IP is banned',
	comment: 'Label in the user ip banned from guild modal. Keep the tone plain and specific.',
});

const UserIpBannedFromGuildModal_YOUR_IP_ADDRESS_IS_BANNED_FROM_THIS_COMMUNITY_DESCRIPTOR = msg({
	message: 'Your IP address is banned from this community and you cannot join.',
	comment: 'Error message in the user ip banned from guild modal. Keep the tone plain and specific.',
});

const UserBannedFromGuildModal_YOU_RE_BANNED_DESCRIPTOR = msg({
	message: "You're banned",
	comment: 'Short label in the user banned from guild modal. Keep it concise. Keep the tone plain and specific.',
});

const UserBannedFromGuildModal_YOU_ARE_BANNED_FROM_THIS_COMMUNITY_AND_CANNOT_DESCRIPTOR = msg({
	message: 'You are banned from this community and cannot join.',
	comment: 'Error message in the user banned from guild modal. Keep the tone plain and specific.',
});

const TemporaryInviteRequiresPresenceModal_GATEWAY_CONNECTION_REQUIRED_DESCRIPTOR = msg({
	message: 'Gateway connection required',
	comment: 'Short label in the temporary invite requires presence modal.',
});

const TemporaryInviteRequiresPresenceModal_YOU_MUST_BE_CONNECTED_TO_THE_GATEWAY_TO_DESCRIPTOR = msg({
	message: 'Temporary invites need a live connection. Check yours and try again.',
	comment:
		'Modal body shown when the user tries to accept a temporary invite while the realtime connection is offline.',
});

const ACCOUNT_VERIFICATION_REQUIRED_DESCRIPTOR = msg({
	message: 'Account verification required',
	comment:
		'Title of the error modal shown when an unclaimed (guest) account tries to accept a community or group DM invite. Auth-related; keep tone plain.',
});
const PLEASE_VERIFY_YOUR_ACCOUNT_BY_SETTING_AN_EMAIL_DESCRIPTOR = msg({
	message: 'Add an email and password to your account first.',
	comment:
		'Body of the error modal shown when an unclaimed (guest) account tries to accept a community invite. Tells the user to complete sign-up first.',
});
const ADD_A_USERNAME_AND_PASSWORD_DESCRIPTOR = msg({
	message: 'Add a username and password to your account first.',
	comment:
		'Body of the error modal shown when an unclaimed (guest) account tries to accept a community invite on an instance where people sign in with a username.',
});
const logger = new Logger('Invites');
const ACCEPT_INVITE_BODY = {} as Invite;
const isUnclaimedAccountInviteError = (code?: string): boolean => {
	return code === APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_JOIN_GROUP_DMS;
};
const shouldOpenInviteGuildChannel = (channelType: number): boolean =>
	channelType !== ChannelTypes.GUILD_CATEGORY && channelType !== ChannelTypes.GUILD_LINK;

function inviteTargetChannelId(channel: {id: string; type: number}): string | undefined {
	return shouldOpenInviteGuildChannel(channel.type) ? channel.id : undefined;
}

function isCurrentUserGuildMember(guildId: string): boolean {
	const currentUserId = Authentication.currentUserId;
	return currentUserId ? GuildMembers.getMember(guildId, currentUserId) != null : false;
}

function guildInviteFeatures(invite: Invite | null): Array<string> {
	return invite && isGuildInvite(invite) && Array.isArray(invite.guild.features) ? invite.guild.features : [];
}

function removeInviteIfMissing(
	code: string,
	target: InstanceHTTPTarget,
	responseErr: HttpError | null,
	errorCode?: string,
): void {
	if (responseErr?.status === 404 || errorCode === APIErrorCodes.UNKNOWN_INVITE) {
		logger.debug(`Invite ${code} not found, removing from store`);
		Invites.handleInviteDelete(code, target);
	}
}

function showUnclaimedAccountInviteModal(i18n: I18n): void {
	ModalCommands.push(
		modal(() => (
			<GenericErrorModal
				title={i18n._(ACCOUNT_VERIFICATION_REQUIRED_DESCRIPTOR)}
				message={i18n._(
					RuntimeConfig.usesUsernameSignIn
						? ADD_A_USERNAME_AND_PASSWORD_DESCRIPTOR
						: PLEASE_VERIFY_YOUR_ACCOUNT_BY_SETTING_AN_EMAIL_DESCRIPTOR,
				)}
				data-flx="invite.invite-commands.show-unclaimed-account-invite-modal.generic-error-modal"
			/>
		)),
	);
}

function showGuildInviteAcceptFailure(
	i18n: I18n,
	invite: Invite | null,
	errorCode: string | undefined,
	responseErr: HttpError | null,
): void {
	const isRaidDetected = guildInviteFeatures(invite).includes(GuildFeatures.RAID_DETECTED);
	if (errorCode === APIErrorCodes.ACCOUNT_LIMITED) {
		showAccountLimitedModal(responseErr ? failureMessage(responseErr) : undefined);
	} else if (errorCode === APIErrorCodes.INVITES_DISABLED) {
		ModalCommands.push(
			modal(() => (
				<InvitesDisabledModal
					isRaidDetected={isRaidDetected}
					data-flx="invite.invite-commands.show-guild-invite-accept-failure.invites-disabled-modal"
				/>
			)),
		);
	} else if (responseErr?.status === 403 && errorCode === APIErrorCodes.FEATURE_TEMPORARILY_DISABLED) {
		ModalCommands.push(
			modal(() => (
				<FeatureTemporarilyDisabledModal data-flx="invite.invite-commands.show-guild-invite-accept-failure.feature-temporarily-disabled-modal" />
			)),
		);
	} else if (errorCode === APIErrorCodes.MAX_GUILD_MEMBERS) {
		ModalCommands.push(
			modal(() => (
				<GuildAtCapacityModal data-flx="invite.invite-commands.show-guild-invite-accept-failure.guild-at-capacity-modal" />
			)),
		);
	} else if (errorCode === APIErrorCodes.MAX_GUILDS) {
		const currentUser = Users.currentUser;
		if (currentUser) {
			ModalCommands.push(
				modal(() => (
					<MaxGuildsModal
						user={currentUser}
						data-flx="invite.invite-commands.show-guild-invite-accept-failure.max-guilds-modal"
					/>
				)),
			);
		}
	} else if (errorCode === APIErrorCodes.TEMPORARY_INVITE_REQUIRES_PRESENCE) {
		showGenericErrorModal({
			title: () => modalI18n._(TemporaryInviteRequiresPresenceModal_GATEWAY_CONNECTION_REQUIRED_DESCRIPTOR),
			message: () =>
				modalI18n._(TemporaryInviteRequiresPresenceModal_YOU_MUST_BE_CONNECTED_TO_THE_GATEWAY_TO_DESCRIPTOR),
			dataFlx: 'app.temporary-invite-requires-presence-modal.confirm-modal',
		});
	} else if (errorCode === APIErrorCodes.USER_BANNED_FROM_GUILD) {
		showGenericErrorModal({
			title: () => modalI18n._(UserBannedFromGuildModal_YOU_RE_BANNED_DESCRIPTOR),
			message: () => modalI18n._(UserBannedFromGuildModal_YOU_ARE_BANNED_FROM_THIS_COMMUNITY_AND_CANNOT_DESCRIPTOR),
			dataFlx: 'moderation.user-banned-from-guild-modal.confirm-modal',
		});
	} else if (errorCode === APIErrorCodes.USER_IP_BANNED_FROM_GUILD) {
		showGenericErrorModal({
			title: () => modalI18n._(UserIpBannedFromGuildModal_YOUR_IP_IS_BANNED_DESCRIPTOR),
			message: () => modalI18n._(UserIpBannedFromGuildModal_YOUR_IP_ADDRESS_IS_BANNED_FROM_THIS_COMMUNITY_DESCRIPTOR),
			dataFlx: 'moderation.user-ip-banned-from-guild-modal.confirm-modal',
		});
	} else if (isUnclaimedAccountInviteError(errorCode)) {
		showUnclaimedAccountInviteModal(i18n);
	} else if (responseErr?.status && responseErr.status >= 400) {
		showGenericErrorModal({
			title: () => modalI18n._(InviteAcceptFailedModal_FAILED_TO_ACCEPT_INVITE_DESCRIPTOR),
			message: () => modalI18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
			dataFlx: 'invite.invite-accept-failed-modal.generic-error-modal',
		});
	}
}

export async function fetch(code: string, target: InstanceHTTPTarget): Promise<Invite> {
	try {
		logger.debug(`Fetching invite with code ${code}`);
		const response = await instanceRequest<Invite>({method: 'GET', path: Endpoints.INVITE(code), target});
		return response.body;
	} catch (error) {
		logger.error(`Failed to fetch invite with code ${code}:`, error);
		throw error;
	}
}

export async function fetchWithCoalescing(code: string, target: InstanceHTTPTarget): Promise<Invite> {
	return Invites.fetchInvite(code, target);
}

const accept = async (code: string, target: InstanceHTTPTarget): Promise<Invite> => {
	if (blockIfAccountLimited()) {
		throw new DOMException('Invite accept skipped', 'AbortError');
	}
	try {
		logger.debug(`Accepting invite with code ${code}`);
		const response = await instanceRequest<Invite>({
			method: 'POST',
			path: Endpoints.INVITE(code),
			target,
			body: ACCEPT_INVITE_BODY,
		});
		return response.body;
	} catch (error) {
		logger.error(`Failed to accept invite with code ${code}:`, error);
		throw error;
	}
};

export async function acceptAndTransitionToChannel(
	code: string,
	i18n: I18n,
	target: InstanceHTTPTarget,
): Promise<void> {
	let invite: Invite | null = null;
	try {
		logger.debug(`Fetching invite details before accepting: ${code}`);
		invite = await fetchWithCoalescing(code, target);
		if (!invite) {
			throw new Error(`Invite ${code} returned no data`);
		}
		if (isGroupDmInvite(invite)) {
			const channelId = invite.channel.id;
			logger.debug(`Accepting group DM invite ${code} and opening channel ${channelId}`);
			await accept(code, target);
			NavigationCommands.selectChannel(ME, channelId);
			return;
		}
		if (!isGuildInvite(invite)) {
			throw new Error(`Invite ${code} is not a guild or group DM invite`);
		}
		const channelId = invite.channel.id;
		const inviteTargetAllowed = shouldOpenInviteGuildChannel(invite.channel.type);
		const targetChannelId = inviteTargetChannelId(invite.channel);
		const guildId = invite.guild.id;
		if (isCurrentUserGuildMember(guildId)) {
			logger.debug(
				inviteTargetAllowed
					? `User already in guild ${guildId}, transitioning to channel ${channelId}`
					: `User already in guild ${guildId}, invite target is non-viewable, transitioning to guild root`,
			);
			NavigationCommands.selectChannel(guildId, targetChannelId);
			return;
		}
		logger.debug(`User not in guild ${guildId}, accepting invite ${code}`);
		await accept(code, target);
		logger.debug(
			inviteTargetAllowed
				? `Transitioning to channel ${channelId} in guild ${guildId}`
				: `Invite target channel ${channelId} in guild ${guildId} is non-viewable, transitioning to guild root`,
		);
		NavigationCommands.selectChannel(guildId, targetChannelId);
	} catch (error) {
		if (isAbortError(error)) throw error;
		const responseErr = error instanceof HttpError ? error : null;
		const errorCode = failureCode(error);
		logger.error(`Failed to accept invite and transition for code ${code}:`, error);
		removeInviteIfMissing(code, target, responseErr, errorCode);
		showGuildInviteAcceptFailure(i18n, invite, errorCode, responseErr);
		throw error;
	}
}

export async function openAcceptModal(code: string, target: InstanceHTTPTarget): Promise<void> {
	void fetchWithCoalescing(code, target).catch(() => {});
	ModalCommands.pushWithKey(
		modal(() => (
			<InviteAcceptModal
				code={code}
				target={target}
				data-flx="invite.invite-commands.open-accept-modal.invite-accept-modal"
			/>
		)),
		`invite-accept-${code}`,
	);
}

export async function create(
	channelId: string,
	params?: {max_age?: number; max_uses?: number; temporary?: boolean},
): Promise<Invite> {
	try {
		logger.debug(`Creating invite for channel ${channelId}`);
		const response = await http.post<Invite>(Endpoints.CHANNEL_INVITES(channelId), {body: params ?? {}});
		return response.body;
	} catch (error) {
		logger.error(`Failed to create invite for channel ${channelId}:`, error);
		throw error;
	}
}

export async function list(channelId: string): Promise<Array<Invite>> {
	try {
		logger.debug(`Listing invites for channel ${channelId}`);
		const response = await http.get<Array<Invite>>(Endpoints.CHANNEL_INVITES(channelId));
		return response.body;
	} catch (error) {
		logger.error(`Failed to list invites for channel ${channelId}:`, error);
		throw error;
	}
}

export async function remove(code: string): Promise<void> {
	try {
		logger.debug(`Deleting invite with code ${code}`);
		await http.delete(Endpoints.INVITE(code));
	} catch (error) {
		logger.error(`Failed to delete invite with code ${code}:`, error);
		throw error;
	}
}
