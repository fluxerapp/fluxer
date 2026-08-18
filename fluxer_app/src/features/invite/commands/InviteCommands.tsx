// SPDX-License-Identifier: AGPL-3.0-or-later

import {FeatureTemporarilyDisabledModal} from '@app/features/app/components/alerts/FeatureTemporarilyDisabledModal';
import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {TemporaryInviteRequiresPresenceModal} from '@app/features/app/components/alerts/TemporaryInviteRequiresPresenceModal';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import Authentication from '@app/features/auth/state/Authentication';
import {GuildAtCapacityModal} from '@app/features/guild/components/alerts/GuildAtCapacityModal';
import {MaxGuildsModal} from '@app/features/guild/components/alerts/MaxGuildsModal';
import {NewAccountGuildLimitModal} from '@app/features/guild/components/alerts/NewAccountGuildLimitModal';
import {InviteAcceptFailedModal} from '@app/features/invite/components/alerts/InviteAcceptFailedModal';
import {InvitesDisabledModal} from '@app/features/invite/components/alerts/InvitesDisabledModal';
import {InviteAcceptModal} from '@app/features/invite/components/modals/InviteAcceptModal';
import Invites from '@app/features/invite/state/Invites';
import {isGroupDmInvite, isGuildInvite, isPackInvite} from '@app/features/invite/types/InviteTypes';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {UserBannedFromGuildModal} from '@app/features/moderation/components/alerts/UserBannedFromGuildModal';
import {UserIpBannedFromGuildModal} from '@app/features/moderation/components/alerts/UserIpBannedFromGuildModal';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {failureCode, failureMessage} from '@app/features/platform/utils/ResponseInspection';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import Users from '@app/features/user/state/Users';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {Invite} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import type {I18n} from '@lingui/core';
import {msg, plural} from '@lingui/core/macro';
import {Trans} from '@lingui/react/macro';

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
const EMOJI_PACK_LIMIT_REACHED_DESCRIPTOR = msg({
	message: 'Emoji pack limit reached',
	comment:
		'Title of the error modal when installing an emoji pack via invite is rejected because the user is at the install limit.',
});
const YOU_HAVE_REACHED_THE_LIMIT_FOR_INSTALLING_EMOJI_DESCRIPTOR = msg({
	message: "You're at the install limit. Remove one to add another.",
	comment: 'Fallback body of the emoji pack install-limit error modal when no exact limit is known.',
});
const EMOJI_PACK_CREATION_LIMIT_REACHED_DESCRIPTOR = msg({
	message: 'Emoji pack creation limit reached',
	comment:
		'Title of the error modal when creating a new emoji pack is rejected because the user is at the creation limit.',
});
const YOU_HAVE_REACHED_THE_LIMIT_FOR_CREATING_EMOJI_DESCRIPTOR = msg({
	message: "You're at the creation limit. Delete one to create another.",
	comment: 'Fallback body of the emoji pack creation-limit error modal when no exact limit is known.',
});
const STICKER_PACK_LIMIT_REACHED_DESCRIPTOR = msg({
	message: 'Sticker pack limit reached',
	comment:
		'Title of the error modal when installing a sticker pack via invite is rejected because the user is at the install limit.',
});
const YOU_HAVE_REACHED_THE_LIMIT_FOR_INSTALLING_STICKER_DESCRIPTOR = msg({
	message: "You're at the install limit. Remove one to add another.",
	comment: 'Fallback body of the sticker pack install-limit error modal when no exact limit is known.',
});
const STICKER_PACK_CREATION_LIMIT_REACHED_DESCRIPTOR = msg({
	message: 'Sticker pack creation limit reached',
	comment:
		'Title of the error modal when creating a new sticker pack is rejected because the user is at the creation limit.',
});
const YOU_HAVE_REACHED_THE_LIMIT_FOR_CREATING_STICKER_DESCRIPTOR = msg({
	message: "You're at the creation limit. Delete one to create another.",
	comment: 'Fallback body of the sticker pack creation-limit error modal when no exact limit is known.',
});
const CANNOT_INSTALL_EMOJI_PACK_DESCRIPTOR = msg({
	message: 'Cannot install emoji pack',
	comment: 'Title of the error modal when installing an emoji pack via invite is rejected for missing permissions.',
});
const CANNOT_INSTALL_STICKER_PACK_DESCRIPTOR = msg({
	message: 'Cannot install sticker pack',
	comment: 'Title of the error modal when installing a sticker pack via invite is rejected for missing permissions.',
});
const YOU_DON_T_HAVE_PERMISSION_TO_INSTALL_THIS_DESCRIPTOR = msg({
	message: "You can't install this emoji pack.",
	comment: 'Body of the error modal when installing an emoji pack via invite is rejected for missing permissions.',
});
const YOU_DON_T_HAVE_PERMISSION_TO_INSTALL_THIS_2_DESCRIPTOR = msg({
	message: "You can't install this sticker pack.",
	comment: 'Body of the error modal when installing a sticker pack via invite is rejected for missing permissions.',
});
const UNABLE_TO_INSTALL_EMOJI_PACK_DESCRIPTOR = msg({
	message: 'Unable to install emoji pack',
	comment: 'Generic title for the error modal when an emoji pack invite fails with an unrecognized error code.',
});
const UNABLE_TO_INSTALL_STICKER_PACK_DESCRIPTOR = msg({
	message: 'Unable to install sticker pack',
	comment: 'Generic title for the error modal when a sticker pack invite fails with an unrecognized error code.',
});
const FAILED_TO_INSTALL_THIS_EMOJI_PACK_PLEASE_TRY_DESCRIPTOR = msg({
	message: "Couldn't install this emoji pack. Try again later.",
	comment: 'Generic body for the error modal when an emoji pack invite fails with an unrecognized error code.',
});
const FAILED_TO_INSTALL_THIS_STICKER_PACK_PLEASE_TRY_DESCRIPTOR = msg({
	message: "Couldn't install this sticker pack. Try again later.",
	comment: 'Generic body for the error modal when a sticker pack invite fails with an unrecognized error code.',
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

function showPackInstalledToast(invite: Invite): void {
	if (!isPackInvite(invite)) {
		return;
	}
	ToastCommands.createToast({
		type: 'success',
		children:
			invite.pack.type === 'emoji' ? (
				<Trans>Emoji pack {invite.pack.name} has been installed.</Trans>
			) : (
				<Trans>Sticker pack {invite.pack.name} has been installed.</Trans>
			),
	});
}

function removeInviteIfMissing(code: string, responseErr: HttpError | null, errorCode?: string): void {
	if (responseErr?.status === 404 || errorCode === APIErrorCodes.UNKNOWN_INVITE) {
		logger.debug(`Invite ${code} not found, removing from store`);
		Invites.handleInviteDelete(code);
	}
}

function showUnclaimedAccountInviteModal(i18n: I18n): void {
	ModalCommands.push(
		modal(() => (
			<GenericErrorModal
				title={i18n._(ACCOUNT_VERIFICATION_REQUIRED_DESCRIPTOR)}
				message={i18n._(PLEASE_VERIFY_YOUR_ACCOUNT_BY_SETTING_AN_EMAIL_DESCRIPTOR)}
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
	if (
		errorCode === APIErrorCodes.ACCOUNT_SUSPICIOUS_ACTIVITY &&
		(Users.currentUser?.requiredActions?.length ?? 0) > 0
	) {
		return;
	}
	if (errorCode === APIErrorCodes.INVITES_DISABLED) {
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
	} else if (errorCode === APIErrorCodes.NEW_ACCOUNT_GUILD_JOIN_RATE_LIMITED) {
		ModalCommands.push(
			modal(() => (
				<NewAccountGuildLimitModal data-flx="invite.invite-commands.show-guild-invite-accept-failure.new-account-guild-limit-modal" />
			)),
		);
	} else if (errorCode === APIErrorCodes.TEMPORARY_INVITE_REQUIRES_PRESENCE) {
		ModalCommands.push(
			modal(() => (
				<TemporaryInviteRequiresPresenceModal data-flx="invite.invite-commands.show-guild-invite-accept-failure.temporary-invite-requires-presence-modal" />
			)),
		);
	} else if (errorCode === APIErrorCodes.USER_BANNED_FROM_GUILD) {
		ModalCommands.push(
			modal(() => (
				<UserBannedFromGuildModal data-flx="invite.invite-commands.show-guild-invite-accept-failure.user-banned-from-guild-modal" />
			)),
		);
	} else if (errorCode === APIErrorCodes.USER_IP_BANNED_FROM_GUILD) {
		ModalCommands.push(
			modal(() => (
				<UserIpBannedFromGuildModal data-flx="invite.invite-commands.show-guild-invite-accept-failure.user-ip-banned-from-guild-modal" />
			)),
		);
	} else if (isUnclaimedAccountInviteError(errorCode)) {
		showUnclaimedAccountInviteModal(i18n);
	} else if (responseErr?.status && responseErr.status >= 400) {
		ModalCommands.push(
			modal(() => (
				<InviteAcceptFailedModal data-flx="invite.invite-commands.show-guild-invite-accept-failure.invite-accept-failed-modal" />
			)),
		);
	}
}

export async function fetch(code: string): Promise<Invite> {
	try {
		logger.debug(`Fetching invite with code ${code}`);
		const response = await http.get<Invite>(Endpoints.INVITE(code));
		return response.body;
	} catch (error) {
		logger.error(`Failed to fetch invite with code ${code}:`, error);
		throw error;
	}
}

export async function fetchWithCoalescing(code: string): Promise<Invite> {
	return Invites.fetchInvite(code);
}

const accept = async (code: string): Promise<Invite> => {
	try {
		logger.debug(`Accepting invite with code ${code}`);
		const response = await http.post<Invite>(Endpoints.INVITE(code), {body: ACCEPT_INVITE_BODY});
		return response.body;
	} catch (error) {
		logger.error(`Failed to accept invite with code ${code}:`, error);
		throw error;
	}
};
export const acceptInvite = accept;

export async function acceptAndTransitionToChannel(code: string, i18n: I18n): Promise<void> {
	let invite: Invite | null = null;
	try {
		logger.debug(`Fetching invite details before accepting: ${code}`);
		invite = await fetchWithCoalescing(code);
		if (!invite) {
			throw new Error(`Invite ${code} returned no data`);
		}
		if (isPackInvite(invite)) {
			await accept(code);
			showPackInstalledToast(invite);
			return;
		}
		if (isGroupDmInvite(invite)) {
			const channelId = invite.channel.id;
			logger.debug(`Accepting group DM invite ${code} and opening channel ${channelId}`);
			await accept(code);
			NavigationCommands.selectChannel(ME, channelId);
			return;
		}
		if (!isGuildInvite(invite)) {
			throw new Error(`Invite ${code} is not a guild, group DM, or pack invite`);
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
		await accept(code);
		logger.debug(
			inviteTargetAllowed
				? `Transitioning to channel ${channelId} in guild ${guildId}`
				: `Invite target channel ${channelId} in guild ${guildId} is non-viewable, transitioning to guild root`,
		);
		NavigationCommands.selectChannel(guildId, targetChannelId);
	} catch (error) {
		const responseErr = error instanceof HttpError ? error : null;
		const errorCode = failureCode(error);
		logger.error(`Failed to accept invite and transition for code ${code}:`, error);
		removeInviteIfMissing(code, responseErr, errorCode);
		if (handlePackInviteError({invite, errorCode, responseErr, i18n})) {
			throw error;
		}
		showGuildInviteAcceptFailure(i18n, invite, errorCode, responseErr);
		throw error;
	}
}

export async function openAcceptModal(code: string): Promise<void> {
	void fetchWithCoalescing(code).catch(() => {});
	ModalCommands.pushWithKey(
		modal(() => (
			<InviteAcceptModal code={code} data-flx="invite.invite-commands.open-accept-modal.invite-accept-modal" />
		)),
		`invite-accept-${code}`,
	);
}

interface HandlePackInviteErrorParams {
	invite: Invite | null;
	errorCode?: string;
	responseErr?: HttpError | null;
	i18n: I18n;
}

interface PackLimitPayload {
	packType?: 'emoji' | 'sticker';
	limit?: number;
	action?: 'create' | 'install';
}

const getPackLimitPayload = (responseErr?: HttpError | null): PackLimitPayload | null => {
	const body = responseErr?.body;
	if (!body || typeof body !== 'object') return null;
	const record = body as Record<string, unknown>;
	const data = record.data;
	if (!data || typeof data !== 'object') return null;
	const dataRecord = data as Record<string, unknown>;
	const limit = dataRecord.limit;
	const packType = dataRecord.pack_type;
	const action = dataRecord.action;
	return {
		packType: packType === 'emoji' || packType === 'sticker' ? packType : undefined,
		limit: typeof limit === 'number' ? limit : undefined,
		action: action === 'create' || action === 'install' ? action : undefined,
	};
};
const buildPackLimitStrings = (
	i18n: I18n,
	packType: 'emoji' | 'sticker',
	action: 'install' | 'create',
	limit?: number,
): {title: string; message: string} => {
	switch (packType) {
		case 'emoji': {
			switch (action) {
				case 'install': {
					const title = i18n._(EMOJI_PACK_LIMIT_REACHED_DESCRIPTOR);
					const message =
						typeof limit === 'number'
							? plural(
									{count: limit},
									{
										one: "You're at the limit of # installed emoji pack. Remove one to add another.",
										other: "You're at the limit of # installed emoji packs. Remove one to add another.",
									},
								)
							: i18n._(YOU_HAVE_REACHED_THE_LIMIT_FOR_INSTALLING_EMOJI_DESCRIPTOR);
					return {title, message};
				}
				default: {
					const title = i18n._(EMOJI_PACK_CREATION_LIMIT_REACHED_DESCRIPTOR);
					const message =
						typeof limit === 'number'
							? plural(
									{count: limit},
									{
										one: "You're at the limit of # emoji pack. Delete one to create another.",
										other: "You're at the limit of # emoji packs. Delete one to create another.",
									},
								)
							: i18n._(YOU_HAVE_REACHED_THE_LIMIT_FOR_CREATING_EMOJI_DESCRIPTOR);
					return {title, message};
				}
			}
		}
		default: {
			switch (action) {
				case 'install': {
					const title = i18n._(STICKER_PACK_LIMIT_REACHED_DESCRIPTOR);
					const message =
						typeof limit === 'number'
							? plural(
									{count: limit},
									{
										one: "You're at the limit of # installed sticker pack. Remove one to add another.",
										other: "You're at the limit of # installed sticker packs. Remove one to add another.",
									},
								)
							: i18n._(YOU_HAVE_REACHED_THE_LIMIT_FOR_INSTALLING_STICKER_DESCRIPTOR);
					return {title, message};
				}
				default: {
					const title = i18n._(STICKER_PACK_CREATION_LIMIT_REACHED_DESCRIPTOR);
					const message =
						typeof limit === 'number'
							? plural(
									{count: limit},
									{
										one: "You're at the limit of # sticker pack. Delete one to create another.",
										other: "You're at the limit of # sticker packs. Delete one to create another.",
									},
								)
							: i18n._(YOU_HAVE_REACHED_THE_LIMIT_FOR_CREATING_STICKER_DESCRIPTOR);
					return {title, message};
				}
			}
		}
	}
};

export function handlePackInviteError(params: HandlePackInviteErrorParams): boolean {
	const {invite, errorCode, responseErr, i18n} = params;
	if (!invite || !isPackInvite(invite)) {
		return false;
	}
	const isEmojiPack = invite.pack.type === 'emoji';
	const cannotInstallTitle = isEmojiPack
		? i18n._(CANNOT_INSTALL_EMOJI_PACK_DESCRIPTOR)
		: i18n._(CANNOT_INSTALL_STICKER_PACK_DESCRIPTOR);
	const cannotInstallMessage = isEmojiPack
		? i18n._(YOU_DON_T_HAVE_PERMISSION_TO_INSTALL_THIS_DESCRIPTOR)
		: i18n._(YOU_DON_T_HAVE_PERMISSION_TO_INSTALL_THIS_2_DESCRIPTOR);
	const defaultTitle = isEmojiPack
		? i18n._(UNABLE_TO_INSTALL_EMOJI_PACK_DESCRIPTOR)
		: i18n._(UNABLE_TO_INSTALL_STICKER_PACK_DESCRIPTOR);
	const defaultMessage = isEmojiPack
		? i18n._(FAILED_TO_INSTALL_THIS_EMOJI_PACK_PLEASE_TRY_DESCRIPTOR)
		: i18n._(FAILED_TO_INSTALL_THIS_STICKER_PACK_PLEASE_TRY_DESCRIPTOR);
	if (errorCode === APIErrorCodes.MISSING_ACCESS) {
		ModalCommands.push(
			modal(() => (
				<GenericErrorModal
					title={cannotInstallTitle}
					message={cannotInstallMessage}
					data-flx="invite.invite-commands.handle-pack-invite-error.generic-error-modal"
				/>
			)),
		);
		return true;
	}
	if (errorCode === APIErrorCodes.MAX_PACKS) {
		const payload = getPackLimitPayload(responseErr);
		const packType = payload?.packType ?? invite.pack.type;
		const action = payload?.action ?? 'install';
		const limit = payload?.limit;
		const {title, message} = buildPackLimitStrings(i18n, packType, action, limit);
		ModalCommands.push(
			modal(() => (
				<GenericErrorModal
					title={title}
					message={message}
					data-flx="invite.invite-commands.handle-pack-invite-error.generic-error-modal--2"
				/>
			)),
		);
		return true;
	}
	const fallbackMessage = responseErr ? failureMessage(responseErr) : null;
	ModalCommands.push(
		modal(() => (
			<GenericErrorModal
				title={defaultTitle}
				message={fallbackMessage || defaultMessage}
				data-flx="invite.invite-commands.handle-pack-invite-error.generic-error-modal--3"
			/>
		)),
	);
	return true;
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
