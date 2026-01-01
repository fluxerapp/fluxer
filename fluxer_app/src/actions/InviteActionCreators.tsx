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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans} from '@lingui/react/macro';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {APIErrorCodes} from '~/Constants';
import {FeatureTemporarilyDisabledModal} from '~/components/alerts/FeatureTemporarilyDisabledModal';
import {GenericErrorModal} from '~/components/alerts/GenericErrorModal';
import {GuildAtCapacityModal} from '~/components/alerts/GuildAtCapacityModal';
import {InviteAcceptFailedModal} from '~/components/alerts/InviteAcceptFailedModal';
import {InvitesDisabledModal} from '~/components/alerts/InvitesDisabledModal';
import {MaxGuildsModal} from '~/components/alerts/MaxGuildsModal';
import {TemporaryInviteRequiresPresenceModal} from '~/components/alerts/TemporaryInviteRequiresPresenceModal';
import {UserBannedFromGuildModal} from '~/components/alerts/UserBannedFromGuildModal';
import {UserIpBannedFromGuildModal} from '~/components/alerts/UserIpBannedFromGuildModal';
import {InviteAcceptModal} from '~/components/modals/InviteAcceptModal';
import {Endpoints} from '~/Endpoints';
import http, {HttpError} from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import {Routes} from '~/Routes';
import type {Invite} from '~/records/MessageRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import InviteStore from '~/stores/InviteStore';
import {isGroupDmInvite, isGuildInvite, isPackInvite} from '~/types/InviteTypes';
import * as RouterUtils from '~/utils/RouterUtils';

const logger = new Logger('Invites');
const extractErrorCode = (error: unknown): string | undefined => {
	if (error instanceof HttpError) {
		const body = error.body;
		if (body && typeof body === 'object' && 'code' in body) {
			const {code} = body as {code?: unknown};
			return typeof code === 'string' ? code : undefined;
		}
	}
	return undefined;
};

export const fetch = async (code: string): Promise<Invite> => {
	try {
		logger.debug(`Fetching invite with code ${code}`);
		const response = await http.get<Invite>(Endpoints.INVITE(code));
		return response.body;
	} catch (error) {
		logger.error(`Failed to fetch invite with code ${code}:`, error);
		throw error;
	}
};

export const fetchWithCoalescing = async (code: string): Promise<Invite> => {
	return InviteStore.fetchInvite(code);
};

const accept = async (code: string): Promise<Invite> => {
	try {
		logger.debug(`Accepting invite with code ${code}`);
		const response = await http.post<Invite>(Endpoints.INVITE(code), {} as Invite);
		return response.body;
	} catch (error) {
		logger.error(`Failed to accept invite with code ${code}:`, error);
		throw error;
	}
};

export const acceptInvite = accept;

export const acceptAndTransitionToChannel = async (code: string, i18n: I18n): Promise<void> => {
	let invite: Invite | null = null;
	try {
		logger.debug(`Fetching invite details before accepting: ${code}`);
		invite = await fetchWithCoalescing(code);
		if (!invite) {
			throw new Error(`Invite ${code} returned no data`);
		}

		if (isPackInvite(invite)) {
			await accept(code);
			const packLabel = invite.pack.type === 'emoji' ? 'emoji pack' : 'sticker pack';
			ToastActionCreators.createToast({
				type: 'success',
				children: (
					<Trans>
						The {packLabel} {invite.pack.name} has been installed.
					</Trans>
				),
			});
			return;
		}

		if (isGroupDmInvite(invite)) {
			const channelId = invite.channel.id;
			logger.debug(`Accepting group DM invite ${code} and opening channel ${channelId}`);
			await accept(code);
			RouterUtils.transitionTo(Routes.dmChannel(channelId));
			return;
		}

		if (!isGuildInvite(invite)) {
			throw new Error(`Invite ${code} is not a guild, group DM, or pack invite`);
		}

		const channelId = invite.channel.id;
		const currentUserId = AuthenticationStore.currentUserId;
		const guildId = invite.guild.id;
		const isMember = currentUserId ? GuildMemberStore.getMember(guildId, currentUserId) != null : false;
		if (isMember) {
			logger.debug(`User already in guild ${guildId}, transitioning to channel ${channelId}`);
			RouterUtils.transitionTo(Routes.guildChannel(guildId, channelId));
			return;
		}
		logger.debug(`User not in guild ${guildId}, accepting invite ${code}`);
		await accept(code);
		logger.debug(`Transitioning to channel ${channelId} in guild ${guildId}`);
		RouterUtils.transitionTo(Routes.guildChannel(guildId, channelId));
	} catch (error) {
		const httpError = error instanceof HttpError ? error : null;
		const errorCode = extractErrorCode(error);
		logger.error(`Failed to accept invite and transition for code ${code}:`, error);

		if (httpError?.status === 404 || errorCode === APIErrorCodes.UNKNOWN_INVITE) {
			logger.debug(`Invite ${code} not found, removing from store`);
			InviteStore.handleInviteDelete(code);
		}

		if (handlePackInviteError({invite, errorCode, httpError, i18n})) {
			throw error;
		}

		if (errorCode === APIErrorCodes.INVITES_DISABLED) {
			ModalActionCreators.push(modal(() => <InvitesDisabledModal />));
		} else if (httpError?.status === 403 && errorCode === APIErrorCodes.FEATURE_TEMPORARILY_DISABLED) {
			ModalActionCreators.push(modal(() => <FeatureTemporarilyDisabledModal />));
		} else if (errorCode === APIErrorCodes.MAX_GUILD_MEMBERS) {
			ModalActionCreators.push(modal(() => <GuildAtCapacityModal />));
		} else if (errorCode === APIErrorCodes.MAX_GUILDS) {
			ModalActionCreators.push(modal(() => <MaxGuildsModal />));
		} else if (errorCode === APIErrorCodes.TEMPORARY_INVITE_REQUIRES_PRESENCE) {
			ModalActionCreators.push(modal(() => <TemporaryInviteRequiresPresenceModal />));
		} else if (errorCode === APIErrorCodes.USER_BANNED_FROM_GUILD) {
			ModalActionCreators.push(modal(() => <UserBannedFromGuildModal />));
		} else if (errorCode === APIErrorCodes.USER_IP_BANNED_FROM_GUILD) {
			ModalActionCreators.push(modal(() => <UserIpBannedFromGuildModal />));
		} else if (errorCode === APIErrorCodes.GUILD_DISALLOWS_UNCLAIMED_ACCOUNTS) {
			ModalActionCreators.push(
				modal(() => (
					<GenericErrorModal
						title={i18n._(msg`Cannot Join Community`)}
						message={i18n._(
							msg`This community requires you to verify your account before joining. Please set an email and password for your account.`,
						)}
					/>
				)),
			);
		} else if (errorCode === APIErrorCodes.UNCLAIMED_ACCOUNT_RESTRICTED) {
			ModalActionCreators.push(
				modal(() => (
					<GenericErrorModal
						title={i18n._(msg`Account Verification Required`)}
						message={i18n._(
							msg`Please verify your account by setting an email and password before joining communities.`,
						)}
					/>
				)),
			);
		} else if (httpError?.status && httpError.status >= 400) {
			ModalActionCreators.push(modal(() => <InviteAcceptFailedModal />));
		}

		throw error;
	}
};

export const openAcceptModal = async (code: string): Promise<void> => {
	void fetchWithCoalescing(code).catch(() => {});
	ModalActionCreators.pushWithKey(
		modal(() => <InviteAcceptModal code={code} />),
		`invite-accept-${code}`,
	);
};

interface HandlePackInviteErrorParams {
	invite: Invite | null;
	errorCode?: string;
	httpError?: HttpError | null;
	i18n: I18n;
}

interface PackLimitPayload {
	packType?: 'emoji' | 'sticker';
	limit?: number;
	action?: 'create' | 'install';
}

const getPackLimitPayload = (httpError?: HttpError | null): PackLimitPayload | null => {
	const body = httpError?.body;
	if (!body || typeof body !== 'object') return null;
	const data = (body as {data?: unknown}).data;
	if (!data || typeof data !== 'object') return null;
	const limit = (data as {limit?: unknown}).limit;
	const packType = (data as {pack_type?: unknown}).pack_type;
	const action = (data as {action?: unknown}).action;
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
					const title = i18n._(msg`Emoji pack limit reached`);
					const message =
						typeof limit === 'number'
							? i18n._(
									limit === 1
										? msg`You have installed the maximum of ${limit} emoji pack. Remove one to install another.`
										: msg`You have installed the maximum of ${limit} emoji packs. Remove one to install another.`,
								)
							: i18n._(
									msg`You have reached the limit for installing emoji packs. Remove one of your installed packs to install another.`,
								);
					return {title, message};
				}
				default: {
					const title = i18n._(msg`Emoji pack creation limit reached`);
					const message =
						typeof limit === 'number'
							? i18n._(
									limit === 1
										? msg`You have created the maximum of ${limit} emoji pack. Delete one to create another.`
										: msg`You have created the maximum of ${limit} emoji packs. Delete one to create another.`,
								)
							: i18n._(
									msg`You have reached the limit for creating emoji packs. Delete one of your packs to create another.`,
								);
					return {title, message};
				}
			}
		}
		default: {
			switch (action) {
				case 'install': {
					const title = i18n._(msg`Sticker pack limit reached`);
					const message =
						typeof limit === 'number'
							? i18n._(
									limit === 1
										? msg`You have installed the maximum of ${limit} sticker pack. Remove one to install another.`
										: msg`You have installed the maximum of ${limit} sticker packs. Remove one to install another.`,
								)
							: i18n._(
									msg`You have reached the limit for installing sticker packs. Remove one of your installed packs to install another.`,
								);
					return {title, message};
				}
				default: {
					const title = i18n._(msg`Sticker pack creation limit reached`);
					const message =
						typeof limit === 'number'
							? i18n._(
									limit === 1
										? msg`You have created the maximum of ${limit} sticker pack. Delete one to create another.`
										: msg`You have created the maximum of ${limit} sticker packs. Delete one to create another.`,
								)
							: i18n._(
									msg`You have reached the limit for creating sticker packs. Delete one of your packs to create another.`,
								);
					return {title, message};
				}
			}
		}
	}
};

export const handlePackInviteError = (params: HandlePackInviteErrorParams): boolean => {
	const {invite, errorCode, httpError, i18n} = params;
	if (!invite || !isPackInvite(invite)) {
		return false;
	}

	const isEmojiPack = invite.pack.type === 'emoji';
	const cannotInstallTitle = isEmojiPack
		? i18n._(msg`Cannot install emoji pack`)
		: i18n._(msg`Cannot install sticker pack`);
	const cannotInstallMessage = isEmojiPack
		? i18n._(msg`You don't have permission to install this emoji pack.`)
		: i18n._(msg`You don't have permission to install this sticker pack.`);
	const defaultTitle = isEmojiPack
		? i18n._(msg`Unable to install emoji pack`)
		: i18n._(msg`Unable to install sticker pack`);
	const defaultMessage = isEmojiPack
		? i18n._(msg`Failed to install this emoji pack. Please try again later.`)
		: i18n._(msg`Failed to install this sticker pack. Please try again later.`);

	if (errorCode === APIErrorCodes.PREMIUM_REQUIRED) {
		ModalActionCreators.push(
			modal(() => (
				<GenericErrorModal
					title={i18n._(msg`Premium required`)}
					message={i18n._(msg`Installing emoji and sticker packs requires a premium subscription.`)}
				/>
			)),
		);
		return true;
	}

	if (errorCode === APIErrorCodes.MISSING_ACCESS) {
		ModalActionCreators.push(
			modal(() => <GenericErrorModal title={cannotInstallTitle} message={cannotInstallMessage} />),
		);
		return true;
	}

	if (errorCode === APIErrorCodes.MAX_PACKS) {
		const payload = getPackLimitPayload(httpError);
		const packType = payload?.packType ?? invite.pack.type;
		const action = payload?.action ?? 'install';
		const limit = payload?.limit;
		const {title, message} = buildPackLimitStrings(i18n, packType, action, limit);

		ModalActionCreators.push(modal(() => <GenericErrorModal title={title} message={message} />));
		return true;
	}

	const fallbackMessage =
		httpError?.body && typeof httpError.body === 'object' && 'message' in httpError.body
			? (httpError.body as {message?: unknown}).message?.toString()
			: null;

	ModalActionCreators.push(
		modal(() => <GenericErrorModal title={defaultTitle} message={fallbackMessage || defaultMessage} />),
	);
	return true;
};

export const create = async (
	channelId: string,
	params?: {max_age?: number; max_uses?: number; temporary?: boolean},
): Promise<Invite> => {
	try {
		logger.debug(`Creating invite for channel ${channelId}`);
		const response = await http.post<Invite>(Endpoints.CHANNEL_INVITES(channelId), params ?? {});
		return response.body;
	} catch (error) {
		logger.error(`Failed to create invite for channel ${channelId}:`, error);
		throw error;
	}
};

export const list = async (channelId: string): Promise<Array<Invite>> => {
	try {
		logger.debug(`Listing invites for channel ${channelId}`);
		const response = await http.get<Array<Invite>>(Endpoints.CHANNEL_INVITES(channelId));
		return response.body;
	} catch (error) {
		logger.error(`Failed to list invites for channel ${channelId}:`, error);
		throw error;
	}
};

export const remove = async (code: string): Promise<void> => {
	try {
		logger.debug(`Deleting invite with code ${code}`);
		await http.delete({url: Endpoints.INVITE(code)});
	} catch (error) {
		logger.error(`Failed to delete invite with code ${code}:`, error);
		throw error;
	}
};
