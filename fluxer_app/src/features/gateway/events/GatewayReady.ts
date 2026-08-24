// SPDX-License-Identifier: AGPL-3.0-or-later

import Initialization from '@app/features/app/state/Initialization';
import AccountManager from '@app/features/auth/state/AccountManager';
import accountStorage from '@app/features/auth/state/AccountStorage';
import Authentication from '@app/features/auth/state/Authentication';
import AuthSession from '@app/features/auth/state/AuthSession';
import ChannelPins from '@app/features/channel/state/ChannelPins';
import Channels from '@app/features/channel/state/Channels';
import UserConnection from '@app/features/connection/state/UserConnection';
import Emoji from '@app/features/emoji/state/Emoji';
import Sticker from '@app/features/emoji/state/EmojiSticker';
import type {FavoriteMemeWire} from '@app/features/expressions/models/FavoriteMeme';
import FavoriteMemes from '@app/features/expressions/state/FavoriteMemes';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import type {PresenceRecord} from '@app/features/gateway/types/GatewayPresenceTypes';
import ChannelMemberCount from '@app/features/guild/state/ChannelMemberCount';
import GuildAvailability from '@app/features/guild/state/GuildAvailability';
import GuildCount from '@app/features/guild/state/GuildCount';
import GuildList from '@app/features/guild/state/GuildList';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import Guilds from '@app/features/guild/state/Guilds';
import GuildVerification from '@app/features/guild/state/GuildVerification';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberSearch from '@app/features/member/state/MemberSearch';
import MemberSidebar from '@app/features/member/state/MemberSidebar';
import MessageReactions from '@app/features/messaging/state/MessageReactions';
import Messages from '@app/features/messaging/state/MessagingMessages';
import SavedMessages from '@app/features/messaging/state/SavedMessages';
import ScheduledMessages from '@app/features/messaging/state/ScheduledMessages';
import MentionFeed from '@app/features/notification/state/MentionFeed';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import Presence from '@app/features/presence/state/Presence';
import ReadStates, {type GatewayReadState} from '@app/features/read_state/state/ReadStates';
import type {RelationshipWire} from '@app/features/relationship/models/Relationship';
import Relationships from '@app/features/relationship/state/Relationships';
import CountryCode from '@app/features/user/state/CountryCode';
import UserGuildSettings, {type GatewayGuildSettings} from '@app/features/user/state/UserGuildSettings';
import UserNote from '@app/features/user/state/UserNote';
import UserPinnedDM from '@app/features/user/state/UserPinnedDM';
import UserSettings, {type UserSettings as UserSettingsWire} from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import WebAuthnCredentials, {type WebAuthnCredential} from '@app/features/user/state/WebAuthnCredentials';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import RtcRegions from '@app/features/voice/state/RtcRegions';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import type {RtcRegionResponse, Channel as WireChannel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {UserPrivate, User as WireUser} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {runInAction} from 'mobx';

const logger = new Logger('READY Handler');

interface ReadyPayload {
	session_id: string;
	guilds: Array<GuildReadyData>;
	user: UserPrivate;
	private_channels?: Array<WireChannel>;
	notes?: Record<string, string>;
	country_code?: string;
	latitude?: string;
	longitude?: string;
	pinned_dms?: Array<string>;
	relationships?: Array<RelationshipWire>;
	favorite_memes?: Array<FavoriteMemeWire>;
	users?: Array<WireUser>;
	user_settings?: UserSettingsWire;
	user_guild_settings?: Array<GatewayGuildSettings>;
	read_states?: Array<GatewayReadState>;
	read_state_proto?: string;
	presences?: Array<PresenceRecord>;
	auth_session_id_hash?: string;
	rtc_regions?: Array<RtcRegionResponse>;
	webauthn_credentials?: Array<WebAuthnCredential>;
}

export function handleReady(data: ReadyPayload, context: GatewayHandlerContext): void {
	runInAction(() => handleReadyInternal(data, context));
}

function handleReadyInternal(data: ReadyPayload, context: GatewayHandlerContext): void {
	const currentSessionId = data.session_id;
	const isNewSession = context.previousSessionId !== null && context.previousSessionId !== currentSessionId;
	if (isNewSession) {
		logger.info(
			`New session detected (previous: ${context.previousSessionId}, current: ${currentSessionId}), clearing message state`,
		);
		Messages.handleSessionInvalidated();
		MemberSidebar.handleSessionInvalidated();
		GuildCount.handleSessionInvalidated();
		ChannelMemberCount.handleSessionInvalidated();
	}
	context.setPreviousSessionId(currentSessionId);
	const guilds = data.guilds;
	const channels: Array<WireChannel> = [];
	if (data.private_channels) {
		for (const channel of data.private_channels) {
			channels.push({...channel});
		}
	}
	for (const guild of guilds) {
		if (guild.unavailable) continue;
		for (const channel of guild.channels) {
			channels.push({...channel, guild_id: guild.id});
		}
	}
	GuildAvailability.loadUnavailableGuilds(guilds);
	if (data.notes) {
		UserNote.loadNotes(data.notes);
	}
	if (data.country_code) {
		CountryCode.setCountryCode(data.country_code);
	}
	context.setConnectionGeoip({
		country_code: data.country_code,
		latitude: data.latitude,
		longitude: data.longitude,
	});
	if (data.pinned_dms) {
		UserPinnedDM.setPinnedDMs(data.pinned_dms);
	}
	if (data.relationships) {
		Relationships.loadRelationships(data.relationships);
	}
	if (data.favorite_memes) {
		FavoriteMemes.loadFavoriteMemes(data.favorite_memes);
	}
	if (data.rtc_regions) {
		RtcRegions.setRegions(data.rtc_regions);
	}
	Users.handleGatewayReady(data.user);
	if (data.users && data.users.length > 0) {
		Users.cacheUsers(data.users);
	}
	const user = data.user;
	if (user.id) {
		const userData = {
			username: user.username,
			discriminator: user.discriminator,
			globalName: user.global_name,
			email: user.email ?? undefined,
			avatar: user.avatar ?? undefined,
		};
		void accountStorage.updateAccountUserData(user.id, userData);
		void AccountManager.updateAccountUserData(user.id, userData);
	}
	VoiceSettings.handleGatewayReady(data.user);
	Authentication.handleGatewayReady({user: data.user});
	void PremiumCommands.refreshPremiumState().catch((error) => {
		logger.warn('Failed to refresh premium state after READY', error);
	});
	Guilds.handleGatewayReady({guilds});
	UserSettings.handleGatewayReady(data.user_settings);
	GuildList.handleGatewayReady(guilds);
	GuildCount.handleGatewayReady(guilds);
	GuildMembers.handleGatewayReady(guilds);
	GuildVerification.handleGatewayReady();
	Channels.handleGatewayReady({channels});
	if (data.auth_session_id_hash) {
		AuthSession.handleGatewayReady(data.auth_session_id_hash);
	} else {
		logger.warn('READY missing auth_session_id_hash; continuing without AuthSession init');
	}
	MessageReactions.handleGatewayReady();
	Sticker.handleGatewayReady(guilds);
	Emoji.handleGatewayReady({guilds});
	Permission.handleGatewayReady();
	MemberSearch.handleGatewayReady();
	SavedMessages.handleGatewayReady();
	MentionFeed.handleGatewayReady();
	ScheduledMessages.handleGatewayReady();
	ChannelPins.handleGatewayReady();
	UserConnection.handleGatewayReady();
	UserGuildSettings.handleGatewayReady(data.user_guild_settings ?? []);
	WebAuthnCredentials.handleGatewayReady(data.webauthn_credentials);
	ReadStates.handleGatewayReady({
		readState: data.read_states ?? [],
		readStateProto: data.read_state_proto,
		channels,
	});
	GuildReadState.handleGatewayReady();
	Presence.handleGatewayReady(data.user, guilds, data.presences);
	MediaEngine.handleGatewayReady(guilds);
	Initialization.setReady();
	context.setReady();
	Messages.handleGatewayReady();
}
