// SPDX-License-Identifier: AGPL-3.0-or-later

import {startDomainMigrationTrigger} from '@app/features/app/domain_migration/DomainMigrationTrigger';
import Initialization from '@app/features/app/state/Initialization';
import {syncAccountUserData} from '@app/features/auth/state/AccountUserDataSync';
import Authentication from '@app/features/auth/state/Authentication';
import AuthSession from '@app/features/auth/state/AuthSession';
import ChannelPins from '@app/features/channel/state/ChannelPins';
import Channels from '@app/features/channel/state/Channels';
import UserConnection from '@app/features/connection/state/UserConnection';
import Emoji from '@app/features/emoji/state/Emoji';
import Sticker from '@app/features/emoji/state/EmojiSticker';
import type {FavoriteMemeWire} from '@app/features/expressions/models/FavoriteMeme';
import FavoriteMemes from '@app/features/expressions/state/FavoriteMemes';
import ForumPosts from '@app/features/forum/state/ForumPosts';
import ForumReadState from '@app/features/forum/state/ForumReadState';
import {scheduleAccountReadyWork} from '@app/features/gateway/events/AccountReadyWork';
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
import NavigationSideEffects from '@app/features/navigation/state/NavigationSideEffects';
import MentionFeed from '@app/features/notification/state/MentionFeed';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Presence from '@app/features/presence/state/Presence';
import ReadStates, {type GatewayReadState} from '@app/features/read_state/state/ReadStates';
import type {RelationshipWire} from '@app/features/relationship/models/Relationship';
import Relationships from '@app/features/relationship/state/Relationships';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import ThreadSubscriptions from '@app/features/threads/state/ThreadSubscriptions';
import UserGuildSettings, {type GatewayGuildSettings} from '@app/features/user/state/UserGuildSettings';
import UserNote from '@app/features/user/state/UserNote';
import UserPinnedDM from '@app/features/user/state/UserPinnedDM';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import WebAuthnCredentials, {type WebAuthnCredential} from '@app/features/user/state/WebAuthnCredentials';
import RtcRegions from '@app/features/voice/state/RtcRegions';
import {seedVoiceStatesFromReady} from '@app/features/voice/VoiceGatewayLifecycle';
import type {RtcRegionResponse, Channel as WireChannel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {
	UserPrivate,
	UserSettingsResponse,
	User as WireUser,
} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {runInAction} from 'mobx';
import PollVotes from '@app/features/messaging/state/PollVotes';

const logger = new Logger('READY Handler');

export interface ReadyPayload {
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
	user_settings?: UserSettingsResponse;
	user_guild_settings?: Array<GatewayGuildSettings>;
	read_states?: Array<GatewayReadState>;
	read_state_proto?: string;
	presences?: Array<PresenceRecord>;
	auth_session_id_hash?: string;
	rtc_regions?: Array<RtcRegionResponse>;
	webauthn_credentials?: Array<WebAuthnCredential>;
}

export class GatewayReadyIdentityMismatchError extends Error {
	constructor(expectedUserId: string, receivedUserId: string) {
		super(`READY user mismatch: expected ${expectedUserId}, received ${receivedUserId}`);
		this.name = 'GatewayReadyIdentityMismatchError';
	}
}

export class GatewayReadyAccountContextMissingError extends Error {
	constructor() {
		super('Gateway READY has no account context');
		this.name = 'GatewayReadyAccountContextMissingError';
	}
}

export function handleReady(data: ReadyPayload, context: GatewayHandlerContext): void {
	const accountKey = context.accountKey;
	if (accountKey === null) {
		throw new GatewayReadyAccountContextMissingError();
	}
	const expectedUserId = context.expectedUserId;
	if (expectedUserId != null && data.user.id !== expectedUserId) {
		throw new GatewayReadyIdentityMismatchError(expectedUserId, data.user.id);
	}
	runInAction(() => handleReadyInternal(data, context, accountKey));
}

function handleReadyInternal(data: ReadyPayload, context: GatewayHandlerContext, accountKey: string): void {
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
	for (const channel of data.private_channels ?? []) {
		channels.push({...channel});
	}
	for (const guild of guilds) {
		if (guild.unavailable) continue;
		for (const channel of guild.channels) {
			channels.push({...channel, guild_id: guild.id});
		}
		if (guild.threads) {
			for (const thread of guild.threads) {
				channels.push({...thread, guild_id: guild.id});
			}
		}
	}
	GuildAvailability.loadUnavailableGuilds(guilds);
	UserNote.loadNotes(data.notes ?? {});
	context.setConnectionGeoip({
		country_code: data.country_code,
		latitude: data.latitude,
		longitude: data.longitude,
	});
	UserPinnedDM.setPinnedDMs(data.pinned_dms ?? []);
	Relationships.loadRelationships(data.relationships ?? []);
	FavoriteMemes.loadFavoriteMemes(data.favorite_memes ?? []);
	RtcRegions.setRegions(data.rtc_regions ?? []);
	Users.handleGatewayReady(accountKey, data.user);
	Users.cacheUsers(data.users ?? []);
	syncAccountUserData(accountKey, data.user);
	Authentication.handleGatewayReady({user: data.user});
	Guilds.handleGatewayReady({guilds});
	UserSettings.handleGatewayReady(data.user_settings);
	GuildList.handleGatewayReady(guilds);
	GuildCount.handleGatewayReady(guilds);
	GuildMembers.handleGatewayReady(guilds);
	GuildVerification.handleGatewayReady();
	ThreadGuilds.handleGatewayReady(guilds);
	Channels.handleGatewayReady({channels});
	ChannelThreads.handleGatewayReady(guilds);
	ThreadSubscriptions.handleConnectionReady();
	ForumPosts.handleGatewayReady();
	ForumReadState.handleGatewayReady();
	NavigationSideEffects.handleGatewayReady();
	AuthSession.handleGatewayReady(data.auth_session_id_hash ?? null);
	MessageReactions.handleGatewayReady();
	PollVotes.handleGatewayReady();
	Sticker.handleGatewayReady(guilds);
	Emoji.handleGatewayReady({guilds});
	Permission.handleGatewayReady();
	MemberSearch.handleGatewayReady();
	SavedMessages.handleGatewayReady();
	MentionFeed.handleGatewayReady();
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
	seedVoiceStatesFromReady(guilds);
	if (!context.setReady()) return;
	Initialization.setReady();
	Messages.handleGatewayReady();
	startDomainMigrationTrigger();
	scheduleAccountReadyWork(data.user.id);
}
