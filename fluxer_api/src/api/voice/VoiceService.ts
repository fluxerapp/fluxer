// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {UnclaimedAccountCannotJoinOneOnOneVoiceCallsError} from '@fluxer/errors/src/domains/channel/UnclaimedAccountCannotJoinOneOnOneVoiceCallsError';
import {UnclaimedAccountCannotJoinVoiceChannelsError} from '@fluxer/errors/src/domains/channel/UnclaimedAccountCannotJoinVoiceChannelsError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {UnknownGuildMemberError} from '@fluxer/errors/src/domains/guild/UnknownGuildMemberError';
import {AccountSuspiciousActivityError} from '@fluxer/errors/src/domains/user/AccountSuspiciousActivityError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {ChannelID, GuildID, UserID} from '../BrandedTypes';
import type {IChannelRepository} from '../channel/IChannelRepository';
import type {IGuildRepositoryAggregate} from '../guild/repositories/IGuildRepositoryAggregate';
import type {ListParticipantsResult} from '../infrastructure/ILiveKitService';
import type {LiveKitService} from '../infrastructure/LiveKitService';
import type {PinnedRoomServer, VoiceRoomStore} from '../infrastructure/VoiceRoomStore';
import {Logger} from '../Logger';
import type {IUserRepository} from '../user/IUserRepository';
import {getEffectiveSuspiciousFlags} from '../user/UserHelpers';
import {generateConnectionId} from '../words/Words';
import type {VoiceAccessContext, VoiceAvailabilityService} from './VoiceAvailabilityService';
import type {VoiceRegionAvailability, VoiceServerRecord} from './VoiceModel';
import {
	resolveVoiceRegionPreference,
	selectClosestPseudoRegionServer,
	selectVoiceRegionId,
} from './VoiceRegionSelection';

interface GetVoiceTokenParams {
	guildId?: GuildID;
	channelId: ChannelID;
	userId: UserID;
	connectionId?: string;
	region?: string;
	latitude?: string;
	longitude?: string;
	canSpeak?: boolean;
	canStream?: boolean;
	canVideo?: boolean;
	tokenNonce?: string;
}

interface VoicePermissions {
	canSpeak: boolean;
	canStream: boolean;
	canVideo: boolean;
}

interface UpdateVoiceStateParams {
	guildId?: GuildID;
	channelId: ChannelID;
	userId: UserID;
	connectionId: string;
	mute?: boolean;
	deaf?: boolean;
	canSpeak?: boolean;
	canStream?: boolean;
	canVideo?: boolean;
}

export class VoiceService {
	constructor(
		private liveKitService: LiveKitService,
		private guildRepository: IGuildRepositoryAggregate,
		private userRepository: IUserRepository,
		private channelRepository: IChannelRepository,
		private voiceRoomStore: VoiceRoomStore,
		private voiceAvailabilityService: VoiceAvailabilityService,
	) {}

	async getVoiceToken(params: GetVoiceTokenParams): Promise<{
		token: string;
		endpoint: string;
		connectionId: string;
		tokenNonce: string;
		regionId: string;
		serverId: string;
	}> {
		const {guildId, channelId, userId, connectionId: providedConnectionId} = params;
		const selectionKey = this.createVoiceRoutingSelectionKey(guildId, channelId);
		let connectionId = providedConnectionId;
		let regionId: string | null = null;
		let serverId: string | null = null;
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const effectiveSuspiciousFlags = getEffectiveSuspiciousFlags(user);
		if (effectiveSuspiciousFlags !== 0) {
			throw new AccountSuspiciousActivityError(effectiveSuspiciousFlags);
		}
		const channel = await this.channelRepository.findUnique(channelId);
		if (!channel) {
			throw new UnknownChannelError();
		}
		const isUnclaimed = user.isUnclaimedAccount();
		if (isUnclaimed) {
			if (channel.type === ChannelTypes.DM) {
				throw new UnclaimedAccountCannotJoinOneOnOneVoiceCallsError();
			}
			if (channel.type === ChannelTypes.GUILD_VOICE) {
				const guild = guildId ? await this.guildRepository.findUnique(guildId) : null;
				const isOwner = guild?.ownerId === userId;
				if (!isOwner) {
					throw new UnclaimedAccountCannotJoinVoiceChannelsError();
				}
			}
		}
		let mute = false;
		let deaf = false;
		let guildFeatures: Set<string> | undefined;
		const voicePermissions: VoicePermissions = {
			canSpeak: params.canSpeak ?? true,
			canStream: params.canStream ?? true,
			canVideo: params.canVideo ?? true,
		};
		if (guildId !== undefined) {
			const member = await this.guildRepository.getMember(guildId, userId);
			if (!member) {
				throw new UnknownGuildMemberError();
			}
			mute = member.isMute;
			deaf = member.isDeaf;
			const guild = await this.guildRepository.findUnique(guildId);
			if (guild) {
				guildFeatures = guild.features;
			}
		}
		const context: VoiceAccessContext = {
			requestingUserId: userId,
			guildId,
			guildFeatures,
		};
		const availableRegions = this.voiceAvailabilityService.getAvailableRegions(context);
		const accessibleRegions = availableRegions.filter((region) => region.isAccessible);
		const defaultRegionId = this.liveKitService.getDefaultRegionId();
		const preferredRegionId = params.region ?? channel.rtcRegion ?? null;
		const regionPreference = resolveVoiceRegionPreference({
			preferredRegionId,
			accessibleRegions,
			availableRegions,
			defaultRegionId,
		});
		const accessibleServers = this.getAccessibleServersForRegions(accessibleRegions, context);
		let serverEndpoint: string | null = null;
		const pinnedServer = await this.voiceRoomStore.getPinnedRoomServer(guildId, channelId);
		const resolvedPinnedServer = await this.resolvePinnedServer({
			pinnedServer,
			guildId,
			channelId,
			context,
			preferredRegionId: regionPreference.regionId,
			mode: regionPreference.mode,
		});
		if (resolvedPinnedServer) {
			regionId = resolvedPinnedServer.regionId;
			serverId = resolvedPinnedServer.serverId;
			serverEndpoint = resolvedPinnedServer.endpoint;
		}
		if (!serverId) {
			const pseudoRegionServer = selectClosestPseudoRegionServer({
				mode: regionPreference.mode,
				accessibleServers,
				connectionCounts: this.voiceAvailabilityService.getServerConnectionCounts(),
				latitude: params.latitude,
				longitude: params.longitude,
				selectionKey,
			});
			if (pseudoRegionServer) {
				regionId = pseudoRegionServer.regionId;
				serverId = pseudoRegionServer.serverId;
				serverEndpoint = pseudoRegionServer.endpoint;
			} else {
				regionId = selectVoiceRegionId({
					preferredRegionId: regionPreference.regionId,
					mode: regionPreference.mode,
					accessibleRegions,
					availableRegions,
					latitude: params.latitude,
					longitude: params.longitude,
					selectionKey,
				});
				if (!regionId) {
					throw new FeatureTemporarilyDisabledError();
				}
				const serverSelection = this.selectServerForRegion({
					regionId,
					context,
					accessibleRegions,
				});
				if (!serverSelection) {
					throw new FeatureTemporarilyDisabledError();
				}
				regionId = serverSelection.regionId;
				serverId = serverSelection.server.serverId;
				serverEndpoint = serverSelection.server.endpoint;
			}
			await this.voiceRoomStore.pinRoomServer(guildId, channelId, regionId, serverId, serverEndpoint);
		}
		if (!serverId || !regionId || !serverEndpoint) {
			throw new FeatureTemporarilyDisabledError();
		}
		const serverRecord = this.liveKitService.getServer(regionId, serverId);
		if (!serverRecord) {
			throw new FeatureTemporarilyDisabledError();
		}
		connectionId = providedConnectionId || generateConnectionId();
		Logger.debug(
			{
				guildId: guildId?.toString(),
				channelId: channelId.toString(),
				userId: userId.toString(),
				providedConnectionId,
				generatedConnectionId: connectionId,
				wasGenerated: !providedConnectionId,
			},
			'Voice token connection ID selection',
		);
		const tokenNonce = params.tokenNonce ?? crypto.randomUUID();
		const {token, endpoint} = await this.liveKitService.createToken({
			userId,
			guildId,
			channelId,
			connectionId,
			tokenNonce,
			regionId,
			serverId,
			mute,
			deaf,
			canSpeak: voicePermissions.canSpeak,
			canStream: voicePermissions.canStream,
			canVideo: voicePermissions.canVideo,
		});
		if (mute || deaf) {
			this.liveKitService
				.updateParticipant({
					userId,
					guildId,
					channelId,
					connectionId,
					regionId,
					serverId,
					mute,
					deaf,
				})
				.catch((error) => {
					Logger.error(
						{
							userId,
							guildId,
							channelId,
							connectionId,
							regionId,
							serverId,
							mute,
							deaf,
							error,
						},
						'Failed to update LiveKit participant after token creation',
					);
				});
		}
		return {token, endpoint, connectionId, tokenNonce, regionId, serverId};
	}

	private createVoiceRoutingSelectionKey(guildId: GuildID | undefined, channelId: ChannelID): string {
		const guildKey = guildId?.toString() ?? 'dm';
		return `${guildKey}:${channelId.toString()}`;
	}

	private async resolvePinnedServer({
		pinnedServer,
		guildId,
		channelId,
		context,
		preferredRegionId,
		mode,
	}: {
		pinnedServer: PinnedRoomServer | null;
		guildId?: GuildID;
		channelId: ChannelID;
		context: VoiceAccessContext;
		preferredRegionId: string | null;
		mode: 'explicit' | 'automatic';
	}): Promise<{
		regionId: string;
		serverId: string;
		endpoint: string;
	} | null> {
		if (!pinnedServer) {
			return null;
		}
		if (mode === 'explicit' && preferredRegionId && pinnedServer.regionId !== preferredRegionId) {
			await this.voiceRoomStore.deleteRoomServer(guildId, channelId);
			return null;
		}
		const serverRecord = this.liveKitService.getServer(pinnedServer.regionId, pinnedServer.serverId);
		if (serverRecord && this.voiceAvailabilityService.isServerAccessible(serverRecord, context)) {
			return {
				regionId: pinnedServer.regionId,
				serverId: pinnedServer.serverId,
				endpoint: serverRecord.endpoint,
			};
		}
		await this.voiceRoomStore.deleteRoomServer(guildId, channelId);
		return null;
	}

	private selectServerForRegion({
		regionId,
		context,
		accessibleRegions,
	}: {
		regionId: string;
		context: VoiceAccessContext;
		accessibleRegions: Array<VoiceRegionAvailability>;
	}): {
		regionId: string;
		server: VoiceServerRecord;
	} | null {
		const initialServer = this.voiceAvailabilityService.selectServer(regionId, context);
		if (initialServer) {
			return {regionId, server: initialServer};
		}
		const fallbackRegion = accessibleRegions.find((region) => region.id !== regionId);
		if (fallbackRegion) {
			const fallbackServer = this.voiceAvailabilityService.selectServer(fallbackRegion.id, context);
			if (fallbackServer) {
				return {
					regionId: fallbackRegion.id,
					server: fallbackServer,
				};
			}
		}
		return null;
	}

	private getAccessibleServersForRegions(
		accessibleRegions: Array<VoiceRegionAvailability>,
		context: VoiceAccessContext,
	): Array<VoiceServerRecord> {
		const servers: Array<VoiceServerRecord> = [];
		for (const region of accessibleRegions) {
			servers.push(...this.voiceAvailabilityService.getAccessibleServersForRegion(region.id, context));
		}
		return servers;
	}

	async listParticipantsOnServer(params: {
		guildId?: GuildID;
		channelId: ChannelID;
		regionId: string;
		serverId: string;
	}): Promise<ListParticipantsResult> {
		return await this.liveKitService.listParticipants(params);
	}

	async updateVoiceState(params: UpdateVoiceStateParams): Promise<void> {
		const {guildId, channelId, userId, connectionId, mute, deaf} = params;
		const pinnedServer = await this.voiceRoomStore.getPinnedRoomServer(guildId, channelId);
		if (!pinnedServer) {
			return;
		}
		await this.liveKitService.updateParticipant({
			userId,
			guildId,
			channelId,
			connectionId,
			regionId: pinnedServer.regionId,
			serverId: pinnedServer.serverId,
			mute,
			deaf,
			canSpeak: params.canSpeak,
			canStream: params.canStream,
			canVideo: params.canVideo,
		});
	}

	async updateParticipant(params: {
		guildId?: GuildID;
		channelId: ChannelID;
		userId: UserID;
		mute: boolean;
		deaf: boolean;
		canSpeak?: boolean;
		canStream?: boolean;
		canVideo?: boolean;
	}): Promise<void> {
		const {guildId, channelId, userId, mute, deaf, canSpeak, canStream, canVideo} = params;
		const pinnedServer = await this.voiceRoomStore.getPinnedRoomServer(guildId, channelId);
		if (!pinnedServer) {
			return;
		}
		const result = await this.liveKitService.listParticipants({
			guildId,
			channelId,
			regionId: pinnedServer.regionId,
			serverId: pinnedServer.serverId,
		});
		if (result.status === 'error') {
			Logger.error(
				{errorCode: result.errorCode, guildId, channelId},
				'Failed to list participants for self mute/deaf update',
			);
			return;
		}
		for (const participant of result.participants) {
			const parts = participant.identity.split('_');
			if (parts.length >= 2 && parts[0] === 'user') {
				const participantUserIdStr = parts[1];
				if (participantUserIdStr === userId.toString()) {
					const connectionId = parts.slice(2).join('_');
					try {
						await this.liveKitService.updateParticipant({
							userId,
							guildId,
							channelId,
							connectionId,
							regionId: pinnedServer.regionId,
							serverId: pinnedServer.serverId,
							mute,
							deaf,
							canSpeak,
							canStream,
							canVideo,
						});
					} catch (error) {
						Logger.error(
							{
								identity: participant.identity,
								userId,
								guildId,
								channelId,
								connectionId,
								regionId: pinnedServer.regionId,
								serverId: pinnedServer.serverId,
								mute,
								deaf,
								error,
							},
							'Failed to update participant',
						);
					}
				}
			}
		}
	}

	async disconnectParticipant(params: {
		guildId?: GuildID;
		channelId: ChannelID;
		userId: UserID;
		connectionId: string;
	}): Promise<void> {
		const {guildId, channelId, userId, connectionId} = params;
		const pinnedServer = await this.voiceRoomStore.getPinnedRoomServer(guildId, channelId);
		if (!pinnedServer) {
			return;
		}
		await this.liveKitService.disconnectParticipant({
			userId,
			guildId,
			channelId,
			connectionId,
			regionId: pinnedServer.regionId,
			serverId: pinnedServer.serverId,
		});
	}

	async updateParticipantPermissions(params: {
		guildId?: GuildID;
		channelId: ChannelID;
		userId: UserID;
		connectionId: string;
		canSpeak: boolean;
		canStream: boolean;
		canVideo: boolean;
		deaf?: boolean;
	}): Promise<void> {
		const {guildId, channelId, userId, connectionId, canSpeak, canStream, canVideo, deaf} = params;
		const pinnedServer = await this.voiceRoomStore.getPinnedRoomServer(guildId, channelId);
		if (!pinnedServer) {
			return;
		}
		await this.liveKitService.updateParticipantPermissions({
			userId,
			guildId,
			channelId,
			connectionId,
			regionId: pinnedServer.regionId,
			serverId: pinnedServer.serverId,
			canSpeak,
			canStream,
			canVideo,
			deaf,
		});
	}

	async disconnectChannel(params: {guildId?: GuildID; channelId: ChannelID}): Promise<{
		success: boolean;
		disconnectedCount: number;
	}> {
		const {guildId, channelId} = params;
		const pinnedServer = await this.voiceRoomStore.getPinnedRoomServer(guildId, channelId);
		if (!pinnedServer) {
			return {
				success: false,
				disconnectedCount: 0,
			};
		}
		try {
			const result = await this.liveKitService.listParticipants({
				guildId,
				channelId,
				regionId: pinnedServer.regionId,
				serverId: pinnedServer.serverId,
			});
			if (result.status === 'error') {
				return {
					success: false,
					disconnectedCount: 0,
				};
			}
			let disconnectedCount = 0;
			for (const participant of result.participants) {
				try {
					const identityMatch = participant.identity.match(/^user_(\d+)_(.+)$/);
					if (identityMatch) {
						const [, userIdStr, connectionId] = identityMatch;
						const userId = BigInt(userIdStr) as UserID;
						await this.liveKitService.disconnectParticipant({
							userId,
							guildId,
							channelId,
							connectionId,
							regionId: pinnedServer.regionId,
							serverId: pinnedServer.serverId,
						});
						disconnectedCount++;
					}
				} catch (error) {
					Logger.error(
						{
							identity: participant.identity,
							guildId,
							channelId,
							regionId: pinnedServer.regionId,
							serverId: pinnedServer.serverId,
							error,
						},
						'Failed to disconnect participant',
					);
				}
			}
			return {
				success: true,
				disconnectedCount,
			};
		} catch (error) {
			Logger.error({guildId, channelId, error}, 'Error disconnecting channel participants');
			return {
				success: false,
				disconnectedCount: 0,
			};
		}
	}
}
