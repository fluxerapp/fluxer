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

import type {ChannelID, GuildID, UserID} from '~/BrandedTypes';
import {ChannelTypes} from '~/Constants';
import type {IChannelRepository} from '~/channel/IChannelRepository';
import {
	FeatureTemporarilyDisabledError,
	UnclaimedAccountRestrictedError,
	UnknownChannelError,
	UnknownGuildMemberError,
	UnknownUserError,
} from '~/Errors';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {LiveKitService} from '~/infrastructure/LiveKitService';
import type {VoiceRoomStore} from '~/infrastructure/VoiceRoomStore';
import type {IUserRepository} from '~/user/IUserRepository';
import {calculateDistance} from '~/utils/GeoUtils';
import type {VoiceAccessContext, VoiceAvailabilityService} from '~/voice/VoiceAvailabilityService';
import type {VoiceRegionAvailability, VoiceServerRecord} from '~/voice/VoiceModel';
import {generateConnectionId} from '~/words/words';

interface GetVoiceTokenParams {
	guildId?: GuildID;
	channelId: ChannelID;
	userId: UserID;
	connectionId?: string;
	latitude?: string;
	longitude?: string;
	canSpeak?: boolean;
	canStream?: boolean;
	canVideo?: boolean;
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
}

export class VoiceService {
	constructor(
		private liveKitService: LiveKitService,
		private guildRepository: IGuildRepository,
		private userRepository: IUserRepository,
		private channelRepository: IChannelRepository,
		private voiceRoomStore: VoiceRoomStore,
		private voiceAvailabilityService: VoiceAvailabilityService,
	) {}

	private findClosestRegion(
		latitude: string,
		longitude: string,
		accessibleRegions: Array<{id: string; latitude: number; longitude: number}>,
	): string | null {
		const userLat = parseFloat(latitude);
		const userLon = parseFloat(longitude);

		if (Number.isNaN(userLat) || Number.isNaN(userLon)) {
			return null;
		}

		let closestRegion: string | null = null;
		let minDistance = Number.POSITIVE_INFINITY;

		for (const region of accessibleRegions) {
			const distance = calculateDistance(userLat, userLon, region.latitude, region.longitude);
			if (distance < minDistance) {
				minDistance = distance;
				closestRegion = region.id;
			}
		}

		return closestRegion;
	}

	async getVoiceToken(params: GetVoiceTokenParams): Promise<{
		token: string;
		endpoint: string;
		connectionId: string;
	}> {
		const {guildId, channelId, userId, connectionId: providedConnectionId} = params;

		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const channel = await this.channelRepository.findUnique(channelId);
		if (!channel) {
			throw new UnknownChannelError();
		}

		const isUnclaimed = user.isUnclaimedAccount();
		if (isUnclaimed) {
			if (channel.type === ChannelTypes.DM) {
				throw new UnclaimedAccountRestrictedError('join 1:1 voice calls');
			}

			if (channel.type === ChannelTypes.GUILD_VOICE) {
				const guild = guildId ? await this.guildRepository.findUnique(guildId) : null;
				const isOwner = guild?.ownerId === userId;
				if (!isOwner) {
					throw new UnclaimedAccountRestrictedError('join voice channels you do not own');
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
		const regionPreference = this.determineRegionPreference({
			channelRtcRegion: channel.rtcRegion,
			accessibleRegions,
			availableRegions,
			defaultRegionId,
		});

		let regionId: string | null = null;
		let serverId: string | null = null;
		let serverEndpoint: string | null = null;

		const pinnedServer = await this.voiceRoomStore.getPinnedRoomServer(guildId, channelId);
		const resolvedPinnedServer = await this.resolvePinnedServer({
			pinnedServer,
			guildId,
			channelId,
			context,
		});

		if (resolvedPinnedServer) {
			regionId = resolvedPinnedServer.regionId;
			serverId = resolvedPinnedServer.serverId;
			serverEndpoint = resolvedPinnedServer.endpoint;
		}

		if (!serverId) {
			const coordinates =
				regionPreference.mode === 'automatic' && params.latitude && params.longitude
					? {latitude: params.latitude, longitude: params.longitude}
					: null;
			regionId = this.chooseRegionId({
				preferredRegionId: regionPreference.regionId,
				accessibleRegions,
				availableRegions,
				coordinates,
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

			await this.voiceRoomStore.pinRoomServer(guildId, channelId, regionId, serverId, serverEndpoint);
		}

		if (!serverId || !regionId || !serverEndpoint) {
			throw new FeatureTemporarilyDisabledError();
		}

		const serverRecord = this.liveKitService.getServer(regionId, serverId);
		if (!serverRecord) {
			throw new FeatureTemporarilyDisabledError();
		}

		const connectionId = providedConnectionId || generateConnectionId();
		const {token, endpoint} = await this.liveKitService.createToken({
			userId,
			guildId,
			channelId,
			connectionId,
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
					console.error('Failed to update LiveKit participant after token creation:', error);
				});
		}

		return {token, endpoint, connectionId};
	}

	private determineRegionPreference({
		channelRtcRegion,
		accessibleRegions,
		availableRegions,
		defaultRegionId,
	}: {
		channelRtcRegion: string | null;
		accessibleRegions: Array<VoiceRegionAvailability>;
		availableRegions: Array<VoiceRegionAvailability>;
		defaultRegionId: string | null;
	}): {regionId: string | null; mode: 'explicit' | 'automatic'} {
		const accessibleRegionIds = new Set(accessibleRegions.map((region) => region.id));

		if (channelRtcRegion) {
			if (accessibleRegionIds.has(channelRtcRegion)) {
				return {regionId: channelRtcRegion, mode: 'explicit'};
			}
			return {regionId: null, mode: 'automatic'};
		}

		if (defaultRegionId && accessibleRegionIds.has(defaultRegionId)) {
			return {regionId: defaultRegionId, mode: 'automatic'};
		}

		const fallbackRegion = accessibleRegions[0] ?? availableRegions[0] ?? null;
		return {regionId: fallbackRegion ? fallbackRegion.id : null, mode: 'automatic'};
	}

	private chooseRegionId({
		preferredRegionId,
		accessibleRegions,
		availableRegions,
		coordinates,
	}: {
		preferredRegionId: string | null;
		accessibleRegions: Array<VoiceRegionAvailability>;
		availableRegions: Array<VoiceRegionAvailability>;
		coordinates: {latitude: string; longitude: string} | null;
	}): string | null {
		if (coordinates && accessibleRegions.length > 0) {
			const closestRegionId = this.findClosestRegion(coordinates.latitude, coordinates.longitude, accessibleRegions);
			if (closestRegionId) {
				return closestRegionId;
			}
		}

		if (preferredRegionId) {
			return preferredRegionId;
		}

		const accessibleFallback = accessibleRegions[0];
		if (accessibleFallback) {
			return accessibleFallback.id;
		}

		return availableRegions[0]?.id ?? null;
	}

	private async resolvePinnedServer({
		pinnedServer,
		guildId,
		channelId,
		context,
	}: {
		pinnedServer: Awaited<ReturnType<VoiceRoomStore['getPinnedRoomServer']>>;
		guildId?: GuildID;
		channelId: ChannelID;
		context: VoiceAccessContext;
	}): Promise<{regionId: string; serverId: string; endpoint: string} | null> {
		if (!pinnedServer) {
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
		});
	}

	async updateParticipant(params: {
		guildId?: GuildID;
		channelId: ChannelID;
		userId: UserID;
		mute: boolean;
		deaf: boolean;
	}): Promise<void> {
		const {guildId, channelId, userId, mute, deaf} = params;

		const pinnedServer = await this.voiceRoomStore.getPinnedRoomServer(guildId, channelId);
		if (!pinnedServer) {
			return;
		}

		const participants = await this.liveKitService.listParticipants({
			guildId,
			channelId,
			regionId: pinnedServer.regionId,
			serverId: pinnedServer.serverId,
		});

		for (const participant of participants) {
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
						});
					} catch (error) {
						console.error(`Failed to update participant ${participant.identity}:`, error);
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
	}): Promise<void> {
		const {guildId, channelId, userId, connectionId, canSpeak, canStream, canVideo} = params;

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
		});
	}

	async disconnectChannel(params: {
		guildId?: GuildID;
		channelId: ChannelID;
	}): Promise<{success: boolean; disconnectedCount: number; message?: string}> {
		const {guildId, channelId} = params;

		const pinnedServer = await this.voiceRoomStore.getPinnedRoomServer(guildId, channelId);
		if (!pinnedServer) {
			return {
				success: false,
				disconnectedCount: 0,
				message: 'No active voice session found for this channel',
			};
		}

		try {
			const participants = await this.liveKitService.listParticipants({
				guildId,
				channelId,
				regionId: pinnedServer.regionId,
				serverId: pinnedServer.serverId,
			});

			let disconnectedCount = 0;

			for (const participant of participants) {
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
					console.error(`Failed to disconnect participant ${participant.identity}:`, error);
				}
			}

			return {
				success: true,
				disconnectedCount,
				message: `Successfully disconnected ${disconnectedCount} participant(s)`,
			};
		} catch (error) {
			console.error('Error disconnecting channel participants:', error);
			return {
				success: false,
				disconnectedCount: 0,
				message: 'Failed to retrieve participants from voice room',
			};
		}
	}
}
