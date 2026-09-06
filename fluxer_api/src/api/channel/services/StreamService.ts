// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {InvalidStreamKeyFormatError} from '@fluxer/errors/src/domains/channel/InvalidStreamKeyFormatError';
import {InvalidStreamThumbnailPayloadError} from '@fluxer/errors/src/domains/channel/InvalidStreamThumbnailPayloadError';
import {StreamKeyChannelMismatchError} from '@fluxer/errors/src/domains/channel/StreamKeyChannelMismatchError';
import {AccessDeniedError} from '@fluxer/errors/src/domains/core/AccessDeniedError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {StreamKeyScopeMismatchError} from '@fluxer/errors/src/domains/oauth/StreamKeyScopeMismatchError';
import type {StreamPreviewUploadUrlResponseSchema} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import {isValidBase64} from '@fluxer/schema/src/primitives/FileValidators';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {seconds} from 'itty-time';
import {type ChannelID, createChannelID, createGuildID, type GuildID, type UserID} from '../../BrandedTypes';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {ChannelService} from './ChannelService';
import type {StreamPreviewService} from './StreamPreviewService';

type ParsedStreamKey = {
	scope: 'guild' | 'dm';
	guildId?: string;
	channelId: string;
	connectionId: string;
};

export class StreamService {
	constructor(
		private readonly cacheService: ICacheService,
		private readonly channelService: ChannelService,
		private readonly gatewayService: IGatewayService,
		private readonly streamPreviewService: StreamPreviewService,
	) {}

	private parseStreamKey(streamKey: string): ParsedStreamKey | null {
		const parts = streamKey.split(':');
		if (parts.length !== 3) return null;
		const [scopeRaw, channelId, connectionId] = parts;
		if (!channelId || !connectionId) return null;
		if (!/^[0-9]+$/.test(channelId)) return null;
		if (scopeRaw === 'dm') {
			return {scope: 'dm', channelId, connectionId};
		}
		if (!/^[0-9]+$/.test(scopeRaw)) return null;
		return {scope: 'guild', guildId: scopeRaw, channelId, connectionId};
	}

	private getParsedStreamKeyOrThrow(streamKey: string): ParsedStreamKey {
		const parsedKey = this.parseStreamKey(streamKey);
		if (!parsedKey) {
			throw new InvalidStreamKeyFormatError();
		}
		return parsedKey;
	}

	private getChannelIdFromParsedKeyOrThrow(parsedKey: ParsedStreamKey): ChannelID {
		try {
			return createChannelID(BigInt(parsedKey.channelId));
		} catch {
			throw new InvalidStreamKeyFormatError();
		}
	}

	private getGuildIdFromParsedKeyOrThrow(parsedKey: ParsedStreamKey): GuildID | undefined {
		if (parsedKey.scope !== 'guild') {
			return undefined;
		}
		if (!parsedKey.guildId) {
			throw new InvalidStreamKeyFormatError();
		}
		try {
			return createGuildID(BigInt(parsedKey.guildId));
		} catch {
			throw new InvalidStreamKeyFormatError();
		}
	}

	private async assertStreamChannelAccess(params: {
		userId: UserID;
		channelId: ChannelID;
		parsedKey: ParsedStreamKey;
	}): Promise<void> {
		const channel = await this.channelService.channelData.operations.getChannel({
			userId: params.userId,
			channelId: params.channelId,
		});
		if (channel.guildId) {
			if (params.parsedKey.scope !== 'guild') {
				throw new StreamKeyScopeMismatchError();
			}
			if (params.parsedKey.guildId !== channel.guildId.toString()) {
				throw new StreamKeyScopeMismatchError();
			}
			const hasConnect = await this.gatewayService.checkPermission({
				guildId: channel.guildId,
				channelId: params.channelId,
				userId: params.userId,
				permission: Permissions.CONNECT,
			});
			if (!hasConnect) {
				throw new MissingPermissionsError();
			}
		} else if (params.parsedKey.scope !== 'dm') {
			throw new StreamKeyScopeMismatchError();
		}
		if (params.parsedKey.channelId !== params.channelId.toString()) {
			throw new StreamKeyChannelMismatchError();
		}
	}

	private async assertStreamMutationAccess(params: {
		userId: UserID;
		channelId: ChannelID;
		parsedKey: ParsedStreamKey;
	}): Promise<void> {
		await this.assertStreamChannelAccess(params);
		const guildId = this.getGuildIdFromParsedKeyOrThrow(params.parsedKey);
		if (guildId !== undefined) {
			const hasStream = await this.gatewayService.checkPermission({
				guildId,
				channelId: params.channelId,
				userId: params.userId,
				permission: Permissions.STREAM,
			});
			if (!hasStream) {
				throw new MissingPermissionsError();
			}
		}
		const {voiceStates} = await this.gatewayService.getVoiceStatesForChannel({
			guildId,
			channelId: params.channelId,
		});
		const ownsStream = voiceStates.some(
			(voiceState) =>
				voiceState.userId === params.userId.toString() &&
				voiceState.channelId === params.channelId.toString() &&
				voiceState.connectionId === params.parsedKey.connectionId,
		);
		if (!ownsStream) {
			throw new AccessDeniedError();
		}
	}

	async updateStreamRegion(params: {userId: UserID; streamKey: string; region?: string}): Promise<void> {
		const parsedKey = this.getParsedStreamKeyOrThrow(params.streamKey);
		const channelId = this.getChannelIdFromParsedKeyOrThrow(parsedKey);
		await this.assertStreamMutationAccess({
			userId: params.userId,
			channelId,
			parsedKey,
		});
		await this.cacheService.set(
			`stream_region:${params.streamKey}`,
			{region: params.region, updatedAt: Date.now()},
			seconds('1 day'),
		);
	}

	async getPreview(params: {userId: UserID; streamKey: string}): Promise<{
		buffer: Uint8Array;
		contentType: string;
	} | null> {
		const parsedKey = this.getParsedStreamKeyOrThrow(params.streamKey);
		const channelId = this.getChannelIdFromParsedKeyOrThrow(parsedKey);
		await this.assertStreamChannelAccess({
			userId: params.userId,
			channelId,
			parsedKey,
		});
		const preview = await this.streamPreviewService.getPreview(params.streamKey);
		if (preview) {
		}
		return preview;
	}

	async uploadPreview(params: {
		userId: UserID;
		streamKey: string;
		channelId: ChannelID;
		thumbnail: string;
		contentType?: string;
	}): Promise<void> {
		const parsedKey = this.getParsedStreamKeyOrThrow(params.streamKey);
		await this.assertStreamMutationAccess({
			userId: params.userId,
			channelId: params.channelId,
			parsedKey,
		});
		if (!isValidBase64(params.thumbnail)) {
			throw new InvalidStreamThumbnailPayloadError();
		}
		const body = Uint8Array.from(Buffer.from(params.thumbnail, 'base64'));
		await this.streamPreviewService.uploadPreview({
			streamKey: params.streamKey,
			channelId: params.channelId,
			userId: params.userId,
			body,
			contentType: params.contentType,
		});
	}

	async createPreviewUploadUrl(params: {
		userId: UserID;
		streamKey: string;
		channelId: ChannelID;
		contentType?: string;
		clientIp?: string | null;
	}): Promise<StreamPreviewUploadUrlResponseSchema> {
		const parsedKey = this.getParsedStreamKeyOrThrow(params.streamKey);
		await this.assertStreamMutationAccess({
			userId: params.userId,
			channelId: params.channelId,
			parsedKey,
		});
		return this.streamPreviewService.createUploadUrl({
			streamKey: params.streamKey,
			channelId: params.channelId,
			userId: params.userId,
			contentType: params.contentType,
			clientIp: params.clientIp,
		});
	}

	async deletePreview(params: {userId: UserID; streamKey: string}): Promise<void> {
		const parsedKey = this.getParsedStreamKeyOrThrow(params.streamKey);
		const channelId = this.getChannelIdFromParsedKeyOrThrow(parsedKey);
		await this.assertStreamMutationAccess({
			userId: params.userId,
			channelId,
			parsedKey,
		});
		await this.streamPreviewService.deletePreview(params.streamKey);
	}
}
