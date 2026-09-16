// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import type {ExpressionMetadata} from '@app/features/expressions/state/ExpressionMetadata';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('ExpressionMetadataCommands');

interface ExpressionMetadataResponse {
	id: string;
	guild_id: string;
	name: string;
	animated: boolean;
	allow_cloning: boolean;
}

function mapResponse(body: ExpressionMetadataResponse): ExpressionMetadata {
	return {
		id: body.id,
		guildId: body.guild_id,
		name: body.name,
		animated: body.animated,
		allowCloning: body.allow_cloning,
	};
}

export type ExpressionKind = 'emoji' | 'sticker';

function metadataEndpoint(kind: ExpressionKind, id: string): string {
	return kind === 'emoji' ? Endpoints.EMOJI_METADATA(id) : Endpoints.STICKER_METADATA(id);
}

async function requestExpressionMetadata(kind: ExpressionKind, id: string): Promise<ExpressionMetadata> {
	const response = await http.get<ExpressionMetadataResponse>(metadataEndpoint(kind, id));
	return mapResponse(response.body);
}

function rethrowMetadataFailure(kind: ExpressionKind, id: string, error: unknown): never {
	logger.error(`Failed to fetch ${kind} metadata for ${id}:`, error);
	throw error;
}

export async function fetchEmojiMetadata(emojiId: string): Promise<ExpressionMetadata> {
	try {
		return await requestExpressionMetadata('emoji', emojiId);
	} catch (error) {
		rethrowMetadataFailure('emoji', emojiId, error);
	}
}

export async function fetchStickerMetadata(stickerId: string): Promise<ExpressionMetadata> {
	try {
		return await requestExpressionMetadata('sticker', stickerId);
	} catch (error) {
		rethrowMetadataFailure('sticker', stickerId, error);
	}
}
